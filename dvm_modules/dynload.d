/*
 * Copyright © 2025, David Priver
 */
module dvm_modules.dynload;

import core.stdc.stdio: fprintf, stderr, snprintf;
import core.stdc.string: strlen, memcpy;

import dvm.dvm_linked: LinkedModule, Function, FunctionType, FunctionInfo;
import dvm.dvm_unlinked: DlimportDecl, DlimportFuncSpec;
import dlib.allocator: MALLOCATOR, Allocator;
import dlib.aliases;

version(Windows){
    import core.sys.windows.winbase: LoadLibraryA, GetProcAddress, GetLastError;
    import core.sys.windows.winnt: HMODULE;
}
else {
    import core.sys.posix.dlfcn: dlopen, dlsym, dlerror, RTLD_NOW;
}

struct DynLoadError {
    str message;
    bool errored;
}

// Convert D slice to null-terminated string for C APIs
// Uses a static buffer - not thread safe but OK for this use case
private const(char)* to_cstr(str s){
    __gshared char[1024] buf;
    if(s.length >= buf.length) return null;
    memcpy(buf.ptr, s.ptr, s.length);
    buf[s.length] = 0;
    return buf.ptr;
}

// Helper to create a str from a C string
private str from_cstr(const(char)* p){
    if(p is null) return "";
    size_t len = strlen(p);
    return (cast(const(char)*)p)[0..len];
}

// Try to open a library at a specific path
private void* try_dlopen(const(char)* path){
    version(Windows){
        return cast(void*)LoadLibraryA(path);
    } else {
        return dlopen(path, RTLD_NOW);
    }
}

// Find and open a library by name, searching standard paths if needed
// Returns null if not found
private void* find_library(str name){
    __gshared char[512] buf;

    // Check if absolute or relative path - use as-is
    if(name.length > 0 && name[0] == '/'){
        return try_dlopen(to_cstr(name));
    }
    if(name.length > 1 && name[0] == '.' && name[1] == '/'){
        return try_dlopen(to_cstr(name));
    }

    // Try as-is first (might be in standard search path)
    void* h = try_dlopen(to_cstr(name));
    if(h) return h;

    // Platform-specific search
    version(OSX){
        // Try framework
        snprintf(buf.ptr, buf.length, "/Library/Frameworks/%.*s.framework/%.*s",
            cast(int)name.length, name.ptr, cast(int)name.length, name.ptr);
        h = try_dlopen(buf.ptr);
        if(h) return h;

        // Try homebrew (Apple Silicon)
        snprintf(buf.ptr, buf.length, "/opt/homebrew/lib/lib%.*s.dylib",
            cast(int)name.length, name.ptr);
        h = try_dlopen(buf.ptr);
        if(h) return h;

        // Try homebrew (Intel)
        snprintf(buf.ptr, buf.length, "/usr/local/lib/lib%.*s.dylib",
            cast(int)name.length, name.ptr);
        h = try_dlopen(buf.ptr);
        if(h) return h;
    }
    else version(linux){
        snprintf(buf.ptr, buf.length, "lib%.*s.so",
            cast(int)name.length, name.ptr);
        h = try_dlopen(buf.ptr);
        if(h) return h;
    }
    else version(Windows){
        snprintf(buf.ptr, buf.length, "%.*s.dll",
            cast(int)name.length, name.ptr);
        h = try_dlopen(buf.ptr);
        if(h) return h;
    }

    return null;
}

// Load a dynamic library and create a LinkedModule from dlimport specifications
DynLoadError
load_dynamic_module(
    Allocator allocator,
    ref DlimportDecl decl,
    LinkedModule* result
){
    DynLoadError err;

    // Try each library path in order until one succeeds
    void* handle = null;
    foreach(lib_path; decl.library_paths[]){
        handle = find_library(lib_path);
        if(handle) break;
    }
    if(!handle){
        err.errored = true;
        version(Windows){
            err.message = "library not found";
        } else {
            const(char)* dl_err = dlerror();
            if(dl_err){
                err.message = from_cstr(dl_err);
            } else {
                err.message = "library not found";
            }
        }
        return err;
    }

    result.name = decl.alias_name;
    result.functions.data.allocator = allocator;

    foreach(ref spec; decl.funcs[]){
        const(char)* sym_cstr = to_cstr(spec.name);
        if(sym_cstr is null){
            err.errored = true;
            err.message = "symbol name too long";
            return err;
        }

        void* sym;
        version(Windows){
            sym = cast(void*)GetProcAddress(cast(HMODULE)handle, sym_cstr);
        }
        else {
            sym = dlsym(handle, sym_cstr);
        }

        if(!sym){
            err.errored = true;
            // Build error message with symbol name
            __gshared char[256] errbuf;
            snprintf(errbuf.ptr, errbuf.length, "symbol not found: %.*s",
                cast(int)spec.name.length, spec.name.ptr);
            err.message = from_cstr(errbuf.ptr);
            return err;
        }

        // Create Function struct
        Function* f = cast(Function*)allocator.alloc(Function.sizeof).ptr;
        f.type = spec.is_varargs ? FunctionType.NATIVE_VARARGS : FunctionType.NATIVE;
        f.n_args = spec.n_args;
        f.n_ret = spec.n_ret;

        // Store the raw function pointer in the union
        // All variants share the same memory location
        f.native_function_ = cast(void function())sym;

        FunctionInfo fi = FunctionInfo(spec.name, f);
        result.functions[spec.name] = fi;
    }

    return err;
}
