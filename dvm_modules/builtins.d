module dvm_modules.builtins;
import dvm.dvm_linked: LinkedModule, Function, FunctionType, FunctionTable, FunctionInfo;
import dlib.allocator: MALLOCATOR;
import dlib.aliases;
import dvm.dvm_defs: Fuzzing, uintptr_t;
import dlib.get_input: get_input_line, LineHistory;

__gshared bool devnull = false;

Function*
expose_varargs(void* fun, uint n_fixed) {
    Function* f = cast(Function*)MALLOCATOR.zalloc(Function.sizeof).ptr;
    f.type = FunctionType.NATIVE_VARARGS;
    f.native_function_ = cast(void function())fun;
    f.n_args = cast(ubyte)n_fixed;
    f.n_ret = 1;
    f.arg_types = 0;   // All integer args by default
    f.ret_types = 0;   // Integer return by default
    return f;
}

Function*
expose_function(F)(F fun){
    Function* f = cast(Function*)MALLOCATOR.zalloc(Function.sizeof).ptr;
    f.type = FunctionType.NATIVE;
    f.arg_types = 0;   // All integer args by default
    f.ret_types = 0;   // Integer return by default
    static if(is(F : uintptr_t function())){
        f.native_function_r = fun;
        f.n_ret = 1;
        f.n_args = 0;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t))){
        f.native_function_ra = fun;
        f.n_ret = 1;
        f.n_args = 1;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t))){
        f.native_function_raa = fun;
        f.n_ret = 1;
        f.n_args = 2;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaa = fun;
        f.n_ret = 1;
        f.n_args = 3;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaaa = fun;
        f.n_ret = 1;
        f.n_args = 4;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaaaa = fun;
        f.n_ret = 1;
        f.n_args = 5;
        return f;
    }
    else static if(is(F : void function())){
        f.native_function_ = fun;
        f.n_ret = 0;
        f.n_args = 0;
        return f;
    }
    else static if(is(F : void function(uintptr_t))){
        f.native_function_a = fun;
        f.n_ret = 0;
        f.n_args = 1;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t))){
        f.native_function_aa = fun;
        f.n_ret = 0;
        f.n_args = 2;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaa = fun;
        f.n_ret = 0;
        f.n_args = 3;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaaa = fun;
        f.n_ret = 0;
        f.n_args = 4;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaaaa = fun;
        f.n_ret = 0;
        f.n_args = 5;
        return f;
    }
    else {
    pragma(msg, F.stringof);
    pragma(msg, typeof(fun));
    static assert(0);
    }
}
LinkedModule*
get_io_module(){
    import core.stdc.stdio;
    __gshared LinkedModule mod;
    __gshared bool is_init;
    if(is_init) return &mod;
    mod.functions.data.allocator = MALLOCATOR;
    void reg(F)(str key, F fun){
        FunctionInfo fi = FunctionInfo(key, expose_function(fun));
        mod.functions[key] = fi;
    }
    // printf: 1 fixed arg (format string), variadic
    mod.functions["printf"] = FunctionInfo("printf", expose_varargs(cast(void*)&printf, 1));
    // fprintf: 2 fixed args (FILE*, format string), variadic
    mod.functions["fprintf"] = FunctionInfo("fprintf", expose_varargs(cast(void*)&fprintf, 2));
    reg("puts",
        (uintptr_t arg){
            if(devnull) return;
            if(!Fuzzing)fprintf(stdout, "%s\n", cast(char*)arg);
        }
    );
    reg("fread",
        (uintptr_t ptr, uintptr_t size, uintptr_t nitems, uintptr_t stream){
            return cast(uintptr_t)fread(cast(void*)ptr, size, nitems, cast(FILE*)stream);
        }
    );
    reg("fopen", (uintptr_t fn, uintptr_t mode){
            return cast(uintptr_t)fopen(cast(const char*)fn, cast(const char*)mode);
        }
    );
    reg("fclose", (uintptr_t fp){
            return cast(uintptr_t)fclose(cast(FILE*)fp);
    });
    reg("fwrite",
        (uintptr_t ptr, uintptr_t size, uintptr_t nitems, uintptr_t stream){
            return cast(uintptr_t)fwrite(cast(void*)ptr, size, nitems, cast(FILE*)stream);
        }
    );
    reg("fputs",
        (uintptr_t ptr, uintptr_t stream){
            return cast(uintptr_t)fputs(cast(const char*)ptr, cast(FILE*)stream);
        }
    );
    reg("fgets",
        (uintptr_t ptr, uintptr_t size, uintptr_t stream){
            return cast(uintptr_t)fgets(cast(char*)ptr, cast(int)size, cast(FILE*)stream);
        }
    );
    reg("fflush",
        (uintptr_t stream){
            return cast(uintptr_t)fflush(cast(FILE*)stream);
        }
    );
    reg("stdin",
        (){
            // return cast(uintptr_t)fopen("hello.txt", "r");
            return cast(uintptr_t)stdin;
        }
    );
    reg("stdout",
        (){
            return cast(uintptr_t)stdout;
        }
    );
    reg("getline",
        (uintptr_t buff, uintptr_t buflen, uintptr_t prompt_){
            __gshared LineHistory history = LineHistory(allocator:MALLOCATOR);
            import core.stdc.string: strlen;
            char* buff_ = cast(char*)buff;
            char* prompt = cast(char*)prompt_;
            str promptbuff;
            if(!prompt)
                promptbuff = "dasm> ";
            else
                promptbuff = prompt[0..strlen(prompt)];
            ptrdiff_t len = get_input_line(&history, promptbuff, buff_[0..buflen]);
            if(len >= 0 && len < buflen)
                buff_[len] = 0;
            else
                buff_[buflen-1] = 0;
            return cast(uintptr_t)len;
        }
    );
    is_init = true;
    return &mod;
}
LinkedModule*
get_mem_module(){
    import core.stdc.stdlib;
    import core.stdc.string;
    __gshared LinkedModule mod;
    __gshared bool is_init;
    if(is_init) return &mod;
    mod.functions.data.allocator = MALLOCATOR;
    void reg(F)(str key, F fun){
        FunctionInfo fi = FunctionInfo(key, expose_function(fun));
        mod.functions[key] = fi;
    }
    reg("calloc", (uintptr_t nitems, uintptr_t size){
        return cast(uintptr_t)calloc(nitems, size);
    });
    reg("set", (uintptr_t dst, uintptr_t c, uintptr_t sz){
        void* buff = cast(void*)dst;
        memset(buff, cast(int)c, sz);
    });
    reg("cpy",
        (uintptr_t dst, uintptr_t src, uintptr_t len){
            return cast(uintptr_t)memcpy(cast(void*)dst, cast(void*)src, len);
        }
    );
    reg("free",
        (uintptr_t ptr){
            free(cast(void*)ptr);
        }
    );
    reg("malloc",
        (uintptr_t size){
            return cast(uintptr_t)malloc(size);
        }
    );
    is_init = true;
    return &mod;
}

version(Posix){
    struct timespec {
        long tv_sec;
        long tv_nsec;
    }
    extern(C)
    int clock_gettime(int __clock_id, timespec *__tp);
}
LinkedModule*
get_misc_module(){
    import core.stdc.stdlib;
    __gshared LinkedModule mod;
    __gshared bool is_init;
    if(is_init) return &mod;
    mod.functions.data.allocator = MALLOCATOR;
    void reg(F)(str key, F fun){
        FunctionInfo fi = FunctionInfo(key, expose_function(fun));
        mod.functions[key] = fi;
    }
    reg("atoi", (uintptr_t p){
        return cast(uintptr_t)atoi(cast(char*)p);
    });
    reg("rand", (){
        return cast(uintptr_t)rand();
    });
    reg("srand", (uintptr_t seed){
        srand(cast(uint)seed);
    });
    reg("exit", (uintptr_t p){
        return exit(cast(int)cast(intptr_t)p);
    });
    version(Posix)
        reg("misc.clock", (){
            timespec tv;
            clock_gettime(6, &tv);
            uintptr_t result =  tv.tv_sec * 1000*1000*1000 + tv.tv_nsec;
            return result;
        });
    is_init = true;
    return &mod;
}
