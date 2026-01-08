/*
 * Copyright © 2026, David Priver
 */
module dlib.file_cache;
import dlib.allocator: Allocator;
import dlib.file_util;
import dlib.table: Table;
import dlib.stringbuilder: StringBuilder;
import dlib.aliases: str;

enum CachedFlags {
    NONE = 0,
    EXISTS = 1 << 0,
    UNREADABLE = 1 << 1,
    DATA_CACHED = 1 << 2,
    SIZE_CACHED = 1 << 3,
}

enum FileError {
    OK = 0,
    FAIL = 1,
};

struct CachedFile {
    const(ubyte)[] data;
    size_t size;
    CachedFlags flags;
}

inout(ubyte)[] strip_bom(inout(ubyte)[] data){
    if(data.length < 3) return data;
    if(data[0] != 0xef) return data;
    if(data[1] != 0xbb) return data;
    if(data[2] != 0xbf) return data;
    return data[3..$];
}

struct FileCache {
    @disable this();
    @disable this(this);
    @disable this(ref typeof(this));
    @disable void opAssign(typeof(this));

    Allocator allocator;
    StringBuilder path_builder;
    Table!(str, CachedFile) cache;
    Table!(str, str) cached_keys;

    this(Allocator a){
        allocator = a;
        path_builder.allocator = a;
        cache.data.allocator = a;
        cached_keys.data.allocator = a;
    }

    void cleanup(){
        path_builder.cleanup();
        // Note: cached file data uses allocator, would need to free each entry
        cache.cleanup();
        cached_keys.cleanup();
    }

    FileError read_file(str path, out const(ubyte)[] data, bool skip_bom = true){
        path_builder.reset();
        path_builder.write(path);
        return read_file(data, skip_bom);
    }
    FileError read_file(out const(ubyte)[] data, bool skip_bom = true){
        path_builder.nul_terminate();
        str key = path_builder.borrow();
        if(!key.length) return FileError.FAIL;
        if(CachedFile* f = key in cache){
            if(!(f.flags & CachedFlags.EXISTS))
                return FileError.FAIL;
            if(f.flags & CachedFlags.DATA_CACHED){
                data = skip_bom ? strip_bom(f.data[0..f.size]) : f.data[0..f.size];
                return FileError.OK;
            }
            if(f.flags & CachedFlags.UNREADABLE)
                return FileError.FAIL;
            // Key is in the cache and file exists, but we didn't cache the actual data.
            FileResult fr = dlib.file_util.read_file(key.ptr, allocator);
            if(fr.errored){
                f.flags |= CachedFlags.UNREADABLE;
                return FileError.FAIL;
            }
            const(ubyte)[] file_data = fr.value.as!(ubyte[])[0..fr.size];
            f.data = file_data;
            f.size = fr.size;
            f.flags |= CachedFlags.DATA_CACHED | CachedFlags.EXISTS | CachedFlags.SIZE_CACHED;
            data = skip_bom ? strip_bom(file_data) : file_data;
            return FileError.OK;
        }

        key = cache_key(key);
        path_builder.reset();
        FileResult fr = dlib.file_util.read_file(key.ptr, allocator);
        if(fr.errored){
            cache[key] = CachedFile(flags:CachedFlags.UNREADABLE);
            return FileError.FAIL;
        }
        const(ubyte)[] file_data = fr.value.as!(ubyte[])[0..fr.size];
        cache[key] = CachedFile(data:file_data, size:fr.size, flags:CachedFlags.DATA_CACHED | CachedFlags.SIZE_CACHED | CachedFlags.EXISTS);
        data = skip_bom ? strip_bom(file_data) : file_data;
        return FileError.OK;
    }

    // Check if path is a regular file (cached).
    FileError is_file(str path){
        path_builder.reset();
        path_builder.write(path);
        return is_file();
    }
    FileError is_file(){
        path_builder.nul_terminate();
        str key = path_builder.borrow();
        if(!key.length) return FileError.FAIL;
        if(CachedFile* f = key in cache){
            return (f.flags & CachedFlags.EXISTS) ? FileError.OK : FileError.FAIL;
        }
        key = cache_key(key);
        path_builder.reset();
        bool exists = dlib.file_util.file_exists(key.ptr);
        if(exists){
            cache[key] = CachedFile(flags: CachedFlags.EXISTS);
            return FileError.OK;
        } else {
            cache[key] = CachedFile(flags: CachedFlags.NONE);
            return FileError.FAIL;
        }
    }

    // Get file size (cached).
    FileError get_size(str path, out size_t size){
        path_builder.reset();
        path_builder.write(path);
        return get_size(size);
    }
    FileError get_size(out size_t size){
        path_builder.nul_terminate();
        str key = path_builder.borrow();
        if(!key.length) return FileError.FAIL;
        if(CachedFile* f = key in cache){
            if(!(f.flags & CachedFlags.EXISTS))
                return FileError.FAIL;
            if(f.flags & CachedFlags.SIZE_CACHED){
                size = f.size;
                return FileError.OK;
            }
            // Exists but size not cached - fetch it
            long result = dlib.file_util.get_file_size(key.ptr);
            if(result < 0) return FileError.FAIL;
            f.size = cast(size_t)result;
            f.flags |= CachedFlags.SIZE_CACHED;
            size = f.size;
            return FileError.OK;
        }
        key = cache_key(key);
        path_builder.reset();
        long result = dlib.file_util.get_file_size(key.ptr);
        if(result < 0){
            cache[key] = CachedFile(flags: CachedFlags.NONE);
            return FileError.FAIL;
        }
        size = cast(size_t)result;
        cache[key] = CachedFile(size: size, flags: CachedFlags.EXISTS | CachedFlags.SIZE_CACHED);
        return FileError.OK;
    }

    // Copy a key string using allocator.
    private str cache_key(str key){
        import core.stdc.string : memcpy;
        if(str* k = key in cached_keys)
            return *k;
        char[] copy = allocator.alloc!(char)(key.length+1);
        memcpy(copy.ptr, key.ptr, key.length);
        copy[key.length] = '\0';
        str result = copy[0..key.length];
        cached_keys[result] = result;
        return result;
    }

    // only use with string literals
    void cache_literal(str key){
        if(key !in cached_keys)
            cached_keys[key] = key;
    }

}
