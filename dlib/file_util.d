/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.file_util;
import dlib.box: Box;
import core.stdc.string: memset;
import dlib.allocator: MALLOCATOR, Allocator;

struct FileResult {
    Box!(void[]) value;
    size_t size;  // Actual file size (before padding)
    int errored;
    pragma(inline, true)
    Box!(void[]) unwrap(){
        assert(!errored);
        return value;
    }
}

version(Posix){
    import core.sys.posix.unistd;
    import core.sys.posix.fcntl;
    import core.sys.posix.sys.stat;
    import core.stdc.errno;
}

version(Windows){
    import core.sys.windows.stat;
    import core.sys.windows.winbase;
}
enum FileFlags: ulong {
    NONE = 0,
    // These are only considered on reading
    NUL_TERMINATE = 1 << 0,
    ZERO_PAD_TO_16 = 1 << 1,
    ZERO_PAD_TO_32 = 1 << 2,
    // These are only considered on writing.
    NEWLINE_TERMINATE = 1 << 3,
}


FileResult
read_file(const char* filepath, Allocator a = MALLOCATOR, FileFlags flags = FileFlags.NONE){
    FileResult result;
    result.value.allocator = a;
    version(Posix){
        int fd = open(filepath, O_RDONLY);
        if(fd < 0){
            result.errored = errno;
            return result;
        }
        scope(exit) close(fd);
        stat_t s;
        int err = fstat(fd, &s);
        if(err == -1){
            result.errored = errno;
            return result;
        }
        size_t size = s.st_size;
        size_t real_size = size;
        if(flags & FileFlags.NUL_TERMINATE)
            size += 1;
        if(flags & FileFlags.ZERO_PAD_TO_16){
            if(size & 15) size += 16 - (size & 15);
        }
        if(flags & FileFlags.ZERO_PAD_TO_32){
            if(size & 31) size += 32 - (size & 31);
        }
        result.value.resize(size);
        auto read_result = read(fd, result.value.data.ptr, real_size);
        if(read_result != real_size){
            result.value.dealloc;
            result.errored = errno;
            return result;
        }
        if(size != real_size)
            memset(result.value.data.ptr+real_size, 0, size-real_size);
        result.size = real_size;
        return result;
    }
    else version(Windows){
        HANDLE handle = CreateFileA(
            cast(char*)filepath,
            GENERIC_READ,
            FILE_SHARE_READ,
            null,
            OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL,
            null
        );
        if(handle == INVALID_HANDLE_VALUE){
            result.errored = GetLastError();
            return result;
        }
        scope(exit) CloseHandle(handle);
        LARGE_INTEGER li_size;
        BOOL size_success = GetFileSize(handle, &li_size);
        if(!size_success){
            result.errored = GetLastError();
            return result;
        }
        size_t size = li_size.QuadPart;
        size_t real_size = size;
        if(flags & FileFlags.NUL_TERMINATE)
            size += 1;
        if(flags & FileFlags.ZERO_PAD_TO_16){
            if(size & 15) size += 16 - (size & 15);
        }
        if(flags & FileFlags.ZERO_PAD_TO_32){
            if(size & 31) size += 32 - (size & 31);
        }
        result.value.resize(size);
        DWORD nread;
        BOOL read_success = ReadFile(handle, result.value.data.ptr, real_size, &nread, null);
        if(!read_success){
            result.value.dealloc;
            result.errored = GetLastError();
            return result;
        }
        if(size != real_size)
            memset(result.value.data.ptr+real_size, 0, size-real_size);
        result.size = real_size;
        return result;
    }
    else {
        assert(0, "unimplemented");
    }
}


int
write_file(const void[] data, const char* filepath, FileFlags flags = FileFlags.NONE){
    version(Windows){
        HANDLE handle = CreateFileA(
            cast(char*)filepath,
            GENERIC_WRITE,
            0,
            NULL,
            CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            NULL
        );
        if(handle == INVALID_HANDLE_VALUE)
            return GetLastError();
        scope(exit) CloseHandle(handle);

        DWORD bytes_written;
        BOOL write_success = WriteFile(
                handle,
                data.ptr,
                data.length,
                &bytes_written,
                null);
        if(!write_success)
            return GetLastError();
        assert(bytes_written == data.length);
        if(flags & FileFlags.NEWLINE_TERMINATE){
            if(!data.length || (cast(char[])data)[$-1] != '\n'){
                WriteFile(handle, "\n".ptr, 1, &bytes_written, null);
            }
        }
        return 0;
    }
    else version(Posix){
        int fd = open(
            filepath,
            O_WRONLY | O_CREAT | O_TRUNC,
            S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
        if(fd < 0)
            return errno;
        scope(exit) close(fd);
        auto nwrit = write(fd, data.ptr, data.length);
        if(nwrit != data.length)
            return errno;
        if(flags & FileFlags.NEWLINE_TERMINATE){
            if((cast(char[])data)[$-1] != '\n'){
                write(fd, "\n".ptr, 1);
            }
        }
        return 0;
    }
    else {
        assert(0, "unimplemented");
    }
}

// Get file size without reading contents. Returns -1 on error.
long get_file_size(const char* filepath){
    version(Posix){
        stat_t s;
        int err = stat(filepath, &s);
        if(err == -1) return -1;
        return s.st_size;
    }
    else version(Windows){
        HANDLE handle = CreateFileA(
            cast(char*)filepath,
            GENERIC_READ,
            FILE_SHARE_READ,
            null,
            OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL,
            null
        );
        if(handle == INVALID_HANDLE_VALUE) return -1;
        scope(exit) CloseHandle(handle);
        LARGE_INTEGER li_size;
        BOOL success = GetFileSizeEx(handle, &li_size);
        if(!success) return -1;
        return li_size.QuadPart;
    }
    else {
        assert(0, "unimplemented");
    }
}

// Check if regular file exists (returns false for directories).
bool file_exists(const char* filepath){
    version(Posix){
        stat_t s;
        if(stat(filepath, &s) != 0) return false;
        return (s.st_mode & S_IFMT) == S_IFREG;
    }
    else version(Windows){
        DWORD attrs = GetFileAttributesA(filepath);
        if(attrs == INVALID_FILE_ATTRIBUTES) return false;
        return (attrs & FILE_ATTRIBUTE_DIRECTORY) == 0;
    }
    else {
        assert(0, "unimplemented");
    }
}
