struct Box(T, Allocator, bool inline_allocator=false){
    static if(!is(T == U[], U)){
        T* pointer;
        enum isVoid = is(T == void);
        static if(isVoid){
            size_t size;
        }
        static if(Allocator.state_size){
            static if(inline_allocator)
                Allocator allocator;
            else
                Allocator* allocator;

            static if(isVoid){
                static if(inline_allocator){
                    static
                    Box
                    alloc(Allocator al, size_t N){
                        typeof(this) result;
                        result.allocator = al;
                        result.pointer = result.allocator.alloc(N).ptr;
                        result.size = N;
                        return result;
                    }
                    static
                    Box
                    from(Allocator al, void* p, size_t N){
                        typeof(this) result;
                        result.allocator = al;
                        result.pointer = p;
                        result.size = N;
                        return result;
                    }

                }
                else {
                    static
                    Box
                    alloc(Allocator* al, size_t N){
                        void* p = al.alloc(N).ptr;
                        return Box(p, N, al);
                    }
                    static
                    Box
                    from(Allocator* al, void* p, size_t N){
                        typeof(this) result;
                        result.allocator = al;
                        result.pointer = p;
                        result.size = N;
                        return result;
                    }
                }
            }
            else{
                static if(inline_allocator){
                    static
                    Box
                    alloc(Allocator al){
                        typeof(this) result;
                        result.allocator = al;
                        result.pointer = cast(T*)result.allocator.alloc(T.sizeof).ptr;
                        return result;
                    }
                    static
                    Box
                    from(Allocator al, T* p){
                        typeof(this) result;
                        result.allocator = al;
                        result.pointer = p;
                        return result;
                    }
                }
                else {
                    static
                    Box
                    alloc(Allocator* al){
                        T* p = cast(T*)al.alloc(T.sizeof).ptr;
                        return Box(p, al);
                    }

                    static
                    Box
                    from(Allocator* al, T* p){
                        typeof(this) result;
                        result.allocator = al;
                        result.pointer = p;
                        return result;
                    }
                }
            }
        }
        else{
            alias allocator = Allocator;
            static if(isVoid){
                static
                Box
                alloc(size_t N){
                    void* p = allocator.alloc(N).ptr;
                    return Box(p, N);
                }
                static
                Box
                from(void* p, size_t N){
                    typeof(this) result;
                    result.pointer = p;
                    result.size = N;
                    return result;
                }
            }
            else {
                static
                Box
                alloc(){
                    T* p = cast(T*)allocator.alloc(T.sizeof).ptr;
                    return Box(p);
                }

                static
                Box
                from(T* p){
                    typeof(this) result;
                    result.pointer = p;
                    return result;
                }
            }
        }
        void
        dealloc(){
            static if(isVoid){
                allocator.free(pointer, size);
                size = 0;
            }
            else
                allocator.free(pointer, T.sizeof);
            pointer = null;
        }
    }
    else {
        U[] data;
        enum isVoid = is(U == void);
        static if(Allocator.state_size){
            Allocator* allocator;

            static
            Box
            alloc(Allocator* al, size_t N){
                static if(isVoid){
                    U[] p = al.alloc(1*N);
                }
                else {
                    void[] d = al.alloc(U.sizeof*N);
                    // https://issues.dlang.org/show_bug.cgi?id=22427
                    // U[] p = cast(U[])d;
                    U[] p = (cast(U*)d.ptr)[0..d.length/U.sizeof];
                }
                return Box(p, al);
            }
            static
            Box
            from(Allocator* al, U[] p){
                typeof(this) result;
                result.allocator = al;
                result.data = p;
                return result;
            }
            // TODO;
            // Box!CastTo
            // as(CastTo)(){
            // }
        }
        else{
            alias allocator = Allocator;
            static
            Box
            alloc(size_t N){
                static if(isVoid){
                    U[] p = allocator.alloc(1*N);
                }
                else{
                    auto d = allocator.alloc(U.sizeof*N);
                    // https://issues.dlang.org/show_bug.cgi?id=22427
                    // U[] p = cast(U[])d;
                    U[] p = (cast(U*)d.ptr)[0..N];
                }
                return Box(p);
            }
            static
            Box
            from(U[] p){
                typeof(this) result;
                result.data = p;
                return result;
            }
            Box!(CastTo, Allocator)
            as(CastTo)(){
                // `is` is so fucking weird
                static if(is(CastTo == C[], C)){
                // casting slices is broken in betterC
                // (causes weird linker errors in unrelated code, with dmd 2.098).
                static if(isVoid){
                    auto size = data.length*U.sizeof;
                }
                else {
                    auto size = data.length;
                }
                assert(size % C.sizeof == 0);
                auto newlength = size / C.sizeof;
                auto slice = (cast(C*)data.ptr)[0 .. newlength];
                return Box!(CastTo, Allocator).from(slice);
                }
                else {
                    static assert(0, "TODO: slice to single object");
                }
            }
        }
        void
        dealloc(){
            static if(isVoid){
                allocator.free(data.ptr, data.length);
            }
            else {
                allocator.free(data.ptr, U.sizeof*data.length);
            }
            data = null;
        }

        void
        resize(size_t N){
            static if(isVoid){
                data = allocator.realloc(data.ptr, data.length, N);
            }
            else {
                auto d = allocator.realloc(cast(void*)data.ptr, data.length*U.sizeof, N*U.sizeof);
                // https://issues.dlang.org/show_bug.cgi?id=22427
                // data = cast(U[])d;
                data = (cast(U*)d.ptr)[0..N];
            }
        }
        void
        good_resize(size_t N){
            static if(isVoid){
                N = allocator.good_size(N);
                data = allocator.realloc(data.ptr, data.length, N);
            }
            else {
                N = allocator.good_size(N*U.sizeof)/U.sizeof;
                auto d = allocator.realloc(cast(void*)data.ptr, data.length*U.sizeof, N*U.sizeof);
                data = (cast(U*)d.ptr)[0..N];
            }
        }
    }
}
