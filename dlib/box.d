/*
 * Copyright © 2021-2023, David Priver
 */
module dlib.box;
///
/// Box: A box is a container around a section of memory +
/// the allocator that allocated that memory.
struct Box(T, Allocator){
    static struct _Allocator {
        static if(Allocator.state_size){
            Allocator a;
        }
    }
    // expands to nothing if state_size is 0
    alias AParam = typeof(_Allocator.tupleof);
    static if(!is(T == U[], U)){
        T* pointer;
        enum isVoid = is(T == void);
        static if(isVoid){
            size_t size;
        }
        static if(Allocator.state_size)
            Allocator allocator;
        else
            alias allocator = Allocator;

        static if(isVoid){
            static
            Box
            alloc(AParam a, size_t N){
                static if(Allocator.state_size){
                    void* p = a[0].alloc(N).ptr;
                    return Box(p, N, a[0]);
                }
                else {
                    void* p = allocator.alloc(N).ptr;
                    return Box(p, N);
                }
            }
            static
            Box
            from(AParam a, void* p, size_t N){
                typeof(this) result;
                static if(Allocator.state_size)
                    result.allocator = a[0];
                result.pointer = p;
                result.size = N;
                return result;
            }
        }
        else{
            static
            Box
            alloc(AParam a){
                static if(Allocator.state_size){
                    T* p = cast(T*)a[0].alloc(T.sizeof).ptr;
                    return Box(p, a[0]);
                }
                else {
                    T* p = cast(T*)allocator.alloc(T.sizeof).ptr;
                    return Box(p);
                }
            }

            static
            Box
            from(AParam a, T* p){
                typeof(this) result;
                static if(Allocator.state_size)
                    result.allocator = a[0];
                result.pointer = p;
                return result;
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
            Allocator allocator;
        }
        else {
            alias allocator = Allocator;
        }

        static
        Box
        alloc(AParam a, size_t N){
            static if(isVoid){
                static if(Allocator.state_size)
                    U[] p = a[0].alloc(1*N);
                else
                    U[] p = allocator.alloc(1*N);
            }
            else {
                static if(Allocator.state_size)
                    void[] d = a[0].alloc(U.sizeof*N);
                else
                    void[] d = allocator.alloc(U.sizeof*N);
                // https://issues.dlang.org/show_bug.cgi?id=22427
                // Because of the above bug, you need to do a --allinst
                U[] p = cast(U[])d;
                // workaround for above bug:
                // U[] p = (cast(U*)d.ptr)[0..d.length/U.sizeof];
            }
            static if(Allocator.state_size)
                return Box(p, a[0]);
            else
                return Box(p);
        }
        static
        Box
        from(AParam a, U[] p){
            typeof(this) result;
            static if(Allocator.state_size)
                result.allocator = a[0];
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
                static if(Allocator.state_size)
                    return Box!(CastTo, Allocator).from(allocator, slice);
                else
                    return Box!(CastTo, Allocator).from(slice);
            }
            else {
                static assert(0, "TODO: slice to single object");
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
