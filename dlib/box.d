/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.box;
import dlib.allocator: Allocator;
///
/// Box: A box is a container around a section of memory +
/// the allocator that allocated that memory.
struct Box(T){
    Allocator allocator;
    static if(!is(T == U[], U)){
        T* pointer;
        enum isVoid = is(T == void);
        static if(isVoid){
            size_t size;
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

        Box!(CastTo) as(CastTo)(){
            // `is` is so fucking weird
            static if(is(CastTo == C[], C)){
                // casting slices is broken in betterC
                // (causes weird linker errors in unrelated code, with dmd 2.098).
                static if(isVoid){
                    size_t size = data.length*U.sizeof;
                }
                else {
                    size_t size = data.length;
                }
                assert(size % C.sizeof == 0);
                size_t newlength = size / C.sizeof;
                C[] slice = (cast(C*)data.ptr)[0 .. newlength];
                return Box!(CastTo)(allocator, slice);
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
                void[] d = allocator.realloc(cast(void*)data.ptr, data.length*U.sizeof, N*U.sizeof);
                // https://issues.dlang.org/show_bug.cgi?id=22427
                // data = cast(U[])d;
                data = (cast(U*)d.ptr)[0..N];
            }
        }
    }
}
