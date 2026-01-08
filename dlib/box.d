/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.box;
import dlib.allocator: Allocator;
///
/// Box: A box is a container around a section of memory +
/// the allocator that allocated that memory.
struct Box(T, A=Allocator){
    static if(A.state_size)
        A allocator;
    else
        alias allocator = A;
    static if(!is(T == U[], U)){
        T* pointer;
        enum isVoid = is(T == void);
        static if(isVoid){
            size_t size;
            bool opCast(T: bool)(){
                return pointer !is null && size;
            }
            Box!(T, Allocator) erase(){ return Box!(T, Allocator)(allocator.allocator(), pointer, size); }
        }
        else {
            ref T opUnary(string s)() if(s == "*"){
                return *pointer;
            }
            bool opCast(T: bool)(){
                return pointer !is null;
            }
            Box!(T, Allocator) erase(){ return Box!(T, Allocator)(allocator.allocator(), pointer); }
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
        inout(T)[] opIndex() inout {return pointer?pointer[0..1]:null;}

        Box!(CastTo) as(CastTo)(){
            // `is` is so fucking weird
            static if(is(CastTo == C[], C)){
                // casting slices is broken in betterC
                // (causes weird linker errors in unrelated code, with dmd 2.098).
                static if(isVoid){
                }
                else {
                    size_t size = T.sizeof;
                }
                assert(size % C.sizeof == 0);
                size_t newlength = size / C.sizeof;
                C[] slice = (cast(C*)pointer)[0 .. newlength];
                return Box!(CastTo)(allocator, slice);
            }
            else {
                static if(isVoid)
                    assert(size == CastTo.sizeof);
                else
                    static assert(T.sizeof == CastTo.sizeof);
                return Box!(CastTo)(allocator, cast(CastTo*)pointer);
            }
        }
    }
    else {
        U[] data;
        enum isVoid = is(U == void);
        bool opCast(T: bool)(){
            return data.length;
        }
        Box!(T, Allocator) erase(){ return Box!(T, Allocator)(allocator.allocator(), data); }

        Box!(CastTo) as(CastTo)(){
            // `is` is so fucking weird
            static if(is(CastTo == C[], C)){
                // casting slices is broken in betterC
                // (causes weird linker errors in unrelated code, with dmd 2.098).
                static if(isVoid){
                    size_t size = data.length;
                }
                else {
                    size_t size = data.length*U.sizeof;
                }
                assert(size % C.sizeof == 0);
                size_t newlength = size / C.sizeof;
                C[] slice = (cast(C*)data.ptr)[0 .. newlength];
                return Box!(CastTo)(allocator, slice);
            }
            else static if(is(CastTo == U)){
                assert(data.length == 1);
                return Box!(CastTo)(allocator, data.ptr);
            }
            else {
                static if(isVoid)
                    assert(data.length == CastTo.sizeof);
                else
                    assert(data.length * U.sizeof == CastTo.sizeof);
                return Box!(CastTo)(allocator, cast(CastTo*)data.ptr);
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
        inout(U)[] opIndex() inout {return data;}
        size_t opDollar(){ return data.length; }
        static if(!isVoid)
            ref inout(U) opIndex(size_t i) inout{
                return data[i];
            }
        inout(U)[]
        opSlice(size_t dim:0)(size_t start, size_t end) inout{
            return data[start .. end];
        }
        inout(U)[]
        opIndex()(inout(U)[] slice) inout{
            return slice;
        }
    }
}


Box!(T, A)
boxed(T, A)(A a, T* val)if(A.state_size){
    T* copy = (a.alloc!T)(1).ptr;
    *copy = *val;
    return Box!(T, A)(a, copy);
}

Box!(T, A)
boxed(T, A)(A a, T[] val)if(A.state_size){
    T[] copy = (a.alloc!T)(val.length);
    copy[] = val[];
    return Box!(T, A)(a, copy);
}

Box!(T[], A)
boxed(T, A)(A a, size_t len)if(A.state_size){
    T[] p = (a.zalloc!T)(len);
    return Box!(T[])(a, p);
}
Box!(T, A)
boxed(T, A)(A a)if(A.state_size){
    T* p = (a.zalloc!T)(1).ptr;
    return Box!T(a, p);
}

Box!(T, A)
boxed(T, A)(T* val)if(!A.state_size){
    T* copy = (a.alloc!T)(1).ptr;
    *copy = *val;
    return Box!(T, A)(copy);
}

Box!(T, A)
boxed(T, A)(T[] val)if(!A.state_size){
    T[] copy = (a.alloc!T)(val.length);
    copy[] = val[];
    return Box!(T, A)(copy);
}

Box!(T[], A)
boxed(T, A)(size_t len)if(!A.state_size){
    T[] p = (a.zalloc!T)(len);
    return Box!(T[], A)(p);
}
Box!(T, A)
boxed(T, A)()if(!A.state_size){
    T* p = (a.zalloc!T)(1).ptr;
    return Box!T(p, A);
}
