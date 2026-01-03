/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.barray;
import dlib.allocator: Allocator;
import dlib.box: Box;
import core.stdc.string: memmove, memcpy;

struct Barray(T){
    size_t count;
    Box!(T[]) bdata;

    void
    clear(){ count = 0; }

    void
    ensure_additional(size_t N){
        size_t needed = count + N;
        ensure_total(needed);
    }

    T pop(){
        assert(count);
        return bdata.data[--count];
    }

    T pop(size_t index){
        T result = bdata.data[index];
        remove(index);
        return result;
    }

    void
    ensure_total(size_t needed){
        if(bdata.data.length >= needed)
            return;
        size_t new_capacity = bdata.data.length? bdata.data.length*2: 4;
        while(new_capacity < needed)
            new_capacity *=2;
        bdata.resize(new_capacity);
        // bdata.good_resize(new_capacity);
    }

    void
    push(in T item){
        // this cast seems bad?
        *alloc_item() = *cast(T*)&item;
    }

    T*
    alloc_item(){
        ensure_additional(1);
        return &bdata.data.ptr[count++];
    }
    T[]
    alloc_items(size_t N){
        ensure_additional(N);
        T[] result = bdata.data[count .. count+N];
        count += N;
        return result;
    }

    size_t
    alloc_index(size_t N){
        ensure_additional(1);
        return count++;
    }
    void
    insert(size_t index, in T value){
        if(index == count)
            return push(value);
        ensure_additional(1);
        size_t n_move = count - index;
        memmove(bdata.data.ptr+index+1, bdata.data.ptr+index, n_move*T.sizeof);
        bdata.data[index] = *cast(T*)&value; // this cast seems bad?
        count++;
    }
    void
    remove(size_t index){
        if(index == count-1){
            count--;
            return;
        }
        size_t n_move = count - index - 1;
        memmove(bdata.data.ptr+index, bdata.data.ptr+index+1, n_move*T.sizeof);
        count--;
    }
    void
    extend(scope T[] values){
        ensure_additional(values.length);
        memcpy(bdata.data.ptr+count, values.ptr, values.length*T.sizeof);
        count += values.length;
    }
    void
    extend(scope const T[] values){
        ensure_additional(values.length);
        memcpy(bdata.data.ptr+count, values.ptr, values.length*T.sizeof);
        count += values.length;
    }
    void
    extend(R)(scope R range){
        static if(__traits(hasMember, range, "length")){
            ensure_additional(range.length);
        }
        else static if(__traits(hasMember, range, "count")){
            ensure_additional(range.count);
        }
        else {
        }
        foreach(ref it; range)
            push(it);
    }
    void
    cleanup(){
        bdata.dealloc;
        bdata.data = null;
        count = 0;
    }
    inout(T)[]
    opIndex() inout {return bdata.data[0 .. count];}

    ref inout(T)
    opIndex(size_t i) inout{
        return bdata.data[0 .. count][i];
    }
    size_t opDollar(){
        return count;
    }

    size_t[2]
    opSlice(size_t start, size_t end) inout{
        size_t[2] result = [start, end];
        return result;
    }

    inout(T)[]
    opIndex(size_t[2] slice) inout{
        return bdata.data[0 .. count][slice[0] .. slice[1]];
    }

    static struct _Allocator {
        static if(Allocator.state_size)
            Allocator a;
    }
    alias AParam = typeof(_Allocator.tupleof);

    static
    typeof(this)
    from(AParam a, scope const(T)[] values){
        typeof(this) result;
        static if(Allocator.state_size)
            result.bdata.allocator = a[0];
        result.extend(values);
        return result;
    }

    void opOpAssign(string op)(in T value){
        push(value);
    }

    void
    swap(ref typeof(this) other){
        auto c = count;
        auto b = bdata;
        bdata = other.bdata;
        other.bdata = b;
        count = other.count;
        other.count = c;
    }
    // idk why `it == item` fails in betterC
    version(D_BetterC){
    }
    else{
        // O(N)
        bool
        contains(in T item){
            foreach(it; this)
                if(it == item) return true;
            return false;
        }
    }
}

Barray!(T)
make_barray(T)(Allocator a){
    Barray!(T) result;
    result.bdata.allocator = a;
    return result;
}


struct Deque(T){
    Box!(T[]) bdata;
    size_t front;
    size_t back;
    size_t count(){
        return back - front;
    }
    T pop(){
        assert(back != front);
        T result = bdata.data[--back];
        return result;
    }
    T pop_front(){
        assert(back != front);
        T result = bdata.data[front++];
        return result;
    }
    void push(T val){
        if(back >= bdata.data.length){
            size_t new_capacity = bdata.data.length?bdata.data.length*2:8;
            bdata.good_resize(new_capacity);
        }
        bdata.data[back++] = val;
    }
    void push_front(T val){
        if(!front && !back){
            push(val);
            return;
        }
        if(!front){
            size_t new_capacity = bdata.data.length?bdata.data.length*2:8;
            bdata.good_resize(new_capacity);
            size_t n_move = back;
            memmove(bdata.data.ptr+new_capacity/2, bdata.data.ptr+n_move, n_move*T.sizeof);
            front = new_capacity/2;
            back += new_capacity/2;
        }
        bdata.data[--front] = val;
    }
}
