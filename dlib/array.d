/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.array;
import dlib.allocator: Allocator;
import core.stdc.string: memmove, memcpy;

struct Barray(T){
    size_t count;
    T[] data;

    void
    clear(){ count = 0; }

    void
    ensure_additional(Allocator allocator, size_t N){
        size_t needed = count + N;
        ensure_total(a, needed);
    }

    T pop(){
        assert(count);
        return data[--count];
    }

    T pop(size_t index){
        T result = data[index];
        remove(index);
        return result;
    }

    void
    ensure_total(Allocator allocator, size_t needed){
        if(data.length >= needed)
            return;
        size_t new_capacity = data.length? data.length*2: 4;
        while(new_capacity < needed)
            new_capacity *=2;
        T[] new_data = allocator.realloc(data, new_capacity);
        data = new_data;
    }

    void
    push(Allocator allocator, in T item){
        // this cast seems bad?
        *alloc_item(allocator) = *cast(T*)&item;
    }

    T*
    alloc_item(Allocator allocator){
        ensure_additional(allocator, 1);
        return &data.ptr[count++];
    }
    T[]
    alloc_items(Alloccator allocator, size_t N){
        ensure_additional(allocator, N);
        T[] result = data[count .. count+N];
        count += N;
        return result;
    }

    size_t
    alloc_index(Allocator allocator, size_t N){
        ensure_additional(allocator, 1);
        return count++;
    }
    void
    insert(Allocator allocator, size_t index, in T value){
        if(index == count)
            return push(allocator, value);
        ensure_additional(allocator, 1);
        size_t n_move = count - index;
        memmove(data.ptr+index+1, data.ptr+index, n_move*T.sizeof);
        data[index] = *cast(T*)&value; // this cast seems bad?
        count++;
    }
    void
    remove(size_t index){
        if(index == count-1){
            count--;
            return;
        }
        size_t n_move = count - index - 1;
        memmove(data.ptr+index, data.ptr+index+1, n_move*T.sizeof);
        count--;
    }
    void
    extend(Allocator allocator, scope T[] values){
        ensure_additional(allocator, values.length);
        memcpy(data.ptr+count, values.ptr, values.length*T.sizeof);
        count += values.length;
    }
    void
    extend(R)(Allocator allocator, scope R range){
        static if(__traits(hasMember, range, "length")){
            ensure_additional(allocator, range.length);
        }
        else static if(__traits(hasMember, range, "count")){
            ensure_additional(allocator, range.count);
        }
        else {
        }
        foreach(ref it; range)
            push(allocator, it);
    }
    void
    cleanup(Allocator allocator){
        allocator.free(data);
        data = null;
        count = 0;
    }
    T[]
    opIndex(){return data[0 .. count];}

    ref T
    opIndex(size_t i){
        return data[0 .. count][i];
    }
    size_t opDollar(){
        return count;
    }

    size_t[2]
    opSlice(size_t start, size_t end){
        size_t[2] result = [start, end];
        return result;
    }

    T[]
    opIndex(size_t[2] slice){
        return data[0 .. count][slice[0] .. slice[1]];
    }

    static
    typeof(this)
    from(Allocator allocator, scope const(T)[] values){
        typeof(this) result;
        result.extend(allocator, values);
        return result;
    }

    // idk why `it == item` fails in betterC
    version(D_BetterC){
    }
    else{
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
    result.allocator = a;
    return result;
}


struct Deque(T){
    Box!(T[]) 
    size_t front;
    size_t back;
    size_t count(){
        return back - front;
    }
    T pop(){
        assert(back != front);
        T result = data[--back];
        return result;
    }
    T pop_front(){
        assert(back != front);
        T result = data[front++];
        return result;
    }
    void push(T val){
        if(back >= data.length){
            size_t new_capacity = data.length?data.length*2:8;
            good_resize(new_capacity);
        }
        data[back++] = val;
    }
    void push_front(T val){
        if(!front && !back){
            push(val);
            return;
        }
        if(!front){
            size_t new_capacity = data.length?data.length*2:8;
            good_resize(new_capacity);
            size_t n_move = back;
            memmove(data.ptr+new_capacity/2, data.ptr+n_move, n_move*T.sizeof);
            front = new_capacity/2;
            back += new_capacity/2;
        }
        data[--front] = val;
    }
}
