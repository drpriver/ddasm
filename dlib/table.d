/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.table;
import dlib.allocator: Mallocator;
import dlib.box: Box;

struct Item(K, V){
    K key;
    V value;
}

uint
hash_fast_reduce(uint x, uint N){
    return (cast(ulong)x * cast(ulong)N) >> 32;
}

size_t
next_p2(size_t n){
    import core.bitop;
    if(popcnt(n) == 1) return n;
    return 1LU << (1+bsr(n));
}

struct Table(K, V){
    alias It = Item!(K, V);
    Box!(void[]) data;
    size_t count = 0;

    size_t capacity(){
        size_t len = data.data.length;
        return len / (It.sizeof + 2*uint.sizeof);
    }

    uint[]
    _idxes(){
        size_t cap = capacity;
        void* ptr = (cast(It*)data.data.ptr) + cap;
        return (cast(uint*)ptr)[0..cap*2];
    }

    It[]
    items(){
        return (cast(It*)data.data.ptr)[0..count];
    }
    It[]
    _items(){
        return (cast(It*)data.data.ptr)[0..capacity];
    }

    void reserve(size_t cap){
        cap = next_p2(cap);
        // TODO: ensure power of 2
        size_t old_cap = capacity;
        if(cap <= old_cap) return;
        size_t new_cap = old_cap * 2;
        if(!new_cap) new_cap = 4;
        data.resize(new_cap *(It.sizeof + 2*uint.sizeof));
        uint[] idxes = _idxes;
        idxes[] = uint.max;
        uint i_len = cast(uint)idxes.length;
        uint mask = i_len - 1;
        foreach(i, it; items){
            const hash = it.key.hashOf;
            uint idx = hash_fast_reduce(cast(uint)hash, i_len);
            while(idxes[idx] != uint.max){
                idx++;
                idx &= mask;
            }
            idxes[idx] = cast(uint)i;
        }
    }

    void clear(){
        count = 0;
        _idxes[] = uint.max;
    }

    void
    set_item(K k, V v){
        *set(k) = v;
    }

    V*
    set(K k){
        reserve(count+1);
        const hash = k.hashOf;
        uint[] idxes = _idxes;
        const i_len = cast(uint)idxes.length;
        uint mask = i_len - 1;
        uint idx = hash_fast_reduce(cast(uint)hash, cast(uint)idxes.length);
        auto items_ = items;
        while(idxes[idx] != uint.max){
            const i = idxes[idx];
            if(items_[i].key == k){
                return &items[i].value;
            }
            idx++;
            idx &= mask;
        }
        idxes[idx] = cast(uint)count;
        _items[count].key = k;
        return &_items[count++].value;
    }

    V* get_item(K k){
        if(!count) return null;
        const hash = k.hashOf;
        uint[] idxes = _idxes;
        const i_len = cast(uint)idxes.length;
        const mask = i_len-1;
        uint idx = hash_fast_reduce(cast(uint)hash, cast(uint)idxes.length);
        auto items_ = items;
        while(idxes[idx] != uint.max){
            const i = idxes[idx];
            if(items_[i].key == k){
                return &_items[i].value;
            }
            idx++;
            idx &= mask;
        }
        return null;
    }
    V*
    opBinaryRight(string op)(in K key) if(op == "in"){
        return get_item(key);
    }

    ref V
    opIndex(in K key){
        return *get_item(key);
    }

    void
    opIndexAssign(V value, in K key){
        set_item(key, value);
    }

    void cleanup(){
        data.dealloc;
        count = 0;
    }

    static struct ValueIter {
        It[] items;
        size_t cursor;
        bool empty(){ return cursor == items.length;}
        void popFront(){cursor++;}
        V front(){ return items[cursor].value;}
    }
    ValueIter values(){
        return ValueIter(items, 0);
    }

    void
    extend(ItemRange)(ItemRange items){
        foreach(it; items){
            static if(__traits(hasMember, it, "key"))
                *set(it.key) = it.value;
            else
                *set(it[0]) = it[1];
        }
    }


}
