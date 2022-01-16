/*
 * Copyright © 2021-2022, David Priver
 */
import allocator: Mallocator;
import box: Box;
import core.stdc.string: memset, memcpy;
struct Item(K, V) {
    K key;
    V value;
}
///
/// A flat array of keys, then values for small
/// tables.
struct BHashlessFlatTable(K, V, Allocator){
    Box!(void[], Allocator) bdata;
    size_t occupancy;
    size_t capacity;
    /// Keys as a slice.
    K[]
    keys(){
        if(!capacity) return null;
        return (cast(K*)bdata.data.ptr)[0..occupancy];
    }
    /// Values as a slice.
    V[]
    values(){
        if(!capacity) return null;
        return (cast(V*)(bdata.data.ptr+K.sizeof*capacity))[0..occupancy];
    }

    static struct ItemIterator {
        size_t cursor;
        size_t occupancy;
        K[] ks;
        V[] vs;
        bool
        empty(){
            return cursor >= occupancy;
        }
        void
        popFront(){
            cursor++;
        }
        Item!(K,V)
        front(){
            return Item!(K, V)(ks[cursor], vs[cursor]);
        }
    }

    /// Items as a range
    ItemIterator
    items(){
        return ItemIterator(0, occupancy, keys(), values());
    }
    ///
    /// Sets the key to `key`, returning the uninitialized
    /// value slot.
    ///
    /// Ensures there is enough space remanining.
    ///
    V*
    set(in K key){
        if(occupancy == capacity){
            expand(capacity?capacity*2:2);
        }
        return set_unchecked(key);
    }
    ///
    /// Like `set`, but does not check for remaining space.
    V*
    set_unchecked(in K key){
        foreach(i, k; keys){
            if(k == key){
                return &values[i];
            }
        }
        occupancy++;
        auto ks = keys();
        keys()[$-1] = cast(K)cast()key;
        return &values[$-1];
    }
    ///
    /// Returns the corresponding value for the given key.
    /// If key is not in the table, returns null.
    ///
    V*
    get(in K key){
        foreach(i, k; keys){
            if(k == key){
                return &values[i];
            }
        }
        return null;
    }
    ///
    /// Resizes the table to hold at least `n_items` items.
    ///
    void
    expand(size_t n_items){
        if(n_items < capacity)
            return;
        static if(!Allocator.state_size)
            auto map = make(n_items);
        else
            auto map = make(bdata.allocator, n_items);
        auto vs = values;
        auto ks = keys;
        foreach(i, k; ks)
            map[k] = vs[i];
        bdata.dealloc;
        this = map;
    }

    /// ditto
    void
    reserve(size_t n_items){
        expand(n_items);
    }

    static if(!Allocator.state_size){
        /// Make a new instance of the given size.
        typeof(this)
        make(size_t n_items){
            typeof(this) result;
            result.bdata.resize(alloc_size(n_items));
            result.capacity = n_items;
            return result;
        }
    }
    else {
        /// Make a new instance of the given size.
        typeof(this)
        make(Allocator*a, size_t n_items){
            typeof(this) result;
            result.bdata.allocator = a;
            result.bdata.resize(alloc_size(n_items));
            result.capacity = n_items;
            return result;
        }
    }

    /// Removes the key from the table. Returns true on success,
    /// false if the key was not in the table.
    bool
    del(in K key){
        auto ks = keys;
        foreach(i, k; ks){
            if(k == key){
                if(i == ks.length-1){
                    occupancy--;
                    return true;
                }
                ks[i] = ks[$-1];
                auto vs = values;
                vs[i] = vs[$-1];
                occupancy--;
                return true;
            }
        }
        return false;
    }


    ///
    /// How much memory is needed to allocate a table of the given size.
    static
    size_t
    alloc_size(size_t n_items){
        return n_items*K.sizeof + n_items*V.sizeof;
    }

    void
    opIndexAssign(in V value, in K key){
        *set(key) = cast(V)cast()value;
    }

    ref V
    opIndex(in K key){
        return *get(key);
    }

    V*
    opBinaryRight(string op)(in K key) if(op == "in"){
        return get(key);
    }

    /// Deallocs the memory allocated for this table.
    void
    cleanup(){
        bdata.dealloc();
        capacity = 0;
        occupancy = 0;
    }
}

struct BHashTable(K, V, Allocator){
    Box!(void[], Allocator) bdata;
    size_t occupancy;
    size_t capacity;
    size_t tomb_count;

    static
    size_t
    alloc_size(size_t n_items){
        size_t mask_length = (n_items/64) + !!(n_items & 63);
        size_t filled_size = mask_length * ulong.sizeof;
        size_t tombs_size = mask_length * ulong.sizeof;
        size_t keys_size = n_items*K.sizeof;
        size_t values_size = n_items*V.sizeof;
        size_t hashes_size = n_items * uint.sizeof;
        size_t size = filled_size + tombs_size + keys_size + values_size + hashes_size;
        return size;
    }

    size_t get_mask_length(){
        size_t mask_length = capacity / 64 + !!(capacity & 63);
        return mask_length;
    }
    ulong[]
    get_filled(){
        auto mask_length = get_mask_length;
        auto p = cast(ulong*)bdata.data.ptr;
        return p[0..mask_length];
    }

    ulong[]
    get_tombs(){
        auto mask_length = get_mask_length;
        char* p = cast(char*)bdata.data.ptr;
        p += mask_length*ulong.sizeof;
        return (cast(ulong*)p)[0..mask_length];
    }
    V[]
    get_values(){
        auto mask_length = get_mask_length;
        char* p = cast(char*)bdata.data.ptr;
        p += 2*mask_length*ulong.sizeof + K.sizeof*capacity;
        return (cast(V*)p)[0 .. capacity];
    }
    K[]
    get_keys(){
        auto mask_length = get_mask_length;
        char* p = cast(char*)bdata.data.ptr;
        p += 2*mask_length*ulong.sizeof;
        return (cast(K*)p)[0 .. capacity];
    }
    uint[]
    get_hashes(){
        auto mask_length = get_mask_length;
        char* p = cast(char*)bdata.data.ptr;
        p += 2*mask_length*ulong.sizeof + K.sizeof*capacity + V.sizeof*capacity;
        return (cast(uint*)p)[0 .. capacity];
    }

    void
    clear(){
        get_filled()[] = 0;
        get_tombs()[] = 0;
        occupancy = 0;
        tomb_count = 0;
    }

    void
    setup(size_t length){
        capacity = length;
        occupancy = 0;
        tomb_count = 0;
        size_t size = alloc_size(length);
        bdata.resize(size);
        get_filled()[] = 0;
        get_tombs()[] = 0;
    }

    void
    cleanup(){
        bdata.dealloc();
        capacity = 0;
        occupancy = 0;
        tomb_count = 0;
    }

    static if(Allocator.state_size){
        static
        typeof(this)
        make(Allocator*allocator, size_t length){
            typeof(this) result;
            result.bdata.allocator = allocator;
            result.setup(length);
            return result;
        }
        void
        expand(){
            auto new_hm = make(bdata.allocator, capacity?capacity*2:16);
            auto filled = get_filled();
            auto ks = get_keys();
            auto vs = get_values();
            for(size_t i = 0; i < capacity; i++){
                size_t filled_idx = i/64;
                if(filled[filled_idx] & (1LU << (i %64))){
                    *new_hm.set(ks[i]) = vs[i];
                }
            }
            bdata.dealloc;
            this = new_hm;
        }
    }
    else {
        static
        typeof(this)
        make(size_t length){
            typeof(this) result;
            result.setup(length);
            return result;
        }
        void
        expand(){
            auto new_hm = make(capacity?capacity*2:16);
            auto filled = get_filled();
            auto keys = get_keys();
            auto values = get_values();
            for(size_t i = 0; i < capacity; i++){
                size_t filled_idx = i/64;
                if(filled[filled_idx] & (1LU << (i %64))){
                    *new_hm.set(keys[i]) = values[i];
                }
            }
            bdata.dealloc;
            this = new_hm;
        }
    }

    void
    clear_tombs(){
        auto ks = get_keys();
        auto vs = get_values();
        size_t tmp_size = K.sizeof*occupancy + V.sizeof*occupancy;
        static if(Allocator.state_size){
            auto tmp = Box!(void, Allocator).alloc(bdata.allocator, tmp_size);
        }
        else {
            auto tmp = Box!(void, Allocator).alloc(tmp_size);
        }
        K[] tmp_keys = (cast(K*)tmp.pointer)[0..occupancy];
        V[] tmp_values = (cast(V*)(tmp_keys.ptr+tmp_keys.length))[0..occupancy];
        ulong[] fs = get_filled;
        size_t count = 0;
        for(ulong i = 0; i < capacity; i++){
            ulong which = i / 64;
            ulong mask = 1LU << (i % 64);
            if(fs[which] & mask){
                tmp_keys[count] = ks[i];
                tmp_values[count] = vs[i];
                count++;
            }
        }
        auto mask_length = get_mask_length;
        auto ts = get_tombs();
        ts[] = 0;
        fs[] = 0;
        occupancy = 0;
        tomb_count = 0;
        for(size_t i = 0; i < count; i++){
            *set_unchecked(tmp_keys[i]) = tmp_values[i];
        }
        tmp.dealloc;
    }

    V*
    set_unchecked(in K key){
        uint hash = cast(uint)(key.hashOf);
        const initial_index = hash_fast_reduce(hash, cast(uint)capacity);
        uint index = initial_index;
        uint last_index = cast(uint)capacity - 1;
        uint first_tomb = uint.max;
        auto filled = get_filled();
        auto tombs = get_tombs();
        auto keys = get_keys();
        auto hashes = get_hashes();
        auto values = get_values();
        do{
            uint which = index / 64;
            ulong mask = 1LU << (index % 64);
            if(filled[which] & mask){
                if(hashes[index] == hash && keys[index] == key){
                    return &values[index];
                }
            }
            else if(tombs[which] & mask){
                if(first_tomb == uint.max)
                    first_tomb = index;
            }
            else if(first_tomb != uint.max){
                uint tomb_which = first_tomb/64;
                ulong tomb_mask = 1LU << (first_tomb % 64);
                keys[first_tomb] = key;
                hashes[first_tomb] = hash;
                filled[tomb_which] |= tomb_mask;
                tombs[tomb_which] &= ~tomb_mask;
                occupancy++;
                tomb_count--;
                return &values[first_tomb];
            }
            else {
                keys[index] = key;
                hashes[index] = hash;
                filled[which] |= mask;
                tombs[which] &= ~mask;
                occupancy++;
                return &values[index];
            }
            index++;
            if(index > last_index) index = 0;
        }while(index != initial_index);
        assert(0);
    }
    V*
    set(in K key){
        if(!capacity)
            setup(16);
        if(tomb_count > capacity /2)
            clear_tombs;
        if((occupancy + tomb_count)*3/2 > capacity)
            expand();
        return set_unchecked(key);
    }
    void
    reserve(size_t N){
        N*=2;
        if(!capacity){
            setup(N);
            return;
        }
        if(capacity < N){
            static if(Allocator.state_size)
                auto new_hm = make(bdata.allocator, N);
            else
                auto new_hm = make(N);
            auto filled = get_filled();
            auto keys = get_keys();
            auto values = get_values();
            for(size_t i = 0; i < capacity; i++){
                size_t filled_idx = i/64;
                if(filled[filled_idx] & (1LU << (i %64))){
                    *new_hm.set(keys[i]) = values[i];
                }
            }
            bdata.dealloc;
            this = new_hm;
        }
    }
    V*
    get(in K key){
        if(!capacity)
            return null;
        uint hash = cast(uint)(key.hashOf);
        const initial_index = hash_fast_reduce(hash, cast(uint)capacity);
        uint index = initial_index;
        uint last_index = cast(uint)capacity - 1;
        auto filled = get_filled();
        auto tombs = get_tombs();
        auto keys = get_keys();
        auto hashes = get_hashes();
        auto values = get_values();
        do {
            uint which = index / 64;
            ulong mask = 1LU << (index % 64);
            if(filled[which] & mask){
                if(hashes[index] == hash && keys[index] == key){
                    return &values[index];
                }
            }
            else if(tombs[which] & mask){
            }
            else {
                return null;
            }
            index++;
            if(index > last_index)
                index = 0;
        }while(index != initial_index);
        assert(0);
    }
    bool
    del(in K key){
        if(!capacity) return false;
        uint hash = cast(uint)(key.hashOf);
        const initial_index = hash_fast_reduce(hash, cast(uint)capacity);
        uint index = initial_index;
        uint last_index = cast(uint)capacity - 1;
        auto filled = get_filled();
        auto tombs = get_tombs();
        auto keys = get_keys();
        auto hashes = get_hashes();
        do {
            uint which = index / 64;
            ulong mask = 1LU << (index % 64);
            if(filled[which] & mask){
                if(hashes[index] == hash && keys[index] == key){
                    tombs[which] |= mask;
                    filled[which] &= ~mask;
                    occupancy--;
                    tomb_count++;
                    return true;
                }
            }
            else {
                if(tombs[which] & mask){
                }
                else {
                    return false;
                }
            }
            index++;
            if(index > last_index)
                index = 0;
        } while(index != initial_index);
        assert(0);
    }
    bool
    contains(in K key){
        return !!get(key);
    }

    void
    opIndexAssign(in V value, in K key){
        *set(key) = cast(V)cast()value;
    }

    ref V
    opIndex(in K key){
        return *get(key);
    }

    V*
    opBinaryRight(string op)(in K key) if(op == "in"){
        return get(key);
    }

    enum IterWhat: uint {
        KEYS = 1,
        VALUES = 2,
        BOTH = KEYS | VALUES,
    }
    static struct TableIterator(IterWhat what){
        size_t position;
        size_t capacity;
        static if(what & IterWhat.KEYS)
            K[] keys;
        static if(what & IterWhat.VALUES)
            V[] values;
        ulong[] filled;
        bool
        empty(){
            return position >= capacity;
        }
        bool
        at_item(){
            size_t filled_idx = position /64;
            ulong mask = 1LU << (position % 64);
            if(filled[filled_idx] & mask)
                return true;
            return false;
        }
        void
        popFront(){
            for(position++;position<capacity;position++){
                if(at_item())
                    break;
            }
        }
        static if(what == IterWhat.KEYS){
            ref K
            front(){
                return keys[position];
            }
        }
        static if(what == IterWhat.VALUES){
            ref V
            front(){
                return values[position];
            }
        }
        static if(what == IterWhat.BOTH){
            Item!(K, V)
            front(){
                return Item!(K, V)(keys[position], values[position]);
            }
        }
    }

    TableIterator!(IterWhat.KEYS)
    keys(){
        TableIterator!(IterWhat.KEYS) result;
        result.capacity = capacity;
        result.keys = capacity?get_keys():null;
        result.filled = capacity?get_filled():null;
        if(capacity && !result.at_item)
            result.popFront();
        return result;
    }
    TableIterator!(IterWhat.VALUES)
    values(){
        TableIterator!(IterWhat.VALUES) result;
        result.capacity = capacity;
        result.values = capacity?get_values():null;
        result.filled = capacity?get_filled():null;
        if(capacity && !result.at_item)
            result.popFront();
        return result;
    }
    TableIterator!(IterWhat.BOTH)
    items(){
        TableIterator!(IterWhat.BOTH) result;
        result.capacity = capacity;
        result.keys = capacity?get_keys():null;
        result.values = capacity?get_values():null;
        result.filled = capacity?get_filled():null;
        if(capacity && !result.at_item)
            result.popFront();
        return result;
    }
    void
    extend(K[] keys, V[] values){
        assert(keys.length == values.length);
        reserve(keys.length+occupancy);
        for(size_t i = 0; i < keys.length; i++)
            *set(keys[i]) = values[i];
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
    void
    extend(KRange, VRange)(KRange krange, VRange vrange){
        while(!krange.empty){
            *set(krange.front) = vrange.front;
            krange.popFront;
            vrange.popFront;
        }
    }
}

// intersection of above two data structures
struct BTableCommon(K, V, Allocator){
    Box!(void[], Allocator) bdata;
    size_t occupancy;
    size_t capacity;
}

// A table that is flat for small sizes, then switches to a hash
// table for larger sizes.
struct BTable(K, V, Allocator, size_t small_size=64){
    union {
        BTableCommon!(K, V, Allocator) common;
        BHashlessFlatTable!(K, V, Allocator) flat;
        BHashTable!(K, V, Allocator) hashed;
    }
    static assert(this.sizeof == hashed.sizeof);
    enum SMALL_SIZE = small_size;
    static if(Allocator.state_size){
        void allocator(Allocator* a){
            common.bdata.allocator = a;
        }
        Allocator* allocator(){
            return common.bdata.allocator;
        }
    }
    void
    reserve(size_t N){
        if(common.capacity <= SMALL_SIZE){
            flat.reserve(N);
        }
        else {
            hashed.reserve(N);
        }
    }
    V*
    get(in K key){
        if(common.capacity <= SMALL_SIZE){
            return flat.get(key);
        }
        else {
            return hashed.get(key);
        }
    }
    V*
    set(in K key){
        if(common.capacity <= SMALL_SIZE){
            if(common.occupancy == common.capacity){
                if(common.occupancy*2 > SMALL_SIZE){
                    static if(!Allocator.state_size)
                        auto map = hashed.make(common.occupancy*2);
                    else
                        auto map = hashed.make(common.bdata.allocator, common.occupancy*2);
                    auto ks = flat.keys;
                    auto vs = flat.values;
                    foreach(i, k; ks){
                        map[k] = vs[i];
                    }
                    flat.cleanup;
                    hashed = map;
                    return hashed.set_unchecked(key);
                }
                else {
                    return flat.set(key);
                }
            }
            else {
                return flat.set(key);
            }
        }
        else {
            return hashed.set(key);
        }
    }
    bool
    del(in K key){
        if(common.capacity <= SMALL_SIZE){
            return flat.del(key);
        }
        else {
            return hashed.del(key);
        }
    }
    bool
    contains(in K key){
        return !!get(key);
    }

    void
    opIndexAssign(in V value, in K key){
        auto v = set(key);
        *v = cast(V)cast()value;
    }

    ref V
    opIndex(in K key){
        return *get(key);
    }

    V*
    opBinaryRight(string op)(in K key) if(op == "in"){
        return get(key);
    }

    Keys
    keys(){
        return Keys(&this);
    }

    Values
    values(){
        return Values(&this);
    }

    Items
    items(){
        return Items(&this);
    }
    static struct Keys {
        BTable!(K, V, Allocator)* table;
        int opApply(scope int delegate(ref K) dg){
            int result = 0;
            if(table.common.capacity <= table.SMALL_SIZE)
                foreach(ref k; table.flat.keys){
                    result = dg(k);
                    if(result) break;
                }
            else
                foreach(ref k; table.hashed.keys){
                    result = dg(k);
                    if(result) break;
                }
            return result;
        }
    }

    static struct Values {
        BTable!(K, V, Allocator)* table;
        int opApply(scope int delegate(ref V) dg){
            int result = 0;
            if(table.common.capacity <= table.SMALL_SIZE)
                foreach(ref v; table.flat.values){
                    result = dg(v);
                    if(result) break;
                }
            else
                foreach(ref v; table.hashed.values){
                    result = dg(v);
                    if(result) break;
                }
            return result;
        }
    }


    static struct Items {
        BTable!(K, V, Allocator)* table;
        int opApply(scope int delegate(ref Item!(K, V)) dg){
            int result = 0;
            if(table.common.capacity <= table.SMALL_SIZE)
                foreach(ref it; table.flat.items){
                    result = dg(it);
                    if(result) break;
                }
            else
                foreach(ref it; table.hashed.items){
                    result = dg(it);
                    if(result) break;
                }
            return result;
        }
    }
    void
    cleanup(){
        if(common.capacity <= SMALL_SIZE)
            flat.cleanup;
        else
            hashed.cleanup;
    }

    void
    extend(K[] keys, V[] values){
        assert(keys.length == values.length);
        reserve(keys.length+common.occupancy);
        for(size_t i = 0; i < keys.length; i++)
            *set(keys[i]) = values[i];
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
    void
    extend(KRange, VRange)(KRange krange, VRange vrange){
        while(!krange.empty){
            *set(krange.front) = vrange.front;
            krange.popFront;
            vrange.popFront;
        }
    }
    size_t
    allocated_size(){
        return common.bdata.data.length;
    }
}

// Convenience template.
struct Table(K, V){
    BTable!(K, V, Mallocator) table;
    alias table this;
}

uint
hash_fast_reduce(uint x, uint N){
    return (cast(ulong)x * cast(ulong)N) >> 32;
}
