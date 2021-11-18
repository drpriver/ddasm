import zstring;
import core.stdc.stdlib: realloc, free, malloc, calloc;
import core.stdc.string: memcpy, memset;

version(darwin){
extern(C) size_t malloc_good_size(size_t);
// extern(C) size_t malloc_size(const void* ptr);
}
//
// Mallocator
//
struct Mallocator {
    enum state_size = 0;

    static
    void[]
    alloc(size_t size){
        void* result = .malloc(size);
        return result[0..size];
    }
    static
    void[]
    zalloc(size_t size){
        void* result = .calloc(size, 1);
        return result[0..size];
    }

    static
    void[]
    realloc(void*data, size_t orig_size, size_t new_size){
        // fprintf(stderr, "%d:realloc, sizeof(%p) = %zu\n", __LINE__, data, malloc_size(data));
        // fprintf(stderr, "%d:realloc %p, %zu, %zu\n", __LINE__, data, orig_size, new_size);
        void* result = .realloc(cast(void*)data, new_size);
        // fprintf(stderr, "%d:realloc %p\n", __LINE__, result);
        // fprintf(stderr, "%d:realloc sizeof(%p): %zu\n", __LINE__, result, malloc_size(result));
        return result[0..new_size];
    }

    static
    void
    free(const(void)* ptr, size_t size){
        .free(cast(void*)ptr);
    }

    static
    size_t
    good_size(size_t size){
        version(darwin){
            size_t result = malloc_good_size(size);
        }
        else {
            size_t result = size;
        }
        return result;
    }
    typeof(this)*
    The(){
        return &TheMallocator;
    }
}

__gshared Mallocator TheMallocator;


struct LinkAllocation {
    LinkAllocation* next;
    LinkAllocation* prev;
    size_t buffsize;
    void[0] buff;
}

//
// Linked allocator
//
// A wrapper allocator that places each allocation into a
// doubly-linked list, along with the size of the allocation.
//
// Adds 24 bytes of overhead, so use this for large allocations.
//
// As a side effect, this also helps to freeing with the wrong
// size.
//
struct LinkAllocator(Allocator){
    static if(Allocator.state_size){
        Allocator* allocator;
        size_t
        good_size(size_t size){
            return allocator.good_size(size+LinkAllocation.sizeof)-LinkAllocation.sizeof;
        }
    }
    else {
        alias allocator = Allocator;
        // good_size can be static if the allocator is stateless.
        static
        size_t
        good_size(size_t size){
            return allocator.good_size(size+LinkAllocation.sizeof)-LinkAllocation.sizeof;
        }
    }
    enum state_size = typeof(this).sizeof;
    LinkAllocation* last_allocation;

    void[]
    alloc(size_t size){
        size_t real_size = size + LinkAllocation.sizeof;
        void[] result = allocator.alloc(real_size);
        auto link = cast(LinkAllocation*)result.ptr;
        link.prev = last_allocation;
        link.next = null;
        link.buffsize = size;
        last_allocation = link;
        return link.buff.ptr[0..result.length-LinkAllocation.sizeof];
    }

    void[]
    zalloc(size_t size){
        size_t real_size = size + LinkAllocation.sizeof;
        void[] result = allocator.zalloc(real_size);
        auto link = cast(LinkAllocation*)result.ptr;
        link.prev = last_allocation;
        link.next = null;
        link.buffsize = size;
        last_allocation = link;
        return link.buff.ptr[0..result.length-LinkAllocation.sizeof];
    }

    void[]
    realloc(void *data, size_t orig_size, size_t new_size){
        if(!data || !orig_size)
            return alloc(new_size);
        if(!new_size){
            free(data, orig_size);
            return null;
        }
        auto link = (cast(LinkAllocation*)data)-1;
        assert(link.buffsize == orig_size);
        auto prev = link.prev;
        auto next = link.next;
        void[] changed = allocator.realloc(link, orig_size + LinkAllocation.sizeof, new_size+LinkAllocation.sizeof);
        void[] result = changed[LinkAllocation.sizeof .. $];
        if(changed.ptr == data)
            return result;
        if(link is last_allocation){
            last_allocation = cast(LinkAllocation*)changed.ptr;
        }
        if(prev)
            prev.next = cast(LinkAllocation*)changed.ptr;
        if(next)
            next.prev = cast(LinkAllocation*)changed.ptr;
        return result;
    }

    void
    free(const(void)*ptr, size_t size){
        if(!ptr) return;
        auto link = (cast(LinkAllocation*)ptr)-1;
        assert(link.buffsize == size);
        if(link is last_allocation){
            assert(!link.prev);
            last_allocation = link.next;
        }
        auto prev = link.prev;
        auto next = link.next;
        if(prev)
            prev.next = next;
        if(next)
            next.prev = prev;
        allocator.free(link, size + LinkAllocation.sizeof);
    }

    void
    free_all(){
        auto link = last_allocation;
        while(link){
            auto prev = link.prev;
            allocator.free(link, link.buffsize + LinkAllocation.sizeof);
            link = prev;
        }
        last_allocation = null;
    }
}

//
// Recorded Allocator
//
// This is similar to LinkAllocator, but it stores the
// allocations in a dynamic array instead of a linked list.
// This means the allocations themselves don't need to be
// intruded and you don't have to do pointer chasing.
// However, it means realloc is slower as the allocation
// has to be found in an array.
//
// This also helps diagnose double free errors.
//
struct RecordingAllocator(Allocator){
    static if(Allocator.state_size){
        Allocator* allocator;
        size_t
        good_size(size_t size){
            return allocator.good_size(size+(void[]).sizeof)-(void[]).sizeof;
        }
    }
    else {
        alias allocator = Allocator;
        // good_size can be static if the allocator is stateless.
        static
        size_t
        good_size(size_t size){
            return allocator.good_size(size+(void[]).sizeof)-(void[]).sizeof;
        }
    }
    enum state_size = typeof(this).sizeof;
    void[][] allocations;
    size_t count;

    void[]
    alloc(size_t size){
        if(count >= allocations.length){
            size_t new_length = allocations.length?allocations.length*2:8;
            void[] p = allocator.realloc(allocations.ptr, allocations.length*(void[]).sizeof, new_length*(void[]).sizeof);
            allocations = (cast(void[]*)p.ptr)[0..new_length];
        }
        void[] result = allocator.alloc(size);
        allocations[count++] = result;
        return result;
    }
    void[]
    zalloc(size_t size){
        if(count >= allocations.length){
            size_t new_length = allocations.length?allocations.length*2:8;
            void[] p = allocator.realloc(allocations.ptr, allocations.length*(void[]).sizeof, new_length*(void[]).sizeof);
            allocations = (cast(void[]*)p.ptr)[0..new_length];
        }
        void[] result = allocator.zalloc(size);
        allocations[count++] = result;
        return result;
    }
    void[]
    realloc(void*data, size_t orig_size, size_t new_size){
        if(!data || !orig_size)
            return alloc(new_size);
        if(!new_size){
            free(data, orig_size);
            return null;
        }
        assert(count);
        if(allocations[count -1].ptr == data){
            auto allocation = &allocations[count-1];
            assert(allocation.length == orig_size);
            *allocation = allocator.realloc(allocation.ptr, allocation.length, new_size);
            return *allocation;
        }
        foreach(ref allocation; allocations[0 .. count-1]){
            if(allocation.ptr == data){
                assert(allocation.length == orig_size);
                allocation = allocator.realloc(data, orig_size, new_size);
                return allocation;
            }
        }
        // double free or reallocing wild pointer?
        assert(0);
    }
    void
    free(const(void)*ptr, size_t size){
        if(!ptr) return;
        // check the last one first.
        assert(count);
        if(allocations[count - 1].ptr == ptr){
            assert(allocations[count-1].length == size);
            allocator.free(allocations[count-1].ptr, allocations[count-1].length);
            count--;
            return;
        }
        foreach(i, ref allocation; allocations[0 .. count-1]){
            if(allocation.ptr == ptr){
                assert(allocation.length == size);
                allocator.free(allocation.ptr, allocation.length);
                allocation = allocations[--count];
                return;
            }
        }
        // double free or freeing wild pointer?
        assert(0);
    }
    void
    free_all(){
        foreach(allocation; allocations[0..count]){
            allocator.free(allocation.ptr, allocation.length);
        }
        allocator.free(allocations.ptr, allocations.length*(void[]).sizeof);
        allocations = null;
        count = 0;
    }
}

//
// Arena Allocator
//
struct Arena {
    enum ARENA_PAGE_SIZE = 4096;
    enum ARENA_SIZE = ARENA_PAGE_SIZE*64;
    enum ARENA_BUFFER_SIZE = ARENA_SIZE - (void*).sizeof - size_t.sizeof;
    Arena* prev;
    size_t used;
    ubyte[ARENA_BUFFER_SIZE] buff;
}

//
// An allocator that has an inline buffer that it manages.
//
struct BufferAllocator(size_t N=4096*64){
    enum DEFAULT_N = 4096*64;
    size_t used;
    enum BUFFER_SIZE = N-size_t.sizeof;
    ubyte[BUFFER_SIZE] buff = void;
    static assert(typeof(this).sizeof == N);
    enum state_size = typeof(this).sizeof;

    static
    size_t
    good_size(size_t size){
        return round_size_up(size);
    }

    void[]
    alloc(size_t size){
        size = round_size_up(size);
        if(size + used > BUFFER_SIZE)
            return null;
        void[] result = buff.ptr[used .. size+used];
        used += size;
        return result;
    }
    void[]
    zalloc(size_t size){
        void[] result = alloc(size);
        if(result.length)
            memset(result.ptr, 0, result.length);
        return result;
    }
    void[]
    realloc(void* ptr, size_t old_size, size_t new_size){
        if(!ptr || !old_size)
            return alloc(new_size);
        if(!new_size){
            free(ptr, old_size);
            return null;
        }
        old_size = round_size_up(old_size);
        new_size = round_size_up(new_size);
        // in-place
        if(used+buff.ptr == ptr+old_size){
            if(new_size - old_size + used > BUFFER_SIZE)
                return null;
            used -= old_size;
            return ptr[0..new_size];
        }
        if(new_size + used > BUFFER_SIZE)
            return null;
        void[] result = alloc(new_size);
        if(result.length){
            if(old_size < new_size)
                memcpy(result.ptr, ptr, old_size);
            else
                memcpy(result.ptr, ptr, new_size);
        }
        return result;
    }
    void
    free(const(void)*ptr, size_t size){
        if(!ptr || !size)
            return;
        size = round_size_up(size);
        // in-place
        if(used + buff.ptr == ptr + size)
            used -= size;
    }

    void
    free_all(){
        used = 0;
    }
}

size_t
round_size_up(size_t size){
    size_t remainder = size & 7;
    if(remainder)
        size += 8 - remainder;
    return size;
}

struct ArenaAllocator(BaseAllocator){
    static if(BaseAllocator.state_size){
        BaseAllocator* base_allocator;
    }
    else {
        alias base_allocator = BaseAllocator;
    }
    enum state_size = typeof(this).sizeof;
    Arena* arena;
    LinkAllocator!BaseAllocator big_allocator;

    static
    size_t
    good_size(size_t size){
        return round_size_up(size);
    }

    void[]
    alloc(size_t size){
        size = round_size_up(size);
        if(size > Arena.ARENA_BUFFER_SIZE){
            return big_allocator.alloc(size);
        }
        if(!arena){
            auto a = cast(Arena*)base_allocator.alloc(Arena.sizeof).ptr;
            a.prev = null;
            a.used = 0;
            arena = a;
        }
        if(size > Arena.ARENA_BUFFER_SIZE - arena.used){
            auto a = cast(Arena*)base_allocator.alloc(Arena.sizeof).ptr;
            a.prev = arena;
            a.used = size;
            arena = a;
            return (cast(void*)a.buff.ptr)[0 .. size];
        }
        void[] result = (cast(void*)arena.buff.ptr+arena.used)[0..size];
        arena.used += size;
        return result;
    }

    void[]
    zalloc(size_t size){
        size = round_size_up(size);
        if(size > Arena.ARENA_BUFFER_SIZE){
            return big_allocator.zalloc(size);
        }
        if(!arena){
            auto a = cast(Arena*)base_allocator.zalloc(Arena.sizeof).ptr;
            a.prev = null;
            a.used = 0;
            arena = a;
        }
        if(size > Arena.ARENA_BUFFER_SIZE - arena.used){
            auto a = cast(Arena*)base_allocator.zalloc(Arena.sizeof).ptr;
            a.prev = arena;
            a.used = size;
            arena = a;
            return (cast(void*)a.buff.ptr)[0 .. size];
        }
        void[] result = (cast(void*)arena.buff.ptr+arena.used)[0..size];
        arena.used += size;
        memset(result.ptr, 0, result.length);
        return result;
    }

    void[]
    realloc(void* ptr, size_t old_size, size_t new_size){
        if(!old_size || !ptr)
            return alloc(new_size);
        if(!new_size){
            free(ptr, old_size);
            return null;
        }
        old_size = round_size_up(old_size);
        new_size = round_size_up(new_size);
        if(new_size > Arena.ARENA_BUFFER_SIZE && old_size > Arena.ARENA_BUFFER_SIZE){
            return big_allocator.realloc(ptr, old_size, new_size);
        }
        if(old_size > Arena.ARENA_BUFFER_SIZE){
            void[] result = alloc(new_size);
            if(old_size < new_size)
                memcpy(result.ptr, ptr, old_size);
            else
                memcpy(result.ptr, ptr, new_size);
            big_allocator.free(ptr, old_size);
            return result;
        }
        assert(arena);
        // 'free' the old allocation.
        if(arena.used + arena.buff.ptr == ptr + old_size)
            arena.used -= old_size;
        if(new_size > Arena.ARENA_BUFFER_SIZE){
            void[] result = big_allocator.alloc(new_size);
            if(old_size < new_size)
                memcpy(result.ptr, ptr, old_size);
            else
                memcpy(result.ptr, ptr, new_size);
            return result;
        }

        void[] result = alloc(new_size);
        if(arena.used + arena.buff.ptr == result.ptr){
            // allocation was in-place.
        }
        else {
            if(old_size < new_size)
                memcpy(result.ptr, ptr, old_size);
            else
                memcpy(result.ptr, ptr, new_size);
        }
        return result;
    }

    void
    free(const(void)*ptr, size_t size){
        if(!ptr || !size)
            return;
        size = round_size_up(size);
        if(size > Arena.ARENA_BUFFER_SIZE)
            return big_allocator.free(ptr, size);
        assert(arena);
        if(arena.used + arena.buff.ptr == ptr + size)
            arena.used -= size;
    }

    void
    free_all(){
        Arena* a = arena;
        while(a){
            Arena* to_free = a;
            a = a.prev;
            base_allocator.free(to_free, Arena.sizeof);
        }
        arena = null;
        big_allocator.free_all();
    }
}


enum AllocatorType {
    UNSET = 0,
    MALLOCATOR = 1,
    ARENA_MALLOCATOR = 2,
    RECORDED_MALLOCATOR = 3,
}
struct IAllocator {
    AllocatorType type;
    void* data;
    enum state_size = IAllocator.sizeof;
    alias ArenaT = ArenaAllocator!Mallocator;
    alias RecordedT = RecordingAllocator!Mallocator;
    this(Allocator)(Allocator* a){
    with(AllocatorType){
        static if(is(Allocator == ArenaT)){
            type = ARENA_MALLOCATOR;
            data = a;
        }
        else static if(is(Allocator == RecordedT)){
            type = RECORDED_MALLOCATOR;
            data = a;
        }
        else static if(is(Allocator == Mallocator)){
            type = MALLOCATOR;
            data = null;
        }
        else
            static assert(0, "Unhandled type for IAllocator");
    }
    }
    size_t
    good_size(size_t size){
        final switch(type) with(AllocatorType){
            case UNSET:
                assert(0);
            case MALLOCATOR:
                return Mallocator.good_size(size);
            case ARENA_MALLOCATOR:
                return ArenaT.good_size(size);
            case RECORDED_MALLOCATOR:
                return RecordedT.good_size(size);
        }
    }

    void[]
    alloc(size_t size){
        final switch(type) with(AllocatorType){
            case UNSET:
                assert(0);
            case MALLOCATOR:
                return Mallocator.alloc(size);
            case ARENA_MALLOCATOR:
                return (cast(ArenaT*)data).alloc(size);
            case RECORDED_MALLOCATOR:
                return (cast(RecordedT*)data).alloc(size);
        }
    }
    void[]
    zalloc(size_t size){
        final switch(type) with(AllocatorType){
            case UNSET:
                assert(0);
            case MALLOCATOR:
                return Mallocator.zalloc(size);
            case ARENA_MALLOCATOR:
                return (cast(ArenaT*)data).zalloc(size);
            case RECORDED_MALLOCATOR:
                return (cast(RecordedT*)data).zalloc(size);
        }
    }
    void[]
    realloc(void* ptr, size_t old_size, size_t new_size){
        final switch(type) with(AllocatorType){
            case UNSET:
                assert(0);
            case MALLOCATOR:
                return Mallocator.realloc(ptr, old_size, new_size);
            case ARENA_MALLOCATOR:
                return (cast(ArenaT*)data).realloc(ptr, old_size, new_size);
            case RECORDED_MALLOCATOR:
                return (cast(RecordedT*)data).realloc(ptr, old_size, new_size);
        }
    }
    void
    free(const(void)*ptr, size_t size){
        if(!ptr) return;
        final switch(type) with(AllocatorType){
            case UNSET:
                assert(0);
            case MALLOCATOR:
                return Mallocator.free(ptr, size);
            case ARENA_MALLOCATOR:
                return (cast(ArenaT*)data).free(ptr, size);
            case RECORDED_MALLOCATOR:
                return (cast(RecordedT*)data).free(ptr, size);
        }
    }

    void
    free_all(){
        final switch(type) with(AllocatorType){
            case UNSET:
                assert(0);
            case MALLOCATOR:
                assert(0);
            case ARENA_MALLOCATOR:
                return (cast(ArenaT*)data).free_all();
            case RECORDED_MALLOCATOR:
                return (cast(RecordedT*)data).free_all();
        }
    }
}

//
// The VAllocator is an escape hatch from template hell.
// It allows you to move the polymorphism from compile time to
// runtime, so you can implement APIs with a uniform interface.
//
struct VAllocator {
    void* pointer;
    static struct AllocatorVTable{
        size_t function(void*, size_t) good_size_proc;
        void[] function(void*, size_t) alloc_proc;
        void[] function(void*, size_t) zalloc_proc;
        void[] function(void*, void*, size_t, size_t) realloc_proc;
        void function(void*, const(void)*, size_t) free_proc;
        void function(void*) free_all_proc;
    }
    immutable(AllocatorVTable)* vtable;
    enum state_size = typeof(this).sizeof;
    size_t
    good_size(size_t size){
        return vtable.good_size_proc(pointer, size);
    }
    void[] alloc(size_t size){
        return vtable.alloc_proc(pointer, size);
    }
    void[] zalloc(size_t size){
        return vtable.zalloc_proc(pointer, size);
    }
    void[] realloc(void* ptr, size_t old, size_t new_){
        return vtable.realloc_proc(pointer, ptr, old, new_);
    }
    void free(const(void)*ptr, size_t size){
        return vtable.free_proc(pointer, ptr, size);
    }
    void free_all(){
        assert(vtable.free_all_proc);
        return vtable.free_all_proc(pointer);
    }
    static
    VAllocator
    from(Allocator)(Allocator*a) if(Allocator.state_size){
        alias A = Allocator;
        static if(__traits(hasMember, A, "free_all"))
            static immutable void function(void*) free_all_proc = (void*p){ (cast(A*)p).free_all();};
        else
            static immutable void function(void*) free_all_proc = null;
        static immutable AllocatorVTable vt = {
            good_size_proc:(void*p, size_t s){return (cast(A*)p).good_size(s);},
            alloc_proc:(void*p, size_t s){return (cast(A*)p).alloc(s);},
            zalloc_proc:(void*p, size_t s){return (cast(A*)p).zalloc(s);},
            realloc_proc:(void*p, void* r, size_t o, size_t n){return (cast(A*)p).realloc(r, o, n);},
            free_proc:(void*p, const(void)*f, size_t s){return (cast(A*)p).free(f, s);},
            free_all_proc: free_all_proc,
        };
        return VAllocator(a, &vt);
    }
    static
    VAllocator
    from(Allocator)()if(!Allocator.state_size){
        alias A = Allocator;
        static if(__traits(hasMember, A, "free_all"))
            static immutable void function(void*) free_all_proc = (void*p){A.free_all();};
        else
            static immutable void function(void*) free_all_proc = null;
        static immutable AllocatorVTable vt = {
            good_size_proc:(void*p, size_t s){return A.good_size(s);},
            alloc_proc:(void*p, size_t s){return A.alloc(s);},
            zalloc_proc:(void*p, size_t s){return A.zalloc(s);},
            realloc_proc:(void*p, void* r, size_t o, size_t n){return A.realloc(r, o, n);},
            free_proc:(void*p, const(void)*f, size_t s){return A.free(f, s);},
            free_all_proc: free_all_proc,
        };
        return VAllocator(null, &vt);
    }
}
// VAllocator, but the wrapped Allocator is stateless.
struct VAllocator0 {

    static struct AllocatorVTable{
        size_t function(size_t) good_size_proc;
        void[] function(size_t) alloc_proc;
        void[] function(size_t) zalloc_proc;
        void[] function(void*, size_t, size_t) realloc_proc;
        void function(const(void)*, size_t) free_proc;
        void function() free_all_proc;
    }
    immutable(AllocatorVTable)* vtable;
    enum state_size = typeof(this).sizeof;
    size_t
    good_size(size_t size){
        return vtable.good_size_proc(size);
    }
    void[] alloc(size_t size){
        return vtable.alloc_proc(size);
    }
    void[] zalloc(size_t size){
        return vtable.zalloc_proc(size);
    }
    void[] realloc(void* ptr, size_t old, size_t new_){
        return vtable.realloc_proc(ptr, old, new_);
    }
    void free(const(void)*ptr, size_t size){
        return vtable.free_proc(ptr, size);
    }
    void free_all(){
        assert(vtable.free_all_proc);
        return vtable.free_all_proc();
    }
    static
    VAllocator0
    from(Allocator)()if(!Allocator.state_size){
        alias A = Allocator;
        static if(__traits(hasMember, A, "free_all"))
            static immutable void function() free_all_proc = &A.free_all;
        else
            static immutable void function() free_all_proc = null;
        static immutable AllocatorVTable vt = {
            good_size_proc:&A.good_size,
            alloc_proc:&A.alloc,
            zalloc_proc:&A.zalloc,
            realloc_proc:&A.realloc,
            free_proc:&A.free,
            free_all_proc: free_all_proc,
        };
        return VAllocator0(&vt);
    }
}

struct RcAllocator(Allocator){
    static if(Allocator.state_size){
        Allocator* allocator;
        size_t
        good_size(size_t size){
            return allocator.good_size(size+RcAllocation.sizeof)-RcAllocation.sizeof;
        }
        enum state_size = typeof(this).sizeof;
    }
    else {
        alias allocator = Allocator;
        // good_size can be static if the allocator is stateless.
        static
        size_t
        good_size(size_t size){
            return allocator.good_size(size+RcAllocation.sizeof)-RcAllocation.sizeof;
        }
        enum state_size = 0;
    }
    static
    void[]
    alloc(size_t size){
        void[] buff = allocator.alloc(size+RcAllocation.sizeof);
        void[] result = buff[RcAllocation.sizeof .. $];
        auto rc = cast(RcAllocation*)buff.ptr;
        rc.rc_realloc = (RcAllocation**r, size_t old, size_t new_){
            *r = (cast(RcAllocation*)realloc((*r).buff.ptr, old, new_).ptr)-1;
        };
        rc.count = 1;
        rc.size = size;
        return result;
    }
    static
    void[]
    zalloc(size_t size){
        void[] buff = allocator.zalloc(size+RcAllocation.sizeof);
        void[] result = buff[RcAllocation.sizeof .. $];
        auto rc = cast(RcAllocation*)buff.ptr;
        rc.rc_realloc = (RcAllocation**r, size_t old, size_t new_){
            *r = (cast(RcAllocation*)realloc((*r).buff.ptr, old, new_).ptr)-1;
        };
        rc.count = 1;
        rc.size = size;
        return result;
    }
    static
    void[]
    realloc(void* ptr, size_t old_size, size_t new_size){
        if(!ptr || !old_size)
            return alloc(new_size);
        if(!new_size){
            free(ptr, old_size);
            return null;
        }
        auto rc = (cast(RcAllocation*)ptr)-1;
        assert(rc.size == old_size);
        if(rc.count == 1){
            rc = cast(RcAllocation*)allocator.realloc(rc, old_size+RcAllocation.sizeof, new_size+RcAllocation.sizeof).ptr;
            rc.size = new_size;
            return rc.buff.ptr[0 .. new_size];
        }
        void[] buff = alloc(new_size);
        if(old_size < new_size)
            memcpy(buff.ptr, ptr, old_size);
        else
            memcpy(buff.ptr, ptr, new_size);
        assert(rc.count);
        rc.count--;
        if(!rc.count){
            rc_free(rc);
        }
        return buff;
    }
    static
    void
    free(const(void)* ptr, size_t size){
        if(!ptr) return;
        auto rc = (cast(RcAllocation*)ptr)-1;
        assert(rc.size == size);
        assert(rc.count);
        rc.count--;
        if(!rc.count){
            rc_free(rc);
        }
    }
    static
    void
    rc_free(RcAllocation* rc){
        allocator.free(rc, rc.size+RcAllocation.sizeof);
    }
    static
    Rc!T
    rc_alloc(T)(){
        void[] buff = alloc(T.sizeof);
        return Rc!T(cast(T*)buff.ptr);
    }
    static
    Rc!(T[])
    rc_alloc(T)(size_t N){
        void[] buff = alloc(T.sizeof * N);
        auto p = (cast(T*)buff.ptr)[0..N];
        return Rc!(T[])(p);
    }
    static
    Rc!T
    rc_zalloc(T)(){
        void[] buff = zalloc(T.sizeof);
        return Rc!T(cast(T*)buff.ptr);
    }
    static
    Rc!(T[])
    rc_zalloc(T)(size_t N){
        void[] buff = zalloc(T.sizeof * N);
        auto p = (cast(T*)buff.ptr)[0..N];
        return Rc!(T[])(p);
    }
}

struct RcAllocation {
    size_t count;
    size_t size;
    void delegate(RcAllocation**, size_t, size_t) rc_realloc;
    void[0] buff;
}

struct Rc(T)if(!is(T == U[], U)){
    T* pointer;
    void
    release(){
        auto rc = (cast(RcAllocation*)pointer)-1;
        assert(rc.count);
        if(rc.count == 1){
            rc.rc_realloc(&rc, rc.size, 0);
            pointer = null;
        }
        else {
            rc.count--;
        }
    }
    void
    retain(){
        auto rc = (cast(RcAllocation*)pointer)-1;
        rc.count++;
    }
    size_t
    ref_count(){
        auto rc = (cast(RcAllocation*)pointer)-1;
        return rc.count;
    }
    size_t
    ref_size(){
        auto rc = (cast(RcAllocation*)pointer)-1;
        return rc.size;
    }
}
struct Rc(T) if(is(T == U[], U)){
    T data;
    void
    release(){
        auto rc = (cast(RcAllocation*)data.ptr)-1;
        assert(rc.count);
        if(rc.count == 1){
            rc.rc_realloc(&rc, rc.size, 0);
            data = null;
        }
        else {
            rc.count--;
        }
    }
    void
    resize(size_t N){
        auto rc = (cast(RcAllocation*)data.ptr)-1;
        assert(rc.count);
        rc.rc_realloc(&rc, rc.size, typeof(T.init.ptr[0]).sizeof*N);
        data = (cast(typeof(T.init.ptr[0])*)rc.buff.ptr)[0 .. N];
    }
    void
    retain(){
        auto rc = (cast(RcAllocation*)data.ptr)-1;
        rc.count++;
    }
    size_t
    ref_count(){
        auto rc = (cast(RcAllocation*)data.ptr)-1;
        return rc.count;
    }
    size_t
    ref_size(){
        auto rc = (cast(RcAllocation*)data.ptr)-1;
        return rc.size;
    }
}

struct LoggingAllocator(Allocator){
    import core.stdc.stdio: fprintf, stderr;
    static if(Allocator.state_size){
        Allocator* allocator;
        size_t
        good_size(size_t size){
            return allocator.good_size(size);
        }
        enum state_size = typeof(this).sizeof;

        void[]
        alloc(size_t size){
            fprintf(stderr, "alloc: %zu bytes requested\n", size);
            void[] result = allocator.alloc(size);
            fprintf(stderr, "alloc: %p (%zu bytes) returned\n", result.ptr, result.length);
            return result;
        }

        void[]
        zalloc(size_t size){
            fprintf(stderr, "zalloc: %zu bytes requested\n", size);
            void[] result = allocator.zalloc(size);
            fprintf(stderr, "zalloc: %p (%zu bytes) returned\n", result.ptr, result.length);
            return result;
        }

        void[]
        realloc(void*data, size_t orig_size, size_t new_size){
            fprintf(stderr, "realloc: %p, %zu, %zu requested\n", data, orig_size, new_size);
            void[] result = allocator.realloc(data, orig_size, new_size);
            fprintf(stderr, "realloc: %p, %zu returned\n", result.ptr, result.length);
            return result;
        }

        void
        free(const(void)* ptr, size_t size){
            fprintf(stderr, "freeing: %p, %zu\n", ptr, size);
            return allocator.free(ptr, size);
        }
        static if(__traits(hasMember, allocator, "free_all"))
            void
            free_all(){
                fprintf(stderr, "freeing all\n");
                allocator.free_all;
            }
    }
    else {
        alias allocator = Allocator;
        // good_size can be static if the allocator is stateless.
        static
        size_t
        good_size(size_t size){
            return allocator.good_size(size);
        }
        enum state_size = 0;

        static
        void[]
        alloc(size_t size){
            fprintf(stderr, "alloc: %zu bytes requested\n", size);
            void[] result = allocator.alloc(size);
            fprintf(stderr, "alloc: %p (%zu bytes) returned\n", result.ptr, result.length);
            return result;
        }

        static
        void[]
        zalloc(size_t size){
            fprintf(stderr, "zalloc: %zu bytes requested\n", size);
            void[] result = allocator.zalloc(size);
            fprintf(stderr, "zalloc: %p (%zu bytes) returned\n", result.ptr, result.length);
            return result;
        }

        static
        void[]
        realloc(void*data, size_t orig_size, size_t new_size){
            fprintf(stderr, "realloc: %p, %zu, %zu requested\n", data, orig_size, new_size);
            void[] result = allocator.realloc(data, orig_size, new_size);
            fprintf(stderr, "realloc: %p, %zu returned\n", result.ptr, result.length);
            return result;
        }

        static
        void
        free(const(void)* ptr, size_t size){
            fprintf(stderr, "freeing: %p, %zu\n", ptr, size);
            return allocator.free(ptr, size);
        }
        static if(__traits(hasMember, allocator, "free_all"))
            static
            void
            free_all(){
                fprintf(stderr, "freeing all\n");
                allocator.free_all;
            }
    }

}

//
// Turns a stateful allocator into a
// "stateless" allocator by making it a
// global.
//
// Useful for things like Box, which can
// save a pointer this way.
struct GlobalAllocator(alias a){
    // pointers are scalars. Idk if there is a better way.
    static if(__traits(isScalar, typeof(a)))
        static auto The(){
            return a;
        }
    else
        static auto The(){
            return &a;
        }
    enum state_size = 0;
    static if(__traits(hasMember, a, "free_all"))
        static
        void
        free_all(){
            The.free_all;
        }
    static
    void[]
    alloc(size_t size){
        return The.alloc(size);
    }
    static
    void[]
    zalloc(size_t size){
        return The.zalloc(size);
    }

    static
    void[]
    realloc(void*data, size_t orig_size, size_t new_size){
        return The.realloc(data, orig_size, new_size);
    }

    static
    void
    free(const(void)* ptr, size_t size){
        return The.free(ptr, size);
    }

    static
    size_t
    good_size(size_t size){
        return The.good_size(size);
    }

}

// I don't remember the point of this?
struct OneAllocator(Allocator)if(!Allocator.state_size){
    enum state_size = 1;
    static void[] alloc(size_t size){ return Allocator.alloc(size);}
    static void[] zalloc(size_t size){ return Allocator.zalloc(size);}
    static void[] realloc(void*data, size_t orig_size, size_t new_size){ return Allocator.realloc(data, orig_size, new_size);}
    static void free(const(void)* ptr, size_t size){ return Allocator.free(ptr, size);}
    static size_t good_size(size_t size){ return Allocator.good_size(size);}
    static if(__traits(hasMember, Allocator, "free_all"))
        static void free_all(){Allocator.free_all();}
}
