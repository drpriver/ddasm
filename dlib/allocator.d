/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.allocator;
import dlib.zstring;
static import core.stdc.stdlib;
import core.stdc.string: memcpy, memset;

enum AllocatorKind {
    UNSET  = 0,
    NULL   = 1,
    MALLOC = 2,
    LINKED = 3,
    ARENA  = 4,
}
struct Allocator {
    static assert(size_t.sizeof == (void*).sizeof);
    size_t _p;
    pragma(inline, true) AllocatorKind kind() const{ return cast(AllocatorKind)(_p & 0x7); }
    pragma(inline, true) void* p() const { return cast(void*)(_p & ~cast(size_t)0x7);}
    this(AllocatorKind kind, void* p=null){
        assert(!(cast(size_t)p & 0x7));
        _p = cast(size_t)kind | cast(size_t)p;
    }

    enum state_size = typeof(this).sizeof;

    void[]
    alloc(size_t size){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return null;
            case AllocatorKind.MALLOC: return Mallocator.alloc(size);
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).alloc(size);
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).alloc(size);
        }
    }
    void[]
    zalloc(size_t size){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return null;
            case AllocatorKind.MALLOC: return Mallocator.zalloc(size);
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).zalloc(size);
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).zalloc(size);
        }
    }

    void[]
    realloc(void*data, size_t orig_size, size_t new_size){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return null;
            case AllocatorKind.MALLOC: return Mallocator.realloc(data, orig_size, new_size);
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).realloc(data, orig_size, new_size);
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).realloc(data, orig_size, new_size);
        }
    }

    void
    free(const(void)* ptr, size_t size){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return;
            case AllocatorKind.MALLOC: return Mallocator.free(ptr, size);
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).free(ptr, size);
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).free(ptr, size);
        }
    }

    void free_all(){ 
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return;
            case AllocatorKind.MALLOC: return;
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).free_all();
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).free_all();
        }
    }
    bool supports_free_all(){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return false;
            case AllocatorKind.MALLOC: return false;
            case AllocatorKind.LINKED: return true;
            case AllocatorKind.ARENA:  return true;
        }
    }
}
//
// Mallocator
//
struct Mallocator {
    enum state_size = 0;

    static
    Allocator allocator(){
        return Allocator(kind: AllocatorKind.MALLOC);
    }

    static
    void[]
    alloc(size_t size){
        void* result = core.stdc.stdlib.malloc(size);
        return result[0..size];
    }
    static
    void[]
    zalloc(size_t size){
        void* result = core.stdc.stdlib.calloc(size, 1);
        return result[0..size];
    }

    static
    void[]
    realloc(void*data, size_t orig_size, size_t new_size){
        if(!data || !orig_size)
            return alloc(new_size);
        if(!new_size){
            free(data, orig_size);
            return null;
        }
        void* result = core.stdc.stdlib.realloc(cast(void*)data, new_size);
        return result[0..new_size];
    }

    static
    void
    free(const(void)* ptr, size_t size){
        core.stdc.stdlib.free(cast(void*)ptr);
    }

    static void free_all(){ }
    static bool supports_free_all(){return false;}
}
enum MALLOCATOR = Mallocator.allocator();

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
struct LinkedAllocator{
    Allocator allocator(){
        return Allocator(kind: AllocatorKind.LINKED, p:&this);
    }
    Allocator base_allocator;
    LinkAllocation* last_allocation;
    enum state_size = typeof(this).sizeof;

    void link(LinkAllocation* l){
        l.next = last_allocation;
        if(l.next) l.next.prev = l;
        l.prev = null;
        last_allocation = l;
    }

    void unlink(LinkAllocation* l){
        if(l.prev) l.prev.next = l.next;
        if(l.next) l.next.prev = l.prev;
        if(l == last_allocation){
            assert(l.prev is null);
            last_allocation = l.next;
        }
    }

    void[]
    alloc(size_t size){
        size_t real_size = size + LinkAllocation.sizeof;
        void[] result = base_allocator.alloc(real_size);
        LinkAllocation* l = cast(LinkAllocation*)result.ptr;
        l.buffsize = size;
        link(l);
        return l.buff.ptr[0..result.length-LinkAllocation.sizeof];
    }


    void[]
    zalloc(size_t size){
        size_t real_size = size + LinkAllocation.sizeof;
        void[] result = base_allocator.zalloc(real_size);
        LinkAllocation* l = cast(LinkAllocation*)result.ptr;
        l.buffsize = size;
        link(l);
        return l.buff.ptr[0..result.length-LinkAllocation.sizeof];
    }

    void[]
    realloc(void *data, size_t orig_size, size_t new_size){
        if(!data || !orig_size)
            return alloc(new_size);
        if(!new_size){
            free(data, orig_size);
            return null;
        }
        LinkAllocation* l = (cast(LinkAllocation*)data)-1;
        unlink(l);
        void[] result = base_allocator.realloc(l, orig_size + LinkAllocation.sizeof, new_size+LinkAllocation.sizeof);
        l = cast(LinkAllocation*)result.ptr;
        l.buffsize = new_size;
        link(l);
        return l.buff.ptr[0..result.length-LinkAllocation.sizeof];
    }

    void
    free(const(void)*ptr, size_t size){
        if(!ptr) return;
        LinkAllocation* l = (cast(LinkAllocation*)ptr)-1;
        assert(l.buffsize == size);
        unlink(l);
        base_allocator.free(l, size + LinkAllocation.sizeof);
    }

    void
    free_all(){
        LinkAllocation* link = last_allocation;
        if(link) assert(!link.prev);
        while(link){
            LinkAllocation* next = link.next;
            base_allocator.free(link, link.buffsize + LinkAllocation.sizeof);
            link = next;
        }
        last_allocation = null;
    }
}

size_t
round_size_up(size_t size){
    size_t remainder = size & 7;
    if(remainder)
        size += 8 - remainder;
    return size;
}

//
// Arena Allocator
//
struct Arena {
    enum ARENA_PAGE_SIZE = 4096;
    enum ARENA_SIZE = ARENA_PAGE_SIZE*64;
    enum ARENA_BUFFER_SIZE = ARENA_SIZE - (void*).sizeof - size_t.sizeof - LinkAllocation.sizeof;
    Arena* prev;
    size_t used;
    void[ARENA_BUFFER_SIZE] buff;
}
struct ArenaAllocator{
    Allocator allocator(){
        return Allocator(kind: AllocatorKind.ARENA, p:&this);
    }
    this(Allocator a){
        base_allocator.base_allocator = a;
    }
    LinkedAllocator base_allocator;
    Arena* arena;
    enum state_size = typeof(this).sizeof;

    Arena* make_arena(){
        Arena* a = cast(Arena*)base_allocator.alloc(Arena.sizeof).ptr;
        if(a){
            a.prev = arena;
            a.used = 0;
            arena = a;
        }
        return a;
    }

    void[]
    alloc(size_t size){
        size = round_size_up(size);
        if(size > Arena.ARENA_BUFFER_SIZE)
            return base_allocator.alloc(size);
        if(!arena){
            arena = make_arena();
            if(!arena) return null;
        }
        if(size > Arena.ARENA_BUFFER_SIZE - arena.used){
            Arena* a = make_arena();
            if(!a) return null;
            arena = a;
        }
        void[] result = arena.buff[arena.used..arena.used+size];
        arena.used += size;
        return result;
    }

    void[]
    zalloc(size_t size){
        size = round_size_up(size);
        if(size > Arena.ARENA_BUFFER_SIZE)
            return base_allocator.zalloc(size);
        if(!arena){
            arena = make_arena();
            if(!arena) return null;
        }
        if(size > Arena.ARENA_BUFFER_SIZE - arena.used){
            Arena* a = make_arena();
            if(!a) return null;
            arena = a;
        }
        void[] result = arena.buff[arena.used..arena.used+size];
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
            return base_allocator.realloc(ptr, old_size, new_size);
        }
        if(old_size > Arena.ARENA_BUFFER_SIZE){
            void[] result = base_allocator.alloc(new_size);
            if(old_size < new_size)
                memcpy(result.ptr, ptr, old_size);
            else
                memcpy(result.ptr, ptr, new_size);
            base_allocator.free(ptr, old_size);
            return result;
        }
        assert(arena);
        // 'free' the old allocation.
        if(arena.used + arena.buff.ptr == ptr + old_size)
            arena.used -= old_size;
        if(new_size > Arena.ARENA_BUFFER_SIZE){
            void[] result = base_allocator.alloc(new_size);
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
            return base_allocator.free(ptr, size);
        assert(arena);
        if(arena.used + arena.buff.ptr == ptr + size)
            arena.used -= size;
    }

    void
    free_all(){
        base_allocator.free_all();
        arena = null;
    }
}

//
// An allocator that has an inline buffer that it manages.
//
/+
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
+/



/+
struct RcAllocator(Allocator){
    static if(Allocator.state_size){
        Allocator allocator;
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
        Allocator allocator;
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
+/

