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
    FIXED  = 5,
}
struct Allocator {
    // Bit-pack the pointer + kind into a single word.
    // Allocators must be 8-byte aligned.
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
            case AllocatorKind.FIXED:  return (cast(FixedAllocator*)p).alloc(size);
        }
    }

    T[] alloc(T)(size_t count){ return cast(T[])alloc(count * T.sizeof); }

    void[]
    zalloc(size_t size){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return null;
            case AllocatorKind.MALLOC: return Mallocator.zalloc(size);
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).zalloc(size);
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).zalloc(size);
            case AllocatorKind.FIXED:  return (cast(FixedAllocator*)p).zalloc(size);
        }
    }

    T[] zalloc(T)(size_t count){return cast(T[])zalloc(count * T.sizeof);}

    void[]
    realloc(void*data, size_t old_size, size_t new_size){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return null;
            case AllocatorKind.MALLOC: return Mallocator.realloc(data, old_size, new_size);
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).realloc(data, old_size, new_size);
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).realloc(data, old_size, new_size);
            case AllocatorKind.FIXED:  return (cast(FixedAllocator*)p).realloc(data, old_size, new_size);
        }
    }

    T[] realloc(T)(T[] data, size_t new_count){
        return realloc(data, new_count * T.sizeof);
    }

    void
    free(const(void)* ptr, size_t size){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return;
            case AllocatorKind.MALLOC: return Mallocator.free(ptr, size);
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).free(ptr, size);
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).free(ptr, size);
            case AllocatorKind.FIXED:  return (cast(FixedAllocator*)p).free(ptr, size);
        }
    }

    void
    free(const(void)[] data){
        return free(data.ptr, data.length);
    }

    void free_all(){ 
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return;
            case AllocatorKind.MALLOC: return;
            case AllocatorKind.LINKED: return (cast(LinkedAllocator*)p).free_all();
            case AllocatorKind.ARENA:  return (cast(ArenaAllocator*)p).free_all();
            case AllocatorKind.FIXED:  return (cast(FixedAllocator*)p).free_all();
        }
    }
    bool supports_free_all(){
        final switch(kind){
            case AllocatorKind.UNSET:  assert(0);
            case AllocatorKind.NULL:   return false;
            case AllocatorKind.MALLOC: return false;
            case AllocatorKind.LINKED: return true;
            case AllocatorKind.ARENA:  return true;
            case AllocatorKind.FIXED:  return true;
        }
    }
    Allocator allocator() { return this; }
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
    realloc(void*data, size_t old_size, size_t new_size){
        if(!data || !old_size)
            return alloc(new_size);
        if(!new_size){
            free(data, old_size);
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
    realloc(void *data, size_t old_size, size_t new_size){
        if(!data || !old_size)
            return alloc(new_size);
        if(!new_size){
            free(data, old_size);
            return null;
        }
        LinkAllocation* l = (cast(LinkAllocation*)data)-1;
        unlink(l);
        void[] result = base_allocator.realloc(l, old_size + LinkAllocation.sizeof, new_size+LinkAllocation.sizeof);
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
    _alloc(size_t size, bool zero){
        size = round_size_up(size);
        if(size > Arena.ARENA_BUFFER_SIZE)
            return zero?base_allocator.zalloc(size):base_allocator.alloc(size);
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
        if(zero) memset(result.ptr, 0, result.length);
        return result;
    }

    void[]
    alloc(size_t size){
        return _alloc(size, zero:false);
    }

    void[]
    zalloc(size_t size){
        return _alloc(size, zero:true);
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


struct FixedAllocator {
    ubyte[] buffer;
    size_t cursor = 0;

    static
    typeof(this) 
    fixed(size_t size)(void* p = imported!"core.stdc.stdlib".alloca(size)){
        return typeof(this)(p[0..size]);
    }
    this(void[] p){
        buffer = cast(ubyte[])p;
    }

    Allocator allocator(){
        return Allocator(kind: AllocatorKind.FIXED, &this);
    }

    void[] alloc(size_t size){
        if(cursor + size > buffer.length) return null;
        void[] result = buffer[cursor .. cursor + size];
        cursor += size;
        return result;
    }
    void[] zalloc(size_t size){
        void[] result = alloc(size);
        memset(result.ptr, 0, result.length);
        return result;
    }
    void[] realloc(void* data, size_t old_size, size_t new_size){
        if(old_size == new_size) return data[0..new_size];
        if(!old_size)
            return alloc(new_size);
        if(!new_size){
            free(data, old_size);
            return null;
        }
        if(buffer.ptr + cursor == data + old_size){
            if(cursor + new_size - old_size > buffer.length) return null;
            cursor += new_size - old_size;
            return buffer[0..new_size];
        }
        void[] result = alloc(new_size);
        if(result is null) return null;
        if(old_size < new_size)
            memcpy(result.ptr, data, old_size);
        else
            memcpy(result.ptr, data, new_size);
        return result;
    }
    void free(const(void)* ptr, size_t size){
        if(buffer.ptr + cursor == ptr + size){
            cursor -= size;
        }
    }
    void free_all(){
        cursor = 0;
    }
}
