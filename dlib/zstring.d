/*
 * Copyright © 2021-2022, David Priver
 */
// A view of a string that is guaranteed to be nul-terminated.

struct ZString {
    // this struct puns with const(char)[]
    size_t length; // excludes the nul-terminator;
    const(char)* ptr;

    size_t
    mem_size(){
        return ptr? length+1 : 0;
    }

    bool
    opEquals(const(char)[] other){
        return ptr[0 .. length] == other;
    }
    bool
    opEquals(in ZString other){
        return ptr[0 .. length] == other.ptr[0 .. other.length];
    }

    const(char)[]
    opIndex(){ return ptr[0 .. length]; }

    char
    opIndex(size_t i){ return ptr[i]; }

    size_t[2]
    opSlice(size_t start, size_t end){
        size_t[2] result = [start, end];
        return result;
    }

    const(char)[]
    opIndex(size_t[2] slice){ return ptr[slice[0] .. slice[1]]; }


    size_t
    opDollar(){ return length; }

    this(const(char)[] str){
        assert(str[$-1] == 0);
        ptr = str.ptr;
        length = str.length-1;
    }
    this(size_t length_, const(char)*str_){
        length = length_;
        ptr = str_;
    }
    // I can't figure out a way to say "this has to be a string literal"
    static
    typeof(this)
    literal(string lit){
        auto result = typeof(this)(lit.length, lit.ptr);
        assert(lit.ptr[lit.length] == 0);
        return result;
    }

}
