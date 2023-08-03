/*
 * Copyright © 2021-2023, David Priver
 */
module dlib.str_util;
import std.typecons: Tuple;
import dlib.aliases;
@safe @nogc pure nothrow
str
lstripped(str  str_){
    for(; str_.length; str_ =  str_[1..$]){
        switch( str_[0]){
            case ' ': case '\t': case '\r': case '\n': case '\f': case '\v':
                continue;
            default:
                break;
        }
        break;
    }
    return  str_;
}

@safe @nogc pure nothrow
str
rstripped(str  str_){
    for(; str_.length; str_ =  str_[0..$-1]){
        switch( str_[$-1]){
            case ' ': case '\t': case '\r': case '\n': case '\f': case '\v':
                continue;
            default:
                break;
        }
        break;
    }
    return  str_;
}

@safe @nogc pure nothrow
str
stripped(str str_){
    return  str_.rstripped.lstripped;
}

alias Split = Tuple!(str, "head", str, "tail");

@trusted @nogc pure nothrow
Split
split(str str_, char c){
    import core.stdc.string: memchr;
    auto s = cast(const char*)memchr( str_.ptr, c,  str_.length);
    if(!s){
        return Split( str_, null);
    }
    else {
        return Split( str_[0..(s- str_.ptr)],  str_[(s- str_.ptr)+1..$]);
    }
}
@trusted @nogc pure nothrow
Split
stripped_split(str str_, char c){
    import core.stdc.string: memchr;
    auto s = cast(const char*)memchr( str_.ptr, c,  str_.length);
    if(!s){
        return Split( str_.stripped, null);
    }
    else {
        return Split( str_[0..(s- str_.ptr)].stripped,  str_[(s- str_.ptr)+1..$].stripped);
    }
}

pure nothrow @nogc
extern(C) const(void)* memmem(const void*, size_t, const void*, size_t);

@trusted @nogc pure nothrow
Split
split(str str_, str c){
    auto s = cast(const char*)memmem( str_.ptr,  str_.length, c.ptr, c.length);
    if(!s){
        return Split( str_, null);
    }
    else {
        return Split( str_[0..(s- str_.ptr)],  str_[(s- str_.ptr)+c.length..$]);
    }
}

@trusted @nogc pure nothrow
Split
stripped_split(str str_, str c){
    auto s = cast(const char*)memmem( str_.ptr,  str_.length, c.ptr, c.length);
    if(!s){
        return Split( str_, null);
    }
    else {
        return Split( str_[0..(s- str_.ptr)].stripped,  str_[(s- str_.ptr)+c.length..$].stripped);
    }
}

@safe @nogc pure nothrow
bool
endswith(str str_, str needle){
    if(needle.length >  str_.length) return false;
    if(!needle.length) return true;
    auto strtail =  str_[$-needle.length .. $];
    return strtail == needle;
}

@safe @nogc pure nothrow
bool
startswith(str str_, str needle){
    if(needle.length >  str_.length) return false;
    if(!needle.length) return true;
    return  str_[0..needle.length] == needle;
}

@safe @nogc pure nothrow
struct Splitter {
    str head;
    str tail;
    char c;

    auto front(){return head;}
    auto popFront(){
        auto s = tail.split(c);
        head = s.head;
        tail = s.tail;
    }
    auto empty(){
        return head.ptr == null;
    }
    auto next(){
        auto result = front;
        popFront;
        return result;
    }
}
struct SplitterS {
    str head;
    str tail;
    str c;

    auto front(){return head;}
    auto popFront(){
        auto s = tail.split(c);
        head = s.head;
        tail = s.tail;
    }
    auto empty(){
        return head.ptr == null;
    }
    auto next(){
        auto result = front;
        popFront;
        return result;
    }
}

@safe @nogc pure nothrow
SplitterS
split_by(str str_, str c){
    auto s =  str_.split(c);
    return SplitterS(s.head, s.tail, c);
}

@safe @nogc pure nothrow
Splitter
split_by(str str_, char c){
    auto s =  str_.split(c);
    return Splitter(s.head, s.tail, c);
}

@trusted @nogc pure nothrow
Split
split_spaces(str str_){
     str_ = lstripped( str_);
    if(! str_.length) return Split(null, null);
    size_t i;
    for(i = 0; i <  str_.length; i++){
        switch( str_[i]){
            case ' ':
            case '\t':
            case '\r':
            case '\n':
                break;
            default:
                continue;
        }
        break;
    }
    str head =  str_[0 .. i];
    str tail = i != str_.length? str_[i+1 .. $]: null;
    tail = lstripped(tail);
    if(!tail.length) tail = null;
    return Split(head, tail);
}

@safe @nogc pure nothrow
struct WhitespaceSplitter {
    str head;
    str tail;
    auto front(){return head;}
    auto popFront(){
        auto s = tail.split_spaces();
        head = s.head;
        tail = s.tail;
    }
    auto empty(){
        return head.ptr == null;
    }
    auto next(){
        auto result = front;
        popFront;
        return result;
    }
}

@safe @nogc pure nothrow
WhitespaceSplitter
split_by_spaces(str str_){
    auto s =  str_.split_spaces();
    return WhitespaceSplitter(s.head, s.tail);
}

auto strip(R)(R r){
    static struct S {
        R r;
        str front(){
            return stripped(r.front);
        }
        bool empty(){
            return r.empty;
        }
        void popFront(){
            r.popFront;
        }
        str next(){
            auto result = front;
            popFront;
            return result;
        }
    }
    return S(r);
}
