/*
 * Copyright © 2021-2022, David Priver
 */
import std.typecons: Tuple;
@safe @nogc pure nothrow
const(char)[]
lstripped(const(char)[] str){
    for(;str.length;str = str[1..$]){
        switch(str[0]){
            case ' ': case '\t': case '\r': case '\n': case '\f': case '\v':
                continue;
            default:
                break;
        }
        break;
    }
    return str;
}

@safe @nogc pure nothrow
const(char)[]
rstripped(const(char)[] str){
    for(;str.length;str = str[0..$-1]){
        switch(str[$-1]){
            case ' ': case '\t': case '\r': case '\n': case '\f': case '\v':
                continue;
            default:
                break;
        }
        break;
    }
    return str;
}

@safe @nogc pure nothrow
const(char)[]
stripped(const(char)[]str){
    return str.rstripped.lstripped;
}

alias Split = Tuple!(const(char)[], "head", const(char)[], "tail");

@trusted @nogc pure nothrow
Split
split(const(char)[]str, char c){
    import core.stdc.string: memchr;
    auto s = cast(const char*)memchr(str.ptr, c, str.length);
    if(!s){
        return Split(str, null);
    }
    else {
        return Split(str[0..(s-str.ptr)], str[(s-str.ptr)+1..$]);
    }
}
@trusted @nogc pure nothrow
Split
stripped_split(const(char)[]str, char c){
    import core.stdc.string: memchr;
    auto s = cast(const char*)memchr(str.ptr, c, str.length);
    if(!s){
        return Split(str.stripped, null);
    }
    else {
        return Split(str[0..(s-str.ptr)].stripped, str[(s-str.ptr)+1..$].stripped);
    }
}

pure nothrow @nogc
extern(C) const(void)* memmem(const void*, size_t, const void*, size_t);

@trusted @nogc pure nothrow
Split
split(const(char)[]str, const(char)[] c){
    auto s = cast(const char*)memmem(str.ptr, str.length, c.ptr, c.length);
    if(!s){
        return Split(str, null);
    }
    else {
        return Split(str[0..(s-str.ptr)], str[(s-str.ptr)+c.length..$]);
    }
}

@trusted @nogc pure nothrow
Split
stripped_split(const(char)[]str, const(char)[] c){
    auto s = cast(const char*)memmem(str.ptr, str.length, c.ptr, c.length);
    if(!s){
        return Split(str, null);
    }
    else {
        return Split(str[0..(s-str.ptr)].stripped, str[(s-str.ptr)+c.length..$].stripped);
    }
}

@safe @nogc pure nothrow
bool
endswith(const(char)[]str, const(char)[] needle){
    if(needle.length > str.length) return false;
    if(!needle.length) return true;
    auto strtail = str[$-needle.length .. $];
    return strtail == needle;
}

@safe @nogc pure nothrow
bool
startswith(const(char)[]str, const(char)[] needle){
    if(needle.length > str.length) return false;
    if(!needle.length) return true;
    return str[0..needle.length] == needle;
}

@safe @nogc pure nothrow
struct Splitter {
    const(char)[] head;
    const(char)[] tail;
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
    const(char)[] head;
    const(char)[] tail;
    const(char)[] c;

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
split_by(const(char)[]str, const(char)[] c){
    auto s = str.split(c);
    return SplitterS(s.head, s.tail, c);
}

@safe @nogc pure nothrow
Splitter
split_by(const(char)[]str, char c){
    auto s = str.split(c);
    return Splitter(s.head, s.tail, c);
}

@trusted @nogc pure nothrow
Split
split_spaces(const(char)[]str){
    str = lstripped(str);
    if(!str.length) return Split(null, null);
    size_t i;
    for(i = 0; i < str.length; i++){
        switch(str[i]){
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
    const(char)[] head = str[0 .. i];
    const(char)[] tail = i !=str.length?str[i+1 .. $]: null;
    tail = lstripped(tail);
    if(!tail.length) tail = null;
    return Split(head, tail);
}

@safe @nogc pure nothrow
struct WhitespaceSplitter {
    const(char)[] head;
    const(char)[] tail;
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
split_by_spaces(const(char)[]str){
    auto s = str.split_spaces();
    return WhitespaceSplitter(s.head, s.tail);
}

auto strip(R)(R r){
    static struct S {
        R r;
        const(char)[] front(){
            return stripped(r.front);
        }
        bool empty(){
            return r.empty;
        }
        void popFront(){
            r.popFront;
        }
        const(char)[] next(){
            auto result = front;
            popFront;
            return result;
        }
    }
    return S(r);
}
