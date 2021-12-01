
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

@safe @nogc pure nothrow
struct Split {
    const(char)[] head;
    const(char)[] tail;
}

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

@safe @nogc pure nothrow
bool
endswith(const(char)[]str, const(char)[] tail){
    if(tail.length > str.length) return false;
    if(!tail.length) return true;
    auto strtail = str[$-tail.length .. $];
    return strtail == tail;
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
        return head.length == 0;
    }
    auto next(){
        auto result = front;
        popFront;
        return result;
    }
}

@safe @nogc pure nothrow
Splitter
split_by(const(char)[]str, char c){
    auto s = str.split(c);
    return Splitter(s.head, s.tail, c);
}
