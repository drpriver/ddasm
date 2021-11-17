import core.stdc.string: memchr;
const(char)[]
lstripped_view(const(char)[] str){
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

const(char)[]
rstripped_view(const(char)[] str){
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

const(char)[]
stripped_view(const(char)[]str){
    return lstripped_view(rstripped_view(str));
}

struct Split {
    const(char)[] head;
    const(char)[] tail;
}

Split
split(const(char)[]str, char c){
    auto s = cast(const char*)memchr(str.ptr, c, str.length);
    if(!s){
        return Split(str, null);
    }
    else {
        return Split(str[0..(s-str.ptr)], str[(s-str.ptr)+1..$]);
    }
}
