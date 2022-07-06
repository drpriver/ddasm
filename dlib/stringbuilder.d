/*
 * Copyright © 2021-2022, David Priver
 */
module dlib.stringbuilder;

import core.stdc.string: memcpy, memset;
import core.simd;
import ldc.simd;

import dlib.aliases;
import dlib.box: Box;
import dlib.zstring;
import dlib.fpconv_ctfe: fpconv_dtoa;
import dlib.str_util: split, Split;

static __gshared immutable hextable = {
    string[] result;
    for(size_t i = 0; i < 16; i++)
        for(size_t j = 0; j < 16; j++)
            result ~= "" ~ "0123456789abcdef"[i] ~ "0123456789abcdef"[j];
    assert(result.length == 256);
    return result;
}();
struct StringBuilder(Allocator){
    static if(Allocator.state_size){
        Allocator* allocator;
        this(Allocator* allocator){
            allocator = allocator;
            cursor = 0;
            capacity = 0;
            data = null;
        }
    }
    else {
        alias allocator = Allocator;
    }
    size_t cursor;
    size_t capacity;
    char* data;

    void
    cleanup(){
        static if(allocator.state_size) assert(allocator);
        allocator.free(data, capacity);
        data = null;
        cursor = 0;
        capacity = 0;
    }
    /// Appends a 0 beyond the end of the string,
    /// for when you need to work with C apis.
    /// The 0 is not counted as part of the length.
    void
    nul_terminate(){
        _check_remaining_size(1);
        data[cursor] = '\0';
    }
    void
    ensure_additional(size_t additional_capacity){
        _check_remaining_size(additional_capacity);
    }

    ZString
    zdetach(){
        nul_terminate;
        void[] ddata = allocator.realloc(data, capacity, cursor+1);
        data = cast(char*)ddata.ptr;
        auto result = ZString(cursor, data);
        data = null;
        capacity = 0;
        cursor = 0;
        return result;
    }

    Box!(char[], Allocator)
    detach(){
        typeof(return) result;
        static if(Allocator.state_size)
            result.allocator = allocator;
        result.data = data[0..capacity];
        result.resize(cursor);
        data = null;
        capacity = 0;
        cursor = 0;
        return result;
    }

    str
    borrow(){
        return data[0..cursor];
    }

    ZString
    zborrow(){
        nul_terminate;
        return ZString(cursor, data);
    }

    void
    reset(){
        cursor = 0;
    }

    void
    _resize(size_t size){
        size = allocator.good_size(size);
        void[] new_data = allocator.realloc(data, capacity, size);
        assert(new_data.ptr);
        assert(new_data.length == size);
        data = cast(char*)new_data.ptr;
        capacity = size;
    }
    void
    _check_remaining_size(size_t len){
        if(cursor + len > capacity){
            size_t new_size = capacity?((capacity*3)/2):16;
            while(new_size < cursor+len)
                new_size *= 2;
            if(new_size & 15)
                new_size += (16- (new_size&15));
            _resize(new_size);
        }
    }

    void
    write(str str_){
        if(!str_.length) return;
        _check_remaining_size(str_.length);
        memcpy(data+cursor, str_.ptr, str_.length);
        cursor += str_.length;
    }
    pragma(inline, true)
    void
    write(ZString  str_){
        write(str_[]);
    }

    void
    write(char c){
        _check_remaining_size(1);
        data[cursor++] = c;
    }


    void
    write(ushort value){
        write(cast(uint)value);
    }

    void
    write(ubyte value){
        write(cast(uint)value);
    }

    void
    write(short value){
        write(cast(int)value);
    }

    void
    write(byte value){
        write(cast(int)value);
    }

    void
    write(uint value){
        char[10] buff = void;
        char* p = uint32_to_str_buffer(buff.ptr, value);
        ptrdiff_t size = (buff.ptr+10) - p;
        _check_remaining_size(size);
        memcpy(data+cursor, p, size);
        cursor += size;
    }
    void
    write(ulong value){
        char[20] buff = void;
        char* p = uint64_to_str_buffer(buff.ptr, value);
        ptrdiff_t size = (buff.ptr+20) - p;
        _check_remaining_size(size);
        memcpy(data+cursor, p, size);
        cursor += size;
    }

    void
    write(int value){
        if(value == int.min){
            write("-2147483648");
            return;
        }
        char[10] buff = void;
        if(value < 0){
            write('-');
            value = -value;
        }
        char* p = uint32_to_str_buffer(buff.ptr, value);
        ptrdiff_t size = (buff.ptr+10) - p;
        _check_remaining_size(size);
        memcpy(data+cursor, p, size);
        cursor += size;
    }

    void
    write(long value){
        if(value == long.min){
            write("-9223372036854775808");
            return;
        }
        if(value < 0){
            write('-');
            value = -value;
        }
        char[20] buff = void;
        char* p = uint64_to_str_buffer(buff.ptr, value);
        ptrdiff_t size = (buff.ptr+20) - p;
        _check_remaining_size(size);
        memcpy(data+cursor, p, size);
        cursor += size;
    }

    void
    write(double value){
        char[24] buff = void;
        int n = fpconv_dtoa(value, buff.ptr);
        write(buff[0..n]);
    }

    void
    write(T)(Quoted!T q){
        write(q.quote_char);
        write(q.val);
        write(q.quote_char);
    }

    void
    write(T)(Hex!T h){
        hex("0x", h.val);
    }

    void
    write(T)(PHex!T p){
        hex("0p", p.val);
    }

    void
    write_nchar(char c, size_t n){
        if(!n) return;
        _check_remaining_size(n);
        memset(data+cursor, c, n);
        cursor += n;
    }
    void
    erase(size_t len){
        if(len > cursor){
            cursor = 0;
            data[0] = '\0';
            return;
        }
        cursor -= len;
        data[cursor] = '\0';
    }
    void
    write(T)(T value) if(__traits(hasMember, T, "write_to")){
        value.write_to(this);
    }
    pragma(inline, true)
    void
    FORMAT(A...)(A args){
        foreach(t; args)
            this.write(t);
    }

    void
    writef(A...)(str fmt, A args){
        foreach(a; args){
            assert(fmt.length);
            Split s = fmt.split('%');
            if(s.head.length)
                write(s.head);
            fmt = s.tail;
            write(a);
        }
        if(fmt.length) write(fmt);
    }

    void
    hex(string prefix, ulong value){
        write(prefix);
        if(!value){
            write('0');
            return;
        }
        ubyte[8] b = void;
        memcpy(b.ptr, &value, 8);
        int i = 7;
        while(b[i] == 0){
            i--;
        }
        for(int z = i;z >= 0; z--){
            write(hextable[b[z]]);
        }
    }

    void
    escaped(str str_){
        ubyte16 controls = ubyte16(31);
        ubyte16 controls2 = ubyte16(127);
        while(str_.length >= 16){
            ubyte16 b16 = cast(ubyte16)str_[0..16];
            ubyte16 cmp = cast(ubyte16)greaterOrEqualMask!(ubyte16)(controls, b16);
            ubyte16 cmp2 = cast(ubyte16)greaterOrEqualMask!(ubyte16)(b16, controls2);
            ubyte16 combo = cmp | cmp2;
            if(combo != 0){
                for(size_t i = 0; i < 16; i++){
                    char c = str_[i];
                    switch(cast(ubyte)c){
                        case '\t'   : write("\\t"); break;
                        case '\r'   : write("\\r"); break;
                        case '\033' : write("\\e"); break;
                        case '\n'   : write("\\n"); break;
                        case '\f'   : write("\\f"); break;
                        case '\\'   : write("\\\\"); break;
                        case '\''   : write("\\'"); break;
                        case '"'    : write("\\\""); break;
                        case 0: .. case 8: case 14: .. case 26: case 28:  .. case 31: case 127: .. case 255:
                            write("\\x");
                            write(hextable[c]);
                            break;
                        default:
                            write(c);
                            break;
                    }
                }
            }
            else {
                write(str_[0..16]);
            }
            str_ = str_[16..$];

        }
        for(size_t i = 0; i < str_.length; i++){
            char c = str_[i];
            switch(cast(ubyte)c){
                case '\t'   : write("\\t"); break;
                case '\r'   : write("\\r"); break;
                case '\033' : write("\\e"); break;
                case '\n'   : write("\\n"); break;
                case '\f'   : write("\\f"); break;
                case '\\'   : write("\\\\"); break;
                case '\''   : write("\\'"); break;
                case '"'    : write("\\\""); break;
                case 0: .. case 8: case 14: .. case 26: case 28:  .. case 31: case 127: .. case 255:
                    write("\\x");
                    write(hextable[c]);
                    break;
                default:
                    write(c);
                    break;
            }
        }
    }

    void write(T)(Escaped!T e){
        escaped(e.val);
    }
    static if(Allocator.state_size){
        static
        Box!(char[], Allocator)
        mwrite(R...)(Allocator* a, R args){
            StringBuilder sb;
            sb.allocator = a;
            foreach(a; args)
                sb.write(a);
            return sb.detach;
        }
        static
        Box!(char[], Allocator)
        mwritef(R...)(Allocator* a, R args){
            StringBuilder sb;
            sb.allocator = a;
            sb.writef(args);
            return sb.detach;
        }
    }
    else {
        static
        Box!(char[], Allocator)
        mwrite(R...)(R args){
            StringBuilder sb;
            foreach(a; args)
                sb.write(a);
            return sb.detach;
        }

        static
        Box!(char[], Allocator)
        mwritef(R...)(R args){
            StringBuilder sb;
            sb.writef(args);
            return sb.detach;
        }
    }
}

Box!(char[], Allocator)
mwritef(Allocator, R...)(R args)if(!Allocator.state_size){
    return StringBuilder!(Allocator).mwritef(args);
}
Box!(char[], Allocator)
mwritef(Allocator, R...)(Allocator* a, R args)if(Allocator.state_size){
    return StringBuilder!(Allocator).mwritef(a, args);
}

struct Quoted(T){
    T val;
    char quote_char = '\'';
}

Quoted!T
Q(T)(T val, char quote_char = '\''){
    return Quoted!T(val, quote_char);
}

struct Hex(T){
    T val;
}
struct PHex(T){
    T val;
}

Hex!T
H(T)(T val){
    return Hex!T(val);
}
PHex!T
P(T)(T val){
    return PHex!T(val);
}

struct
Escaped(T){
    T val;
}

Escaped!T
E(T)(T val){
    return Escaped!T(val);
}



static immutable ushort[100] ZERO_TO_NINETY_NINE = [
    0x3030, 0x3130, 0x3230, 0x3330, 0x3430, 0x3530, 0x3630, 0x3730, 0x3830, 0x3930,
    0x3031, 0x3131, 0x3231, 0x3331, 0x3431, 0x3531, 0x3631, 0x3731, 0x3831, 0x3931,
    0x3032, 0x3132, 0x3232, 0x3332, 0x3432, 0x3532, 0x3632, 0x3732, 0x3832, 0x3932,
    0x3033, 0x3133, 0x3233, 0x3333, 0x3433, 0x3533, 0x3633, 0x3733, 0x3833, 0x3933,
    0x3034, 0x3134, 0x3234, 0x3334, 0x3434, 0x3534, 0x3634, 0x3734, 0x3834, 0x3934,
    0x3035, 0x3135, 0x3235, 0x3335, 0x3435, 0x3535, 0x3635, 0x3735, 0x3835, 0x3935,
    0x3036, 0x3136, 0x3236, 0x3336, 0x3436, 0x3536, 0x3636, 0x3736, 0x3836, 0x3936,
    0x3037, 0x3137, 0x3237, 0x3337, 0x3437, 0x3537, 0x3637, 0x3737, 0x3837, 0x3937,
    0x3038, 0x3138, 0x3238, 0x3338, 0x3438, 0x3538, 0x3638, 0x3738, 0x3838, 0x3938,
    0x3039, 0x3139, 0x3239, 0x3339, 0x3439, 0x3539, 0x3639, 0x3739, 0x3839, 0x3939,
];
static assert(ZERO_TO_NINETY_NINE.sizeof ==200);

//
// buff: A pointer to a buffer that is at least 10 bytes long.
// value: the value to be turned into a string.
//
// Returns: A pointer into the buffer that is the first character of the string.
// Note that this is not necessarily the first character of the buffer.
// You can get the length of the written string via pointer arithmetic:
//
// Example:
//   buff[10]; // note that a length of 10 leaves no room for a nul.
//   char* p = uint32_to_str_buffer(buff, some_unsigned_integer);
//   ptrdiff_t length = (buff+10) - p; // always buff+10, even if buff is longer.
//
char*
uint32_to_str_buffer(char* buff, uint value){
    // UINT32_MAX: 4294967295 (10 characters)
    char *p = buff+10; // 1 past the end
    // Loop over the traditional naive way, but write two characters at a time.
    // Write back to front, in a single pass.
    while(value >= 100){
        uint old = value;
        p -= 2;
        // any compiler worth its salt should optimize this to a mul + shift
        value /= 100;
        uint last_two_digits = old - 100*value; // Will always be in range of [00, 99]
        memcpy(p, &ZERO_TO_NINETY_NINE[last_two_digits], ushort.sizeof);
    }
    p -= 2;
    // Value is < 100 at this point.
    memcpy(p, &ZERO_TO_NINETY_NINE[value], ushort.sizeof);
    // If value < 10, then we ended up writing an extra leading 0.
    // So, add one if less than 10.
    // Also note, that in the exact case that value == 0, we write 00
    // and then return a pointer to the second 0.
    return p+(value < 10);
}

// Just do the same for u64
// There might be a faster way to do this? It kind of depends on your prior for
// what the ranges of values are.
//
// The math is:
//   ptrdiff_t length = (buff+20) - p for this, as UINT64_MAX is 20 characters.

//
// buff: A pointer to a buffer that is at least 20 bytes long.
// value: the value to be turned into a string.
//
// Returns: A pointer into the buffer that is the first character of the string.
// Note that this is not necessarily the first character of the buffer.
// You can get the length of the written string via pointer arithmetic:
//
char*
uint64_to_str_buffer(char*  buff, ulong value){
    // UINT64_MAX: 18446744073709551615 (20 characters)
    char *p = buff+20; // 1 past the end
    // Loop over the traditional naive way, but write two characters at a time.
    // Write back to front, in a single pass.
    while(value >= 100){
        ulong old = value;
        p -= 2;
        value /= 100;
        ulong last_two_digits = old - 100*value; // Will always be in range of [00, 99]
        memcpy(p, &ZERO_TO_NINETY_NINE[last_two_digits], ushort.sizeof);
    }
    p -= 2;
    // Value is < 100 at this point.
    memcpy(p, &ZERO_TO_NINETY_NINE[value], ushort.sizeof);
    // If value < 10, then we ended up writing an extra leading 0.
    // So, add one if less than 10.
    // Also note, that in the exact case that value == 0, we write 00
    // and then return pointer to the second 0.
    return p+(value < 10);
}

