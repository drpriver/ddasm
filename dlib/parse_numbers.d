/*
 * Copyright © 2021-2025, David Priver
 */
module dlib.parse_numbers;
import dlib.aliases;
enum ParseNumberError: ubyte {
    NO_ERROR = 0,
    UNEXPECTED_END = 1,
    OVERFLOWED_VALUE = 2,
    INVALID_CHARACTER = 3,
}

struct IntegerResult(T) {
    T value;
    ParseNumberError errored;
    pragma(inline, true)
    T unwrap(){
        assert(!errored);
        return value;
    }
}

auto Err(T)(ParseNumberError error){
    IntegerResult!T result = {cast(T)0, error};
    return result;
}

IntegerResult!ulong
parse_uint64(str s_){
with(ParseNumberError){
    const(ubyte)[] s = cast(const(ubyte)[])s_;
    alias err = Err!ulong;
    if(!s.length) return err(UNEXPECTED_END);
    if(s[0] == '+')
        s = s[1..$];
    if(!s.length) return err(UNEXPECTED_END);
    if(s.length > 20) return err(OVERFLOWED_VALUE);
    int bad = false;
    ulong value = 0;
    foreach(val; cast(const(ubyte)[])s[0..$-1]){
        uint cval = val;
        cval -= '0';
        if(cval > 9u)
            bad = true;
        value *= 10;
        value += cval;
    }
    if(bad) return err(INVALID_CHARACTER);
    uint cval = s[$-1];
    cval -= '0';
    if(cval > 9u) return err(INVALID_CHARACTER);
    ulong newvalue = value * 10;
    if(newvalue < value) return err(OVERFLOWED_VALUE);
    value = newvalue;
    newvalue += cval;
    if(newvalue < value) return err(OVERFLOWED_VALUE);
    return IntegerResult!ulong(newvalue, NO_ERROR);
}
}

IntegerResult!T
parse(T)(str s){
    static if(is(T == ulong)){
        return parse_uint64(s);
    }
    static if(is(T == long)){
        // FIXME: I don't check all error cases.
        auto pe = parse_uint64(s);
        if(pe.errored) return typeof(return)(0, pe.errored);
        if(pe.value > long.max){
            return typeof(return)(0, ParseNumberError.OVERFLOWED_VALUE);
        }
        return typeof(return)(pe.value, ParseNumberError.NO_ERROR);
    }
}

IntegerResult!ulong
parse_hex_inner(const char[] s){ with(ParseNumberError){
    if(s.length > 16)
        return IntegerResult!ulong(0LU, OVERFLOWED_VALUE);
    ulong val = 0;
    for(size_t i = 0; i < s.length; i++){
        val <<= 4;
        switch(s[i]){
            case 'a': .. case 'f':
                val += s[i]-'a'+10;
                break;
            case 'A': .. case 'F':
                val += s[i]-'A'+10;
                break;
            case '0': .. case '9':
                val += s[i]-'0';
                break;
            default:
                return IntegerResult!ulong(0LU, INVALID_CHARACTER);
        }
    }
    return IntegerResult!ulong(val, NO_ERROR);
}
}


IntegerResult!ulong
parse_hex(const char[] s){ with(ParseNumberError){
    if(s.length < 3)
        return IntegerResult!ulong(0LU, UNEXPECTED_END);
    if(s[0] != '0')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    if(s[1] != 'x' && s[1] != 'X')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    return parse_hex_inner(s[2..$]);
}}
IntegerResult!ulong
parse_binary(const char[] s){ with(ParseNumberError){
    if(s.length < 3)
        return IntegerResult!ulong(0LU, UNEXPECTED_END);
    if(s[0] != '0')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    if(s[1] != 'b' && s[1] != 'B')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    return parse_binary_inner(s[2..$]);
}}

IntegerResult!ulong
parse_binary_inner(const char[]s){with(ParseNumberError){
    ulong val = 0;
    if(!s.length)
        return IntegerResult!ulong(0, UNEXPECTED_END);
    for(size_t i = 0; i < s.length; i++){
        val <<= 1;
        switch(s[i]){
            case '0':
                continue;
            case '1':
                val |= 1LU;
                continue;
            default:
            return IntegerResult!ulong(0LU, INVALID_CHARACTER);
        }
    }
    return IntegerResult!ulong(val, NO_ERROR);
}}
IntegerResult!ulong
parse_psize(const char[] s){ with(ParseNumberError){
    if(s.length < 3)
        return IntegerResult!ulong(0LU, UNEXPECTED_END);
    if(s[0] != '0')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    if(s[1] != 'p' && s[1] != 'P')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    auto result = parse_hex_inner(s[2..$]);
    if(result.errored) return result;
    // TODO: overflow
    result.value *= size_t.sizeof;
    return result;
}}
IntegerResult!ulong
parse_strnum(str s){ with(ParseNumberError){
    import core.stdc.stdio;
    // printf("%s:%d: s = '%.*s;\n", __FUNCTION__.ptr, __LINE__, cast(int)s.length, s.ptr);
    if(s.length < 3)
        return IntegerResult!ulong(0LU, UNEXPECTED_END);
    if(s[0] != '0')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    if(s[1] != 's' && s[1] != 'S')
        return IntegerResult!ulong(0LU, INVALID_CHARACTER);
    if(s.length > 10){
        return IntegerResult!ulong(0LU, OVERFLOWED_VALUE);
    }
    ulong result = 0;
    s = s[2..$];
    for(size_t i = 0; i < s.length; i++){
        result |= (cast(ulong)s[i]) << i*8;
    }
    return IntegerResult!ulong(result, NO_ERROR);
}}
IntegerResult!ulong
parse_unsigned_human(const char[] s){ with(ParseNumberError){
    auto result = parse_hex(s);
    if(!result.errored)
        return result;
    result = parse_uint64(s);
    if(!result.errored)
        return result;
    result = parse_binary(s);
    if(!result.errored)
        return result;
    result = parse_psize(s);
    if(!result.errored)
        return result;
    result = parse_strnum(s);
    if(!result.errored)
        return result;
    return result;
}}

struct FloatResult {
    double value;
    ParseNumberError errored;
}

// Check if a string represents a float literal (has '.' or 'e/E')
// But NOT a hex literal (0x...)
bool is_float_literal(const char[] s){
    // Hex literals are not floats (even if they contain 'e')
    if(s.length >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
        return false;
    foreach(c; s){
        if(c == '.' || c == 'e' || c == 'E')
            return true;
    }
    return false;
}

// Parse a float literal
FloatResult parse_float(const char[] s){ with(ParseNumberError){
    if(!s.length) return FloatResult(0.0, UNEXPECTED_END);

    // Strip suffix (f, F, l, L)
    const(char)[] num = s;
    if(num.length > 0){
        char last = num[$-1];
        if(last == 'f' || last == 'F' || last == 'l' || last == 'L')
            num = num[0..$-1];
    }
    if(!num.length) return FloatResult(0.0, UNEXPECTED_END);

    // Parse the float manually
    double result = 0.0;
    bool negative = false;
    size_t i = 0;

    // Handle sign
    if(num[i] == '-'){
        negative = true;
        i++;
    } else if(num[i] == '+'){
        i++;
    }

    // Parse integer part
    while(i < num.length && num[i] >= '0' && num[i] <= '9'){
        result = result * 10.0 + (num[i] - '0');
        i++;
    }

    // Parse fractional part
    if(i < num.length && num[i] == '.'){
        i++;
        double fraction = 0.1;
        while(i < num.length && num[i] >= '0' && num[i] <= '9'){
            result += (num[i] - '0') * fraction;
            fraction *= 0.1;
            i++;
        }
    }

    // Parse exponent
    if(i < num.length && (num[i] == 'e' || num[i] == 'E')){
        i++;
        bool exp_negative = false;
        if(i < num.length && num[i] == '-'){
            exp_negative = true;
            i++;
        } else if(i < num.length && num[i] == '+'){
            i++;
        }
        int exp = 0;
        while(i < num.length && num[i] >= '0' && num[i] <= '9'){
            exp = exp * 10 + (num[i] - '0');
            i++;
        }
        if(exp_negative) exp = -exp;
        // Apply exponent
        if(exp > 0){
            while(exp-- > 0) result *= 10.0;
        } else {
            while(exp++ < 0) result *= 0.1;
        }
    }

    if(negative) result = -result;
    return FloatResult(result, NO_ERROR);
}}
