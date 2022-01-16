/*
 * Copyright © 2021-2022, David Priver
 */
import core.stdc.stdio: printf, puts, fputs, stdout, fprintf, stderr, putchar;
import core.stdc.string: strlen;
import core.stdc.stdlib: strtof, strtod;

import dlib.parse_numbers;
import dlib.zstring;
enum ArgParseError: int {
    NO_ERROR = 0,
    CONVERSION_ERROR = 1,
    UNKNOWN_KWARG = 2,
    DUPLICATE_KWARG = 3,
    EXCESS_ARGS = 4,
    INSUFFICIENT_ARGS = 5,
    VISITED_NO_ARG_GIVEN = 6,
    INTERNAL_ERROR = 7,
}
enum ArgParseFlags: size_t {
    NONE = 0,
    UNKNOWN_KWARGS_AS_ARGS = 1 << 0,
    SKIP_EMPTY_STRINGS = 1 << 1,
    SKIP_NULL_STRINGS = 1 << 2,
}
enum ArgType: ubyte {
    INTEGER64,
    INT,
    FLAG,
    STRING,
    UINTEGER64,
    FLOAT32,
    FLOAT64,
    BITFLAG,
    ENUM,
    USER_DEFINED,
}

static immutable string[ArgType.max+1] ArgTypeNames = [
    "int64",
    "int",
    "flag",
    "string",
    "uint64",
    "float32",
    "float64",
    "flag",
    "enum",
    "user",
];

struct ArgParseEnumType {
    size_t enum_size;
    size_t enum_count;
    const(char[])* enum_names;
}

struct SimpleDest {
    void* pointer;
    union {
        const ArgParseEnumType* enum_pointer;
        ulong bitflag;
    }
}

struct UserType {
    int delegate(const(char)[]) parse_proc;
    const(char)[] type_name;
    const(char)[] default_str;
}

struct ArgParseDestination {
    ArgType type;
    union {
        SimpleDest dest;
        UserType ut;
    };
    int delegate(void*) user_append_proc;
}

ArgParseDestination
ArgUser(scope int delegate(const(char)[]) parse_proc, const(char)[] type_name, const(char)[] default_str = null){
    ArgParseDestination result;
    result.type = ArgType.USER_DEFINED;
    result.ut = UserType(parse_proc, type_name, default_str);
    return result;
}
ArgParseDestination
ARGDEST(T)(T* dest){
    static if(is(T==int))
        return ArgParseDestination(ArgType.INT, SimpleDest(dest));
    else static if(is(T==long))
        return ArgParseDestination(ArgType.INTEGER64, SimpleDest(dest));
    else static if(is(T==ulong))
        return ArgParseDestination(ArgType.UINTEGER64, SimpleDest(dest));
    else static if(is(T==float))
        return ArgParseDestination(ArgType.FLOAT32, SimpleDest(dest));
    else static if(is(T==double))
        return ArgParseDestination(ArgType.FLOAT64, SimpleDest(dest));
    else static if(is(T==bool))
        return ArgParseDestination(ArgType.FLAG, SimpleDest(dest));
    // This is improperly constrained - it doesn't reject string.
    // You shouldn't go char* -> immutable(char)*.
    else static if(is(T:const(char)[]))
        return ArgParseDestination(ArgType.STRING, SimpleDest(dest));
    else static if(is(T==ZString))
        return ArgParseDestination(ArgType.STRING, SimpleDest(dest));
    else
        static assert(0, "Illegal type for ARGDEST " ~ T.stringof);
}

ArgParseDestination
ARGDEST(T)(scope int delegate(T*) dg){
    ArgParseDestination result = void;
    static if(is(T==int))
        result = ArgParseDestination(ArgType.INT);
    else static if(is(T==long))
        result = ArgParseDestination(ArgType.INTEGER64);
    else static if(is(T==ulong))
        result = ArgParseDestination(ArgType.UINTEGER64);
    else static if(is(T==float))
        result = ArgParseDestination(ArgType.FLOAT32);
    else static if(is(T==double))
        result = ArgParseDestination(ArgType.FLOAT64);
    else static if(is(T==bool))
        result = ArgParseDestination(ArgType.FLAG);
    // This is improperly constrained - it doesn't reject string.
    // You shouldn't go char* -> immutable(char)*.
    else static if(is(T:const(char)[]))
        result = ArgParseDestination(ArgType.STRING);
    else static if(is(T==ZString))
        result = ArgParseDestination(ArgType.STRING);
    else
        static assert(0, "Illegal type for ARGDEST " ~ T.stringof);
    result.user_append_proc = cast(int delegate(void*))dg;
    return result;
}

ArgParseDestination
ArgBitFlagDest(T)(T* dest, ulong flag) if(is(T:ulong)){
    ArgParseDestination result = void;
    result.type = ArgType.BITFLAG;
    result.dest.pointer = dest;
    result.dest.bitflag = flag;
    return result;
}
ArgParseDestination
ArgEnumDest(void* dest, const ArgParseEnumType* enu){
    return ArgParseDestination(ArgType.ENUM, SimpleDest(dest, enu));
}

enum ArgToParseFlags: ulong {
    ARG_TO_PARSE_NONE = 0,
    SHOW_DEFAULT = 1 << 0,
    HIDDEN = 1 << 1,
}

struct NumRequired {
    int min = 0;
    int max = 1;
    enum UNLIMITED = -1;
}

struct ArgToParse {
    const(char)[] name;
    const(char)[] altname1;
    const(char)[] help;
    ArgParseDestination dest;
    NumRequired num = NumRequired(0, 1);
    ArgToParseFlags flags;
    // private
    int num_parsed;
    bool visited;
}

struct ArgParser {
    const(char)[] name;
    const(char)[] description;
    ArgToParse[] early_out;
    ArgToParse[] positional;
    ArgToParse[] keyword;
    ArgToParse* failed_arg_to_parse;
    const(char)[] failed_arg;
}

struct HelpState {
    int output_width;
    int lead;
    int remaining;
    void update(int n_to_print){
        if(remaining - n_to_print < 0){
            // This is a string with 80 spaces in it.
            const char* SPACES = "                                                                                ";
            printf("\n%.*s", lead, SPACES);
            remaining = output_width;
        }
        remaining -= n_to_print;
    }
}

void
print_wrapped(const(char)[] text, int columns){
    auto hs = HelpState(columns, 0, );
    hs.remaining = hs.output_width;
    for(;text.length;){
        auto tok = next_tokenize_help(text);
        text = tok.rest;
        if(tok.is_newline){
            if(hs.remaining != hs.output_width){
                putchar('\n');
                hs.remaining = hs.output_width;
            }
            continue;
        }
        hs.update(cast(int)tok.token.length);
        printf("%.*s", cast(int)tok.token.length, tok.token.ptr);
        if(hs.remaining){
            putchar(' ');
            hs.remaining--;
        }
    }
    putchar('\n');
}

void
print_argparse_help(const ArgParser* p, int columns){
    printf("%.*s: %.*s\n", cast(int)p.name.length, p.name.ptr, cast(int)p.description.length, p.description.ptr);
    puts("");
    const int printed = printf("usage: %.*s", cast(int)p.name.length, p.name.ptr);
    HelpState hs = HelpState(columns - printed, printed, 0);
    hs.remaining = hs.output_width;
    foreach(const ref ArgToParse arg; p.positional){
        if(arg.num.max != 1){
            if(arg.num.max < 0 || arg.num.max > 10){
                auto to_print = 1 + arg.name.length + 4;
                hs.update(cast(int)to_print);
                printf(" %.*s ...", cast(int)arg.name.length, arg.name.ptr);
            }
            else {
                auto to_print = 1 + arg.name.length;
                hs.update(cast(int)to_print);
                printf(" %.*s", cast(int)arg.name.length, arg.name.ptr);
                for(int i = 0 ; i < arg.num.max; i++){
                    to_print = 1 + arg.name.length+2;
                    hs.update(cast(int)to_print);
                    printf(" [%.*s]", cast(int)arg.name.length, arg.name.ptr);

                }
            }
        }
        else {
            auto to_print = 1 + arg.name.length;
            hs.update(cast(int)to_print);
            printf(" %.*s", cast(int)arg.name.length, arg.name.ptr);
            }
        }
    foreach(const ref ArgToParse arg; p.keyword){
        if(arg.flags & ArgToParseFlags.HIDDEN)
            continue;
        if(arg.dest.type == ArgType.FLAG){
            if(arg.altname1.length){
                auto to_print = " [%.*s | %.*s]".length - 8 + arg.name.length + arg.altname1.length;
                hs.update(cast(int)to_print);
                printf(" [%.*s | %.*s]", cast(int)arg.name.length, arg.name.ptr, cast(int)arg.altname1.length, arg.altname1.ptr);
            }
            else{
                auto to_print = " [%.*s]".length - 4 + arg.name.length;
                hs.update(cast(int)to_print);
                printf(" [%.*s]", cast(int)arg.name.length, arg.name.ptr);
            }
        }
        else {
            if(arg.altname1.length){
                auto tn = ArgTypeNames[arg.dest.type];
                auto to_print = " [%.*s | %.*s <%.*s>%s]".length - "%.*s%.*s%.*s%s".length + arg.name.length + arg.altname1.length + tn.length + (arg.num.max != 1?" ...".length: 0);
                hs.update(cast(int)to_print);
                printf(" [%.*s | %.*s <%.*s>%s]", cast(int)arg.name.length, arg.name.ptr, cast(int)arg.altname1.length, arg.altname1.ptr, cast(int)tn.length, tn.ptr, arg.num.max != 1?" ...".ptr:"".ptr);
            }
            else{
                auto tn = ArgTypeNames[arg.dest.type];
                auto to_print = " [%.*s <%.*s>%s]".length - 10 + arg.name.length + tn.length + (arg.num.max != 1?" ...".length:0);
                hs.update(cast(int)to_print);
                printf(" [%.*s <%.*s>%s]", cast(int)arg.name.length, arg.name.ptr, cast(int)tn.length, tn.ptr, arg.num.max != 1?" ...".ptr:"".ptr);
            }
        }
    }
    puts("\n");
    if(p.early_out.length){
        puts("Early Out Arguments:\n"
           ~ "--------------------");
    }
    foreach(ref early; p.early_out){
        if(early.altname1.length){
            printf("%.*s, %.*s:", cast(int)early.name.length, early.name.ptr, cast(int)early.altname1.length, early.altname1.ptr);
        }
        else{
            printf("%.*s:", cast(int)early.name.length, early.name.ptr);
        }
        print_wrapped_help(early.help, columns);
        putchar('\n');
    }
    if(p.positional.length){
        puts("Positional Arguments:\n"
           ~ "---------------------");
        foreach(ref arg; p.positional){
            print_arg_help(&arg, columns);
            putchar('\n');
        }
    }
    // It's possible for all keyword arguments to be hidden,
    // so only print the header until we hit a non-hidden argument.
    bool printed_keyword_header = false;
    foreach(ref arg; p.keyword){
        if(arg.flags & ArgToParseFlags.HIDDEN)
            continue;
        if(!printed_keyword_header){
            printed_keyword_header = true;
            fputs("Keyword Arguments:\n"
               ~ "------------------", stdout);
        }
        putchar('\n');
        print_arg_help(&arg, columns);
    }
}

void
print_wrapped_help(const(char)[] help, int columns){
    if(!help.length){
        putchar('\n');
        return;
    }
    printf("\n    ");
    auto hs = HelpState(columns-4, 4, 0);
    hs.remaining = hs.output_width;
    for(;help.length;){
        auto tok = next_tokenize_help(help);
        help = tok.rest;
        if(tok.is_newline){
            if(hs.remaining != hs.output_width){
                printf("\n    ");
                hs.remaining = hs.output_width;
            }
            continue;
        }
        hs.update(cast(int)tok.token.length);
        printf("%.*s", cast(int)tok.token.length, tok.token.ptr);
        if(hs.remaining){
            putchar(' ');
            hs.remaining--;
        }
    }
    putchar('\n');
}

struct HelpTokenized {
    const(char)[] token;
    bool is_newline;
    const(char)[] rest;
}

HelpTokenized
next_tokenize_help(const(char)[] help){
    for(;help.length;help = help[1..$]){
        switch(help[0]){
            case ' ': case '\r': case '\t': case '\f':
                continue;
            default: break;
        }
        break;
    }
    if(help[0] == '\n'){
        return HelpTokenized(null, true, help[1..$]);
    }
    const(char)[] begin = help;
    for(;;){
        if(!help.length)
            return HelpTokenized(begin, false, help);
        switch(help[0]){
            case ' ': case '\n': case '\r': case '\t': case '\f': case '\0':{
                auto tok = begin[0..begin.length - help.length];
                return HelpTokenized(tok, false, help);
                }
            default: break;
        }
        help = help[1..$];
    }
    assert(0);
}

void
print_arg_help(const ArgToParse* arg, int columns){
    auto help = arg.help;
    auto name = arg.name;
    auto typename = arg.dest.type != ArgType.USER_DEFINED?ArgTypeNames[arg.dest.type]:arg.dest.ut.type_name;
    printf("%.*s", cast(int)name.length, name.ptr);
    if(arg.altname1.length){
        printf(", %.*s", cast(int)arg.altname1.length, arg.altname1.ptr);
    }
    printf(": %.*s", cast(int)typename.length, typename.ptr);
    if(arg.num.min != 0 || !(arg.flags & ArgToParseFlags.SHOW_DEFAULT)){
        print_wrapped_help(help, columns);
        if(arg.dest.type == ArgType.ENUM)
            print_enum_options(arg.dest.dest.enum_pointer);
        return;
    }
    final switch(arg.dest.type){
        case ArgType.INTEGER64:{
            auto data = cast(long*)arg.dest.dest.pointer;
            printf(" = %lld", *data);
            print_wrapped_help(help, columns);
        }break;
        case ArgType.UINTEGER64:{
            auto data = cast(ulong*)arg.dest.dest.pointer;
            printf(" = %llu", *data);
            print_wrapped_help(help, columns);
        }break;
        case ArgType.INT:{
            auto data = cast(int*)arg.dest.dest.pointer;
            printf(" = %d", *data);
            print_wrapped_help(help, columns);
        }break;
        case ArgType.FLOAT32:{
            auto data = cast(float*)arg.dest.dest.pointer;
            printf(" = %f", cast(double)*data);
            print_wrapped_help(help, columns);
        }break;
        case ArgType.FLOAT64:{
            auto data = cast(double*)arg.dest.dest.pointer;
            printf(" = %f", *data);
            print_wrapped_help(help, columns);
        }break;
        case ArgType.BITFLAG:{
            print_wrapped_help(help, columns);
        }break;
        case ArgType.FLAG:{
            print_wrapped_help(help, columns);
        }break;
        case ArgType.STRING:{
            auto s = cast(const(char)[]*)arg.dest.dest.pointer;
            printf(" = '%.*s'", cast(int)s.length, s.ptr);
            print_wrapped_help(help, columns);
        }break;
        case ArgType.ENUM:{
            const ArgParseEnumType* enu = arg.dest.dest.enum_pointer;
            const(char)[] enu_name = "???";
            final switch(enu.enum_size){
                case 1:{
                    auto def = cast(ubyte*)arg.dest.dest.pointer;
                    if(*def < enu.enum_count)
                        enu_name = enu.enum_names[*def];
                }break;
                case 2:{
                    auto def = cast(ushort*)arg.dest.dest.pointer;
                    if(*def < enu.enum_count)
                        enu_name = enu.enum_names[*def];
                }break;
                case 4:{
                    auto def = cast(uint*)arg.dest.dest.pointer;
                    if(*def < enu.enum_count)
                        enu_name = enu.enum_names[*def];
                }break;
                case 8:{
                    auto def = cast(ulong*)arg.dest.dest.pointer;
                    if(*def < enu.enum_count)
                        enu_name = enu.enum_names[*def];
                }break;
            }
            printf(" = %.*s", cast(int)enu_name.length, enu_name.ptr);
            print_wrapped_help(help, columns);
            print_enum_options(enu);
        }break;
        case ArgType.USER_DEFINED:{
            if(arg.dest.ut.default_str.length)
                printf(" = %.*s", cast(int)arg.dest.ut.default_str.length, arg.dest.ut.default_str.ptr);
            print_wrapped_help(help, columns);
        }break;
    }
}

void
print_enum_options(const ArgParseEnumType* enu){
    if(!enu) return;
    printf("    Options:\n");
    printf("    --------\n");
    for(size_t i = 0; i < enu.enum_count; i++){
        printf("    [%2zu] %.*s\n", i, cast(int)enu.enum_names[i].length, enu.enum_names[i].ptr);
    }
}

long
check_for_early_out_args(const ArgParser* parser, char*[] args){
    foreach(arg_; args){
        char[] arg = arg_[0..strlen(arg_)];
        foreach(i, ref early; parser.early_out){
            if(arg == early.name)
                return i;
            if(arg == early.altname1)
                return i;
        }
    }
    return -1;
}

ArgParseError
parse_args(ArgParser* parser, char*[]args, ArgParseFlags flags){
    with(ArgType) with(ArgParseFlags) with(ArgParseError){
    ArgToParse* pos_arg = null;
    ArgToParse* past_the_end = null;
    if(parser.positional.length){
        pos_arg = &parser.positional[0];
        past_the_end = pos_arg + parser.positional.length;
    }
    ArgToParse* kwarg = null;
    foreach(arg; args){
        if(!arg && (flags & SKIP_NULL_STRINGS))
            continue;
        if(!arg)
            return INTERNAL_ERROR;
        char[] s = arg[0..strlen(arg)];
        if(!s.length && (flags & SKIP_EMPTY_STRINGS))
            continue;
        if(s.length > 1){
            if(s[0] == '-'){
                switch(s[1]){
                    case '0': .. case '9':
                    case '.':
                        // number, not an argument.
                        break;
                    default:{
                        // Not a number, find matching kwarg
                        ArgToParse* new_kwarg = find_matching_kwarg(parser, s);
                        if(!new_kwarg){
                            if(flags & UNKNOWN_KWARGS_AS_ARGS)
                                break;
                            parser.failed_arg = s;
                            return UNKNOWN_KWARG;
                        }
                        if(new_kwarg.visited){
                            parser.failed_arg_to_parse = new_kwarg;
                            parser.failed_arg = s;
                            return DUPLICATE_KWARG;
                        }
                        if(pos_arg && pos_arg != past_the_end && pos_arg.visited)
                            pos_arg++;
                        kwarg = new_kwarg;
                        kwarg.visited = true;
                        if(kwarg.dest.type == FLAG || kwarg.dest.type == BITFLAG){
                            ArgParseError error = set_flag(kwarg);
                            if(error) {
                                parser.failed_arg_to_parse = kwarg;
                                parser.failed_arg = s;
                                return error;
                            }
                            kwarg = null;
                        }
                        continue;
                    }
                }
            }
        }
        if(kwarg){
            ArgParseError err = parse_arg(kwarg, s);
            if(err){
                parser.failed_arg = s;
                parser.failed_arg_to_parse = kwarg;
                return err;
            }
            if(kwarg.num_parsed == kwarg.num.max)
                kwarg = null;
        }
        else if(pos_arg && pos_arg != past_the_end){
            pos_arg.visited = true;
            ArgParseError err = parse_arg(pos_arg, s);
            if(err){
                parser.failed_arg = s;
                parser.failed_arg_to_parse = pos_arg;
                return err;
            }
            if(pos_arg.num_parsed == pos_arg.num.max)
                pos_arg++;
        }
        else {
            parser.failed_arg = s;
            return EXCESS_ARGS;
        }
    }
    foreach(ref arg; parser.positional){
        if(arg.num_parsed < arg.num.min){
            parser.failed_arg_to_parse = &arg;
            return INSUFFICIENT_ARGS;
        }
        if(arg.num.max >= 0 && arg.num_parsed > arg.num.max){
            parser.failed_arg_to_parse = &arg;
            return EXCESS_ARGS;
        }
    }
    foreach(ref arg; parser.keyword){
        if(arg.num_parsed < arg.num.min){
            parser.failed_arg_to_parse = &arg;
            return INSUFFICIENT_ARGS;
            }
        if(arg.num.max >= 0 && arg.num_parsed > arg.num.max){
            parser.failed_arg_to_parse = &arg;
            return EXCESS_ARGS;
            }
        // This only makes sense for keyword arguments.
        if(arg.visited && arg.num_parsed == 0){
            parser.failed_arg_to_parse = &arg;
            return VISITED_NO_ARG_GIVEN;
            }
        }
    return NO_ERROR;
    }
}

ArgToParse*
find_matching_kwarg(ArgParser* parser, char[] s){
    // do an inefficient linear search for now.
    foreach(ref arg; parser.keyword){
        if(arg.name == s)
            return &arg;
        if(arg.altname1 == s)
            return &arg;
    }
    return null;
}



ArgParseError
set_flag(ArgToParse* arg){
with(ArgType) with(ArgParseError){
    if(arg.dest.type == BITFLAG){
        auto dest = cast(ulong*)arg.dest.dest.pointer;
        if(*dest & arg.dest.dest.bitflag)
            return DUPLICATE_KWARG;
        *dest |= arg.dest.dest.bitflag;
        arg.num_parsed += 1;
        return NO_ERROR;
    }
    assert(arg.dest.type == FLAG);
    if(arg.num_parsed >= arg.num.max)
        return DUPLICATE_KWARG;
    auto dest = cast(bool*)arg.dest.dest.pointer;
    *dest = true;
    arg.num_parsed += 1;
    return NO_ERROR;
}
}

ArgParseError
parse_arg(ArgToParse* arg, char[] s){
with(ArgType) with(ArgParseError){
    // Append_procs should signal their own error.
    if(arg.num.max >= 0 && arg.num_parsed >= arg.num.max){
        return EXCESS_ARGS;
    }
    // If previous num parsed is nonzero, this means
    // that what we are pointing to is an array.
    void APPEND_ARG(T)(T value){
        if(arg.dest.user_append_proc){
            arg.dest.user_append_proc(&value);
            arg.num_parsed++;
            return;
        }
        auto dest = cast(T*) arg.dest.dest.pointer;
        dest += arg.num_parsed;
        *dest = value;
        arg.num_parsed++;
    }
    final switch(arg.dest.type){
        case INTEGER64:{
            auto e = parse_uint64(s);
            if(e.errored)
                return CONVERSION_ERROR;
            APPEND_ARG(cast(long)e.value);
        }break;
        case UINTEGER64:{
            // struct Uint64Result e = parse_unsigned_human(s.text, s.length);
            auto e = parse_uint64(s);
            if(e.errored)
                return CONVERSION_ERROR;
            APPEND_ARG(e.value);
        }break;
        case INT:{
            // struct IntResult e = parse_int(s.text, s.length);
            auto e = parse_uint64(s);
            if(e.errored)
                return CONVERSION_ERROR;
            APPEND_ARG(cast(int)e.value);
        }break;
        case FLOAT32:{
            char* endptr;
            // I'd rather roll my own strtof, but that's too much work right now.
            // FIXME: assumes nul-terminated strings.
            float value = strtof(s.ptr, &endptr);
            if(endptr == s.ptr)
                return CONVERSION_ERROR;
            if(*endptr != '\0')
                return CONVERSION_ERROR;
            APPEND_ARG(value);
            }break;
        case FLOAT64:{
            char* endptr;
            // Ditto on rolling my own.
            double value = strtod(s.ptr, &endptr);
            if(endptr == s.ptr)
                return CONVERSION_ERROR;
            if(*endptr != '\0')
                return CONVERSION_ERROR;
            APPEND_ARG(value);
            }break;
        // for flags, using the append_proc doesn't make sense.
        case BITFLAG:
            // fall-through
        case FLAG:
            // This is weird, but it is a configuration error.
            return set_flag(arg);
        case STRING:{
            // This is a hack, our target is actually a LongString.
            // But it can also be a StringView and they pun to each other.
            APPEND_ARG(s);
            }break;
        case ENUM:{
            if(!s.length) return CONVERSION_ERROR;
            const ArgParseEnumType* enu = arg.dest.dest.enum_pointer;
            // We just do a linear search over the strings.  In theory this is
            // very bad, but in practice a typical parse line will need to
            // match against a given enum once so any fancy algorithm would
            // require a pre-pass over all the data anyway.  We could be faster
            // if we required enums to be sorted, but this harms usability too
            // much.  If you need to parse a lot of enums with weird
            // requirements, then just a create a user defined type instead of
            // using this.
            for(size_t i = 0; i < enu.enum_count; i++){
                if(enu.enum_names[i] == s){
                    switch(enu.enum_size){
                        case 1:{
                            APPEND_ARG(cast(ubyte)i);
                        }return NO_ERROR;
                        case 2:{
                            APPEND_ARG(cast(ushort)i);
                        }return NO_ERROR;
                        case 4:{
                            APPEND_ARG(cast(uint)i);
                        }return NO_ERROR;
                        case 8:{
                            APPEND_ARG(cast(ulong)i);
                        }return NO_ERROR;
                        default:
                            return INTERNAL_ERROR;
                    }
                }
            }
            // allow specifying enums by numeric value.
            // struct Uint64Result uint_res = parse_unsigned_human(s.text, s.length);
            auto e = parse_uint64(s);
            if(e.errored)
                return CONVERSION_ERROR;
            if(e.value >= enu.enum_count)
                return CONVERSION_ERROR;
            switch(enu.enum_size){
                case 1:{
                    APPEND_ARG(cast(ubyte)e.value);
                }return NO_ERROR;
                case 2:{
                    APPEND_ARG(cast(ushort)e.value);
                }return NO_ERROR;
                case 4:{
                    APPEND_ARG(cast(uint)e.value);
                }return NO_ERROR;
                case 8:{
                    APPEND_ARG(e.value);
                }return NO_ERROR;
                default:
                    return INTERNAL_ERROR;
            }
        }
        case USER_DEFINED:{
            int err = arg.dest.ut.parse_proc(s);
            if(err) return CONVERSION_ERROR;
            arg.num_parsed++;
        }break;
    }
    return NO_ERROR;
}
}

void
print_argparse_error(const ArgParser* parser, ArgParseError error){
with(ArgParseError) with (ArgType){
    if(auto arg_to_parse = parser.failed_arg_to_parse){
        fprintf(stderr, "Error when parsing argument for '%.*s': ", cast(int)arg_to_parse.name.length, arg_to_parse.name.ptr);
    }
    final switch(error){
        case NO_ERROR:
            break;
        case CONVERSION_ERROR:
            if(auto arg_to_parse = parser.failed_arg_to_parse){
                if(auto arg = parser.failed_arg){
                    switch(arg_to_parse.dest.type){
                        case INTEGER64:
                            fprintf(stderr, "Unable to parse an int64 from '%.*s'\n", cast(int)arg.length, arg.ptr);
                            return;
                        case INT:
                            fprintf(stderr, "Unable to parse an int from '%.*s'\n", cast(int)arg.length, arg.ptr);
                            return;
                        // These seem bizarre.
                        case STRING:
                            fprintf(stderr, "Unable to parse a string from '%.*s'\n", cast(int)arg.length, arg.ptr);
                            return;
                        case UINTEGER64:
                            fprintf(stderr, "Unable to parse a uint64 from '%.*s'\n", cast(int)arg.length, arg.ptr);
                            return;
                        case FLOAT32:
                            fprintf(stderr, "Unable to parse a float32 from '%.*s'\n", cast(int)arg.length, arg.ptr);
                            return;
                        case FLOAT64:
                            fprintf(stderr, "Unable to parse a float64 from '%.*s'\n", cast(int)arg.length, arg.ptr);
                            return;
                        case ENUM:
                            fprintf(stderr, "Unable to parse a choice from '%.*s'. Not a valid option.\n", cast(int)arg.length, arg.ptr);
                            return;
                        case BITFLAG:
                            // fall-through
                        case FLAG:
                            fprintf(stderr, "Unable to parse a flag. This is a bug.\n");
                            return;
                        case USER_DEFINED:
                            fprintf(stderr, "Unable to parse a %.*s from '%.*s'\n", cast(int)arg_to_parse.dest.ut.type_name.length, arg_to_parse.dest.ut.type_name.ptr,cast(int)arg.length, arg.ptr);
                            return;
                        default:
                            break;
                    }
                    fprintf(stderr, "Unable to parse an unknown type from '%.*s'\n",  cast(int)arg.length, arg.ptr);
                    return;
                }
                else {
                    switch(arg_to_parse.dest.type){
                        case INTEGER64:
                            fprintf(stderr, "Unable to parse an int64 from unknown argument'\n");
                            return;
                        case INT:
                            fprintf(stderr, "Unable to parse an int from unknown argument'\n");
                            return;
                        // These seem bizarre.
                        case STRING:
                            fprintf(stderr, "Unable to parse a string from unknown argument.\n");
                            return;
                        case UINTEGER64:
                            fprintf(stderr, "Unable to parse a uint64 from unknown argument.\n");
                            return;
                        case FLOAT32:
                            fprintf(stderr, "Unable to parse a float32 from unknown argument.\n");
                            return;
                        case FLOAT64:
                            fprintf(stderr, "Unable to parse a float64 from unknown argument.\n");
                            return;
                        case ENUM:
                            fprintf(stderr, "Unable to parse a choice from unknown argument.\n");
                            return;
                        case BITFLAG:
                            // fall-through
                        case FLAG:
                            fprintf(stderr, "Unable to parse a flag. This is a bug.\n");
                            return;
                        case USER_DEFINED:
                            fprintf(stderr, "Unable to parse a %.*s from unkown argument.\n", cast(int)arg_to_parse.dest.ut.type_name.length, arg_to_parse.dest.ut.type_name.ptr);
                            return;
                        default: break;
                    }
                    fprintf(stderr, "Unable to parse an unknown type from unknown argument'\n");
                    return;
                }
            }
            else if(auto arg = parser.failed_arg){
                fprintf(stderr, "Unable to parse an unknown type from '%.*s'\n", cast(int)arg.length, arg.ptr);
                return;
            }
            else {
                fprintf(stderr, "Unable to parse an unknown type from an unknown argument. This is a bug.\n");
            }
            return;
        case UNKNOWN_KWARG:
            if(auto arg = parser.failed_arg)
                fprintf(stderr, "Unrecognized argument '%.*s'\n", cast(int)arg.length, arg.ptr);
            else
                fprintf(stderr, "Unrecognized argument is unknown. This is a bug.\n");
            return;
        case DUPLICATE_KWARG:
            fprintf(stderr, "Option given more than once.\n");
            return;
        case EXCESS_ARGS:{
            // Args were given after all possible args were consumed.
            if(!parser.failed_arg_to_parse){
                fprintf(stderr, "More arguments given than needed. First excess argument: '%.*s'.\n", cast(int)parser.failed_arg.length, parser.failed_arg.ptr);
                return;
            }
            auto arg_to_parse = parser.failed_arg_to_parse;

            if(!parser.failed_arg){
                fprintf(stderr, "Excess arguments. No more than %d arguments needed. Unknown first excess argument (this is a bug)\n", arg_to_parse.num.max);
                return;
            }
            auto arg = parser.failed_arg;
            fprintf(stderr, "Excess arguments. No more than %d arguments needed. First excess argument: '%.*s'\n", arg_to_parse.num.max, cast(int)arg.length, arg.ptr);
        }return;
        case INSUFFICIENT_ARGS:{
            if(!parser.failed_arg_to_parse){
                fprintf(stderr, "Insufficent arguments for unknown option. This is a bug.\n");
                return;
                }
            auto arg_to_parse = parser.failed_arg_to_parse;
            fprintf(stderr, "Insufficient arguments. %d argument%s required.\n", arg_to_parse.num.min, arg_to_parse.num.min==1?" is".ptr:"s are".ptr);
        }return;
        case VISITED_NO_ARG_GIVEN:{
            auto arg_to_parse = parser.failed_arg_to_parse;
            if(!arg_to_parse){
                fprintf(stderr, "An unknown argument was visited. This is a bug.\n");
                return;
                }
            fprintf(stderr, "No arguments given.\n");
        }return;
        case INTERNAL_ERROR:{
            fprintf(stderr, "An internal error occurred. This is a bug.\n");
            return;
        }
    }
    fprintf(stderr, "Unknown error when parsing arguments.\n");
    return;
}
}
