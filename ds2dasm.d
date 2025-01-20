/*
 * Copyright © 2021-2023, David Priver
 */
import dlib.allocator: Mallocator, report_leaks;
import dlib.box: Box;
import dlib.stringbuilder: StringBuilder;
static import dscript.dscript;
static import dscript_to_dasm;
import core.stdc.stdio: fprintf, stdout, stderr, stdin, fread;

import dlib.zstring: ZString;
static import dlib.argparse;
import dlib.term_util: stdin_is_interactive, get_cols;
import dlib.get_input: LineHistory, get_input_line;
import dlib.file_util: read_file, FileFlags;

static import core.stdc.string;

extern(C)
int main(int argc, char** argv){
    dscript.dscript.powerup;
    scope(exit) dscript.dscript.powerdown;
    bool force_interactive = false;
    ZString sourcefile;
    with(dlib.argparse)with(ArgParseFlags) with(ArgToParseFlags){
        import core.stdc.stdio: fprintf, stdout, stderr;
        ArgToParse[1] pos_args = [
            ArgToParse(
                name: "source",
                help: "Source file (.ds file) to read from.
                If not given, will read from stdin.",
                dest: ARGDEST(&sourcefile),
            ),
        ];
        ArgToParse[1] kw_args = [
            ArgToParse(
                name:"--force-interactive", altname: "-i",
                help: "Force interactive command history mode when reading from stdin.",
                dest: ARGDEST(&force_interactive),
            ),
        ];
        enum {HELP=0, VERSION=1}
        ArgToParse[2] early_args = [
            ArgToParse(
                name: "-h", altname: "--help",
                help: "Print this help and exit.",
            ),
            ArgToParse(
                name: "-v", altname: "--version",
                help: "Print the version and exit.",
            ),
        ];
        int columns = get_cols();
        ArgParser parser = {
            name: argc?argv[0][0 .. core.stdc.string.strlen(argv[0])]:"dscript",
            description: "A davescript compiler",
            early_out: early_args,
            positional: pos_args,
            keyword: kw_args,
        };
        switch(check_for_early_out_args(&parser, argc?argv[1 .. argc]:null)){
            case HELP:
                print_argparse_help(&parser, columns);
                return 0;
            case VERSION:
                fprintf(stdout, "dscript V1337\n");
                return 0;
            default:
                break;
        }
        auto error = parser.parse_args(argc?argv[1 .. argc]:null);
        if(error) {
            print_argparse_error(&parser, error);
            fprintf(stderr, "Use --help to see usage.\n");
            return error;
        }
    }
    Box!(const(ubyte)[], Mallocator) bscript;
    scope(exit) bscript.dealloc;
    if(sourcefile.length){
        auto fe = read_file!Mallocator(sourcefile.ptr);
        if(fe.errored){
            version(Windows)
                fprintf(stderr, "Unable to read from '%s'\n", sourcefile.ptr);
            else
                fprintf(stderr, "Unable to read from '%s': %s\n", sourcefile.ptr, core.stdc.string.strerror(fe.errored));
            // TODO: get error message from windows
            return fe.errored;
        }
        bscript = fe.value.as!(const(ubyte)[]);
    }
    else if(force_interactive || stdin_is_interactive){
        StringBuilder!Mallocator sb;
        LineHistory!() history;
        const char* HISTORYFILE = "dscript.history";
        history.load_history(HISTORYFILE);
        scope(exit){
            history.dump(HISTORYFILE);
            history.cleanup;
        }
        char[4096] buff = void;
        for(;;){
            auto len = get_input_line(&history, "> ", buff[]);
            if(len < 0) break;
            if(len == 1 && buff[0] == 'q') break;
            if(len){
                history.add_line(buff[0..len]);
                sb.write(buff[0..len]);
            }
            sb.write('\n');
        }
        bscript = sb.detach.as!(const(ubyte)[]);
    }
    else {
        StringBuilder!Mallocator sb;
        for(;;){
            enum N = 4096;
            sb.ensure_additional(N);
            char* buff= sb.data + sb.cursor;
            auto numread = fread(buff, 1, N, stdin);
            sb.cursor += numread;
            if(numread != N)
                break;
        }
        if(!sb.cursor)
            sb.write(' ');
        bscript = sb.detach.as!(const(ubyte)[]);
    }
    // fprintf(stderr, "%.*s\n", cast(int)bscript.data.length, bscript.data.ptr);
    Box!(char[], Mallocator) progtext;
    scope(exit) progtext.dealloc;
    int err = dscript_to_dasm.compile_to_dasm(bscript.data, &progtext);
    if(err) return err;
    fprintf(stdout, "%.*s\n", cast(int)progtext.data.length, progtext.data.ptr);
    report_leaks;
    return 0;
}
