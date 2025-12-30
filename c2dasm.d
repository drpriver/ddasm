/*
 * C to DASM compiler - outputs DASM text for debugging
 * Copyright 2025, David Priver
 */
import dlib.allocator : Mallocator, Allocator, MALLOCATOR, FixedAllocator;
import dlib.box : Box;
import dlib.stringbuilder : StringBuilder;
static import cfront.cfront;
import core.stdc.stdio : fprintf, stdout, stderr, stdin, fread;

import dlib.zstring : ZString;
static import dlib.argparse;
import dlib.term_util : stdin_is_interactive, get_cols;
import dlib.get_input : LineHistory, get_input_line;
import dlib.file_util : read_file, FileFlags;

static import core.stdc.string;

extern(C)
int main(int argc, char** argv) {
    bool force_interactive = false;
    ZString sourcefile;
    with (dlib.argparse) with (ArgParseFlags) with (ArgToParseFlags) {
        import core.stdc.stdio : fprintf, stdout, stderr;
        ArgToParse[1] pos_args = [
            ArgToParse(
                name: "source",
                help: "Source file (.c file) to read from.
                If not given, will read from stdin.",
                dest: ARGDEST(&sourcefile),
            ),
        ];
        ArgToParse[1] kw_args = [
            ArgToParse(
                name: "--force-interactive", altname: "-i",
                help: "Force interactive command history mode when reading from stdin.",
                dest: ARGDEST(&force_interactive),
            ),
        ];
        enum { HELP = 0, VERSION = 1 }
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
            name: argc ? argv[0][0 .. core.stdc.string.strlen(argv[0])] : "c2dasm",
            description: "A C to DASM compiler",
            early_out: early_args,
            positional: pos_args,
            keyword: kw_args,
        };
        switch (check_for_early_out_args(&parser, argc ? argv[1 .. argc] : null)) {
            case HELP:
                print_argparse_help(&parser, columns);
                return 0;
            case VERSION:
                fprintf(stdout, "c2dasm V1.0\n");
                return 0;
            default:
                break;
        }
        auto error = parser.parse_args(argc ? argv[1 .. argc] : null);
        if (error) {
            print_argparse_error(&parser, error);
            fprintf(stderr, "Use --help to see usage.\n");
            return error;
        }
    }
    Box!(const(ubyte)[]) bscript;
    scope(exit) bscript.dealloc;
    if (sourcefile.length) {
        auto fe = read_file(sourcefile.ptr);
        if (fe.errored) {
            version(Windows)
                fprintf(stderr, "Unable to read from '%s'\n", sourcefile.ptr);
            else
                fprintf(stderr, "Unable to read from '%s': %s\n", sourcefile.ptr, core.stdc.string.strerror(fe.errored));
            return fe.errored;
        }
        bscript = fe.value.as!(const(ubyte)[]);
    }
    else if (force_interactive || stdin_is_interactive) {
        StringBuilder sb = {
            allocator: Mallocator.allocator(),
        };
        LineHistory history = { allocator: MALLOCATOR };
        const char* HISTORYFILE = "c2dasm.history";
        history.load_history(HISTORYFILE);
        scope(exit) {
            history.dump(HISTORYFILE);
            history.cleanup;
        }
        char[4096] buff = void;
        for (;;) {
            auto len = get_input_line(&history, "> ", buff[]);
            if (len < 0) break;
            if (len == 1 && buff[0] == 'q') break;
            if (len) {
                history.add_line(buff[0 .. len]);
                sb.write(buff[0 .. len]);
            }
            sb.write('\n');
        }
        bscript = sb.detach.as!(const(ubyte)[]);
    }
    else {
        StringBuilder sb = { allocator: Mallocator.allocator() };
        for (;;) {
            enum N = 4096;
            sb.ensure_additional(N);
            char* buff = sb.data + sb.cursor;
            auto numread = fread(buff, 1, N, stdin);
            sb.cursor += numread;
            if (numread != N)
                break;
        }
        if (!sb.cursor)
            sb.write(' ');
        bscript = sb.detach.as!(const(ubyte)[]);
    }

    FixedAllocator f = FixedAllocator.fixed!(1024 * 1024);
    Box!(char[]) progtext = {
        f.allocator(),
    };
    scope(exit) progtext.dealloc;
    int err = cfront.cfront.compile_c_to_dasm(bscript.data, &progtext, sourcefile[]);
    if (err) return err;
    fprintf(stdout, "%.*s\n", cast(int)progtext.data.length, progtext.data.ptr);
    return 0;
}
