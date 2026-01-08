/*
 * C to DASM compiler - outputs DASM text for debugging
 * Copyright 2025, David Priver
 */
import dlib.allocator : Mallocator, Allocator, MALLOCATOR, FixedAllocator;
import dlib.barray : Barray, Marray;
import dlib.box : Box;
import dlib.stringbuilder : StringBuilder;
import dlib.logger;
static import cfront.cfront;
import core.stdc.stdio : fprintf, stdout, stderr, stdin, fread, fwrite;

import dlib.zstring : ZString;
static import dlib.argparse;
import dlib.term_util : stdin_is_interactive, get_cols;
import dlib.get_input : LineHistory, get_input_line;
import dlib.file_util : read_file, FileFlags;
import dlib.aliases : str;
import dlib.file_cache: FileCache;
import cfront.cfront : DEFAULT_INCLUDE_PATHS, DEFAULT_FRAMEWORK_PATHS, CompileFlags;

static import core.stdc.string;

extern(C)
int main(int argc, char** argv) {
    FileCache file_cache = FileCache(MALLOCATOR);
    Logger logger;
    bool force_interactive = false;
    bool syntax_only = false;
    bool pp_only = false;
    bool print_ast = false;
    ZString sourcefile;
    Marray!str include_paths;
    scope(exit) include_paths.cleanup();
    Marray!str framework_paths;
    scope(exit) framework_paths.cleanup();
    Marray!str force_includes;
    scope(exit) force_includes.cleanup();

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
        ArgToParse[7] _kw_args = [
            ArgToParse(
                name: "-I",
                help: "Add directory to include search path. Can be specified multiple times.",
                dest: ArgUser((str path) { include_paths ~= path; return 0; }, "path"),
                num: NumRequired(0, int.max),
            ),
            ArgToParse(
                name: "-include",
                help: "Include file(s) before processing source.",
                dest: ArgUser((str path) { force_includes ~= path; return 0; }, "file"),
                num: NumRequired(0, int.max),
            ),
            ArgToParse(
                name: "--force-interactive", altname: "-i",
                help: "Force interactive command history mode when reading from stdin.",
                dest: ARGDEST(&force_interactive),
            ),
            ArgToParse(
                name: "--syntax-only", altname: "-fsyntax-only",
                help: "Parse source and check for errors, but don't generate code.",
                dest: ARGDEST(&syntax_only),
            ),
            ArgToParse(
                name: "-E", altname: "--pp-only",
                help: "Preprocess only, output preprocessed source.",
                dest: ARGDEST(&pp_only),
            ),
            ArgToParse(
                name: "--print-ast", altname: "-ast-dump",
                help: "Print the AST after parsing.",
                dest: ARGDEST(&print_ast),
            ),
            ArgToParse(
                name: "-F",
                help: "Add directory to framework search path. Can be specified multiple times.",
                dest: ArgUser((str path) { framework_paths ~= path; return 0; }, "path"),
                num: NumRequired(0, int.max),
            ),
        ];
        version(OSX) ArgToParse[] kw_args = _kw_args[];
        else ArgToParse[] kw_args = _kw_args[0..$-1];
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
        auto fe = read_file(sourcefile.ptr, MALLOCATOR, FileFlags.NUL_TERMINATE | FileFlags.ZERO_PAD_TO_16);
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

    // Add default paths after user-specified ones
    include_paths.extend(DEFAULT_INCLUDE_PATHS);
    framework_paths.extend(DEFAULT_INCLUDE_PATHS);
    FixedAllocator f = FixedAllocator.fixed!(1024 * 1024);
    Box!(char[]) progtext = {f.allocator()};
    scope(exit) progtext.dealloc;
    CompileFlags flags = {
        syntax_only: syntax_only,
        pp_only: pp_only,
        print_ast: print_ast,
    };
    int err = cfront.cfront.compile_c_to_dasm(bscript.data, &progtext, sourcefile[], include_paths[], framework_paths[], flags, force_includes[], &logger, &file_cache);
    if (err) return err;
    // Don't print progtext for flags that don't produce code
    if (!syntax_only && !pp_only && !print_ast)
        fprintf(stdout, "%.*s\n", cast(int)progtext.data.length, progtext.data.ptr);
    return 0;
}
