/*
 * C Preprocessor tool (like gcc -E)
 * Outputs preprocessed tokens for debugging
 */
import core.stdc.stdio : fprintf, stderr, stdout, fwrite;
static import core.stdc.stdio;
static import core.stdc.string;
import dlib.aliases;
import dlib.allocator : Mallocator, Allocator, MALLOCATOR;
import dlib.barray : Barray, make_barray;
import dlib.file_cache: FileCache, FileError;
import dlib.zstring : ZString;
import dlib.term_util : get_cols;
import dlib.logger;
static import dlib.argparse;

import cfront.c_pp_token;
import cfront.c_pp_lexer : pp_tokenize;
import cfront.c_preprocessor : CPreprocessor;
import cfront.cfront : DEFAULT_INCLUDE_PATHS, DEFAULT_FRAMEWORK_PATHS;

extern(C) int main(int argc, char** argv) {
    FileCache file_cache = FileCache(MALLOCATOR);
    Logger logger;
    ZString sourcefile;
    Barray!str include_paths;
    include_paths.bdata.allocator = MALLOCATOR;
    scope(exit) include_paths.cleanup();
    Barray!str framework_paths;
    framework_paths.bdata.allocator = MALLOCATOR;
    scope(exit) framework_paths.cleanup();
    Barray!str force_includes;
    force_includes.bdata.allocator = MALLOCATOR;
    scope(exit) force_includes.cleanup();

    with (dlib.argparse) {
        ArgToParse[1] pos_args = [
            ArgToParse(
                name: "source",
                help: "Source file (.c file) to preprocess.",
                dest: ARGDEST(&sourcefile),
            ),
        ];
        ArgToParse[3] _kw_args = [
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
                name: "-F",
                help: "Add directory to framework search path. Can be specified multiple times.",
                dest: ArgUser((str path) { framework_paths ~= path; return 0; }, "path"),
                num: NumRequired(0, int.max),
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
        // Only have -F arg on macos
        version(OSX) ArgToParse[] kw_args = _kw_args[];
        else ArgToParse[] kw_args = _kw_args[0..$-1];
        ArgParser parser = {
            name: argc ? argv[0][0 .. core.stdc.string.strlen(argv[0])] : "cpp",
            description: "A C preprocessor (like gcc -E)",
            early_out: early_args,
            positional: pos_args,
            keyword: kw_args,
        };
        switch (check_for_early_out_args(&parser, argc ? argv[1 .. argc] : null)) {
            case HELP:
                print_argparse_help(&parser, columns);
                return 0;
            case VERSION:
                core.stdc.stdio.fprintf(core.stdc.stdio.stdout, "cpp V1.0\n");
                return 0;
            default:
                break;
        }
        auto error = parser.parse_args(argc ? argv[1 .. argc] : null);
        if (error) {
            print_argparse_error(&parser, error);
            core.stdc.stdio.fprintf(core.stdc.stdio.stderr, "Use --help to see usage.\n");
            return error;
        }
    }

    if (sourcefile.length == 0) {
        fprintf(stderr, "Error: source file required\n");
        return 1;
    }

    // Add default paths
    include_paths.extend(DEFAULT_INCLUDE_PATHS);
    framework_paths.extend(DEFAULT_FRAMEWORK_PATHS);

    Allocator alloc = Mallocator.allocator();

    // Read input file
    const(ubyte)[] source;
    FileError fe = file_cache.read_file(sourcefile[], source);
    if(fe != FileError.OK){
        logger.error("Error reading file: ", sourcefile, "\n");
        return 1;
    }

    // Find actual content length
    size_t actual_len = 0;
    while (actual_len < source.length && source[actual_len] != 0) {
        actual_len++;
    }
    source = source[0 .. actual_len];

    // Tokenize
    Barray!PPToken tokens = make_barray!PPToken(alloc);
    int err = pp_tokenize(source, sourcefile[], &tokens, alloc);
    if (err) {
        fprintf(stderr, "Tokenization error\n");
        return 1;
    }

    // Preprocess
    CPreprocessor pp;
    pp.allocator = alloc;
    pp.current_file = sourcefile[];
    pp.include_paths = include_paths[];
    pp.framework_paths = framework_paths[];
    pp.force_includes = force_includes[];
    pp.logger = &logger;
    pp.file_cache = &file_cache;
    pp.initialize();

    Barray!PPToken output = make_barray!PPToken(alloc);
    err = pp.process(tokens[], &output);
    if (err || pp.error_occurred) {
        fprintf(stderr, "Preprocessing error\n");
        return 1;
    }

    // Output preprocessed tokens
    str last_file = "";
    int last_line = 0;

    foreach (tok; output[]) {
        // Skip whitespace and newlines for cleaner output
        if (tok.type == PPTokenType.PP_WHITESPACE) {
            fwrite(" ".ptr, 1, 1, stdout);
            continue;
        }
        if (tok.type == PPTokenType.PP_NEWLINE) {
            fwrite("\n".ptr, 1, 1, stdout);
            continue;
        }
        if (tok.type == PPTokenType.PP_EOF) {
            continue;
        }

        // Use expansion location if set (where macro was invoked)
        str eff_file = tok.expansion_file.length > 0 ? tok.expansion_file : tok.file;
        int eff_line = tok.expansion_file.length > 0 ? tok.expansion_line : tok.line;

        // Print line markers when file/line changes
        if (eff_file != last_file || eff_line != last_line) {
            if (last_file.length > 0) {
                fwrite("\n".ptr, 1, 1, stdout);
            }
            fprintf(stdout, "# %d \"%.*s\"\n",
                eff_line,
                cast(int)eff_file.length, eff_file.ptr);
            last_file = eff_file;
            last_line = eff_line;
        }

        // Output token
        fwrite(tok.lexeme.ptr, 1, tok.lexeme.length, stdout);
    }

    fwrite("\n".ptr, 1, 1, stdout);
    return 0;
}
