/*
 * C Preprocessor tool (like gcc -E)
 * Outputs preprocessed tokens for debugging
 */
import core.stdc.stdio : fprintf, stderr, stdout, fwrite;
import dlib.aliases;
import dlib.allocator : Mallocator, Allocator;
import dlib.barray : Barray, make_barray;
import dlib.file_util : read_file, FileResult, FileFlags;
import cfront.c_pp_token;
import cfront.c_pp_lexer : pp_tokenize;
import cfront.c_preprocessor : CPreprocessor;

extern(C) int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: cpp <file.c>\n");
        return 1;
    }

    Allocator alloc = Mallocator.allocator();

    // Read input file
    FileResult fr = read_file(argv[1], alloc, FileFlags.NUL_TERMINATE | FileFlags.ZERO_PAD_TO_16);
    if (fr.errored) {
        fprintf(stderr, "Error reading file: %s\n", argv[1]);
        return 1;
    }

    const(ubyte)[] source = cast(const(ubyte)[])fr.value.data;

    // Find actual content length
    size_t actual_len = 0;
    while (actual_len < source.length && source[actual_len] != 0) {
        actual_len++;
    }
    source = source[0 .. actual_len];

    // Get filename as string
    str filename;
    size_t len = 0;
    while (argv[1][len] != 0) len++;
    filename = cast(str)argv[1][0 .. len];

    // Tokenize
    Barray!PPToken tokens = make_barray!PPToken(alloc);
    int err = pp_tokenize(source, filename, &tokens, alloc);
    if (err) {
        fprintf(stderr, "Tokenization error\n");
        return 1;
    }

    // Preprocess
    CPreprocessor pp;
    pp.allocator = alloc;
    pp.current_file = filename;
    static immutable str[3] paths = [
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include",
        "/Library/Frameworks/Python.framework/Versions/3.14/include/python3.14",
        "/usr/local/include",
    ];
    pp.include_paths = cast(str[])paths;
    pp.init();

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
