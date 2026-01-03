/*
 * C Front-End Entry Point for ddasm
 * Copyright 2025, David Priver
 */
module cfront.cfront;

import dlib.aliases;
import dlib.allocator : MALLOCATOR, ArenaAllocator, Allocator;
import dlib.box : Box;
import dlib.stringbuilder : StringBuilder;
import dlib.barray : Barray, make_barray;

// New token-based preprocessor
import cfront.c_pp_token : PPToken;
import cfront.c_pp_lexer : pp_tokenize;
import cfront.c_preprocessor : CPreprocessor;
import cfront.c_pp_to_c : CToken, pp_to_c_tokens;

import cfront.c_parser : CParser;
import cfront.c_ast : CTranslationUnit;
import cfront.c_to_dasm : CDasmWriter;

struct CompileFlags {
    bool syntax_only;
    bool pp_only;
    bool print_ast;
    bool debug_types;
}

// Default system include paths (compile-time)
version(OSX) {
    enum str[] DEFAULT_INCLUDE_PATHS = [
        "/usr/local/include",
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include",
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/17/include",
    ];
    enum str[] DEFAULT_FRAMEWORK_PATHS = [
        "/System/Library/Frameworks",
        "/Library/Frameworks",
    ];
} else version(linux) {
    version(X86_64)
        enum str[] DEFAULT_INCLUDE_PATHS = [
            // XXX
            "/usr/lib/gcc/x86_64-linux-gnu/9/include",
            "/usr/local/include",
            "/usr/include/x86_64-linux-gnu",
            "/usr/include",
        ];
    else
        // FIXME: arch-specific includes
        enum str[] DEFAULT_INCLUDE_PATHS = [
            "/usr/local/include",
            "/usr/include",
        ];
    enum str[] DEFAULT_FRAMEWORK_PATHS = [];  // No frameworks on Linux
} else {
    enum str[] DEFAULT_INCLUDE_PATHS = [
        "/usr/include",
        "/usr/local/include",
    ];
    enum str[] DEFAULT_FRAMEWORK_PATHS = [];
}

// Default library search paths (runtime, for dlopen)
version(OSX) {
    enum str[] DEFAULT_LIBRARY_PATHS = [
        "/usr/local/lib",
        "/usr/lib",
    ];
} else version(linux) {
    enum str[] DEFAULT_LIBRARY_PATHS = [
        "/usr/local/lib",
        "/usr/lib",
        "/lib",
    ];
} else {
    enum str[] DEFAULT_LIBRARY_PATHS = [
        "/usr/local/lib",
        "/usr/lib",
    ];
}

int compile_c_to_dasm(const(ubyte)[] source, Box!(char[])* progtext, str source_file = "", str[] include_paths = [], str[] framework_paths = [], CompileFlags flags = CompileFlags.init) {
    ArenaAllocator arena = ArenaAllocator(MALLOCATOR);
    scope(exit) arena.free_all();

    // Find actual content length (before NUL terminator, if any)
    size_t actual_len = source.length;
    while(actual_len && source[actual_len-1] == 0)
        actual_len--;
    const(ubyte)[] trimmed_source = source[0 .. actual_len];

    // Phase 3: Tokenize source to PPTokens
    Barray!PPToken pp_tokens = make_barray!PPToken(arena.allocator());
    int err = pp_tokenize(trimmed_source, source_file, &pp_tokens, arena.allocator());
    if (err) return 1;

    // Phase 4: Preprocess (directives, macro expansion)
    Barray!PPToken processed = make_barray!PPToken(arena.allocator());
    CPreprocessor preprocessor;
    preprocessor.allocator = arena.allocator();
    preprocessor.current_file = source_file;
    preprocessor.include_paths = include_paths;
    preprocessor.framework_paths = framework_paths;
    preprocessor.initialize();
    err = preprocessor.process(pp_tokens[], &processed);
    if (err) return 1;
    if(flags.pp_only){
        str last_file = "";
        int last_line = 0;
        import cfront.c_pp_token;
        import core.stdc.stdio;
        foreach (tok; processed[]) {
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

    // Phase 5+: Convert PPTokens to CTokens
    Barray!CToken tokens = make_barray!CToken(arena.allocator());
    err = pp_to_c_tokens(processed[], &tokens, arena.allocator());
    if (err) return 1;

    // Parse
    CParser parser = CParser(arena.allocator(), tokens[]);
    parser.debug_types = flags.debug_types;
    CTranslationUnit unit;
    err = parser.parse(&unit);
    if (err) return 1;

    // Dump type tables if debug_types is enabled
    if (flags.debug_types) {
        import core.stdc.stdio : fprintf, stderr;
        fprintf(stderr, "\n=== Type Tables ===\n");
        parser.dump_type_tables();
    }

    // Print AST if requested
    if (flags.print_ast) {
        import cfront.c_ast_printer : print_ast;
        print_ast(&unit);
    }

    // Stop after parsing if syntax_only
    if (flags.syntax_only || flags.print_ast) {
        return 0;
    }

    // Generate DASM
    StringBuilder sb = { allocator: progtext.allocator };
    CDasmWriter writer = CDasmWriter(&sb, arena.allocator());
    err = writer.generate(&unit);
    writer.cleanup();
    if (err) {
        sb.cleanup();
        return err;
    }

    *progtext = sb.detach();
    return 0;
}
