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

// Default system include paths
version(OSX) {
    enum str[] DEFAULT_INCLUDE_PATHS = [
        "/usr/local/include",
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include",
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/17/include",
    ];
} else version(linux) {
    enum str[] DEFAULT_INCLUDE_PATHS = [
        "/usr/include",
        "/usr/local/include",
    ];
} else {
    enum str[] DEFAULT_INCLUDE_PATHS = [
        "/usr/include",
        "/usr/local/include",
    ];
}

int compile_c_to_dasm(const(ubyte)[] source, Box!(char[])* progtext, str source_file = "", str[] include_paths = []) {
    ArenaAllocator arena = ArenaAllocator(MALLOCATOR);
    scope(exit) arena.free_all();

    // Find actual content length (before NUL terminator, if any)
    size_t actual_len = 0;
    while (actual_len < source.length && source[actual_len] != 0) {
        actual_len++;
    }
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
    preprocessor.init();
    err = preprocessor.process(pp_tokens[], &processed);
    if (err) return 1;

    // Phase 5+: Convert PPTokens to CTokens
    Barray!CToken tokens = make_barray!CToken(arena.allocator());
    err = pp_to_c_tokens(processed[], &tokens, arena.allocator());
    if (err) return 1;

    // Parse
    CParser parser = CParser(arena.allocator(), tokens[]);
    CTranslationUnit unit;
    err = parser.parse(&unit);
    if (err) return 1;

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
