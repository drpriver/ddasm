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

import cfront.c_tokenizer : CTokenizer, CToken;
import cfront.c_parser : CParser;
import cfront.c_ast : CTranslationUnit;
import cfront.c_to_dasm : CDasmWriter;

int compile_c_to_dasm(const(ubyte)[] source, Box!(char[])* progtext) {
    ArenaAllocator arena = ArenaAllocator(MALLOCATOR);
    scope(exit) arena.free_all();

    // Tokenize
    Barray!CToken tokens = make_barray!CToken(arena.allocator());
    CTokenizer tokenizer = CTokenizer(source, &tokens);
    int err = tokenizer.tokenize_tokens();
    if (err) return 1;
    tokens.bdata.resize(tokens.count);

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
