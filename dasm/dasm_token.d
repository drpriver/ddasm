/*
 * Copyright © 2021-2022, David Priver
 */
module dasm.dasm_token;
enum TokenType: ubyte {
    BANG = '!',
    AT = '@',
    POUND = '#',
    DOLLAR = '$',
    PERCENT = '%',
    CARROT = '^',
    AMPERSAN = '&',
    ASTERISK = '*',
    LEFTPAREN = '(',
    RIGHTPAREN = ')',
    DASH = '-',
    EQUALS = '=',
    PLUS = '+',
    LEFTSQUAREBRACKET = '[',
    RIGHTSQUAREBRACKET = ']',
    LEFTCURLYBRACKET = '{',
    RIGHTCURLYBRACKET = '}',
    BACKSLASH = '\\',
    PIPE = '|',
    SLASH = '/',
    COMMA = ',',
    LESSTHAN = '<',
    GREATERTHAN = '>',
    DOT = '.',
    QUESTION = '?',
    SEMICOLON = ';',
    COLON = ':',
    APOSTROPHE = '\'',
    QUOTATION = '"',
    BACKTICK = '`',
    TILDE = '~',
    SPACE = ' ',
    NEWLINE = '\n',
    CARRIAGERETURN = '\r',
    TAB = '\t',
    UNPRINTABLE = 0,
    NUMBER,
    IDENTIFIER,
    EOF,
}

struct Token {
    TokenType type;
    ubyte length;
    ushort column;
    uint line;
    const(char)* _text;
    const(char)[] text() {
        return _text[0..length];
    }
}
