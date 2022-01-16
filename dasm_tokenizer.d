import dasm_token;

Token
skip_comment(ref Tokenizer tokenizer, Token tok){
    with(TokenType){
        if(tok.type != POUND) return tok;
        while(tok.type != EOF && tok.type != NEWLINE){
            tok = tokenizer.current_token_and_advance;
        }
        return tok;
    }
}

struct Tokenizer {
    const(char)[] text;
    size_t cursor;
    uint line;
    ushort column;
    Token current_token;
    Token next_token;
    Token current_token_and_advance(){
        auto result = current_token;
        advance();
        return result;
    }

    static
    Tokenizer
    from(const(char)[] text){ with(TokenType){
        Tokenizer result;
        result.text = text;
        result.line = 1;
        result.column = 1;
        result._tokenizer_a_token(&result.current_token);
        if(result.current_token.type == NEWLINE){
            result.line++;
            result.column=1;
            }
        if(result.current_token.type == EOF){
            result.next_token.type = EOF;
            return result;
        }
        result._tokenizer_a_token(&result.next_token);
        return result;
    }
    }
    void
    _tokenizer_a_token(Token* tok){ with(TokenType){
        tok.line = line;
        tok.column = column;
        if(cursor == text.length){
            tok._text = "(EOF)";
            tok.length = 5;
            tok.type = EOF;
            return;
        }
        tok._text = &text[cursor];
        auto first_c = text[cursor];
        switch(first_c){
            // 93
            // 35
            case '_':
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
                tok.type = IDENTIFIER;
                break;
            case '0': .. case '9':
                tok.type = NUMBER;
                break;
            case 0: .. case 8:
            case 11: .. case 12:
            case 14: .. case 31:
            case 127:
                tok.type = UNPRINTABLE;
                break;
            // All other values are valid
            default:
                tok.type = cast(TokenType)first_c;
                break;
        }
        size_t token_length = 1;
        auto cur = cursor + 1;
        ushort col = cast(ushort)(column + 1);
        auto s = text;
        enum MAX_TOK_LENGTH=255;
        if(tok.type == IDENTIFIER){
            for(;token_length < MAX_TOK_LENGTH && cur < s.length; ++token_length, ++cur, ++col){
                auto c = s[cur];
                switch(c){
                    case 0: .. case 47:
                    case 58: .. case 64:
                    case 91: .. case 94:
                    case 96:
                    case 123: .. case 127:
                        break;
                    default:
                    // case 48: .. case 57:
                    // case 65: .. case 90:
                    // case 95:
                    // case 97: .. case 122:
                    // case 128: .. case 255:
                        continue;
                }
                break;
            }
        }
        else if(tok.type == NUMBER){
            for(;token_length < MAX_TOK_LENGTH && cur < s.length; ++token_length, ++cur, ++col){
                auto c = s[cur];
                if((c<='9') & (c >='0') || ((c<='z') & (c>= 'a')) || ((c<='Z') & (c>='A')) || c == '.')
                    {}
                else
                    break;
            }
        }
        else if(tok.type == SPACE){
            for(;token_length < MAX_TOK_LENGTH && cur < s.length; ++token_length, ++cur, ++col){
                auto c = s[cur];
                if(c == ' ')
                    {}
                else
                    break;
            }
        }
        tok.length = cast(ubyte)token_length;
        column = col;
        cursor = cur;
    }
    }
    void
    advance(){ with(TokenType){
        if(current_token.type == EOF)
            return;
        if(next_token.type == EOF){
            current_token = next_token;
            return;
        }
        current_token = next_token;
        if(next_token.type == NEWLINE){
            line++;
            column = 1;
        }
        _tokenizer_a_token(&next_token);
        return;
    }
    }
}

