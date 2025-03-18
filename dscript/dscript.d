module dscript.dscript;
import core.stdc.stdio: fprintf, stderr; // FIXME: should be a callback to report errors
import dlib.aliases;
import dlib.allocator: MALLOCATOR, Allocator;
import dlib.barray: Barray, make_barray;
import dlib.parse_numbers: parse_unsigned_human;
import dlib.table: Table;

enum TokenType: ubyte{
    // Single character tokens
    LEFT_PAREN = '(', RIGHT_PAREN =')', LEFT_BRACE='{', RIGHT_BRACE = '}',
    COMMA = ',', DOT = '.', MINUS ='-', PLUS = '+', SEMICOLON = ';', SLASH = '/', STAR = '*', MOD = '%', AMP = '&', BAR = '|', POUND = '#',

    // One or two character tokens
    BANG = '!', BANG_EQUAL = BANG + 127, EQUAL = '=', EQUAL_EQUAL = EQUAL+127,
    GREATER = '>', GREATER_EQUAL = GREATER + 127, LESS = '<', LESS_EQUAL = LESS + 127,

    MINUS_EQUAL = MINUS + 127,
    PLUS_EQUAL  = PLUS + 127,
    SLASH_EQUAL = SLASH + 127,
    STAR_EQUAL  = STAR + 127,
    MOD_EQUAL   = MOD + 127,
    AMP_EQUAL   = AMP + 127,
    BAR_EQUAL   = BAR + 127,

    // Literals
    IDENTIFIER = 127, STRING = 128, NUMBER = 129, HEX=130, PHEX=131, SNUM=132, BNUM=133,

    // Keywords
    AND = 1, ELSE = 2, FALSE = 3, FUN = 4, FOR = 5, 
    IF = 6, NIL = 7, OR = 8, RETURN = 9, TRUE = 10, LET = 11,
    WHILE = 12, GOTO = 13, LABEL = 14, BREAK = 15, CONTINUE = 16, 
    IMPORT = 17, HALT = 18, ABORT = 19, DASM = 20,

    EOF = 0,
}

struct Token {
    TokenType type;
    str lexeme;
    int line;
}

struct Tokenizer {
    const ubyte[] source;
    Barray!Token* tokens;
    int start = 0, current = 0, line = 1;

    bool ERROR_OCCURRED = false;

    void
    error(int line, str message){
        ERROR_OCCURRED = true;
        fprintf(stderr, "[line %d]: Parse Error: %.*s\n", line, cast(int)message.length, message.ptr);
    }

    bool at_end(){
        return current >= source.length;
    }

    int
    tokenize_tokens(){
        while(!at_end){
            start = current;
            tokenize_token();
            if(ERROR_OCCURRED) return 1;
        }
        *tokens ~= Token(TokenType.EOF, "", line);
        return 0;
    }
    void tokenize_token(){ with(TokenType){
        auto c = advance();
        switch(c){
            case '(':
            case ')':
            case '{':
            case '}':
            case ',':
            case '.':
            case ';':
                add_token(cast(TokenType)c);
                break;
            case '-':
            case '+':
            case '*':
            case '%':
            case '&':
                if(match('&')){
                    add_token(TokenType.AND);
                    break;
                }
                goto case;
            case '|':
                if(match('|')){
                    add_token(TokenType.OR);
                    break;
                }
                goto case;
            case '!':
            case '=':
            case '<':
            case '>':
                add_token(cast(TokenType)(match('=')?127+c:c));
                break;
            case '#':
                while(peek != '\n' && !at_end) current++;
                break;
            case '/':
                if(match('=')){
                    add_token(SLASH_EQUAL);
                    break;
                }
                if(match('/')){
                    // ignore comments
                    while(peek != '\n' && !at_end) current++;
                }
                else {
                    add_token(SLASH);
                }
                break;
            // skip whitespace
            case ' ': case '\r': case '\t':
                break;
            case '\n':
                line++;
                break;
            case '\'':
            case '"':
                do_string(c);
                break;
            case '0': .. case '9':
                do_number(c);
                break;
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
            case '_':
                do_identifier();
                break;
            default:
                error(line, "Unexpected byte");
                break;
        }
    }}

    void do_identifier(){
        while(peek.is_ident_char) current++;
        auto text = cast(str)source[start .. current];
        if(auto type = text in KEYWORDS)
            add_token(*type);
        else
            add_token(TokenType.IDENTIFIER);
    }

    void do_string(char c){
        bool backslash = false;
        while((peek != c || backslash) && !at_end()){
            if(peek == '\n') line++;
            if(peek == '\\') backslash = !backslash;
            else             backslash = false;
            current++;
        }
        if(at_end){
            error(current, "Unterminated string");
            return;
        }
        current++; // closing '"'
        add_token(TokenType.STRING);
    }
    void do_number(ubyte c){
        if(c == '0'){
            if(peek == 'x' || peek == 'X'){
                current++;
                while(peek.is_hex_digit) current++;
                add_token(TokenType.HEX);
                return;
            }
            if(peek == 'p' || peek == 'P'){
                current++;
                while(peek.is_hex_digit) current++;
                add_token(TokenType.PHEX);
                return;
            }
            if(peek == 'b' || peek == 'B'){
                current++;
                while(peek == '0' || peek == '1') current++;
                add_token(TokenType.BNUM);
                return;
            }
            if(peek == 's' || peek == 'S'){
                current++;
                while(peek.is_alpha_numeric) current++;
                add_token(TokenType.SNUM);
                return;
            }
        }
        while(peek.is_digit) current++;
        if(peek == '.'){
            error(current, "Non-integer numbers are not allowed");
            return;
        }
        add_token(TokenType.NUMBER);
    }
    bool match(ubyte c){
        if(at_end) return false;
        if(source[current] != c) return false;
        current++; return true;
    }
    ubyte advance(){
        return source[current++];
    }
    ubyte peek(){
        return at_end? 0: source[current];
    }
    ubyte peekNext(){
        return current +1 >= source.length? 0 : source[current+1];
    }
    // The casts to char[] are safe as we have already scanned those bytes
    // and found them to be valid.
    void add_token(TokenType type){
        auto lex = cast(char[])(source[start .. current]);
        *tokens ~= Token(type, lex, line);
    }
}

bool is_alpha()(ubyte c){
    c |= 32; // ascii tolower
    return c >= 'a' && c <= 'z';
}

bool is_digit()(ubyte c){
    return c >= '0' && c <= '9';
}
bool is_hex_digit()(ubyte c){
    c |= 0x20;
    bool number =  c >= '0' && c <= '9' ;
    bool af = c >= 'a' && c <= 'f';
    return number | af;
}

bool is_alpha_numeric()(ubyte c){
    return is_alpha(c) || is_digit(c);
}
bool is_ident_char()(ubyte c){
    return is_alpha(c) || is_digit(c) || c == '_' || c == '.';
}

__gshared Table!(str, TokenType) KEYWORDS = {data:{allocator:MALLOCATOR}};

__gshared KEYWORDSPOWERED = false;
void powerup(){
    if(KEYWORDSPOWERED) return;
    KEYWORDSPOWERED = true;
    with(TokenType){
        immutable string[20] keys = [
            "and",
            "else",
            "false",
            "for",
            "function",
            "if",
            "nil",
            "or",
            "return",
            "true",
            "let",
            "while",
            "goto",
            "label",
            "break",
            "continue",
            "import",
            "halt",
            "abort",
            "dasm",
        ];
        immutable TokenType[20] values = [
            AND,
            ELSE,
            FALSE,
            FOR,
            FUN,
            IF,
            NIL,
            OR,
            RETURN,
            TRUE,
            LET,
            WHILE,
            GOTO,
            LABEL,
            BREAK,
            CONTINUE,
            IMPORT,
            HALT,
            ABORT,
            DASM,
        ];
        static assert(keys.length == values.length);
        for(size_t i = 0; i < keys.length; i++){
            KEYWORDS[keys[i]] = values[i];
        }
    }
}
void powerdown(){
    if(!KEYWORDSPOWERED) return;
    KEYWORDSPOWERED = false;
    KEYWORDS.cleanup;
}

// Expressions

enum ExprType {
    ASSIGN,
    BINARY,
    GROUPING,
    LITERAL,
    UNARY,
    VARIABLE,
    LOGICAL,
    CALL,
}


Expr* ungroup(Expr* expr){
    if(expr.type == ExprType.GROUPING){
        return (cast(Grouping*)expr).expression.ungroup;
    }
    return expr;
}

struct Expr{
    ExprType type;
    inout(Assign)* is_assign() inout{
        return type == ExprType.ASSIGN? cast(typeof(return))&this:null;
    }
    alias as_assign = is_assign;
    inout(Binary)* is_binary() inout{
        return type == ExprType.BINARY? cast(typeof(return))&this:null;
    }
    alias as_binary = is_binary;
    inout(Grouping)* is_grouping() inout{
        return type == ExprType.GROUPING? cast(typeof(return))&this:null;
    }
    alias as_grouping = is_grouping;
    inout(Literal)* is_literal() inout{
        return type == ExprType.LITERAL? cast(typeof(return))&this:null;
    }
    alias as_literal = is_literal;
    inout(Unary)* is_unary() inout{
        return type == ExprType.UNARY? cast(typeof(return))&this:null;
    }
    alias as_unary = is_unary;
    inout(VarExpr)* is_variable() inout{
        return type == ExprType.VARIABLE? cast(typeof(return))&this:null;
    }
    alias as_variable = is_variable;
    inout(Logical)* is_logical() inout{
        return type == ExprType.LOGICAL? cast(typeof(return))&this:null;
    }
    alias as_logical = is_logical;
    inout(Call)* is_call() inout{
        return type == ExprType.CALL? cast(typeof(return))&this:null;
    }
    alias as_call = is_call;

    void dump(){
        final switch(type)with(ExprType){
        case ASSIGN   : (cast(Assign*)&this).dump; break;
        case BINARY   : (cast(Binary*)&this).dump; break;
        case GROUPING : (cast(Grouping*)&this).dump; break;
        case LITERAL  : (cast(Literal*)&this).dump; break;
        case UNARY    : (cast(Unary*)&this).dump; break;
        case VARIABLE : (cast(VarExpr*)&this).dump; break;
        case LOGICAL  : (cast(Logical*)&this).dump; break;
        case CALL     : (cast(Call*)&this).dump; break;
        }
    }
}
struct Assign {
    Expr exp;
    Token name;
    Expr* right_;
    Expr* right(){
        return right_.ungroup;
    }
    static Expr*
    make(Allocator a, Token o, Expr* r){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.ASSIGN;
        result.name = o;
        result.right_ = r;
        return &result.exp;
    }
    static
    Assign
    make(Token o, Expr* r){
        return Assign(Expr(ExprType.ASSIGN), o, r);
    }

    void dump(){
        fprintf(stderr, "Assign(");
        fprintf(stderr, "%.*s = ", cast(int)name.lexeme.length, name.lexeme.ptr);
        right_.dump;
        fprintf(stderr, ")");
    }
}

struct Call {
    Expr exp;
    Expr* callee_;
    Expr* callee(){return callee_.ungroup;}
    Token paren;
    Expr*[] args;
    static Expr*
    make(Allocator a, Expr* c, Token t, Expr*[] args){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.CALL;
        result.callee_ = c;
        result.paren = t;
        result.args = args;
        return &result.exp;
    }
    void dump(){
        fprintf(stderr, "Call(");
        callee_.dump;
        fprintf(stderr, "(");
        foreach(arg; args){
            arg.dump;
        fprintf(stderr, ", ");
        }
        fprintf(stderr, ")");
        fprintf(stderr, ")");
    }
}

struct Binary {
    Expr exp;
    Expr* left_;
    Expr* left(){return left_.ungroup;}
    Token operator;
    Expr* right_;
    Expr* right(){return right_.ungroup;}
    static Expr*
    make(Allocator a, Expr* l, Token o, Expr* r){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.BINARY;
        result.left_ = l;
        result.operator = o;
        result.right_ = r;
        return &result.exp;
    }
    void dump(){
        fprintf(stderr, "Binary(");
        left_.dump;
        fprintf(stderr, "%.*s", cast(int)operator.lexeme.length, operator.lexeme.ptr);
        right_.dump;
        fprintf(stderr, ")");
    }
}

struct Grouping {
    Expr exp;
    Expr* expression;
    static Expr*
    make(Allocator a, Expr* e){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.GROUPING;
        result.expression = e;
        return &result.exp;
    }
    void dump(){
        fprintf(stderr, "Group(");
        expression.dump;
        fprintf(stderr, ")");
    }
}

struct Literal {
    Expr exp;
    Token value;

    size_t as_number(){
        switch(value.type)with(TokenType){
            case NUMBER:
            case PHEX:
            case HEX:
            case BNUM:
                return parse_unsigned_human(value.lexeme).value;
            case SNUM:
                return 1; // FIXME
            default:
                return 0;
        }
    }
    static Expr*
    make(Allocator a, Token t){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.LITERAL;
        result.value = t;
        return &result.exp;
    }
    void dump(){
        fprintf(stderr, "%.*s", cast(int)value.lexeme.length, value.lexeme.ptr);
    }
}

__gshared Literal NilExpr_ = {
    exp: {ExprType.LITERAL},
    value: Token(TokenType.NIL, "nil", 0),
};

struct Logical {
    Expr exp;
    Expr* left_;
    Expr* left(){return left_.ungroup;}
    Token operator;
    Expr* right_;
    Expr* right(){return right_.ungroup;}
    static Expr*
    make(Allocator a, Expr* l, Token o, Expr* r){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.LOGICAL;
        result.left_ = l;
        result.operator = o;
        result.right_ = r;
        return &result.exp;
    }
    void dump(){
        fprintf(stderr, "Logical(");
        left_.dump;
        fprintf(stderr, "%.*s", cast(int)operator.lexeme.length, operator.lexeme.ptr);
        right_.dump;
        fprintf(stderr, ")");
    }
}

struct Unary {
    Expr exp;
    Token operator;
    Expr* right_;
    Expr* right(){return right_.ungroup;}
    static Expr*
    make(Allocator a, Token t, Expr* e){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.UNARY;
        result.operator = t;
        result.right_ = e;
        return &result.exp;
    }
    void dump(){
        fprintf(stderr, "Unary(");
        fprintf(stderr, "%.*s", cast(int)operator.lexeme.length, operator.lexeme.ptr);
        right_.dump;
        fprintf(stderr, ")");
    }
}

struct VarExpr {
    Expr exp;
    Token name;
    static Expr*
    make(Allocator a, Token t){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.VARIABLE;
        result.name = t;
        return &result.exp;
    }
    void dump(){
        fprintf(stderr, "Var(");
        fprintf(stderr, "%.*s", cast(int)name.lexeme.length, name.lexeme.ptr);
        fprintf(stderr, ")");
    }
}

// Statements
enum StatementType {
    EXPRESSION,
    LET,
    BLOCK,
    IF,
    WHILE,
    FUNCTION,
    RETURN,
    GOTO,
    LABEL,
    IMPORT,
    HALT,
    ABORT,
    DASM,
    BREAK,
    CONTINUE,
}

struct Statement {
    StatementType type;
}

struct DasmStatement {
    Statement stmt;
    Token dasm;
    static
    Statement* make(Allocator allocator, Token dasm){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.DASM;
        p.dasm = dasm;
        return &p.stmt;
    }
}

struct ReturnStatement {
    Statement stmt;
    Token keyword;
    Expr* value;
    static
    Statement* make(Allocator allocator, Token k, Expr* v){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.RETURN;
        p.keyword = k;
        p.value = v;
        return &p.stmt;
    }
}

struct HaltStatement {
    Statement stmt;

    static
    Statement*
    get(){
        return cast(Statement*)&hlt_stmt;
    }
}

immutable HaltStatement hlt_stmt = HaltStatement(Statement(StatementType.HALT));

struct AbortStatement {
    Statement stmt;

    static
    Statement*
    get(){
        return cast(Statement*)&abort_stmt;
    }
}
immutable AbortStatement abort_stmt = AbortStatement(Statement(StatementType.ABORT));



struct FuncStatement {
    Statement stmt;
    Token name;
    Token[] params;
    Statement*[] body;
    static
    Statement*
    make(Allocator allocator, Token n, Token[] params, Statement*[] s){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.FUNCTION;
        p.name = n;
        p.params = params;
        p.body = s;
        return &p.stmt;
    }
}

struct ImportStatement {
    Statement stmt;
    Token name;
    static
    Statement*
    make(Allocator allocator, Token n){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.IMPORT;
        p.name = n;
        return &p.stmt;
    }
}

struct WhileStatement {
    Statement stmt;
    Expr* condition;
    Statement* statement;

    static
    Statement*
    make(Allocator allocator, Expr* c, Statement* s){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.WHILE;
        p.condition = c;
        p.statement = s;
        return &p.stmt;
    }
}

struct IfStmt {
    Statement stmt;
    Expr* condition;
    Statement* then_branch;
    Statement* else_branch;

    static
    Statement*
    make(Allocator allocator, Expr* c, Statement* t, Statement* e){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.IF;
        p.condition = c;
        p.then_branch = t;
        p.else_branch = e;
        return &p.stmt;
    }
}


struct Block {
    Statement stmt;
    Statement*[] statements;

    static
    Statement*
    make(Allocator allocator, Statement*[] s){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.BLOCK;
        p.statements = s;
        return &p.stmt;
    }
}

struct ExpressionStmt {
    Statement stmt;
    Expr* expr;

    static
    Statement*
    make(Allocator allocator, Expr* e){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.EXPRESSION;
        p.expr = e;
        return &p.stmt;
    }
}

struct LetStmt {
    Statement stmt;
    Token name;
    Expr* initializer;

    static
    Statement*
    make(Allocator allocator, Token t, Expr* i){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.LET;
        p.name = t;
        p.initializer = i;
        return &p.stmt;
    }
}

struct BreakStatement {
    Statement stmt;
    static
    Statement*
    get(){
        return cast(Statement*)&break_stmt;
    }
}
immutable BreakStatement break_stmt = {stmt:{type:StatementType.BREAK}};

struct ContinueStatement {
    Statement stmt;
    static
    Statement*
    get(){
        return cast(Statement*)&continue_stmt;
    }
}
immutable ContinueStatement continue_stmt = {stmt:{type:StatementType.CONTINUE}};

struct GotoStatement {
    Statement stmt;
    Token label;

    static
    Statement*
    make(Allocator allocator, Token t){
        void[] data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.GOTO;
        p.label = t;
        return &p.stmt;
    }
}
struct LabelStatement {
    Statement stmt;
    Token label;

    static
    Statement*
    make(Allocator allocator, Token t){
        void[] data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.LABEL;
        p.label = t;
        return &p.stmt;
    }
}

// Parser

struct Parser {
    Allocator allocator;
    Token[] tokens;
    int current = 0;
    int funcdepth = 0;
    int loop_depth = 0;
    bool ERROR_OCCURRED = false;

    void
    error(Token token, str message){
        ERROR_OCCURRED = true;
        int line = token.line;
        if(token.type == TokenType.EOF)
            fprintf(stderr, "[line %d]: Parse Error at end: %.*s\n", line, cast(int)message.length, message.ptr);
        else
            fprintf(stderr, "[line %d]: Parse Error at '%.*s': %.*s\n", line, cast(int)token.lexeme.length, token.lexeme.ptr, cast(int)message.length, message.ptr);
    }

    int
    parse(R)(R* result){
        while(!at_end){
            auto s = declaration();
            if(ERROR_OCCURRED) return 1;
            *result ~= s;
        }
        return 0;
    }

    Statement*
    declaration(){
        if(match(TokenType.FUN)) return function_();
        if(match(TokenType.LET)) return varDeclaration();
        if(match(TokenType.IMPORT)) return import_statement;
        if(match(TokenType.DASM)) return dasm_statement;
        return statement();
    }

    Statement*
    import_statement(){with(TokenType){
        Token name = consume(IDENTIFIER, "Expected import name");
        if(ERROR_OCCURRED) return null;
        consume_or_nl(SEMICOLON, "Expected semicolon");
        if(ERROR_OCCURRED) return null;
        return ImportStatement.make(allocator, name);
    }
    }
    Statement*
    dasm_statement(){with(TokenType){
        Token dasm = consume(STRING, "Expected dasm string");
        if(ERROR_OCCURRED) return null;
        consume_or_nl(SEMICOLON, "Expected semicolon");
        if(ERROR_OCCURRED) return null;
        return DasmStatement.make(allocator, dasm);
    }
    }

    Statement*
    function_(){with(TokenType){
        funcdepth++;
        scope(exit) funcdepth--;
        Token name = consume(IDENTIFIER, "Expect function name");
        if(ERROR_OCCURRED) return null;
        consume(LEFT_PAREN, "Expected a paren noob");
        if(ERROR_OCCURRED) return null;
        auto params = make_barray!Token(allocator);
        if(!check(RIGHT_PAREN)){
            do {
                if(params.count >= 255){
                    error(peek(), "Too many params, can only have 255");
                    return null;
                }
                params ~= consume(IDENTIFIER, "Expect parameter name");
                if(ERROR_OCCURRED) return null;
            }while(match(COMMA));
        }
        consume(RIGHT_PAREN, "Expect ')' after params");
        if(ERROR_OCCURRED) return null;
        consume(LEFT_BRACE, "Expect {");
        if(ERROR_OCCURRED) return null;
        auto bod = make_barray!(Statement*)(allocator);
        int err = parse_statement_list(&bod);
        if(err) return null;
        return FuncStatement.make(allocator, name, params[], bod[]);
    }}

    Statement*
    varDeclaration(){
        Token name = consume(TokenType.IDENTIFIER, "Expect variable name");
        if(ERROR_OCCURRED) return null;
        Expr* initializer = &NilExpr_.exp;
        if(match(TokenType.EQUAL)){
            initializer = expression();
            if(!initializer) return null;
        }
        consume_or_nl(TokenType.SEMICOLON, "Expect ';' after var decl");
        return LetStmt.make(allocator, name, initializer);

    }
    Statement*
    statement(){
        if(match(TokenType.HALT)){
            consume_or_nl(TokenType.SEMICOLON, "Expect ';' after halt");
            if(ERROR_OCCURRED) return null;
            return HaltStatement.get();
        }
        if(match(TokenType.ABORT)){
            consume_or_nl(TokenType.SEMICOLON, "Expect ';' after abort");
            if(ERROR_OCCURRED) return null;
            return AbortStatement.get();
        }
        if(match(TokenType.RETURN)) return returnStatement();
        if(match(TokenType.FOR)) return forStatement();
        if(match(TokenType.IF)) return ifStatement();
        if(match(TokenType.WHILE)) return whileStatement();
        if(match(TokenType.LEFT_BRACE)) return block();
        if(match(TokenType.LABEL)) return label();
        if(match(TokenType.GOTO)) return goto_();
        if(match(TokenType.CONTINUE)) return continue_();
        if(match(TokenType.BREAK)) return break_();
        return expressionStatement();
    }

    Statement*
    label(){
        Token target = consume(TokenType.IDENTIFIER, "Expect label name");
        if(ERROR_OCCURRED) return null;
        consume_or_nl(TokenType.SEMICOLON, "Expect ';' after label decl");
        return LabelStatement.make(allocator, target);
    }

    Statement*
    goto_(){
        Token target = consume(TokenType.IDENTIFIER, "Expect goto target");
        if(ERROR_OCCURRED) return null;
        consume_or_nl(TokenType.SEMICOLON, "Expect ';' after goto target");
        return GotoStatement.make(allocator, target);
    }

    Statement*
    continue_(){
        if(!loop_depth){
            error(previous, "Can't continue outside of a loop");
            return null;
        }
        consume_or_nl(TokenType.SEMICOLON, "Expect ';' after continue");
        return ContinueStatement.get();
    }
    Statement*
    break_(){
        if(!loop_depth){
            error(previous, "Can't break outside of a loop");
            return null;
        }
        consume_or_nl(TokenType.SEMICOLON, "Expect ';' after continue");
        return BreakStatement.get();
    }

    Statement*
    returnStatement(){
        if(!funcdepth) {
            error(previous, "Can't return at global scope");
            return null;
        }
        Token keyword = previous;
        Expr* value = &NilExpr_.exp;
        if(!check(TokenType.SEMICOLON) && previous().line == peek().line){
            value = expression();
            if(value is null) return null;
        }
        consume_or_nl(TokenType.SEMICOLON, "Expect ';' after return");
        if(ERROR_OCCURRED) return null;
        return ReturnStatement.make(allocator, keyword, value);
    }

    Statement*
    forStatement(){
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
        Statement* initializer;
        if(match(TokenType.SEMICOLON))
            initializer = null;
        else if(match(TokenType.LET)){
            initializer = varDeclaration();
            if(initializer is null) return null;
        }else {
            initializer = expressionStatement();
            if(initializer is null) return null;
        }
        Expr* condition = null;
        if(!check(TokenType.SEMICOLON)){
            condition = expression();
            if(condition is null) return null;
        }
        consume(TokenType.SEMICOLON, "expect ';' after loop condition");
        Expr* increment = null;
        if(!check(TokenType.RIGHT_PAREN)){
            increment = expression();
            if(increment is null) return null;
        }
        consume(TokenType.RIGHT_PAREN, "Expect a ')' after for clauses");
        loop_depth++;
        Statement* body = statement();
        loop_depth--;
        if(body is null) return null;
        if(increment !is null){
            void[] p = allocator.alloc((Statement*).sizeof*2);
            Statement*[] s = cast(Statement*[])p;
            s[0] = body;
            s[1] = ExpressionStmt.make(allocator, increment);
            body = Block.make(allocator, s);
        }
        if(condition is null) condition = Literal.make(allocator, Token(TokenType.TRUE, "true", 0));
        body = WhileStatement.make(allocator, condition, body);
        if(initializer !is null) {
            auto p = allocator.alloc((Statement*).sizeof*2);
            Statement*[] s = (cast(Statement**)p.ptr)[0 .. 2];
            s[0] = initializer;
            s[1] = body;
            body = Block.make(allocator, s);
        }
        return body;

    }

    Statement*
    whileStatement(){
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
        if(ERROR_OCCURRED) return null;
        Expr* condition = expression();
        if(condition is null) return null;
        consume(TokenType.RIGHT_PAREN, "Expect ')' after condition of 'while'");
        if(ERROR_OCCURRED) return null;
        loop_depth++;
        Statement* stmt = statement();
        loop_depth--;
        if(stmt is null) return null;
        return WhileStatement.make(allocator, condition, stmt);
    }

    Statement*
    ifStatement(){
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
        if(ERROR_OCCURRED) return null;
        auto condition = expression();
        if(condition is null) return null;
        consume(TokenType.RIGHT_PAREN, "Expect ')' after condition of 'if'");
        if(ERROR_OCCURRED) return null;
        Statement* then = statement();
        if(then is null) return null;
        Statement* else_ = null;
        if(match(TokenType.ELSE)){
            else_ = statement();
            if(else_ is null) return null;
        }
        return IfStmt.make(allocator, condition, then, else_);
    }

    int
    parse_statement_list(Barray!(Statement*)* stmts){
        while(!check(TokenType.RIGHT_BRACE) && !at_end){
            auto d = declaration();
            if(d is null) return 1;
            *stmts ~= d;
        }
        consume(TokenType.RIGHT_BRACE, "Expect '}' after block");
        if(ERROR_OCCURRED) return 1;
        return 0;
    }

    Statement*
    block(){
        auto stmts = make_barray!(Statement*)(allocator);
        int err = parse_statement_list(&stmts);
        if(err) return null;
        return Block.make(allocator, stmts[]);
    }


    Statement*
    expressionStatement(){
        auto value = expression();
        consume_or_nl(TokenType.SEMICOLON, "Expected ';' after value");
        return ExpressionStmt.make(allocator, value);
    }

    Expr*
    comma(){
        Expr* expr = equality();
        while(match(TokenType.COMMA)){
            Token operator = previous;
            Expr* right = equality();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }

    Expr*
    expression(){
        return assignment();
        // return comma();
        // return equality();
    }

    Expr*
    or(){
        Expr* expr = and();
        if(expr is null) return null;
        while(match(TokenType.OR)){
            Token operator = previous;
            Expr* right = and();
            if(right is null) return null;
            expr = Logical.make(allocator, expr, operator, right);
        }
        return expr;
    }
    Expr*
    and(){
        Expr* expr = equality();
        if(expr is null) return null;
        while(match(TokenType.AND)){
            Token operator = previous;
            Expr* right = equality();
            if(right is null) return null;
            expr = Logical.make(allocator, expr, operator, right);
        }
        return expr;
    }
    Expr*
    assignment(){
        Expr* expr = or();
        if(expr is null) return null;
        if(match(TokenType.EQUAL)){
            Token equals = previous;
            Expr* value = assignment();
            if(value is null) return null;
            if(expr.type == ExprType.VARIABLE){
                Token name = (cast(VarExpr*)expr).name;
                return Assign.make(allocator, name, value);
            }
            error(equals, "Invalid assignment target");
            return null;
        }
        return expr;
    }
    Expr*
    equality(){with(TokenType){
        Expr* expr = comparison();

        while(match(BANG_EQUAL, EQUAL_EQUAL)){
            Token operator = previous;
            Expr* right = comparison();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}

    Expr*
    comparison(){with(TokenType){
        Expr* expr = term();
        while(match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)){
            Token operator = previous;
            Expr* right = term();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}
    Expr*
    term(){with(TokenType){
        Expr* expr = factor();
        while(match(MINUS, PLUS)){
            Token operator = previous;
            Expr* right = factor();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}
    Expr*
    bitops(){with(TokenType){
        Expr* expr = unary();
        while(match(BAR, AMP)){
            import core.stdc.stdio;
            Token operator = previous;
            Expr* right = unary();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}
    Expr*
    factor(){with(TokenType){
        Expr* expr = bitops();
        while(match(STAR, SLASH, MOD)){
            Token operator = previous;
            Expr* right = bitops();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}

    Expr*
    unary(){with(TokenType){
        if(match(PLUS)) return unary();
        if(match(BANG, MINUS, AMP)){
            Token operator = previous();
            Expr* right = unary();
            return Unary.make(allocator, operator, right);
        }
        return call();
    }}

    Expr*
    call(){
        Expr* expr = primary;
        if(expr is null) return null;
        for(;;){
            if(match(TokenType.LEFT_PAREN)){
                expr = finish_call(expr);
                if(expr is null) return null;
            }
            else {
                break;
            }
        }
        return expr;
    }

    Expr*
    finish_call(Expr* callee){
        Barray!(Expr*) args = make_barray!(Expr*)(allocator);
        if(!check(TokenType.RIGHT_PAREN)){
            do {
                Expr* e = expression;
                if(e is null) return null;
                args ~= e;
                if(args.count >= 255){
                    error(peek, "Can't have more than 255 args");
                    return null;
                }
            }while(match(TokenType.COMMA));
        }
        if(args.count) args.bdata.resize(args.count);
        Token paren = consume(TokenType.RIGHT_PAREN, "Expect a ')' after args");
        if(ERROR_OCCURRED) return null;
        return Call.make(allocator, callee, paren, args[]);
    }

    Expr*
    primary(){with(TokenType){
        if(match(FALSE, TRUE, NIL, NUMBER, STRING, HEX, PHEX, SNUM, BNUM)) return Literal.make(allocator, previous);
        if(match(IDENTIFIER)){
            return VarExpr.make(allocator, previous);
        }
        if(match(LEFT_PAREN)){
            Expr* expr = expression();
            consume(RIGHT_PAREN, "Expect ')' after expression");
            return Grouping.make(allocator, expr);
        }
        error(peek(), "Expect expression");
        return null;
    }}

    Token
    consume(TokenType type, str message){
        if(check(type)) return advance();
        error(peek(), message);
        return advance();
    }

    Token
    consume_or_nl(TokenType type, str message){
        if(previous.line != peek.line) return peek();
        if(check(type)) return advance();
        error(peek(), message);
        return advance();
    }

    bool
    match(TokenType[] types...){
        foreach(type; types){
            if(check(type)){
                advance();
                return true;
            }
        }
        return false;
    }
    bool
    check(TokenType type){
        if(at_end) return false;
        return peek.type == type;
    }

    Token
    advance(){
        if(!at_end) current++;
        return previous;
    }

    bool
    at_end(){
        return peek.type == TokenType.EOF;
    }

    Token
    peek(){
        return tokens[current];
    }
    Token
    previous(){
        return tokens[current-1];
    }
}

