import core.stdc.stdio: fprintf, stdout, stderr, stdin, fread;
import dlib.allocator: Mallocator, ArenaAllocator, LoggingAllocator, LinkAllocator;
import dlib.box: Box;
import dlib.stringbuilder: StringBuilder, P;
import dlib.barray: Barray, Array, make_barray;
import dlib.parse_numbers: parse_unsigned_human;
import dlib.btable: BTable, Table;
import dlib.bettercobject: BCObject;
static import core.time;
alias str = const(char)[];

int
compile_to_dasm(const ubyte[] source, Box!(char[], Mallocator)* progtext){
    ArenaAllocator!(Mallocator) arena;
    scope(exit) arena.free_all;
    auto tokens = make_barray!Token(&arena);
    auto tokenizer = Tokenizer!(typeof(tokens))(source, &tokens);
    int err = tokenizer.tokenizeTokens();
    if(err) return 1;
    tokens.bdata.resize(tokens.count);
    auto parser = Parser!(typeof(arena))(&arena, tokens[]);
    auto statements = make_barray!(Statement*)(&arena);
    err = parser.parse(&statements);
    if(err) return 1;
    StringBuilder!Mallocator sb;
    scope writer = new DasmWriter!(typeof(sb), typeof(arena))(&sb, &arena);
    err = writer.do_it(statements[]);
    writer.cleanup;
    if(err){
        sb.cleanup;
        return err;
    }
    *progtext = sb.detach;
    return 0;
}

enum TokenType: ubyte{
    // Single character tokens
    LEFT_PAREN = '(', RIGHT_PAREN =')', LEFT_BRACE='{', RIGHT_BRACE = '}',
    COMMA = ',', DOT = '.', MINUS ='-', PLUS = '+', SEMICOLON = ';', SLASH = '/', STAR = '*', MOD = '%',

    // One or two character tokens
    BANG = '!', BANG_EQUAL = BANG + 127, EQUAL = '=', EQUAL_EQUAL = EQUAL+127,
    GREATER = '>', GREATER_EQUAL = GREATER + 127, LESS = '<', LESS_EQUAL = LESS + 127,

    // Literals
    IDENTIFIER = 127, STRING = 128, NUMBER = 129,

    // Keywords
    AND, ELSE, FALSE, FUN, FOR, IF, NIL, OR, RETURN, TRUE, LET,
    WHILE, GOTO, LABEL, BREAK, CONTINUE,

    EOF = 0,
}

struct Token {
    TokenType type;
    str lexeme;
    union {
        ulong number;
        str string_;
    }
    int line;
    this(TokenType ty, str lex, ulong num, int l){
        type = ty, number = num, line = l, lexeme=lex;
    }
    this(TokenType ty, str lex, str string__, int l){
        type = ty, string_ = string__, line = l, lexeme=lex;
    }
    this(TokenType ty, str lex, int l){
        type = ty, line = l, lexeme = lex;
    }
}

struct Tokenizer(B) {
    @disable this();
    const ubyte[] source;
    B* tokens;
    int start = 0, current = 0, line = 1;

    bool ERROR_OCCURRED = false;

    void
    error(int line, str message){
        ERROR_OCCURRED = true;
        fprintf(stderr, "[line %d]: Parse Error: %.*s\n", line, cast(int)message.length, message.ptr);
    }

    this(const ubyte[] s, B* toks){
        source = s;
        tokens = toks;
    }

    bool isAtEnd(){
        return current >= source.length;
    }

    int
    tokenizeTokens(){
        while(!isAtEnd){
            start = current;
            tokenizeToken();
            if(ERROR_OCCURRED) return 1;
        }
        *tokens ~= Token(TokenType.EOF, "", line);
        return 0;
    }
    void tokenizeToken(){ with(TokenType){
        auto c = advance();
        switch(c){
            case '(':
            case ')':
            case '{':
            case '}':
            case ',':
            case '.':
            case '-':
            case '+':
            case ';':
            case '*':
            case '%':
                addToken(cast(TokenType)c);
                break;
            case '!':
                addToken(match('=')?BANG_EQUAL:BANG);
                break;
            case '=': addToken(match('=')? EQUAL_EQUAL:   EQUAL);   break;
            case '<': addToken(match('=')? LESS_EQUAL:    LESS);    break;
            case '>': addToken(match('=')? GREATER_EQUAL: GREATER); break;
            case '/':
                if(match('/')){
                    // ignore comments
                    while(peek != '\n' && !isAtEnd) current++;
                }
                else {
                    addToken(SLASH);
                }
                break;
            // skip whitespace
            case ' ': case '\r': case '\t':
                break;
            case '\n':
                line++;
                break;
            case '"':
                do_string();
                break;
            case '0': .. case '9':
                do_number();
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
        while(peek.isIdentChar) current++;
        auto text = cast(const(char)[])source[start .. current];
        if(auto type = text in KEYWORDS)
            addToken(*type);
        else
            addToken(TokenType.IDENTIFIER);
    }

    void do_string(){
        bool backslash = false;
        while((peek != '"' || backslash) && !isAtEnd()){
            if(peek == '\n') line++;
            if(peek == '\\') backslash = !backslash;
            else             backslash = false;
            current++;
        }
        if(isAtEnd){
            error(current, "Unterminated string");
            return;
        }
        current++; // closing '"'
        auto value = cast(const char[])source[start+1 .. current-1];
        addToken(TokenType.STRING, value);
    }
    void do_number(){
        while(peek.isDigit) current++;
        if(peek == '.'){
            error(current, "Non-integer numbers are not allowed");
            return;
        }
        auto slice = cast(const(char)[])source[start .. current];
        auto res = parse_unsigned_human(slice);
        if(res.errored){
            error(current, "Unable to parse number");
            return;
        }
        addToken(TokenType.NUMBER, res.value);
    }
    bool match(ubyte c){
        if(isAtEnd) return false;
        if(source[current] != c) return false;
        current++; return true;
    }
    ubyte advance(){
        return source[current++];
    }
    ubyte peek(){
        return isAtEnd? 0: source[current];
    }
    ubyte peekNext(){
        return current +1 >= source.length? 0 : source[current+1];
    }
    // The casts to char[] are safe as we have already scanned those bytes
    // and found them to be valid.
    void addToken(TokenType type){
        auto lex = cast(char[])(source[start .. current]);
        *tokens ~= Token(type, lex, line);
    }
    void addToken(TokenType type, str string_){
        auto lex = cast(char[])(source[start .. current]);
        *tokens ~= Token(type, lex, string_, line);
    }
    void addToken(TokenType type, ulong num){
        auto lex = cast(char[])(source[start .. current]);
        *tokens ~= Token(type, lex, num, line);
    }
}

bool isAlpha()(ubyte c){
    c |= 32; // ascii tolower
    return c >= 'a' && c <= 'z';
}

bool isDigit()(ubyte c){
    return c >= '0' && c <= '9';
}

bool isAlphaNumeric()(ubyte c){
    return isAlpha(c) || isDigit(c);
}
bool isIdentChar()(ubyte c){
    return isAlpha(c) || isDigit(c) || c == '_';
}

__gshared BTable!(str, TokenType, Mallocator) KEYWORDS;

__gshared KEYWORDSPOWERED = false;
void powerup(){
    if(KEYWORDSPOWERED) return;
    KEYWORDSPOWERED = true;
    with(TokenType){
        immutable string[16] keys = [
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
        ];
        immutable TokenType[16] values = [
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

interface Visitor(R) {
    R visit(Binary* expr);
    R visit(Call* expr);
    R visit(Grouping* expr);
    R visit(Literal* expr);
    R visit(Unary* expr);
    R visit(VarExpr* expr);
    R visit(Assign* expr);
    R visit(Logical* expr);
}
interface RegVisitor(R) {
    R visit(Binary* expr, int target);
    R visit(Call* expr, int target);
    R visit(Grouping* expr, int target);
    R visit(Literal* expr, int target);
    R visit(Unary* expr, int target);
    R visit(VarExpr* expr, int target);
    R visit(Assign* expr, int target);
    R visit(Logical* expr, int target);
}

struct Expr{
    ExprType type;
    R accept(R)(Visitor!R visitor){
        final switch(type)with(ExprType){
        case ASSIGN   : return visitor.visit(cast(Assign*)&this);
        case BINARY   : return visitor.visit(cast(Binary*)&this);
        case GROUPING : return visitor.visit(cast(Grouping*)&this);
        case LITERAL  : return visitor.visit(cast(Literal*)&this);
        case UNARY    : return visitor.visit(cast(Unary*)&this);
        case VARIABLE : return visitor.visit(cast(VarExpr*)&this);
        case LOGICAL  : return visitor.visit(cast(Logical*)&this);
        case CALL     : return visitor.visit(cast(Call*)&this);
        }
    }
    R accept(R)(RegVisitor!R visitor, int target){
        final switch(type)with(ExprType){
        case ASSIGN   : return visitor.visit(cast(Assign*)&this, target);
        case BINARY   : return visitor.visit(cast(Binary*)&this, target);
        case GROUPING : return visitor.visit(cast(Grouping*)&this, target);
        case LITERAL  : return visitor.visit(cast(Literal*)&this, target);
        case UNARY    : return visitor.visit(cast(Unary*)&this, target);
        case VARIABLE : return visitor.visit(cast(VarExpr*)&this, target);
        case LOGICAL  : return visitor.visit(cast(Logical*)&this, target);
        case CALL     : return visitor.visit(cast(Call*)&this, target);
        }
    }
}
struct Assign {
    Expr exp;
    Token name;
    Expr* right;
    static Expr*
    make(A)(A* a, Token o, Expr* r){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.ASSIGN;
        result.name = o;
        result.right = r;
        return &result.exp;
    }
    static
    Assign
    make(Token o, Expr* r){
        return Assign(Expr(ExprType.ASSIGN), o, r);
    }
}

struct Call {
    Expr exp;
    Expr* callee;
    Token paren;
    Expr*[] args;
    static Expr*
    make(A)(A* a, Expr* c, Token t, Expr*[] args){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.CALL;
        result.callee = c;
        result.paren = t;
        result.args = args;
        return &result.exp;
    }
}

struct Binary {
    Expr exp;
    Expr* left;
    Token operator;
    Expr* right;
    static Expr*
    make(A)(A* a, Expr* l, Token o, Expr* r){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.BINARY;
        result.left = l;
        result.operator = o;
        result.right = r;
        return &result.exp;
    }
}

struct Grouping {
    Expr exp;
    Expr* expression;
    static Expr*
    make(A)(A* a, Expr* e){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.GROUPING;
        result.expression = e;
        return &result.exp;
    }
}

struct Literal {
    Expr exp;
    Token value;
    static Expr*
    make(A)(A* a, Token t){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.LITERAL;
        result.value = t;
        return &result.exp;
    }
}

__gshared Literal NilExpr_ = {
    {ExprType.LITERAL},
    Token(TokenType.NIL, "nil", 0),
};

struct Logical {
    Expr exp;
    Expr* left;
    Token operator;
    Expr* right;
    static Expr*
    make(A)(A* a, Expr* l, Token o, Expr* r){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.LOGICAL;
        result.left = l;
        result.operator = o;
        result.right = r;
        return &result.exp;
    }
}

struct Unary {
    Expr exp;
    Token operator;
    Expr* right;
    static Expr*
    make(A)(A* a, Token t, Expr* e){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.UNARY;
        result.operator = t;
        result.right = e;
        return &result.exp;
    }
}

struct VarExpr {
    Expr exp;
    Token name;
    static Expr*
    make(A)(A* a, Token t){
        auto data = a.alloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.exp.type = ExprType.VARIABLE;
        result.name = t;
        return &result.exp;
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
}

interface StatementVisitor(R){
    R visit(ExpressionStmt* stmt);
    R visit(LetStmt* stmt);
    R visit(Block* stmt);
    R visit(IfStmt* stmt);
    R visit(WhileStatement* stmt);
    R visit(FuncStatement* stmt);
    R visit(ReturnStatement* stmt);
    R visit(GotoStatement* stmt);
    R visit(LabelStatement* stmt);
}

struct Statement {
    StatementType type;
    R accept(R)(StatementVisitor!R visitor){
        final switch(type)with(StatementType){
        case BLOCK      : return visitor.visit(cast(Block*)&this);
        case EXPRESSION : return visitor.visit(cast(ExpressionStmt*)&this);
        case LET        : return visitor.visit(cast(LetStmt*)&this);
        case IF         : return visitor.visit(cast(IfStmt*)&this);
        case WHILE      : return visitor.visit(cast(WhileStatement*)&this);
        case FUNCTION   : return visitor.visit(cast(FuncStatement*)&this);
        case RETURN     : return visitor.visit(cast(ReturnStatement*)&this);
        case GOTO       : return visitor.visit(cast(GotoStatement*)&this);
        case LABEL      : return visitor.visit(cast(LabelStatement*)&this);
        }
    }
}

struct ReturnStatement {
    Statement stmt;
    Token keyword;
    Expr* value;
    static
    Statement* make(A)(A* allocator, Token k, Expr* v){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.RETURN;
        p.keyword = k;
        p.value = v;
        return &p.stmt;
    }
}



struct FuncStatement {
    Statement stmt;
    Token name;
    Token[] params;
    Statement*[] body;
    static
    Statement*
    make(A)(A* allocator, Token n, Token[] params, Statement*[] s){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.FUNCTION;
        p.name = n;
        p.params = params;
        p.body = s;
        return &p.stmt;
    }
}

struct WhileStatement {
    Statement stmt;
    Expr* condition;
    Statement* statement;

    static
    Statement*
    make(A)(A* allocator, Expr* c, Statement* s){
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
    Statement* thenBranch;
    Statement* elseBranch;

    static
    Statement*
    make(A)(A* allocator, Expr* c, Statement* t, Statement* e){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.IF;
        p.condition = c;
        p.thenBranch = t;
        p.elseBranch = e;
        return &p.stmt;
    }
}


struct Block {
    Statement stmt;
    Statement*[] statements;

    static
    Statement*
    make(A)(A* allocator, Statement*[] s){
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
    make(A)(A* allocator, Expr* e){
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
    make(A)(A* allocator, Token t, Expr* i){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.LET;
        p.name = t;
        p.initializer = i;
        return &p.stmt;
    }
}

struct GotoStatement {
    Statement stmt;
    Token label;

    static
    Statement*
    make(A)(A* allocator, Token t){
        auto data = allocator.alloc(typeof(this).sizeof);
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
    make(A)(A* allocator, Token t){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.LABEL;
        p.label = t;
        return &p.stmt;
    }
}

// Parser

struct Parser(A) {
    A* allocator;
    Token[] tokens;
    int current = 0;
    int funcdepth = 0;
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

    int parse(R)(R* result){
        while(!isAtEnd){
            auto s = declaration();
            if(ERROR_OCCURRED) return 1;
            *result ~= s;
        }
        return 0;
    }

    Statement* declaration(){
        if(match(TokenType.FUN)) return function_!"function"();
        if(match(TokenType.LET)) return varDeclaration();
        return statement();
    }

    Statement* function_(string kind)(){with(TokenType){
        funcdepth++;
        scope(exit) funcdepth--;
        Token name = consume(IDENTIFIER, "Expect " ~ kind ~ " name");
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
        int err = parse_statement_list(bod);
        if(err) return null;
        return FuncStatement.make(allocator, name, params[], bod[]);
    }}

    Statement* varDeclaration(){
        Token name = consume(TokenType.IDENTIFIER, "Expect variable name");
        if(ERROR_OCCURRED) return null;
        Expr* initializer = &NilExpr_.exp;
        if(match(TokenType.EQUAL)){
            initializer = expression();
            if(!initializer) return null;
        }
        consume(TokenType.SEMICOLON, "Expect ';' after var decl");
        return LetStmt.make(allocator, name, initializer);

    }
    Statement* statement(){
        if(match(TokenType.RETURN)) return returnStatement();
        if(match(TokenType.FOR)) return forStatement();
        if(match(TokenType.IF)) return ifStatement();
        if(match(TokenType.WHILE)) return whileStatement();
        if(match(TokenType.LEFT_BRACE)) return block();
        if(match(TokenType.LABEL)) return label();
        if(match(TokenType.GOTO)) return goto_();
        return expressionStatement();
    }

    Statement* label(){
        Token target = consume(TokenType.IDENTIFIER, "Expect label name");
        if(ERROR_OCCURRED) return null;
        consume(TokenType.SEMICOLON, "Expect ';' after label decl");
        return LabelStatement.make(allocator, target);
    }
    Statement* goto_(){
        Token target = consume(TokenType.IDENTIFIER, "Expect goto target");
        if(ERROR_OCCURRED) return null;
        consume(TokenType.SEMICOLON, "Expect ';' after goto target");
        return GotoStatement.make(allocator, target);
    }

    Statement* returnStatement(){
        if(!funcdepth) {
            error(previous, "Can't return at global scope");
            return null;
        }
        Token keyword = previous;
        Expr* value = &NilExpr_.exp;
        if(!check(TokenType.SEMICOLON)){
            value = expression();
            if(value is null) return null;
        }
        consume(TokenType.SEMICOLON, "Expect ';' after return");
        if(ERROR_OCCURRED) return null;
        return ReturnStatement.make(allocator, keyword, value);
    }

    Statement* forStatement(){
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
        Statement* body = statement();
        if(body is null) return null;
        if(increment !is null){
            auto p = allocator.alloc((Statement*).sizeof*2);
            Statement*[] s = (cast(Statement**)p.ptr)[0 .. 2];
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

    Statement* whileStatement(){
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
        if(ERROR_OCCURRED) return null;
        auto condition = expression();
        if(condition is null) return null;
        consume(TokenType.RIGHT_PAREN, "Expect ')' after condition of 'while'");
        if(ERROR_OCCURRED) return null;
        auto stmt = statement();
        if(stmt is null) return null;
        return WhileStatement.make(allocator, condition, stmt);
    }

    Statement* ifStatement(){
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

    int parse_statement_list(ref Barray!(Statement*, typeof(*allocator)) stmts){
        while(!check(TokenType.RIGHT_BRACE) && !isAtEnd){
            auto d = declaration();
            if(d is null) return 1;
            stmts ~= d;
        }
        consume(TokenType.RIGHT_BRACE, "Expect '}' after block");
        if(ERROR_OCCURRED) return 1;
        return 0;
    }

    Statement* block(){
        auto stmts = make_barray!(Statement*)(allocator);
        int err = parse_statement_list(stmts);
        if(err) return null;
        return Block.make(allocator, stmts[]);
    }


    Statement* expressionStatement(){
        auto value = expression();
        consume(TokenType.SEMICOLON, "Expected ';' after value");
        return ExpressionStmt.make(allocator, value);
    }

    Expr* comma(){
        Expr* expr = equality();
        while(match(TokenType.COMMA)){
            Token operator = previous;
            Expr* right = equality();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }

    Expr* expression(){
        return assignment();
        // return comma();
        // return equality();
    }

    Expr* or(){
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
    Expr* and(){
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
    Expr* assignment(){
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
    Expr* equality(){with(TokenType){
        Expr* expr = comparison();

        while(match(BANG_EQUAL, EQUAL_EQUAL)){
            Token operator = previous;
            Expr* right = comparison();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}

    Expr* comparison(){with(TokenType){
        Expr* expr = term();
        while(match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)){
            Token operator = previous;
            Expr* right = term();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}
    Expr* term(){with(TokenType){
        Expr* expr = factor();
        while(match(MINUS, PLUS)){
            Token operator = previous;
            Expr* right = factor();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}
    Expr* factor(){with(TokenType){
        Expr* expr = unary();
        while(match(STAR, SLASH, MOD)){
            Token operator = previous;
            Expr* right = unary();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}

    Expr* unary(){with(TokenType){
        if(match(BANG, MINUS)){
            Token operator = previous();
            Expr* right = unary();
            return Unary.make(allocator, operator, right);
        }
        return call();
    }}

    Expr* call(){
        Expr* expr = primary;
        if(expr is null) return null;
        for(;;){
            if(match(TokenType.LEFT_PAREN)){
                expr = finishCall(expr);
                if(expr is null) return null;
            }
            else {
                break;
            }
        }
        return expr;
    }

    Expr* finishCall(Expr* callee){
        auto args = make_barray!(Expr*)(allocator);
        if(!check(TokenType.RIGHT_PAREN)){
            do {
                auto e = expression;
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

    Expr* primary(){with(TokenType){
        if(match(FALSE, TRUE, NIL, NUMBER, STRING)) return Literal.make(allocator, previous);
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

    Token consume(TokenType type, str message){
        if(check(type)) return advance();
        error(peek(), message);
        return advance();
    }

    bool match(TokenType[] types...){
        foreach(type; types){
            if(check(type)){
                advance();
                return true;
            }
        }
        return false;
    }
    bool check(TokenType type){
        if(isAtEnd) return false;
        return peek.type == type;
    }

    Token advance(){
        if(!isAtEnd) current++;
        return previous;
    }

    bool isAtEnd(){
        return peek.type == TokenType.EOF;
    }

    Token peek(){
        return tokens[current];
    }
    Token previous(){
        return tokens[current-1];
    }
}

struct RegisterAllocator {
    int alloced;
    int local_max = 0;
    enum {LOCAL_MAX = 100};
    int allocate(){
        int result = alloced++;
        // fprintf(stderr, "%d: alloced: %d\n", __LINE__, alloced);
        if(result > local_max){
            local_max = result;
            // fprintf(stderr, "New high water mark: %d\n", local_max);
        }
        return result;
    }
    void reset(){
        alloced = 0;
        // fprintf(stderr, "%d: reset: alloced: %d\n", __LINE__, alloced);
    }
    void reset_to(int r){
        alloced = r;
        // fprintf(stderr, "%d: reset_to: alloced: %d\n", __LINE__, alloced);
    }
}
struct StackAllocator {
    int nalloced;
    size_t allocate(){
        int depth = nalloced++;
        return depth;
    }
    void reset(){
        nalloced = 0;
    }
}

struct LabelAllocator {
    int nalloced;
    int allocate(){
        return nalloced++;
    }
    void reset(){
        nalloced = 0;
    }
}

struct Analysis(A) {
    Barray!(str, A) vars;
    bool vars_on_stack;
}
class DasmAnalyzer(A): BCObject, Visitor!void, StatementVisitor!void {
    @disable this();
    Analysis!A analysis;
    this(A* allocator){
        analysis.vars.bdata.allocator = allocator;
    }

    void visit(Binary* expr){
        expr.left.accept(this);
        expr.right.accept(this);
    }
    void visit(Call* expr){
        expr.callee.accept(this);
        foreach(a; expr.args)
            a.accept(this);
    }
    void visit(Grouping* expr){
        expr.expression.accept(this);
    }
    void visit(Literal* expr){
    }
    void visit(Unary* expr){
        expr.right.accept(this);
    }
    void visit(VarExpr* expr){
    }
    void visit(Assign* expr){
        expr.right.accept(this);
    }
    void visit(Logical* expr){
        expr.left.accept(this);
        expr.right.accept(this);
    }
    void visit(ExpressionStmt* stmt){
        stmt.expr.accept(this);
    }
    void visit(LetStmt* stmt){
        stmt.initializer.accept(this);
        analysis.vars ~= stmt.name.lexeme;
    }
    void visit(IfStmt* stmt){
        stmt.condition.accept(this);
        stmt.thenBranch.accept(this);
        if(stmt.elseBranch !is null)
            stmt.elseBranch.accept(this);
    }
    void visit(WhileStatement* stmt){
        stmt.condition.accept(this);
        stmt.statement.accept(this);
    }
    void visit(ReturnStatement* stmt){
        stmt.value.accept(this);
    }
    void visit(Block* stmt){
        foreach(s; stmt.statements)
            s.accept(this);
    }
    void visit(FuncStatement* stmt){
        analysis.vars.clear();
        foreach(s; stmt.body)
            s.accept(this);
    }
    void visit(GotoStatement* stmt){
    }
    void visit(LabelStatement* stmt){
    }

}
class DasmWriter(SB, A): BCObject, RegVisitor!int, StatementVisitor!int {
    A* allocator;
    SB* sb;
    RegisterAllocator regallocator;
    StackAllocator stackallocator;
    LabelAllocator labelallocator;
    Table!(str, int) locals;
    Table!(str, int) reglocals;
    Analysis!A analysis;
    void cleanup(){
        locals.cleanup;
        reglocals.cleanup;
        analysis.vars.cleanup;
    }
    bool ERROR_OCCURRED = false;

    @disable this();
    this(SB* s, A* a){
        allocator = a;
        sb = s;
    }

    void
    error(Token token, str message){
        ERROR_OCCURRED = true;
        int line = token.line;
        fprintf(stderr, "[line %d]: Write Error at '%.*s': %.*s\n", line, cast(int)token.lexeme.length, token.lexeme.ptr, cast(int)message.length, message.ptr);
    }

    void
    error(int line, str message){
        ERROR_OCCURRED = true;
        fprintf(stderr, "[line %d]: Write Error: %.*s\n", line, cast(int)message.length, message.ptr);
    }

    void save_reglocals(){
        for(int i = 0; i < regallocator.alloced; i++){
            sb.writef("  push r%\n", i);
        }
    }
    void restore_reglocals(){
        for(int i = regallocator.alloced-1; i >= 0; i--){
            sb.writef("  pop r%\n", i);
        }
    }

    void restore_stack(){
        sb.write("  move rsp rbp\n");
        sb.write("  pop rbp\n");
    }
    void save_stack(){
        sb.write("  push rbp\n");
        sb.write("  move rbp rsp\n");
    }
    int funcdepth;
    enum {TARGET_IS_CMP_FLAGS = -2};
    enum {TARGET_IS_NOTHING   = -1};
    int binary_cmp(Binary* expr){
        int before = regallocator.alloced;
        scope(exit) regallocator.reset_to(before);
        int lhs = regallocator.allocate();
        int res = expr.left.accept(this, lhs);
        if(res != 0) return res;
        if(expr.right.type == ExprType.LITERAL && (cast(Literal*)expr.right).value.type == TokenType.NUMBER){
            auto lit = cast(Literal*)expr.right;
            auto rhs = cast(size_t)lit.value.number;
            sb.writef("  scmp r% %\n", lhs, rhs);
            return 0;
        }
        else {
            int rhs = regallocator.allocate();
            int res2 = expr.right.accept(this, rhs);
            if(res2 != 0) return res2;
            sb.writef("  scmp r% r%\n", lhs, rhs);
            return 0;
        }
    }
    int visit(Binary* expr, int target){
        if(target == TARGET_IS_CMP_FLAGS){
            return binary_cmp(expr);
        }
        // std.stdio.writefln("HERE: %d", __LINE__);
        int lhs = target;
        int res_ = expr.left.accept(this, lhs);
        if(res_ != 0) return res_;
        if(expr.right.type == ExprType.LITERAL){
            if(target == TARGET_IS_NOTHING)
                return 0;
            auto lit = cast(Literal*)expr.right;
            if(lit.value.type == TokenType.NUMBER){
                auto rhs = cast(size_t) lit.value.number;
                switch(expr.operator.type)with(TokenType){
                    case MINUS:
                        sb.writef("  sub r% r% %\n", target, lhs, rhs);
                        break;
                    case PLUS:
                        sb.writef("  add r% r% %\n", target, lhs, rhs);
                        break;
                    case STAR:
                        sb.writef("  mul r% r% %\n", target, lhs, rhs);
                        break;
                    case SLASH:
                        sb.writef("  div r% rjunk r% %\n", target, lhs, rhs);
                        break;
                    case MOD:
                        sb.writef("  div rjunk r% r% %\n", target, lhs, rhs);
                        break;
                    case BANG_EQUAL:
                        sb.writef("  scmp r% %\n", lhs, rhs);
                        sb.writef("  move r% 0\n", target);
                        sb.writef("  cmov ne r% 1\n", target);
                        break;
                    case EQUAL_EQUAL:
                        sb.writef("  scmp r% %\n", lhs, rhs);
                        sb.writef("  move r% 0\n", target);
                        sb.writef("  cmov eq r% 1\n", target);
                        break;
                    case GREATER:
                        sb.writef("  scmp r% %\n", lhs, rhs);
                        sb.writef("  move r% 0\n", target);
                        sb.writef("  cmov gt r% 1\n", target);
                        break;
                    case GREATER_EQUAL:
                        sb.writef("  scmp r% %\n", lhs, rhs);
                        sb.writef("  move r% 0\n", target);
                        sb.writef("  cmov ge r% 1\n", target);
                        break;
                    case LESS:
                        sb.writef("  scmp r% %\n", lhs, rhs);
                        sb.writef("  move r% 0\n", target);
                        sb.writef("  cmov lt r% 1\n", target);
                        break;
                    case LESS_EQUAL:
                        sb.writef("  scmp r% %\n", lhs, rhs);
                        sb.writef("  move r% 0\n", target);
                        sb.writef("  cmov le r% 1\n", target);
                        break;
                    default:
                        error(expr.operator, "Unhandled binary op");
                        return -1;
                }
            }
            if(lit.value.type == TokenType.STRING){
                auto rhs = lit.value.string_;
                switch(expr.operator.type)with(TokenType){
                    case MINUS:
                        sb.writef("  sub r% r% %\n", target, lhs, rhs);
                        break;
                    case PLUS:
                        sb.writef("  add r% r% %\n", target, lhs, rhs);
                        break;
                    case STAR:
                        sb.writef("  mul r% r% %\n", target, lhs, rhs);
                        break;
                    case SLASH:
                        sb.writef("  div r% rjunk r% %\n", target, lhs, rhs);
                        break;
                    case MOD:
                        sb.writef("  div rjunk r% r% %\n", target, lhs, rhs);
                        break;
                    default:
                        error(expr.operator, "Unhandled binary op");
                        return -1;
                }
            }
        }
        else {
            if(target == TARGET_IS_NOTHING){
                return expr.right.accept(this, target);
            }
            int rhs = regallocator.allocate();
            int res = expr.right.accept(this, rhs);
            if(res != 0) return res;
            switch(expr.operator.type)with(TokenType){
                case MINUS:
                    sb.writef("  sub r% r% r%\n", target, lhs, rhs);
                    break;
                case PLUS:
                    sb.writef("  add r% r% r%\n", target, lhs, rhs);
                    break;
                case STAR:
                    sb.writef("  mul r% r% r%\n", target, lhs, rhs);
                    break;
                case SLASH:
                    sb.writef("  div r% rjunk r% r%\n", target, lhs, rhs);
                    break;
                case MOD:
                    sb.writef("  div rjunk r% r% r%\n", target, lhs, rhs);
                    break;
                case BANG_EQUAL:
                    sb.writef("  scmp r% r%\n", lhs, rhs);
                    sb.writef("  move r% 0\n", target);
                    sb.writef("  cmov ne r% 1\n", target);
                    break;
                case EQUAL_EQUAL:
                    sb.writef("  scmp r% r%\n", lhs, rhs);
                    sb.writef("  move r% 0\n", target);
                    sb.writef("  cmov eq r% 1\n", target);
                    break;
                case GREATER:
                    sb.writef("  scmp r% r%\n", lhs, rhs);
                    sb.writef("  move r% 0\n", target);
                    sb.writef("  cmov gt r% 1\n", target);
                    break;
                case GREATER_EQUAL:
                    sb.writef("  scmp r% r%\n", lhs, rhs);
                    sb.writef("  move r% 0\n", target);
                    sb.writef("  cmov ge r% 1\n", target);
                    break;
                case LESS:
                    sb.writef("  scmp r% r%\n", lhs, rhs);
                    sb.writef("  move r% 0\n", target);
                    sb.writef("  cmov lt r% 1\n", target);
                    break;
                case LESS_EQUAL:
                    sb.writef("  scmp r% r%\n", lhs, rhs);
                    sb.writef("  move r% 0\n", target);
                    sb.writef("  cmov le r% 1\n", target);
                    break;
                default:
                    error(expr.operator, "Unhandled binary op");
                    return -1;
            }
        }
        regallocator.reset_to(lhs);
        return 0;
    }
    int visit(Literal* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;
        if(expr.value.type == TokenType.NUMBER){
            sb.writef("  move r% %\n", target, cast(size_t)expr.value.number);
            return 0;
        }
        if(expr.value.type == TokenType.NIL){
            sb.writef("  move r% 0\n", target);
            return 0;
        }
        if(expr.value.type == TokenType.TRUE){
            sb.writef("  move r% 1\n", target);
            return 0;
        }
        if(expr.value.type == TokenType.FALSE){
            sb.writef("  move r% 0\n", target);
            return 0;
        }
        if(expr.value.type == TokenType.STRING){
            sb.writef("  move r% \"%\"\n", target, expr.value.string_);
            return 0;
        }
        fprintf(stderr, "expr.value.type: %d\n", cast(int)expr.value.type);
        fprintf(stderr, "Label: %d\n", cast(int)TokenType.LABEL);
        fprintf(stderr, "%d\n", __LINE__);
        return -1;
    }
    int visit(Unary* expr, int target){
        int v = expr.right.accept(this, target);
        if(v < 0) return v;
        if(target != TARGET_IS_NOTHING){
            switch(expr.operator.type)with(TokenType){
                case MINUS:
                    sb.writef("  neg r% r%\n", target, target);
                    break;
                case BANG:
                    sb.writef("  not r% r%\n", target, target);
                    break;
                default:
                    error(expr.operator, "Unhandled unary operator");
                    return -1;
            }
        }
        return 0;
    }
    int visit(Call* expr, int target){
        enum rarg1 = 10;
        for(int i = 0; i < expr.args.length; i++){
            auto arg = expr.args[i];
            int before = regallocator.alloced;
            int res = arg.accept(this, rarg1+i);
            if(res != 0) return res;
            if(i != expr.args.length-1){
                //can elide the push/pop for last arg
                sb.writef("  push r%\n", rarg1+i);
            }
            regallocator.reset_to(before);
        }
        for(int i = 1; i < expr.args.length; i++){
            sb.writef("  pop r%\n", rarg1+expr.args.length-1-i);
        }
        bool called = false;
        // this is wrong but whatever
        if(expr.callee.type == ExprType.VARIABLE){
            auto l = cast(Literal*)expr.callee;
            if(l.value.type == TokenType.IDENTIFIER){
                // HACK
                bool hack = (target == regallocator.alloced-1);
                if(hack) regallocator.alloced--;
                save_reglocals();
                sb.writef("  call function %\n", l.value.lexeme);
                restore_reglocals();
                if(hack) regallocator.alloced++;
                called = true;
            }
        }
        if(!called){
            int before = regallocator.alloced;
            auto func = regallocator.allocate();
            int res = expr.callee.accept(this, func);
            if(res != 0) return res;
            regallocator.reset_to(before);
            save_reglocals();
            sb.writef("  call r%\n", func);
            restore_reglocals();
        }
        if(target != TARGET_IS_NOTHING)
            sb.writef(  "  move r% rout1\n", target);
        return 0;
    }
    int do_tail_call(Call* expr){
        enum rarg1 = 10;
        for(int i = 0; i < expr.args.length; i++){
            auto arg = expr.args[i];
            int before = regallocator.alloced;
            int res = arg.accept(this, rarg1+i);
            if(res != 0) return res;
            regallocator.reset_to(before);
        }
        bool called = false;
        // this is wrong but whatever
        if(expr.callee.type == ExprType.VARIABLE){
            auto l = cast(Literal*)expr.callee;
            if(l.value.type == TokenType.IDENTIFIER){
                if(analysis.vars_on_stack)
                    restore_stack();
                sb.writef("  tail_call function %\n", l.value.lexeme);
                called = true;
            }
        }
        if(!called){
            int before = regallocator.alloced;
            auto func = regallocator.allocate();
            int res = expr.callee.accept(this, func);
            if(res != 0) return res;
            regallocator.reset_to(before);
            if(analysis.vars_on_stack)
                restore_stack();
            sb.writef("  tail_call r%\n", func);
        }
        return 0;
    }
    int visit(Grouping* expr, int target){
        return 0;
    }
    int visit(VarExpr* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;
        if(auto rlocal = expr.name.lexeme in reglocals){
            if(target == *rlocal){
                // loading into the same register
                return 0;
            }
            sb.writef("  move r% r%\n", target, *rlocal);
            return 0;
        }
        if(auto local = expr.name.lexeme in locals){
            sb.writef("  local_read r% %\n", target, P(*local));
            return 0;
        }
        // must be a global.
        sb.writef("  read r% var %\n", target, expr.name.lexeme);
        return 0;
    }
    int visit(Assign* expr, int target){
        // ignore target;
        if(auto rlocal = expr.name.lexeme in reglocals){
            if(expr.right.type == ExprType.LITERAL){
                auto lit = cast(Literal*)expr.right;
                switch(lit.value.type)with(TokenType){
                    case NIL:
                    case FALSE:
                        sb.writef("  move r% 0\n", *rlocal);
                        break;
                    case TRUE:
                        sb.writef("  move r% 1\n", *rlocal);
                        break;
                    case STRING:
                        sb.writef("  move r% \"%\"\n", *rlocal, lit.value.string_);
                        break;
                    case NUMBER:
                        sb.writef("  move r% %\n", *rlocal, cast(size_t)lit.value.number);
                        break;
                    default:
                        error(lit.value, "Unhandled literal type in assign");
                        return -1;
                }
            }
            else {

                int before = regallocator.alloced;
                int temp = regallocator.allocate();
                int res = expr.right.accept(this, temp);
                if(res != 0) return res;
                regallocator.reset_to(before);
                sb.writef("  move r% r%\n", *rlocal, temp);
            }
            return 0;
        }
        if(auto local  = expr.name.lexeme in locals){
            if(expr.right.type == ExprType.LITERAL){
                auto lit = cast(Literal*)expr.right;
                switch(lit.value.type)with(TokenType){
                    case NIL:
                    case FALSE:
                        sb.writef("  local_write % 0\n", P(*local));
                        break;
                    case TRUE:
                        sb.writef("  local_write % 1\n", P(*local));
                        break;
                    case STRING:
                        sb.writef("  local_write % \"%\"\n", P(*local), lit.value.string_);
                        break;
                    case NUMBER:
                        sb.writef("  local_write % %\n", P(*local), cast(size_t)lit.value.number);
                        break;
                    default:
                        error(lit.value, "Unhandled literal type in assign");
                        return -1;
                }
            }
            else {
                int before = regallocator.alloced;
                int temp = regallocator.allocate();
                int res = expr.right.accept(this, temp);
                if(res != 0) return res;
                regallocator.reset_to(before);
                sb.writef("  local_write % r%\n", P(*local), temp);
            }
            return 0;
        }
        // must be a global.
        if(expr.right.type == ExprType.LITERAL){
            auto lit = cast(Literal*)expr.right;
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            sb.writef("  move r% var %\n", temp, expr.name.lexeme);
            regallocator.reset_to(before);
            switch(lit.value.type)with(TokenType){
                case NIL:
                case FALSE:
                    sb.writef("  write r% 0\n", temp);
                    break;
                case TRUE:
                    sb.writef("  write r% 1\n", temp);
                    break;
                case STRING:
                    sb.writef("  write r% \"%\"\n", temp, lit.value.string_);
                    break;
                case NUMBER:
                    sb.writef("  write r% %\n", temp, cast(size_t)lit.value.number);
                    break;
                default:
                    error(lit.value, "Unhandled literal type in assign");
                    return -1;
            }
        }
        else {
            int before = regallocator.alloced;
            int lhs = regallocator.allocate();
            int rhs = regallocator.allocate();
            int res = expr.right.accept(this, rhs);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("  move r% var %\n", lhs, expr.name.lexeme);
            sb.writef("  write r% r%\n", lhs, rhs);
        }
        return 0;
    }
    int visit(Logical* expr, int target){
        if(target == TARGET_IS_NOTHING){
        }
        error(expr.operator, "Unhandled logical operator");
        return -1;
    }
    int visit(ExpressionStmt* stmt){
        if(!funcdepth){
            error(0, "Expression outside of function");
            return -1;
        }
        int before = regallocator.alloced;
        int result = stmt.expr.accept(this, TARGET_IS_NOTHING);
        regallocator.reset_to(before);
        return result;
    }
    int visit(LetStmt* stmt){
        if(!funcdepth) {
            // global variable;
            if(stmt.initializer.type != ExprType.LITERAL){
                error(stmt.name, "Non constant initializer for global variable");
                return -1;
            }
            auto lit = cast(Literal*)stmt.initializer;
            switch(lit.value.type) with(TokenType){
                case FALSE:
                case NIL:
                    sb.writef("var % 0\n", stmt.name.lexeme);
                    return 0;
                case TRUE:
                    sb.writef("var % 1\n", stmt.name.lexeme);
                    return 0;
                case STRING:
                    sb.writef("var % \"%\"\n", stmt.name.lexeme, lit.value.string_);
                    return 0;
                case NUMBER:
                    sb.writef("var % %\n", stmt.name.lexeme, lit.value.number);
                    return 0;
                default:
                    error(lit.value, "Unhandled literal type");
                    return -1;
            }
        }
        // initializer is guaranteed to be the NIl Expr or a real expr.
        // if(!stmt.initializer) return 0;
        // Turn the initializer into an assignment statement.
        Assign assign = Assign.make(stmt.name, stmt.initializer);
        int res = (&assign.exp).accept(this, 0);
        return res;
        static if(0){
        if(auto rlocal = stmt.name.lexeme in reglocals){
            int res = stmt.initializer.accept(this, *rlocal);
            if(res != 0) return res;
            return 0;
        }
        if(auto local = stmt.name.lexeme in locals){
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            int res = stmt.initializer.accept(this, temp);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("  local_write % r%\n", P(*local), temp);
            return 0;
        }
        error(stmt.name, "Unhandled var stmt");
        return -1;
        }
    }
    int visit(Block* stmt){
        if(!funcdepth) {
            error(0, "Block outside of function");
            return -1;
        }
        foreach(s; stmt.statements){
            int res = s.accept(this);
            if(res != 0) return res;
        }
        return 0;
    }
    int visit(IfStmt* stmt){
        if(!funcdepth) {
            error(0, "If outside of function");
            return -1;
        }
        auto label = labelallocator.allocate();
        if(stmt.condition.type == ExprType.BINARY){
            Binary* b = cast(Binary*)stmt.condition;
            switch(b.operator.type)with(TokenType){
                case GREATER:
                case EQUAL_EQUAL:
                case GREATER_EQUAL:
                case LESS_EQUAL:
                case LESS:
                case BANG_EQUAL:
                    int res = stmt.condition.accept(this, TARGET_IS_CMP_FLAGS);
                    if(res != 0) return res;
                    break;
                default:
                    goto Lgeneric;
            }
            string jmpmode;
            // jump modes need to be inverted
            // as we only jump if condition is false
            switch(b.operator.type)with(TokenType){
                case GREATER:
                    jmpmode = "le";
                    break;
                case EQUAL_EQUAL:
                    jmpmode = "ne";
                    break;
                case GREATER_EQUAL:
                    jmpmode = "lt";
                    break;
                case LESS_EQUAL:
                    jmpmode = "gt";
                    break;
                case LESS:
                    jmpmode = "ge";
                    break;
                case BANG_EQUAL:
                    jmpmode = "eq";
                    break;
                default:
                    assert(0);
            }
            sb.writef("  jump % label L%\n", jmpmode, label);
        }
        else {
            Lgeneric:
            int before = regallocator.alloced;
            int cond = regallocator.allocate();
            scope(exit) regallocator.reset_to(before);
            int res = stmt.condition.accept(this, cond);
            if(res != 0) return res;
            sb.writef("  cmp r% 0\n", cond);
            sb.writef("  jump eq label L%\n", label);
        }
        int res = stmt.thenBranch.accept(this);
        if(res != 0) return res;
        sb.writef("  label L%\n", label);
        if(stmt.elseBranch){
            int r = stmt.elseBranch.accept(this);
            if(r != 0) return r;

        }
        return 0;
    }
    int visit(FuncStatement* stmt){
        if(funcdepth){
            error(stmt.name, "Nested function");
            return -1;
        }
        scope analyzer = new DasmAnalyzer!(typeof(*allocator))(allocator);
        analyzer.visit(stmt);
        analysis = analyzer.analysis;
        funcdepth++;
        scope(exit) {
            funcdepth--;
            locals.cleanup();
            reglocals.cleanup();
            regallocator.reset();
            stackallocator.reset();
            labelallocator.reset();
            analysis.vars.cleanup();
        }
        sb.writef("function % %\n", stmt.name.lexeme, stmt.params.length);
        int rarg = 10;
        foreach(p; stmt.params){
            auto r = regallocator.allocate();
            reglocals[p.lexeme] = r;
            sb.writef("  move r% r%\n", r, rarg++);
        }
        if(analysis.vars[].length >= 4)
            analysis.vars_on_stack = true;
        if(analysis.vars_on_stack)
            save_stack();
        if(!analysis.vars_on_stack){
            foreach(v; analysis.vars){
                auto r = regallocator.allocate();
                reglocals[v] = r;
            }
        }
        else {
            // our stack grows upwards
            uint nvars = 0;
            foreach(v; analysis.vars){
                auto s = stackallocator.allocate();
                locals[v] = cast(int)s;
                nvars++;
            }
            sb.writef("  add rsp rsp %\n", P(nvars));
        }
        foreach(s; stmt.body){
            int res = s.accept(this);
            if(res != 0) return res;
        }
        if(!stmt.body.length || stmt.body[$-1].type != StatementType.RETURN){
            if(analysis.vars_on_stack)
                restore_stack();
            sb.write("  ret\n");
        }
        sb.write("end\n");
        return 0;
    }
    int visit(ReturnStatement* stmt){
        if(!funcdepth) {
            error(stmt.keyword, "Return outside of function");
            return -1;
        }
        if(stmt.value.type == ExprType.CALL){
            return do_tail_call(cast(Call*)stmt.value);
        }
        if(stmt.value !is &NilExpr_.exp){
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            int rout = 15;
            int res = stmt.value.accept(this, temp);
            regallocator.reset_to(before);
            sb.writef("  move r% r%\n", rout, temp);
            if(res != 0) return res;
        }
        if(analysis.vars_on_stack)
            restore_stack();
        sb.write("  ret\n");
        return 0;
    }
    int visit(WhileStatement* stmt){
        if(!funcdepth) {
            error(0, "While outside of function");
            return -1;
        }
        int top = labelallocator.allocate();
        // after the loop
        int after = labelallocator.allocate();
        // TODO: abstract over this instead of this
        // copy paste silliness?
        if(stmt.condition.type == ExprType.LITERAL){
            auto lit = cast(Literal*)stmt.condition;
            switch(lit.value.type)with(TokenType){
                case TRUE:
                    sb.writef("  label L%\n", top);
                    break;
                case FALSE:
                    labelallocator.nalloced = top-1;
                    return 0;
                case NIL:
                    labelallocator.nalloced = top-1;
                    return 0;
                case NUMBER:
                    if(lit.value.number){
                        sb.writef("  label L%\n", top);
                        break;
                    }
                    labelallocator.nalloced = top-1;
                    return 0;
                default:
                    error(lit.value, "Unhandled literal type for while condition");
                    return -1;
            }
        }
        else if(stmt.condition.type == ExprType.BINARY){
            sb.writef("  label L%\n", top);
            Binary* b = cast(Binary*)stmt.condition;
            switch(b.operator.type)with(TokenType){
                case GREATER:
                case EQUAL_EQUAL:
                case GREATER_EQUAL:
                case LESS_EQUAL:
                case LESS:
                case BANG_EQUAL:
                    int res = stmt.condition.accept(this, TARGET_IS_CMP_FLAGS);
                    if(res != 0) return res;
                    break;
                default:
                    goto Lgeneric;
            }
            string jmpmode;
            // jump modes need to be inverted
            // as we only jump if condition is false
            switch(b.operator.type)with(TokenType){
                case GREATER:
                    jmpmode = "le";
                    break;
                case EQUAL_EQUAL:
                    jmpmode = "ne";
                    break;
                case GREATER_EQUAL:
                    jmpmode = "lt";
                    break;
                case LESS_EQUAL:
                    jmpmode = "gt";
                    break;
                case LESS:
                    jmpmode = "ge";
                    break;
                case BANG_EQUAL:
                    jmpmode = "eq";
                    break;
                default:
                    assert(0);
            }
            sb.writef("  jump % label L%\n", jmpmode, after);
        }
        else {
            sb.writef("  label L%\n", top);
            Lgeneric:
            int before = regallocator.alloced;
            int cond = regallocator.allocate();
            scope(exit) regallocator.reset_to(before);
            int res = stmt.condition.accept(this, cond);
            if(res != 0) return res;
            sb.writef("  cmp r% 0\n", cond);
            sb.writef("  jump eq label L%\n", after);
        }
        int res = stmt.statement.accept(this);
        if(res != 0) return res;
        sb.writef("  move rip label L%\n", top);
        sb.writef("  label L%\n", after);
        return 0;
    }
    int do_it(Statement*[] stmts){
        foreach(s; stmts){
            int res = s.accept(this);
            if(res != 0) return res;
        }
        return 0;
    }
    int visit(GotoStatement* stmt){
        sb.writef("  move rip label L%\n", stmt.label.lexeme);
        return 0;
    }
    int visit(LabelStatement* stmt){
        sb.writef("  label L%\n", stmt.label.lexeme);
        return 0;
    }
}

