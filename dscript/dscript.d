module dscript.dscript;
import core.stdc.stdio: fprintf, stderr; // FIXME: should be a callback to report errors
import dlib.aliases;
import dlib.allocator: Mallocator;
import dlib.barray: Barray, make_barray;
import dlib.parse_numbers: parse_unsigned_human;
import dlib.btable: BTable;

enum TokenType: ubyte{
    // Single character tokens
    LEFT_PAREN = '(', RIGHT_PAREN =')', LEFT_BRACE='{', RIGHT_BRACE = '}',
    COMMA = ',', DOT = '.', MINUS ='-', PLUS = '+', SEMICOLON = ';', SLASH = '/', STAR = '*', MOD = '%',

    // One or two character tokens
    BANG = '!', BANG_EQUAL = BANG + 127, EQUAL = '=', EQUAL_EQUAL = EQUAL+127,
    GREATER = '>', GREATER_EQUAL = GREATER + 127, LESS = '<', LESS_EQUAL = LESS + 127,

    // Literals
    IDENTIFIER = 127, STRING = 128, NUMBER = 129, HEX=130, PHEX=131, SNUM=132, BNUM=133,

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
        while(peek.isIdentChar) current++;
        auto text = cast(str)source[start .. current];
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
    void do_number(ubyte c){
        if(c == '0'){
            // TODO: these don't check how long the resulting literal
            // is, which means the assembler ends up doing it.
            if(peek == 'x' || peek == 'X'){
                current++;
                while(peek.isHexDigit) current++;
                auto slice = cast(str)source[start .. current];
                addToken(TokenType.HEX, slice);
                return;
            }
            if(peek == 'p' || peek == 'P'){
                current++;
                while(peek.isHexDigit) current++;
                auto slice = cast(str)source[start .. current];
                addToken(TokenType.PHEX, slice);
                return;
            }
            if(peek == 'b' || peek == 'B'){
                current++;
                while(peek == '0' || peek == '1') current++;
                auto slice = cast(str)source[start .. current];
                addToken(TokenType.BNUM, slice);
                return;
            }
            if(peek == 's' || peek == 'S'){
                current++;
                while(peek.isAlphaNumeric) current++;
                auto slice = cast(str)source[start .. current];
                addToken(TokenType.SNUM, slice);
                return;
            }
        }
        while(peek.isDigit) current++;
        if(peek == '.'){
            error(current, "Non-integer numbers are not allowed");
            return;
        }
        auto slice = cast(str)source[start .. current];
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
bool isHexDigit()(ubyte c){
    c |= 0x20;
    bool number =  c >= '0' && c <= '9' ;
    bool af = c >= 'a' && c <= 'f';
    return number | af;
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
    Statement* make(A)(A allocator, Token k, Expr* v){
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
    make(A)(A allocator, Token n, Token[] params, Statement*[] s){
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
    make(A)(A allocator, Expr* c, Statement* s){
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
    make(A)(A allocator, Expr* c, Statement* t, Statement* e){
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
    make(A)(A allocator, Statement*[] s){
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
    make(A)(A allocator, Expr* e){
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
    make(A)(A allocator, Token t, Expr* i){
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
    make(A)(A allocator, Token t){
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
    make(A)(A allocator, Token t){
        auto data = allocator.alloc(typeof(this).sizeof);
        auto p = cast(typeof(this)*)data.ptr;
        p.stmt.type = StatementType.LABEL;
        p.label = t;
        return &p.stmt;
    }
}

// Parser

struct Parser(A) {
    A allocator;
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

    int
    parse(R)(R* result){
        while(!isAtEnd){
            auto s = declaration();
            if(ERROR_OCCURRED) return 1;
            *result ~= s;
        }
        return 0;
    }

    Statement*
    declaration(){
        if(match(TokenType.FUN)) return function_!"function"();
        if(match(TokenType.LET)) return varDeclaration();
        return statement();
    }

    Statement*
    function_(string kind)(){with(TokenType){
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
        consume(TokenType.SEMICOLON, "Expect ';' after var decl");
        return LetStmt.make(allocator, name, initializer);

    }
    Statement*
    statement(){
        if(match(TokenType.RETURN)) return returnStatement();
        if(match(TokenType.FOR)) return forStatement();
        if(match(TokenType.IF)) return ifStatement();
        if(match(TokenType.WHILE)) return whileStatement();
        if(match(TokenType.LEFT_BRACE)) return block();
        if(match(TokenType.LABEL)) return label();
        if(match(TokenType.GOTO)) return goto_();
        return expressionStatement();
    }

    Statement*
    label(){
        Token target = consume(TokenType.IDENTIFIER, "Expect label name");
        if(ERROR_OCCURRED) return null;
        consume(TokenType.SEMICOLON, "Expect ';' after label decl");
        return LabelStatement.make(allocator, target);
    }
    Statement*
    goto_(){
        Token target = consume(TokenType.IDENTIFIER, "Expect goto target");
        if(ERROR_OCCURRED) return null;
        consume(TokenType.SEMICOLON, "Expect ';' after goto target");
        return GotoStatement.make(allocator, target);
    }

    Statement*
    returnStatement(){
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

    Statement*
    whileStatement(){
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
    parse_statement_list(Barray!(Statement*, A)* stmts){
        while(!check(TokenType.RIGHT_BRACE) && !isAtEnd){
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
        consume(TokenType.SEMICOLON, "Expected ';' after value");
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
    factor(){with(TokenType){
        Expr* expr = unary();
        while(match(STAR, SLASH, MOD)){
            Token operator = previous;
            Expr* right = unary();
            expr = Binary.make(allocator, expr, operator, right);
        }
        return expr;
    }}

    Expr*
    unary(){with(TokenType){
        if(match(BANG, MINUS)){
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
                expr = finishCall(expr);
                if(expr is null) return null;
            }
            else {
                break;
            }
        }
        return expr;
    }

    Expr*
    finishCall(Expr* callee){
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
        if(isAtEnd) return false;
        return peek.type == type;
    }

    Token
    advance(){
        if(!isAtEnd) current++;
        return previous;
    }

    bool
    isAtEnd(){
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

