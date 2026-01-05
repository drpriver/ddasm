/*
 * Constant Expression Evaluator for C Front-End
 * Copyright 2025, David Priver
 *
 * Evaluates constant expressions at compile time for optimization.
 */
module cfront.c_const_eval;

import dlib.aliases : str;
import dlib.parse_numbers : parse_unsigned_human, parse_float;
import dlib.table : Table;
import cfront.c_ast;
import cfront.c_pp_to_c : CTokenType;

// Alias for enum constants table type
alias EnumTable = Table!(str, long);

struct ConstValue {
    enum Kind {
        NOT_CONST,  // Expression cannot be evaluated at compile time
        INTEGER,    // Signed/unsigned integer value
        FLOAT,      // Floating point value (double precision)
    }
    Kind kind = Kind.NOT_CONST;
    union {
        long int_val;
        ulong uint_val;
        double float_val;
    }
    bool is_unsigned;

    static ConstValue not_const() {
        return ConstValue(Kind.NOT_CONST);
    }

    static ConstValue from_int(long v) {
        ConstValue cv;
        cv.kind = Kind.INTEGER;
        cv.int_val = v;
        cv.is_unsigned = false;
        return cv;
    }

    static ConstValue from_uint(ulong v) {
        ConstValue cv;
        cv.kind = Kind.INTEGER;
        cv.uint_val = v;
        cv.is_unsigned = true;
        return cv;
    }

    static ConstValue from_double(double v) {
        ConstValue cv;
        cv.kind = Kind.FLOAT;
        cv.float_val = v;
        cv.is_unsigned = false;
        return cv;
    }

    bool is_const() const { return kind != Kind.NOT_CONST; }
    bool is_integer() const { return kind == Kind.INTEGER; }
    bool is_float() const { return kind == Kind.FLOAT; }
    bool is_zero() const {
        if (kind == Kind.INTEGER) return int_val == 0;
        if (kind == Kind.FLOAT) return float_val == 0.0;
        return false;
    }

    // Get value as signed long
    long as_long() const {
        if (kind == Kind.FLOAT) return cast(long)float_val;
        return is_unsigned ? cast(long)uint_val : int_val;
    }

    // Get value as unsigned long
    ulong as_ulong() const {
        if (kind == Kind.FLOAT) return cast(ulong)float_val;
        return is_unsigned ? uint_val : cast(ulong)int_val;
    }

    // Get value as double
    double as_double() const {
        if (kind == Kind.FLOAT) return float_val;
        if (is_unsigned) return cast(double)uint_val;
        return cast(double)int_val;
    }
}

// Try to evaluate an expression at compile time
// Returns NOT_CONST if the expression cannot be evaluated
ConstValue try_eval_constant(CExpr* expr) {
    if (expr is null) return ConstValue.not_const();

    // Unwrap grouping parentheses
    expr = expr.ungroup();

    final switch(expr.kind) with(CExprKind){
        case LITERAL:
            return eval_literal(expr.as_literal);

        case IDENTIFIER:
            return eval_identifier(expr.as_identifier);

        case BINARY:
            return eval_binary(expr.as_binary);

        case UNARY:
            return eval_unary(expr.as_unary);

        case CAST:
            return eval_cast(expr.as_cast);

        case SIZEOF:
            return eval_sizeof(expr.as_sizeof);

        case ALIGNOF:
            return eval_alignof(expr.as_alignof);

        case COUNTOF:
            return eval_countof(expr.as_countof);

        case TERNARY:
            return eval_ternary(expr.as_ternary);

        case GROUPING:
            // Already handled by ungroup()
            assert(0);

        // These cannot be constant evaluated
        case CALL, SUBSCRIPT, MEMBER_ACCESS, ASSIGN, VA_ARG,
             INIT_LIST, COMPOUND_LITERAL, GENERIC, EMBED, STMT_EXPR:
            // FIXME: some of these are constant evaluable.
            return ConstValue.not_const();
    }
}

// Evaluate identifier - only enum constants are compile-time constant
ConstValue eval_identifier(CIdentifier* ident) {
    if (ident is null) return ConstValue.not_const();

    // Use resolved ref_kind from parsing - enum constants have their value stored
    if (ident.ref_kind == IdentifierRefKind.ENUM_CONST) {
        return ConstValue.from_int(ident.enum_value);
    }

    return ConstValue.not_const();
}

ConstValue eval_literal(CLiteral* lit) {
    if (lit is null) return ConstValue.not_const();

    str lexeme = lit.value.lexeme;

    if (lit.value.type == CTokenType.CHAR_LITERAL) {
        return ConstValue.from_int(parse_char_literal(lexeme));
    }

    if (lit.value.type == CTokenType.NUMBER || lit.value.type == CTokenType.HEX) {
        bool is_unsigned = is_unsigned_literal(lexeme);
        auto parsed = parse_unsigned_human(lexeme);
        if (parsed.errored) return ConstValue.not_const();

        if (is_unsigned) {
            return ConstValue.from_uint(parsed.value);
        } else {
            return ConstValue.from_int(cast(long)parsed.value);
        }
    }

    if (lit.value.type == CTokenType.FLOAT_LITERAL) {
        auto parsed = parse_float(lexeme);
        if (parsed.errored) return ConstValue.not_const();
        return ConstValue.from_double(parsed.value);
    }

    // String literals are not integer constants
    return ConstValue.not_const();
}

ConstValue eval_binary(CBinary* expr) {
    auto left = try_eval_constant(expr.left);
    if (!left.is_const()) return ConstValue.not_const();

    // Short-circuit evaluation for && and ||
    if (expr.op == CTokenType.AMP_AMP) {
        if (left.is_zero()) return ConstValue.from_int(0);
        auto right = try_eval_constant(expr.right);
        if (!right.is_const()) return ConstValue.not_const();
        return ConstValue.from_int(right.is_zero() ? 0 : 1);
    }

    if (expr.op == CTokenType.PIPE_PIPE) {
        if (!left.is_zero()) return ConstValue.from_int(1);
        auto right = try_eval_constant(expr.right);
        if (!right.is_const()) return ConstValue.not_const();
        return ConstValue.from_int(right.is_zero() ? 0 : 1);
    }

    auto right = try_eval_constant(expr.right);
    if (!right.is_const()) return ConstValue.not_const();

    // If either operand is a float, use float arithmetic
    if (left.is_float() || right.is_float()) {
        return eval_binary_float(expr.op, left, right);
    }

    // Integer arithmetic
    bool is_unsigned = left.is_unsigned || right.is_unsigned;
    ulong lhs = left.as_ulong();
    ulong rhs = right.as_ulong();
    long lhs_s = left.as_long();
    long rhs_s = right.as_long();

    ulong result;
    switch (expr.op) with (CTokenType) {
        // Bitwise operations (same for signed/unsigned at bit level)
        case PLUS:      result = lhs + rhs; break;
        case MINUS:     result = lhs - rhs; break;
        case STAR:      result = lhs * rhs; break;
        case AMP:       result = lhs & rhs; break;
        case PIPE:      result = lhs | rhs; break;
        case CARET:     result = lhs ^ rhs; break;
        case LESS_LESS: result = lhs << rhs; break;
        case EQUAL_EQUAL: return ConstValue.from_int(lhs == rhs ? 1 : 0);
        case BANG_EQUAL:  return ConstValue.from_int(lhs != rhs ? 1 : 0);

        // Division/modulo differ for signed vs unsigned
        case SLASH:
            if (rhs == 0) return ConstValue.not_const();
            if (is_unsigned)
                result = lhs / rhs;
            else
                result = cast(ulong)(lhs_s / rhs_s);
            break;
        case PERCENT:
            if (rhs == 0) return ConstValue.not_const();
            if (is_unsigned)
                result = lhs % rhs;
            else
                result = cast(ulong)(lhs_s % rhs_s);
            break;

        // Shift right differs for signed (arithmetic) vs unsigned (logical)
        case GREATER_GREATER:
            if (is_unsigned)
                result = lhs >> rhs;
            else
                result = cast(ulong)(lhs_s >> rhs);
            break;

        // Comparison operators return signed 0 or 1
        case LESS:
            if (is_unsigned)
                return ConstValue.from_int(lhs < rhs ? 1 : 0);
            else
                return ConstValue.from_int(lhs_s < rhs_s ? 1 : 0);
        case LESS_EQUAL:
            if (is_unsigned)
                return ConstValue.from_int(lhs <= rhs ? 1 : 0);
            else
                return ConstValue.from_int(lhs_s <= rhs_s ? 1 : 0);
        case GREATER:
            if (is_unsigned)
                return ConstValue.from_int(lhs > rhs ? 1 : 0);
            else
                return ConstValue.from_int(lhs_s > rhs_s ? 1 : 0);
        case GREATER_EQUAL:
            if (is_unsigned)
                return ConstValue.from_int(lhs >= rhs ? 1 : 0);
            else
                return ConstValue.from_int(lhs_s >= rhs_s ? 1 : 0);

        // Comma operator: return right value
        case COMMA:
            return right;

        default:
            return ConstValue.not_const();
    }

    if (is_unsigned)
        return ConstValue.from_uint(result);
    else
        return ConstValue.from_int(cast(long)result);
}

// Float binary operations
ConstValue eval_binary_float(CTokenType op, ConstValue left, ConstValue right) {
    double lhs = left.as_double();
    double rhs = right.as_double();

    switch (op) with (CTokenType) {
        case PLUS:  return ConstValue.from_double(lhs + rhs);
        case MINUS: return ConstValue.from_double(lhs - rhs);
        case STAR:  return ConstValue.from_double(lhs * rhs);
        case SLASH:
            if (rhs == 0.0) return ConstValue.not_const();
            return ConstValue.from_double(lhs / rhs);

        // Comparison operators return integer 0 or 1
        case EQUAL_EQUAL:   return ConstValue.from_int(lhs == rhs ? 1 : 0);
        case BANG_EQUAL:    return ConstValue.from_int(lhs != rhs ? 1 : 0);
        case LESS:          return ConstValue.from_int(lhs < rhs ? 1 : 0);
        case LESS_EQUAL:    return ConstValue.from_int(lhs <= rhs ? 1 : 0);
        case GREATER:       return ConstValue.from_int(lhs > rhs ? 1 : 0);
        case GREATER_EQUAL: return ConstValue.from_int(lhs >= rhs ? 1 : 0);

        // Bitwise and modulo operations are not valid for floats
        case AMP, PIPE, CARET, LESS_LESS, GREATER_GREATER, PERCENT:
            return ConstValue.not_const();

        default:
            return ConstValue.not_const();
    }
}

ConstValue eval_unary(CUnary* expr) {
    auto operand = try_eval_constant(expr.operand);
    if (!operand.is_const()) return ConstValue.not_const();

    switch (expr.op) with (CTokenType) {
        case MINUS:
            if (operand.is_float())
                return ConstValue.from_double(-operand.float_val);
            else if (operand.is_unsigned)
                return ConstValue.from_uint(-operand.uint_val);
            else
                return ConstValue.from_int(-operand.int_val);

        case PLUS:
            return operand;

        case TILDE:
            // Bitwise NOT is not valid for floats
            if (operand.is_float())
                return ConstValue.not_const();
            if (operand.is_unsigned)
                return ConstValue.from_uint(~operand.uint_val);
            else
                return ConstValue.from_int(~operand.int_val);

        case BANG:
            return ConstValue.from_int(operand.is_zero() ? 1 : 0);

        // Increment/decrement are not constant
        case PLUS_PLUS, MINUS_MINUS:
            return ConstValue.not_const();

        // Address-of and dereference are not constant
        case AMP, STAR:
            return ConstValue.not_const();

        default:
            return ConstValue.not_const();
    }
}

ConstValue eval_cast(CCast* expr) {
    auto operand = try_eval_constant(expr.operand);
    if (!operand.is_const()) return ConstValue.not_const();

    CType* t = expr.cast_type;
    if (t is null) return ConstValue.not_const();

    // Handle float casts
    if (t.is_float()) {
        return ConstValue.from_double(operand.as_double());
    }

    // Only integer/enum casts can be constant evaluated beyond this point
    if (!t.is_integer() && !t.is_enum()) return ConstValue.not_const();

    // Apply truncation/sign extension based on target type
    long val = operand.as_long();

    switch (t.kind) with (CTypeKind) {
        case CHAR:
            if(!t.is_signed)
                return ConstValue.from_uint(cast(ubyte)val);
            else
                return ConstValue.from_int(cast(byte)val);
        case SHORT:
            if(!t.is_signed)
                return ConstValue.from_uint(cast(ushort)val);
            else
                return ConstValue.from_int(cast(short)val);
        case INT:
            if(t.is_signed)
                return ConstValue.from_uint(cast(uint)val);
            else
                return ConstValue.from_int(cast(int)val);
        case LONG, ENUM:
            if(t.is_signed)
                return ConstValue.from_uint(cast(ulong)val);
            else
                return ConstValue.from_int(val);
        default:
            return ConstValue.not_const();
    }
}

ConstValue eval_sizeof(CSizeof* expr) {
    // sizeof is precomputed during parsing when possible
    if (expr.size > 0) {
        return ConstValue.from_uint(expr.size);
    }

    // Try to compute from type
    if (expr.sizeof_type !is null) {
        size_t sz = expr.sizeof_type.size_of();
        if (sz > 0) return ConstValue.from_uint(sz);
    }

    // sizeof(expr) - try to get type of expression
    if (expr.sizeof_expr !is null && expr.sizeof_expr.type !is null) {
        size_t sz = expr.sizeof_expr.type.size_of();
        if (sz > 0) return ConstValue.from_uint(sz);
    }

    return ConstValue.not_const();
}

ConstValue eval_alignof(CAlignof* expr) {
    if (expr.alignment > 0) {
        return ConstValue.from_uint(expr.alignment);
    }

    if (expr.alignof_type !is null) {
        size_t align_ = expr.alignof_type.align_of();
        if (align_ > 0) return ConstValue.from_uint(align_);
    }

    return ConstValue.not_const();
}

ConstValue eval_countof(CCountof* expr) {
    if (expr.count > 0) {
        return ConstValue.from_uint(expr.count);
    }
    return ConstValue.not_const();
}

ConstValue eval_ternary(CTernary* expr) {
    auto cond = try_eval_constant(expr.condition);
    if (!cond.is_const()) return ConstValue.not_const();

    // Only evaluate the branch that will be taken
    if (!cond.is_zero()) {
        return try_eval_constant(expr.if_true);
    } else {
        return try_eval_constant(expr.if_false);
    }
}

// Helper to check if a literal has unsigned suffix
bool is_unsigned_literal(str lex) {
    if (lex.length == 0) return false;
    char last = lex[$ - 1];
    if (last == 'u' || last == 'U') return true;
    if (lex.length >= 2) {
        char prev = lex[$ - 2];
        // Check for ul, uL, Ul, UL, lu, lU, Lu, LU
        if ((last == 'l' || last == 'L') && (prev == 'u' || prev == 'U')) return true;
        if ((last == 'u' || last == 'U') && (prev == 'l' || prev == 'L')) return true;
    }
    return false;
}

// Parse a character literal to its integer value
long parse_char_literal(str s) {
    if (s.length < 2) return 0;

    // Handle L'x' wide character literals
    size_t start = 1;
    if (s[0] == 'L' && s.length >= 3) start = 2;

    if (s[start] == '\\' && s.length > start + 1) {
        // Escape sequence
        char c = s[start + 1];
        switch (c) {
            case 'n': return '\n';
            case 't': return '\t';
            case 'r': return '\r';
            case '0': return '\0';
            case '\\': return '\\';
            case '\'': return '\'';
            case '"': return '"';
            case 'a': return '\a';
            case 'b': return '\b';
            case 'f': return '\f';
            case 'v': return '\v';
            case 'x':
                // Hex escape \xNN
                if (s.length > start + 3) {
                    long val = 0;
                    foreach (i; start + 2 .. s.length - 1) {
                        char ch = s[i];
                        if (ch >= '0' && ch <= '9')
                            val = val * 16 + (ch - '0');
                        else if (ch >= 'a' && ch <= 'f')
                            val = val * 16 + (ch - 'a' + 10);
                        else if (ch >= 'A' && ch <= 'F')
                            val = val * 16 + (ch - 'A' + 10);
                        else
                            break;
                    }
                    return val;
                }
                return 0;
            default:
                // Octal escape \NNN
                if (c >= '0' && c <= '7') {
                    long val = c - '0';
                    foreach (i; start + 2 .. s.length - 1) {
                        char ch = s[i];
                        if (ch >= '0' && ch <= '7')
                            val = val * 8 + (ch - '0');
                        else
                            break;
                    }
                    return val;
                }
                return c;
        }
    } else if (s.length > start) {
        return s[start];
    }
    return 0;
}
