// Blue paint algorithm specific tests
// Tests for the macro expansion "painting" that prevents infinite recursion
// Uses declarations to make painted identifiers valid C

// Basic blue paint - macro sees itself
#define PAINT PAINT
int PAINT;  // Declares variable named PAINT (painted, won't expand again)

// Blue paint through indirect
#define BLUE RED
#define RED BLUE
int BLUE;  // BLUE->RED->BLUE, second BLUE painted, declares 'BLUE'

// Multiple instances in one expansion
#define MULTI MULTI ## MULTI
int MULTI;  // MULTIMULTI as identifier (both painted after paste)

// Paint removed between separate expansions
#define CONTEXT context_val
#define context_val 42
int bp_e1 = CONTEXT;  // 42
int bp_e2 = CONTEXT;  // 42 (paint cleared between)

// Test that non-recursive macros still work through chains
#define L1 L2
#define L2 L3
#define L3 L4
#define L4 100
int bp_chain = L1;  // 100

// Complex: indirect recursion with multiple paths
#define PATH1 PATH2
#define PATH2 PATH3
#define PATH3 PATH1
int PATH1;  // PATH1 (PATH1->PATH2->PATH3->PATH1 stops)

// Same name, different macro type - object expanding to func-like name
#define OBJ_FIRST 42
int bp_k = OBJ_FIRST;  // 42

// Verify paint doesn't incorrectly block non-recursive expansion
#define NOT_SELF OTHER
#define OTHER 99
int bp_l = NOT_SELF;  // 99

// Self-reference creates valid identifier
#define SELF_ID SELF_ID
int SELF_ID;  // declares int SELF_ID

// Indirect self-reference
#define IND_A IND_B
#define IND_B IND_A
int IND_A;  // declares int IND_A

int main(){ return 0; }
