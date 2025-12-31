// Test file for comma-separated bitfield declarations

struct TestBitfields {
    int x:1, y:2, z:3;
    unsigned int a:4, b:5;
    int regular_field;
    int c:1, d:2, e:3, f:4;
};

struct MixedDeclarations {
    int :8, x:1;           // anonymous bitfield followed by named
    int y:2, :4, z:3;      // named, anonymous, named
    int w, v:2;            // regular field and bitfield
};

union TestUnionBitfields {
    int x:1, y:2;
    unsigned int a:4, b:5;
};
