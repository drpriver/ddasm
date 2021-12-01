/// A subclass of Object that implements methods needed for
/// classes to work in betterC.

class BCObject {
    override
    int opCmp(Object o){
        if(!o) return 1;
        if(o is this) return 0;
        return cast(void*)this < cast(void*)o?-1: 1;
    }
    @trusted
    override
    size_t toHash(){ return cast(size_t)cast(void*)this;}

    override
    bool opEquals(Object o){
        return this is o;
    }
    override
    string toString(){
        return "BCObject";
    }
}
version(D_BetterC){
extern(C) void _d_callfinalizer(void*p){
    // do nothing, call your own destructors
}
}
