module dvm_modules.sdl;

import dvm.dvm_linked: LinkedModule, Function, FunctionType, FunctionTable, FunctionInfo;
import dlib.allocator: MALLOCATOR;
import dlib.aliases;

Function*
expose_function(F)(F fun){
    Function* f = cast(Function*)MALLOCATOR.alloc(Function.sizeof).ptr;
    f.type = FunctionType.NATIVE;
    static if(is(F : uintptr_t function())){
        f.native_function_r = fun;
        f.n_ret = 1;
        f.n_args = 0;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t))){
        f.native_function_ra = fun;
        f.n_ret = 1;
        f.n_args = 1;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t))){
        f.native_function_raa = fun;
        f.n_ret = 1;
        f.n_args = 2;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaa = fun;
        f.n_ret = 1;
        f.n_args = 3;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaaa = fun;
        f.n_ret = 1;
        f.n_args = 4;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaaaa = fun;
        f.n_ret = 1;
        f.n_args = 5;
        return f;
    }
    else static if(is(F : void function())){
        f.native_function_ = fun;
        f.n_ret = 0;
        f.n_args = 0;
        return f;
    }
    else static if(is(F : void function(uintptr_t))){
        f.native_function_a = fun;
        f.n_ret = 0;
        f.n_args = 1;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t))){
        f.native_function_aa = fun;
        f.n_ret = 0;
        f.n_args = 2;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaa = fun;
        f.n_ret = 0;
        f.n_args = 3;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaaa = fun;
        f.n_ret = 0;
        f.n_args = 4;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaaaa = fun;
        f.n_ret = 0;
        f.n_args = 5;
        return f;
    }
    else {
    pragma(msg, F.stringof);
    pragma(msg, typeof(fun));
    static assert(0);
    }
}
LinkedModule*
get_module(){
    __gshared LinkedModule sdl_module;
    __gshared bool is_init;
    if(is_init) return &sdl_module;
    sdl_module.functions.data.allocator = MALLOCATOR;
    __gshared void* handle;
    version(Windows){
        import core.sys.windows;
        handle = LoadLibraryA("SDL2.dll");
        if(!handle) return null;
    }
    else {
        import core.sys.posix.dlfcn;
        version(OSX)
            handle = dlopen("/Library/Frameworks/SDL2.framework/SDL2", RTLD_NOW);
        else
            handle = dlopen("/usr/lib/x86_64-linux-gnu/libSDL2.so", RTLD_NOW);
        if(!handle) return null;
    }
    void reg(F)(str key, F fun){
        FunctionInfo fi = FunctionInfo(key, expose_function(fun));
        sdl_module.functions[key] = fi;
    }
    extern(C) static struct SDL_Window;
    extern(C) static struct SDL_Renderer;
    extern(C) static struct SDL_Event;
    extern(C) static struct SDL_Rect;
    static struct SDL {
        extern(C) int function(uint) SDL_Init;
        extern(C) SDL_Window* function(const(char)*, int, int, int, int, uint) SDL_CreateWindow;
        extern(C) SDL_Renderer* function(SDL_Window*, int, uint) SDL_CreateRenderer;
        extern(C) void function() SDL_Quit;
        extern(C) int function(SDL_Event*) SDL_PollEvent;
        extern(C) int function(SDL_Event*) SDL_WaitEvent;
        extern(C) int function(SDL_Renderer*, ubyte, ubyte, ubyte, ubyte) SDL_SetRenderDrawColor;
        extern(C) int function(SDL_Renderer*) SDL_RenderClear;
        extern(C) int function(SDL_Renderer*, void*) SDL_RenderFillRect;
        extern(C) void function(SDL_Renderer*) SDL_RenderPresent;
        extern(C) void function(SDL_Renderer*) SDL_DestroyRenderer;
        extern(C) void function(SDL_Window*) SDL_DestroyWindow;
        extern(C) void function(SDL_Window*, int*, int*) SDL_GetWindowSize;
        extern(C) void function(SDL_Window*, int, int) SDL_SetWindowSize;
        extern(C) void function(uint) SDL_Delay;
        extern(C) void function(SDL_Renderer*, int) SDL_SetRenderDrawBlendMode;
        extern(C) int function(SDL_Renderer*, int*, int*) SDL_GetRendererOutputSize;
        extern(C) int function(SDL_Renderer*, float, float) SDL_RenderSetScale;
        extern(C) ulong function() SDL_GetTicks64;
        extern(C) uint function() SDL_GetTicks;
    }
    __gshared SDL sdl;
    foreach(memb; __traits(allMembers, SDL)){
        version(Windows)
            __traits(getMember, sdl, memb) = cast(typeof(__traits(getMember, sdl, memb)))GetProcAdress(handle, memb);
        else
            __traits(getMember, sdl, memb) = cast(typeof(__traits(getMember, sdl, memb)))dlsym(handle, memb);
    }
    reg("Init", (uintptr_t arg){
        return cast(uintptr_t)sdl.SDL_Init(cast(uint)arg);
    });
    reg("CreateWindow",
        (uintptr_t name, uintptr_t width, uintptr_t height, uintptr_t flags){
            int windowpos_undefined = 0x1FFF0000u;
            return cast(uintptr_t)sdl.SDL_CreateWindow(
                cast(const(char)*)name, 
                windowpos_undefined, windowpos_undefined,
                cast(int)width, cast(int)height, 
                cast(uint)flags,
            );
        }
    );
    reg("CreateRenderer", (uintptr_t window, uintptr_t idx, uintptr_t flags){
        SDL_Renderer* r = sdl.SDL_CreateRenderer(cast(SDL_Window*)window, cast(int)idx, cast(uint)flags);
        if(r){
            enum {SDL_BLENDMODE_BLEND = 0x00000001};
            sdl.SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);
            int rend_w, rend_h;
            sdl.SDL_GetRendererOutputSize(r, &rend_w, &rend_h);
            int w, h;
            sdl.SDL_GetWindowSize(cast(SDL_Window*)window, &w, &h);
            sdl.SDL_RenderSetScale(r, rend_w/w, rend_h/h);
        }
        return cast(uintptr_t)r;

    });
    reg("Quit", (){
        sdl.SDL_Quit();
    });
    reg("PollEvent", (uintptr_t e){
        return cast(uintptr_t)sdl.SDL_PollEvent(cast(SDL_Event*)e);
    });
    reg("WaitEvent", (uintptr_t e){
            return cast(uintptr_t)sdl.SDL_WaitEvent(cast(SDL_Event*)e);
    });
    reg("SetRenderDrawColor", (uintptr_t rend, uintptr_t r, uintptr_t g, uintptr_t b, uintptr_t a){
        return cast(uintptr_t)sdl.SDL_SetRenderDrawColor(cast(SDL_Renderer*)rend, cast(ubyte)r, cast(ubyte)g, cast(ubyte)b, cast(ubyte)a);
    });
    reg("RenderClear", (uintptr_t rend){
        sdl.SDL_RenderClear(cast(SDL_Renderer*)rend);
    });
    reg("RenderFillRect", (uintptr_t rend, uintptr_t prect){
        sdl.SDL_RenderFillRect(cast(SDL_Renderer*)rend, cast(SDL_Rect*)prect);
    });
    reg("RenderPresent", (uintptr_t rend){
        sdl.SDL_RenderPresent(cast(SDL_Renderer*)rend);
    });
    reg("DestroyRenderer", (uintptr_t rend){
        sdl.SDL_DestroyRenderer(cast(SDL_Renderer*)rend);
    });
    reg("DestroyWindow", (uintptr_t window){
        sdl.SDL_DestroyWindow(cast(SDL_Window*)window);
    });
    reg("GetWindowSize", (uintptr_t window, uintptr_t px, uintptr_t py){
        sdl.SDL_GetWindowSize(cast(SDL_Window*)window, cast(int*)px, cast(int*)py);
    });
    reg("SetWindowSize", (uintptr_t window, uintptr_t x, uintptr_t y){
        sdl.SDL_SetWindowSize(cast(SDL_Window*)window, cast(int)x, cast(int)y);
    });
    reg("Delay", (uintptr_t tm){
        sdl.SDL_Delay(cast(uint)tm);
    });
    reg("GetTicks", (){
        return sdl.SDL_GetTicks64? sdl.SDL_GetTicks64() : sdl.SDL_GetTicks();
    });
    is_init = true;
    return &sdl_module;
}
