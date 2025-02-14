


#define _Float16 float
#define SDL_DISABLE_ARM_NEON_H 1
#define SDL_DISABLE_IMMINTRIN_H 1
#define SDL_DISABLE_MMINTRIN_H 1
#define SDL_DISABLE_MM3DNOW_H 1
#define SDL_DISABLE_XMMINTRIN_H 1
#define SDL_DISABLE_EMMINTRIN_H 1
#define SDL_DISABLE_PMMINTRIN_H 1

#ifdef __APPLE__
#include <SDL2/SDL.h>
#include <SDL2_image/SDL_image.h>
#include <SDL2_ttf/SDL_ttf.h>
#include <SDL2_mixer/SDL_mixer.h>
#else

// <stdint.h> has literals ending in i64, weird msvc extension
// This was fixed in newer dmd, but I dont think ldc 1.35 has the fix.
#ifdef _WIN64
    #define SIZE_MAX 0xffffffffffffffffllu
#elif defined(_WIN32)
    #define SIZE_MAX 0xfffffffflu
#endif
// try to get SDL to not include these files
#define SDL_cpuinfo_h_ 1
#define SDL_endian_h_ 1
#include <SDL.h>
#include <SDL_image.h>
#include <SDL_ttf.h>
// #include <SDL_mixer.h>
#endif
