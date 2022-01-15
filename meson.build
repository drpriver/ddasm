project('ddasm', 'd')

# Meson is passing braindead linker args (why are you linking against phobos
# and libdruntime)?
# Why are you linking my exe with -undefined,dynamic_lookup? I want linker
# errors at link time, not runtime!
# ugh.

# On the plus side, it generates correct ninja files.

add_global_arguments('-betterC', '-i', language : 'd')
executable('ds2dasm', 'ds2dasm.d')
executable('ddasm', 'ddasm.d')
