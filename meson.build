project('ddasm', 'd')

# Meson is passing braindead linker args (why are you linking against phobos
# and libdruntime)?
# Why are you linking my exe with -undefined,dynamic_lookup? I want linker
# errors at link time, not runtime!
# ugh.

# On the plus side, it generates correct ninja files.

add_global_arguments('-betterC', '-i', language : 'd')
executable('ds2dasm', 'source/ds2dasm.d', include_directories:['source'])
executable('ddasm', 'source/ddasm.d', include_directories:['source'])
