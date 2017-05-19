# tyke

## Linux

Dependencies:
 - opengl
 - sdl2
 - libfreetype
 - libftgl

1. checkout recursively (all submodules)
2. sudo apt-get install libfreetype6-dev libsdl2-dev libftgl-dev
3. cd cimgui/linuxdeps && ./build.sh
4. sudo cp libcimgui.so /usr/bin (sry about this im not sure how to make the .cabal work with a local lib on linux)
5. stack build

## Windows

To compile on windows, the dependencies are already included.

1. checkout submodules recursively
2. stack build
