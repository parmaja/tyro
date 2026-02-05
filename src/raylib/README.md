### Interduce ###

Header translation of [raylib 4](https://www.raylib.com/) for videogames programming, compiled in FPC and Delphi, Dynamic loading dll, with wrapping classes to make it easy to use.

### Install ###

Get the right version of raylib [raylib 4](https://github.com/raysan5/raylib/releases)
Rename dll/so file to raylib4.dll put it in path or bin folder

### Compile ###

Use fpc 3.2 or Delphi XE 10 to compile, we have a problem in Delphi for struct as return from function, read "struct" section in this article

http://rvelthuis.de/articles/articles-dlls.html

for that 8 bytes struct declared as int64 result then typecast it to that struct.

### Compitations ###

https://github.com/turborium/TurboRaylib

https://github.com/GuvaCode/Ray4Laz

https://github.com/tazdij/raylib-pas

https://github.com/drezgames/raylib-pascal