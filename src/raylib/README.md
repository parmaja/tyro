### Interduce ###

Header translation of [raylib v5.5](https://www.raylib.com/) for videogames programming, compiled in FPC and Delphi, Dynamic loading dll, with wrapping classes to make it easy to use.

### Install ###

Get the right version of [raylib](https://github.com/raysan5/raylib/releases)

Extract dll/so file raylib.dll put it in path or bin folder, rename the dll file to `raylib-5.5-64.dll`

### Compile ###

Use fpc 3.3 or Delphi XE 10 to compile, we have a problem in Delphi for struct as return from function, read "struct" section in this article

http://rvelthuis.de/articles/articles-dlls.html

For that 8 bytes struct declared as int64 result then typecast it to that struct, look at `GetMousePosition`.

### Compitations ###

https://github.com/turborium/TurboRaylib

https://github.com/GuvaCode/Ray4Laz

https://github.com/tazdij/raylib-pas

https://github.com/drezgames/raylib-pascal