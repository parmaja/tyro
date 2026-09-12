@echo off
cd chipmunk
rem Build Chipmunk2D as a DLL for Tyro (MinGW-w64).
rem Requires gcc on PATH or edit GCC below.
set GCC=C:\Programs\mingw64\bin\gcc.exe
"%GCC%" -shared -O2 -o "..\..\bin\chipmunk.dll" -Iinclude src\*.c -lm
if errorlevel 1 goto err
echo Built ..\..\bin\chipmunk.dll
goto end
:err
echo Build FAILED
:end