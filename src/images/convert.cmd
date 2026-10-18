@echo off
setlocal enabledelayedexpansion
echo Convert to ICO
REM 1. Define the common icon sizes
set SIZES=16 24 32 48 64 128 256 512

REM 2. Loop through each size and export sharp PNGs using Inkscape
for %%S in (%SIZES%) do (
    echo Size %%S
    inkscape icon.svg -w %%S -h %%S -o "icon_%%S.png"
)

REM 3. Bundle them into a single .ico file using ImageMagick
magick convert icon_*.png ..\tyro.ico

copy icon_128.png ..\..\bin\tyro.png
del *.png

REM 4. Rebuild the Windows resource used by {$R *.res} in tyro.lpr / tyro.dpr
pushd ..
windres -i tyro.rc -o tyro.res
popd