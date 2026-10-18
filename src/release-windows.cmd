@echo off

set file=tyro-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%-alpha.7z
cd ..\bin
7z a -tzip "..\release\%file%" @release.conf
cd ..
7z a -tzip "release\%file%" @games\games.conf
goto noerrors

:erroroccurred
echo ???????????????????
echo    Error compile
echo ???????????????????
pause
goto :EOF
:noerrors
echo #######################
echo    Compile completed
echo #######################
pause
