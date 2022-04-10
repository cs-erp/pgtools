@echo off
if exist .\bin\pgtools64.exe (del .\bin\pgtools64.exe)

lazbuild --build-mode=Release64 pgtools.lpr -r -B
if errorlevel 1 goto erroroccurred

upx .\bin\pgtools64.exe

cd ./bin
7z a -r "./pgtools-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.7z" pgtools64.exe bin64\*.*
if errorlevel 1 goto erroroccurred

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
