@echo off
if exist .\bin\pgtools64.exe (del .\bin\pgtools64.exe)

lazbuild --build-mode=Release64 pgtools.lpr -r -B
if errorlevel 1 goto erroroccurred

upx .\bin\pgtools64.exe

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
