@echo off
if exist .\bin\pgtools32.exe (del .\bin\pgtools32.exe)
if exist .\bin\pgtools64.exe (del .\bin\pgtools64.exe)
 
lazbuild --build-mode=Release32 pgtools.lpr -r -B
if errorlevel 1 goto erroroccurred

lazbuild --build-mode=Release64 pgtools.lpr -r -B
if errorlevel 1 goto erroroccurred

upx .\bin\pgtools32.exe
upx .\bin\pgtools64.exe

7z a "pgtools-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.7z" .\bin\pgtools32.exe .\bin\pgtools64.exe .\bin32\*.* .\bin64\*.*
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
