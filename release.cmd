@echo off
if exist pgtools32.exe (del pgtools32.exe)
if exist pgtools64.exe (del pgtools64.exe)
 
lazbuild --build-mode=Release32 pgtools.lpr -r -B
if errorlevel 1 goto erroroccurred

lazbuild --build-mode=Release64 pgtools.lpr -r -B
if errorlevel 1 goto erroroccurred

7z a "pgtools-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.7z" pgtools32.exe pgtools64.exe
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
