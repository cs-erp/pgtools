@echo off

upx .\bin\pgtools32.exe
upx .\bin\pgtools64.exe

cd ./bin
7z a -r "./pgtools-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.7z" pgtools32.exe pgtools64.exe bin32\*.* bin64\*.* languages\*.po
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
