@echo off
if exist .\pgserver.exe (del .\pgserver.exe)
 
lazbuild --build-mode=Release pgserver.lpr -r -B
if errorlevel 1 goto erroroccurred

upx .\pgserver.exe

cd ./bin
7z a -r "./pgserver-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.zip" pgserver.exe pgserver.ini.example README.md
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
