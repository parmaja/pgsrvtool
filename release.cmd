@echo off
if exist .\pgsrvtool.exe (del .\pgsrvtool.exe)
 
lazbuild --build-mode=Release pgserver.lpr -r -B
if errorlevel 1 goto erroroccurred

upx .\pgsrvtool.exe

cd ./bin
7z a -r "./pgsrvtool-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.zip" pgsrvtool.exe pgsrvtool.ini.example README.md
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
