@echo off
rem
rem VEREN.BAT - Verify Rename File.
rem
rem     An argument count test could go here.
rem

if not exist "%1" goto nofile
if exist "%2" del %2
echo %1  renamed to  %2
ren %1 %2
goto end
:nofile
echo %1  not found
:end
rem
