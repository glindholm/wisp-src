@echo off
echo This batch file Deletes the old VIDEO.LIB and recompiles all .C modules.
echo.
echo Press Ctrl-C to abort.
echo.
pause

echo Delete old VIDEO.LIB
del VIDEO.lib

echo Create new VIDEO.LIB
lib VIDEO /NOLOG ;

echo Deleting Video .OBJ files
DEL *.OBJ

echo Making VIDEO.LIB with NMK LIBVIDEO.DMF
echo.
NMK LIBVIDEO.DMF

echo Finished building VIDEO.LIB
