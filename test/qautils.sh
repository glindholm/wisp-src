#!/bin/sh
# qautils.sh	This shell script runs the QA tests on some utilities.
echo
echo wexists file xyz libin volin
wexists file xyz libin volin
RC=$?
echo [0] RC=$RC
if [ $RC != 0 ]
then
echo The file XYZ LIBIN VOLIN does not exist
exit
fi 
echo
echo wcopy xyz libin volin xxx libin volin
wcopy xyz libin volin xxx libin volin
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wcopy qqq libin volin xxx libin volin
wcopy qqq libin volin xxx libin volin
RC=$?
echo [20] RC=$RC

if [ $RC != 20 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wcopy xyz libin volin xyz newlib volout
wcopy xyz libin volin xyz newlib volout
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wcopy library newlib volout newlib volin
wcopy library newlib volout newlib volin
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wexists file qqq libin volin
wexists file qqq libin volin
RC=$?
echo [1] RC=$RC

if [ $RC != 1 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wexists library libin volin
wexists library libin volin
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wexists library qqqq volin
wexists library qqqq volin
RC=$?
echo [1] RC=$RC

if [ $RC != 1 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wexists volume volin
wexists volume volin
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wexists volume qqqq
wexists volume qqqq
RC=$?
echo [1] RC=$RC

if [ $RC != 1 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wrename xyz newlib volout new newlib
wrename xyz newlib volout new newlib
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wrename new newlib volout new2 newlib2
wrename new newlib volout new2 newlib2
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wrename new newlib volout new2 newlib2
wrename new newlib volout new2 newlib2
RC=$?
echo [20] RC=$RC

if [ $RC != 20 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wrename library newlib2 volout newlib3
wrename library newlib2 volout newlib3
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wscratch new2 newlib3 volout
wscratch new2 newlib3 volout
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wscratch new2 newlib3 volout
wscratch new2 newlib3 volout
RC=$?
echo [20] RC=$RC

if [ $RC != 20 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wscratch library newlib volin
wscratch library newlib volin
RC=$?
echo [0] RC=$RC

if [ $RC != 0 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo wscratch library newlib volin
wscratch library newlib volin
RC=$?
echo [16] RC=$RC

if [ $RC != 16 ]
then
echo This is WRONG!!!
echo
echo Do you want to stop [y,n]?
read ANS
if [ "$ANS" = "y" ]
then
echo BYE
exit
fi
fi

echo
echo END OF TEST
echo


