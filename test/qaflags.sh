#
#	qaflags.sh
#
#	wusage flags set <flag>=<logical>
#
#   <flag> = HELP,SETFILE,SETPRINT,SETSUB,FILES,MODFILES,SYSTEM,PRTQUE,BATCHQ,
#            ERRLOG,UTILS,DISPLAY,EDIT,DISPRINT,KCSI,TERMINAL,PSEUDO,CURSOR,
#            SCREEN,COMMANDS,SUBMIT,SAVE,PRINTSCR,CANCEL
#
#   <logical> = Y, N, T, F

echo Setting all flags to 'N'
wusage flags set HELP=N
wusage flags set SETFILE=N
wusage flags set SETPRINT=N
wusage flags set SETSUB=N
wusage flags set FILES=N
wusage flags set MODFILES=N
wusage flags set SYSTEM=N
wusage flags set PRTQUE=N
wusage flags set BATCHQ=N
wusage flags set ERRLOG=N
wusage flags set UTILS=N
wusage flags set DISPLAY=N
wusage flags set EDIT=N
wusage flags set DISPRINT=N
wusage flags set KCSI=N
wusage flags set TERMINAL=N
wusage flags set PSEUDO=N
wusage flags set CURSOR=N
wusage flags set SCREEN=N
wusage flags set COMMANDS=N
wusage flags set SUBMIT=N
wusage flags set SAVE=N
wusage flags set PRINTSCR=N
wusage flags set CANCEL=N

echo Press enter to check flags...
read
wusage flags

echo Set HELP=Y
wusage flags set HELP=Y
echo Press enter to run wshell, 
echo 'Only (1) RUN and (16) EXIT should be active.'
read
wshell

echo Setting all flags to 'Y'
wusage flags set SETFILE=Y
wusage flags set SETPRINT=Y
wusage flags set SETSUB=Y
wusage flags set FILES=Y
wusage flags set MODFILES=Y
wusage flags set SYSTEM=Y
wusage flags set PRTQUE=Y
wusage flags set BATCHQ=Y
wusage flags set ERRLOG=Y
wusage flags set UTILS=Y
wusage flags set DISPLAY=Y
wusage flags set EDIT=Y
wusage flags set DISPRINT=Y
wusage flags set KCSI=Y
wusage flags set TERMINAL=Y
wusage flags set PSEUDO=Y
wusage flags set CURSOR=Y
wusage flags set SCREEN=Y
wusage flags set COMMANDS=Y
wusage flags set SUBMIT=Y
wusage flags set SAVE=Y
wusage flags set PRINTSCR=Y
wusage flags set CANCEL=Y

echo Press enter to check flags...
read
wusage flags

echo "Done"
