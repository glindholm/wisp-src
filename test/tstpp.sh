#
#
#	tstpp.sh	- test putparms as part of the WISP QA
#
#

echo wputparm CLEAR
wputparm CLEAR

echo wputparm ENTER INPUT -c 3 FILE=XYZ LIB=MYLIB VOL=VOL100
wputparm ENTER INPUT -c 3 FILE=XYZ LIB=MYLIB VOL=VOL100

echo wputparm DISPLAY OUTPUT -a PF1 FILE=ABC
wputparm DISPLAY OUTPUT -a PF1 FILE=ABC

echo wputparm ENTER PRINT -l PRINT FILE=##FRED
wputparm ENTER PRINT -l PRINT FILE=##FRED

echo wputparm SHOW
wputparm SHOW
echo Should be 3 putparms in list
echo Press enter to continue
read

echo wputparm CLEAR -l PRINT
wputparm CLEAR -l PRINT

echo wputparm SHOW
wputparm SHOW
echo The ENTER PRINT putparm should be deleted
echo Press enter to continue
read

echo wputparm CLEAR
wputparm CLEAR

echo wputparm SHOW
wputparm SHOW
echo There should be no putparms in the list
echo Press enter to continue
read

echo wputparm ENTER SEQFIL -l SEQFIL
wputparm ENTER SEQFIL -l SEQFIL

echo 
echo Going to run QAFILEIO
echo "From the menu press (6) to create the sequential file"
echo Next enter FILE=##TMP
echo Then exit
echo Press enter to continue
read
wrun QAFILEIO

echo wputparm SHOW
wputparm SHOW
echo The SEQFIL putparm should have the modified values and USED
echo Press enter to continue
read

echo wputparm GET FILE -l SEQFIL
echo FILE = `wputparm GET FILE -l SEQFIL`
echo Should have got the FILE value
echo Press enter to continue
read

echo wputparm DISPLAY INPUT -l NEW -r SEQFIL
wputparm DISPLAY INPUT -l NEW -r SEQFIL
echo wputparm SHOW
wputparm SHOW

echo Going to run DISPLAY
echo Press enter to continue
read

display
echo wputparm SHOW
wputparm SHOW
echo INPUT should be filled in with the values of SEQFILE
echo Press enter to continue
read

echo wputparm CLEAR
wputparm CLEAR
echo wputparm SHOW
wputparm SHOW

