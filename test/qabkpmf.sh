# QABKPMF: this is a unix shell script that runs QABCKGRD for MF COBOL
#           and test submit with parameters.
wusage set invol=$1
wusage set outvol=$2
wputparm ENTER PRTFILE FILE=%%BCKP
wrun QABCKGRD

