# QABCKACU: this is a unix shell script that runs QABCKGRD for ACUCOBOL
wusage set invol=back1
wusage set workvol=V$$
wputparm ENTER PRTFILE FILE=%%BK1A
wrun QABCKGRD
wputparm ENTER PRTFILE FILE=%%BK2A
wrun QABCKGRD
wsubmit QABCKAC2

