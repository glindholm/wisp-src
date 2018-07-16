# QABCKAC2: this is a unix shell script that runs QABCKGRD for ACUCOBOL
wusage set invol=BACK2
wusage set workvol=V$$
wputparm ENTER PRTFILE FILE=%%BK1B
wrun QABCKGRD
wputparm ENTER PRTFILE FILE=%%BK2B
wrun QABCKGRD

