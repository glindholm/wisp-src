# qabcklp2.sh: this is a unix shell script that is submited from qabcklpi.sh
#              to run qabckgrd for LPI COBOL
wusage set invol=BACK2
wusage set workvol=V$$
wputparm ENTER PRTFILE FILE=%%BK1B
qabckgrd
wputparm ENTER PRTFILE FILE=%%BK2B
wrun QqabckgrdA

