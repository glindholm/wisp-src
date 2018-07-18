# qabckmf2.sh: this is a unix shell script that is submited from qabckmf.sh
#              to run qabckgrd for AIX VS COBOL or Micro Focus COBOL/2
wusage set invol=BACK2
wusage set workvol=V$$
wputparm ENTER PRTFILE FILE=%%BK1B
wrun QABCKGRD
wputparm ENTER PRTFILE FILE=%%BK2B
wrun QABCKGRD

