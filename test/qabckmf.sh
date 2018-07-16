# qabckmf.sh: this is a unix shell script that is submited from sample to
#           run qabckgrd then submit qabckmf2.sh for AIX VS COBOL or MF COBOL/2
wusage set invol=back1
wusage set workvol=V$$
wputparm ENTER PRTFILE FILE=%%BK1A
wrun QABCKGRD
wputparm ENTER PRTFILE FILE=%%BK2A
wrun QABCKGRD
wsubmit QABCKMF2

