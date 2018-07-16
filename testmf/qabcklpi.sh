# qabcklpi.sh: this is a unix shell script that is submited from sample to
#              run qabckgrd then submit qabcklp2.sh for LPI COBOL
wusage set invol=back1
wusage set workvol=V$$
wputparm ENTER PRTFILE FILE=%%BK1A
qabckgrd
wputparm ENTER PRTFILE FILE=%%BK2A
qabckgrd
wsubmit QABCKLP2 SRC TEST

