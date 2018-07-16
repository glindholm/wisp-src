2					! Number of forms in the form file.
MOMVHD04
I					! Default display enhancement.
HIB					! Error enhancement.
1					! Message window line.
I					! Window enhancement.
MOMVHD04
N					! Repeat Option
C					! Next Form Option
$HEAD					! Next Form
14
MOMVHD04
              ORIGINATION COMMITMENT MANAGEMENT SUBSYSTEM MENU

  ADD   CHANGE   DISPLAY   PURGE   FUNCTION

  A1      C1       D1       P1     BUILDER RECORDS

          C2       D2       P2     COMMITMENT DATA FOR A BUILDER

  R                                RUN REPORTS

  USER ID  ID..     PASSWORD [PASS]
  SELECT:  MODE ASS BUILDER  BLDRNO COMMITMENT BLDRCOM REPORT # RPTNO.

10					! Number of lines
H  3  1 80				! The lines
H  5  1 80
H 11  1 80
H 14  1 80
V  3  1 12
v  5  7  7
v  5 16  7
v  5 26  7
v  5 34  7
V  3 80 12
10					! The number of fields
COMPNAME COMPNAME	14  1 23 36 D
			NONE
			CHAR
			0
			0
			0
DATE	TODAY		 1  1 63  8 D
			NONE
			MDY
			0
			0
			0
TIME	CURRTIME	 2  1 73  6 D
			NONE
			DIG
			0
			0
			0
ID	USERID		10 12 12  4 R
			I
			CHAR
			0
			0
			0
PASS	USERPASS	11 12 31  4 R
			S
			CHAR
			0
			0
			0
A	PROCMODE	12 13 17  1 R
			I
			CHAR
			0
			0
			0
SS	SELECTN		13 13 18  2 P
			I
			DIG
			0
			0
			0
BLDRNO	BLDRNO		 9 13 30  6 P
			I
			DIG
			0
			0
			0
BLDRCOM	BLDRCOM		17 13 48  7 O
			I
			CHAR
			0
			0
			0
RPTNO	RPTNO		15 13 65  6 P
			I
			CHAR
			0
			0
			0
MOMV00000
N					! Repeat Option
C					! Next Form Option
$HEAD					! Next Form
4

 MOMV00000          THIS IS NOTHING BUT A TEST         DATE
                    Field here = 

4					! Number of lines
H  1  1 80
H  4  1 80				! The lines
V  1  1 4
V  1 80 4
1					! The number of fields
SS	SELECTN		13  3 34  13 R
			I
			DIG
			0
			0
			0
