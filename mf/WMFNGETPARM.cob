000100**   Copyright (c) Shell Stream Software LLC, All Rights Reserved.
000200**
000300**
000400**   File:       WMFNGETPARM.cob
000500**
000600**   Project:    WISP for Micro Focus with Native Screens
000700**
000800**   Purpose:    Display a WISP GETPARM screen
000900**
001000**   Build:      cob WMFNGETPARM.cob
001100**
001200**
001300
001400 IDENTIFICATION DIVISION.
001500 PROGRAM-ID.  WMFNGETPARM IS INITIAL.
001600 ENVIRONMENT DIVISION.
001700 CONFIGURATION SECTION.
001800 SPECIAL-NAMES.
001900     CURSOR         IS WISP-CURSOR
002000     CRT STATUS     IS WISP-CRT-STATUS.
002200
002300 DATA DIVISION.
002400
002500 WORKING-STORAGE SECTION.
002600*    Special-names CURSOR clause.
002700 01  WISP-CURSOR.
002800     05  WISP-CURSOR-LINE    PIC 999 VALUE 1.
002900     05  WISP-CURSOR-COL     PIC 999 VALUE 1.
003000
003100*    Special-names CRT STATUS clause.
       01  WISP-CRT-STATUS.
           05  WISP-CRT-STATUS-1            PIC X.
               88  WISP-CRT-STATUS-TERMINATED  VALUE '0'.
               88  WISP-CRT-STATUS-FUNCKEY     VALUE '1'.
               88  WISP-CRT-STATUS-ERROR       VALUE '9'.
           05  WISP-CRT-STATUS-2            PIC 99 COMP-X.
               88  WISP-CRT-EX-ESC             VALUE 0.
               88  WISP-CRT-EX-HELP            VALUE 33.
               88  WISP-CRT-EX-GETKEY          VALUE 34.
               88  WISP-CRT-EX-ENTER           VALUE 48.
           05  WISP-CRT-STATUS-3            PIC 99 COMP-X.

      *    Accept Omitted Field.
       01  WISP-OMITTED-FIELD PIC X.
004600
005300
005400*    WISP workstation working items.
005500 01  WISP-PFKEY                 PIC 99.
005600     88  WISP-PFKEY-ENTER       VALUE  0.
005700     88  WISP-PFKEY-HELP        VALUE 33.
005800     88  WISP-PFKEY-INVALID     VALUE 99.
005900
006000*    WISP DISPLAY AND READ working items.
006100 01  WISP-ALLOWABLE-PF-KEYS-CNT PIC 99.
006200
006300 01  WISP-DNR-DONE-FLAG        PIC X.
006400     88  WISP-DNR-DONE         VALUE "Y".
006500     88  WISP-DNR-NOT-DONE     VALUE "N".
006600
006700 01  CONTROL-FLAGS.
006800     05  V-01                PIC X(40) VALUE "PROTECT".
006900     05  V-02                PIC X(40) VALUE "PROTECT".
007000     05  V-03                PIC X(40) VALUE "PROTECT".
007100     05  V-04                PIC X(40) VALUE "PROTECT".
007200     05  V-05                PIC X(40) VALUE "PROTECT".
007300     05  V-06                PIC X(40) VALUE "PROTECT".
007400     05  V-07                PIC X(40) VALUE "PROTECT".
007500     05  V-08                PIC X(40) VALUE "PROTECT".
007600     05  V-09                PIC X(40) VALUE "PROTECT".
007700     05  V-10                PIC X(40) VALUE "PROTECT".
007800     05  V-11                PIC X(40) VALUE "PROTECT".
007900     05  V-12                PIC X(40) VALUE "PROTECT".
008000     05  V-13                PIC X(40) VALUE "PROTECT".
008100     05  V-14                PIC X(40) VALUE "PROTECT".
008200     05  V-15                PIC X(40) VALUE "PROTECT".
008300     05  V-16                PIC X(40) VALUE "PROTECT".
008400     05  V-17                PIC X(40) VALUE "PROTECT".
008500     05  V-18                PIC X(40) VALUE "PROTECT".
008600     05  V-19                PIC X(40) VALUE "PROTECT".
008700     05  V-20                PIC X(40) VALUE "PROTECT".
008800     05  V-21                PIC X(40) VALUE "PROTECT".
008900     05  V-22                PIC X(40) VALUE "PROTECT".
009000     05  V-23                PIC X(40) VALUE "PROTECT".
009100     05  V-24                PIC X(40) VALUE "PROTECT".
009200     05  V-25                PIC X(40) VALUE "PROTECT".
009300     05  V-26                PIC X(40) VALUE "PROTECT".
009400     05  V-27                PIC X(40) VALUE "PROTECT".
009500     05  V-28                PIC X(40) VALUE "PROTECT".
009600     05  V-29                PIC X(40) VALUE "PROTECT".
009700     05  V-30                PIC X(40) VALUE "PROTECT".
009800     05  V-31                PIC X(40) VALUE "PROTECT".
009900     05  V-32                PIC X(40) VALUE "PROTECT".
010000 01  REDEFINES CONTROL-FLAGS.
010100     05  V-XX OCCURS 32      PIC X(40).
010200
014000
014100 01  IDX1                    PIC 99.
014200
014300 LINKAGE SECTION.
014400 01  STATIC-TEXT             PIC X(1920).
014500 01  FILLER REDEFINES STATIC-TEXT.
014600     05  STATIC-TEXT-LINES   PIC X(80) OCCURS 24.
014700 01  FIELD-CNT               COMP-5 PIC XX.
014800 01  FIELD-TABLE.
014900     05  OCCURS 32.
015000         10  FIELD-ROW       COMP-5 PIC XX.
015100         10  FIELD-COL       COMP-5 PIC XX.
015200         10  FIELD-LEN       COMP-5 PIC XX.
015300         10  FIELD-FAC       PIC X COMP-X.
015400         10  FIELD-DATA      PIC X(79).
015500 01  FILLER REDEFINES FIELD-TABLE.
015600
015700     05  R-01                COMP-5 PIC XX.
015800     05  C-01                COMP-5 PIC XX.
015900     05  L-01                COMP-5 PIC XX.
016000     05  F-01                PIC X COMP-X.
016100     05  D-01                PIC X(79).
016200
016300     05  R-02                COMP-5 PIC XX.
016400     05  C-02                COMP-5 PIC XX.
016500     05  L-02                COMP-5 PIC XX.
016600     05  F-02                PIC X COMP-X.
016700     05  D-02                PIC X(79).
016800
016900     05  R-03                COMP-5 PIC XX.
017000     05  C-03                COMP-5 PIC XX.
017100     05  L-03                COMP-5 PIC XX.
017200     05  F-03                PIC X COMP-X.
017300     05  D-03                PIC X(79).
017400
017500     05  R-04                COMP-5 PIC XX.
017600     05  C-04                COMP-5 PIC XX.
017700     05  L-04                COMP-5 PIC XX.
017800     05  F-04                PIC X COMP-X.
017900     05  D-04                PIC X(79).
018000
018100     05  R-05                COMP-5 PIC XX.
018200     05  C-05                COMP-5 PIC XX.
018300     05  L-05                COMP-5 PIC XX.
018400     05  F-05                PIC X COMP-X.
018500     05  D-05                PIC X(79).
018600
018700     05  R-06                COMP-5 PIC XX.
018800     05  C-06                COMP-5 PIC XX.
018900     05  L-06                COMP-5 PIC XX.
019000     05  F-06                PIC X COMP-X.
019100     05  D-06                PIC X(79).
019200
019300     05  R-07                COMP-5 PIC XX.
019400     05  C-07                COMP-5 PIC XX.
019500     05  L-07                COMP-5 PIC XX.
019600     05  F-07                PIC X COMP-X.
019700     05  D-07                PIC X(79).
019800
019900     05  R-08                COMP-5 PIC XX.
020000     05  C-08                COMP-5 PIC XX.
020100     05  L-08                COMP-5 PIC XX.
020200     05  F-08                PIC X COMP-X.
020300     05  D-08                PIC X(79).
020400
020500     05  R-09                COMP-5 PIC XX.
020600     05  C-09                COMP-5 PIC XX.
020700     05  L-09                COMP-5 PIC XX.
020800     05  F-09                PIC X COMP-X.
020900     05  D-09                PIC X(79).
021000
021100     05  R-10                COMP-5 PIC XX.
021200     05  C-10                COMP-5 PIC XX.
021300     05  L-10                COMP-5 PIC XX.
021400     05  F-10                PIC X COMP-X.
021500     05  D-10                PIC X(79).
021600
021700     05  R-11                COMP-5 PIC XX.
021800     05  C-11                COMP-5 PIC XX.
021900     05  L-11                COMP-5 PIC XX.
022000     05  F-11                PIC X COMP-X.
022100     05  D-11                PIC X(79).
022200
022300     05  R-12                COMP-5 PIC XX.
022400     05  C-12                COMP-5 PIC XX.
022500     05  L-12                COMP-5 PIC XX.
022600     05  F-12                PIC X COMP-X.
022700     05  D-12                PIC X(79).
022800
022900     05  R-13                COMP-5 PIC XX.
023000     05  C-13                COMP-5 PIC XX.
023100     05  L-13                COMP-5 PIC XX.
023200     05  F-13                PIC X COMP-X.
023300     05  D-13                PIC X(79).
023400
023500     05  R-14                COMP-5 PIC XX.
023600     05  C-14                COMP-5 PIC XX.
023700     05  L-14                COMP-5 PIC XX.
023800     05  F-14                PIC X COMP-X.
023900     05  D-14                PIC X(79).
024000
024100     05  R-15                COMP-5 PIC XX.
024200     05  C-15                COMP-5 PIC XX.
024300     05  L-15                COMP-5 PIC XX.
024400     05  F-15                PIC X COMP-X.
024500     05  D-15                PIC X(79).
024600
024700     05  R-16                COMP-5 PIC XX.
024800     05  C-16                COMP-5 PIC XX.
024900     05  L-16                COMP-5 PIC XX.
025000     05  F-16                PIC X COMP-X.
025100     05  D-16                PIC X(79).
025200
025300     05  R-17                COMP-5 PIC XX.
025400     05  C-17                COMP-5 PIC XX.
025500     05  L-17                COMP-5 PIC XX.
025600     05  F-17                PIC X COMP-X.
025700     05  D-17                PIC X(79).
025800
025900     05  R-18                COMP-5 PIC XX.
026000     05  C-18                COMP-5 PIC XX.
026100     05  L-18                COMP-5 PIC XX.
026200     05  F-18                PIC X COMP-X.
026300     05  D-18                PIC X(79).
026400
026500     05  R-19                COMP-5 PIC XX.
026600     05  C-19                COMP-5 PIC XX.
026700     05  L-19                COMP-5 PIC XX.
026800     05  F-19                PIC X COMP-X.
026900     05  D-19                PIC X(79).
027000
027100     05  R-20                COMP-5 PIC XX.
027200     05  C-20                COMP-5 PIC XX.
027300     05  L-20                COMP-5 PIC XX.
027400     05  F-20                PIC X COMP-X.
027500     05  D-20                PIC X(79).
027600
027700     05  R-21                COMP-5 PIC XX.
027800     05  C-21                COMP-5 PIC XX.
027900     05  L-21                COMP-5 PIC XX.
028000     05  F-21                PIC X COMP-X.
028100     05  D-21                PIC X(79).
028200
028300     05  R-22                COMP-5 PIC XX.
028400     05  C-22                COMP-5 PIC XX.
028500     05  L-22                COMP-5 PIC XX.
028600     05  F-22                PIC X COMP-X.
028700     05  D-22                PIC X(79).
028800
028900     05  R-23                COMP-5 PIC XX.
029000     05  C-23                COMP-5 PIC XX.
029100     05  L-23                COMP-5 PIC XX.
029200     05  F-23                PIC X COMP-X.
029300     05  D-23                PIC X(79).
029400
029500     05  R-24                COMP-5 PIC XX.
029600     05  C-24                COMP-5 PIC XX.
029700     05  L-24                COMP-5 PIC XX.
029800     05  F-24                PIC X COMP-X.
029900     05  D-24                PIC X(79).
030000
030100     05  R-25                COMP-5 PIC XX.
030200     05  C-25                COMP-5 PIC XX.
030300     05  L-25                COMP-5 PIC XX.
030400     05  F-25                PIC X COMP-X.
030500     05  D-25                PIC X(79).
030600
030700     05  R-26                COMP-5 PIC XX.
030800     05  C-26                COMP-5 PIC XX.
030900     05  L-26                COMP-5 PIC XX.
031000     05  F-26                PIC X COMP-X.
031100     05  D-26                PIC X(79).
031200
031300     05  R-27                COMP-5 PIC XX.
031400     05  C-27                COMP-5 PIC XX.
031500     05  L-27                COMP-5 PIC XX.
031600     05  F-27                PIC X COMP-X.
031700     05  D-27                PIC X(79).
031800
031900     05  R-28                COMP-5 PIC XX.
032000     05  C-28                COMP-5 PIC XX.
032100     05  L-28                COMP-5 PIC XX.
032200     05  F-28                PIC X COMP-X.
032300     05  D-28                PIC X(79).
032400
032500     05  R-29                COMP-5 PIC XX.
032600     05  C-29                COMP-5 PIC XX.
032700     05  L-29                COMP-5 PIC XX.
032800     05  F-29                PIC X COMP-X.
032900     05  D-29                PIC X(79).
033000
033100     05  R-30                COMP-5 PIC XX.
033200     05  C-30                COMP-5 PIC XX.
033300     05  L-30                COMP-5 PIC XX.
033400     05  F-30                PIC X COMP-X.
033500     05  D-30                PIC X(79).
033600
033700     05  R-31                COMP-5 PIC XX.
033800     05  C-31                COMP-5 PIC XX.
033900     05  L-31                COMP-5 PIC XX.
034000     05  F-31                PIC X COMP-X.
034100     05  D-31                PIC X(79).
034200
034300     05  R-32                COMP-5 PIC XX.
034400     05  C-32                COMP-5 PIC XX.
034500     05  L-32                COMP-5 PIC XX.
034600     05  F-32                PIC X COMP-X.
034700     05  D-32                PIC X(79).
034800
034900 01  KEY-LIST.
035000     05  KEY-ITEM            PIC 99 OCCURS 40.
035100 01  KEY-CNT                 COMP-5 PIC XX.
035200 01  TERM-KEY                COMP-5 PIC XX.
035300
035400 SCREEN SECTION.
035500
       01  WISP-BLANK-SCREEN.
           05  BLANK SCREEN.

       01  STATIC-TEXT-SCREEN.
           05  BLANK SCREEN.
	   05  LINE 01 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(01).
	   05  LINE 02 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(02).
	   05  LINE 03 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(03).
	   05  LINE 04 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(04).
	   05  LINE 05 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(05).
	   05  LINE 06 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(06).
	   05  LINE 07 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(07).
	   05  LINE 08 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(08).
	   05  LINE 09 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(09).
	   05  LINE 10 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(10).
	   05  LINE 11 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(11).
	   05  LINE 12 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(12).
	   05  LINE 13 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(13).
	   05  LINE 14 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(14).
	   05  LINE 15 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(15).
	   05  LINE 16 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(16).
	   05  LINE 17 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(17).
	   05  LINE 18 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(18).
	   05  LINE 19 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(19).
	   05  LINE 20 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(20).
	   05  LINE 21 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(21).
	   05  LINE 22 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(22).
	   05  LINE 23 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(23).
	   05  LINE 24 COL 01 PIC X(80) FROM STATIC-TEXT-LINES(24).

036000
036100 01  ENTRY-FIELDS-SCREEN.
036300     05  USING   D-01 PIC X(80)
036400         LINE    R-01
036500         COL     C-01
036600         SIZE    L-01
036800         CONTROL V-01.
036900
037000     05  USING   D-02 PIC X(80)
037100         LINE    R-02
037200         COL     C-02
037300         SIZE    L-02
037500         CONTROL V-02.
037600
037700     05  USING   D-03 PIC X(80)
037800         LINE    R-03
037900         COL     C-03
038000         SIZE    L-03
038200         CONTROL V-03.
038300
038400     05  USING   D-04 PIC X(80)
038500         LINE    R-04
038600         COL     C-04
038700         SIZE    L-04
038900         CONTROL V-04.
039000
039100     05  USING   D-05 PIC X(80)
039200         LINE    R-05
039300         COL     C-05
039400         SIZE    L-05
039600         CONTROL V-05.
039700
039800     05  USING   D-06 PIC X(80)
039900         LINE    R-06
040000         COL     C-06
040100         SIZE    L-06
040300         CONTROL V-06.
040400
040500     05  USING   D-07 PIC X(80)
040600         LINE    R-07
040700         COL     C-07
040800         SIZE    L-07
041000         CONTROL V-07.
041100
041200     05  USING   D-08 PIC X(80)
041300         LINE    R-08
041400         COL     C-08
041500         SIZE    L-08
041700         CONTROL V-08.
041800
041900     05  USING   D-09 PIC X(80)
042000         LINE    R-09
042100         COL     C-09
042200         SIZE    L-09
042400         CONTROL V-09.
042500
042600     05  USING   D-10 PIC X(80)
042700         LINE    R-10
042800         COL     C-10
042900         SIZE    L-10
043100         CONTROL V-10.
043200
043300     05  USING   D-11 PIC X(80)
043400         LINE    R-11
043500         COL     C-11
043600         SIZE    L-11
043800         CONTROL V-11.
043900
044000     05  USING   D-12 PIC X(80)
044100         LINE    R-12
044200         COL     C-12
044300         SIZE    L-12
044500         CONTROL V-12.
044600
044700     05  USING   D-13 PIC X(80)
044800         LINE    R-13
044900         COL     C-13
045000         SIZE    L-13
045200         CONTROL V-13.
045300
045400     05  USING   D-14 PIC X(80)
045500         LINE    R-14
045600         COL     C-14
045700         SIZE    L-14
045900         CONTROL V-14.
046000
046100     05  USING   D-15 PIC X(80)
046200         LINE    R-15
046300         COL     C-15
046400         SIZE    L-15
046600         CONTROL V-15.
046700
046800     05  USING   D-16 PIC X(80)
046900         LINE    R-16
047000         COL     C-16
047100         SIZE    L-16
047300         CONTROL V-16.
047400
047500     05  USING   D-17 PIC X(80)
047600         LINE    R-17
047700         COL     C-17
047800         SIZE    L-17
048000         CONTROL V-17.
048100
048200     05  USING   D-18 PIC X(80)
048300         LINE    R-18
048400         COL     C-18
048500         SIZE    L-18
048700         CONTROL V-18.
048800
048900     05  USING   D-19 PIC X(80)
049000         LINE    R-19
049100         COL     C-19
049200         SIZE    L-19
049400         CONTROL V-19.
049500
049600     05  USING   D-20 PIC X(80)
049700         LINE    R-20
049800         COL     C-20
049900         SIZE    L-20
050100         CONTROL V-20.
050200
050300     05  USING   D-21 PIC X(80)
050400         LINE    R-21
050500         COL     C-21
050600         SIZE    L-21
050800         CONTROL V-21.
050900
051000     05  USING   D-22 PIC X(80)
051100         LINE    R-22
051200         COL     C-22
051300         SIZE    L-22
051500         CONTROL V-22.
051600
051700     05  USING   D-23 PIC X(80)
051800         LINE    R-23
051900         COL     C-23
052000         SIZE    L-23
052200         CONTROL V-23.
052300
052400     05  USING   D-24 PIC X(80)
052500         LINE    R-24
052600         COL     C-24
052700         SIZE    L-24
052900         CONTROL V-24.
053000
053100     05  USING   D-25 PIC X(80)
053200         LINE    R-25
053300         COL     C-25
053400         SIZE    L-25
053600         CONTROL V-25.
053700
053800     05  USING   D-26 PIC X(80)
053900         LINE    R-26
054000         COL     C-26
054100         SIZE    L-26
054300         CONTROL V-26.
054400
054500     05  USING   D-27 PIC X(80)
054600         LINE    R-27
054700         COL     C-27
054800         SIZE    L-27
055000         CONTROL V-27.
055100
055200     05  USING   D-28 PIC X(80)
055300         LINE    R-28
055400         COL     C-28
055500         SIZE    L-28
055700         CONTROL V-28.
055800
055900     05  USING   D-29 PIC X(80)
056000         LINE    R-29
056100         COL     C-29
056200         SIZE    L-29
056400         CONTROL V-29.
056500
056600     05  USING   D-30 PIC X(80)
056700         LINE    R-30
056800         COL     C-30
056900         SIZE    L-30
057100         CONTROL V-30.
057200
057300     05  USING   D-31 PIC X(80)
057400         LINE    R-31
057500         COL     C-31
057600         SIZE    L-31
057800         CONTROL V-31.
057900
058000     05  USING   D-32 PIC X(80)
058100         LINE    R-32
058200         COL     C-32
058300         SIZE    L-32
058500         CONTROL V-32.
058501


058600
058700
058800*    Invalid CRT status.
       01  WISP-INVALID-CRT-STATUS-SCREEN.
           05  BLANK SCREEN.
           05  LINE 10 COL 30 VALUE "INVALID CRT STATUS".
           05  LINE 14 COL  3 VALUE "WISP-CRT-STATUS-1 =".
           05  LINE 14 COL 25 PIC X  FROM WISP-CRT-STATUS-1.
           05  LINE 16 COL  3 VALUE "WISP-CRT-STATUS-2 =".
           05  LINE 16 COL 25 PIC 99 FROM WISP-CRT-STATUS-2.
           05  LINE 18 COL  3 VALUE "WISP-CRT-STATUS-3 =".
           05  LINE 18 COL 25 PIC 99 FROM WISP-CRT-STATUS-3.
           05  LINE 24 COL 25 VALUE "PRESS (ENTER) TO CONTINUE.".
059500
059600
059700
059800
059900 PROCEDURE DIVISION USING
060000             STATIC-TEXT,
060100             FIELD-CNT,
060200             FIELD-TABLE,
060300             KEY-LIST,
060400             KEY-CNT,
060500             TERM-KEY.
060600
060700 0000-START.
060800     PERFORM 1000-INIT.
060900     PERFORM 2000-GETPARM.
061000     PERFORM 3000-CLEANUP.
061100     PERFORM 9999-EXIT.
061200     PERFORM 9999-STOP.
061300
061400 1000-INIT.
061500

           MOVE "PROTECT" TO
	       V-01 V-02 V-03 V-04 V-05 V-06 V-07 V-08 V-09 V-10
	       V-11 V-12 V-13 V-14 V-15 V-16 V-17 V-18 V-19 V-20
	       V-21 V-22 V-23 V-24 V-25 V-26 V-27 V-28 V-29 V-30
	       V-31 V-32.

061600     PERFORM VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > FIELD-CNT
061700         CALL "WMFNFAC2SCREEN" USING
061800             FIELD-FAC(IDX1), V-XX(IDX1)
061900     END-PERFORM.
062000
062100 2000-GETPARM.
062200
062400     MOVE 0 TO WISP-CURSOR-COL, WISP-CURSOR-LINE.
062500     MOVE "N" TO WISP-DNR-DONE-FLAG.
062600     PERFORM UNTIL WISP-DNR-DONE
               DISPLAY WISP-BLANK-SCREEN
062700         DISPLAY STATIC-TEXT-SCREEN
062800         IF FIELD-CNT > 0 THEN
062900             DISPLAY ENTRY-FIELDS-SCREEN
063000             ACCEPT  ENTRY-FIELDS-SCREEN
063100                 EXCEPTION CONTINUE
063200             END-ACCEPT
063300         ELSE
063400             PERFORM WISP-DNR-ACCEPT-NOFIELDS
063700         END-IF
063800         PERFORM WISP-CHECK-PFKEY
063900     END-PERFORM.
064000
064100
064200 3000-CLEANUP.
064400     MOVE WISP-PFKEY TO TERM-KEY.
064500
064600 9999-EXIT.
064700     EXIT PROGRAM.
064800
064900 9999-STOP.
065000     STOP RUN.
065100
065200***** CHECK PF KEYS AFTER AN ACCEPT
065300 WISP-CHECK-PFKEY.
065400
           SET WISP-PFKEY-INVALID TO TRUE.
           IF WISP-CRT-STATUS-TERMINATED AND WISP-CRT-EX-ENTER
               MOVE 0 TO WISP-PFKEY
           ELSE IF WISP-CRT-STATUS-FUNCKEY
               IF WISP-CRT-EX-GETKEY OR WISP-CRT-EX-ESC
                   CALL "WMFNGETPFKEY" USING WISP-PFKEY
               ELSE
                   MOVE WISP-CRT-STATUS-2 TO WISP-PFKEY
               END-IF
           ELSE
               DISPLAY WISP-BLANK-SCREEN
               DISPLAY WISP-INVALID-CRT-STATUS-SCREEN
               PERFORM WISP-DNR-ACCEPT-NOFIELDS
               MOVE 0 TO WISP-PFKEY
           END-IF.
067100
067200     IF WISP-PFKEY-HELP THEN
067300         CALL "WMFNHELP"
067400     ELSE IF NOT WISP-PFKEY-INVALID THEN
067500         PERFORM VARYING IDX1 FROM 1 BY 1 UNTIL WISP-DNR-DONE
067600             OR IDX1 > KEY-CNT
067700             IF WISP-PFKEY = KEY-ITEM(IDX1)
067800                 MOVE "Y" TO WISP-DNR-DONE-FLAG
067900             END-IF
068000         END-PERFORM
068100
068200         IF WISP-DNR-DONE-FLAG = "N" THEN
068300              PERFORM WISP-DNR-ALARM
068400         END-IF
068500     END-IF.
068600
068700

065101***** ACCEPT A SCREEN WITH NO FIELDS
065102 WISP-DNR-ACCEPT-NOFIELDS.
065103     ACCEPT WISP-OMITTED-FIELD LINE 1 COL 1 WITH SECURE
065104         EXCEPTION CONTINUE
065105     END-ACCEPT.

      **** RING THE BELL
       WISP-DNR-ALARM.
           CALL X"E5".
