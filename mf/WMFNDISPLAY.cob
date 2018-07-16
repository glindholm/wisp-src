000100**   Copyright (c) 2003 NeoMedia Technologies, All rights reserved.
000200**   $Id:$
000300**
000400**   File:       WMFNDISPLAY.cob
000500**
000600**   Project:    WISP for Micro Focus with Native Screens
000700**
000800**   Purpose:    Support the DISPLAY statement.
000900**
001000**   Build:      cob WMFNDISPLAY.cob
001100**
001200**
001300
001400 IDENTIFICATION DIVISION.
001500 PROGRAM-ID.
001600     WMFNDISPLAY.
001700
001800 ENVIRONMENT DIVISION.
001900 CONFIGURATION SECTION.
002000 SPECIAL-NAMES.
002100     CURSOR         IS WISP-CURSOR
002200     CRT STATUS     IS WISP-CRT-STATUS.
002400
002500
002600 DATA DIVISION.
002700
002800 WORKING-STORAGE SECTION.
002900
003000*****************************************************************
003100*    Native Screens fields
003200*****************************************************************
003300
003400*    Special-names CURSOR clause.
003500 01  WISP-CURSOR.
003600     05  WISP-CURSOR-LINE    PIC 999 VALUE 1.
003700     05  WISP-CURSOR-COL     PIC 999 VALUE 1.
003800

      *    Accept Omitted Field.
       01  WISP-OMITTED-FIELD PIC X.

      *    Special-names CRT STATUS clause.
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

      *    WISP workstation working items.
       01  WISP-PFKEY                 PIC 99.
           88  WISP-PFKEY-ENTER       VALUE  0.
           88  WISP-PFKEY-HELP        VALUE 33.
           88  WISP-PFKEY-INVALID     VALUE 99.
       01  WISP-CURSOR-POSITION.
           05  WISP-CURSOR-POSITION-COL COMP-5 PIC S9(4).
           05  WISP-CURSOR-POSITION-ROW COMP-5 PIC S9(4).

005400
006100
006200
006300 LINKAGE SECTION.
006400 01  WISP-APPLICATION-NAME    PIC X(8).
006500 01  WISP-DISPLAY-FIELDS-DATA PIC X(1185).
006600 01  FILLER REDEFINES WISP-DISPLAY-FIELDS-DATA.
006700     05  WISP-DISPLAY-FIELDS OCCURS 15 PIC X(79).
006800
006900
007000 SCREEN SECTION.

       01  WISP-BLANK-SCREEN.
           05  BLANK SCREEN.
007100
007200*    Native translation of DISPLAY verb.
007300 01  WISP-DISPLAY-SCREEN.
           05  BLANK SCREEN.
007400     05  LINE 1 COL 2 VALUE "DISPLAY FROM PROGRAM:".
007500     05  LINE 1 COL 24 PIC X(8) HIGHLIGHT
               FROM WISP-APPLICATION-NAME.
           05  LINE 6 COL 1 VALUE
           "----------------------------------------".
           05  LINE 6 COL 41 VALUE
           "----------------------------------------".
007900     05  LINE 7 COL 2 VALUE "PRESS (ENTER) TO CONTINUE PROGRAM.".
           05  LINE 8 COL 1 VALUE " ".
007600     05  LINE 7.
007700         10  COL 2 LINE PLUS 1 PIC X(79) OCCURS 15
007800             FROM WISP-DISPLAY-FIELDS.
008000
008100
008200 PROCEDURE DIVISION USING WISP-APPLICATION-NAME
008300                          WISP-DISPLAY-FIELDS-DATA.
008400
008500 WISP-DISPLAY-PARA.
008600     PERFORM WITH TEST AFTER UNTIL WISP-PFKEY-ENTER
008900         DISPLAY WISP-DISPLAY-SCREEN
009000         ACCEPT WISP-OMITTED-FIELD LINE 1 COL 1 WITH SECURE
009100             EXCEPTION CONTINUE
009200         END-ACCEPT
               IF WISP-CRT-STATUS-TERMINATED AND WISP-CRT-EX-ENTER
                   MOVE 0 TO WISP-PFKEY
               ELSE IF WISP-CRT-STATUS-FUNCKEY
                   IF WISP-CRT-EX-GETKEY OR WISP-CRT-EX-ESC
                       CALL "WMFNGETPFKEY" USING WISP-PFKEY
                   ELSE
                       MOVE WISP-CRT-STATUS-2 TO WISP-PFKEY
                   END-IF
               END-IF
               IF WISP-PFKEY-HELP THEN
                   CALL "WMFNHELP"
	           DISPLAY WISP-BLANK-SCREEN
               END-IF
009700     END-PERFORM.
009800
009900
010000 9999-EXIT.
010100     EXIT PROGRAM.
010200
010300 9999-STOP.
010400     STOP RUN.
