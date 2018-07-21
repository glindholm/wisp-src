000100**   Copyright (c) 2003 NeoMedia Technologies, All rights reserved.
000200**
000300**
000400**   File:       WMFNFAC2SCREEN.cob
000500**
000600**   Project:    WISP for Micro Focus with Native Screens
000700**
000800**   Purpose:    Convert Wang FAC into Control value
000900**
001000**   Build:      cob WMFNFAC2SCREEN.cob
001100**
001200**
001300/*****************************************************************
001400*                                                                *
001500*                       IDENTIFICATION DIVISION                  *
001600*                                                                *
001700******************************************************************
001800*
001900 IDENTIFICATION DIVISION.
002000 PROGRAM-ID.
002100     WMFNFAC2SCREEN.
002800
002900/*****************************************************************
003000*                                                                *
003100*                         DATA DIVISION                          *
003200*                                                                *
003300******************************************************************
003400*
003500 DATA DIVISION.
003600
003700*****************************************************************
003800*                                                                *
003900*                         WORKING STORAGE                        *
004000*                                                                *
004100******************************************************************
004200*
004300 WORKING-STORAGE SECTION.
004400
005900
006000 01  IDX                   PIC 99 VALUE 0.
006100 01  WS-FAC-CHAR           PIC X.
006200 01  WS-FAC-NUM REDEFINES WS-FAC-CHAR PIC X COMP-X.
006300
006400 01 COLOR-CONTROL-VALUES.
006500*----------------------------------------------------------------
006600*       (01)   NOLINE BRIGHT MODIFY  ALL
006700
006900    03  FILLER   PIC X(40) value "HIGHLIGHT".
007000
007100*       (02)   NOLINE BRIGHT MODIFY  UPCASE
007200
007400    03  FILLER   PIC X(40) value "HIGHLIGHT".
007500
007600*       (03)   NOLINE BRIGHT MODIFY  NUMERIC
007700
007900    03  FILLER   PIC X(40) value "HIGHLIGHT".
008000
008100*       (04)   Reserved
008200
008400    03  FILLER   PIC X(40) value "HIGHLIGHT".
008500*----------------------------------------------------------------
008600*       (05)   NOLINE BRIGHT PROTECT ALL
008700
008900    03  FILLER   PIC X(40) value "PROTECT,HIGHLIGHT".
009000
009100*       (06)   NOLINE BRIGHT PROTECT UPCASE
009200
009400    03  FILLER   PIC X(40) value "PROTECT,HIGHLIGHT".
009500
009600*       (07)   NOLINE BRIGHT PROTECT NUMERIC
009700
009900    03  FILLER   PIC X(40) value "PROTECT,HIGHLIGHT".
010000
010100*       (08)   Reserved
010200
010400    03  FILLER   PIC X(40) value "PROTECT,HIGHLIGHT".
010500*----------------------------------------------------------------
010600*       (09)   NOLINE DIM    MODIFY  ALL
010700
010900    03  FILLER   PIC X(40) value "LOWLIGHT".
011000
011100*       (10)   NOLINE DIM    MODIFY  UPCASE
011200
011400    03  FILLER   PIC X(40) value "LOWLIGHT".
011500
011600*       (11)   NOLINE DIM    MODIFY  NUMERIC
011700
011900    03  FILLER   PIC X(40) value "LOWLIGHT".
012000
012100*       (12)   Reserved
012200
012400    03  FILLER   PIC X(40) value "LOWLIGHT".
012500*----------------------------------------------------------------
012600*       (13)   NOLINE DIM    PROTECT ALL
012700
012900    03  FILLER   PIC X(40) value "PROTECT".
013000
013100*       (14)   NOLINE DIM    PROTECT UPCASE
013200
013400    03  FILLER   PIC X(40) value "PROTECT".
013500
013600*       (15)   NOLINE DIM    PROTECT NUMERIC
013700
013900    03  FILLER   PIC X(40) value "PROTECT".
014000
014100*       (16)   Reserved
014200
014400    03  FILLER   PIC X(40) value "PROTECT".
014500*----------------------------------------------------------------
014600*       (17)   NOLINE BLINK  MODIFY  ALL
014700
014900    03  FILLER   PIC X(40) value "BLINK,HIGHLIGHT".
015000
015100*       (18)   NOLINE BLINK  MODIFY  UPCASE
015200
015400    03  FILLER   PIC X(40) value "BLINK,HIGHLIGHT".
015500
015600*       (19)   NOLINE BLINK  MODIFY  NUMERIC
015700
015900    03  FILLER   PIC X(40) value "BLINK,HIGHLIGHT".
016000
016100*       (20)   Reserved
016200
016400    03  FILLER   PIC X(40) value "BLINK,HIGHLIGHT".
016500*----------------------------------------------------------------
016600*       (21)   NOLINE BLINK  PROTECT ALL
016700
016900    03  FILLER   PIC X(40) value "PROTECT,BLINK,HIGHLIGHT".
017000
017100*       (22)   NOLINE BLINK  PROTECT UPCASE
017200
017400    03  FILLER   PIC X(40) value "PROTECT,BLINK,HIGHLIGHT".
017500
017600*       (23)   NOLINE BLINK  PROTECT NUMERIC
017700
017900    03  FILLER   PIC X(40) value "PROTECT,BLINK,HIGHLIGHT".
018000
018100*       (24)   Reserved
018200
018400    03  FILLER   PIC X(40) value "PROTECT,BLINK,HIGHLIGHT".
018500*----------------------------------------------------------------
018600*       (25)   NOLINE BLANK  MODIFY  ALL
018700
018900    03  FILLER   PIC X(40) value "SECURE".
019000
019100*       (26)   NOLINE BLANK  MODIFY  UPCASE
019200
019400    03  FILLER   PIC X(40) value "SECURE".
019500
019600*       (27)   NOLINE BLANK  MODIFY  NUMERIC
019700
019900    03  FILLER   PIC X(40) value "SECURE".
020000
020100*       (28)   Reserved
020200
020400    03  FILLER   PIC X(40) value "SECURE".
020500*----------------------------------------------------------------
020600*       (29)   NOLINE BLANK  PROTECT ALL
020700
020900    03  FILLER   PIC X(40) value "PROTECT,SECURE".
021000
021100*       (30)   NOLINE BLANK  PROTECT UPCASE
021200
021400    03  FILLER   PIC X(40) value "PROTECT,SECURE".
021500
021600*       (31)   NOLINE BLANK  PROTECT NUMERIC
021700
021900    03  FILLER   PIC X(40) value "PROTECT,SECURE".
022000
022100*       (32)   Reserved
022200
022400    03  FILLER   PIC X(40) value "PROTECT,SECURE".
022500*----------------------------------------------------------------
022600*       (33)   LINE   BRIGHT MODIFY  ALL
022700
022900    03  FILLER   PIC X(40) value "UNDERLINE,HIGHLIGHT".
023000
023100*       (34)   LINE   BRIGHT MODIFY  UPCASE
023200
023400    03  FILLER   PIC X(40) value "UNDERLINE,HIGHLIGHT".
023500
023600*       (35)   LINE   BRIGHT MODIFY  NUMERIC
023700
023900    03  FILLER   PIC X(40) value "UNDERLINE,HIGHLIGHT".
024000
024100*       (36)   Reserved
024200
024400    03  FILLER   PIC X(40) value "UNDERLINE,HIGHLIGHT".
024500*----------------------------------------------------------------
024600*       (37)   LINE   BRIGHT PROTECT ALL
024700
024900    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE,HIGHLIGHT".
025000
025100*       (38)   LINE   BRIGHT PROTECT UPCASE
025200
025400    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE,HIGHLIGHT".
025500
025600*       (39)   LINE   BRIGHT PROTECT NUMERIC
025700
025900    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE,HIGHLIGHT".
026000
026100*       (40)   Reserved
026200
026400    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE,HIGHLIGHT".
026500*----------------------------------------------------------------
026600*       (41)   LINE   DIM    MODIFY  ALL
026700
026900    03  FILLER   PIC X(40) value "UNDERLINE".
027000
027100*       (42)   LINE   DIM    MODIFY  UPCASE
027200
027400    03  FILLER   PIC X(40) value "UNDERLINE".
027500
027600*       (43)   LINE   DIM    MODIFY  NUMERIC
027700
027900    03  FILLER   PIC X(40) value "UNDERLINE".
028000
028100*       (44)   Reserved
028200
028400    03  FILLER   PIC X(40) value "UNDERLINE".
028500*----------------------------------------------------------------
028600*       (45)   LINE   DIM    PROTECT ALL
028700
028900    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE".
029000
029100*       (46)   LINE   DIM    PROTECT UPCASE
029200
029400    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE".
029500
029600*       (47)   LINE   DIM    PROTECT NUMERIC
029700
029900    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE".
030000
030100*       (48)   Reserved
030200
030400    03  FILLER   PIC X(40) value "PROTECT,UNDERLINE".
030500*----------------------------------------------------------------
030600*       (49)   LINE   BLINK  MODIFY  ALL
030700
030900    03  FILLER   PIC X(40) value "UNDERLINE,BLINK,HIGHLIGHT".
031000
031100*       (50)   LINE   BLINK  MODIFY  UPCASE
031200
031400    03  FILLER   PIC X(40) value "UNDERLINE,BLINK,HIGHLIGHT".
031500
031600*       (51)   LINE   BLINK  MODIFY  NUMERIC
031700
031900    03  FILLER   PIC X(40) value "UNDERLINE,BLINK,HIGHLIGHT".
032000
032100*       (52)   Reserved
032200
032400    03  FILLER   PIC X(40) value "UNDERLINE,BLINK,HIGHLIGHT".
032500*----------------------------------------------------------------
032600*       (53)   LINE   BLINK  PROTECT ALL
032700
032900    03  FILLER   PIC X(40) value 
                       "PROTECT,UNDERLINE,BLINK,HIGHLIGHT".
033000
033100*       (54)   LINE   BLINK  PROTECT UPCASE
033200
033400    03  FILLER   PIC X(40) value 
                       "PROTECT,UNDERLINE,BLINK,HIGHLIGHT".
033500
033600*       (55)   LINE   BLINK  PROTECT NUMERIC
033700
033900    03  FILLER   PIC X(40) value 
                       "PROTECT,UNDERLINE,BLINK,HIGHLIGHT".
034000
034100*       (56)   Reserved
034200
034400    03  FILLER   PIC X(40) value 
                       "PROTECT,UNDERLINE,BLINK,HIGHLIGHT".
034500*----------------------------------------------------------------
034600*       (57)   LINE   BLANK  MODIFY  ALL
034700
034900    03  FILLER   PIC X(40) value "SECURE,UNDERLINE".
035000
035100*       (58)   LINE   BLANK  MODIFY  UPCASE
035200
035400    03  FILLER   PIC X(40) value "SECURE,UNDERLINE".
035500
035600*       (59)   LINE   BLANK  MODIFY  NUMERIC
035700
035900    03  FILLER   PIC X(40) value "SECURE,UNDERLINE".
036000
036100*       (60)   Reserved
036200
036400    03  FILLER   PIC X(40) value "SECURE,UNDERLINE".
036500*----------------------------------------------------------------
036600*       (61)   LINE   BLANK  PROTECT ALL
036700
036900    03  FILLER   PIC X(40) value "PROTECT,SECURE,UNDERLINE".
037000
037100*       (62)   LINE   BLANK  PROTECT UPCASE
037200
037400    03  FILLER   PIC X(40) value "PROTECT,SECURE,UNDERLINE".
037500
037600*       (63)   LINE   BLANK  PROTECT NUMERIC
037700
037900    03  FILLER   PIC X(40) value "PROTECT,SECURE,UNDERLINE".
038000
038100*       (64)   Reserved
038200
038400    03  FILLER   PIC X(40) value "PROTECT,SECURE,UNDERLINE".
      *----------------------------------------------------------------
038500 01 COLOR-CONTROL-TABLE REDEFINES COLOR-CONTROL-VALUES.
038600    03  COLOR-CONTROL       OCCURS 64 TIMES.
038800        05  CONTROL-VALUE   PIC X(40).
038900
039000/*****************************************************************
039100*                                                                *
039200*                       LINKAGE SECTION                          *
039300*                                                                *
039400******************************************************************
039500*
039600 LINKAGE SECTION.
039700 01  FAC-CHAR               PIC X(1).
039900 01  CONTROL-STR            PIC X(40).
040000
040100/*****************************************************************
040200*                                                                *
040300*                         PROCEDURE DIVISION                     *
040400*                                                                *
040500******************************************************************
040600*
040700 PROCEDURE DIVISION USING FAC-CHAR,
040900                          CONTROL-STR.
041000
041100 0000-START.
042300     MOVE FAC-CHAR TO WS-FAC-CHAR.
042400*  Remove FAC bit
042500     SUBTRACT 128 FROM WS-FAC-NUM.
042600*  Remove FAC MOD bit
042700     IF WS-FAC-NUM >= 64 THEN
042800         SUBTRACT 64 FROM WS-FAC-NUM.
042900
043000     ADD 1 TO WS-FAC-NUM GIVING IDX.
043100     
041400     MOVE CONTROL-VALUE(IDX) TO CONTROL-STR.
041500
041600 9999-EXIT.
041700     EXIT PROGRAM.
041800
041900 9999-STOP.
042000     STOP RUN.
042100
043200**
043300**   End of WMFNFAC2SCREEN.cob
043400**
