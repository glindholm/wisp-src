000100**   Copyright (c) Shell Stream Software LLC, All rights reserved.
000200**   $Id:$
000300**
000400**   File:       mflink.wcb
000500**
000600**   Project:    WISP for Micro Focus COBOL
000700**
      ******************************************************************
      *
      *  MFLINK	- WISP Micro Focus LINK INTERFACE
      *
      *           This program is used by WISP as a frontend to the
      *           program that was called by VSSUB "LINK" to facilitate
      *           parameter passing.  The file "MFLINK.int" must be 
      *           present on the $COBPATH path.
      *
      *           To build use the following command.
      *
      *                   $ cob mflink.cob
      *                   $ mv mflink.int MFLINK.int
      *
      *           There is a maximum of 32 parameters that may
      *           be passed through a LINK.
      *
      *           The maximum size of each parameter may be
      *           changed if neccesary by changing the 
      *           working-storage sizes in the program.
      *
      ******************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       MFLINK.
       AUTHOR.           Shell Stream Software LLC
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  ARGCNT        BINARY PIC S9(4).
       01  LINK-PATH     PIC X(80).
       01  CMD           PIC X(80).

      *
      * The Maximum length of a passed parameter is determined by the
      * size of the variables below. They are currently set at 1024
      * but they may be increased to any size you prefer.
      *

       01  LINK-P1       PIC X(1024).
       01  LINK-P2       PIC X(1024).
       01  LINK-P3       PIC X(1024).
       01  LINK-P4       PIC X(1024).
       01  LINK-P5       PIC X(1024).
       01  LINK-P6       PIC X(1024).
       01  LINK-P7       PIC X(1024).
       01  LINK-P8       PIC X(1024).
       01  LINK-P9       PIC X(1024).
       01  LINK-P10      PIC X(1024).
       01  LINK-P11      PIC X(1024).
       01  LINK-P12      PIC X(1024).
       01  LINK-P13      PIC X(1024).
       01  LINK-P14      PIC X(1024).
       01  LINK-P15      PIC X(1024).
       01  LINK-P16      PIC X(1024).
       01  LINK-P17      PIC X(1024).
       01  LINK-P18      PIC X(1024).
       01  LINK-P19      PIC X(1024).
       01  LINK-P20      PIC X(1024).
       01  LINK-P21      PIC X(1024).
       01  LINK-P22      PIC X(1024).
       01  LINK-P23      PIC X(1024).
       01  LINK-P24      PIC X(1024).
       01  LINK-P25      PIC X(1024).
       01  LINK-P26      PIC X(1024).
       01  LINK-P27      PIC X(1024).
       01  LINK-P28      PIC X(1024).
       01  LINK-P29      PIC X(1024).
       01  LINK-P30      PIC X(1024).
       01  LINK-P31      PIC X(1024).
       01  LINK-P32      PIC X(1024).


       PROCEDURE DIVISION.

       START-PARA.

           CALL "LINKGARG" USING LINK-PATH, ARGCNT,
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26, LINK-P27, LINK-P28,
                               LINK-P29, LINK-P30, LINK-P31, LINK-P32.

      *  The call to ISRUNUSING must be made after the call to LINKGARG

           ACCEPT CMD FROM COMMAND-LINE.
           IF CMD EQUAL "RUNUSING" THEN
               CALL "ISRUNUSING".

       CALL-PROGRAM.

           IF ARGCNT = 0 THEN
           CALL LINK-PATH
           ELSE IF ARGCNT =  1 THEN
           CALL LINK-PATH USING
                               LINK-P1
           ELSE IF ARGCNT =  2 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2
           ELSE IF ARGCNT =  3 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3
           ELSE IF ARGCNT =  4 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4
           ELSE IF ARGCNT =  5 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5
           ELSE IF ARGCNT =  6 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6
           ELSE IF ARGCNT =  7 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7
           ELSE IF ARGCNT =  8 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8
           ELSE IF ARGCNT =  9 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9
           ELSE IF ARGCNT = 10 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10
           ELSE IF ARGCNT = 11 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11
           ELSE IF ARGCNT = 12 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12
           ELSE IF ARGCNT = 13 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13
           ELSE IF ARGCNT = 14 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14
           ELSE IF ARGCNT = 15 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15
           ELSE IF ARGCNT = 16 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16
           ELSE IF ARGCNT = 17 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17
           ELSE IF ARGCNT = 18 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18
           ELSE IF ARGCNT = 19 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19
           ELSE IF ARGCNT = 20 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20
           ELSE IF ARGCNT = 21 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21
           ELSE IF ARGCNT = 22 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22
           ELSE IF ARGCNT = 23 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23
           ELSE IF ARGCNT = 24 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24
           ELSE IF ARGCNT = 25 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25
           ELSE IF ARGCNT = 26 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26
           ELSE IF ARGCNT = 27 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26, LINK-P27
           ELSE IF ARGCNT = 28 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26, LINK-P27, LINK-P28
           ELSE IF ARGCNT = 29 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26, LINK-P27, LINK-P28,
                               LINK-P29
           ELSE IF ARGCNT = 30 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26, LINK-P27, LINK-P28,
                               LINK-P29, LINK-P30
           ELSE IF ARGCNT = 31 THEN
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26, LINK-P27, LINK-P28,
                               LINK-P29, LINK-P30, LINK-P31
           ELSE 
           CALL LINK-PATH USING
                               LINK-P1,  LINK-P2,  LINK-P3,  LINK-P4,
                               LINK-P5,  LINK-P6,  LINK-P7,  LINK-P8,
                               LINK-P9,  LINK-P10, LINK-P11, LINK-P12,
                               LINK-P13, LINK-P14, LINK-P15, LINK-P16,
                               LINK-P17, LINK-P18, LINK-P19, LINK-P20,
                               LINK-P21, LINK-P22, LINK-P23, LINK-P24,
                               LINK-P25, LINK-P26, LINK-P27, LINK-P28,
                               LINK-P29, LINK-P30, LINK-P31, LINK-P32.

       CLEAN-UP.
           CALL "LINKPARG".

       EXIT-PROGRAM.
           EXIT PROGRAM.

       STOP-RUN.
           STOP RUN.

