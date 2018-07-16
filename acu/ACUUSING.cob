000100**   Copyright (c) 1997 NeoMedia Technologies, All rights reserved.
000200**   $Id:$
000300**
000400**   File:       acuusing.cob
000500**
000600**   Project:    WISP for ACUCOBOL
000700**
      ******************************************************************
      *
      *  ACUUSING	- WISP ACUCOBOL RUN USING INTERFACE
      *
      *                   This program is used as the frontend for the
      *                   "wrun program USING args..." command. It must be
      *                   present on the ACUCOBOL path from wherever
      *                   you are doing a wrun-using.
      *
      *                   There is a maximum of 32 parameters that may
      *                   be passed through a wrun-using.
      *
      *                   The maximum size of each parameter may be
      *                   changed if neccesary by changing this 
      *                   program, recompiling it.
      *
      *                   $ ccbl -da4 -o ACUUSING.acu ACUUSING.cob
      *
      ******************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           ACUUSING.
       AUTHOR.
           NeoMedia Technologies Inc.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           UNIX.
       OBJECT-COMPUTER.
           UNIX.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  ARGCNT       PIC 99.

       01  RUN-PRG      PIC X(80).

      *
      * The Maximum length of a passed parameter is determined by the
      * size of the variables below. They are currently set at 1024
      * but they may be increased to any size you prefer.
      *

       01  ARG-P1       PIC X(256).
       01  ARG-P2       PIC X(256).
       01  ARG-P3       PIC X(256).
       01  ARG-P4       PIC X(256).
       01  ARG-P5       PIC X(256).
       01  ARG-P6       PIC X(256).
       01  ARG-P7       PIC X(256).
       01  ARG-P8       PIC X(256).
       01  ARG-P9       PIC X(256).
       01  ARG-P10      PIC X(256).
       01  ARG-P11      PIC X(256).
       01  ARG-P12      PIC X(256).
       01  ARG-P13      PIC X(256).
       01  ARG-P14      PIC X(256).
       01  ARG-P15      PIC X(256).
       01  ARG-P16      PIC X(256).
       01  ARG-P17      PIC X(256).
       01  ARG-P18      PIC X(256).
       01  ARG-P19      PIC X(256).
       01  ARG-P20      PIC X(256).
       01  ARG-P21      PIC X(256).
       01  ARG-P22      PIC X(256).
       01  ARG-P23      PIC X(256).
       01  ARG-P24      PIC X(256).
       01  ARG-P25      PIC X(256).
       01  ARG-P26      PIC X(256).
       01  ARG-P27      PIC X(256).
       01  ARG-P28      PIC X(256).
       01  ARG-P29      PIC X(256).
       01  ARG-P30      PIC X(256).
       01  ARG-P31      PIC X(256).
       01  ARG-P32      PIC X(256).


       PROCEDURE DIVISION CHAINING RUN-PRG, ARGCNT,
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26, ARG-P27, ARG-P28,
                               ARG-P29, ARG-P30, ARG-P31, ARG-P32.

       START-PARA.

           IF ARGCNT = 0 THEN
           CALL RUN-PRG
           ELSE IF ARGCNT =  1 THEN
           CALL RUN-PRG USING
                               ARG-P1
           ELSE IF ARGCNT =  2 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2
           ELSE IF ARGCNT =  3 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3
           ELSE IF ARGCNT =  4 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4
           ELSE IF ARGCNT =  5 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5
           ELSE IF ARGCNT =  6 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6
           ELSE IF ARGCNT =  7 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7
           ELSE IF ARGCNT =  8 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8
           ELSE IF ARGCNT =  9 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9
           ELSE IF ARGCNT = 10 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10
           ELSE IF ARGCNT = 11 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11
           ELSE IF ARGCNT = 12 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12
           ELSE IF ARGCNT = 13 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13
           ELSE IF ARGCNT = 14 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14
           ELSE IF ARGCNT = 15 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15
           ELSE IF ARGCNT = 16 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16
           ELSE IF ARGCNT = 17 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17
           ELSE IF ARGCNT = 18 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18
           ELSE IF ARGCNT = 19 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19
           ELSE IF ARGCNT = 20 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20
           ELSE IF ARGCNT = 21 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21
           ELSE IF ARGCNT = 22 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22
           ELSE IF ARGCNT = 23 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23
           ELSE IF ARGCNT = 24 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24
           ELSE IF ARGCNT = 25 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25
           ELSE IF ARGCNT = 26 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26
           ELSE IF ARGCNT = 27 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26, ARG-P27
           ELSE IF ARGCNT = 28 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26, ARG-P27, ARG-P28
           ELSE IF ARGCNT = 29 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26, ARG-P27, ARG-P28,
                               ARG-P29
           ELSE IF ARGCNT = 30 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26, ARG-P27, ARG-P28,
                               ARG-P29, ARG-P30
           ELSE IF ARGCNT = 31 THEN
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26, ARG-P27, ARG-P28,
                               ARG-P29, ARG-P30, ARG-P31
           ELSE 
           CALL RUN-PRG USING
                               ARG-P1,  ARG-P2,  ARG-P3,  ARG-P4,
                               ARG-P5,  ARG-P6,  ARG-P7,  ARG-P8,
                               ARG-P9,  ARG-P10, ARG-P11, ARG-P12,
                               ARG-P13, ARG-P14, ARG-P15, ARG-P16,
                               ARG-P17, ARG-P18, ARG-P19, ARG-P20,
                               ARG-P21, ARG-P22, ARG-P23, ARG-P24,
                               ARG-P25, ARG-P26, ARG-P27, ARG-P28,
                               ARG-P29, ARG-P30, ARG-P31, ARG-P32.


       EXIT-PROGRAM.
           EXIT PROGRAM.

       STOP-RUN.
           STOP RUN.



