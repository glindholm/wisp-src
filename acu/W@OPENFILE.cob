000100**   Copyright (c) Shell Stream Software LLC, All rights reserved.
000200**
000300**
000400**   File:       W@OPENFILE.cob
000500**
000600**   Project:    WISP Acucobol 
000700**
000800**   Purpose:    Prepare for OPEN file statement
000900**
001000**   Build:      ccbl W@OPENFILE.cob
001100**
001200**
001300
001400 IDENTIFICATION DIVISION.
001500 PROGRAM-ID. "W@OPENFILE".
001700
001800 ENVIRONMENT DIVISION.
001900 CONFIGURATION SECTION.
002500
002600 DATA DIVISION.
002700
002800 WORKING-STORAGE SECTION.
002900
006200
006300 LINKAGE SECTION.
006700 01  FILE-ATTRIBUTES         PIC X(10).
006800 01  FILE-VOL                PIC X(6).
006800 01  FILE-LIB                PIC X(8).
006800 01  FILE-FILE               PIC X(8).
       01  FILE-PATH               PIC X(80).
       01  APP-NAME                PIC X(40).
       01  PRNAME                  PIC X(8).
       01  OPEN-MODE     COMP-4    PIC 9(9).
006900
008200 PROCEDURE DIVISION USING
               FILE-ATTRIBUTES
               FILE-VOL
               FILE-LIB
               FILE-FILE
               FILE-PATH
               APP-NAME
               PRNAME
               OPEN-MODE.
               
008300 MAIN SECTION.                          
008400
           CALL "WFOPEN4" USING
               FILE-ATTRIBUTES
               FILE-VOL
               FILE-LIB
               FILE-FILE
               FILE-PATH
               APP-NAME
               PRNAME
               OPEN-MODE.
           
009900
010000 9999-EXIT.
010100     EXIT PROGRAM.
010200
