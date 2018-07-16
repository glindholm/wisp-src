
      /*****************************************************************
      *                       IDENTIFICATION DIVISION                  *
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           WISPRUN.
       AUTHOR.
           DEV BRADLEY.
       INSTALLATION.
           INTERNATIONAL DIGITAL SCIENTIFIC INCORPORATED.
           25050 AVENUE KEARNY, SUITE 203.
           VALENCIA, CALIFORNIA 91355.
           (805) 295-1155 [OFFICE].
           (805) 295-8755 [FAX].
           COPYRIGHT 1991, ALL RIGHTS RESERVED.
       DATE-WRITTEN.
           07/18/91.
       DATE-COMPILED.
      /*****************************************************************
      *                         ENVIRONMENT DIVISION                   *
      ******************************************************************
      *
       ENVIRONMENT DIVISION.

      /*****************************************************************
      *                         DATA DIVISION                          *
      ******************************************************************
      *
       DATA DIVISION.

      /*****************************************************************
      *                         WORKING STORAGE                        *
      ******************************************************************
      *
       WORKING-STORAGE SECTION.

       01  COM-LINE                       PIC X(80).
       01  IN-LINE                        PIC X(80).

      /*****************************************************************
      *                         PROCEDURE DIVISION                     *
      ******************************************************************
      *
       PROCEDURE DIVISION.

       MAIN SECTION.

       START-WISP-RUN-TIME.

           DISPLAY "Loading Run Time System...".

           CALL "WISPRTS.EXE".

           ACCEPT COM-LINE FROM COMMAND-LINE.

           DISPLAY "Run Time Loaded.".

           DISPLAY "Starting routine ", COM-LINE.
           CALL COM-LINE.

      ******************************************************************
      * ALL DONE.
      ******************************************************************

       EXIT-PROGRAM.
           EXIT PROGRAM.

       STOP-RUN.
           STOP RUN.

