       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTIDY.
      
      ****************************************************************
      * COBTIDY - Needed by Micro Focus COBOL/2 to exit the run-time *
      *           system.  A STOP RUN will properly close files.     *
      ****************************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  IN-LINE                      PIC X(80).

       PROCEDURE DIVISION. 
       
       MAIN-COBTIDY SECTION.

           ENTRY "_cobtidy".

           STOP RUN.
