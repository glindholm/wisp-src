      ****************************************************************
      * WCLINK -- A PROGRAM TO INTERPRET CALLS TO THE "LINK" ROUTINE *
      *           EMULATING A WANG ON A MSDOS SYSTEM USING MICRO     *
      *           FOCUS COBOL.                                       *
      *           THIS PROGRAM IS CALLED BY THE 'C' ROUTINE 'LINK'   *
      *           AND IT TRYS TO CALL THE PROGRAM NAMED IN THE       *
      *           PROGRAM-NAME FIELD.                                *
      ****************************************************************
      $SET OBJ "WCLINK1"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "_wclink1".
       COPY "wclinkx.cob".
       END PROGRAM "_wclink1".
