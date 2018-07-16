       IDENTIFICATION DIVISION.
       PROGRAM-ID. WISPSUB.
       DATE-WRITTEN.   1-MAR-84    FJD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. VAX.
       OBJECT-COMPUTER. VAX.
       DATA DIVISION.

       LINKAGE SECTION.

       01  PROGRAM-NAME PIC X(8).
       01  ITEM-1       PIC X.
       01  ITEM-2       PIC X.
       01  ITEM-3       PIC X.
       01  ITEM-4       PIC X.
       01  ITEM-5       PIC X.
       01  ITEM-6       PIC X.
       01  ITEM-7       PIC X.
       01  ITEM-8       PIC X.
       01  ITEM-9       PIC X.
       01  ITEM-10      PIC X.
       01  ITEM-11      PIC X.
       01  ITEM-12      PIC X.
       01  ITEM-13      PIC X.
       01  ITEM-14      PIC X.
       01  ITEM-15      PIC X.
       01  ITEM-16      PIC X.
       01  ITEM-17      PIC X.
       01  ITEM-18      PIC X.
       01  ITEM-19      PIC X.
       01  ITEM-20      PIC X.
       01  ITEM-21      PIC X.
       01  ITEM-22      PIC X.
       01  ITEM-23      PIC X.
       01  ITEM-24      PIC X.
       01  ITEM-25      PIC X.
       01  ITEM-26      PIC X.
       01  ITEM-27      PIC X.
       01  ITEM-28      PIC X.
       01  ITEM-29      PIC X.
       01  ITEM-30      PIC X.
       01  ITEM-31      PIC X.
       01  ITEM-32      PIC X.

       PROCEDURE DIVISION USING    PROGRAM-NAME,
                                   ITEM-1, ITEM-2, ITEM-3, ITEM-4,
                                   ITEM-5, ITEM-6, ITEM-7, ITEM-8,
                                   ITEM-9, ITEM-10,ITEM-11,ITEM-12,
                                   ITEM-13,ITEM-14,ITEM-15,ITEM-16,
                                   ITEM-17,ITEM-18,ITEM-19,ITEM-20,
                                   ITEM-21,ITEM-22,ITEM-23,ITEM-24,
                                   ITEM-25,ITEM-26,ITEM-27,ITEM-28,
                                   ITEM-29,ITEM-30,ITEM-31,ITEM-32.
       LEVEL-1 SECTION.
       START-PROGRAM.

              CALL PROGRAM-NAME USING ITEM-1, ITEM-2, ITEM-3, ITEM-4,
                                      ITEM-5, ITEM-6, ITEM-7, ITEM-8,
                                      ITEM-9, ITEM-10,ITEM-11,ITEM-12,
                                      ITEM-13,ITEM-14,ITEM-15,ITEM-16,
                                      ITEM-17,ITEM-18,ITEM-19,ITEM-20,
                                      ITEM-21,ITEM-22,ITEM-23,ITEM-24,
                                      ITEM-25,ITEM-26,ITEM-27,ITEM-28,
                                      ITEM-29,ITEM-30,ITEM-31,ITEM-32.

       END-PROGRAM.
           EXIT PROGRAM.
