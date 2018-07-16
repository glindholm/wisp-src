       01  CMD-LINE.
           05  CMD-ARG            PIC S9(4) COMP.
           05  CMD-VALUE.
               10  CMD-BYTE PIC X OCCURS 0 TO 50
                         DEPENDING ON CMD-ARG.
