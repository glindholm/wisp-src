/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/* PGCBLSRC.H															*/
/*		 COBOL source that needs to be inserted into the .WCB that is generated.					*/

#ifdef INIT_COMMON

EXT char *ident_div[] = {
			"000100/*****************************************************************\n",        
			"000200*                                                                *\n",        
			"000300*                       IDENTIFICATION DIVISION                  *\n",        
			"000400*                                                                *\n",        
			"000500******************************************************************\n",        
			"000600*\n",                                                                         
			"000700 IDENTIFICATION DIVISION.\n",                                                 
			"000800 PROGRAM-ID.\n",
			"001000 AUTHOR.\n",
			"001100     PROCTRAN Wang VS Procedure Translator.\n",
			"001200 DATE-WRITTEN.\n",                                                            
			""
			};

EXT char *env_div[] = {
			"001400/*****************************************************************\n",        
			"001500*                                                                *\n",        
			"001600*                         ENVIRONMENT DIVISION                   *\n",        
			"001700*                                                                *\n",
			"001800******************************************************************\n",        
			"001900*\n",
			"002000 ENVIRONMENT DIVISION.\n",
			"002100 CONFIGURATION SECTION.\n",                                                   
			"002200 SOURCE-COMPUTER.\n",                                                         
			"002300     WANG-VS.\n",
			"002400 OBJECT-COMPUTER.\n",                                                         
			"002500     WANG-VS.\n",
			"002600 FIGURATIVE-CONSTANTS.\n",                                                    
			"002700     BRMOD       IS \"80\"\n",
			"002800     BRMODUL     IS \"A0\"\n",                                                  
			"002900     BRMODUPR    IS \"81\"\n",                                                  
			"003000     BRMODUPRUL  IS \"A1\"\n",                                                  
			"003100     BRMODNUM    IS \"82\"\n",                                                  
			"003200     BRMODNUMUL  IS \"A2\"\n",                                                  
			"003300     BRPRO       IS \"84\"\n",                                                  
			"003400     BRPROUL     IS \"A4\"\n",                                                  
			"003500     BRPROUPR    IS \"85\"\n",                                                  
			"003600     BRPROUPRUL  IS \"A5\"\n",                                                  
			"003700     BRPRONUM    IS \"86\"\n",                                                  
			"003800     BRPRONUMUL  IS \"A6\"\n",                                                  
			"003900     DIMMOD      IS \"88\"\n",                                                  
			"004000     DIMMODUL    IS \"A8\"\n",                                                  
			"004100     DIMMODUPR   IS \"89\"\n",                                                  
			"004200     DIMMODUPRUL IS \"A9\"\n",                                                  
			"004300     DIMMODNUM   IS \"8A\"\n",                                                  
			"004400     DIMMODNUMUL IS \"AA\"\n",                                                  
			"004500     DIMPRO      IS \"8C\"\n",                                                  
			"004600     DIMPROUL    IS \"AC\"\n",                                                  
			"004700     DIMPROUPR   IS \"8D\"\n",                                                  
			"004800     DIMPROUPRUL IS \"AD\"\n",                                                  
			"004900     DIMPRONUM   IS \"8E\"\n",                                                  
			"005000     DIMPRONUMUL IS \"AE\"\n",                                                  
			"005100     BLIMOD      IS \"90\"\n",                                                  
			"005200     BLIMODUL    IS \"B0\"\n",                                                  
			"005300     BLIMODUPR   IS \"91\"\n",                                                  
			"005400     BLIMODUPRUL IS \"B1\"\n",                                                  
			"005500     BLIMODNUM   IS \"92\"\n",                                                  
			"005600     BLIMODNUMUL IS \"B2\"\n",                                                  
			"005700     BLIPRO      IS \"94\"\n",                                                  
			"005800     BLIPROUL    IS \"B4\"\n",                                                  
			"005900     BLIPROUPR   IS \"95\"\n",                                                  
			"006000     BLIPROUPRUL IS \"B5\"\n",                                                  
			"006100     BLIPRONUM   IS \"96\"\n",                                                  
			"006200     BLIPRONUMUL IS \"B6\"\n",                                                  
			"006300     BLAMOD      IS \"98\"\n",                                                  
			"006400     BLAMODUL    IS \"B8\"\n",                                                  
			"006500     BLAMODUPR   IS \"99\"\n",                                                  
			"006600     BLAMODUPRUL IS \"B9\"\n",                                                  
			"006700     BLAMODNUM   IS \"9A\"\n",                                                  
			"006800     BLAMODNUMUL IS \"BA\"\n",                                                  
			"006900     BLAPRO      IS \"9C\"\n",                                                  
			"007000     BLAPROUL    IS \"BC\"\n",                                                  
			"007100     BLAPROUPR   IS \"9D\"\n",                                                  
			"007200     BLAPROUPRUL IS \"BD\"\n",                                                  
			"007300     BLAPRONUM   IS \"9E\"\n",                                                  
			"007400     BLAPRONUMUL IS \"BE\"\n",                                                  
			"007500     SOLIDS     IS \"0B\"\n",                                                  
			"007600     POS-CURSOR IS \"A0\"\n",                            /* Unlock keyboard and position cursor.	*/
			"007700     SOUNDALARM IS \"C0\"\n",   				/* Unlock keyboard and sound alarm.	*/
			"007700     ALARMPOSN  IS \"E0\"\n",   				/* Unlock keybrd, sound alarm, posn cur.*/
			"007705     KEYBRDLOCK IS \"00\"\n",                            /* Lock the keyboard.			*/
			"007706     LOCKALARM  IS \"40\"\n",				/* Lock keyboard and sound alarm.	*/
			"007710     ONE        IS \"01\"\n",                                                  
			"007710     NINE       IS \"09\"\n",                                                  
			"007710     FOURTYFOUR IS \"2C\"\n",                                                  
			"007710     FOURTEEN   IS \"0E\"\n",                                                  
			"007710     SIXTYSEVEN IS \"43\"\n",                                                  
			"007711     EIGHTY     IS \"50\"\n",                                                  
			"007712     HEX-80     IS \"80\"\n",        
			"007712     HEX-00     IS \"00\"\n",        
			"007712     HEX-01     IS \"01\"\n",        
			"007713     HEX-21     IS \"21\"\n",
			"007720     TWENTYFOUR IS \"18\".\n",
			""
		};

EXT char *i_o_div[] = {
			"007800/*****************************************************************\n",
			"007900*                                                                *\n",
			"008000*                         INPUT-OUTPUT SECTION                   *\n",
			"008100*                                                                *\n",
			"008200******************************************************************\n",
			"008300*\n",
			"008400 INPUT-OUTPUT SECTION.\n",
			"008500 FILE-CONTROL.\n",
			"008600*\n",
			"008700     SELECT CRT\n",
			"008800         ASSIGN TO \"CRT\",      \"DISPLAY\",\n",
			"008900         ORGANIZATION    IS SEQUENTIAL\n",
			"009000         ACCESS MODE     IS RANDOM\n",
			"009100         RELATIVE KEY    IS LINENUM\n",
			"009200         PFKEY           IS PF-KEY\n",
			"009300         CURSOR POSITION IS CURSOR-POS\n",
			"009400         FILE STATUS     IS FILSTAT.\n",
			""
		};

EXT char *data_div[] = {
			"010200/*****************************************************************\n",
			"010300*                                                                *\n",
			"010400*                      DATA DIVISION - FILE SECTION              *\n",
			"010500*                                                                *\n",
			"010600******************************************************************\n",
			"010700*\n",
			"010800 DATA DIVISION.\n",
			"010900 FILE SECTION.\n",
			"011000*\n",
			"011100 FD  CRT\n",
			"011200     LABEL RECORDS ARE OMITTED.\n",
			"011300 01  CRTREC.\n",
			"011310     03  CRTREC-OA                  PIC X(4).\n",
			"011320     03  CRTREC-MAPPING-AREA        PIC X(1920).\n",
			"011400*\n",
			""
		}; 

EXT char *work_stg_sec[] = {
			"011900/*****************************************************************\n",
			"012000*                                                                *\n",
			"012100*                         WORKING STORAGE                        *\n",
			"012200*                                                                *\n",
			"012300******************************************************************\n",
			"012400*\n",
			"012500 WORKING-STORAGE SECTION.\n",
			"013800******************************************************************\n",
			"013900*                   FILE CONTROL STATUS SWITCHES                 *\n",
			"014000******************************************************************\n",
			"014010 01  ORDERAREA.\n",
			"014020     03  ROW-NUMBER                 PIC X(1).\n",
			"014030     03  WRITE-CONTROL              PIC X(1).\n",
			"014040     03  CURSOR-COLUMN              PIC X(1).\n",
			"014050     03  CURSOR-ROW                 PIC X(1).\n",
			"014055\n",                                                                          
			"014100 01  FILSTAT.\n",
			"014200     03  ERR-FLAG                   PIC X(1).\n",
			"014300     03  PFK-BYTE                   PIC X(1).\n",
			"015000 01  ERROR-RC                       PIC 9(2).\n",
			"015100 01  ERROR-DISP                     PIC X(40).\n",
			"017600 01  DEL-BLANK                      PIC X(1) VALUE SPACE.\n",
			"016000 77  LINENUM                        PIC 9(2) VALUE 1.\n",
			"016100 01  PF-KEY                         PIC 9(2) VALUE 1.\n",
			"016200 01  CURSOR-POS.\n",
			"016300     03  MOD-COL                    BINARY.\n",
			"016400     03  MOD-ROW                    BINARY.\n",
			"016500 01  REDEF-CURS-POS                 REDEFINES CURSOR-POS.\n",
			"016600     03  FILLER                     PIC X(1).\n",
			"016700     03  CURS-COL                   PIC X(1).\n",
			"016800     03  FILLER                     PIC X(1).\n",
			"016900     03  CURS-ROW                   PIC X(1).\n",
			"016910 01  WSA-SCREEN-TOP.\n",
			"016920     03  WSA-LINE-AREA OCCURS 24 TIMES.\n",
			"016930         05  WSA-80-LINE            PIC X(80) VALUE SPACES.\n",
			"016940 01  WSA-KEYWORD                    PIC X(2) VALUE SPACES.\n",
			"016950 01  WSA-VARIABLE                   PIC X(24) VALUE SPACES.\n",
			"016960 01  WSA-NUMERAL.\n",
			"016300     03  WSA-NUM-1                  BINARY VALUE 0.\n",
			"016400     03  WSA-NUM-2                  BINARY VALUE 0.\n",
			"028100 01  WSA-HOLD-BINARY.\n",
			"028200     03  B-1                        BINARY    VALUE 0.\n",
			"028300     03  B-2                        BINARY    VALUE 0.\n",
			"028370 01  WSA-FILE-NAME                  PIC X(8)  VALUE SPACES.\n",
			"028380 01  WSA-LIB-NAME                   PIC X(8)  VALUE SPACES.\n",
			"028390 01  WSA-VOL-NAME                   PIC X(6)  VALUE SPACES.\n",
			"028400 01  WSA-DATE-AREA.\n",
			"028500     05  WSA-NDATE                      PIC 9(6).\n",
			"028600     05  WSA-ADATE REDEFINES WSA-NDATE  PIC X(6).\n",
			"028400 01  WSA-TIME-AREA.\n",
			"028500     05  WSA-NTIME                      PIC 9(8).\n",
			"028600     05  WSA-ATIME REDEFINES WSA-NTIME  PIC X(8).\n",
			"028650\n",
			""
			};

EXT char *displ_def[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   DISPLAY DEFINITIONS                          *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  DISPLAY-REC USAGE IS DISPLAY-WS.\n",
			"           05  ROW1-COL1  OCCURS 24 TIMES.\n",
			"               10  FILLER    PIC X(80)    ROW 01 COLUMN 01\n",
			"                   SOURCE WSA-LINE-AREA OBJECT WSA-LINE-AREA.\n",
			"017700*\n",
			""
		};           

EXT char *cobol_rename[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   RENAME SUBROUTINE DEFINITIONS                *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  RENAME-TYPE                    PIC X VALUE SPACES.\n",
			"017600 01  RENAME-FILE                    PIC X(8) VALUE SPACES.\n",
			"017600 01  RENAME-LIB                     PIC X(8) VALUE SPACES.\n",
			"017600 01  RENAME-BYPASS-FLAG             PIC X VALUE \"B\".\n",
			"017600 01  RENAME-ACCESS-FLAG             PIC X VALUE SPACES.\n",
			"017600 01  RENAME-OPEN                    PIC X VALUE \"O\".\n",
			"017600 01  RENAME-RETURN-CODE.\n",
			"017600     05  RENAME-R-1                 BINARY VALUE 0.\n",
			"017600     05  RENAME-R-2                 BINARY VALUE 0.\n",
			""
			};
           
EXT char *cobol_scratch[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   SCRATCH SUBROUTINE DEFINITIONS               *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  SCRATCH-TYPE                   PIC X VALUE SPACES.\n",
			"017600 01  SCRATCH-BYPASS-FLAG            PIC X VALUE \"B\".\n",
			"017600 01  SCRATCH-ACCESS-FLAG            PIC X VALUE SPACES.\n",
			"017600 01  SCRATCH-RETURN-CODE.\n",
			"017600     05  SCRATCH-R-1                BINARY VALUE 0.\n",
			"017600     05  SCRATCH-R-2                BINARY VALUE 0.\n",
			""
			};
           
EXT char *cobol_print[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   PRINT SUBROUTINE DEFINITIONS                  *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  PRINT-MODE                     PIC X VALUE \"S\".\n",
			"017600 01  PRINT-DISPOSITION              PIC X(2) VALUE \"DS\".\n",
			"017600 01  PRINT-NUMBER-COPIES.\n",
			"017600     05  PRINT-N-1                  BINARY VALUE 0.\n",
			"017600     05  PRINT-N-2                  BINARY VALUE 1.\n",
			"017600 01  PRINT-CLASS                    PIC X VALUE SPACES.\n",
			"017600 01  PRINT-FORM-NUMBER.\n",
			"017600     05  PRINT-F-1                  BINARY VALUE 0.\n",
			"017600     05  PRINT-F-2                  BINARY VALUE 0.\n",
			"017600 01  PRINT-RETURN-CODE.\n",
			"017600     05  PRINT-R-1                  BINARY VALUE 0.\n",
			"017600     05  PRINT-R-2                  BINARY VALUE 0.\n",
			""
			};           

EXT char *cobol_submit[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   SUBMIT SUBROUTINE DEFINITIONS                 *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  SUBMIT-JOB-NAME                PIC X(8) VALUE SPACES.\n",
			"017600 01  SUBMIT-STATUS                  PIC X(1) VALUE SPACES.\n",
			"017600 01  SUBMIT-DISPOSITION             PIC X(1) VALUE \"D\".\n",
			"017600 01  SUBMIT-JOB-CLASS               PIC X(1) VALUE SPACES.\n",
			"017600 01  SUBMIT-ABORT-ACTION            PIC X(1) VALUE \"R\".\n",
			"017600 01  SUBMIT-CPU-TIME.\n",
			"017600     05  SUBMIT-CPU-1               BINARY VALUE 0.\n",
			"017600     05  SUBMIT-CPU-2               BINARY VALUE 0.\n",
			"017600 01  SUBMIT-LIMIT-FLAG              PIC X VALUE \"W\".\n",
			"017600 01  SUBMIT-RETURN-CODE.\n",
			"017600     05  SUBMIT-R-1                 BINARY VALUE 0.\n",
			"017600     05  SUBMIT-R-2                 BINARY VALUE 0.\n",
			""
			};

EXT char *cobol_putparm[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   PUTPARM SUBROUTINE DEFINITIONS                *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  PPARM-FUNC                     PIC X    VALUE SPACES.\n",
			"017600 01  PPARM-PRNAME                   PIC X(8) VALUE SPACES.\n",
			"017600 01  PPARM-KW-COUNT.\n",
			"017600     05  FILLER                     BINARY   VALUE 0.\n",
			"017600     05  KW-CNT                     BINARY   VALUE 0.\n",
			"017600 01  PPARM-PFKEY                    PIC X    VALUE \"@\".\n",
			"017600 01  PPARM-LABEL                    PIC X(8) VALUE SPACES.\n",
			"017600 01  PPARM-RETURN-CODE.\n",
			"017600     03  PPARM-R-1                  BINARY    VALUE ZERO.\n",
			"017600     03  PPARM-R-2                  BINARY    VALUE ZERO.\n",                 
			"017600 01  FILLER.\n",
			"017600     03  PPARM-PFKEY-ELEMENT.\n",
			"017600         05 FILLER                  PIC X     VALUE \"@\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"A\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"B\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"C\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"D\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"E\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"F\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"G\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"H\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"I\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"J\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"K\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"L\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"M\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"N\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"O\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"P\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"a\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"b\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"c\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"d\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"e\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"f\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"g\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"h\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"i\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"j\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"k\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"l\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"m\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"n\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"o\".\n",
			"017600         05 FILLER                  PIC X     VALUE \"p\".\n",
			"017600     03  PPARM-PFKEY-TABLE REDEFINES PPARM-PFKEY-ELEMENT\n",
			"017600                 OCCURS 33 TIMES    PIC X.\n",
			""
			};

EXT char *cobol_readfdr[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   READFDR SUBROUTINE DEFINITIONS               *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  READFDR-FILE                   PIC X(8) VALUE SPACES.\n",
			"017600 01  READFDR-LIB                    PIC X(8) VALUE SPACES.\n",
			"017600 01  READFDR-VOL                    PIC X(6) VALUE SPACES.\n",
			"017600 01  READFDR-MODE.\n",
			"017600     05  RFDR-MODE-R-1              BINARY   VALUE 0.\n",
			"017600     05  RFDR-MODE-R-2              BINARY   VALUE 0.\n",
			"017600 01  READFDR-FLD-ID                 PIC X(2) VALUE SPACES.\n",
			"017600 01  READFDR-RECVR.\n",
			"017600     05  RFDR-RECVR-R-1             BINARY VALUE 0.\n",
			"017600     05  RFDR-RECVR-R-2             BINARY VALUE 0.\n",
			"017600 01  READFDR-RETURN-CODE.\n",
			"017600     05  RFDR-RC-R-1                BINARY VALUE 0.\n",
			"017600     05  RFDR-RC-R-2                BINARY VALUE 0.\n",
			""
			};

EXT char *link_parms[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   CALL LINK PARAMATERS                         *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"026600 01  LINK-TO-NAME                   PIC X(08) VALUE SPACES.\n",
			"026700 01  LINK-LIBRARY                   PIC X(08) VALUE SPACES.\n",
			"026800 01  LINK-VOLUME                    PIC X(06) VALUE SPACES.\n",
			"026900 01  LINK-TYPE                      PIC X(01) VALUE SPACE.\n",
			"027000 01  LINK-PCOUNT.\n",
			"027100     03  FILLER                     BINARY    VALUE ZERO.\n",
			"027200     03  LINK-PCOUNT-NO             BINARY    VALUE ZERO.\n",
			"027300 01  LINK-CEXIT-FLAG                PIC X(01) VALUE \" \".\n",
			"027400 01  LINK-CEXIT-MSG                 PIC X(27) VALUE SPACES.\n",
			"027500 01  LINK-CEXIT-MSG-LEN.\n",
			"027600     03  FILLER                     BINARY    VALUE ZERO.\n",
			"027700     03  FILLER                     BINARY    VALUE ZERO.\n",
			"027800 01  LINK-HELP-FLAG                 PIC X(1)  VALUE SPACE.\n",
			"027900 01  LINK-PFKEY-MASK                PIC X(02) VALUE LOW-VALUES.\n",
			"028000 01  LINK-CANCEL-RCVR               PIC X(128) VALUE SPACES.\n",
			"028100 01  LINK-CANCEL-RCVR-LEN.\n",
			"028200     03  FILLER                     BINARY    VALUE ZERO.\n",
			"028300     03  FILLER                     BINARY    VALUE 128.\n",
			"028400 01  LINK-CODE.\n",                                                           
			"028500     03  FILLER                     BINARY    VALUE ZERO.\n",
			"028600     03  LINK-CODE-VAL              BINARY    VALUE ZERO.\n",
			"028700 01  LINK-RETURN-CODE.\n",
			"028800     03  LINK-R-1                   BINARY    VALUE ZERO.\n",
			"028900     03  LINK-R-2                   BINARY    VALUE ZERO.\n",                 
			""
			};

EXT char *cobol_find[] = {
			"028400/*****************************************************************\n",
			"028450*                                                                *\n",
			"028500*                   FIND SUBROUTINE DEFINITIONS                  *\n",
			"028550*                                                                *\n",
			"028600******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"028700 01  STARTER.\n",
			"028800     05  FILLER  USAGE IS           BINARY VALUE 0.\n",
			"028900     05  START-INTEGER USAGE IS     BINARY VALUE 1.\n",
			"029000 01  COUNTER.\n",
			"029100     05  FILLER  USAGE IS           BINARY VALUE 0.\n",
			"029200     05  COUNT-INTEGER USAGE IS     BINARY VALUE 1.\n",
			"029300 01  RECEIVER                       PIC X(8800) VALUE SPACES.\n",
			"029400 01  REC-AREA.\n",
			"029500     05  REC-FILL OCCURS 400 TIMES INDEXED BY IND-001.\n",
			"029600         10  FILE-VOLUME            PIC X(6).\n",
			"029700         10  FILE-LIBRARY           PIC X(8).\n",
			"029800         10  FILE-NAME              PIC X(8).\n",
			"030200 \n",
			"030300 01  ENTRIES.\n",
			"030400     05  FILLER        USAGE IS     BINARY VALUE 0.\n",
			"030500     05  ENTRY-COUNT   USAGE IS     BINARY VALUE 0.\n",
			"030600 \n",
			""
			};

EXT char *set_extct[] = {
			"035600******************************************************************\n",
			"035800* Call Set and Extract variables for Keywords.\n",
			"035891******************************************************************\n",
			"035300 \n",
			"030301 01  FC                             PIC X(2) VALUE \"FC\".\n",
			"030302 01  FH                             PIC X(2) VALUE \"FH\".\n",
			"030302 01  FN                             PIC X(2) VALUE \"FN\".\n",
			"030303 01  ID                             PIC X(2) VALUE \"ID\".\n",
			"030304 01  IL                             PIC X(2) VALUE \"IL\".\n",
			"030305 01  IV                             PIC X(2) VALUE \"IV\".\n",
			"030306 01  JC                             PIC X(2) VALUE \"JC\".\n",
			"030307 01  JL                             PIC X(2) VALUE \"JL\".\n",
			"030308 01  JQ                             PIC X(2) VALUE \"JQ\".\n",
			"030308 01  JS                             PIC X(2) VALUE \"JS\".\n",
			"030309 01  LI                             PIC X(2) VALUE \"LI\".\n",
			"030310 01  NA                             PIC X(2) VALUE \"NA\".\n",
			"030311 01  OL                             PIC X(2) VALUE \"OL\".\n",
			"030312 01  OV                             PIC X(2) VALUE \"OV\".\n",
			"030313 01  PC                             PIC X(2) VALUE \"PC\".\n",
			"030314 01  PK                             PIC X(2) VALUE \"PF\".\n",
			"030314 01  PL                             PIC X(2) VALUE \"PL\".\n",
			"030315 01  PM                             PIC X(2) VALUE \"PM\".\n",
			"030316 01  PN                             PIC X(2) VALUE \"P#\".\n",
			"030317 01  PR                             PIC X(2) VALUE \"PR\".\n",
			"030318 01  PV                             PIC X(2) VALUE \"PV\".\n",
			"030319 01  RC                             PIC X(2) VALUE \"RC\".\n",
			"030320 01  RL                             PIC X(2) VALUE \"RL\".\n",
			"030321 01  RR                             PIC X(2) VALUE \"RR\".\n",
			"030321 01  RS                             PIC X(2) VALUE \"RS\".\n",
			"030321 01  RV                             PIC X(2) VALUE \"RV\".\n",
			"030322 01  SL                             PIC X(2) VALUE \"SL\".\n",
			"030323 01  SV                             PIC X(2) VALUE \"SV\".\n",
			"030323 01  XL                             PIC X(2) VALUE \"XL\".\n",
			"030323 01  XV                             PIC X(2) VALUE \"XV\".\n",
			"030324 01  TT                             PIC X(2) VALUE \"TT\".\n",
			"030325 01  WN                             PIC X(2) VALUE \"W#\".\n",
			"030326 01  WV                             PIC X(2) VALUE \"WV\".\n",
			""
		};

EXT char *cobol_string[] = {
			"017000/*****************************************************************\n",
			"017100*                                                                *\n",
			"017200*                   STRING SUBROUTINE DEFINITIONS                *\n",
			"017300*                                                                *\n",
			"017400******************************************************************\n",
			"*23456*89012345678901234567890123456789012345678901234567890123456789012\n",
			"017600 01  STR-FUNC                       PIC X(2) VALUE \"MI\".\n",
			"017600 01  STR-INPUT-INDEX.\n",
			"017600     05  STR-IN-INDEX-R-1           BINARY VALUE 0.\n",
			"017600     05  STR-IN-INDEX-R-2           BINARY VALUE 0.\n",
			"017600 01  STR-INPUT-LENGTH.\n",
			"017600     05  STR-IN-LEN-R-1             BINARY VALUE 0.\n",
			"017600     05  STR-IN-LEN-R-2             BINARY VALUE 0.\n",
			"017600 01  STR-OUTPUT-INDEX.\n",
			"017600     05  STR-OUT-INDEX-R-1          BINARY VALUE 0.\n",
			"017600     05  STR-OUT-INDEX-R-2          BINARY VALUE 0.\n",
			"017600 01  STR-OUTPUT-LENGTH.\n",
			"017600     05  STR-OUT-LEN-R-1            BINARY VALUE 0.\n",
			"017600     05  STR-OUT-LEN-R-2            BINARY VALUE 0.\n",
			""
			};

EXT char *link_sec[] = {
			"035600******************************************************************\n",
			"035700*\n",                                                                         
			"035800 LINKAGE SECTION.\n",
			"035891******************************************************************\n",
			""
		};

EXT char *proc_div[] = {
			"035600******************************************************************\n",
			"035800 PROCEDURE DIVISION.\n",
/*2*/			"035891******************************************************************\n",
			"035900 START-PROGRAM.             \n",
			"036000     PERFORM INITIALIZATION.\n",
			"036100     PERFORM MAIN-PROCESS THRU MAIN-EXIT.\n",
			"036300\n",
			"036400 EXIT-PROGRAM.\n",
			"            STOP RUN.\n",
			"036412\n",
			"",
/*11*/			"035800 PROCEDURE DIVISION USING \n",
		};

EXT char *init_src[] = {
			"036700******************************************************************\n",
			"036800*                         INITIALIZATION                         *\n",        
			"036900******************************************************************\n",
			"037000*\n",
			"037100 INITIALIZATION. \n",
			"037200     ACCEPT WSA-NTIME FROM TIME. \n",
			"037200     ACCEPT WSA-NDATE FROM DATE. \n",
			"037800\n",
			"",
		};

EXT char *main_process[] = {
			"037900******************************************************************\n",
			"038000*                         MAIN PROCESS                           *\n",
			"038100******************************************************************\n",
			"038200*\n",
			"038300 MAIN-PROCESS.\n",
			""
			};

EXT char *main_exit[] = {
			"038300 MAIN-EXIT.\n",
			"038300       EXIT.\n",
			""
		};

EXT char *disp_read[] = {
			"037900******************************************************************\n",
			"038000*                         DISPLAY AND READ                       *\n",
			"038100******************************************************************\n",
			"038300 A-DISPLAY-AND-READ.\n",		
			"038400     MOVE ORDERAREA    TO ORDER-AREA OF DISPLAY-REC.\n",
			"038800     DISPLAY AND READ DISPLAY-REC  ON CRT\n",
			"049100           PFKEY 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16\n",
			"049150           17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32.\n",
			"009150*\n",
			""
		};

EXT char *call_set[] = {
			"037900******************************************************************\n",
			"038000*                         CALL SET                               *\n",
			"038100******************************************************************\n",
			"038200 SET-VARIABLE.  \n",
			"038300     CALL \"SET\" USING WSA-KEYWORD WSA-VARIABLE.\n",
			"038400 SET-NUMERAL.  \n",
			"038500     CALL \"SET\" USING WSA-KEYWORD WSA-NUMERAL.\n",
			""
		};

EXT char *call_extract[] = {
			"037900******************************************************************\n",
			"038000*                         CALL EXTRACT                           *\n",
			"038100******************************************************************\n",
			"038200 EXTRACT-VARIABLE.  \n",
			"038300     CALL \"EXTRACT\" USING WSA-KEYWORD WSA-VARIABLE.\n",
			"038400 EXTRACT-NUMERAL.  \n",
			"038500     CALL \"EXTRACT\" USING WSA-KEYWORD WSA-NUMERAL.\n",
			""
			};

EXT char *call_link[] = {
			"MOVE \"C\" TO LINK-CEXIT-FLAG",
			"MOVE \"RETURN TO PROGRAM          \" TO LINK-CEXIT-MSG",
			"MOVE \"H\" TO LINK-HELP-FLAG",
/*3*/			"CALL \"LINK\" USING LINK-TO-NAME,",
			"                  LINK-TYPE, LINK-LIBRARY, LINK-VOLUME,",
			"                  LINK-PCOUNT,",
/*6*/			"                  LINK-CODE, LINK-RETURN-CODE",
			"MOVE LINK-R-2 TO RETURN-CODE",
/*8*/			"IF LINK-CODE-VAL = 8",
/*9*/			"    MOVE LINK-R-2 TO ERROR-RC",                            
/*10*/			"    DISPLAY \"ERROR CALL LINK TO PROGRAM \", LINK-TO-NAME,",
/*11*/			"            \" RETURN CODE = \", ERROR-RC",
			""
		};

EXT char *call_rename[] = {
			"037900******************************************************************\n",
			"038000*                         PERFORM CALL RENAME                    *\n",
			"038100******************************************************************\n",
			"039300 RENAME-FILE-SUB.\n",
			"053500     CALL \"RENAME\" USING RENAME-TYPE, WSA-FILE-NAME,\n",
			"053550                       WSA-LIB-NAME, WSA-VOL-NAME,\n",
			"053600                       RENAME-FILE, RENAME-LIB,\n",
			"053700                       RENAME-BYPASS-FLAG, RENAME-ACCESS-FLAG,\n",
			"053950                       RENAME-OPEN, RENAME-RETURN-CODE.\n",
			"",
/*9*/			"053950*\n",
			"054000     IF RENAME-R-2 IS GREATER THAN ZERO\n",                            
			"054005        MOVE RENAME-R-2 TO ERROR-RC.\n",
			"054100        DISPLAY \"ERROR CALL RENAME \", WSA-FILE-NAME,\n",
			"054105                \" RETURN CODE = \", ERROR-RC.\n",
			""
			};

EXT char *call_logoff[] = {
			"037900******************************************************************\n",
			"038000*                         PERFORM CALL LOGOFF                    *\n",
			"038100******************************************************************\n",
			"039300 LOGOFF-USER.\n",
			"053500     CALL \"LOGOFF\".\n",
			""
			};

EXT char *call_scratch[] = {
			"037900******************************************************************\n",
			"038000*                         PERFORM CALL SCRATCH                   *\n",
			"038100******************************************************************\n",
			"039300 SCRATCH-FILE-SUB.\n",
			"053500     CALL \"SCRATCH\" USING SCRATCH-TYPE, WSA-FILE-NAME,\n",
			"053550                       WSA-LIB-NAME, WSA-VOL-NAME,\n",
			"053600                       SCRATCH-BYPASS-FLAG,\n",
			"053950                       SCRATCH-ACCESS-FLAG, SCRATCH-RETURN-CODE.\n",
			"",
/*9*/			"053950*\n",
			"054000     IF SCRATCH-R-2 IS GREATER THAN ZERO\n",                            
			"054010        MOVE SCRATCH-R-2 TO ERROR-RC.\n",
			"054100        DISPLAY \"ERROR CALL SCRATCH \", WSA-FILE-NAME,\n",
			"054105                \" RETURN CODE = \", ERROR-RC.\n",
			""
			};

EXT char *call_print[] = {
			"037900******************************************************************\n",
			"038000*                         PERFORM CALL PRINT                     *\n",
			"038100******************************************************************\n",
			"039300 PRINT-FILE-SUB.\n",
			"053500     CALL \"PRINT\" USING WSA-FILE-NAME, WSA-LIB-NAME,\n",
			"053600                       WSA-VOL-NAME, PRINT-MODE,\n",
			"053700                       PRINT-DISPOSITION, PRINT-NUMBER-COPIES,\n",
			"053900                       PRINT-CLASS, PRINT-FORM-NUMBER,\n",
			"053950                       PRINT-RETURN-CODE.\n",
			"",
/*10*/			"053950*\n",
			"054000     IF PRINT-R-2 IS GREATER THAN ZERO\n",                            
			"054010        MOVE PRINT-R-2 TO ERROR-RC.\n",
			"054100        DISPLAY \"ERROR CALL PRINT \", WSA-FILE-NAME,\n",
			"054105                \" RETURN CODE = \", ERROR-RC.\n",
			""
			};

EXT char *call_submit[] = {
			"037900******************************************************************\n",
			"038000*                         PERFORM CALL SUBMIT                    *\n",
			"038100******************************************************************\n",
			"039300 SUBMIT-PROGRAM-AREA.\n",
			"053500     CALL \"SUBMIT\" USING WSA-FILE-NAME, WSA-LIB-NAME,\n",
			"053600                       WSA-VOL-NAME, SUBMIT-JOB-NAME,\n",
			"053600                       SUBMIT-STATUS,\n",
			"053700                       SUBMIT-DISPOSITION, SUBMIT-JOB-CLASS,\n",
			"053900                       SUBMIT-ABORT-ACTION, SUBMIT-CPU-TIME,\n",
			"053950                       SUBMIT-LIMIT-FLAG, SUBMIT-RETURN-CODE.\n",
			"",
/*11*/			"053950*\n",
			"054000     IF SUBMIT-R-2 IS GREATER THAN ZERO\n",                            
			"054010        MOVE SUBMIT-R-2 TO ERROR-RC\n",
			"054100        DISPLAY \"ERROR CALL SUBMIT \", WSA-FILE-NAME,\n",
			"054105                \" RETURN CODE = \", ERROR-RC.\n",
			""
			};

EXT char *call_find[] = {
			"037900******************************************************************\n",
			"038000*                         PERFORM CALL FIND                      *\n",
			"038100******************************************************************\n",
			"037850 FIND-FILE-INFO.\n",
			"037900     MOVE 400 TO COUNT-INTEGER.\n",
			"038000     MOVE SPACES  TO RECEIVER REC-AREA.\n",
			"038100     MOVE 1        TO START-INTEGER.\n",
			"038200     MOVE 0        TO ENTRY-COUNT.\n",
			"038200     IF WSA-FILE-NAME = SPACES \n",
                        "038210          MOVE \"?       \"  TO WSA-FILE-NAME.\n",
			"038220     IF WSA-LIB-NAME = SPACES\n",
                        "038230          MOVE \"?       \"  TO WSA-LIB-NAME.\n",
			"038300    \n",
			"038400     CALL \"FIND\" USING WSA-FILE-NAME, WSA-LIB-NAME, WSA-VOL-NAME,\n",
			"038500                     STARTER, COUNTER, RECEIVER, ENTRIES.\n",
			"038600     MOVE RECEIVER TO REC-AREA.\n",
			""
		};

EXT char *call_putparm[] = {
			"CALL \"PUTPARM\" USING PPARM-FUNC, PPARM-PRNAME",
/*1*/			"                         PPARM-KW-COUNT",
/*2*/			"                         PPARM-KW-",
/*3*/			", PPARM-VAL-",
/*4*/			"                         PPARM-LENGTH-",
/*5*/			"                         PPARM-PFKEY,\n",
/*6*/			"                         PPARM-LABEL,\n",
			"",
/*8*/			"              PPARM-RETURN-CODE",
/*9*/			"IF PPARM-R-2 IS GREATER THAN ZERO",                            
/*10*/			"    DISPLAY \"ERROR CALL PUTPARM \", PPARM-PRNAME",
/*11*/			"           MOVE PPARM-PFKEY-TABLE-TABLE(",
/*12*/			") TO PPARM-PFKEY.\n",
/*13*/			"PPARM-FUNC",
/*14*/			"PPARM-KW-",
/*15*/			"PPARM-VAL-",
/*16*/			"PPARM-LEN-",
			""
			};

EXT char *call_readfdr[] = {
			"037900******************************************************************\n",
			"038000*                      PERFORM CALL READFDR                      *\n",
			"038100******************************************************************\n",
			"037850 READFDR-FILE-INFO.\n",
			"038400     CALL \"READFDR\" USING READFDR-FILE, READFDR-LIB, READFDR-VOL,\n",
			"038500                     READFDR-MODE, READFDR-FLD-ID, READFDR-RECVR,\n",
			"038500                     READFDR-RETURN-CODE.\n",
			"",
/*8*/			"053950*\n",
			"038500     IF RFDR-RC-R-2 IS GREATER THAN ZERO\n",                            
			"038500        MOVE RFDR-RC-R-2  TO ERROR-RC\n",
			"038500        DISPLAY \"ERROR CALL READFDR \", READFDR-FILE,\n",
			"038500                \" RETURN CODE = \", ERROR-RC.\n",
			""
		};

#else

EXT char *ident_div[];
EXT char *env_div[];
EXT char *i_o_div[];
EXT char *data_div[];
EXT char *work_stg_sec[];
EXT char *displ_def[];
EXT char *cobol_rename[];
EXT char *cobol_scratch[];
EXT char *cobol_print[];
EXT char *cobol_submit[];
EXT char *cobol_string[];
EXT char *cobol_readfdr[];
EXT char *link_parms[];
EXT char *cobol_find[];
EXT char *cobol_putparm[];
EXT char *set_extct[];
EXT char *link_sec[];
EXT char *proc_div[];
EXT char *init_src[];
EXT char *main_process[];
EXT char *main_exit[];
EXT char *disp_read[];
EXT char *call_set[];
EXT char *call_extract[];
EXT char *call_link[];
EXT char *call_rename[];
EXT char *call_logoff[];
EXT char *call_scratch[];
EXT char *call_print[];
EXT char *call_submit[];
EXT char *call_find[];
EXT char *call_putparm[];
EXT char *call_readfdr[];

#endif
/*
**	History:
**	$Log: pgcblsrc.h,v $
**	Revision 1.12  2003/02/04 21:51:17  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  1997/04/21 14:50:29  scass
**	Corrected copyright
**	
**	Revision 1.10  1997-02-18 10:32:14-05  gsl
**	Changed AUTHOR name
**
**	Revision 1.9  1996-09-12 19:21:55-04  gsl
**	Add drcs headers
**
**
**
*/
