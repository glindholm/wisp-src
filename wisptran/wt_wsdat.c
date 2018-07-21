/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/


/*
**	wisp_wsdat.c
*/

#define EXT extern
#include "wisp.h"
#include "crt.h"
#include "wispfile.h"
#include "cobfiles.h"

#include "wcommon.h"
#include "intdef.h"

#ifdef WIN32
#include "isonames.h"
#endif

int ws_init(void)
{
	register int i;
	int4	fval;
	char	fileAttrib[WISP_FILE_ATTR_SIZE+1];
	char	COPY_WORKSTOR[40];
	int	use_copy, output_copy;
	cob_file	*cob_file_ptr;

	use_copy = 0;						/* Use a "COPY" statement		*/
	output_copy = 0;					/* Write the copy code.			*/

	/*
		Version	Revision Date		Wisp
		--------------------------------------
		 7	1.5			V3_3x12
		 8	1.11	 1997/04/29	V4_2_00
		 9	1.12     1998-02-10 
		10	1.14     1998-03-17	4.2.02 
		11	1.18	 2002/05/16	5.0.xx
		51xx	1.41     2003/08/11	5.1.xx - change to using 4 digit WISP version
		--------------------------------------
						      Copybook number ( 001=figcon, 002=Work-Stor )
						      |COBOL (2=ACUCOBOL 5=MF)
						      || Version (4 digit WISP version)
						      || |
	*/
	if      (acu_cobol) sprintf(COPY_WORKSTOR, "wc22%04d.%s", (int)(WISP_VERSION_NUM), copybookext());
	else if (mf_cobol)  sprintf(COPY_WORKSTOR, "wc25%04d.%s", (int)(WISP_VERSION_NUM), copybookext());
	else                sprintf(COPY_WORKSTOR, "wc2x%04d.%s", (int)(WISP_VERSION_NUM), copybookext());

	tput_blank();
	tput_scomment		("*****************************************************************");
	tput_scomment		("**** Fields inserted by the WISP translator %s", WISP_VERSION);
	tput_scomment		("*****************************************************************");

	if ( writing_cob_main() || !opt_gen_copylib )					/* If not writing to a copybook file	*/
	{
		tput_line_at(12, "COPY \"%s\".",COPY_WORKSTOR);				/* Write the copy statement		*/
		use_copy = 1;
		if ( opt_forcegenwispcpy || access(COPY_WORKSTOR,0) != 0 )			/* If copybook doesn't exist ...	*/
		{
			output_copy = 1;
			cob_file_ptr = open_cob_file(COPY_WORKSTOR,FOR_OUTPUT,1);	/* open it and start writing.		*/
			override_output_stream(cob_file_ptr);
		}
	}
	else
	{
		output_copy = 1;							/* Just output the code to the file	*/
	}

	if (output_copy)
	{
		tput_scomment		("*****************************************************************");
		tput_scomment		("**** WISP STANDARD WORKING STORAGE VARIABLES");
		tput_scomment		("****");
		tput_scomment		("**** COPYBOOK FILE = %s",COPY_WORKSTOR);
		tput_scomment		("****");
		tput_scomment		("*****************************************************************");
		tput_line		("       01  VWANG-WRITE-ALL           PIC X VALUE WISP-SYMB-1.");
		tput_line		("       01  VWANG-READ-ALL            PIC X VALUE WISP-SYMB-2.");
		tput_line		("       01  VWANG-CLOSE-WS            PIC X VALUE WISP-SYMB-4.");
		tput_line		("       01  VWANG-WRITE-SELECTED      PIC X VALUE WISP-SYMB-5.");
		tput_line		("       01  VWANG-READ-ALTERED        PIC X VALUE WISP-SYMB-6.");
		tput_line		("       01  VWANG-READ-MODIFIABLE     PIC X VALUE WISP-SYMB-10.");
		tput_line		("       01  VWANG-DISP-AND-READ       PIC X VALUE WISP-SYMB-3.");
		tput_line		("       01  VWANG-DISP-AND-READ-ALT   PIC X VALUE WISP-SYMB-7.");
		tput_line		("       01  VWANG-LOAD-SUB-TABLE      PIC X VALUE WISP-SYMB-8.");
		tput_line		("       01  VWANG-INIT-DEFAULT-TABLE  PIC X VALUE WISP-SYMB-9.");
		tput_line		("       01  VWANG-LINES               PIC X.");
		tput_line		("       01  VWANG-FULL-SCREEN         PIC X VALUE WISP-SYMB-24.");
		tput_line		("       01  WISP-SET-BYTE             PIC X.");
		tput_line		("       01  WISP-TEST-BYTE            PIC X.");
		tput_line		("       01  WISP-ALL-PF-KEYS          PIC X VALUE \"A\".");
		tput_line		("       01  WISP-ALLOWABLE-PF-KEYS.");
		tput_line		("           03  WISP-ALLOWABLE-PF-KEYS-SUB  PIC 99 OCCURS 40.");
		tput_line		("           03  WISP-ALLOWABLE-PF-KEYS-STOP PIC X.");
		tput_line		("       01  WISP-ON-PF-KEYS.");
		tput_line		("           03  WISP-ON-PF-KEYS-SUB   PIC 99 OCCURS 40.");
		tput_line		("           03  WISP-ON-PF-KEYS-STOP  PIC X.");

		tput_line		("       01  WISP-DNR-DONE-FLAG        PIC X.");
		tput_line		("           88  WISP-DNR-DONE         VALUE \"Y\".");
		tput_line		("           88  WISP-DNR-NOT-DONE     VALUE \"N\".");

		tput_line		("       01  WISP-DNR-ALT-FLAG         PIC X.");
		tput_line		("           88  WISP-DNR-ALT          VALUE \"Y\".");
		tput_line		("           88  WISP-DNR-NOT-ALT      VALUE \"N\".");

		tput_line		("       01  WISP-DNR-FUNCTION         PIC X.");

		tput_line		("       01  WISP-BIT-FLAG             PIC X(1).");
		tput_line		("           88  WISP-FAC-BIT-IS-ON    VALUE \"Y\".");
		tput_line		("           88  WISP-FAC-BIT-IS-OFF   VALUE \"N\".");
		tput_line		("       01  WISP-FAC-PROTECT-FLAG     PIC X(1).");
		tput_line		("           88  WISP-FAC-PROTECTED    VALUE \"Y\".");
		tput_line		("           88  WISP-FAC-MODIFIABLE   VALUE \"N\".");
		tput_line		("       01  WISP-FAC-ALTERED-FLAG     PIC X(1).");
		tput_line		("           88  WISP-FAC-ALTERED      VALUE \"Y\".");
		tput_line		("           88  WISP-FAC-NOT-ALTERED  VALUE \"N\".");

		tput_blank();
	
		tput_line		("       01  WISP-LOCK-ID           PIC X(%d) VALUE SPACES.", COB_SELECT_NAME_SIZE);
		tput_line		("       01  WISP-NEW-LOCK-ID       PIC X(%d) VALUE SPACES.", COB_SELECT_NAME_SIZE);

		tput_line		("       01  WISP-BUFF-TABLE-16.");
		tput_line		("           05  WISP-BUFF          PIC X(80) OCCURS 16.");

		tput_line		("       01  WISP-ARGCNT            %s PIC S9(9).",bin4_type);
/*		tput_line		("       01  WISP-LONGWORD          %s PIC S9(9).",bin4_type); */
/*		tput_line		("       01  WISP-LONGWORD-1        %s PIC S9(9).",bin4_type); */
/*		tput_line		("       01  WISP-LONGWORD-2        %s PIC S9(9).",bin4_type); */
		tput_line		("       01  WISPX1                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISPX2                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISPX3                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISPX4                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISP-BIT-SET           %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISP-BIT-CLEAR         %s PIC S9(9).",bin4_type);
		tput_line		("       01  WFOPEN-INPUT         %s PIC 9(9) VALUE %d.",bin4_type,WFOPEN_INPUT);
		tput_line		("       01  WFOPEN-SHARED        %s PIC 9(9) VALUE %d.",bin4_type,WFOPEN_SHARED);
		tput_line		("       01  WFOPEN-OUTPUT        %s PIC 9(9) VALUE %d.",bin4_type,WFOPEN_OUTPUT);
		tput_line		("       01  WFOPEN-EXTEND        %s PIC 9(9) VALUE %d.",bin4_type,WFOPEN_EXTEND);
		tput_line		("       01  WFOPEN-SPECIAL-INPUT %s PIC 9(9) VALUE %d.",bin4_type,WFOPEN_SPECIAL_INPUT);
		tput_line		("       01  WFOPEN-I-O           %s PIC 9(9) VALUE %d.",bin4_type,WFOPEN_I_O);
		tput_line		("       01  WFOPEN-SORT          %s PIC 9(9) VALUE %d.",bin4_type,WFOPEN_SORT);

/*		tput_line		("       01  WISP-OUTPUT  %s PIC 9(9) VALUE %ld.",bin4_type,IS_OUTPUT);  */
/*		tput_line		("       01  WISP-PRINT   %s PIC 9(9) VALUE %ld.",bin4_type,IS_PRINTFILE); */
/*		tput_line		("       01  WISPSCRTCH   %s PIC 9(9) VALUE %ld.",bin4_type,IS_SCRATCH); */
/*		tput_line		("       01  WISPSUBMIT   %s PIC 9(9) VALUE %ld.",bin4_type,IS_USE_PVPL); */
/*		tput_line		("       01  WISP-EXTEND  %s PIC 9(9) VALUE %ld.",bin4_type,IS_EXTEND); */
/*		tput_line		("       01  WISP-I-O     %s PIC 9(9) VALUE %ld.",bin4_type,IS_IO); */
/*		tput_line		("       01  WISP-SORT    %s PIC 9(9) VALUE %ld.",bin4_type,IS_SORT); */
/*		tput_line		("       01  WISP-S-OUT   %s PIC 9(9) VALUE %ld.",bin4_type,IS_OUTPUT+IS_SORT); */
/*		tput_line		("       01  WISP-NORES   %s PIC 9(9) VALUE %ld.",bin4_type,IS_NORESPECIFY); */
/*		tput_line		("       01  WISP-NOWRITE %s PIC 9(9) VALUE %ld.",bin4_type,IS_NOTSHARE); */
/*		tput_line		("       01  WISP-PROC    %s PIC 9(9) VALUE %ld.",bin4_type, IS_USE_PVPL); */
/*		tput_line		("       01  WISP-DECL    %s PIC 9(9) VALUE %ld.",bin4_type, IS_DECLARE); */

		tput_line		("       01  WISP-LOCK-TIMEOUT %s PIC S9(9) VALUE 0.",bin4_type);
	
		tput_line		("       01  WISP-RELATIVE-KEY         PIC 9(8).");
		tput_line		("       01  WISP-FILE-STATUS          PIC X(2).");
		tput_line		("       01  WISP-SKIP-DECLARATIVES    PIC X.");
		tput_line		("       01  WISP-HAS-DECLARATIVES     PIC X.");
/*		tput_line		("       01  WISP-DECLARATIVES-STATUS  PIC X(2)."); */
		tput_line		("       01  WISP-LASTFILEOP           PIC X(%d).", COB_FILEOP_SIZE);
		tput_line		("       01  WISP-EXTENDED-FILE-STATUS PIC X(%d).", COB_EXTENDED_FILE_STATUS_SIZE);
/*		tput_line		("       01  WISP-EXTENDED-FILE-STATUS-2 PIC X(10)."); */
	
		tput_line		("       01  WISP-RUN-NAME             PIC X(8)  VALUE SPACES.");
		tput_line		("       01  WISP-CURRENT-FILE-ID      PIC X(%d).",COB_SELECT_NAME_SIZE);
		tput_line		("       01  WISP-SAVE-FILE-STATUS     PIC X(2).");
		tput_line		("       01  WISP-PFKEY-VALUE          PIC 9(2).");
		tput_line		("       01  WISP-SAVE-WCC             PIC X.");
		tput_line		("       01  WISP-PROTECT-BIT          PIC X(1) VALUE WISP-SYMB-4.");
/*		tput_line		("       01  WISP-XERROR-BIT           PIC X(1) VALUE WISP-SYMB-8."); */
/*		tput_line		("       01  WISP-ERROR-BIT            PIC X(1) VALUE WISP-SYMB-16."); */
		tput_line		("       01  WISP-MOD-BIT              PIC X(1) VALUE WISP-SYMB-64.");
		tput_line		("       01  WISP-ALTERED-BIT          PIC X(1) VALUE WISP-SYMB-64.");
		tput_blank();
	
		tput_line		("       01  WISP-ALPHA-CONVERSION-FIELD     PIC X(18).");
		tput_line		("       01  WISP-CONVERTED-FRACTION-FIELD   PIC SV9(18)");
		tput_line		("                SIGN IS TRAILING.");
		tput_line		("       01  WISP-CONVERTED-INTEGER-FIELD    PIC S9(18)");
		tput_line		("                SIGN IS TRAILING.");
		tput_line		("       01  WISP-CONVERSION-LENGTH  PIC X(1) VALUE WISP-SYMB-18.");
	
		tput_line		("       01  WISP-CONVERSION-ERROR-FIELD %s PIC S9(9).",bin4_type);
	
		tput_line		("       01  WISP-NUMERIC-CONVERSION-FLAG    PIC X(1).");
		tput_line		("           88  WISP-NO-NUMERIC-CONV-ERROR VALUE \"N\".");
		tput_line		("           88  WISP-A-NUMERIC-CONV-ERROR  VALUE \"Y\".");

		tput_blank();
		tput_scomment		("*****************************************************************");
		tput_scomment		("**** FIELDS USED FOR ON PFKEYS PFKEY-LIST");
		tput_scomment		("*****************************************************************");
		tput_blank();
	
		tput_line		("       01  WISP-DFIELD-0                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-1                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-2                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-3                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-4                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-5                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-6                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-7                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-8                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-9                   PIC 99.");
		tput_line		("       01  WISP-DFIELD-10                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-11                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-12                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-13                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-14                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-15                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-16                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-17                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-18                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-19                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-20                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-21                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-22                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-23                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-24                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-25                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-26                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-27                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-28                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-29                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-30                  PIC 99.");
		tput_line		("       01  WISP-DFIELD-31                  PIC 99.");
		tput_blank();
	
		tput_scomment		("*****************************************************************");
		tput_scomment		("**** Scratch fields ");
		tput_scomment		("*****************************************************************");
		tput_blank();
			
		tput_line		("       01  WISP-SCRATCH-BYTE-1            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-1-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-2            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-2-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-3            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-3-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-4            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-4-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-5            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-5-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-6            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-6-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-7            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-7-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-8            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-8-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-9            PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-9-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-10           PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-10-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-11           PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-11-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-12           PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-12-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-13           PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-13-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-14           PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-14-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-15           PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-15-TRUE   VALUE \"Y\".");
		tput_line		("       01  WISP-SCRATCH-BYTE-16           PIC X.");
		tput_line		("           88  WISP-SCRATCH-BYTE-16-TRUE   VALUE \"Y\".");

		tput_blank();
		tput_scomment		("*    WISP RETURN-CODE handling.");
		tput_line		("       01  WISP-RETURN-CODE          PIC XXX   VALUE \"000\".");
		tput_line		("       01  KW-RETURN-CODE REDEFINES WISP-RETURN-CODE PIC 999.");
		tput_blank();

		tput_blank();
		tput_scomment		("*    WISP Workstation processing fields.");
		tput_line		("       01  WISP-FULL-SCREEN-ORDER-AREA.");
		tput_line		("           03  WISP-IOCW-1           PIC X(1) VALUE WISP-SYMB-1.");
		tput_line		("           03  WISP-IOCW-2           PIC X(1) VALUE WISP-SCWCC.");
		tput_line		("           03  WISP-IOCW-3           PIC X(1) VALUE WISP-SYMB-0.");
		tput_line		("           03  WISP-IOCW-4           PIC X(1) VALUE WISP-SYMB-0.");
		tput_blank();
		tput_line		("       01  WISP-CRT-RECORD.");
		tput_line		("           03  WISP-CRT-ORDER-AREA.");
		tput_line		("               05  WISP-CRT-ORDER-AREA-1  PIC X VALUE WISP-SYMB-1.");
		tput_line		("               05  WISP-CRT-ORDER-AREA-2  PIC X VALUE WISP-SCWCC.");
		tput_line		("               05  WISP-CRT-ORDER-AREA-3  PIC X VALUE WISP-SYMB-0.");
		tput_line		("               05  WISP-CRT-ORDER-AREA-4  PIC X VALUE WISP-SYMB-0.");
		tput_line		("           03  WISP-CRT-O-A REDEFINES WISP-CRT-ORDER-AREA PIC X(4).");
		tput_line		("           03  WISP-CRT-SCREEN-AREA.");
		tput_line		("               05  FILLER     PIC X(1920).");
		tput_line		("           03  WISP-CRT-SCREEN-LINES REDEFINES WISP-CRT-SCREEN-AREA.");
		tput_line		("               05  WISP-SCREEN-LINE PIC X(80) OCCURS 24 TIMES.");
		tput_blank();

		tput_blank();
		tput_scomment( "*****************************************************************");
		tput_scomment( "**** Native Screens fields");	/* opt_native_screens */
		tput_scomment( "*****************************************************************");
		tput_blank();
	
		tput_scomment( "*    Special-names CURSOR clause.");
		tput_line_at(8, "01  WISP-CURSOR.");
		tput_line_at(8, "    05  WISP-CURSOR-LINE    PIC 999 VALUE 1.");
		tput_line_at(8, "    05  WISP-CURSOR-COL     PIC 999 VALUE 1.");
		
		if (acu_cobol)
		{
			tput_blank();
			tput_scomment( "*    Special-names CRT STATUS clause.");
			tput_line_at(8, "01  WISP-CRT-STATUS.");
			tput_line_at(8, "    05  WISP-CRT-KEY-1               PIC X.");
			tput_line_at(8, "        88  WISP-CRT-STATUS-TERMINATED  VALUE '0'.");
			tput_line_at(8, "        88  WISP-CRT-STATUS-EXCEPTION   VALUE '1'.");
			tput_line_at(8, "        88  WISP-CRT-STATUS-TIMEOUT     VALUE '3'.");
			tput_line_at(8, "        88  WISP-CRT-STATUS-NOITEM      VALUE '9'.");
			tput_line_at(8, "    05  WISP-CRT-KEY-EXCEPTION       PIC X COMP-X.");
			tput_line_at(8, "        88  WISP-CRT-EX-HELP            VALUE 33.");
			tput_line_at(8, "        88  WISP-CRT-EX-GETKEY          VALUE 34.");
			tput_line_at(8, "        88  WISP-CRT-KEY-AUTOSKIP       VALUE 49.");
			tput_line_at(8, "    05  WISP-CRT-KEY-3               PIC X COMP-X.");		       
			tput_line_at(8, "        88  WISP-CRT-KEY-TAB            VALUE  9.");
			tput_line_at(8, "        88  WISP-CRT-KEY-ENTER          VALUE 13.");

			tput_blank();
			tput_scomment( "*    Special-names SCREEN CONTROL clause.");
			tput_line_at(8, "01  WISP-SCREEN-CONTROL.");
			tput_line_at(8, "    05  WISP-ACCEPT-CONTROL     PIC 9   VALUE 0.");
			tput_line_at(8, "    05  WISP-CONTROL-VALUE      PIC 999 VALUE 1.");
			tput_line_at(8, "    05  WISP-CONTROL-HANDLE     USAGE HANDLE.");
			tput_line_at(8, "    05  WISP-CONTROL-ID         PIC XX COMP-X.");
		}
			
		if (mf_cobol)
		{
			tput_blank();
			/*        123456*89+123456789+123456789+123456789+123456789+123456789+123456789+123456789+ */
			tput_scomment( "*    Micro Focus ADIS Routine Calls.");
			tput_line_at(8, "01  WISP-ADIS-SET-BIT-PAIRS        PIC 9(2) COMP-X VALUE 1.");
			tput_line_at(8, "01  WISP-ADIS-USER-KEY-CONTROL.");
			tput_line_at(8, "    05  WISP-ADIS-USER-KEY-SETTING PIC 9(2) COMP-X.");
			tput_line_at(8, "    05  FILLER                     PIC X    VALUE \"1\".");
			tput_line_at(8, "    05  WISP-ADIS-FIRST-USER-KEY   PIC 9(2) COMP-X.");
			tput_line_at(8, "    05  WISP-ADIS-NUMBER-OF-KEYS   PIC 9(2) COMP-X.");

			tput_blank();
			tput_scomment( "*    Accept Omitted Field.");
			tput_line_at(8, "01  WISP-OMITTED-FIELD PIC X.");

			tput_blank();
			tput_scomment( "*    Special-names CRT STATUS clause.");
			tput_line_at(8, "01  WISP-CRT-STATUS.");
			tput_line_at(8, "    05  WISP-CRT-STATUS-1            PIC X.");
			tput_line_at(8, "        88  WISP-CRT-STATUS-TERMINATED  VALUE '0'.");
			tput_line_at(8, "        88  WISP-CRT-STATUS-FUNCKEY     VALUE '1'.");
			tput_line_at(8, "        88  WISP-CRT-STATUS-ERROR       VALUE '9'.");
			tput_line_at(8, "    05  WISP-CRT-STATUS-2            PIC 99 COMP-X.");
			tput_line_at(8, "        88  WISP-CRT-EX-ESC             VALUE 0.");
			tput_line_at(8, "        88  WISP-CRT-EX-HELP            VALUE 33.");
			tput_line_at(8, "        88  WISP-CRT-EX-GETKEY          VALUE 34.");
			tput_line_at(8, "        88  WISP-CRT-EX-ENTER           VALUE 48.");
			tput_line_at(8, "    05  WISP-CRT-STATUS-3            PIC 99 COMP-X.");		       
		}
		
		tput_blank();
		tput_scomment( "*    Fields for DISPLAY statements.");
		tput_line_at(8, "01  WISP-DISPLAY-FIELDS-DATA PIC X(1185).");
		tput_line_at(8, "01  FILLER REDEFINES WISP-DISPLAY-FIELDS-DATA.");
		tput_line_at(8, "    05  WISP-DISPLAY-FIELDS OCCURS 15 PIC X(79).");
		
		if (acu_cobol)
		{
			tput_blank();
			tput_scomment( "*    Screen items COLOR phrase values.");
			tput_line_at(8, "78  WISP-CLR-REVERSE VALUE  1024.");
			tput_scomment( "*                            1024 = Reversed");
			tput_line_at(8, "78  WISP-CLR-BRIGHT  VALUE  4096.");
			tput_scomment( "*                            4096 = High Intensity");
			tput_line_at(8, "78  WISP-CLR-UNDER   VALUE  8192.");
			tput_scomment( "*                            8192 = Underline");
			tput_line_at(8, "78  WISP-CLR-BLINK   VALUE 16384.");
			tput_scomment( "*                           16384 = Blink");
			tput_line_at(8, "78  WISP-CLR-PROTECT VALUE 32768.");
			tput_scomment( "*                           32768 = Protected");
			tput_line_at(8, "78  WISP-CLR-ERROR   VALUE 17408.");
			tput_scomment( "*                           17408 = Blink + Reversed");
		}
		if (mf_cobol)
		{
			tput_line_at(8, "78  WISP-CTR-ERROR   VALUE \"REVERSE-VIDEO\".");
			tput_line_at(8, "78  WISP-CTR-NONERR  VALUE \"HIGHLIGHT\".");
		}
		
		tput_blank();
		tput_scomment( "*    WISP workstation working items.");
		tput_line_at(8, "01  WISP-PFKEY                 PIC 99.");
		tput_line_at(8, "    88  WISP-PFKEY-ENTER       VALUE  0.");
		tput_line_at(8, "    88  WISP-PFKEY-HELP        VALUE 33.");
		tput_line_at(8, "    88  WISP-PFKEY-INVALID     VALUE 99.");
		tput_line_at(8, "01  WISP-CURSOR-POSITION.");
		tput_line_at(8, "    05  WISP-CURSOR-POSITION-COL %s PIC S9(4).", bin2_type);
		tput_line_at(8, "    05  WISP-CURSOR-POSITION-ROW %s PIC S9(4).", bin2_type);
	
		tput_blank();
		tput_scomment( "*    WISP DISPLAY AND READ working items.");
		tput_line_at(8, "01  WISP-ALLOWABLE-PF-KEYS-CNT PIC 99.");
		tput_line_at(8, "01  WISP-ON-PF-KEYS-CNT        PIC 99.");
		tput_line_at(8, "01  WISP-DNR-ON-PFKEY          PIC X VALUE \"N\".");
		tput_line_at(8, "01  WISP-DNR-NO-MOD            PIC X VALUE \"N\".");
		tput_line_at(8, "01  WISP-DNR-RANGE-ERROR       PIC X VALUE \"N\".");
		
		tput_blank();
		tput_line_at(8, "01  WISP-DNR-ORDER-AREA.");
		tput_line_at(8, "    05  WISP-DNR-ROW         PIC X COMP-X VALUE 1.");
		tput_line_at(8, "    05  WISP-DNR-WCC         PIC X COMP-X VALUE 160.");
		tput_line_at(8, "        88  WISP-DNR-WCC-UNLOCK-KEYBOARD  VALUES");
		tput_line_at(8, "            128 THRU 255.");
		tput_line_at(8, "        88  WISP-DNR-WCC-SOUND-ALARM      VALUES ");
		tput_line_at(8, "             64 THRU 127, 192 THRU 255.");
		tput_line_at(8, "        88  WISP-DNR-WCC-POSITION-CURSOR  VALUES");
		tput_line_at(8, "             32 THRU  63,  96 THRU 127,");
		tput_line_at(8, "            160 THRU 171, 224 THRU 255.");
		tput_line_at(8, "    05  WISP-DNR-CURSOR-COL  PIC X COMP-X VALUE 0.");
		tput_line_at(8, "    05  WISP-DNR-CURSOR-ROW  PIC X COMP-X VALUE 0.");
	
		tput_blank();
		tput_line_at(8, "01  WISP-DNR-WCC-CURSOR-BIT  PIC X VALUE X\"20\".");
		tput_line_at(8, "01  WISP-DNR-WCC-ALARM-BIT   PIC X VALUE X\"40\".");

		tput_blank();
		tput_scomment		("*****************************************************************");
		tput_scomment		("**** END OF COPYBOOK FILE = %s",COPY_WORKSTOR);
		tput_scomment		("*****************************************************************");
	}

	if (use_copy && output_copy)
	{
		release_output_stream();
		close_cob_file(cob_file_ptr);
	}
		

	if (!opt_native)
	{
		tput_line	("       01  WISP-VERSION              PIC X(%d) VALUE \"%s\".",WISP_VERSION_SIZE, WISP_VERSION);
		tput_line  	("       01  WISP-SWAP-ON              PIC S9(9) %s VALUE %s.",bin4_type,
					(opt_no_word_swap) ? "0":"1" );
	}
	tput_line		("       01  WISP-APPLICATION-NAME     PIC X(8)  VALUE \"%-8.8s\".",prog_id);

	tput_blank();
	tput_scomment		("*****************************************************************");
	tput_scomment		("**** Fields for file path translation");
	tput_scomment		("*****************************************************************");

	for (i=0; i<prog_cnt; i++)
	{
		tput_line("       01  %s PIC X(80) VALUE SPACES.", get_prog_nname(i));

		if (is_literal(prog_fnames[i]) || (!prog_fnames[i][0]))		/* file name is a literal/null	*/
		{
			if (prog_fnames[i][0])
			{
				tput_line("       01  %s PIC X(8) VALUE %s.",get_prog_fname(i),prog_fnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				tput_line("       01  %s PIC X(8) VALUE SPACES.",get_prog_fname(i));
			}
		}

		if (is_literal(prog_lnames[i]) || (!prog_lnames[i][0]))		/* Library name is a literal/null */
		{
			if (prog_lnames[i][0])
			{
				tput_line("       01  %s PIC X(8) VALUE %s.",get_prog_lname(i),prog_lnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				tput_line("       01  %s PIC X(8) VALUE SPACES.",get_prog_lname(i));
			}
		}
		if (is_literal(prog_vnames[i]) || (!prog_vnames[i][0]))		/* Volume name is a literal/null */
		{
			if (prog_vnames[i][0])
			{
				tput_line("       01  %s PIC X(6) VALUE %s.",get_prog_vname(i),prog_vnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				tput_line("       01  %s PIC X(6) VALUE SPACES.",get_prog_vname(i));
			}
		}

		tput_line("       01  %s PIC X(8) VALUE \"%-8s\".", get_prog_prname(i), prog_prname[i]);

		fval = 0;
		fileAttrib[0] = '\0';

		if (prog_ftypes[i] & PRINTER_FILE)
		{									/* init flags as a print file	*/
			fval += IS_PRINTFILE;
			strcat(fileAttrib, IS_PRINTFILE_ATTR);
		}

		if (prog_ftypes[i] & INDEXED_FILE)					/* Flag as indexed.		*/
		{
			fval += IS_INDEXED;
			strcat(fileAttrib, IS_INDEXED_ATTR);
		}

		if (prog_ftypes[i] & SEQ_DYN)						/* File is SEQ/DYN.		*/
		{
			fval += IS_SEQDYN;
			strcat(fileAttrib, IS_SEQDYN_ATTR);
		}

		if ((prog_ftypes[i] & SEQ_FILE) && !(prog_ftypes[i] & SEQ_DYN))		/* File is SEQ/SEQ		*/
		{
			fval += IS_SEQSEQ;
			strcat(fileAttrib, IS_SEQSEQ_ATTR);
		}

		if (prog_ftypes[i] & NORESPECIFY)					/* File is NORESPECIFY.		*/
		{
			fval += IS_NORESPECIFY;
			strcat(fileAttrib, IS_NORESPECIFY_ATTR);
		}

		if (prog_ftypes[i] & RELATIVE_FILE)					/* File is RELATIVE.		*/
		{
			fval += IS_RELATIVE;
			strcat(fileAttrib, IS_RELATIVE_ATTR);
		}

		if (prog_ftypes[i] & DBFILE_FILE)					/* File is DATABASE.		*/
		{
			fval += IS_DBFILE;
			strcat(fileAttrib, IS_DBFILE_ATTR);
		}

		/* tput_line("       01  %s %s PIC 9(9) VALUE %ld.", get_prog_status(i), bin4_type,fval); */
		if (0==strlen(fileAttrib))
		{
			strcpy(fileAttrib," "); /* Acucobol warns about an empty "" string */
		}
		tput_line("       01  %s PIC X(%d) VALUE \"%s\".", 
			get_prog_status(i), WISP_FILE_ATTR_SIZE, fileAttrib);
	}

	if (fig_count)
	{
		tput_blank();
		tput_scomment("*****************************************************************");
		tput_scomment("**** 2 byte FIGURATIVE-CONSTANTS ");
		tput_scomment("*****************************************************************");
		for (i=0; i<fig_count; i++)
		{
			tput_line("       01  %s.",fig_cons[i]);
			tput_line("           05  FILLER PIC X VALUE WISP-SYMB-%d.",fig_val[i][0]);
			tput_line("           05  FILLER PIC X VALUE WISP-SYMB-%d.",fig_val[i][1]);
		}
	}

	tput_blank();

	if (crt_file_ptr)								/* if there was a crt file...	*/
	{
		close_cob_file(crt_file_ptr);

		copy_file(crt_file_ptr->a_file->name);					/* copy the crt definitions	*/
		delete(crt_file_ptr->a_file->name);					/* now remove the file		*/
	}
	return 0;
}


/*
**	History:
**	$Log: wt_wsdat.c,v $
**	Revision 1.50  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.49  2003/12/02 21:23:20  gsl
**	Fix so native screen sections don't get generated in a copybook file.
**	Change generated copybooks (internal) to use same file extension rules
**	as translated copybooks. Default to .cob extension.
**	
**	Revision 1.48  2003/09/05 18:15:22  gsl
**	MF Native Screens
**	
**	Revision 1.47  2003/09/03 20:05:56  gsl
**	MF native screens
**	
**	Revision 1.46  2003/08/27 16:50:45  gsl
**	MF Native Screens
**	
**	Revision 1.45  2003/08/15 20:54:27  gsl
**	Micro Focus Native Screens
**	
**	Revision 1.44  2003/08/13 20:57:38  gsl
**	Micro Focus Native Screens
**	
**	Revision 1.43  2003/08/12 20:56:38  gsl
**	Micro Focus native screens
**	
**	Revision 1.42  2003/08/11 21:08:44  gsl
**	Use WISP_VERSION_NUM a 4 digit version number as version for cpy file
**	
**	Revision 1.41  2003/08/11 17:18:18  gsl
**	MF Native screens
**	
**	Revision 1.40  2003/03/17 20:33:16  gsl
**	remove commented old code
**	
**	Revision 1.39  2003/03/10 22:39:42  gsl
**	Remove WISP-LONGWORD usage
**	
**	Revision 1.38  2003/03/10 18:55:44  gsl
**	Added nosleep option and for ACU default to using C$SLEEP instead
**	of WFWAIT on a READ with HOLD
**	
**	Revision 1.37  2003/03/07 20:11:37  gsl
**	Use defines for all the field sizes passed from Cobol to C
**	
**	Revision 1.36  2003/03/06 21:50:55  gsl
**	Change to use ATTR instead of MODE
**	
**	Revision 1.35  2003/02/28 21:49:04  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.34  2003/02/25 21:56:03  gsl
**	cleanup some global "parm" variables
**	
**	Revision 1.33  2003/02/04 17:33:18  gsl
**	fix copyright header
**	
**	Revision 1.32  2002/12/12 19:57:37  gsl
**	comments
**	
**	Revision 1.31  2002/08/13 20:21:11  gsl
**	Don't gen empty ATTR- values clause
**	
**	Revision 1.30  2002/08/12 20:13:50  gsl
**	quotes and literals
**	
**	Revision 1.29  2002/07/30 22:00:48  gsl
**	globals
**	
**	Revision 1.28  2002/07/30 19:12:38  gsl
**	SETRETCODE
**	
**	Revision 1.27  2002/07/29 21:13:24  gsl
**	
**	Revision 1.26  2002/07/17 15:15:40  gsl
**	MF doesn't like underscores in COBOL names
**	
**	Revision 1.25  2002/07/17 15:03:27  gsl
**	MF doesn't like underscores in COBOL names
**	
**	Revision 1.24  2002/07/02 03:55:23  gsl
**	Add WISP-SKIP-DECLARATIVES
**	
**	Revision 1.23  2002/06/28 04:02:56  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.22  2002/06/21 20:49:35  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.21  2002/06/21 03:46:58  gsl
**	Comment out unneede IS_xxx bits
**	
**	Revision 1.20  2002/06/20 23:06:12  gsl
**	Native
**	
**	Revision 1.19  2002/06/19 22:50:55  gsl
**	Fix comments
**	Remove obslete code
**	Add opt_native changes
**	
**	Revision 1.18  2002/05/16 21:53:51  gsl
**	add WISP-LASTFILEOP
**	increment copybook version
**	
**	Revision 1.17  1998-04-03 14:17:48-05  gsl
**	Add Id info to copybook
**
**	Revision 1.16  1998-03-26 14:26:41-05  gsl
**	Change to use WISP-SYMB-xx
**	Change to use WISP-CLR-xx
**	Change to use WISPX1,2,3
**
**	Revision 1.15  1998-03-23 13:41:55-05  gsl
**	Changed to use get_prog_xxx() routines
**	Changed W-RETURN-CODE to KW-RETURN-CODE
**
**	Revision 1.14  1998-03-17 14:57:49-05  gsl
**	Move items from wt_scrn to be included in the copybook
**
**	Revision 1.13  1998-03-04 15:56:54-05  gsl
**	Ensure WISP-APPLICATION-NAME is truncated to 8 characters
**
**	Revision 1.12  1998-02-10 15:11:57-05  gsl
**	Added WISP-ALL-PF-KEYS and up the version number to '9'.
**
**	Revision 1.11  1997-04-29 12:56:03-04  gsl
**	Change the working-storage copybook for WISP-EXTENDED-FILE-STATUS-1 and 2.
**	Now is a 10 digit field.
**
**	Revision 1.10  1996-08-30 21:56:27-04  gsl
**	drcs update
**
**
**
*/
