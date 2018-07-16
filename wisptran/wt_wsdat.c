static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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

ws_init()
{
	register int i;
	int4	fval;
	char	COPY_WORKSTOR[40];
	int	use_copy, output_copy;
	cob_file	*cob_file_ptr;

	use_copy = 0;									/* Use a "COPY" statement		*/
	output_copy = 0;								/* Write the copy code.			*/

	/*
						      Copybook number ( 001=figcon, 002=Work-Stor )
						      |COBOL (1=VAX, 2=ACUCOBOL etc)
						      || Version 
						      || |
	*/
	if      (vax_cobol) strcpy(COPY_WORKSTOR,"wc002108.cpy");
	else if (acu_cobol) strcpy(COPY_WORKSTOR,"wc002208.cpy");
	else if (lpi_cobol) strcpy(COPY_WORKSTOR,"wc002308.cpy");
	else if (aix_cobol) strcpy(COPY_WORKSTOR,"wc002408.cpy");
	else if (mf_cobol)  strcpy(COPY_WORKSTOR,"wc002508.cpy");
	else if (dmf_cobol) strcpy(COPY_WORKSTOR,"wc002608.cpy");
	else                strcpy(COPY_WORKSTOR,"wc002x08.cpy");

	tput_scomment		("*****************************************************************");
	tput_scomment		("*  Special WISP variables inserted by the translator program");
	tput_scomment		("*  Translated by WISP %s",WISP_VERSION);
	tput_scomment		("*****************************************************************");

	if ( writing_cob_main() || !copylib )						/* If not writing to a copybook file	*/
	{
		tput_line_at(12, "COPY \"%s\".",COPY_WORKSTOR);				/* Write the copy statement		*/
		use_copy = 1;
		if ( access(COPY_WORKSTOR,0) != 0 )					/* If copybook doesn't exist ...	*/
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
		tput_scomment		("* WISP STANDARD WORKING STORAGE VARIABLES");
		tput_scomment		("*****************************************************************");
		tput_line		("       01  VWANG-WRITE-ALL           PIC X VALUE DEC-BYTE-1.");
		tput_line		("       01  VWANG-READ-ALL            PIC X VALUE DEC-BYTE-2.");
		tput_line		("       01  VWANG-CLOSE-WS            PIC X VALUE DEC-BYTE-4.");
		tput_line		("       01  VWANG-WRITE-SELECTED      PIC X VALUE DEC-BYTE-5.");
		tput_line		("       01  VWANG-READ-ALTERED        PIC X VALUE DEC-BYTE-6.");
		tput_line		("       01  VWANG-READ-MODIFIABLE     PIC X VALUE DEC-BYTE-10.");
		tput_line		("       01  VWANG-DISP-AND-READ       PIC X VALUE DEC-BYTE-3.");
		tput_line		("       01  VWANG-DISP-AND-READ-ALT   PIC X VALUE DEC-BYTE-7.");
		tput_line		("       01  VWANG-LOAD-SUB-TABLE      PIC X VALUE DEC-BYTE-8.");
		tput_line		("       01  VWANG-INIT-DEFAULT-TABLE  PIC X VALUE DEC-BYTE-9.");
		tput_line		("       01  VWANG-LINES               PIC X.");
		tput_line		("       01  VWANG-FULL-SCREEN         PIC X VALUE DEC-BYTE-24.");
		tput_line		("       01  WISP-SET-BYTE             PIC X.");
		tput_line		("       01  WISP-TEST-BYTE            PIC X.");
#ifdef OLD
		tput_line		("       01  WISP-ALLOWABLE-PF-KEYS    PIC X(67).");
		tput_line		("       01  WISP-ON-PF-KEYS           PIC X(67).");
#endif
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
	
		tput_line		("       01  WISP-LOCK-ID           PIC X(40) VALUE SPACES.");
		tput_line		("       01  WISP-NEW-LOCK-ID       PIC X(40) VALUE SPACES.");

		tput_line		("       01  WISP-BUFF-TABLE-16.");
		tput_line		("           05  WISP-BUFF          PIC X(80) OCCURS 16.");

		tput_line		("       01  WISP-LONGWORD          %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISP-LONGWORD-1        %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISP-LONGWORD-2        %s PIC S9(9).",bin4_type);
		tput_line		("       01  WIDX-1                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WIDX-2                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WIDX-3                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WIDX-4                 %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISP-BIT-SET           %s PIC S9(9).",bin4_type);
		tput_line		("       01  WISP-BIT-CLEAR         %s PIC S9(9).",bin4_type);
		tput_line		("       01  WFOPEN-INPUT         %s PIC 9(9) VALUE %d.",bin4_type,OPEN_INPUT);
		tput_line		("       01  WFOPEN-SHARED        %s PIC 9(9) VALUE %d.",bin4_type,OPEN_SHARED);
		tput_line		("       01  WFOPEN-OUTPUT        %s PIC 9(9) VALUE %d.",bin4_type,OPEN_OUTPUT);
		tput_line		("       01  WFOPEN-EXTEND        %s PIC 9(9) VALUE %d.",bin4_type,OPEN_EXTEND);
		tput_line		("       01  WFOPEN-SPECIAL-INPUT %s PIC 9(9) VALUE %d.",bin4_type,OPEN_SPECIAL_INPUT);
		tput_line		("       01  WFOPEN-I-O           %s PIC 9(9) VALUE %d.",bin4_type,OPEN_I_O);
		tput_line		("       01  WFOPEN-SORT          %s PIC 9(9) VALUE %d.",bin4_type,OPEN_SORT);
		tput_line		("       01  WISP-OUTPUT  %s PIC 9(9) VALUE %ld.",bin4_type,IS_OUTPUT);
		tput_line		("       01  WISP-PRINT   %s PIC 9(9) VALUE %ld.",bin4_type,IS_PRINTFILE);
		tput_line		("       01  WISPSCRTCH   %s PIC 9(9) VALUE %ld.",bin4_type,IS_SCRATCH);
		tput_line		("       01  WISPSUBMIT   %s PIC 9(9) VALUE %ld.",bin4_type,IS_SUBMIT);
		tput_line		("       01  WISP-PROC    %s PIC 9(9) VALUE %ld.",bin4_type,IS_SUBMIT);
		tput_line		("       01  WISP-EXTEND  %s PIC 9(9) VALUE %ld.",bin4_type,IS_EXTEND);
		tput_line		("       01  WISP-I-O     %s PIC 9(9) VALUE %ld.",bin4_type,IS_IO);
		tput_line		("       01  WISP-SORT    %s PIC 9(9) VALUE %ld.",bin4_type,IS_SORT);
		tput_line		("       01  WISP-S-OUT   %s PIC 9(9) VALUE %ld.",bin4_type,IS_OUTPUT+IS_SORT);
		tput_line		("       01  WISP-LIB     %s PIC 9(9) VALUE %ld.",bin4_type,IS_OUTPUT+IS_LIB);
		tput_line		("       01  WISP-NORES   %s PIC 9(9) VALUE %ld.",bin4_type,IS_NORESPECIFY);
		tput_line		("       01  WISP-NOWRITE %s PIC 9(9) VALUE %ld.",bin4_type,IS_NOWRITE);
		tput_line		("       01  WISP-DECL    %s PIC 9(9) VALUE %ld.",bin4_type,IS_DECLARE);
		tput_line		("       01  WISP-FILE-TIMEOUT      %s PIC S9(9) VALUE 9999.",bin4_type);
	
		tput_line		("       01  WISP-RELATIVE-KEY         PIC 9(8).");
		tput_line		("       01  WISP-FILE-STATUS          PIC X(2).");
		tput_line		("       01  WISP-DECLARATIVES-STATUS  PIC X(2).");
		tput_line		("       01  WISP-DECLARATIVES-STATUS-ITEMS");
		tput_line		("           REDEFINES WISP-DECLARATIVES-STATUS.");
		tput_line		("           05  WISP-DECLARATIVES-STATUS-1 PIC X.");
		tput_line		("           05  WISP-DECLARATIVES-STATUS-2 PIC X.");
		if (vax_cobol)
		{
			tput_line	("       01  WISP-EXTENDED-FILE-STATUS-1 PIC S9(9)");
			tput_line	("           LEADING SEPARATE.");
			tput_line	("       01  WISP-EXTENDED-FILE-STATUS-2 PIC S9(9)");
			tput_line	("           LEADING SEPARATE.");
		}
		else
		{
			tput_line	("       01  WISP-EXTENDED-FILE-STATUS-1 PIC X(10).");
			tput_line	("       01  WISP-EXTENDED-FILE-STATUS-2 PIC X(10).");
		}
		tput_line		("       01  WISP-CURRENT-FILE-ID      PIC X(40).");
		tput_line		("       01  WISP-SAVE-FILE-STATUS     PIC X(2).");
		tput_line		("       01  WISP-PFKEY-VALUE          PIC 9(2).");
		tput_line		("       01  WISP-SAVE-WCC             PIC X.");
		tput_line		("       01  WISP-PROTECT-BIT          PIC X(1) VALUE DEC-BYTE-4.");
		tput_line		("       01  WISP-XERROR-BIT           PIC X(1) VALUE DEC-BYTE-8.");
		tput_line		("       01  WISP-ERROR-BIT            PIC X(1) VALUE DEC-BYTE-16.");
		tput_line		("       01  WISP-MOD-BIT              PIC X(1) VALUE DEC-BYTE-64.");
		tput_line		("       01  WISP-ALTERED-BIT          PIC X(1) VALUE DEC-BYTE-64.");
		tput_blank();
	
		tput_line		("       01  WISP-ALPHA-CONVERSION-FIELD     PIC X(18).");
		tput_line		("       01  WISP-CONVERTED-FRACTION-FIELD   PIC SV9(18)");
		tput_line		("                SIGN IS TRAILING.");
		tput_line		("       01  WISP-CONVERTED-INTEGER-FIELD    PIC S9(18)");
		tput_line		("                SIGN IS TRAILING.");
		tput_line		("       01  WISP-CONVERSION-LENGTH          PIC X(1) VALUE DEC-BYTE-18.");
	
		tput_line		("       01  WISP-CONVERSION-ERROR-FIELD %s PIC S9(9).",bin4_type);
	
		tput_line		("       01  WISP-NUMERIC-CONVERSION-FLAG    PIC X(1).");
		tput_line		("           88  WISP-NO-NUMERIC-CONV-ERROR VALUE \"N\".");
		tput_line		("           88  WISP-A-NUMERIC-CONV-ERROR  VALUE \"Y\".");
		tput_line		("       01  WISP-LONGWORD-ZEROS.");
		tput_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.");
		tput_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.");
		tput_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.");
		tput_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.");
		tput_blank();

		tput_scomment		("*****************************************************************");
		tput_scomment		("**             FIELDS USED FOR ON PFKEYS PFKEY-LIST      ********");
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
		tput_scomment		("*              Scratch fields created by WISP program      ******");
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
	
		tput_line		("       01  WISPRUNNAME               PIC X(8)  VALUE SPACES.");
	
		if (acu_cobol || mf_cobol)
		{
			tput_line	("       01  WISPRETURNCODE            PIC XXX   VALUE \"000\".");
			tput_line	("       01  W-RETURN-CODE REDEFINES WISPRETURNCODE PIC 999.");
			tput_blank();
	
			tput_line	("       01  WISPFILEXT                               PIC X(39).");
			tput_line	("       01  WISP-FILE-EXTENSION REDEFINES WISPFILEXT PIC X(39).");
			tput_blank();
		}
		else
		{
			tput_line	("       01  WISPRETURNCODE         EXTERNAL        PIC XXX.");
			tput_line	("       01  W-RETURN-CODE REDEFINES WISPRETURNCODE PIC 999.");
			tput_blank();

			tput_line	("       01  WISPFILEXT             EXTERNAL          PIC X(39).");
			tput_line	("       01  WISP-FILE-EXTENSION REDEFINES WISPFILEXT PIC X(39).");
			tput_blank();
		}
	}

	if (use_copy && output_copy)
	{
		release_output_stream();
		close_cob_file(cob_file_ptr);
	}
		

	tput_line		("       01  WISP-TRAN-VERSION PIC X(20) VALUE \"%s\".",WISP_VERSION);
	tput_line		("       01  WISP-LIB-VERSION          PIC X     VALUE DEC-BYTE-%d.",LIBRARY_VERSION);
	tput_line		("       01  WISP-COBOL-TYPE           PIC X(3)  VALUE \"%s\".",cobol_type);
	tput_line		("       01  WISP-APPLICATION-NAME     PIC X(8)  VALUE \"%-8s\".",prog_id);
	tput_line		("       01  WISP-ERR-LOGGING          PIC S9(9) %s VALUE 0.",bin4_type);
        if (swap_words)
		tput_line  	("       01  WISP-SWAP-ON              PIC S9(9) %s VALUE 1.",bin4_type);
	else
		tput_line  	("       01  WISP-SWAP-ON              PIC S9(9) %s VALUE 0.",bin4_type); 

	tput_scomment		("*****************************************************************");
	tput_scomment		("*              File name fields created by WISP program    ******");
	tput_scomment		("*****************************************************************");

	for (i=0; i<prog_cnt; i++)
	{
		make_fld(o_parms[0],prog_files[i],"N-");				/* put the N on			*/
		tput_line("       01  %s PIC X(80) VALUE SPACES.",o_parms[0]);

		if ((prog_fnames[i][0] == '\"') || (!prog_fnames[i][0]))		/* file name is a literal/null	*/
		{
			make_fld(o_parms[0],prog_files[i],"F-");			/* put the F on			*/
			if (prog_fnames[i][0])
			{
				tput_line("       01  %s PIC X(8) VALUE %s.",o_parms[0],prog_fnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				tput_line("       01  %s PIC X(8) VALUE SPACES.",o_parms[0]);
			}
		}
		if ((prog_lnames[i][0] == '\"') || (!prog_lnames[i][0]))		/* Library name is a literal/null */
		{
			make_fld(o_parms[0],prog_files[i],"L-");			/* put the L on			*/
			if (prog_lnames[i][0])
			{
				tput_line("       01  %s PIC X(8) VALUE %s.",o_parms[0],prog_lnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				tput_line("       01  %s PIC X(8) VALUE SPACES.",o_parms[0]);
			}
		}
		if ((prog_vnames[i][0] == '\"') || (!prog_vnames[i][0]))		/* Volume name is a literal/null */
		{
			make_fld(o_parms[0],prog_files[i],"V-");			/* put the V on			*/
			if (prog_vnames[i][0])
			{
				tput_line("       01  %s PIC X(6) VALUE %s.",o_parms[0],prog_vnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				tput_line("       01  %s PIC X(6) VALUE SPACES.",o_parms[0]);
			}
		}

		make_fld(o_parms[0],prog_files[i],"PR-");				/* build a prname field		*/
		tput_line("       01  %s PIC X(8) VALUE \"%-8s\".",o_parms[0],prog_prname[i]);

		make_fld(o_parms[0],prog_files[i],"S-");				/* build a status field		*/

		fval = IS_PRNAME;							/* Flag that Prnames are in now.*/

		if (prog_ftypes[i] & PRINTER_FILE)
		{									/* init flags as a print file	*/
			fval += IS_PRINTFILE;
		}

		if (prog_ftypes[i] & INDEXED_FILE)					/* Flag as indexed.		*/
		{
			fval += IS_INDEXED;
		}

		if (prog_ftypes[i] & SEQ_DYN)						/* File is SEQ/DYN.		*/
		{
			fval += IS_SEQDYN;
		}

		if ((prog_ftypes[i] & SEQ_FILE) && !(prog_ftypes[i] & SEQ_DYN))		/* File is SEQ/SEQ		*/
		{
			fval += IS_SEQSEQ;
		}

		if (prog_ftypes[i] & NORESPECIFY)					/* File is NORESPECIFY.		*/
		{
			fval += IS_NORESPECIFY;
		}

		if (prog_ftypes[i] & DBFILE_FILE)					/* File is DATABASE.		*/
		{
			fval += IS_DBFILE;
		}

		tput_line("       01  %s %s PIC 9(9) VALUE %ld.",o_parms[0],bin4_type,fval);
	}

	if (fig_count)
	{
		tput_scomment("*****************************************************************");
		tput_scomment("*   2 byte FIGURATIVE-CONSTANTS relocated by WISP program  ******");
		tput_scomment("*****************************************************************");
		tput_blank();
		for (i=0; i<fig_count; i++)
		{
			tput_line("       01  %s.",fig_cons[i]);
			tput_line("           05  FILLER PIC X VALUE DEC-BYTE-%d.",fig_val[i][0]);
			tput_line("           05  FILLER PIC X VALUE DEC-BYTE-%d.",fig_val[i][1]);
		}
	}

	tput_blank();
	tput_line("       01  WISP-FULL-SCREEN-ORDER-AREA.");
	tput_line("           03  WISP-IOCW-1           PIC X(1) VALUE DEC-BYTE-1.");
	tput_line("           03  WISP-IOCW-2           PIC X(1) VALUE WISP-SCWCC.");
	tput_line("           03  WISP-IOCW-3           PIC X(1) VALUE DEC-BYTE-0.");
	tput_line("           03  WISP-IOCW-4           PIC X(1) VALUE DEC-BYTE-0.");
	tput_blank();
	tput_line("       01  WISP-CRT-RECORD.");
	tput_line("           03  WISP-CRT-ORDER-AREA.");
	tput_line("               05  WISP-CRT-ORDER-AREA-1     PIC X VALUE ZERO.");
	tput_line("               05  WISP-CRT-ORDER-AREA-2     PIC X VALUE ZERO.");
	tput_line("               05  WISP-CRT-ORDER-AREA-3     PIC X VALUE ZERO.");
	tput_line("               05  WISP-CRT-ORDER-AREA-4     PIC X VALUE ZERO.");
	tput_line("           03  WISP-CRT-O-A REDEFINES WISP-CRT-ORDER-AREA PIC X(4).");
	tput_line("           03  WISP-CRT-SCREEN-AREA.");
	tput_line("               05  FILLER     PIC X(1920).");
	tput_line("           03  WISP-CRT-SCREEN-LINES REDEFINES WISP-CRT-SCREEN-AREA.");
	tput_line("               05  WISP-SCREEN-LINE PIC X(80) OCCURS 24 TIMES.");
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
