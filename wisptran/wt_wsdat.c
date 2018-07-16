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
ws_init()
{
	register int i;
	long	fval;
	char	COPY_WORKSTOR[40];
	int	use_copy, output_copy;

	use_copy = 0;									/* Use a "COPY" statement		*/
	output_copy = 0;								/* Write the copy code.			*/

	/*
						      Copybook number ( 001=figcon, 002=Work-Stor )
						      |COBOL (1=VAX, 2=ACUCOBOL etc)
						      || Version 
						      || |
	*/
	if      (vax_cobol) strcpy(COPY_WORKSTOR,"wc002105.cpy");
	else if (acu_cobol) strcpy(COPY_WORKSTOR,"wc002205.cpy");
	else if (lpi_cobol) strcpy(COPY_WORKSTOR,"wc002305.cpy");
	else if (aix_cobol) strcpy(COPY_WORKSTOR,"wc002405.cpy");
	else if (mf_cobol)  strcpy(COPY_WORKSTOR,"wc002505.cpy");
	else if (dmf_cobol) strcpy(COPY_WORKSTOR,"wc002605.cpy");
	else                strcpy(COPY_WORKSTOR,"wc002x05.cpy");

	put_line		("      *****************************************************************\n");
	put_line		("      *  Special WISP variables inserted by the translator program\n");
	write_line		("      *  Translated by WISP %s\n",WISP_VERSION);
	put_line		("      *****************************************************************\n");

	if ( open_files == 1 || concat )						/* If not writing to copy file		*/
	{
		write_line("000001     COPY \"%s\".\n",COPY_WORKSTOR);			/* Write the copy statement		*/
		use_copy = 1;
		if ( access(COPY_WORKSTOR,0) != 0 )					/* If copybook doesn't exist ...	*/
		{
			open_output(COPY_WORKSTOR);					/* open it and start writing.		*/
			output_copy = 1;
		}
	}
	else
	{
		output_copy = 1;							/* Just output the code to the file	*/
	}

	if (output_copy)
	{
		put_line		("      *****************************************************************\n");
		put_line		("      * WISP STANDARD WORKING STORAGE VARIABLES\n");
		put_line		("      *****************************************************************\n");
		put_line		("       01  VWANG-WRITE-ALL           PIC X VALUE DEC-BYTE-1.\n");
		put_line		("       01  VWANG-READ-ALL            PIC X VALUE DEC-BYTE-2.\n");
		put_line		("       01  VWANG-CLOSE-WS            PIC X VALUE DEC-BYTE-4.\n");
		put_line		("       01  VWANG-WRITE-SELECTED      PIC X VALUE DEC-BYTE-5.\n");
		put_line		("       01  VWANG-READ-ALTERED        PIC X VALUE DEC-BYTE-6.\n");
		put_line		("       01  VWANG-READ-MODIFIABLE     PIC X VALUE DEC-BYTE-10.\n");
		put_line		("       01  VWANG-DISP-AND-READ-ALT   PIC X VALUE DEC-BYTE-7.\n");
		put_line		("       01  VWANG-LOAD-SUB-TABLE      PIC X VALUE DEC-BYTE-8.\n");
		put_line		("       01  VWANG-INIT-DEFAULT-TABLE  PIC X VALUE DEC-BYTE-9.\n");
		put_line		("       01  VWANG-LINES               PIC X.\n");
		put_line		("       01  VWANG-FULL-SCREEN         PIC X VALUE DEC-BYTE-24.\n");
		put_line		("       01  WISP-SET-BYTE             PIC X.\n");
		put_line		("       01  WISP-TEST-BYTE            PIC X.\n");
		put_line		("       01  WISP-ALLOWABLE-PF-KEYS    PIC X(67).\n");
		put_line		("       01  WISP-ON-PF-KEYS           PIC X(67).\n");
		put_line		("       01  WISP-BIT-FLAG             PIC X(1).\n");
		put_line		("           88  WISP-FAC-BIT-IS-ON    VALUE \"Y\".\n");
		put_line		("           88  WISP-FAC-BIT-IS-OFF   VALUE \"N\".\n\n");
	
		put_line		("       01  WISP-LOCK-ID           PIC X(40) VALUE SPACES.\n");
		put_line		("       01  WISP-NEW-LOCK-ID       PIC X(40) VALUE SPACES.\n");

		put_line		("       01  WISP-BUFF-TABLE-16.\n");
		put_line		("           05  WISP-BUFF          PIC X(80) OCCURS 16.\n");

		write_line		("       01  WISP-LONGWORD          %s PIC S9(9).\n",bin4_type);
		write_line		("       01  WISP-LONGWORD-1        %s PIC S9(9).\n",bin4_type);
		write_line		("       01  WISP-LONGWORD-2        %s PIC S9(9).\n",bin4_type);
		write_line		("       01  WISP-BIT-SET           %s PIC S9(9).\n",bin4_type);
		write_line		("       01  WISP-BIT-CLEAR         %s PIC S9(9).\n",bin4_type);
		write_line		("       01  WFOPEN-INPUT         %s PIC 9(9) VALUE %d.\n",bin4_type,OPEN_INPUT);
		write_line		("       01  WFOPEN-SHARED        %s PIC 9(9) VALUE %d.\n",bin4_type,OPEN_SHARED);
		write_line		("       01  WFOPEN-OUTPUT        %s PIC 9(9) VALUE %d.\n",bin4_type,OPEN_OUTPUT);
		write_line		("       01  WFOPEN-EXTEND        %s PIC 9(9) VALUE %d.\n",bin4_type,OPEN_EXTEND);
		write_line		("       01  WFOPEN-SPECIAL-INPUT %s PIC 9(9) VALUE %d.\n",bin4_type,OPEN_SPECIAL_INPUT);
		write_line		("       01  WFOPEN-I-O           %s PIC 9(9) VALUE %d.\n",bin4_type,OPEN_I_O);
		write_line		("       01  WFOPEN-SORT          %s PIC 9(9) VALUE %d.\n",bin4_type,OPEN_SORT);
		write_line		("       01  WISP-OUTPUT  %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_OUTPUT);
		write_line		("       01  WISP-PRINT   %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_PRINTFILE);
		write_line		("       01  WISPSCRTCH   %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_SCRATCH);
		write_line		("       01  WISPSUBMIT   %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_SUBMIT);
		write_line		("       01  WISP-PROC    %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_SUBMIT);
		write_line		("       01  WISP-EXTEND  %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_EXTEND);
		write_line		("       01  WISP-I-O     %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_IO);
		write_line		("       01  WISP-SORT    %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_SORT);
		write_line		("       01  WISP-S-OUT   %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_OUTPUT+IS_SORT);
		write_line		("       01  WISP-LIB     %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_OUTPUT+IS_LIB);
		write_line		("       01  WISP-NORES   %s PIC 9(9) VALUE %ld.\n\n",bin4_type,IS_NORESPECIFY);
		write_line		("       01  WISP-NOWRITE %s PIC 9(9) VALUE %ld.\n",bin4_type,IS_NOWRITE);
		write_line		("       01  WISP-DECL    %s PIC 9(9) VALUE %ld.\n\n",bin4_type,IS_DECLARE);
		write_line		("       01  WISP-FILE-TIMEOUT      %s PIC S9(9) VALUE 9999.\n",bin4_type);
	
		put_line		("       01  WISP-RELATIVE-KEY         PIC 9(8).\n");
		put_line		("       01  WISP-FILE-STATUS          PIC X(2).\n");
		put_line		("       01  WISP-DECLARATIVES-STATUS  PIC X(2).\n");
		put_line		("       01  WISP-DECLARATIVES-STATUS-ITEMS\n");
		put_line		("           REDEFINES WISP-DECLARATIVES-STATUS.\n");
		put_line		("           05  WISP-DECLARATIVES-STATUS-1 PIC X.\n");
		put_line		("           05  WISP-DECLARATIVES-STATUS-2 PIC X.\n");
		put_line		("       01  WISP-EXTENDED-FILE-STATUS   PIC X(4).\n");
		put_line		("       01  WISP-EXTENDED-FILE-STATUS-1 PIC S9(9)\n");
		put_line		("           LEADING SEPARATE.\n");
		put_line		("       01  WISP-EXTENDED-FILE-STATUS-2 PIC S9(9)\n");
		put_line		("           LEADING SEPARATE.\n");
		put_line		("       01  WISP-CURRENT-FILE-ID      PIC X(40).\n");
		put_line		("       01  WISP-SAVE-FILE-STATUS     PIC X(2).\n");
		put_line		("       01  WISP-PFKEY-VALUE          PIC 9(2).\n");
		put_line		("       01  WISP-SAVE-WCC             PIC X.\n");
		put_line		("       01  WISP-ERROR-BIT            PIC X(1) VALUE DEC-BYTE-16.\n");
		put_line		("       01  WISP-MOD-BIT              PIC X(1) VALUE DEC-BYTE-64.\n");
		put_line		("       01  WISP-ALTERED-BIT          PIC X(1) VALUE DEC-BYTE-64.\n\n");
	
		put_line		("       01  WISP-ALPHA-CONVERSION-FIELD     PIC X(18).\n");
		put_line		("       01  WISP-CONVERTED-FRACTION-FIELD   PIC SV9(18)\n");
		put_line		("                SIGN IS TRAILING.\n");
		put_line		("       01  WISP-CONVERTED-INTEGER-FIELD    PIC S9(18)\n");
		put_line		("                SIGN IS TRAILING.\n");
		put_line		("       01  WISP-CONVERSION-LENGTH          PIC X(1) VALUE DEC-BYTE-18.\n");
	
		write_line		("       01  WISP-CONVERSION-ERROR-FIELD %s PIC S9(9).\n",bin4_type);
	
		put_line		("       01  WISP-NUMERIC-CONVERSION-FLAG    PIC X(1).\n");
		put_line		("           88  WISP-NO-NUMERIC-CONV-ERROR VALUE \"N\".\n");
		put_line		("           88  WISP-A-NUMERIC-CONV-ERROR  VALUE \"Y\".\n");
		put_line		("       01  WISP-LONGWORD-ZEROS.\n");
		put_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.\n");
		put_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.\n");
		put_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.\n");
		put_line		("           05  FILLER                     PIC X VALUE DEC-BYTE-0.\n\n");
		put_line		("      *****************************************************************\n");
		put_line		("      **             FIELDS USED FOR ON PFKEYS PFKEY-LIST      ********\n");
		put_line		("      *****************************************************************\n\n");
	
		put_line		("       01  WISP-DFIELD-0                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-1                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-2                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-3                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-4                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-5                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-6                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-7                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-8                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-9                   PIC 99.\n");
		put_line		("       01  WISP-DFIELD-10                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-11                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-12                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-13                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-14                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-15                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-16                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-17                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-18                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-19                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-20                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-21                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-22                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-23                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-24                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-25                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-26                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-27                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-28                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-29                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-30                  PIC 99.\n");
		put_line		("       01  WISP-DFIELD-31                  PIC 99.\n\n");
	
		put_line		("      *****************************************************************\n");
		put_line		("      *              Scratch fields created by WISP program      ******\n");
		put_line		("      *****************************************************************\n\n");
			
		put_line		("       01  WISP-SCRATCH-BYTE-1            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-1-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-2            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-2-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-3            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-3-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-4            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-4-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-5            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-5-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-6            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-6-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-7            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-7-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-8            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-8-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-9            PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-9-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-10           PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-10-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-11           PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-11-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-12           PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-12-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-13           PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-13-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-14           PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-14-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-15           PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-15-TRUE   VALUE \"Y\".\n");
		put_line		("       01  WISP-SCRATCH-BYTE-16           PIC X.\n");
		put_line		("           88  WISP-SCRATCH-BYTE-16-TRUE   VALUE \"Y\".\n\n");
	
		put_line		("       01  WISPRUNNAME               PIC X(8)  VALUE SPACES.\n");
	
		if (acu_cobol || mf_cobol)
		{
			put_line	("       01  WISPRETURNCODE            PIC XXX   VALUE \"000\".\n");
			put_line	("       01  W-RETURN-CODE REDEFINES WISPRETURNCODE PIC 999.\n\n");
	
			put_line	("       01  WISPFILEXT                               PIC X(39).\n");
			put_line	("       01  WISP-FILE-EXTENSION REDEFINES WISPFILEXT PIC X(39).\n\n");
		}
		else
		{
			put_line	("       01  WISPRETURNCODE         EXTERNAL        PIC XXX.\n");
			put_line	("       01  W-RETURN-CODE REDEFINES WISPRETURNCODE PIC 999.\n\n");
	
			put_line	("       01  WISPFILEXT             EXTERNAL          PIC X(39).\n");
			put_line	("       01  WISP-FILE-EXTENSION REDEFINES WISPFILEXT PIC X(39).\n\n");
		}
	}

	if (use_copy && output_copy) close_output();
		

	write_line		("       01  WISP-TRAN-VERSION PIC X(20) VALUE \"%s\".\n",WISP_VERSION);
	write_line		("       01  WISP-LIB-VERSION          PIC X     VALUE DEC-BYTE-%d.\n",LIBRARY_VERSION);
	write_line		("       01  WISP-COBOL-TYPE           PIC X(3)  VALUE \"%s\".\n",cobol_type);
	write_line		("       01  WISP-APPLICATION-NAME     PIC X(8)  VALUE \"%-8s\".\n",prog_id);
	write_line		("       01  WISP-ERR-LOGGING          PIC S9(9) %s VALUE 0.\n",bin4_type);
        if (swap_words)
		write_line  	("       01  WISP-SWAP-ON              PIC S9(9) %s VALUE 1.\n",bin4_type);
	else
		write_line  	("       01  WISP-SWAP-ON              PIC S9(9) %s VALUE 0.\n",bin4_type); 

	put_line		("      *****************************************************************\n");
	put_line		("      *              File name fields created by WISP program    ******\n");
	put_line		("      *****************************************************************\n\n");

	for (i=0; i<prog_cnt; i++)
	{
		make_fld(o_parms[0],prog_files[i],"N-");				/* put the N on			*/
		write_line("       01  %s PIC X(80) VALUE SPACES.\n",o_parms[0]);

		if ((prog_fnames[i][0] == '\042') || (!prog_fnames[i][0]))		/* file name is a literal/null	*/
		{
			make_fld(o_parms[0],prog_files[i],"F-");			/* put the F on			*/
			if (prog_fnames[i][0])
			{
				write_line("       01  %s PIC X(8) VALUE %s.\n",o_parms[0],prog_fnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				write_line("       01  %s PIC X(8) VALUE SPACES.\n",o_parms[0]);
			}
		}
		if ((prog_lnames[i][0] == '\042') || (!prog_lnames[i][0]))		/* Library name is a literal/null */
		{
			make_fld(o_parms[0],prog_files[i],"L-");			/* put the L on			*/
			if (prog_lnames[i][0])
			{
				write_line("       01  %s PIC X(8) VALUE %s.\n",o_parms[0],prog_lnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				write_line("       01  %s PIC X(8) VALUE SPACES.\n",o_parms[0]);
			}
		}
		if ((prog_vnames[i][0] == '\042') || (!prog_vnames[i][0]))		/* Volume name is a literal/null */
		{
			make_fld(o_parms[0],prog_files[i],"V-");			/* put the V on			*/
			if (prog_vnames[i][0])
			{
				write_line("       01  %s PIC X(6) VALUE %s.\n",o_parms[0],prog_vnames[i]);
			}
			else								/* it was a null, space it out	*/
			{
				write_line("       01  %s PIC X(6) VALUE SPACES.\n",o_parms[0]);
			}
		}

		make_fld(o_parms[0],prog_files[i],"PR-");				/* build a prname field		*/
		write_line("       01  %s PIC X(8) VALUE \"%-8s\".\n",o_parms[0],prog_prname[i]);

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

		write_line("       01  %s %s PIC 9(9) VALUE %ld.\n",o_parms[0],bin4_type,fval);
	}

	if (fig_count)
	{
		put_line("      *****************************************************************\n");
		put_line("      *   2 byte FIGURATIVE-CONSTANTS relocated by WISP program  ******\n");
		put_line("      *****************************************************************\n\n");
		for (i=0; i<fig_count; i++)
		{
			write_line("       01  %s.\n",fig_cons[i]);
			write_line("           05  FILLER PIC X VALUE DEC-BYTE-%d.\n",fig_val[i][0]);
			write_line("           05  FILLER PIC X VALUE DEC-BYTE-%d.\n",fig_val[i][1]);
		}
	}

	put_char('\n');
	put_line("       01  WISP-FULL-SCREEN-ORDER-AREA.\n\n");
	put_line("           03  WISP-IOCW-1           PIC X(1) VALUE DEC-BYTE-1.\n");
	put_line("           03  WISP-IOCW-2           PIC X(1) VALUE WISP-SCWCC.\n");
	put_line("           03  WISP-IOCW-3           PIC X(1) VALUE DEC-BYTE-0.\n");
	put_line("           03  WISP-IOCW-4           PIC X(1) VALUE DEC-BYTE-0.\n\n");
	put_line("       01  WISP-CRT-RECORD.\n\n");
	put_line("           03  WISP-CRT-ORDER-AREA.\n\n");
	put_line("               05  WISP-CRT-ORDER-AREA-1     PIC X VALUE ZERO.\n");
	put_line("               05  WISP-CRT-ORDER-AREA-2     PIC X VALUE ZERO.\n");
	put_line("               05  WISP-CRT-ORDER-AREA-3     PIC X VALUE ZERO.\n");
	put_line("               05  WISP-CRT-ORDER-AREA-4     PIC X VALUE ZERO.\n\n");
	put_line("           03  WISP-CRT-O-A REDEFINES WISP-CRT-ORDER-AREA PIC X(4).\n\n");
	put_line("           03  WISP-CRT-SCREEN-AREA.\n\n");
	put_line("               05  FILLER     PIC X(1920).\n\n");
	put_line("           03  WISP-CRT-SCREEN-LINES REDEFINES WISP-CRT-SCREEN-AREA.\n");
	put_line("               05  WISP-SCREEN-LINE PIC X(80) OCCURS 24 TIMES.\n\n");

	if (crt_name[0])								/* if there was a crt file...	*/
	{
		copy_file(crt_name);							/* copy the crt definitions	*/
		delete(crt_name);							/* now remove the file		*/
	}

}


