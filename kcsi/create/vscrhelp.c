static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
#include "vscrglb.h"
#include "wispscr.h"
#include "kcsifunc.h"

static char sccs_id[]="@(#)vscrhelp.c	1.1 8/15/93";

WISPSCR_FLDS(hof) = {
{LEN(0)	ROW(4)	COL(3)	VALUE("Some Helpful Information")},
{LASTITEM}};

static char hofpfs[]="00X";
static char status[3];
static int crt_row, crt_col;


static char *name_and_type_help_text[]={
"TYPE     - File type options are:",
"           A  = Alternate indexed",
"           I  = Indexed records",
"           R  = Relative records",
"           C  = Consecutive (Line Sequential) records",
"           B  = Biniary Sequential records",
" ",
"RECSIZE  - Can be up to 2048 bytes",
" ",
#ifdef KCSI_ACU
"FORMAT   - Vision Version for Indexed files:",
"           0  = Default",
"           2  = Vision 2",
"           3  = Vision 3",
"           4  = Vision 4",
#endif
#ifdef KCSI_MF
"FORMAT   - Indexed file format: 0  = Default",
"                                1  = C-ISAM",
"                                2  = LEVEL II COBOL",
"                                3  = Format used by system",
"                                4  = IDXFORMAT 4",
#endif
NULL};

static void cr_help(char **text);


void cr_name_and_type_help(void)
{
	cr_help(name_and_type_help_text);
}

static char *primary_key_help_text[]={
"KEYPOS   - Starting position of the primary key in the record",
"KEYLEN   - Length of the key",
" ",
#ifdef	SPLITKEYS
"Systems that support split keys, allow the entry of the values",
"KEYPOS = SPLIT, KEYLEN = KEY.",
"If this entry is accepted you will be prompted for up to eight",
"positions and lengths for the parts of the the key.",
#endif
NULL};


void cr_primary_key_help(void)
{
	cr_help(primary_key_help_text);
}

static char *split_help_text[]={
"Systems that allow split keys, provide for up to eight pairs of",
"positions and lengths that can be used to define the parts of a",
"single key.",
NULL};

void cr_split_help(void)
{
	cr_help(split_help_text);
}

static char *alt_keys_help_text[]={
"An alternate key is identified by specifying the starting position,",
"length and duplicates flag for each of up to 16 alternate keys.",
" ",
#ifdef SPLITKEYS
"Systems that support split keys, will alow an entry of",
"KEYPOSnn = SPLIT and KEYLENnn = KEY.",
"When split keys are allowed and specified, you will be prompted",
"for up to 8 pairs of positions and lengths that make up the parts of the",
"split key.",
#endif
NULL};

void cr_alt_keys_help(void)
{
	cr_help(alt_keys_help_text);
}


static char *file_field_help_text[]={
"1) The field to extract from the input file is identified by its starting",
"   position in the input record (INPOS), its starting location in the output",
"   record (OUTPOS), and its LENGTH.",
"2) The range of records to be used is specified as follows:",
"   a) Consecutive - range is specified by START, END, and INCRMENT. START and",
"      or relative   END must each be valid record numbers or \"FIRST\" and/or",
"        file        \"LAST\". INCRMENT may be any nonzero value.",
"   b) Indexed     - range is specified by START and END primary key values,",
"        file        which may be specified in ASCII or hex.",
"3) If COMPARE is nonblank, a particular field in the input record will be",
"   compared (EQ, NE, GT, LT, GE or LE) to the user-specified value",
"   (CSTRING) to decide whether or not to include the record. CLENGTH",
"   defaults to the nonblank CSTRING length. It may be set set explicitly",
"   by the user for ASCII values, but not for hex values.",
NULL
};
void cr_file_field_help(void)
{
	cr_help(file_field_help_text);
}

static void cr_help(char **text)
{
	int row,idx;


	wpload();
	GPSETUP();
	GPSTD("FILEINFO","CREATE");

	for(row = 9, idx = 0; (row < 24) && (text[idx]) ; ++idx, ++row)
		{
		GPCTEXT(text[idx],row,2);
		}
	GPCTEXT("Press (ENTER) to return",24,2);
	display_and_read_gp();

}

/*
**	History:
**	$Log: vscrhelp.c,v $
**	Revision 1.5  1998-05-19 10:02:57-04  gsl
**	Add the FORMAT field to the help text
**
**	Revision 1.4  1996-10-02 20:11:58-04  gsl
**	fixed pfkey tags for w4w suport
**
**	Revision 1.3  1996-10-02 09:10:31-07  gsl
**	Add standard headers
**	Fix prototypes
**
**
**
*/
