/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

#ifndef	_VSEGLB_H
#define	_VSEGLB_H
#include "vsegp.h"
#include "intdef.h"

#define VSE_COPYRIGHT	vse_copyright_message

/*----
Language defaults
------*/

#define	NO_OF_TABS		10

#define	VSE_LANGUAGE_LEN	9

#define	COBOL_LANGUAGE		"COBOL    "
#define	C_LANGUAGE		"C        "
#define	TEXT_LANGUAGE		"TEXT     "
#define	SHELL_LANGUAGE		"SHELL    "
#define BASIC_LANGUAGE		"BASIC    "
#define PROC_LANGUAGE		"PROCEDURE"

#define LANG_NONE	0
#define LANG_COBOL	1
#define LANG_BASIC	2
#define LANG_PROC	3
#define LANG_SHELL	4
#define LANG_C		5

#define TAB_TO_FIELD 	0
#define TAB_NORMAL 	1

#define	ALL_PFS			"000102030405060708091011121314151617181920212223242526272829303132X"

#define OA_NUM	0
#define OA_WCC	1
#define OA_COL	2
#define OA_CURSOR_ROW	3

/*----
Setup so that these are defined in vseglb.c but declared everywhere
else
------*/

#ifdef	EXTERN_DEF 
#undef	EXTERN_DEF
#endif

#ifdef	_VSEGLB_C
#define	EXTERN_DEF	
#define	INIT_DEF_OA	={0x01,0xa0,0x00,0x00}
#define	INIT_LOCKED_OA	={0x01,0x00,0x00,0x00}
#define	INIT_100	=100L
#define	INIT_3_SPACES	="   "
#define	INIT_8_SPACES	="        "
#define	INIT_6_SPACES	="      "
#define	INIT_16_SPACES	="                "
#define	INIT_80_SPACES	="                                                                                "
#define	INIT_100_STRING	="100   "
#define	INIT_ALL_STRING	="ALL             "
#define	INIT_YES	="YES"
#define	INIT_NO		="NO "
#define	INIT_1		=1L
#define	INIT_ZERO	=0L
#define	INIT_15		=15
#define INIT_MINUS_ONE  = -1
#else
#define	EXTERN_DEF	extern
#define	INIT_DEF_OA
#define	INIT_LOCKED_OA
#define	INIT_100		
#define	INIT_3_SPACES
#define	INIT_8_SPACES
#define	INIT_6_SPACES
#define	INIT_16_SPACES
#define	INIT_39_SPACES
#define	INIT_80_SPACES
#define	INIT_100_STRING
#define	INIT_ALL_STRING
#define	INIT_YES
#define	INIT_NO
#define	INIT_ZERO
#define	INIT_1
#define	INIT_15
#define INIT_MINUS_ONE
#endif	/*_VSEGLB_C */


EXTERN_DEF	char	vse_copyright_message[81];				/* The global copyright message 		*/

/*----
Globals for vse. Includes default parameters etc.
Eventually some of these will be modifiable by getparms or
some other method.

Some getparms have defaults that are only valid as Compile or Link
Options, these are included in the globals, but will have no
validity and are not issued as part of the getparms.
(unless this program is modified to include a compiel and
link option which is not intended for now)
------*/

/*----
INPUT GETPARM

Does not use output options
------*/
#define	VSE_FILENAME_LEN	8
#define	VSE_LIBNAME_LEN		8
#define	VSE_VOLNAME_LEN		6

EXTERN_DEF 	char vse_gp_input_language[VSE_LANGUAGE_LEN+1];
EXTERN_DEF	char vse_gp_input_file[VSE_FILENAME_LEN+1]	INIT_8_SPACES;
EXTERN_DEF	char vse_gp_input_library[VSE_LIBNAME_LEN+1]	INIT_8_SPACES;
EXTERN_DEF	char vse_gp_input_volume[VSE_VOLNAME_LEN+1]	INIT_6_SPACES;

/*----
The actual resulting system name
------*/

#define	VSE_SYSNAME_LEN	81	

EXTERN_DEF	char vse_sysname[VSE_SYSNAME_LEN+1]	INIT_80_SPACES;

/*----
RENUMBER GETPARM
------*/
EXTERN_DEF	char vse_gp_renumber_number[]	INIT_100_STRING;
EXTERN_DEF	char vse_gp_renumber_incr[]	INIT_100_STRING;
EXTERN_DEF	char vse_gp_renumber_start[]	INIT_ALL_STRING;
EXTERN_DEF	char vse_gp_renumber_end[]	INIT_16_SPACES;

/*----
REPLACE GETPARM

COMPRESS (output file) is not valid but included as a possible way of
indicating that a file should be hard-tabbed.
Not currently used
------*/
EXTERN_DEF	char vse_gp_replace_number[]	INIT_YES;
EXTERN_DEF 	char vse_gp_replace_compress[]	INIT_NO;

/*----
OPTIONS GETPARM
Does not use SCRATCH (scratch listing and OBJ before compile)
Does not use REPLACE (replace SOURCE with edited text before compile)
------*/
EXTERN_DEF	char vse_gp_options_scratch[]	INIT_YES;
EXTERN_DEF	char vse_gp_options_replace[]	INIT_NO;
EXTERN_DEF	char vse_gp_options_renumber[]	INIT_NO;
EXTERN_DEF	char vse_gp_options_number[]	INIT_100_STRING;
EXTERN_DEF	char vse_gp_options_incr[]	INIT_100_STRING;
EXTERN_DEF	char vse_gp_options_modcode[]	INIT_8_SPACES;

/*----
DEFAULTS GETPARM
------*/
#define	TABS_LEN	(NO_OF_TABS * 3)
#define	MODE_LEN	5
#define	CASE_LEN	5
#define	COLS_LEN	5

EXTERN_DEF	char vse_gp_defaults_tabs[TABS_LEN+1];
EXTERN_DEF	char vse_gp_defaults_mode[MODE_LEN+1];
EXTERN_DEF	char vse_gp_defaults_case[CASE_LEN+1];
EXTERN_DEF	char vse_gp_defaults_columns[COLS_LEN+1];
EXTERN_DEF	char vse_gp_defaults_showmods[4] INIT_NO;

/*----
C versions of the numerics
------*/
EXTERN_DEF	int4 vse_renumber_number	INIT_100;
EXTERN_DEF	int4 vse_renumber_incr		INIT_100;
EXTERN_DEF	int4 vse_renumber_start		INIT_ZERO;
EXTERN_DEF	int4 vse_renumber_end		INIT_ZERO;


EXTERN_DEF	int4 vse_tab[NO_OF_TABS];

EXTERN_DEF	int4 vse_column[2];

EXTERN_DEF	int4 vse_append;

EXTERN_DEF	int4 vse_options_number		INIT_100;
EXTERN_DEF	int4 vse_options_incr		INIT_100;

/*----
Some switches that control program behaviour
------*/
EXTERN_DEF	int4 vse_native			INIT_ZERO;
EXTERN_DEF	int4 vse_new_file		INIT_ZERO;
EXTERN_DEF	int4 vse_page_size		INIT_15;

#define VSE_FIRST_SCREEN_COL	9
#define VSE_FIRST_SCREEN_ROW	5

#define VSE_BOTTOM_ROWS		5
#define VSE_EDIT_ROWS		20
#define VSE_SCREEN_WIDTH	80

#define VSE_NUM_WIDTH		6

EXTERN_DEF	int4 vse_line_width;		/* Total expected line width		*/

EXTERN_DEF	int4 vse_edit_start_col;	/* The first modifiable column 		*/
EXTERN_DEF	int4 vse_edit_end_col;		/* The last modifiable column		*/
EXTERN_DEF	int4 vse_edit_width;		/* The number of modifiable columns	*/

EXTERN_DEF	int4 vse_mod_start_col;		/* The first modcode column 		*/
EXTERN_DEF	int4 vse_mod_end_col;		/* The last modcode column		*/
EXTERN_DEF	int4 vse_mod_width;		/* The number of modcodes columns	*/

EXTERN_DEF	int4 vse_num_start_col;		/* The first number column		*/

/*----
Globals used in getparms
------*/

EXTERN_DEF int4 GPINT[255];
EXTERN_DEF int4 gpcnt;
EXTERN_DEF char* gparg[500];
EXTERN_DEF int4 gppfkeys;
EXTERN_DEF int4 gppfkey;
EXTERN_DEF char gppfrcvr[1];

/*----
The result codes of each getparm
------*/

EXTERN_DEF	int4	vse_input_pick;
EXTERN_DEF	int4	vse_edit_pick;
EXTERN_DEF	int4	vse_special_pick;
EXTERN_DEF	int4	vse_set_command_pick;


/*----
The pointer structure used for loading up a file
------*/
typedef struct _text{
	struct _text *next;
	struct _text *prev;
	char *text;
	char *modfld;
	int4	lineno;
	} TEXT;

EXTERN_DEF	TEXT	*text_first;
EXTERN_DEF	TEXT	*text_last;
EXTERN_DEF	TEXT	*scr_first;

/*
Pointer structure used to keep track of embedded line numbers in BASIC
*/
typedef struct _line_num
{
	struct _line_num *last;
	TEXT *line;
	char start_state;
	int start_pos;
	int length;
	int4 branch;
	int4 old_branch;
	struct _line_num *next;
} line_num;

/*---
Some shorthand
------*/

#define	CLEAR_FIELD(x)	{memset(x,' ',sizeof(x));}
#define	CLEAR_STRING(x)	{memset(x,' ',sizeof(x)); x[sizeof(x)-1]=(char)0;}
#define	NULL_FIELD(x)	{memset(x,(char)0,sizeof(x));}

/*----
More bloody globals
------*/

#define RESP_NUMBER 	1
#define RESP_INCR 	2
#define RESP_START 	3
#define RESP_END 	4
#define RESP_NUMINCR	5
#define RESP_RANGE	6
#define RESP_EMPTY	7
#define RESP_TARGET	8
#define RESP_TABS	9
#define RESP_MODE	10
#define RESP_CASE	11
#define RESP_SHOWMODS	12
#define RESP_RENUMBER	13
#define RESP_KEEPMC	14

#define RENUM_INVAL_INCR -1
#define RENUM_BAD_RANGE -2

#define	EDIT_MENU	0L
#define	SPECIAL_MENU	1L

EXTERN_DEF	int4 vse_menu;
EXTERN_DEF	unsigned char vse_default_oa[4]	INIT_DEF_OA;
EXTERN_DEF	char vse_locked_oa[4]	INIT_LOCKED_OA;
EXTERN_DEF	int4 vse_lines;
EXTERN_DEF	char vse_lines_message[81];
EXTERN_DEF	char vse_file_message[81];

EXTERN_DEF char vse_stat_message[81] INIT_80_SPACES;

EXTERN_DEF unsigned char vse_tab_setting[81];

EXTERN_DEF int4 vse_save_row INIT_MINUS_ONE;
EXTERN_DEF int4 vse_save_col INIT_MINUS_ONE;

EXTERN_DEF int4 show_col_flag INIT_ZERO;

EXTERN_DEF int4 vse_file_changed INIT_ZERO;

#endif	/*_VSEGLB_H */


/*
**	History:
**	$Log: vseglb.h,v $
**	Revision 1.14  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.13  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.12  2002/08/01 15:57:50  gsl
**	type warnings
**	
**	Revision 1.11  1996/09/03 22:24:05  gsl
**	drcs update
**	
**
**
*/
