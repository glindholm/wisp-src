			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* WISP common variable definition file, EXT is defined to be "extern" for the wangutils, and blank for MAIN  */

#include <ctype.h>
#include <stdio.h>

#ifndef NOSTDLIB
#include <stdlib.h>
#endif
#ifndef VMS	/* unix or MSDOS */
#include <malloc.h>
#endif
#ifdef MSDOS
#include <io.h>
#else	/* VMS or unix */
extern char *mktemp();
#endif

#ifdef INIT_COMMON

#define INIT_TRUE	= 1
#define INIT_FALSE	= 0 
#define INIT_ZERO	= 0 
#define INIT_NULL	= NULL
#define INIT_NULLSTR	= { 0 }

#else

#define INIT_TRUE
#define INIT_FALSE
#define INIT_ZERO
#define INIT_NULL
#define INIT_NULLSTR

#endif

#define EXIT_OK			0
#define EXIT_WITH_ERR		1
#define EXIT_FAST		2
#define EXIT_AND_RUN		-2							/* Exit WISP and re run it.		*/

EXT char prog_id[16];								/* the program id of this program		*/

EXT char inline[256];								/* The current input line.			*/
EXT char templine[256];

EXT int re_copy	   INIT_FALSE;							/* Is this the scond time for this copy lib.	*/
EXT int cpy_seq    INIT_FALSE;							/* Copy lib sequence counter.			*/

#define MAX_NAME_LIST		500

EXT char name_list[MAX_NAME_LIST][100];							/* A list of names, used as scratch.	*/
EXT int name_count INIT_FALSE;								/* The number in the list.		*/

#define MAX_PARAGRAPHS		512							/* How many paragraphs can be copied.	*/

EXT int  proc_paras_cnt		INIT_FALSE;						/* How many were read.			*/
EXT int  decl_paras_cnt		INIT_FALSE;						/* How many paragraphs were in decl.	*/
EXT int  decl_performs_cnt	INIT_FALSE;						/* How many performs' were in decl.	*/
EXT int  proc_performs_cnt	INIT_FALSE;						/* How many performs from the proc div	*/

EXT char proc_paras[MAX_PARAGRAPHS][40];						/* The list.				*/
EXT char decl_paras[MAX_PARAGRAPHS][40];						/* The names in the declaratives.	*/
EXT char decl_performs[MAX_PARAGRAPHS][40];						/* The performs in the Declaratives.	*/
EXT char proc_performs[MAX_PARAGRAPHS][40];						/* The performs from the proc div.	*/

EXT int  blank_count	INIT_FALSE;							/* How many items to blank.		*/
EXT int  nfs_count	INIT_FALSE;							/* How many SELECTs to skip file status	*/
EXT int  sf_count	INIT_FALSE;							/* How many SORT files.			*/

#define MAX_STATUS_ITEMS	32							/* The max to allow.			*/
#define MAX_SF_ITEMS		32

EXT char sf_item[MAX_SF_ITEMS][40];							/* The identified SORT files.		*/
EXT char nfs_item[MAX_STATUS_ITEMS][40];						/* The list of status's to skip.	*/

#define MAX_BLANK_ITEMS		100
EXT char blank_item[MAX_BLANK_ITEMS][40];						/* Screen items to be blank when zero.	*/

#define MAX_FIGCONS2		255							/* Maximum nuber of 2 byte fig cons.	*/

EXT char fig_cons[MAX_FIGCONS2][32];							/* names of figcons that are 2 bytes	*/
EXT int  fig_val [MAX_FIGCONS2][2];							/* their values				*/
EXT int  fig_count INIT_FALSE;								/* the counter				*/

EXT int  dump_ifbuff	INIT_FALSE;							/* Currently dumping the IF buffer.	*/
EXT int  delrecon	INIT_FALSE;							/* Should we just delete RECORD CONTAINS*/
EXT int  do_xref	INIT_FALSE;							/* Keep cross ref information.		*/
EXT int  do_dlink	INIT_FALSE;							/* Do VMS dynamic link.			*/
EXT int  nowarnings	INIT_FALSE;							/* Don't show WARNINGS messages.	*/
EXT int  swap_words	INIT_TRUE;							/* We have to swap words when asked.	*/
EXT int	 use_optional	INIT_TRUE;							/* Use the OPTIONAL phrase on SEQ-SEQ.	*/

struct c_list
	{
		int call_count;								/* How many calls in this struct.	*/
		char call_list[100][40];						/* The list of calls.			*/
		int ref_count[100];							/* How many times are they referenced.	*/
		struct c_list *next_list;						/* Pointer to the next list.		*/
	};

typedef struct c_list c_list;
EXT c_list *xref_ptr;									/* Pointer to cross ref list.		*/

#define STRING_BUFFER_SIZE 4096

EXT char pf_str[STRING_BUFFER_SIZE];							/* Buffer for ON PFKEY phrases		*/
EXT char nm_str[STRING_BUFFER_SIZE];							/* Buffer for NO-MOD phrases		*/

EXT int in_decl		INIT_FALSE;							/* Are we in the declaratives section?	*/
EXT int has_lit		INIT_FALSE;							/* Does the current line have a literal?*/
EXT int isalit		INIT_FALSE;							/* Is the current input open literal?	*/
EXT int has_cont	INIT_FALSE;							/* Does this line have a continuation?	*/
EXT int ws_blank	INIT_TRUE;							/* Allow BLANK WHEN ZERO in WS.		*/
EXT int trans_lib	INIT_FALSE;							/* Don't translate library names.	*/
EXT int decl_stop_exit	INIT_FALSE;							/* STOP RUN or EXIT PROGRAM in DECLAR	*/

EXT int g_newline	INIT_FALSE;							/* Last get_parm() read a newline.	*/

EXT int division INIT_FALSE;								/* Which division are we scanning?	*/

#define IDENTIFICATION_DIVISION		0
#define ENVIRONMENT_DIVISION		1
#define INPUT_OUTPUT_SECTION		2
#define DATA_DIVISION			3
#define WORKING_STORAGE_SECTION		4
#define PROCEDURE_DIVISION		5

EXT int pmode 		INIT_FALSE;							/* Parsing mode (if any)		*/
#define DISPLAY		3
EXT int copy_sect 	INIT_FALSE;							/* SECTION copy mode.			*/
EXT int copy_to_dcl_file 	INIT_FALSE;						/* Paragraph copying mode.		*/
EXT int copy_to_dtp_file 	INIT_FALSE;						/* Declarative copy mode.		*/
EXT int ptype;										/* the parm type (first last middle)	*/
EXT int kwproc    	INIT_TRUE;							/* flag to stop keyword processing.	*/
EXT int d_period  	INIT_FALSE;							/* flag to indicate a period is found	*/
EXT int pfkeys	  	INIT_FALSE;							/* flag to indicate PFKEYS phrase	*/
EXT int no_mod    	INIT_FALSE;							/* flag to indicate a NO-MOD		*/
EXT int on_pfkeys 	INIT_FALSE;							/* flag to indicate an ON PFKEYS	*/
EXT int keepstop  	INIT_FALSE;							/* Keep STOP RUN statements.		*/
EXT int changestop	INIT_FALSE;							/* Change STOP RUN to EXIT PROGRAM	*/
EXT int trap_start	INIT_FALSE;							/* Trap START timeouts.			*/


/*						Common variables								*/

EXT int do_optfile INIT_FALSE;								/* Flag to process the option file.	*/
#define MAX_PARMS 	36
EXT int 	p_parms[MAX_PARMS];							/* the offset of the parms in the line	*/
EXT char 	parms  [MAX_PARMS] [81];						/* 36 parms				*/
EXT char 	o_parms[MAX_PARMS] [81];						/* 36 output parms in this statement	*/

EXT char area_a[8];									/* The text found in area a		*/
EXT int area_a_num;									/* The number it represents (if any)	*/

EXT int blanklines 	INIT_FALSE;							/* Blank lines are no no's		*/
EXT int comments 	INIT_FALSE;							/* Comments are not ok			*/
EXT int compress 	INIT_TRUE;							/* Compress screen output		*/
EXT int copylib 	INIT_FALSE;							/* Generate copy libs			*/
EXT int copy_only 	INIT_FALSE;							/* don't just copy it, process it.	*/
EXT int logging 	INIT_FALSE;							/* no logging by default		*/
EXT int log_stats 	INIT_FALSE;							/* no stats either.			*/
EXT int data_conv	INIT_FALSE;							/* Default to not data conversion	*/

EXT int  vax_cobol 	INIT_FALSE;							/* VAX COBOL				*/
EXT int  lpi_cobol 	INIT_FALSE;							/* LPI COBOL				*/
EXT int  acu_cobol 	INIT_FALSE;							/* ACUCOBOL 				*/
EXT int  aix_cobol 	INIT_FALSE;							/* AIX VS COBOL 			*/
EXT int  mf_cobol 	INIT_FALSE;							/* Micro Focus COBOL 			*/
EXT int  dmf_cobol 	INIT_FALSE;							/* Micro Focus MSDOS COBOL 		*/
EXT int  unix_cobol 	INIT_FALSE;							/* Any UNIX COBOL 			*/
EXT int  dos_cobol 	INIT_FALSE;							/* Any DOS COBOL 			*/
EXT int  mf_aix 	INIT_FALSE;							/* Micro Focus (or AIX) COBOL 		*/
EXT char cobol_type[4];									/* The type of COBOL			*/

EXT int init_move INIT_FALSE;								/* don't change move spaces to initial	*/
EXT int init_data INIT_TRUE;								/* program id is initial.		*/
EXT int do_locking INIT_TRUE;								/* generate record locking logic	*/
EXT int del_use    INIT_FALSE;								/* not deleting a USE procedure.	*/
EXT int fdfiller   INIT_TRUE;								/* allow fillers in fd's		*/
EXT int wsfiller   INIT_TRUE;								/* allow fillers in working-storage	*/
EXT int do_keyfile INIT_FALSE;								/* Don't look for key files.		*/
EXT int proc_display INIT_TRUE;								/* Process DISPLAY statements.		*/

EXT int lock_clear_para INIT_FALSE;							/* Gen WISP-RECORD-LOCK-CLEAR para	*/

EXT char hard_lock[5];									/* The file status for hard lock	*/
EXT char soft_lock[5];									/* The file status for soft lock	*/
EXT char bin2_type[8];									/* The thing to convert BINARY to.	*/
EXT char bin4_type[8];									/* The thing to create 4 byte binary.	*/
EXT char packdec[8]; 									/* The thing to convert COMP to		*/

EXT int  wrote_special_names INIT_FALSE;						/* Was SPECIAL-NAMES written		*/

#define QUOTE_CHAR '"'
#define QUOTE_STR  "\""

