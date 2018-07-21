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


#ifndef WISP_H
#define WISP_H

/* WISP common variable definition file, EXT is defined to be "extern" for the wangutils, and blank for MAIN  */

#include <ctype.h>
#include <stdio.h>

#include <stdlib.h>
#include <string.h>

#ifdef unix
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
#endif

#include "proto.h"
#include "output.h"
#include "keywords.h"

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
#define EXIT_AND_RUN		-2		/* Exit WISP and re run it.		*/


#define SEVER_SUCCESS	0
#define SEVER_WARNING	1
#define SEVER_ERROR	2
#define SEVER_FATAL	3


EXT char prog_id[40];					/* the program id of this program		*/
EXT char linein[256];					/* The current input line.			*/

EXT int re_copy	   INIT_FALSE;				/* Is this the scond time for this copy lib.	*/

#define MAX_PARAGRAPHS		512							/* How many paragraphs can be copied.	*/

EXT int  proc_paras_cnt		INIT_ZERO;						/* How many were read.			*/
EXT int  decl_paras_cnt		INIT_ZERO;						/* How many paragraphs were in decl.	*/
EXT int  decl_performs_cnt	INIT_ZERO;						/* How many performs' were in decl.	*/

EXT char proc_paras[MAX_PARAGRAPHS][40];		/* The list.				*/
EXT char decl_paras[MAX_PARAGRAPHS][40];		/* The names in the declaratives.	*/
EXT char decl_performs[MAX_PARAGRAPHS][40];		/* The performs in the Declaratives.	*/

EXT int  opt_copy_declarative_para_cnt	INIT_ZERO;					/* How many performs from the proc div	*/
EXT int  opt_blank_when_zero_cnt INIT_ZERO;						/* How many items to blank.		*/
EXT int  opt_no_file_status_cnt	INIT_ZERO;						/* How many SELECTs to skip file status	*/
EXT int  opt_sort_file_cnt	INIT_ZERO;						/* How many SORT files.			*/


#define MAX_FIGCONS2		255							/* Maximum nuber of 2 byte fig cons.	*/

EXT char fig_cons[MAX_FIGCONS2][32];							/* names of figcons that are 2 bytes	*/
EXT int  fig_val [MAX_FIGCONS2][2];							/* their values				*/
EXT int  fig_count INIT_ZERO;								/* the counter				*/

struct c_list
	{
		int call_count;								/* How many calls in this struct.	*/
		char call_list[100][40];						/* The list of calls.			*/
		int ref_count[100];							/* How many times are they referenced.	*/
		struct c_list *next_list;						/* Pointer to the next list.		*/
	};

typedef struct c_list c_list;
EXT c_list *xref_ptr;									/* Pointer to cross ref list.		*/

EXT int in_decl		INIT_FALSE;							/* Are we in the declaratives section?	*/
EXT int decl_stop_exit	INIT_FALSE;							/* STOP RUN or EXIT PROGRAM in DECLAR	*/

EXT int division INIT_FALSE;								/* Which division are we scanning?	*/

#define IDENTIFICATION_DIVISION		0
#define ENVIRONMENT_DIVISION		1
#define INPUT_OUTPUT_SECTION		2
#define DATA_DIVISION			3
#define WORKING_STORAGE_SECTION		4
#define PROCEDURE_DIVISION		5

EXT int copy_sect 		INIT_FALSE;		/* SECTION copy mode.			*/
EXT int copy_to_dcl_file 	INIT_FALSE;		/* Paragraph copying mode.		*/
EXT int copy_to_dtp_file 	INIT_FALSE;		/* Declarative copy mode.		*/


#define FAC_OF_PREFIX		"FAC-OF-"
#define ORDER_AREA_PREFIX	"ORDER-AREA-"

#define DOUBLE_QUOTE '"'
#define SINGLE_QUOTE '\''



/* 
** PROCESSING STATUS FIELDS 
*/
EXT int wrote_special_names	INIT_FALSE;		/* Was SPECIAL-NAMES written		*/
EXT int del_use			INIT_FALSE;		/* not deleting a USE procedure.	*/
EXT int lock_clear_para		INIT_FALSE;		/* Gen WISP-RECORD-LOCK-CLEAR para	*/

/*
**	Translation Options
*/

EXT int  acu_cobol 		INIT_FALSE;		/* ACUCOBOL 				*/
EXT int  mf_cobol 		INIT_FALSE;		/* Micro Focus COBOL 			*/
EXT int  mfoc_cobol 		INIT_FALSE;		/* Micro Focus Object COBOL		*/
EXT int  mfse_cobol 		INIT_FALSE;		/* Micro Focus Server Express COBOL 	*/
EXT int  unix_cobol 		INIT_FALSE;		/* Any UNIX COBOL 			*/
EXT char cobol_type[4];					/* The type of COBOL			*/

EXT char hard_lock[5];					/* The file status for hard lock	*/
EXT char soft_lock[5];					/* The file status for soft lock	*/
EXT char bin2_type[8];					/* The thing to convert BINARY to.	*/
EXT char bin4_type[8];					/* The thing to create 4 byte binary.	*/
EXT char packdec[8]; 					/* The thing to convert COMP to		*/


EXT int opt_compressed_screens  INIT_TRUE;		/* Compress screen output		*/
EXT int opt_gen_copylib 	INIT_FALSE;		/* Generate copy libs			*/
EXT int opt_noprocess 		INIT_FALSE;		/* don't process it just copy it, 	*/
EXT int opt_logging 		INIT_FALSE;		/* no logging by default		*/
EXT int opt_log_stats 		INIT_FALSE;		/* no stats either.			*/
EXT int opt_data_conv		INIT_FALSE;		/* Default to not data conversion	*/
EXT int opt_x4dbfile		INIT_FALSE;		/* Do not add check for database file	*/
EXT int opt_xref		INIT_FALSE;		/* Keep cross ref information.		*/
EXT int opt_xtab		INIT_FALSE;		/* Keep cross ref tab file.		*/
EXT int opt_nowarnings		INIT_FALSE;		/* Don't show WARNINGS messages.	*/
EXT int opt_init_move		INIT_FALSE;		/* don't change move spaces to initial	*/
EXT int opt_init_data		INIT_FALSE;		/* program id is initial.		*/
EXT int opt_gen_dms_locking	INIT_TRUE;		/* generate record locking logic	*/
EXT int opt_fdfiller		INIT_TRUE;		/* allow fillers in fd's		*/
EXT int opt_wsfiller		INIT_TRUE;		/* allow fillers in working-storage	*/
EXT int opt_keyfile_flag	INIT_FALSE;		/* Don't look for key files.		*/
EXT int opt_optfile_flag	INIT_FALSE;		/* Flag to process the option file.	*/
EXT int opt_manual_locking	INIT_FALSE;		/* LOCK mode is MANUAL			*/

EXT int opt_keep_config_computer	INIT_FALSE;	/* #KEEP_CONFIG_COMPUTER */
EXT int opt_translate_config_computer	INIT_FALSE;	/* #TRANSLATE_CONFIG_COMPUTER */
EXT int opt_change_level77		INIT_FALSE;	/* #CHANGE_LEVEL77 */
EXT int opt_dont_force_area_a_levels	INIT_FALSE;	/* #DONT_FORCE_AREA_A_LEVELS */
EXT int opt_copybook_ext_cpy		INIT_FALSE;	/* #COPYBOOK_EXT_CPY */	
EXT int opt_copybook_ext_lib		INIT_FALSE;	/* #COPYBOOK_EXT_LIB */	
EXT int opt_native			INIT_FALSE;	/* #NATIVE */
EXT int opt_nogetlastfileop		INIT_FALSE;	/* #NOGETLASTFILEOP */
EXT int opt_nosleep			INIT_FALSE;	/* #NOSLEEP */
EXT int opt_forcegenwispcpy		INIT_FALSE;	/* #FORCEGENWISPCPY */
EXT int opt_sign_trailing_separate	INIT_FALSE;	/* #SIGN_TRAILING_SEPARATE */
EXT int opt_deldatarecords		INIT_FALSE;	/* #DELETE_DATA_RECORDS */
EXT int opt_nodisplay_processing	INIT_FALSE;	/* #NO_DISPLAY_PROCESSING */
EXT int opt_no_ws_blank_when_zero	INIT_FALSE;	/* #NO_WS_BLANK_WHEN_ZERO */
EXT int opt_keep_stop_run  		INIT_FALSE;	/* #KEEP_STOP_RUN - (default).	*/
EXT int opt_change_stop_run		INIT_FALSE;	/* #CHANGE_STOP_RUN - Change STOP RUN to EXIT PROGRAM	*/
EXT int opt_delete_record_contains	INIT_FALSE;	/* #DELETE_RECORD_CONTAINS */
EXT int opt_no_word_swap		INIT_FALSE;	/* #NO_WORD_SWAP */
EXT int	opt_no_seq_seq_optional		INIT_FALSE;	/* #NO_SEQ_SEQ_OPTIONAL - Use the OPTIONAL phrase on SEQ-SEQ files. */
EXT int	opt_symbzero			INIT_FALSE;

#define MAX_BLANK_ITEMS		100
EXT char* opt_blank_when_zero_item[MAX_BLANK_ITEMS];	/* #BLANK_WHEN_ZERO - Screen items to be blank when zero.	*/

#define MAX_STATUS_ITEMS	32							/* The max to allow.			*/
EXT char* opt_no_file_status_item[MAX_STATUS_ITEMS];	/* #NO_FILE_STATUS -  Prevent WISP from adding file status	*/

#define MAX_SF_ITEMS		32
EXT char* opt_sort_file_item[MAX_SF_ITEMS];		/* #SORT_FILE - The identified SORT files.		*/

EXT char* opt_copy_declarative_para[MAX_PARAGRAPHS];	/* #COPY_DECLARATIVE - Copy named declarative para to procedure div	*/

EXT int opt_native_screens		INIT_FALSE;	/* #NATIVE_SCREENS */
EXT int opt_nofac			INIT_FALSE;	/* #NOFAC */

#endif /* WISP_H */

/*
**	History:
**	$Log: wisp.h,v $
**	Revision 1.39  2005/12/02 15:22:47  gsl
**	Keep track of the highest severity level reported.
**	Ensure an non-zero exit status if severity is fatal or higher.
**	
**	Revision 1.38  2003/08/13 20:57:06  gsl
**	#NOFAC option
**	
**	Revision 1.37  2003/08/08 19:52:47  gsl
**	Add native screens comments
**	
**	Revision 1.36  2003/08/06 18:12:10  gsl
**	
**	Revision 1.35  2003/03/10 18:55:45  gsl
**	Added nosleep option and for ACU default to using C$SLEEP instead
**	of WFWAIT on a READ with HOLD
**	
**	Revision 1.34  2003/03/07 21:44:20  gsl
**	rremove unused opt_native_acu flag
**	
**	Revision 1.33  2003/03/07 17:00:07  gsl
**	For ACU default to using "C$GETLASTFILEOP" to retrieve the last file op.
**	Add option #NOGETLASTFILEOP to use if not C$GETLASTFILEOP is
**	not available.
**	
**	Revision 1.32  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.31  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.30  2003/02/25 21:56:03  gsl
**	cleanup some global "parm" variables
**	
**	Revision 1.29  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.28  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.27  2003/01/21 16:25:57  gsl
**	remove VMS /DLINK option
**	
**	Revision 1.26  2003/01/15 18:23:33  gsl
**	add #SIGN_TRAILING_SEPARATE support for MF
**	
**	Revision 1.25  2002/12/12 21:31:30  gsl
**	When #NATIVE and Acubol then set the opt_native_acu and
**	the opt_getlastfileop options flags
**	
**	Revision 1.24  2002/10/14 19:08:01  gsl
**	Remove the -c options as obsolete since comments are now always
**	included in the output.
**	
**	Revision 1.23  2002/08/13 18:47:29  gsl
**	#DELETE_DATA_RECORDS
**	
**	Revision 1.22  2002/08/12 20:13:51  gsl
**	quotes and literals
**	
**	Revision 1.21  2002/07/25 17:53:35  gsl
**	Fix FAC-OF- prefix
**	
**	Revision 1.20  2002/07/18 21:04:26  gsl
**	Remove MSDOS code
**	
**	Revision 1.19  2002/06/21 20:49:32  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.18  2002/05/22 20:25:59  gsl
**	#KEEP_CONFIG_COMPUTER and #TRANSLATE_CONFIG_COMPUTER options
**	
**	Revision 1.17  2002/05/16 21:55:40  gsl
**	new options
**	
**	Revision 1.16  2002-03-21 17:02:44-05  gsl
**	Add mfoc_cobol and mfse_cobol
**
**	Revision 1.15  2001-09-13 09:58:49-04  gsl
**	Add opt_xtab -X flag
**
**	Revision 1.14  1998-06-06 14:57:39-04  gsl
**	Add opt_manual_locking flag
**
**	Revision 1.13  1998-03-04 15:58:15-05  gsl
**	Enlarge prog_id to 40 characters for cobol-85
**
**	Revision 1.12  1997-08-28 17:11:08-04  gsl
**	Add flags for native screens with acucobol
**
**	Revision 1.11  1996-06-24 14:22:52-04  gsl
**	add NT cobol and fix MSDOS for WINNT
**
**
**
*/
