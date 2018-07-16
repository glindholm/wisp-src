/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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

/*
**	File:		wperson.h
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Header for wperson.c
** 			These are the definitions of the structures used in the personality files.
**
*/

#ifndef WPERSON_H
#define WPERSON_H

/*
**	Structures and Defines
*/

/* 					define the default WANG environment record type						*/

#define DEFAULTS_PM	1
#define DEFAULTS_PC	2
#define	DEFAULTS_PR	3
#define	DEFAULTS_FN	4
#define	DEFAULTS_IL	5
#define	DEFAULTS_IV	6
#define	DEFAULTS_OL	7
#define	DEFAULTS_OV	8
#define	DEFAULTS_RL	9
#define	DEFAULTS_RV	10
#define	DEFAULTS_SL	11
#define	DEFAULTS_SV	12
#define	DEFAULTS_PL	13
#define	DEFAULTS_PV	14
#define	DEFAULTS_WL	15
#define	DEFAULTS_WV	16
#define	DEFAULTS_JS	17
#define	DEFAULTS_JC	18
#define	DEFAULTS_JL	19
#define	DEFAULTS_FLAGS	20
#define	DEFAULTS_PSB_CHAR	21
#define	DEFAULTS_PSB_SET	22
#define	DEFAULTS_PSB_REN	23
#define	DEFAULTS_LI		24
#define	DEFAULTS_MP_CURSOR	25
#define	DEFAULTS_AUTOMOVE	26
#define	DEFAULTS_AUTOTAB	27
#define	DEFAULTS_BGCHANGE	28
#define	DEFAULTS_BGCOLOR	29
#define	DEFAULTS_EXCOLOR	30

#define		SETUP_CORRECTLY			1
#define		SPACES_BLINK			2
#define		KEYPAD_OFF			4
#define		PRO_RT				8

/*
**	HELP Flags
**		These flags may be re-used but don't reorder or change the values of them
**		as they are stored in the PERSONALITY files.
*/

#define		HELP_ENABLED			0x00000001				/* Allow the HELP screen		*/
#define		HELP_SET_FILES			0x00000002				/* Allow the SET FILE USAGE CONSTANTS	*/
#define		HELP_SET_PRINTER		0x00000004				/* Allow set printer control		*/
#define		HELP_SET_PROC			0x00000008				/* Allow Set procedure queue control.	*/

#define		HELP_PRINT_SCREEN		0x00000010				/* Allow print screen.			*/
#define		HELP_COMMANDS			0x00000020				/* Allow commands.			*/
#define		HELP_DISPRINT			0x00000040				/* Allow DISPRINT utility		*/
#define		HELP_TERMINAL			0x00000080				/* Allow terminal attribute changes.	*/

#define		HELP_SETPSB			0x00000100				/* Allow pseudo blank char changing.	*/
#define		HELP_DISPLAY	 		0x00000200				/* Allow ability to use DISPLAY util.	*/
#define		HELP_USAGE_WRITE		0x00000400				/* Allow write usage constants.		*/
#define		HELP_PRINT_QUEUE		0x00000800				/* Allow PRINT queue management.       	*/

#define		HELP_SBATCH_ENABLED		0x00001000				/* UNUSED.	*/
#define		HELP_SGENERIC_ENABLED		0x00002000				/* UNUSED.	*/
#define		HELP_SOUTPUT_ENABLED		0x00004000				/* UNUSED.	*/
#define		HELP_RESTRJOBS_DISABLED		0x00008000				/* UNUSED 	*/

#define		HELP_SETCURCHAR			0x00010000				/* Allow cursor characteristic changing.*/
#define		HELP_ERROR_LOG 			0x00020000				/* ERROR LOG				*/
#define		HELP_SCREEN			0x00040000				/* Allow setting of screen character	*/
#define		HELP_BATCH_QUEUE		0x00080000				/* BATCH QUEUE				*/

#define		HELP_MANAGE_SYSTEM		0x00100000				/* Allow ability to manage system.	*/
#define		HELP_MANAGE_FILES_LIBS		0x00200000				/* Allow ability to manage files/libs.	*/
#define		HELP_CANCEL			0x00400000				/* Allow user to cancel processing	*/
#define		HELP_CHANGE_FILES_LIBS		0x00800000				/* Allow change from manage files/libs.	*/

#define		HELP_EDIT			0x01000000				/* Allow EDIT utility			*/
#define		HELP_CRID			0x02000000				/* Allow CRID utilies			*/
#define		HELP_SUBMIT			0x04000000				/* Allow SUBMIT utilies			*/


typedef struct	{
			struct prt_id *next;						/* pointer to the next one		*/
			char	class;							/* The printer class 			*/
			char	prt_string[80];						/* Printer control string		*/
		} prt_id;

#define MAX_TRANSLATE 60

typedef struct logical_id_struct logical_id;

struct logical_id_struct {
			logical_id *next;					/* pointer to the next one		*/
			char 	logical[6+1];						/* the logical name			*/
			char	translate[MAX_TRANSLATE+1];				/* it's translation			*/
		};

/*
**	Function Prototypes
*/

void WL_wpload(void);
int  WL_save_defaults(void);
int  WL_load_defaults(void);
int  WL_write_defaults_to_file(char *file);
int  WL_read_defaults_from_file(char *file);
void WL_delete_defaults_temp(void);

char *WL_wforms(int num);
char *WL_getprmap(int num);
char *WL_wlpclass(char lpclass);
int WL_getscmapnice(char jobclass, int *nice_value);
int WL_getcqmap(char jobclass, char *queue_value);

int  WL_ttyid5(char *tty);
void WL_build_wisp_config_path(char *file, char *path);
int  WL_opt_linkvectoroff(void);
int  WL_load_options(void);

int WL_get_defs(int code, void *void_ptr);
int WL_set_defs(int code, const void *void_ptr);

int WL_clear_progdefs(void);
int WL_set_progdefs_env(char *progvol, char *proglib);
int WL_save_progdefs(void);
int WL_restore_progdefs(void);

prt_id     *WL_get_lpmap_list(void);
logical_id *WL_get_lgmap_list(void);

int WL_get_dispfac_char(unsigned char c_hex, unsigned char *facchar, int *font);

const char *wispprbdir(char *dir);
const char *wisplinkdir(char *dir);
const char *wisptmpdir(char *dir);
const char *wispdefdir(char *dir);
const char *wisp_defaults_path(char *path);
const char *wisp_temp_defaults_path(char *path);

void USESOFTLINK(char *laststate);
void USEHARDLINK(char *laststate);
int WL_softlink(void);

int wisp_nativescreens(void);
int wisp_acu_nativescreens(void);
int WL_pfkeys12(void);
const char *WL_get_wisp_option(const char *keyword);
const char *WL_get_wisp_option_env(const char *keyword);

#define OPTION_IDNUMERIC	(NULL != WL_get_wisp_option("IDNUMERIC"))	/* UNIX EXTRACT ID returns Numeric user ID	*/
#define OPTION_IDFIVE		(NULL != WL_get_wisp_option("IDFIVE"))		/* UNIX EXTRACT ID returns chars 5-7 user ID	*/
#define OPTION_ALLSTATUSKEYS	(NULL != WL_get_wisp_option("ALLSTATUSKEYS"))	/* Pass All STATUS keys thru to user declaritive*/
#define OPTION_SIGNALSOFF	(NULL != WL_get_wisp_option("SIGNALSOFF"))	/* Disable Unix signal trapping.		*/
#define OPTION_CREATEVOLUMEON	(NULL != WL_get_wisp_option("CREATEVOLUMEON"))	/* UNIX auto create VOLUME if not found		*/
#define OPTION_OUTPUTVERIFYOFF	(NULL != WL_get_wisp_option("OUTPUTVERIFYOFF"))	/* Turn off the PF3 to continue screens		*/
#define OPTION_NULLISDOT	(NULL != WL_get_wisp_option("NULLISDOT"))	/* Set so NULLs will display as a dot.		*/

const char* WL_batchqueue_name();

#endif /* WPERSON_H */

/*
**	History:
**	$Log: wperson.h,v $
**	Revision 1.34  2003/03/20 18:28:45  gsl
**	Fix logical_id typedef
**	
**	Revision 1.33  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.32  2003/01/29 16:20:57  gsl
**	change loadpadnull() and WL_set_defs() to use const for the source arg
**	
**	Revision 1.31  2002/12/03 22:15:11  gsl
**	Replace the w_err_flag bitmask with wispdebug mode that can be set to "FULL"
**	"ERRORS" or "NONE" to simplify.
**	
**	Revision 1.30  2002/11/27 20:09:31  gsl
**	Add WL_get_wisp_option_env() which looks for option first in environment then
**	in the OPTIONS file.
**	
**	Revision 1.29  2002/07/12 20:40:46  gsl
**	Global unique WL_ changes
**	
**	Revision 1.28  2002/07/12 19:10:25  gsl
**	Global unique WL_ changes
**	
**	Revision 1.27  2002/07/11 20:29:21  gsl
**	Fix WL_ globals
**	
**	Revision 1.26  2002/07/10 21:06:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.25  2002/07/02 04:03:18  gsl
**	Add wisp_acu_nativescreens()
**	
**	Revision 1.24  2001/10/31 20:26:05  gsl
**	Add wisp_temp_defaults_path()
**	
**	Revision 1.23  2001-09-07 15:29:51-04  gsl
**	Removed VMS code
**	Added HELP_ERROR_LOG and HELP_BATCH_QUEUE
**
**	Revision 1.22  1997-10-23 15:22:15-04  gsl
**	Add get_wisp_option()
**
**	Revision 1.21  1997-09-22 12:48:40-04  gsl
**	Add prototypes for nativescreens() and pfkeys12()
**
**	Revision 1.20  1997-08-18 15:03:46-04  gsl
**	Add SUBMIT flag
**
**	Revision 1.19  1996-12-06 18:42:35-05  jockc
**	proto for getcqmap (CQMAP is class to queue mapping for NT/95)
**
**	Revision 1.18  1996-11-12 16:09:31-08  gsl
**	Cleanup for NT
**
**	Revision 1.17  1996-11-08 13:49:00-08  gsl
**	Change USEHARDLINK and USESOFTLINK to pass back a laststate param.
**
**	Revision 1.16  1996-10-08 17:31:16-07  gsl
**	move prototypes to wispcfg.h
**
**	Revision 1.15  1996-09-05 09:55:10-07  jockc
**	include decl of wforms getprmap et al for WIN32
**
**	Revision 1.14  1996-09-04 17:20:54-07  gsl
**	Moved the softlink prototypes from link.h
**
**	Revision 1.13  1996-08-26 17:14:07-07  gsl
**	Added wispdefdir() for location of temp defaults
**	added delete_defaults_temp() to delete temp deafults
**	added wisp_defaults_path() for path to default personality file
**
**	Revision 1.12  1996-08-23 14:12:11-07  gsl
**	Add prototypes for the wispxxxdir() routines
**
**	Revision 1.11  1996-06-24 11:29:10-07  gsl
**	fix for NT
**
**	Revision 1.10  1995-04-25 02:51:59-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.9  1995/04/17  11:45:31  gsl
 * drcs state V3_3_14
 *
 * Revision 1.8  1995/03/20  13:29:05  gsl
 * For get_defs() and set_defs() use a void *.
 *
 * Revision 1.7  1995/03/20  13:17:38  gsl
 * Added build_wisp_config_path() plus updated
 * added standard headers
 *
**
*/
