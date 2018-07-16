			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	NAME:	wperson.h
*/

/* 			These are the definitions of the structures used in the personality files				*/


#ifndef INIT_WPERSON
#define INIT_WPERSON extern
#endif


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

/* NOTE: the libraries and volumes are padded to 8 or 6 chars then null terminated */

typedef struct {
		char	prt_mode;							/* Default print mode, O, S, H, K	*/
		char	prt_class;							/* Default print class (A-Z)		*/
		long	prt_num;							/* default printer number (000-999)	*/
		long	prt_form;							/* Default form number (000-999)	*/
		char	inlib[9];							/* Default INLIB			*/
		char	invol[7];							/* Default INVOL			*/
		char	outlib[9];							/* Default OUTLIB			*/
		char	outvol[7];
		char	runlib[9];							/* Default RUNLIB			*/
		char	runvol[7];
		char	spoolib[9];							/* default SPOOL lib			*/
		char	spoolvol[7];
		char	proglib[9];							/* PL Program lib			*/
		char	progvol[7];							/* PV Program volume			*/
		char	worklib[9];							/* default WORK lib			*/
		char	workvol[7];
		char	proc_stat;							/* Procedure queue submit status.	*/
		char	proc_class;							/* Procedure queue class.		*/
		char	proc_cpu[7];							/* Cpu limit.				*/
		unsigned	long	flags;						/* Help screen flags.			*/
		char	kb_map[7];							/* NO LONGER IN USE.			*/
		long	pf_version;							/* Personality file version.		*/
		char	psb_select;							/* Default pseudo blank char selection. */
		long	psb_charset;							/* Default pseudo blank char. set.	*/
		long	psb_rendition;							/* Default rendition for pseudo blanks.	*/
		long	prt_lines;							/* Default lines per page (000-255)	*/
		long	mp_cursor;							/* Cursor flag for menu pick items.	*/
		long	automove;							/* Flag set for auto_move functionality.*/
		long	autotab;							/* Flag set for auto_tab functionality.	*/
		long	bgchange;							/* Flag background to be changed.	*/
		long	bgcolor;							/* Background color flag.		*/
		long	excolor;							/* Exit background color flag.		*/
		} usr_defaults;

INIT_WPERSON usr_defaults defaults;							/* the actual defaults record		*/

#ifdef VMS
typedef struct	{
			char 	termname[12];						/* the terminal name			*/
			int  	termnum;						/* it's number				*/
			int	flags;							/* the flags				*/
			struct term_id	 *next;						/* pointer to the next one		*/
		} term_id;

INIT_WPERSON term_id 	*term_list;							/* this is the actual list		*/

typedef struct	{
			char 	latname[36];						/* the LAT name				*/
			int  	termnum;						/* it's number				*/
			int	flags;							/* the flags				*/
			struct lat_id	 *next;						/* pointer to the next one		*/
		} lat_id;

INIT_WPERSON lat_id 	*lat_list;							/* this is the actual list		*/

typedef struct	{
			char	class;							/* The procedure class 			*/
			char	qname[80];						/* The queue name			*/
			struct pq_id *next;						/* pointer to the next one		*/
		} pq_id;

INIT_WPERSON pq_id	*pq_list;

#endif	/* VMS */

#define		SETUP_CORRECTLY			1
#define		SPACES_BLINK			2
#define		KEYPAD_OFF			4
#define		PRO_RT				8

#define		HELP_ENABLED			0x00000001				/* Allow the HELP screen		*/
#define		HELP_SET_FILES			0x00000002				/* Allow the SET FILE USAGE CONSTANTS	*/
#define		HELP_SET_PRINTER		0x00000004				/* Allow set printer control		*/
#define		HELP_SET_PROC			0x00000008				/* Allow Set procedure queue control.	*/
#define		HELP_PRINT_SCREEN		0x00000010				/* Allow print screen.			*/
#define		HELP_COMMANDS			0x00000020				/* Allow commands.			*/
#define		HELP_KBMAP			0x00000040				/* NO LONGER IN USE.			*/
#define		HELP_TERMINAL			0x00000080				/* Allow terminal attribute changes.	*/
#define		HELP_SETPSB			0x00000100				/* Allow pseudo blank char changing.	*/
#define		HELP_DISPLAY	 		0x00000200				/* Allow ability to use DISPLAY util.	*/
#define		HELP_USAGE_WRITE		0x00000400				/* Allow write usage constants.		*/
#define		HELP_QUEUE_MNGMNT		0x00000800				/* Allow queue management.		*/
#define		HELP_SBATCH_ENABLED		0x00001000				/* Allow ability to see batch queues.	*/
#define		HELP_SGENERIC_ENABLED		0x00002000				/* Allow ability to see generic queues.	*/
#define		HELP_SOUTPUT_ENABLED		0x00004000				/* Allow ability to see output queues.	*/
#define		HELP_RESTRJOBS_DISABLED		0x00008000				/* Disable listing only jobs with the 	*/
											/*  callers user name.			*/
#define		HELP_SETCURCHAR			0x00010000				/* Allow cursor characteristic changing.*/
#define		HELP_GENERAL_UTILS 		0x00020000				/* Allow ability to see general utils.	*/
#define		HELP_SCREEN			0x00040000				/* Allow setting of screen character	*/
#define		HELP_GOODIES_UTILS		0x00080000				/* Allow ability use goodies utils	*/
#define		HELP_MANAGE_SYSTEM		0x00100000				/* Allow ability to manage system.	*/
#define		HELP_MANAGE_FILES_LIBS		0x00200000				/* Allow ability to manage files/libs.	*/
#define		HELP_CANCEL			0x00400000				/* Allow user to cancel processing	*/


typedef struct	{
			struct prt_id *next;						/* pointer to the next one		*/
			char	class;							/* The printer class 			*/
#ifdef VMS
			int	prtnum;							/* The printer number			*/
			char	qname[80];						/* The queue name			*/
#else
			char	prt_string[80];						/* Printer control string		*/
#endif
		} prt_id;

INIT_WPERSON prt_id	*prt_list;

#define MAX_TRANSLATE 60

#ifndef VMS	/* unix && MSDOS */
typedef struct	{
			struct logical_id *next;					/* pointer to the next one		*/
			char 	logical[6+1];						/* the logical name			*/
			char	translate[MAX_TRANSLATE+1];				/* it's translation			*/
		} logical_id;

INIT_WPERSON logical_id 	*logical_list;						/* this is the actual list		*/

typedef struct	{
			struct forms_id *next;						/* pointer to the next one		*/
			int	form_num;						/* the form number			*/
			char	form_string[80];					/* the string to  insert into the lp cmd*/
		} forms_id;

INIT_WPERSON forms_id 	*forms_list;							/* this is the actual list		*/

typedef struct	{
			struct  prmap_id *next;						/* pointer to the next one		*/
			int	prmap_num;						/* the Printer number			*/
			char	prmap_string[80];					/* the string to  insert into the lp cmd*/
		} prmap_id;

INIT_WPERSON prmap_id 	*prmap_list;							/* this is the actual list		*/

typedef struct	{
			struct scmap_id *next;						/* pointer to the next one		*/
			int	nice;							/* The NICE value			*/
			char 	class;							/* the job class			*/
		} scmap_id;

INIT_WPERSON scmap_id 	*scmap_list;							/* this is the actual list		*/
#endif	/* unix & MSDOS */

