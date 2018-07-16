			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	NAME:	wperson.h
*/

/* 			These are the definitions of the structures used in the personality files				*/

#ifndef WPERSON_H
#define WPERSON_H

void wpload();

int get_defs();
int set_defs();
int save_defaults();
int load_defaults();
int write_defaults_to_file();
int read_defaults_from_file();

#ifndef VMS
char *wforms();
char *getprmap();
char *wlpclass();
int getscmapnice();
int ttyid5();
#endif

int load_options();
int clearprogsymb();
int setprogdefs();
int saveprogdefs();
int restoreprogdefs();


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
#define		HELP_QUEUE_MNGMNT		0x00000800				/* Allow queue management.		*/

#define		HELP_SBATCH_ENABLED		0x00001000				/* Allow ability to see batch queues.	*/
#define		HELP_SGENERIC_ENABLED		0x00002000				/* Allow ability to see generic queues.	*/
#define		HELP_SOUTPUT_ENABLED		0x00004000				/* Allow ability to see output queues.	*/
#define		HELP_RESTRJOBS_DISABLED		0x00008000				/* Disable listing only jobs with the 	*/
											/*  callers user name.			*/
#define		HELP_SETCURCHAR			0x00010000				/* Allow cursor characteristic changing.*/
#define		HELP_GENERAL_UTILS_X 		0x00020000			/* NO LONGER USED			*/
#define		HELP_SCREEN			0x00040000				/* Allow setting of screen character	*/
#define		HELP_GOODIES_UTILS_X		0x00080000			/* NO LONGER USED			*/

#define		HELP_MANAGE_SYSTEM		0x00100000				/* Allow ability to manage system.	*/
#define		HELP_MANAGE_FILES_LIBS		0x00200000				/* Allow ability to manage files/libs.	*/
#define		HELP_CANCEL			0x00400000				/* Allow user to cancel processing	*/
#define		HELP_CHANGE_FILES_LIBS		0x00800000				/* Allow change from manage files/libs.	*/

#define		HELP_EDIT			0x01000000				/* Allow EDIT utility			*/
#define		HELP_CRID			0x02000000				/* Allow CRID utilies			*/

#ifdef VMS
typedef struct	{
			char 	termname[12];						/* the terminal name			*/
			int  	termnum;						/* it's number				*/
			int	flags;							/* the flags				*/
			struct term_id	 *next;						/* pointer to the next one		*/
		} term_id;

typedef struct	{
			char 	latname[36];						/* the LAT name				*/
			int  	termnum;						/* it's number				*/
			int	flags;							/* the flags				*/
			struct lat_id	 *next;						/* pointer to the next one		*/
		} lat_id;

typedef struct	{
			char	class;							/* The procedure class 			*/
			char	qname[80];						/* The queue name			*/
			struct pq_id *next;						/* pointer to the next one		*/
		} pq_id;

term_id *get_term_list();
lat_id *get_lat_list();
pq_id *get_pq_list();

#endif /* VMS */

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
prt_id *get_prt_list();

#define MAX_TRANSLATE 60

#ifndef VMS	/* unix && MSDOS */
typedef struct	{
			struct logical_id *next;					/* pointer to the next one		*/
			char 	logical[6+1];						/* the logical name			*/
			char	translate[MAX_TRANSLATE+1];				/* it's translation			*/
		} logical_id;
logical_id *get_logical_list();
#endif	/* unix & MSDOS */


#endif /* WPERSON_H */
