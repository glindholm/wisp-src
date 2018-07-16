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

#ifndef unix	/* VMS or MSDOS */
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

#include "wcommon.h"

#ifdef INIT_COMMON

#define INIT_TRUE     = { 1 }
#define INIT_FALSE    = { 0 }

#else

#define INIT_TRUE
#define INIT_FALSE

#endif


struct item_record
	{
		struct item_record *next_item;						/* A ptr to the next item		*/
		short	x_level;							/* The level				*/
		short	x_occurs;							/* The occurs clause			*/
		short	row;								/* The calculated row 			*/
		short	x_row;								/* The row clause			*/
		short	col;								/* The calculated column		*/
		short	x_col;								/* The column clause			*/
		short	vert_1;								/* First vertical occurs count		*/
		short	vert_2;								/* Second vertical occurs count		*/
		short	horiz;								/* Horizontal occurs count		*/
		short	vert_off;							/* Number of lines between vertical OCC	*/
		short	vert_off2;							/* Number of lines between vertical OCC	*/
		short	num_range;							/* The number of range items		*/
		char	item_num[4];							/* Cobol ref number example = 01 or 05	*/
		char	name[40];							/* The variable name			*/
		char	pic[40];							/* The pic definition for the var	*/
		char	source[80*3];							/* The var that is the source for output*/
		char	object[80];							/* The var that is the target for input */
		char	lo_range[80];							/* The low end range item		*/
		char	hi_range[80];							/* The High end range item		*/
	};
typedef struct item_record item_record;

#ifdef INIT_COMMON

/* 					This is a list of keywords which begin something					*/

EXT char *proc_keywords[] = 	{	"ACCEPT", "ADD","ALTER",
					"BEGIN",
					"CALL","CANCEL","CLOSE","COMMIT","COMPUTE","CONTINUE",
					"DELETE","DISABLE","DISPLAY","DIVIDE",
					"ELSE","ENABLE","ENTER","EXIT",
					"FREE",
					"GENERATE","GO",
					"HOLD",
					"IF","INITIATE","INSPECT",
					"MERGE","MOVE","MULTIPLY",
					"NEXT",
					"OPEN",
					"PERFORM",
					"READ","READY","RESET","RECEIVE","RELEASE","RETURN","REWRITE","ROLLBACK",
					"SEARCH","SEND","SET","SORT","START","STOP","STRING","SUBTRACT","SUPRESS",
					"TERMINATE",
					"UNSTRING",
					"WHEN","WRITE",
					""	};

EXT char *brk_keywords[] = 	{	"ACCEPT",
					"CALL","CLOSE",
					"DELETE","DISPLAY",
					"EXIT",
					"FREE",
					"GO",
					"IF",
					"MOVE",
					"OPEN",
					"PERFORM",
					"READ","REWRITE","ROLLBACK",
					"SEARCH","SORT","START","STOP",
					"WRITE",
					""	};

/* 				These are keywords specific to DISPLAY AND READ statements					*/

EXT char *disp_keywords[] =	{	"NO-MOD","ON","PFKEY","PFKEYS","ONLY",""	};

/*				These are COBOL-85 reserved words that may appear as field names in WANG COBOL			*/

EXT char *res_defaults[] =	{	"BELL",
					"BEEP",
					"BEGINNING",
					"BLINK",
					"BLINKING",
					"CLASS",
					"CRT",
					"CURRENT",
					"DAY-OF-WEEK",
					"DB",
					"DEFAULT",
					"EMPTY",
					"END-DELETE",
					"END-READ",
					"END-WRITE",
					"FALSE",
					"ID",
					"INITIALIZE",
					"LENGTH",
					"MATCH",
					"MESSAGE",
					"NAME",
					"NO-ECHO",
					"OTHER",
					"PROCESS",
					"REFERENCE",
					"RESEND",
					"RETURN-CODE",
					"SEQUENCE-NUMBER",
					"SCREEN",
					"SHUT-DOWN",
					"STANDBY",
					"TAB",
					"TERMINAL",
					"TRUE",	
					"WAIT",
					""
				};

#else

EXT char *proc_keywords[];
EXT char *disp_keywords[];
EXT char *res_defaults[];
EXT char *brk_keywords[];

#endif

EXT char **res_keywords;

EXT char fname[256];								/* the name of the primary input file		*/
EXT char out_fname[256];							/* the name of the primary output file		*/
EXT char prog_id[16];								/* the program id of this program		*/
EXT char libname[133];

#define		USE_SPECIFIC_FILE		1
#define		USE_GENERAL_FILE		2

EXT char cpy_lib[18],cpy_file[18];						/* Current copy lib, file.			*/
EXT char key_fname[256];							/* The file with key info.			*/
EXT char xref_fname[256];							/* The file to receive cross ref information.	*/
EXT char lib_file[256];								/* The name of the file to hold copied paras.	*/
EXT char decl_fname[256];							/* The file to hold DECLARATIVES copied paras.	*/
EXT char par_fname[256];							/* The name of the file that has the para list.	*/
EXT char opt_fname[256];							/* The name of the option file used.		*/
EXT char word_fname[256];							/* The name of the WORD file used.		*/
EXT char work_fname[256];							/* The name of the work file.			*/
EXT char inline[256];								/* The current input line.			*/
EXT char brkline[256];								/* The line to hold rest of inline after break.	*/
EXT char templine[256];

EXT char cli_infile[200];							/* The initial input file name			*/
EXT char cli_listfile[200];							/* The file for conversion information listing	*/
EXT char cli_ildir[200];							/* the Directory name for INLIB			*/

EXT struct rcpy_struct
{
	char	file[10];
	char	lib[10];
	char	native[80];
} rcpy_element;
EXT char *rcpy_list_ring INIT_FALSE;						/* List of copy files for xref			*/

EXT int re_copy	   INIT_FALSE;							/* Is this the scond time for this copy lib.	*/
EXT int had_read   INIT_FALSE;							/* Did we see a read statement in this copy lib?*/
EXT int cpy_seq    INIT_FALSE;							/* Copy lib sequence counter.			*/

EXT int used_wrk INIT_FALSE;							/* Flag that the workfile was used.		*/

#define CRT_FILE  "$WCXXXXXX"
#define READ_FILE "$WRXXXXXX"

#ifdef VMS
#define WISP_OPTION_FILE	"WISP.OPT"
#define WISP_KEY_FILE		"WISP.KEY"
#else
#define WISP_OPTION_FILE	"wisp.opt"
#define WISP_KEY_FILE		"wisp.key"
#endif

EXT char read_name[132] INIT_FALSE;							/* the name of the READ routine file	*/
EXT char crt_name[132] INIT_FALSE;							/* the name of the CRT scratch file	*/
EXT char proc_name[132] INIT_FALSE;							/* the name of the Proc scratch file	*/

EXT FILE *read_temp_file INIT_FALSE;							/* The temp scratch file for READ's	*/
EXT FILE *crt_temp_file INIT_FALSE;							/* The temp scratch file for CRT	*/
EXT FILE *xref_file	INIT_FALSE;							/* The cross ref file.			*/

#define		MAX_SCREENS	100							/* the maximum number of screens	*/

EXT item_record *screen_item[MAX_SCREENS];						/* A list of screen items		*/
EXT item_record *this_item INIT_FALSE;							/* A ptr to the current screen item	*/
EXT item_record *last_item INIT_FALSE;							/* A ptr to the previous screen item	*/
EXT item_record *occurs_item INIT_FALSE;						/* A ptr to the first item in a group	*/

#define		SCRN_IN_DECLARATIVES		0x01					/* flag if screen used in declaratives	*/
#define		SCRN_IN_PROCDIV			0x02					/* flag screen used in procedure div.	*/

EXT char scrn_name[MAX_SCREENS][40];							/* The names of screen records		*/
EXT int  scrn_crt[MAX_SCREENS];								/* The crt files they are using.	*/
EXT char scrn_flags[MAX_SCREENS];							/* 8 bit flags				*/

#define MAX_CRT_FILES	8

EXT char crt_file[MAX_CRT_FILES][40];							/* the local filename of the screen	*/
EXT char crt_pfkey[MAX_CRT_FILES][40];							/* the local name of the pfkey field	*/
EXT char crt_status[MAX_CRT_FILES][40];							/* the local name of the ret status	*/
EXT char crt_cursor[MAX_CRT_FILES][40];							/* the local name of the cursor position*/
EXT char crt_relative[MAX_CRT_FILES][40];						/* the local name of the relative key	*/
EXT int  crt_prec[MAX_CRT_FILES];							/* The array item of primary crt record.*/

EXT int cur_crt		INIT_FALSE;							/* The current crt being referenced.	*/
EXT int crt_fcount	INIT_FALSE;							/* How many crt files are there?	*/
											/* These are used by prog_ftypes.	*/
#define DISK_FILE		0x0001							/* The file is a normal disk file	*/
#define PRINTER_FILE		0x0002							/* The file is a printer file		*/
#define SORT_FILE		0x0004							/* The file is a SORT file		*/
#define HAS_DECLARATIVES	0x0008							/* The file has a DECLARATIVES section	*/
#define NORESPECIFY		0x0010							/* The file has a NORESPECIFY option.	*/
#define INDEXED_FILE		0x0020							/* The file is indexed.			*/
#define TAPE_FILE		0x0040							/* The file is a tape file.		*/
#define SEQ_DYN			0x0080							/* The file was SEQUENTIAL/DYNAMIC.	*/
#define SEQ_FILE		0x0100							/* The file is SEQUENTIAL.		*/
#define HAD_FD			0X0200							/* The file had an FD statement.	*/
#define AUTOLOCK		0X0400							/* The file to use automatic locking.	*/
#define SEQ_SEQ			0x0800							/* The file was SEQUENTIAL/SEQUENTIAL.	*/
#define FILE_MASK		~(DISK_FILE + PRINTER_FILE + SORT_FILE + TAPE_FILE)	/* The mask used to remove file bits.	*/

#define FSET(F,V) 		(F = (F & FILE_MASK) | V)				/* Macro to set a file value.		*/

#define EXIT_AND_RUN		1							/* Exit WISP and re run it.		*/

#define MAX_FILES		200							/* The maximum number of files		*/

EXT int  prog_ftypes[MAX_FILES];							/* the file type flags			*/
EXT char prog_files[MAX_FILES][40];							/* the program files in SELECT's	*/
EXT char prog_vnames[MAX_FILES][40];							/* the program file volume names	*/
EXT char prog_lnames[MAX_FILES][40];							/* the program file library names	*/
EXT char prog_fnames[MAX_FILES][40];							/* the program file names in SELECT's	*/
EXT char prog_fstats[MAX_FILES][40];							/* the program file status fields	*/
EXT char prog_prname[MAX_FILES][40];							/* The prnames.				*/
EXT char prog_dsel[MAX_FILES][40];							/* The files to delete their SELECT's.	*/
EXT char *prog_recs[MAX_FILES];								/* Pointers to a files DATA RECORD names*/
EXT int prog_ref[MAX_FILES];								/* Were they actually opened?		*/

EXT struct
	{
		char *list;
		int count;
	} prog_keys[MAX_FILES];								/* A pointer to the lists of keys.	*/

#define MAX_RECORD_CNT		500							/* The max number of record names	*/

EXT struct
	{
		char	name[40];
		short	fd;
	} record_list[MAX_RECORD_CNT];							/* List of every record name & it's fd	*/
EXT int record_cnt INIT_FALSE;								/* Count of records in record_list	*/

#define MAX_KEY_LIST		100							/* Maximum number fo keys to correct.	*/
#define KL_LEAD		1								/* Key list type LEADING SEPARATE	*/
#define KL_COMP		2								/*               COMPUTATIONAL		*/
struct key_list_struct
{
	char	name[40];
	char	qual[40];
	short	type;
};
typedef struct key_list_struct key_list_struct;

EXT key_list_struct key_list[MAX_KEY_LIST];						/* The list of keys.			*/
EXT int kl_count	INIT_FALSE;							/* How many are there.			*/

struct key_struct									/* Hold KEY list for each SELECT	*/
{
	char			key[40];						/* The KEY				*/
	struct	key_struct	*next;							/* Next Key on list			*/
};
typedef struct key_struct key_struct;

struct file_master_struct								/* Master File info			*/
{
	short		key_cnt;							/* Number of keys			*/
	key_struct	*key_start;							/* Link list of Keys			*/
};
typedef struct file_master_struct file_master_struct;

EXT file_master_struct file_master[MAX_FILES];	

EXT int  prog_cnt INIT_FALSE;								/* the count of them			*/
EXT int  prog_sort INIT_FALSE;								/* how many were sort files		*/
EXT int  prog_dscnt INIT_FALSE;								/* How many need their SELECT deleted.	*/

#define MAX_NAME_LIST		500

EXT char name_list[MAX_NAME_LIST][100];							/* A list of names, used as scratch.	*/
EXT int name_count INIT_FALSE;								/* The number in the list.		*/

#define MAX_RECORD_STORAGE	512

EXT char rec_stor[MAX_RECORD_STORAGE];							/* Temp storage for record names.	*/

#define MAX_PARAGRAPHS		512							/* How many paragraphs can be copied.	*/

EXT int  par_count	INIT_FALSE;							/* How many were read.			*/
EXT int  pd_count	INIT_FALSE;							/* How many paragraphs were in decl.	*/
EXT int  pr_count	INIT_FALSE;							/* How many performs' were in decl.	*/
EXT int  ppdiv_count	INIT_FALSE;							/* How many performs from the proc div	*/
EXT int  blank_count	INIT_FALSE;							/* How many items to blank.		*/
EXT int  nfs_count	INIT_FALSE;							/* How many SELECTs to skip file status	*/
EXT int  sf_count	INIT_FALSE;							/* How many SORT files.			*/

#define MAX_STATUS_ITEMS	32							/* The max to allow.			*/
#define MAX_SF_ITEMS		32

EXT char sf_item[MAX_SF_ITEMS][40];							/* The identified SORT files.		*/
EXT char nfs_item[MAX_STATUS_ITEMS][40];						/* The list of status's to skip.	*/
EXT char par_name[MAX_PARAGRAPHS][40];							/* The list.				*/
EXT char par_decl[MAX_PARAGRAPHS][40];							/* The names in the declaratives.	*/
EXT char perf_decl[MAX_PARAGRAPHS][40];							/* The performs in the Declaratives.	*/
EXT char perf_pdiv[MAX_PARAGRAPHS][40];							/* The performs from the proc div.	*/

#define MAX_BLANK_ITEMS		100
EXT char blank_item[MAX_BLANK_ITEMS][40];						/* Screen items to be blank when zero.	*/

#define MAX_FIGCONS2		255							/* Maximum nuber of 2 byte fig cons.	*/

EXT char fig_cons[MAX_FIGCONS2][32];							/* names of figcons that are 2 bytes	*/
EXT int  fig_val [MAX_FIGCONS2][2];							/* their values				*/
EXT int  fig_count INIT_FALSE;								/* the counter				*/

EXT int  dump_ifbuff	INIT_FALSE;							/* Currently dumping the IF buffer.	*/
EXT int  re_exec	INIT_FALSE;							/* WISP re-execution flag.		*/
EXT int  no_output	INIT_FALSE;							/* Don'e generate output.		*/
EXT int  else_flag	INIT_FALSE;							/* Check for ELSE ELSE.			*/
EXT int  has_link	INIT_FALSE;							/* Did we process a call to LINK?	*/
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
c_list *xref_ptr;									/* Pointer to cross ref list.		*/

#define COMMENT_BUFFER_SIZE 4096
EXT char com_buf[COMMENT_BUFFER_SIZE]	INIT_FALSE;					/* Buffer to hold pending comments.	*/
EXT char *com_ptr;									/* Pointer to current end of comment buf*/

#define STRING_BUFFER_SIZE 4096

EXT char pf_str[STRING_BUFFER_SIZE];							/* Buffer for ON PFKEY phrases		*/
EXT char nm_str[STRING_BUFFER_SIZE];							/* Buffer for NO-MOD phrases		*/
EXT char rd_str[STRING_BUFFER_SIZE];							/* Buffer for READ statements.		*/
EXT char tm_str[STRING_BUFFER_SIZE];							/* Buffer for TIMEOUT statements.	*/
EXT char *d_ptr;									/* pointer to current buffer in use	*/
EXT char on_str[72];									/* Buffer for the ON PFKEY key list	*/

#define MAX_CRT_RECORDS		40

EXT char crt_record[MAX_CRT_RECORDS][40];						/* the list of possible crt records	*/
EXT int  crt_size[MAX_CRT_RECORDS];							/* the size of these records		*/
EXT int  crt_count INIT_FALSE;

EXT int num_screens INIT_FALSE;								/* How many screen items so far		*/

EXT int io_flag   	INIT_FALSE;							/* INPUT-OUTPUT SECTION flag		*/
EXT int file_sect 	INIT_FALSE;							/* FILE SECTION flag			*/
EXT int oa_flag   	INIT_FALSE;							/* pending ORDER-AREA flag		*/
EXT int fac_flag  	INIT_FALSE;							/* pending FAC flag			*/
EXT int facnum    	INIT_FALSE;							/* the number of the last fac done	*/
EXT int fillernum 	INIT_FALSE;							/* Current number to append to FILLER	*/
EXT int readnum	  	INIT_FALSE;							/* current paragraph number for READ's	*/
EXT int did_figcons	INIT_FALSE;							/* Did we generate figurative const.	*/
EXT int in_decl		INIT_FALSE;							/* Are we in the declaratives section?	*/
EXT int has_lit		INIT_FALSE;							/* Does the current line have a literal?*/
EXT int isalit		INIT_FALSE;							/* Is the current input open literal?	*/
EXT int isaproc		INIT_FALSE;							/* Is the next OPEN for a procedure?	*/
EXT int has_cont	INIT_FALSE;							/* Does this line have a continuation?	*/
EXT int ws_blank	INIT_TRUE;							/* Allow BLANK WHEN ZERO in WS.		*/
EXT int trans_lib	INIT_FALSE;							/* Don't translate library names.	*/
EXT int held_line	INIT_FALSE;							/* Current line was a held line.	*/
EXT int decl_stop_exit	INIT_FALSE;							/* STOP RUN or EXIT PROGRAM in DECLAR	*/

EXT int autolockprint	INIT_FALSE;							/* VMS use automatic locking for print	*/
EXT int autolockfile	INIT_FALSE;							/* VMS use automatic locking next file	*/
EXT int compressfile	INIT_FALSE;							/* Add file compression.		*/
EXT int nooptional	INIT_FALSE;							/* Don't add OPTIONAL clause.		*/
EXT int sortfile	INIT_FALSE;							/* Next file is a SORT file		*/
EXT int seqline		INIT_FALSE;							/* Force to LINE SEQUENTIAL		*/
EXT int seqbinary	INIT_FALSE;							/* Force to BINARY SEQUENTIAL		*/
EXT int linkmain	INIT_FALSE;							/* Flag to add LINK frontend stuff	*/
EXT int multiplelock	INIT_FALSE;							/* Flag to add WITH LOCK ON MULTIPLE	*/

EXT int g_newline	INIT_FALSE;							/* Last get_parm() read a newline.	*/

EXT int division INIT_FALSE;								/* Which division are we scanning?	*/

#define IDENTIFICATION_DIVISION		0
#define ENVIRONMENT_DIVISION		1
#define INPUT_OUTPUT_SECTION		2
#define DATA_DIVISION			3
#define WORKING_STORAGE_SECTION		4
#define PROCEDURE_DIVISION		5

EXT int sect_num INIT_FALSE;								/* Current SECTION number.		*/
EXT int pmode INIT_FALSE;								/* Parsing mode (if any)		*/
EXT int copy_sect INIT_FALSE;								/* SECTION copy mode.			*/
EXT int copy_para INIT_FALSE;								/* Paragraph copying mode.		*/
EXT int copy_decl INIT_FALSE;								/* Declarative copy mode.		*/
EXT int ptype;										/* the parm type (first last middle)	*/
EXT int save_inline INIT_FALSE;								/* flag to re-use input lines		*/
EXT int skip_kw   INIT_FALSE;								/* Skip keyword processing once.	*/
EXT int kwproc INIT_TRUE;								/* flag to stop VAX keyword processing.	*/
EXT int d_period  INIT_FALSE;								/* flag to indicate a period is found	*/
EXT int pfkeys	  INIT_FALSE;								/* flag to indicate PFKEYS phrase	*/
EXT int no_mod    INIT_FALSE;								/* flag to indicate a NO-MOD		*/
EXT int on_pfkeys INIT_FALSE;								/* flag to indicate an ON PFKEYS	*/
EXT int keepstop  INIT_FALSE;								/* Keep STOP RUN statements.		*/
EXT int trap_start INIT_FALSE;								/* Trap START timeouts.			*/

/* 						Constants for PMODE								*/

#define WS_SCREEN			1
#define FIG_CONS			2
#define DISPLAY				3
/*						Common variables								*/

EXT int open_files INIT_FALSE;								/* Current number of open in files	*/
EXT int out_files INIT_FALSE;								/* Current number of open out files	*/

EXT FILE *infile;									/* Current input file			*/
EXT int  outfile;									/* Current output file			*/
											/* NOTE: uses UNIX IO for speed		*/
EXT FILE *par_file INIT_FALSE;								/* The lib file to hold paragraphs.	*/
EXT FILE *decl_file INIT_FALSE;								/* The lib file to hold declaratives.	*/
EXT FILE *opt_file INIT_FALSE;								/* The option file.			*/

EXT int do_optfile INIT_FALSE;								/* Flag to process the option file.	*/

#define OUTPUT_BUFFER_SIZE 30720

EXT int  fo_cnt INIT_FALSE;								/* The output file buffer count		*/
EXT char *fo_buff;									/* The output file buffer		*/
EXT char sprtemp[2048];									/* the sprintf temp buffer		*/
EXT int outlen INIT_FALSE;								/* The length of the current output line*/

EXT FILE *logfile;									/* Current logfile (stdout by default)	*/

#define MAX_INPUT_FILES		16

EXT FILE *file_ptrs[MAX_INPUT_FILES];							/* A list of ptrs for the input files	*/
EXT int o_file_ptrs[MAX_INPUT_FILES];							/* A list of descr for the output files	*/
EXT int o_skip[MAX_INPUT_FILES];							/* A list of skip file statuses		*/

EXT int p_parms[36];									/* the offset of the parms in the line	*/
EXT char parms [36] [81];								/* 36 parms				*/
EXT char o_parms [36] [81];								/* 36 output parms in this statement	*/

EXT char area_a[8];									/* The text found in area a		*/
EXT char mod_code[12];									/* The mod code if any.			*/
EXT char last_field[40];								/* The last field name seen.		*/

EXT int area_a_num;									/* The number it represents (if any)	*/
EXT int parm_num;									/* The current parm being scanned	*/

EXT int blanklines INIT_FALSE;								/* Blank lines are no no's		*/
EXT int comments INIT_FALSE;								/* Comments are not ok			*/
EXT int compress INIT_TRUE;								/* Compress screen output		*/
EXT int concat   INIT_FALSE;								/* Don't concatenate files		*/
EXT int skiplib	 INIT_FALSE;								/* Don't skip the libs.			*/
EXT int copylib  INIT_TRUE;								/* Generate copy libs			*/
EXT int copy_only INIT_FALSE;								/* don't just copy it, process it.	*/
EXT int logging INIT_FALSE;								/* no logging by default		*/
EXT int log_stats INIT_FALSE;								/* no stats either.			*/

EXT int  vax_cobol INIT_FALSE;								/* VAX COBOL				*/
EXT int  lpi_cobol INIT_FALSE;								/* LPI COBOL				*/
EXT int  acu_cobol INIT_FALSE;								/* ACUCOBOL 				*/
EXT int  aix_cobol INIT_FALSE;								/* AIX VS COBOL 			*/
EXT int  mf_cobol INIT_FALSE;								/* Micro Focus COBOL 			*/
EXT int  dmf_cobol INIT_FALSE;								/* Micro Focus MSDOS COBOL 		*/
EXT int  unix_cobol INIT_FALSE;								/* Any UNIX COBOL 			*/
EXT int  dos_cobol INIT_FALSE;								/* Any DOS COBOL 			*/
EXT char cobol_type[4];									/* The type of COBOL			*/
EXT int  mf_aix INIT_FALSE;								/* Micro Focus (or AIX) COBOL 		*/


EXT int init_move INIT_FALSE;								/* don't change move spaces to initial	*/
EXT int init_data INIT_TRUE;								/* program id is initial.		*/
EXT int do_locking INIT_TRUE;								/* generate record locking logic	*/
EXT int delete_code INIT_FALSE;								/* not currently deleting source code	*/
EXT int del_use    INIT_FALSE;								/* not deleting a USE procedure.	*/
EXT int copy_code   INIT_FALSE;								/* not currently copying commented src	*/
EXT int fdfiller   INIT_TRUE;								/* allow fillers in fd's		*/
EXT int wsfiller   INIT_TRUE;								/* allow fillers in working-storage	*/
EXT int do_keyfile INIT_FALSE;								/* Don't look for key files.		*/
EXT int scount;										/* global screen counter		*/
EXT int r_count;									/* The  count for the current RANGE	*/
EXT int proc_display INIT_TRUE;								/* Process DISPLAY statements.		*/

EXT int num_inlines INIT_FALSE;								/* the number of input lines read	*/
EXT int num_comments INIT_FALSE;							/* the number of comments		*/
EXT int out_number INIT_FALSE;								/* the current output line number	*/

EXT int num_main INIT_FALSE;								/* The number of input lines from main	*/
EXT int num_copy INIT_FALSE;								/* The num input lines from current cpy	*/
EXT char main_name[80];
EXT char copy_name[80];

EXT int item_level INIT_FALSE;								/* the level number of screen items	*/
EXT int cur_v1 INIT_FALSE;								/* the current vertical 1 occurs count	*/
EXT int cur_v2 INIT_FALSE;								/* the current vertical 2 occurs count	*/
EXT int cur_h INIT_FALSE;								/* the current horizontal occurs count	*/

EXT int cur_row INIT_FALSE;								/* current output row			*/
EXT int cur_col INIT_FALSE;								/* current output column		*/

EXT int lock_clear_para INIT_FALSE;							/* Gen WISP-RECORD-LOCK-CLEAR para	*/

EXT char hard_lock[5];									/* The file status for hard lock	*/
EXT char soft_lock[5];									/* The file status for soft lock	*/
EXT char bin2_type[8];									/* The thing to convert BINARY to.	*/
EXT char bin4_type[8];									/* The thing to create 4 byte binary.	*/
EXT char packdec[8]; 									/* The thing to convert COMP to		*/

EXT int  wrote_special_names INIT_FALSE;						/* Was SPECIAL-NAMES written		*/


#define QUOTE_CHAR '"'
#define QUOTE_STR  "\""


