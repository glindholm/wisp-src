/*
******************************************************************************
**
** $Id:$
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

static	char 	*SORT_VERSION = "SyncSort SORT Program - Version 1.01.02";

/*
**	File:		sswsort.c
**
**	Project:	SyncSort version of wsort
**
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <signal.h>
#include <errno.h>

#ifdef WIN32
#define popen _popen
#define pclose _pclose
#endif

#ifdef WISP4400
/*
**	If using WISP 44xx then you need to define WISP4400 to translate the API.
*/
#define	WL_wtrace	wtrace
#define	WL_initglbs	initglbs
#define WL_makepath	makepath
#define WL_use_last_prb use_last_prb
#define WVASET		wvaset
#define WFNAME		wfname
#define SETRETCODE	setretcode
#endif /* WISP4400 */

/* #include "intdef.h" */
typedef short		int2; 
typedef unsigned short	uint2;  
typedef int		int4;
typedef unsigned int	uint4;


/*
**	External functions
*/

/* VSSUBS */
extern void EXTRACT(const char* keyword, ...);
extern void FIND(char* the_file, char* the_lib, char* the_vol, int4 *starter, int4 *counter, char* receiver, ...);
extern void SCRATCH(char *tflag, char *fname, char *lib, char *vol, ...);
extern void READFDR4(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, ...);
#define READFDR_RC_24_NO_FILE_HEADER		24
extern void GETPARM();
#define GETPARM_MAX_ARGS	500

/* WISPLIB Public API routines */
extern void WISPEXIT(void);
extern void WVASET(int *x);
extern char *WFNAME(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);
#define WTITLE_TITLE_LEN 80
extern void WTITLE(const char the_title[WTITLE_TITLE_LEN]);
#define WANG_RETCODE_LEN 3
extern void SETRETCODE(const char code[WANG_RETCODE_LEN]);

/* WISPLIB Private API routines */
extern void wisp_signal_handler(void);
extern int  WL_initglbs(const char *wisprunname);
extern int  WL_makepath(const char* fullpath );
extern void WL_use_last_prb(void);
extern void WL_wtrace(const char* routine, const char* code, const char* format, ... /* args */);

/*
**	Structures and Defines
*/

/* #include "wangkeys.h" */
#define PFKEY_1_ENABLED  0x80000000
#define PFKEY_2_ENABLED  0x40000000
#define PFKEY_3_ENABLED  0x20000000
#define PFKEY_4_ENABLED  0x10000000
#define PFKEY_5_ENABLED  0x08000000
#define PFKEY_6_ENABLED  0x04000000
#define PFKEY_7_ENABLED  0x02000000
#define PFKEY_8_ENABLED  0x01000000
#define PFKEY_9_ENABLED  0x00800000
#define PFKEY_10_ENABLED 0x00400000
#define PFKEY_11_ENABLED 0x00200000
#define PFKEY_12_ENABLED 0x00100000
#define PFKEY_13_ENABLED 0x00080000
#define PFKEY_14_ENABLED 0x00040000
#define PFKEY_15_ENABLED 0x00020000
#define PFKEY_16_ENABLED 0x00010000
#define PFKEY_17_ENABLED 0x00008000
#define PFKEY_18_ENABLED 0x00004000
#define PFKEY_19_ENABLED 0x00002000
#define PFKEY_20_ENABLED 0x00001000
#define PFKEY_21_ENABLED 0x00000800
#define PFKEY_22_ENABLED 0x00000400
#define PFKEY_23_ENABLED 0x00000200
#define PFKEY_24_ENABLED 0x00000100
#define PFKEY_25_ENABLED 0x00000080
#define PFKEY_26_ENABLED 0x00000040
#define PFKEY_27_ENABLED 0x00000020
#define PFKEY_28_ENABLED 0x00000010
#define PFKEY_29_ENABLED 0x00000008
#define PFKEY_30_ENABLED 0x00000004
#define PFKEY_31_ENABLED 0x00000002
#define PFKEY_32_ENABLED 0x00000001

#define ENTER_KEY_PRESSED '@'
#define PFKEY_1_PRESSED   'A'
#define PFKEY_2_PRESSED   'B'
#define PFKEY_3_PRESSED   'C'
#define PFKEY_4_PRESSED   'D'
#define PFKEY_5_PRESSED   'E'
#define PFKEY_6_PRESSED   'F'
#define PFKEY_7_PRESSED   'G'
#define PFKEY_8_PRESSED   'H'
#define PFKEY_9_PRESSED   'I'
#define PFKEY_10_PRESSED  'J'
#define PFKEY_11_PRESSED  'K'
#define PFKEY_12_PRESSED  'L'
#define PFKEY_13_PRESSED  'M'
#define PFKEY_14_PRESSED  'N'
#define PFKEY_15_PRESSED  'O'
#define PFKEY_16_PRESSED  'P'
#define PFKEY_17_PRESSED  'a'
#define PFKEY_18_PRESSED  'b'
#define PFKEY_19_PRESSED  'c'
#define PFKEY_20_PRESSED  'd'
#define PFKEY_21_PRESSED  'e'
#define PFKEY_22_PRESSED  'f'
#define PFKEY_23_PRESSED  'g'
#define PFKEY_24_PRESSED  'h'
#define PFKEY_25_PRESSED  'i'
#define PFKEY_26_PRESSED  'j'
#define PFKEY_27_PRESSED  'k'
#define PFKEY_28_PRESSED  'l'
#define PFKEY_29_PRESSED  'm'
#define PFKEY_30_PRESSED  'n'
#define PFKEY_31_PRESSED  'o'
#define PFKEY_32_PRESSED  'p'



#define MAXRECSIZE	2048
#define MAXSORTINFILES	20
#define	MAXSORTKEYS	8
#define MAXSORTSELECTS 	32


#define	SIZEOF_FILE		8
#define SIZEOF_LIB		8
#define SIZEOF_VOL		6
#define COB_FILEPATH_LEN	80

#define RECSIZE_FIELD_LEN	4
#define YESNO_FIELD_LEN		3

#define FILEORG_FIELD_LEN	1
#define RECTYPE_FIELD_LEN	1

#define NUMKEYS_FIELD_LEN	1

#define FLDPOS_FIELD_LEN	4
#define FLDLEN_FIELD_LEN	3
#define FLDTYP_FIELD_LEN	1

#define TSTREL_FIELD_LEN	2
#define TSTVAL_FIELD_LEN	18
#define TSTCON_FIELD_LEN	3

#define ORDER_FIELD_LEN		1

struct wang_file_s
{
	unsigned int	recsize;		/* Record size 			*/
	char	file[SIZEOF_FILE+1];		/* Wang FILE name		*/
	char	lib[SIZEOF_LIB+1];		/* Wang LIBRARY name		*/
	char	vol[SIZEOF_VOL+1];		/* Wang VOLUME name		*/
	char	filename[COB_FILEPATH_LEN+1];	/* Native file name		*/
	char	fileorg;			/* Organization (C/F, R, or I) Consecutive/Fixed is the same */
	char	rectype;			/* Record type (F ov V)		*/
};

enum e_sortfunc { FUNC_SORT, FUNC_MERGE };
struct options_getparm_s
{
	enum e_sortfunc	function_flag;	/* SORT or MERGE		*/
	int	stable_flag;		/* STABLE=TRUE or FALSE		*/
	int	reformat_flag;		/* REFORMAT=TRUE or FALSE	*/
};

struct input_getparms_s
{
	int	select_flag;		/* SELECT=TRUE or FALSE		*/
	unsigned int	infile_count;	/* Number of input files	*/
	unsigned int	min_recsize;	/* Minimum record size		*/
	unsigned int	max_recsize;	/* Maximum record size		*/
	struct wang_file_s list[MAXSORTINFILES];
};

enum e_tstrel {REL_UNKNOWN, REL_EQ, REL_NE, REL_GT, REL_LT, REL_GE, REL_LE };
static struct 
{
	char	*relstr;
	enum e_tstrel tstrel;
} rel_tab[] = 
{
	{ "EQ", REL_EQ },
	{ "NE", REL_NE },
	{ "GT", REL_GT },
	{ "GE", REL_GE },
	{ "LT", REL_LT },
	{ "LE", REL_LE },
	{ NULL, REL_UNKNOWN }
};

enum e_tstcon {CON_UNKNOWN, CON_AND, CON_OR, CON_BLANK};
	
struct select_getparms_s
{
	unsigned int	selcnt;
	struct 
	{
		unsigned int fldpos;			/* Test field starting position */
		unsigned int fldlen;			/* Test field lenght */
		char fldtyp;				/* Test field type */
		enum e_tstrel tstrel;			/* Test relationship to either tstval or tstpos */
		char tstval[TSTVAL_FIELD_LEN+1];	/* Test constant string (or empty string if tstpos!=0) */
		unsigned int tstpos;			/* Position of the test field (or 0 if a tstval ) */
		enum e_tstcon tstcon;			/* AND or OR for next select test */
	} selinfo[MAXSORTSELECTS];
};

struct keys_getparm_s
{
	unsigned int	keycnt;
	struct
	{
		unsigned int fldpos;
		unsigned int fldlen;
		char fldtyp;
		char order;
	} keyinfo[MAXSORTKEYS];
};

#define MAXREFORMATFIELDS	100
struct format_getparms_s
{
	unsigned int	recsize;
	char		pad_char;
	unsigned int	field_cnt;
	struct
	{
		unsigned int	inpos;
		unsigned int	len;
		unsigned int	outpos;
	} field[MAXREFORMATFIELDS];
};

struct output_getparm_s
{
	int	replace_flag;
	struct wang_file_s o;
};


#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

/*
**	GETPARM macros
*/
#define GP				gp_args[gp_cnt++] = (char *)
#define	GPNUM(num)			GP &N[num]
#define	GPAT(row,col)			GP "A";GPNUM(row);GP "A";GPNUM(col)
#define	GPFLD(fld,len,row,col)		GP fld;GPNUM(len);GPAT(row,col)
#define	GPTEXT(txt,ln,rw,cl)		GP "T";GPFLD(txt,ln,rw,cl)
#define	GPCTEXT(txt,rw,cl)		GPTEXT(txt,(strlen(txt)),rw,cl)
#define	GPCTEXTU(txt,rw,cl)		GP "U";GPFLD(txt,(strlen(txt)),rw,cl)
#define	GPKW(x,kw,rcv,ln,rw,cl,typ)	GP x;GP kw;GPFLD(rcv,ln,rw,cl);GP typ
#define	GPID(pn,pf,mn,mi)		GP pn;GP pf;GP mn; GP mi
#define	GPTYP(tp,rq)			GP tp;GP rq
#define	GPTOP(tp,rq,pn,pf,mn,mi,mc)	GPTYP(tp,rq);GPID(pn,pf,mn,mi);GPNUM(mc);
#define GPMSG(ln)                       GP ln; GPNUM(strlen(ln))

#define	GPPFS(x)			GP "P";GP x
#define	GPENTER()			GP "E"
#define	GPNOENTER()			GP "N"


#define GP_PRNAME_LEN	8
#define GP_MESSID_LEN	4
#define GP_ISSUER_LEN	6
#define GP_GPTYPE_LEN	2

#define ISSUER_SORT	"SORT  "

char WISPFILEXT[39];	/* Define the file extension variable.	*/
char WISPRETURNCODE[3];	/* Define the return code field.	*/

/*
**	Static data
*/
static int debug_mode = 0;

static	int4	N[255];
static char* gp_args[GETPARM_MAX_ARGS];
static int gp_cnt;

static char *terminate_text = "Press (16) to terminate SORT.";

/*
**	Usage constant defaults
*/
static char def_inlib[SIZEOF_LIB+1];
static char def_invol[SIZEOF_VOL+1];
static char def_outlib[SIZEOF_LIB+1];
static char def_outvol[SIZEOF_VOL+1];
static char def_wrklib[SIZEOF_LIB+1];
static char def_wrkvol[SIZEOF_VOL+1];

/*
**	Static Function Prototypes
*/
struct varstr_s
{
	size_t	size;
	char*	str;
};
static char* varstr_cat(struct varstr_s *buf, const char* addition);
static void varstr_printf(struct varstr_s *str, const char* format, ... /* args */);
static void varstr_free(struct varstr_s **buf);
static struct varstr_s *varstr_new(void);

static int wsort(void);
static int get_options(struct options_getparm_s *options);
static int get_input(enum e_sortfunc function_flag, struct input_getparms_s *input);
static int get_select(unsigned int reclen, struct select_getparms_s *select);
static int get_keys(unsigned int reclen, struct keys_getparm_s *keys);
static int get_reformat(unsigned int reclen, struct format_getparms_s *reformat);
static int get_output(struct output_getparm_s *output,  
		      struct input_getparms_s *input,
		      int reformat_flag);

static int perform_syncsort(struct options_getparm_s *options,
			    struct input_getparms_s *input,
			    struct select_getparms_s *select,
			    struct keys_getparm_s *keys,
			    struct format_getparms_s *reformat,
			    struct output_getparm_s *output,
			    struct varstr_s *outstr);

static int run_command(const char* command, struct varstr_s *outstr);

static void leftjust(char *ptr, int cnt);
static int  field2uint(const char *str, unsigned int len, unsigned int *num);
static void uint2field(unsigned int num, char *field, unsigned int len);
static void wswap(void *lword);
static void set_arg_count(int x);

static void error_getparm(	
	char prname[GP_PRNAME_LEN], char messid[GP_MESSID_LEN], char issuer[GP_ISSUER_LEN],
	char *msg1,char *msg2,char *msg3,char *msg4,
	char *msg5,char *msg6,char *msg7,char *msg8,
	char *msg9,char *msg10,char *msg11,char *msg12);

static void CALL_GETPARM2(char* args[], int args_count);




/*
**	Routine:	main()
**
**	Function:	Main routine for WSORT utility
**
**	Description:	This routine emulates the Wang SORT utility.
**
**				wsort
**
**	Arguments:	no arguments, use GETPARM interface
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
int main(int argc, char* argv[])
{
	int rc = 0;
	char the_title[WTITLE_TITLE_LEN+1] = {"SSB SORT"};

	/*
	**	Enable "debug" mode by:
	**	- compiling for DEBUG
	**	- setting "-d" command line option
	**	- setting $SSWSORTDEBUG envvar
	*/
#if defined(DEBUG) || defined(_DEBUG)
	debug_mode = 1;
#endif
	if ( argc > 1 && 0 == strcmp(argv[1],"-d"))
	{
		debug_mode = 1;
	}
	if ( NULL != getenv("SSWSORTDEBUG"))
	{
		debug_mode = 1;
	}

	/*
	**	Set the screen TITLE
	*/
	WTITLE(the_title);

	/*
	**	Call WL_initglbs() to setup for a WISP utility:
	**	- Record start time
	**	- Set the link level
	**	- Set the the process group id
	**	- Set the Run Name
	**	- Set the Program Id
	**	- trace the entry
	*/
	WL_initglbs("SORT    ");

#ifndef _DEBUG
	/*
	**	Call wisp_signal_handler() to setup the standard WISP signal catching.
	*/
	wisp_signal_handler();
#endif

	/*
	**	Get the default values
	*/
	set_arg_count(12);
	EXTRACT("IL",def_inlib, 
		"IV",def_invol,
		"OL",def_outlib,
		"OV",def_outvol,
		"WL",def_wrklib,
		"WV",def_wrkvol);

	
	/*
	**	Perform the SORT
	*/
	rc = wsort();

	/*
	**	Set the return code
	*/
	{
		char retcode[WANG_RETCODE_LEN+1];
		uint2field((unsigned int)rc, retcode, WANG_RETCODE_LEN);
		SETRETCODE(retcode);
	}

	/*
	**	Cleanup for EXIT
	*/
	WISPEXIT();


	return rc;
}

static void wsort_abort(char* message)
{
	error_getparm("ABORT   ", "0000", ISSUER_SORT, 
			message, NULL, NULL, NULL, 
			NULL, NULL, NULL, NULL, 
			NULL, NULL, NULL, NULL);

	WISPEXIT();
	abort();
}

/*
**	Routine:	wsort()
**
**	Function:	To provide a GETPARM interface like the Wang SORT utility.
**
**	Description:	To emulate the Wang SORT utility.
**
**			GETPARM flowchart:
**				- OPTIONS
**				- TABLE (OPTIONS: ALTSEQ=TABLE) - not supported
**				- INPUT
**				- LOCK (INPUT: SHARED=YES) - not supported
**				- SELECT (INPUT: SELECT=YES)
**				- KEYS
**				- FORMAT (OPTION: REFORMAT=YES)
**				- OUTPUT
**				- FILEKEYS (OUTPUT: FILEORG=I) - not supported
**
**
**	Arguments:	None
**
**	Return:
**	0		success
**	16		Sort cancelled
**	32		Sort ERROR
**
**	Warnings:	None
**
*/
static int wsort(void)
{
	struct options_getparm_s options;
	struct input_getparms_s  input;
	struct select_getparms_s select;
	struct keys_getparm_s    keys;
	struct format_getparms_s reformat;
	struct output_getparm_s  output;

	int	pfkey;
	int	i;


	/* Initialize a swapped numbers array for GP macros */
	for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 	
	{
		N[i]=i; 
		wswap(&N[i]);
	}
	
	/*
	**	OPTIONS getparm
	*/
	memset(&options,0,sizeof(options));
	options.function_flag = FUNC_SORT;	/* FUNCTION=SORT	*/
	options.stable_flag = FALSE;		/* STABLE  =NO		*/
	options.reformat_flag = FALSE;		/* REFORMAT=NO		*/

	pfkey = get_options(&options);
		
	if (16 == pfkey) 
	{
		return 16;
	}

	/*
	**	INPUT getparm(s)
	*/
	memset(&input,0,sizeof(input));
	input.select_flag = FALSE;		/* SELECT=NO		*/
	input.infile_count = 0;
	input.min_recsize = MAXRECSIZE;
	input.max_recsize = 0;

	pfkey = get_input(options.function_flag, &input);
	if (16 == pfkey)
	{
		return 16;
	}

	/*
	**	SELECT getparm(s)
	*/
	memset(&select,0,sizeof(select));
	select.selcnt = 0;

	if (input.select_flag)
	{
		pfkey = get_select(input.min_recsize, &select);
		if (16 == pfkey) 
		{
			return 16;
		}
	}

	/*
	**	KEYS getparm
	*/
	memset(&keys,0,sizeof(keys));
	keys.keycnt = 0;
	pfkey = get_keys(input.min_recsize, &keys);
	if (16 == pfkey)
	{
		return 16;
	}


	/*
	**	FORMAT getparms
	*/
	memset(&reformat,0,sizeof(reformat));
	reformat.field_cnt = 0;
	reformat.recsize = 0;
	reformat.pad_char = ' ';

	if (options.reformat_flag)
	{
		pfkey = get_reformat(input.min_recsize, &reformat);
		if (16 == pfkey)
		{
			return 16;
		}
	}

	/* 
	**	Getparm OUTPUT info
	*/
	memset(&output,0,sizeof(output));
	memset(output.o.file,' ',SIZEOF_FILE);
	memset(output.o.lib,' ',SIZEOF_LIB);
	memset(output.o.vol,' ',SIZEOF_VOL);
	output.replace_flag = FALSE;
	output.o.fileorg = 'C';
	output.o.rectype = 'F';
	
	if (options.reformat_flag)
	{
		output.o.recsize = reformat.recsize;
	}
	else
	{
		output.o.recsize = input.max_recsize;
	}

	pfkey = get_output(&output, &input, options.reformat_flag);
	switch(pfkey)
	{
	case 0:		break;
	case 1:		return 0;	/* Terminate without error */
	case 16:	return 16;
	default:	return 32;
	}


	/*
	**	PERFORM THE SORT <<<<<<<< HERE >>>>>>>>
	*/
	{
		int	retcode;
		struct varstr_s	*syncsort_output = varstr_new();

		retcode = perform_syncsort(&options, &input, &select, &keys, &reformat, &output, syncsort_output);


		/*
		**	TEST FOR ERRORS
		*/

		if (0!=retcode) 
		{
#define SSERRORLINES	10
#define SSERRLINELEN	79
			char op[SSERRORLINES][SSERRLINELEN+1];
			int i;
			char	errmess1[256];
			char *ptr = syncsort_output->str;
			for(i=0; i<SSERRORLINES; i++)
			{
				op[i][0] = '\0';
				if (ptr != NULL)
				{
					char *nl = strchr(ptr,'\n');
					
					if (nl != NULL)
					{
						*nl = '\0';
						if (strlen(ptr) > SSERRLINELEN)
						{
							ptr[SSERRLINELEN] = '\0';
						}
						
						strcpy(op[i], ptr);
						ptr = nl+1;
					}
					else if (strlen(ptr) <= SSERRLINELEN)
					{
						strcpy(op[i],ptr);
						ptr = NULL;
					}
					else
					{
						memcpy(op[i],ptr,SSERRLINELEN);
						op[i][SSERRLINELEN] = '\0';
						ptr += SSERRLINELEN;
					}
				}
			}

			sprintf(errmess1, "\224ERROR\204- RUN SYNCSORT FAILED: RETURN CODE [%d]",retcode);

			error_getparm("ERROR   ", "0000", "SORT  ", 
				errmess1, NULL, op[0], op[1], 
				op[2], op[3], op[4], op[5],
				op[6], op[7], op[8], op[9]);
		}

		varstr_free(&syncsort_output);

		return retcode;
	}
}

static const char*enum2tstrel(enum e_tstrel tstrel)
{
	unsigned int idx;

	for(idx=0; rel_tab[idx].relstr != NULL; idx++)
	{
		if (rel_tab[idx].tstrel == tstrel)
		{
			return rel_tab[idx].relstr;
		}
	}
	return "??";
}

/*
**	DEBUG MODE - Write the GETPARM data to a file
*/
static void print_getparm_info(struct options_getparm_s *options,
			    struct input_getparms_s *input,
			    struct select_getparms_s *select,
			    struct keys_getparm_s *keys,
			    struct format_getparms_s *reformat,
			    struct output_getparm_s *output)
{
	FILE *f;
	char *prname;
	unsigned int i;

	f = fopen("debug_ss.txt","w+");
	if (f == NULL)
	{
		f = stdout;
	}

	/*
	**	Print ALL INFO
	*/
	prname = "OPTIONS";
	fprintf(f,"%-8.8s: FUNCTION=%s\n", prname, options->function_flag==FUNC_SORT ? "SORT":"MERGE");
	fprintf(f,"%-8.8s: STABLE  =%s\n", prname, options->stable_flag ? "YES":"NO");
	fprintf(f,"%-8.8s: REFORMAT=%s\n", prname, options->reformat_flag ? "YES":"NO");


	prname = "INPUT";
	fprintf(f,"%-8.8s: SELECT  =%s\n", prname, input->select_flag ? "YES":"NO");
	fprintf(f,"%-8.8s: FILECNT =%d\n", prname, input->infile_count);
	fprintf(f,"%-8.8s: MINREC  =%d\n", prname, input->min_recsize);
	fprintf(f,"%-8.8s: MAXREC  =%d\n", prname, input->max_recsize);
	for(i=0; i<input->infile_count; i++)
	{
		fprintf(f,"%-8.8s:[%2d] FILE=%-8.8s LIB=%-8.8s VOL=%-6.6s\n", prname, i+1, 
			input->list[i].file, input->list[i].lib, input->list[i].vol);
		fprintf(f,"              FILENAME=%s\n", input->list[i].filename);
		fprintf(f,"              FILEORG=%c RECSIZE=%d\n", 
			input->list[i].fileorg, input->list[i].recsize);
	}

	if (input->select_flag)
	{
		prname = "SELECT";
		fprintf(f,"%-8.8s: COUNT  =%d\n", prname, select->selcnt);
		for(i=0; i<select->selcnt; i++)
		{
			const char *connect = "   ";

			if (select->selinfo[i].tstcon == CON_AND)
			{
				connect = "AND";
			}
			else if (select->selinfo[i].tstcon == CON_OR)
			{
				connect = "OR ";
			}

			fprintf(f,"%-8.8s: FLDPOS%-2d = %4d  LENGTH%-2d = %3d  FLDTYP%-2d = %c\n", 
				prname, i+1, select->selinfo[i].fldpos, 
					i+1, select->selinfo[i].fldlen, 
					i+1, select->selinfo[i].fldtyp);
			fprintf(f,"%-8.8s  TSTREL%-2d = \"%s\"  VALUE%-2d = \"%s\" or %d CONECT%-2d = \"%s\"\n", 
				" ",	i+1, enum2tstrel(select->selinfo[i].tstrel), 
					i+1, select->selinfo[i].tstval, select->selinfo[i].tstpos, 
					i+1, connect);
		}
	}

	prname = "KEYS";
	fprintf(f,"%-8.8s: COUNT  =%d\n", prname, keys->keycnt);
	for(i=0; i<keys->keycnt; i++)
	{
		fprintf(f,"%-8.8s: POST%d = %4d  LENGTH%d = %3d  TYPE%d = %c  ORDER%d = %c\n", 
			prname, i+1, keys->keyinfo[i].fldpos, 
				i+1, keys->keyinfo[i].fldlen, 
				i+1, keys->keyinfo[i].fldtyp, 
				i+1, keys->keyinfo[i].order);
	}

	if (options->reformat_flag)
	{
		prname = "FORMAT";
		fprintf(f,"%-8.8s: LENGTH   =%d\n", prname, reformat->recsize);
		fprintf(f,"%-8.8s: PAD      =0x%02x\n", prname, (int)reformat->pad_char);
		fprintf(f,"%-8.8s: FIELDCNT =%d\n", prname, reformat->field_cnt);
		for(i=0; i<reformat->field_cnt; i++)
		{
			fprintf(f,"%-8.8s: FIELD%-2d: INPOS%-2d = %4d  LENGTH%-2d = %4d  OUTPOS%-2d = %4d\n", 
				prname, i+1,
					i+1, reformat->field[i].inpos, 
					i+1, reformat->field[i].len, 
					i+1, reformat->field[i].outpos);
		}
	}

	prname = "OUTPUT";
	fprintf(f,"%-8.8s: FILE     =%-8.8s LIBRARY=%-8.8s VOLUME=%-6.6s\n", prname,
		output->o.file, output->o.lib, output->o.vol);
	fprintf(f,"%-8.8s: FILENAME =%s\n", prname, output->o.filename);
	fprintf(f,"%-8.8s: REPLACE  =%s\n", prname, output->replace_flag?"YES":"NO");
	fprintf(f,"%-8.8s: FILEORG  =%c\n", prname, output->o.fileorg);
	fprintf(f,"%-8.8s: RECSIZE  =%d\n", prname, output->o.recsize);
	fprintf(f,"%-8.8s: RECTYPE  =%c\n", prname, output->o.rectype);

	if (f != stdout)
	{
		fclose(f);
	}
}


/*
**	Routine:	fldtyp2format()
**
**	Function:	Get the syncsort format type from a Wang style field type.
**
**	Description:	Lookup the field type, return NULL if not found.
**			Valid Wang field type is one of "BCDFPZLU"
**
**			Type				Format
**			B Binary			INTEGER
**			C Char				CHARACTER
**			D Decimal (separate)		LS or TS or AN ??????
**			F Float				DFLOAT (8) or FLOAT (4)
**			P Packed Decimal (trailing)	PD
**			Z Zoned decimal (trailing)	ZD
**			L Zoned decimal (leading)	LZ
**			U Unsigned Binary		UINTEGER
**
**			NOTE: "D" is not supported
**
**	Arguments:
**	fldtyp		The Wang style field type
**	len		The lenght of the field
**
**	Return:		Pointer to the syncsort format string or NULL.
**
*/
static const char* fldtyp2format(char fldtyp, unsigned int len)
{
	static char *format = NULL;
	switch(fldtyp)
	{
	case 'B':	format = "INTEGER";	break;
	case 'C':	format = "CHARACTER";	break;
	case 'F':	
		if (len == 4)
		{
			format = "FLOAT";
		}
		else if (len == 8)
		{
			format = "DFLOAT";
		}
		break;
	case 'P':	format = "PD";		break;
	case 'Z':	format = "ZD";		break;
	case 'L':	format = "LZ";		break;
	case 'U':	format = "UINTEGER";	break;
	default:	format = NULL;		break;
	}
	return format;
}

/*
**	Routine:	perform_syncsort()
**
**	Function:	Perform the SORT/MERGE using SyncSort
**
**	Description:	Build the syncsort command then run it.
**
**	Arguments:
**	options		The OPTIONS getparm parameters
**	input		The INPUT getparm(s) parameters
**	select		The SELECT getparm(s) parameters
**	keys		The KEYS getparm parameters
**	reformat	The FORAMT getparm(s) parameters
**	output		The OUTPUT getparm parameters
**	outstr		The captured stdout/stderr text from SyncSort
**
**	Return:		The return code from syncsort
**
*/
static int perform_syncsort(struct options_getparm_s *options,
			    struct input_getparms_s *input,
			    struct select_getparms_s *select,
			    struct keys_getparm_s *keys,
			    struct format_getparms_s *reformat,
			    struct output_getparm_s *output,
			    struct varstr_s *outstr)
{
	unsigned int i;
	char buff[2048];
	struct varstr_s *cmd = varstr_new();

	if (debug_mode)
	{
		print_getparm_info(options,input,select,keys,reformat,output);
	}

	/*
		syncsort
	*/
	varstr_printf(cmd,"syncsort \n");
	varstr_printf(cmd," /SILENT \n");		/* Not sure this does anything useful */
	varstr_printf(cmd," /WARNINGS 10 \n");		/* Limit to 10 warnings */
	varstr_printf(cmd," /NOPROMPT \n");		/* Non-interactive usage */
	varstr_printf(cmd," /NOINTERRUPT \n");		/* Used with /NOPROMPT to disble interrupt trapping */

	/*
		FUNCTION=MERGE
		/MERGE
	*/
	if (options->function_flag == FUNC_MERGE)
	{
		varstr_printf(cmd," /MERGE \n");
	}
	else if (keys->keycnt == 0)
	{
		/*
		**	If no keys and not a MERGE then its a COPY
		*/
		varstr_printf(cmd," /COPY \n");
	}

	/*
		STABLE=YES
		/STABLE
	*/
	if (options->stable_flag)
	{
		varstr_printf(cmd," /STABLE \n");
	}

	/*
		FILEORG=I
		/INFILE {filename} INDEXED FIXED

		FILEORG=C/F
		/INFILE {filename} SEQUENTIAL FIXED {recsize}

  		FILEORG=C/L
		/INFILE {filename} SEQUENTIAL MFLINESEQUENTIAL {recsize}

	*/
	for(i=0; i<input->infile_count; i++)
	{
		if (input->list[i].fileorg == 'I')
		{
			varstr_printf(cmd," /INFILE \"%s\" INDEXED FIXED \n", 
				input->list[i].filename);
		}
		else if (input->list[i].fileorg == 'L' )
		{
			varstr_printf(cmd," /INFILE \"%s\" MFLINESEQUENTIAL %d \n", 
				input->list[i].filename, input->list[i].recsize);
		}
		else if (input->list[i].fileorg == 'C' || input->list[i].fileorg == 'F' )
		{
			varstr_printf(cmd," /INFILE \"%s\" SEQUENTIAL FIXED %d \n", 
				input->list[i].filename, input->list[i].recsize);
		}
		else
		{
			sprintf(buff,"Unsupported FILEORG=[%c]",input->list[i].fileorg);
			wsort_abort(buff);
		}
	}

	/*
		SELECT
		/FIELDS
		/CONDITION condition_name
		/INCLUDE condition_name
	*/
	if (input->select_flag && select->selcnt > 0)
	{
		char field_compare[60]; /* 11 char name + 5 char connect + 20 value + 6 paren = 42 */
		char condition[2048];	/* 32 field_compare's * 42  == 1344 */

#define SEL_FIELD_PREFIX	"sel_field"
#define TST_FIELD_PREFIX	"tst_field"
#define SELECT_CONDITION	"select_condition"
		/*
			First define the select fields and the test fields 

			/FIELDS sel_field%d {pos} {len} {format}
		*/
		for(i=0; i<select->selcnt; i++)
		{
			const char *format = fldtyp2format(select->selinfo[i].fldtyp, select->selinfo[i].fldlen);
			if (format == NULL)
			{
				sprintf(buff,"Unsupported SELECT TYPE=[%c]", select->selinfo[i].fldtyp);
				wsort_abort(buff);
			}
			varstr_printf(cmd," /FIELDS %s%d %d %d %s \n", SEL_FIELD_PREFIX, i+1,
				select->selinfo[i].fldpos, select->selinfo[i].fldlen, format);

			/*
			**	If the value (tstpos) field contains the starting position
			**	of another field then need to define this value field.
			*/
			if (select->selinfo[i].tstpos != 0)
			{
				varstr_printf(cmd," /FIELDS %s%d %d %d %s \n", TST_FIELD_PREFIX, i+1,
					select->selinfo[i].tstpos, select->selinfo[i].fldlen, format);
			}
		}

		/*
			Define the condition(s)

			/CONDITION select_condition  field_compare [{AND OR} field_compare]...

			field_compare ==
				( sel_field%d {tstrel} tst_field%d ) 
				( sel_field%d {tstrel} "tstval" )
		*/

		condition[0] = '\0';
		for(i=0; i<select->selcnt; i++)
		{
			char value[50];

			if (select->selinfo[i].tstpos != 0)
			{
				sprintf(value, "%s%d", TST_FIELD_PREFIX, i+1);
			}
			else
			{
				strcpy(value, select->selinfo[i].tstval);
			}

			/*
			**	field_compare == "( sel_field%d {rel} value )"
			*/
			sprintf(field_compare, "( %s%d %s %s )",
				SEL_FIELD_PREFIX, i+1,
				enum2tstrel(select->selinfo[i].tstrel),
				value);

			strcat(condition, field_compare);

			if (select->selinfo[i].tstcon == CON_AND)
			{
				strcat(condition," AND ");
			}
			else if (select->selinfo[i].tstcon == CON_OR)
			{
				strcat(condition," OR ");
			}
			else if (select->selinfo[i].tstcon == CON_BLANK)
			{
				/*
				**	if connect is blank then this 
				**	must be the last condition
				*/
				if (i+1 != select->selcnt)
				{
					wsort_abort("MISSING SELECT CONNECTOR");
				}
			}
			else
			{
				wsort_abort("UNKNOWN SELECT CONNECTOR");
			}
		}

		varstr_printf(cmd," /CONDITION %s %s \n", SELECT_CONDITION, condition);
		/*
		**	Include the records with this condition
		*/
		varstr_printf(cmd," /INCLUDE %s \n", SELECT_CONDITION);

	}


	/*
		KEYS
	*/
	if (keys->keycnt > 0)
	{
#define KEY_FIELD_PREFIX	"key_field"
		/*
			/FIELDS key_field%d {pos} {len} {format}
		*/
		for(i=0; i<keys->keycnt; i++)
		{
			const char *format = fldtyp2format(keys->keyinfo[i].fldtyp, keys->keyinfo[i].fldlen);
			if (format == NULL)
			{
				sprintf(buff,"Unsupported KEY TYPE=[%c]", keys->keyinfo[i].fldtyp);
				wsort_abort(buff);
			}
			varstr_printf(cmd," /FIELDS %s%d %d %d %s \n", KEY_FIELD_PREFIX, i+1, 
				keys->keyinfo[i].fldpos, keys->keyinfo[i].fldlen, format);
		}
		/*
			/KEYS key_field%d  [ASCENDING|DESCENDING] (,...)
		*/
		varstr_printf(cmd," /KEYS");
		for(i=0; i<keys->keycnt; i++)
		{
			varstr_printf(cmd, "%s %s%d %s", (i>0)? ",":"", KEY_FIELD_PREFIX, i+1, 
				(keys->keyinfo[i].order != 'D') ? "ASCENDING":"DESCENDING");
		}
		varstr_printf(cmd," \n");
	}

	/*
		OUTPUT:

		- REPLACE=YES will need the OVERWITE clause
		- ##TEMP files need the OVERWRITE clause sense the file
		will exist.

		/OUTFILE {filename} [OVERWRITE] SEQUENTIAL FIXED {recsize}

  		/OUTFILE {filename} [OVERWRITE] SEQUENTIAL MFLINESEQUENTIAL {recsize}

	*/
	if (output->o.recsize > 0)
	{
		int overwrite = FALSE;

		/*
		**	REPLACE=YES
		*/
		if (output->replace_flag)
		{
			overwrite = TRUE;
		}

		/*
		**	##TEMP files (anything in the worklib) gets an OVERWRITE
		*/
		if (0==memcmp(output->o.lib,def_wrklib,SIZEOF_LIB) &&
		    0==memcmp(output->o.vol,def_wrkvol,SIZEOF_VOL) )
		{
			overwrite = TRUE;
		}

		if( input->list[i].fileorg == 'L' )
		{
			varstr_printf(cmd," /OUTFILE \"%s\" %s SEQUENTIAL MFLINESEQUENTIAL %d \n", 
				output->o.filename, 
				(overwrite?"OVERWRITE":""),
				output->o.recsize);
		}
		else
		{
			varstr_printf(cmd," /OUTFILE \"%s\" %s SEQUENTIAL FIXED %d \n", 
				output->o.filename, 
				(overwrite?"OVERWRITE":""),
				output->o.recsize);
		}
	}
	else
	{
		sprintf(buff,"Invalid OUTPUT RECSIZE=[%d]",output->o.recsize);
		wsort_abort(buff);
	}

	/*
		REFORMAT=YES

		NOTE: The /REFORMAT option must appear *AFTER* the /OUTFILE option

		Define fields for each of the format fields.
		Derived fields from pad chars for each of the gaps in the format.

		/FIELDS
		/DERIVEDFIELD
		/REFORMAT
	*/
	if (options->reformat_flag && reformat->field_cnt > 1)
	{
#define REFORMAT_FIELD_PREFIX	"reformat_field"
#define PAD_FIELD_PREFIX	"pad_field"
		unsigned int nextpos = 1;
		unsigned int foundpos = 0;
		struct varstr_s *reformat_cmd = varstr_new();

		for(i=0; i<reformat->field_cnt; i++)
		{
			/*
			**	define a field for each reformat field 
			*/
			varstr_printf(cmd," /FIELDS %s_%d_%d %d %d CHARACTER \n", 
				REFORMAT_FIELD_PREFIX, 
				reformat->field[i].outpos, reformat->field[i].len,
				reformat->field[i].inpos, reformat->field[i].len);
		}

		/*
		**	Build the /REFORMAT option in output position order into 
		**	a temp buffer because may need to define pad fields to fill the gaps.
		*/
		varstr_printf(reformat_cmd," /REFORMAT ");
		nextpos = 1;
		for(;;)
		{
			foundpos = reformat->recsize+1;	/* Assume we found the end */

			/*
			**	Look for the next field in position order
			*/
			for(i=0; i<reformat->field_cnt; i++)
			{
				if (reformat->field[i].outpos == nextpos)
				{
					/*
					**	Found the exact field looking for.
					*/
					foundpos = nextpos;
					break;
				}
				else if (reformat->field[i].outpos > nextpos)
				{
					/*
					**	Found a posible field.
					**	Check if better then we already found.
					*/
					if (reformat->field[i].outpos < foundpos)
					{
						foundpos = reformat->field[i].outpos;
					}
				}
				else	/* less then nextpos */
				{
					/* already past this point keep looking */
				}
			}

			if (foundpos == nextpos)
			{
				/*
				**	Add the next field in position order
				*/
				varstr_printf(reformat_cmd,"%s %s_%d_%d ", 
					((nextpos==1)?"":","),
					REFORMAT_FIELD_PREFIX, 
					reformat->field[i].outpos, reformat->field[i].len);
				nextpos += reformat->field[i].len;
			}
			else
			{
				int pad_len;
				/*
				**	Define a pad field
				*/
				pad_len = foundpos-nextpos;
				varstr_printf(cmd, " /DERIVEDFIELD %s_%d_%d %d X\"%02X\" CHARACTER %d \n",
					PAD_FIELD_PREFIX, nextpos, pad_len, 
					pad_len, (int)reformat->pad_char ,pad_len);

				/*
				**	Add pad field to reformat
				*/
				varstr_printf(reformat_cmd,"%s %s_%d_%d ", 
					((nextpos==1)?"":","),
					PAD_FIELD_PREFIX, nextpos, pad_len);
				nextpos = foundpos;
			}

			if (nextpos >= reformat->recsize+1)
			{
				break;
			}
		}

		/*
		**	Add the temp reformat buff to the cmd then free it.
		*/
		varstr_printf(cmd, "%s \n", reformat_cmd->str);

		varstr_free(&reformat_cmd);

	}


	/*
		/END
	*/
	varstr_printf(cmd," /END \n");

	/*
	**	Execute the SyncSort command
	**
	**	Options:
	**	1) Call WL_wsystem() with the command.
	**	2) Put the command into a shell script then LINK to it.
	**	3) WL_run_unixcommand_silent() (wsystem.c)
	*/
	{
		int rc;
		rc = run_command(cmd->str, outstr);

		varstr_free(&cmd);
		return rc;
	}
}

/*
**	Routine:	shell_escape()
**
**	Function:	Apply shell escapes to a string
**
**	Description:	Copy in to out and shell escape any special shell characters,
**			Add a backslash ('\\') before a special shell char to escape.
**
**		Special charactesrs are '\\', '\n', '\'', '\"', '(', ')', '$'
**
**	Arguments:
**	in		The input string
**	out		The output string (varstr_s)
**
**	Return:		
**	0		Success
**	-1		Out size is too small
**
**
*/
static int shell_escape(const char* in, struct varstr_s *out)
{
	char buff[1024];
	size_t size;
	char *ptr;

	out->str[0] = '\0'; /* empty string */

	/*
	**	Copy into temp area (buff) until buff is full or done
	**	then concatinate buff onto the output (out).
	*/
	size = sizeof(buff);
	ptr = buff;

	for(;;)
	{
		if (*in == '\0')			/* Stop on a null character */
		{
			*ptr = *in;
			varstr_cat(out, buff);
			return 0;
		}

		if (size < 3)				/* Ensure enough space to handle an escaped char */
		{
			/*
			**	Temp area (buff) is full,
			**	concatinate it onto the output 
			**	then reset temp area
			*/
			*ptr = '\0';
			varstr_cat(out, buff);
			size = sizeof(buff);
			ptr = buff;
		}

		if (NULL!=strchr("$()\\\n\'\"",*in))	/* If a special shell char... */
		{
			*ptr++ = '\\';			/* Add '\' before special char to escape */
			size--;
		}
		*ptr++ = *in++;				/* Copy the char to output */
		size--;
	}
}


/*
**	Routine:	run_command()
**
**	Function:	Run a command silently.
**
**	Description:	Run a command thru a pipe and trace the output.
**
**	Arguments:
**	command		The command string. (without output redirection)
**
**	Return:		Exit status from the command or -1 if the command fails.
**
**
*/
static int run_command(const char* command, struct varstr_s *outstr)
{
	int 	rc;
	FILE    *pipe;
	struct varstr_s	*unixcommand = varstr_new();
#ifdef SIGCLD
	void	(*save_sig)();
#endif

	shell_escape(command, unixcommand);
	varstr_cat(unixcommand, " 2>&1");		/* Add redirection */
	
	WL_wtrace("COMMAND","RUN","%s",unixcommand->str);

	if (debug_mode)
	{
		/*
		**	Write it out to a temp file
		*/
		FILE *f;
		f = fopen("debug_ss.sh","w+");
		if (f == NULL)
		{
			char buff[100];
			sprintf(buff,"Unable to open debug script file errno=[%d]",errno);
			wsort_abort(buff);
		}
		fprintf(f,"%s\n",unixcommand->str);
		fclose(f);
	}


#ifdef SIGCLD
	save_sig = signal(SIGCLD,SIG_DFL);
#endif

	if ((pipe = popen(unixcommand->str,"r")) != NULL)
	{
		char	buff[1024];
		while (fgets(buff, sizeof(buff), pipe) != NULL)
		{
			int i;
			i = strlen(buff);

			/*
			**	Save the output in a malloc'ed buffer
			*/
			if (outstr != NULL && i > 0)
			{
				varstr_cat(outstr,buff);
			}

			if (i > 0 && '\n' == buff[i-1]) 
			{
				buff[i-1] = '\0'; /* Remove trailing newline */
			}
			
			WL_wtrace("COMMAND","OUTPUT","%s",buff);
		}

		rc =  pclose(pipe);
	}
	else
	{
		rc = -1;
	}

#ifdef SIGCLD
	signal(SIGCLD,save_sig);
#endif

	WL_wtrace("COMMAND","RETURN","rc=[%d]",rc);

	varstr_free(&unixcommand);

	return rc;
}

/*
**	Routine:	get_options()
**
**	Function:	Issue the OPTIONS getparm and validate.
**
**	Description:	Put up the OPTIONS getparm for SORT and validate all fields.
**			Validate all fields and report if unsupported options are requested.
**			REFORMAT=YES option is not available with the FUNCTION=MERGE.
**
**	Arguments:
**	options->
**	    function_flag	SORT or MERGE  (returned)
**	    stable_flag		true or false for stable sort  (returned)
**	    reformat_flag	true or false  (returned)
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
*/
static int get_options(struct options_getparm_s *options)
{
	int4	pfkey_mask;
	char	*gptype;
	char	message[80];
	char	*messid,*mess1,*mess2,*mess3,*mess4,*blank;
	char	*x_function, *x_memory, *x_addrout, *x_keyout, *x_stable, *x_reformat, *x_altseq;
	char	*issuer = ISSUER_SORT;

#define FUNCTION_FIELD_LEN	5
#define MEMORY_FIELD_LEN	4
#define ALTSEQ_FIELD_LEN	6
	char	function_field[FUNCTION_FIELD_LEN+1];
	char	memory_field[MEMORY_FIELD_LEN+1];
	char	addrout_field[YESNO_FIELD_LEN+1];
	char	keyout_field[YESNO_FIELD_LEN+1];
	char    stable_field[YESNO_FIELD_LEN+1];
	char	reformat_field[YESNO_FIELD_LEN+1];
	char	altseq_field[ALTSEQ_FIELD_LEN+1];

	pfkey_mask = PFKEY_16_ENABLED ;
	wswap(&pfkey_mask);
	gptype = "I ";
	blank=" ";
	mess1=mess2=blank;
	
	sprintf(message,"You are now running %s",SORT_VERSION);
	mess3=message;
	mess4="Please specify the program options below:";
	messid = "0000";

	memcpy(function_field,	"SORT ",  FUNCTION_FIELD_LEN);
	uint2field(128,memory_field,MEMORY_FIELD_LEN);
	memcpy(addrout_field,	"NO ",	  YESNO_FIELD_LEN);
	memcpy(keyout_field,	"NO ",	  YESNO_FIELD_LEN);
	memcpy(stable_field,	"NO ",	  YESNO_FIELD_LEN);
	memcpy(reformat_field,	"NO ",	  YESNO_FIELD_LEN);
	memcpy(altseq_field,	"NONE  ", ALTSEQ_FIELD_LEN);
	
	x_function = x_memory = x_addrout = x_keyout = x_stable = x_reformat = x_altseq = "K";

	for(;;)
	{
		int	row;
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "OPTIONS "; GP pfkey_recv; GP messid; GP issuer; GPNUM(4); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));
			GP mess3; GPNUM(strlen(mess3));
			GP mess4; GPNUM(strlen(mess4));

		row = 12;
		GPKW(x_function,"FUNCTION",function_field,FUNCTION_FIELD_LEN,row,13,"A");
		GPCTEXT("(SORT or MERGE)",row,34);

		row += 1;
		GPKW(x_memory,"MEMORY  ",memory_field,MEMORY_FIELD_LEN,row,13,"A");
		GPCTEXT("(NOT USED)",row,34);
		
		row += 2;
		GPCTEXT("Do you want an ADDROUT output file ?",row,2);
		GPKW(x_addrout,"ADDROUT ",addrout_field,YESNO_FIELD_LEN,row,46,"A");
		GPCTEXT("(NO)",row,65);

		row += 1;
		GPCTEXT("Do you want a KEYOUT output file ?",row,2);
		GPKW(x_keyout,"KEYOUT  ",keyout_field,YESNO_FIELD_LEN,row,46,"A");
		GPCTEXT("(NO)",row,65);

		row += 1;
		GPCTEXT("Do you require a STABLE sort ?",row,2);
		GPKW(x_stable,"STABLE  ",stable_field,YESNO_FIELD_LEN,row,46,"A");
		GPCTEXT("(YES or NO)",row,65);
		
		row += 1;
		GPCTEXT("Do you want to REFORMAT the records ?",row,2);
		GPKW(x_reformat,"REFORMAT",reformat_field,YESNO_FIELD_LEN,row,46,"A");
		GPCTEXT("(YES or NO)",row,65);
		
		row += 1;
		GPCTEXT("Type of ALTERNATE collating sequence ?",row,2);
		GPKW(x_altseq,"ALTSEQ  ",altseq_field,ALTSEQ_FIELD_LEN,row,46,"A");
		GPCTEXT("(NONE)",row,65);

		GPCTEXT(terminate_text,23,26);

		GPENTER();
		GPPFS(&pfkey_mask);

		CALL_GETPARM2(gp_args,gp_cnt);


		if (pfkey_recv[0] == PFKEY_16_PRESSED ) 
		{
			return 16;
		}
	
		x_function = x_memory = x_addrout = x_keyout = x_stable = x_reformat = x_altseq = "K";
		gptype = "R ";

		if (0 == strncmp(function_field,"SORT ",FUNCTION_FIELD_LEN))
		{
			options->function_flag = FUNC_SORT;
		}
		else if (0 == strncmp(function_field,"MERGE",FUNCTION_FIELD_LEN))
		{
			options->function_flag = FUNC_MERGE;
		}
		else
		{
			messid = "ER12";
			mess1 = "\224SORRY\204- Invalid FUNCTION.";
			x_function="R";
			continue;
		}

		if (0 != strncmp(addrout_field,"NO ",YESNO_FIELD_LEN))
		{
			messid = "ER15";
			mess1 = "\224SORRY\204- ADDROUT is not supported.";
			x_addrout="R";
			continue;
		}

		if (0 != strncmp(keyout_field,"NO ",YESNO_FIELD_LEN))
		{
			messid = "ER16";
			mess1 = "\224SORRY\204- KEYOUT is not supported.";
			x_keyout="R";
			continue;
		}

		if (0==strncmp(stable_field,"YES",YESNO_FIELD_LEN))
		{
			options->stable_flag=TRUE;
		}
		else if (0==strncmp(stable_field,"NO ",YESNO_FIELD_LEN))
		{
			options->stable_flag=FALSE;
		}
		else
		{
			messid = "ER17";
			mess1 = "\224SORRY\204- STABLE option must be YES or NO.";
			x_stable="R";
			continue;
		}

		if (0==strncmp(reformat_field,"YES",YESNO_FIELD_LEN))
		{
			if (options->function_flag == FUNC_MERGE) /* MERGE */
			{
				/*
				**	REFORMAT is not available with MERGE.
				**	- This is a documented Wang restriction that may be legal with Syncsort
				*/

				/*
				messid = "ER18";
				mess1 = "\224SORRY\204- REFORMAT cannot be specifed with MERGE";
				x_reformat="R";
				continue;
				*/
			}
			options->reformat_flag=TRUE;
		}
		else if (0==strncmp(reformat_field,"NO ",YESNO_FIELD_LEN))
		{
			options->reformat_flag=FALSE;
		}
		else
		{
			messid = "ER18";
			mess1 = "\224SORRY\204- REFORMAT option must be YES or NO.";
			x_reformat="R";
			continue;
		}

		if (strncmp(altseq_field,"NONE  ",ALTSEQ_FIELD_LEN))
		{
			messid = "ER19";
			mess1 = "\224SORRY\204- ALTSEQ is not supported";
			x_altseq="R";
			continue;
		}

		/*
		**	Passed all the tests
		*/
		break;
	}

	return 0;
}

/*
**	Routine:	get_input()
**
**	Function:	To get the input file specs for SORT.
**
**	Description:	Put up the INPUT getparm for SORT and validate all fields.
**
**	Arguments:
**	function_flag	true==sort false==merge
**	in->file	The input file 			(returned)	
**	in->lib		The input library	 	(returned)
**	in->vol		The input volume	 	(returned)
**	in->filename	The native input filename 	(returned)
**	in->fileorg	The file organization		(returned)
**	in->recsize	The record size 		(returned)
**	in->select_flag	Is select records requested 	(returned)
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**
*/
static int get_input(enum e_sortfunc function_flag, struct input_getparms_s *input)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*mess1,*mess2,*mess3,*mess4;
	char	*messid, *blank=" ";
	char	*issuer = ISSUER_SORT;
	char	*x_file, *x_library, *x_volume, *x_shared, *x_select, *x_morefile, *x_filetype, *x_recsize;
	char    shared_field[YESNO_FIELD_LEN+1];
	char	select_field[YESNO_FIELD_LEN+1];
	char	morefile_field[YESNO_FIELD_LEN+1];
	char	filetype_field[FILEORG_FIELD_LEN+1];
	char	recsize_field[RECSIZE_FIELD_LEN+1];
	int	shared_flag;
	struct wang_file_s *in;

	int	morefile_flag = FALSE;		/* Is more files requested  */
	int	more_input = FALSE;		/* Issues the secondary more files getparm */


	pfkey_mask = PFKEY_16_ENABLED;
	wswap(&pfkey_mask);

	gptype = "I ";

	for(;;)
	{
		char	pfkey_recv[1];

		if (0==memcmp(gptype,"I ",GP_GPTYPE_LEN))
		{
			/*
			**	Initial Getparm (not a respecify to fix an error)
			*/
			in = &input->list[input->infile_count];

			memset(in->file,' ',SIZEOF_FILE);
			memcpy(in->lib,def_inlib,SIZEOF_LIB);
			memcpy(in->vol,def_invol,SIZEOF_VOL);
			in->fileorg = 'I';
			in->rectype = 'F';
			in->recsize = 0;

			messid = "0000";

			x_file = x_library = x_volume = x_shared = x_select = x_morefile = x_filetype = x_recsize = "K";

			memcpy(shared_field,"NO ",YESNO_FIELD_LEN);
			memcpy(select_field,"NO ",YESNO_FIELD_LEN);
			memcpy(morefile_field,"NO ",YESNO_FIELD_LEN);

			filetype_field[0]='I';
			memcpy(recsize_field,"   0",RECSIZE_FIELD_LEN);
			recsize_field[RECSIZE_FIELD_LEN]= (char)0;
			shared_flag = 0;

			mess1=mess2=mess3=mess4=blank;

			if (function_flag == FUNC_SORT) /* SORT */
			{
				if (!more_input)
				{
					mess3="Please enter the name of the file to be sorted.";
				}
				else
				{
					mess3="Please enter the name of the next file to be sorted.";
					mess4="To end input, leave the input file blank.";
				}
			}
			else	/* MERGE */
			{
				if (!more_input)
				{
					mess3="Please enter the name of the file to be merged.";
				}
				else
				{
					mess3="Please enter the name of the next file to be merged.";
					mess4="To end input, leave the input file blank.";
				}
			}
		}


		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "INPUT   "; GP pfkey_recv; GP messid; GP issuer; GPNUM(4); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));
			GP mess3; GPNUM(strlen(mess3));
			GP mess4; GPNUM(strlen(mess4));

		GPCTEXT("INPUT",12,2);
		GPKW(x_file,"FILE    ",in->file,SIZEOF_FILE,12,8,"L");
		GPCTEXT("in",12,28);
		GPKW(x_library,"LIBRARY ",in->lib,SIZEOF_LIB,12,31,"L");
		GPCTEXT("on",12,51);
		GPKW(x_volume,"VOLUME  ",in->vol,SIZEOF_VOL,12,54,"L");

		GPCTEXT("Is this a SHARED file ?",14,2);
		GPKW(x_shared,"SHARED  ",shared_field,YESNO_FIELD_LEN,14,47,"A");
 		GPCTEXT("(NOT USED)",14,65);

		if (!more_input)
		{
			GPCTEXT("Do you want to select input records ?",15,2);
			GPKW(x_select,"SELECT  ",select_field,YESNO_FIELD_LEN,15,47,"A");
	 		GPCTEXT("(YES or NO)",15,65);

			if (function_flag == FUNC_SORT)
			{
				GPCTEXT("Do you have more input files ?",16,2);
				GPKW(x_morefile,"MOREFILE",morefile_field,YESNO_FIELD_LEN,16,47,"A");
	 			GPCTEXT("(YES or NO)",16,65);
			}
		}

		GPCTEXT("Is file type Indexed, Fixed, Line Sequential ?",18,2);
		GPKW(x_filetype,"FILETYPE",filetype_field,FILEORG_FIELD_LEN,18,47,"A");
 		GPCTEXT("(I, F or L)",18,65);

		GPCTEXT("For FILETYPE=F, what is the record size ?",19,2);
		GPKW(x_recsize,"RECSIZE ",recsize_field,RECSIZE_FIELD_LEN,19,47,"N");

		GPCTEXT(terminate_text,22,26);

		GPENTER();
		GPPFS(&pfkey_mask);

		CALL_GETPARM2(gp_args,gp_cnt);


		if (pfkey_recv[0] == PFKEY_16_PRESSED ) 
		{
			return 16;
		}

		gptype = "R ";
		x_file = x_library = x_volume = x_shared = x_select = x_morefile = x_filetype = x_recsize = "K";
		mess1=mess2=mess3=mess4=blank;
		
		leftjust(in->file,SIZEOF_FILE);
		leftjust(in->lib,SIZEOF_LIB);
		leftjust(in->vol,SIZEOF_VOL);

		in->file[SIZEOF_FILE] = (char)0;
		in->lib[SIZEOF_LIB] = (char)0;
		in->vol[SIZEOF_VOL] = (char)0;

		if (more_input && ' ' == in->file[0])
		{
			/*
			**	On a more_input a blank file name means no more files.
			*/
			return 0;
		}

		if (' '==in->vol[0] || ' '==in->lib[0] || ' '==in->file[0])
		{
			messid = "R001";
			mess1 = "\224SORRY\204- File specification is incomplete, please respecify.";
			
			if (' '==in->vol[0])
			{
				x_volume = "R";
			}
			if (' '==in->lib[0])
			{
				x_library = "R";
			}
			if (' '==in->file[0])
			{
				x_file = "R";
			}
			continue;
		}

		/*
		**	Check if volume and library exists
		*/
		{
			int4	start, count;
			char	recvr[22];	/* 8+8+6 */
			char	find_file[SIZEOF_FILE], find_lib[SIZEOF_LIB];


			memset(find_file, ' ', SIZEOF_FILE);
			memset(find_lib, ' ', SIZEOF_LIB);
			start = 1;
			wswap(&start);
			count = 1;
			wswap(&count);

			set_arg_count(6);
			FIND(find_file, find_lib, in->vol, &start, &count, recvr);

			wswap(&count);
			if (count == 0)
			{
				messid = "ER28";
				mess1="\224SORRY\204- The input VOLUME specified was not found, please respecify.";
				x_volume = "R";
				continue;
			}

			memset(find_file, ' ', SIZEOF_FILE);
			start = 1;
			wswap(&start);
			count = 1;
			wswap(&count);

			set_arg_count(6);
			FIND(find_file, in->lib, in->vol, &start, &count, recvr);

			wswap(&count);
			if (count == 0)
			{
				messid = "R014";
				mess1 = "\224SORRY\204- The input LIBRARY was not found, please respecify.";
				x_library = "R";
				continue;
			}


			start = 1;
			wswap(&start);
			count = 1;
			wswap(&count);

			set_arg_count(6);
			FIND(in->file, in->lib, in->vol, &start, &count, recvr);

			wswap(&count);
			if (count == 0)
			{
				messid = "R014";
				mess1 = "\224SORRY\204- The input FILE was not found, please respecify.";
				x_file = "R";
				continue;
			}
		}


		/*
		**	Translate the INPUT file name.
		*/
		{
			int4	mode;
			char	*ptr;

			mode = 0;
			ptr = WFNAME(&mode, in->vol, in->lib, in->file, in->filename);
			*ptr = (char)0;
		}

		if (strncmp(shared_field,"YES",YESNO_FIELD_LEN) && strncmp(shared_field,"NO ",YESNO_FIELD_LEN))
		{
			x_shared="R";
			messid="ER20";
			mess1 = "\224SORRY\204- SHARED option must be YES or NO, please respecify.";
			continue;
		}
		else if (!strncmp(shared_field,"YES",YESNO_FIELD_LEN))
		{
			shared_flag=1;
		}
		else
		{
			shared_flag=0;
		}

		if (!more_input)
		{
			if (0 == strncmp(select_field,"YES",YESNO_FIELD_LEN) )
			{
				input->select_flag=TRUE;
			}
			else if (0 == strncmp(select_field,"NO ",YESNO_FIELD_LEN) )
			{
				input->select_flag=FALSE;
			}
			else
			{
				x_select="R";
				messid="ER20";
				mess1 = "\224SORRY\204- SELECT option must be YES or NO, please respecify.";
				continue;
			}

			if (function_flag == FUNC_SORT)
			{
				if (0 == strncmp(morefile_field,"YES",YESNO_FIELD_LEN) )
				{
					morefile_flag=TRUE;
				}
				else if (0 == strncmp(morefile_field,"NO ",YESNO_FIELD_LEN) )
				{
					morefile_flag=FALSE;
				}
				else
				{
					x_morefile ="R";
					messid="ER19";
					mess1 = "\224SORRY\204- MOREFILE option must be YES or NO, please respecify.";
					continue;
				}
			}
			else
			{
				morefile_flag=TRUE;
			}
		}


		/*
		**	Handle the FILETYPE and RECSIZE fields which are only used
		**	for unix and DOS.
		*/

		if (	filetype_field[0] != 'I' && 
			filetype_field[0] != 'F'   )
		{
			x_filetype="R";
			messid="ER99";
			mess1 = "\224SORRY\204- FILETYPE must be \"I\" or \"F\", please respecify.";
			continue;
		}
		else
		{
			int4	fdr_mode, fdr_retcode;
			char	fdr_filetype;

			fdr_mode = 0;
			wswap(&fdr_mode);
			fdr_retcode = 0;
			set_arg_count(7);			
			READFDR4(in->file,in->lib,in->vol,&fdr_mode,"FT",&fdr_filetype,&fdr_retcode);

			wswap(&fdr_retcode);
			if (READFDR_RC_24_NO_FILE_HEADER == fdr_retcode)
			{
				fdr_filetype = 'C'; /* No file header so it's consecutive */
			}
			else if (0 != fdr_retcode)
			{
				messid = "R014";
				mess1 = "\224SORRY\204- Unable to access input file, please respecify.";
				x_file = "R";
				continue;
			}

			if ('A' == fdr_filetype)	/* Alternate Indexed is Indexed */
			{
				fdr_filetype = 'I';
			}

			if ( ('I' == fdr_filetype && 'I' != filetype_field[0]) ||
			     ('I' != fdr_filetype && 'I' == filetype_field[0])   )
			{
				x_filetype="R";
				messid="ER99";
				mess1 = "\224SORRY\204- FILETYPE does not match file, please respecify.";
				continue;
			}
		}
		in->fileorg=filetype_field[0];

		if ('F' == in->fileorg)
		{
			if (field2uint(recsize_field,RECSIZE_FIELD_LEN,&in->recsize))
			{
				messid="ER98";
				mess1="\224SORRY\204- RECSIZE field is invalid, please respecify.";
				x_recsize="R";
				continue;
			}

			if (in->recsize < 1 || in->recsize > MAXRECSIZE)
			{
				x_recsize="R";
				messid="ER99";
				mess1 = "\224SORRY\204- Invalid RECSIZE must be for FILETYPE=F, please respecify.";
				continue;
			}
		}

		/*
		**	Call READFDR to get the record length.
		*/
		if ('I' == in->fileorg)
		{
			int4	fdr_mode, fdr_retcode;
			int4	fdr_recsize;

			fdr_mode = 0;
			wswap(&fdr_mode);
			fdr_retcode = 0;
			fdr_recsize = 0;
			wswap(&fdr_recsize);
			set_arg_count(7);			
			READFDR4(in->file,in->lib,in->vol,&fdr_mode,"RS",&fdr_recsize,&fdr_retcode);

			wswap(&fdr_retcode);
			if (0 != fdr_retcode)
			{
				messid = "R014";
				mess1 = "\224SORRY\204- Unable to access input file, please respecify.";
				x_file = "R";
				continue;
			}

			wswap(&fdr_recsize);
			in->recsize = fdr_recsize;
		}

		/*
		**	MERGE requires all records to have the same record size
		**
		**	- This is documented Wang restriction, may not be needed for SyncSort
		*/
		if (function_flag==FUNC_MERGE && input->infile_count > 0)
		{
			if (in->recsize != input->list[0].recsize)
			{
				x_file="R";
				messid="ER99";
				mess1 = "\224SORRY\204- MERGE requires all input files to have equal record lenght";
				continue;
			}
		}

		/*
		**	We've successfully got an INPUT file.
		**	- increment the count
		**	- keep the min/max record sizes
		**	- return or loop again for another file
		*/

		++input->infile_count;

		if (in->recsize > input->max_recsize) 
		{
			input->max_recsize = in->recsize;
		}
		if (in->recsize < input->min_recsize) 
		{
			input->min_recsize = in->recsize;
		}



		if (!morefile_flag || input->infile_count >= MAXSORTINFILES)
		{
			/*
			**	No more files requested - done
			*/
			return 0;
		}

		/*
		**	Issue another Initial INPUT getparm
		*/
		more_input = TRUE;
		gptype = "I ";
	}
}

/*
**	Routine:	get_select()
**
**	Function:	Issue the SELECT getparm(s)
**
**	Description:	
**
**	Arguments:
**	reclen		The record length (0=variable)
**	select->
**	    selinfo	Select criteria list
**	    selcnt	Select count
**
**	Globals:	None
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
*/
static int get_select(unsigned int reclen, struct select_getparms_s *select)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*messid,*mess1,*mess2,*mess3,*mess4,*mess5,*mess6,*blank=" ";
	char	*issuer = ISSUER_SORT;
	char    prname[GP_PRNAME_LEN+1];
	int 	stvalidx,endvalidx;
	int 	chkidx;
	struct selstrs
	{
		char	fldpos_field[FLDPOS_FIELD_LEN+1];
		char	length_field[FLDLEN_FIELD_LEN+1];
		char	fldtyp_field[FLDTYP_FIELD_LEN+1];
		char	tstrel_field[TSTREL_FIELD_LEN+1];
		char	tstval_field[TSTVAL_FIELD_LEN+1];
		char	tstcon_field[TSTCON_FIELD_LEN+1];
	} 	s[4];
	char	*x_fldpos[4], *x_length[4], *x_fldtyp[4], *x_tstrel[4], *x_tstval[4], *x_tstcon[4];
	char 	kw_fldpos[4][9], kw_lenght[4][9], kw_fldtyp[4][9], kw_tstrel[4][9], kw_tstval[4][9], kw_tstcon[4][9];
	int	select_idx, base_idx;
	int	next_select;
	char	pfkey_recv[1];

	select->selcnt = 0;

	x_fldpos[0] = x_length[0] = x_fldtyp[0] = x_tstrel[0] = x_tstval[0] = x_tstcon[0] =
	x_fldpos[1] = x_length[1] = x_fldtyp[1] = x_tstrel[1] = x_tstval[1] = x_tstcon[1] =		
	x_fldpos[2] = x_length[2] = x_fldtyp[2] = x_tstrel[2] = x_tstval[2] = x_tstcon[2] = 
	x_fldpos[3] = x_length[3] = x_fldtyp[3] = x_tstrel[3] = x_tstval[3] = x_tstcon[3] = "K";

	pfkey_mask = PFKEY_16_ENABLED ;
	wswap(&pfkey_mask);

	mess1=blank;
	mess2="Enter record selection criteria below, supplying field position, length and";
	mess3="  format type (Binary,Char,Decimal,...), test relation (EQ,NE,GT,GE,LT,LE),";
	mess4="  and test value (in quotes) or comparison field position (without quotes).";
	mess5="Set connector to \"AND\" to continue current criterion; use \"OR\" to begin";
	mess6="  alternative criterion.     (Use Chars or Decimal numeric for test value.)";
	

	select_idx = -1;
	next_select = 1;

	for(;;)
	{

		/*
		**	Setup for the next SELECT getparm
		*/
		if (next_select)
		{
			int 	kwidx;

			memset(s,' ',sizeof(s));
			gptype = "I ";
			messid = "SEL ";
			mess1 = blank;
			s[0].fldtyp_field[0] =	s[1].fldtyp_field[0] =	s[2].fldtyp_field[0] =	s[3].fldtyp_field[0] = 'C';

			select_idx += 1;


			{
				char	buff[20];

				if (select_idx>0)
				{
					sprintf(buff,"SELECT%d  ",select_idx+1);
				}
				else
				{
					strcpy(buff,"SELECT  ");
				}
				memcpy(prname,buff,GP_PRNAME_LEN);
			}

			base_idx = select_idx * 4;

			for (kwidx=0; kwidx<4; ++kwidx)
			{
				sprintf(kw_fldpos[kwidx], "FLDPOS%-2d", kwidx + base_idx + 1);
				sprintf(kw_lenght[kwidx], "LENGTH%-2d", kwidx + base_idx + 1);
				sprintf(kw_fldtyp[kwidx], "FLDTYP%-2d", kwidx + base_idx + 1);
				sprintf(kw_tstrel[kwidx], "TSTREL%-2d", kwidx + base_idx + 1);
				sprintf(kw_tstval[kwidx], "VALUE%-2d ", kwidx + base_idx + 1);
				sprintf(kw_tstcon[kwidx], "CONECT%-2d", kwidx + base_idx + 1);
			}

			next_select = 0;
		}
		
		pfkey_recv[0] = '@';
		
		gp_cnt = 0;
		GP gptype; GP "R"; GP prname; GP pfkey_recv; GP messid; GP issuer; GPNUM(6); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));
			GP mess3; GPNUM(strlen(mess3));
			GP mess4; GPNUM(strlen(mess4));
			GP mess5; GPNUM(strlen(mess5));
			GP mess6; GPNUM(strlen(mess6));

		GPKW(x_fldpos[0],kw_fldpos[0],s[0].fldpos_field,FLDPOS_FIELD_LEN,14,7,"N");
		GPKW(x_fldpos[1],kw_fldpos[1],s[1].fldpos_field,FLDPOS_FIELD_LEN,17,7,"N");
		GPKW(x_fldpos[2],kw_fldpos[2],s[2].fldpos_field,FLDPOS_FIELD_LEN,20,7,"N");
		GPKW(x_fldpos[3],kw_fldpos[3],s[3].fldpos_field,FLDPOS_FIELD_LEN,23,7,"N");
		
		GPKW(x_length[0],kw_lenght[0],s[0].length_field,FLDLEN_FIELD_LEN,14,26,"N");
		GPKW(x_length[1],kw_lenght[1],s[1].length_field,FLDLEN_FIELD_LEN,17,26,"N");
		GPKW(x_length[2],kw_lenght[2],s[2].length_field,FLDLEN_FIELD_LEN,20,26,"N");
		GPKW(x_length[3],kw_lenght[3],s[3].length_field,FLDLEN_FIELD_LEN,23,26,"N");
		
		GPKW(x_fldtyp[0], kw_fldtyp[0],s[0].fldtyp_field,FLDTYP_FIELD_LEN,14,45,"A");
		GPKW(x_fldtyp[1], kw_fldtyp[1],s[1].fldtyp_field,FLDTYP_FIELD_LEN,17,45,"A");
		GPKW(x_fldtyp[2], kw_fldtyp[2],s[2].fldtyp_field,FLDTYP_FIELD_LEN,20,45,"A");
		GPKW(x_fldtyp[3], kw_fldtyp[3],s[3].fldtyp_field,FLDTYP_FIELD_LEN,23,45,"A");
		
		GPKW(x_tstrel[0], kw_tstrel[0],s[0].tstrel_field,TSTREL_FIELD_LEN,15,7,"A");
		GPKW(x_tstrel[1], kw_tstrel[1],s[1].tstrel_field,TSTREL_FIELD_LEN,18,7,"A");
		GPKW(x_tstrel[2], kw_tstrel[2],s[2].tstrel_field,TSTREL_FIELD_LEN,21,7,"A");
		GPKW(x_tstrel[3], kw_tstrel[3],s[3].tstrel_field,TSTREL_FIELD_LEN,24,7,"A");
		
		GPKW(x_tstval[0], kw_tstval[0],s[0].tstval_field,TSTVAL_FIELD_LEN,15,26,"C");
		GPKW(x_tstval[1], kw_tstval[1],s[1].tstval_field,TSTVAL_FIELD_LEN,18,26,"C");
		GPKW(x_tstval[2], kw_tstval[2],s[2].tstval_field,TSTVAL_FIELD_LEN,21,26,"C");
		GPKW(x_tstval[3], kw_tstval[3],s[3].tstval_field,TSTVAL_FIELD_LEN,24,26,"C");

		GPKW(x_tstcon[0], kw_tstcon[0],s[0].tstcon_field,TSTCON_FIELD_LEN,15,62,"A");
		GPKW(x_tstcon[1], kw_tstcon[1],s[1].tstcon_field,TSTCON_FIELD_LEN,18,62,"A");
		GPKW(x_tstcon[2], kw_tstcon[2],s[2].tstcon_field,TSTCON_FIELD_LEN,21,62,"A");
		GPKW(x_tstcon[3], kw_tstcon[3],s[3].tstcon_field,TSTCON_FIELD_LEN,24,62,"A");
		
		GPENTER();
		GPPFS(&pfkey_mask);

		CALL_GETPARM2(gp_args,gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED ) 
		{
			return 16;
		}

		messid = "ERR ";

		x_fldpos[0] = x_length[0] = x_fldtyp[0] = x_tstrel[0] = x_tstval[0] = x_tstcon[0] =
		x_fldpos[1] = x_length[1] = x_fldtyp[1] = x_tstrel[1] = x_tstval[1] = x_tstcon[1] =		
		x_fldpos[2] = x_length[2] = x_fldtyp[2] = x_tstrel[2] = x_tstval[2] = x_tstcon[2] = 
		x_fldpos[3] = x_length[3] = x_fldtyp[3] = x_tstrel[3] = x_tstval[3] = x_tstcon[3] = "K";

		for (chkidx = 0; chkidx < 4; chkidx++)
		{
			unsigned int fldpos = 0;		/* Test field starting position */
			unsigned int fldlen = 0;		/* Test field lenght */
			char fldtyp = ' ';			/* Test field type */
			enum e_tstrel tstrel = REL_UNKNOWN;	/* Test relationship to either tstval or tstpos */
			char tstval[TSTVAL_FIELD_LEN+1] = {""};	/* Test constant string (or empty string if tstpos!=0) */
			unsigned int tstpos = 0;		/* Position of the test field (or 0 if a tstval ) */
			enum e_tstcon tstcon = CON_UNKNOWN;	/* AND or OR for next select test */

			/*
			**	FLDPOS	- FIELD POSITION
			*/

			if (0 != field2uint(s[chkidx].fldpos_field, FLDPOS_FIELD_LEN, &fldpos) || fldpos < 1)
			{
				messid = "ERS1";
				mess1 = "\224SORRY\204- Field position is invalid";
				x_fldpos[chkidx]="R";
				goto sel_respec;
			}

			/*
			**	LENGTH	- FIELD LENGTH
			*/

			if (0 != field2uint(s[chkidx].length_field, FLDLEN_FIELD_LEN, &fldlen) || fldlen < 1)
			{
				messid = "ERS2";
				mess1 = "\224SORRY\204- Field length is invalid";
				x_length[chkidx]="R";
				goto sel_respec;
			}

			if (fldpos+fldlen-1 > reclen)
			{
				messid = "ERS1";
				mess1 = "\224SORRY\204- The comparison field is not imbedded within the record";
				x_fldpos[chkidx]="R";
				goto sel_respec;
			}

			/*
			**	FLDTYP	- FIELD TYPE
			*/

			if (!strchr("BCDLPZ",s[chkidx].fldtyp_field[0]))
			{
				messid = "ES10";
				mess1 = "\224SORRY\204- Field type must be one of B, C, D, L, P or Z";
				x_fldtyp[chkidx]="R";
				goto sel_respec;
			}
			if (s[chkidx].fldtyp_field[0] == 'D')
			{
				wsort_abort("SELECT TYPE=D is not supported with SyncSort");
			}
			fldtyp = s[chkidx].fldtyp_field[0];

			if ( 'B' == fldtyp &&
			      2 != fldlen  &&
			      4 != fldlen  &&
			      8 != fldlen     )
			{
				messid = "ER08";
				mess1 = "\224SORRY\204- The binary key length must be 2,4 or 8 bytes";
				x_length[chkidx]="R";
				goto sel_respec;
			}


			/*
			**	TSTREL	- TEST RELATION
			*/
			{
				int idx;
				tstrel = REL_UNKNOWN;
				for(idx=0; rel_tab[idx].relstr != NULL; idx++)
				{
					if (0==memcmp(rel_tab[idx].relstr, s[chkidx].tstrel_field, TSTREL_FIELD_LEN))
					{
						tstrel = rel_tab[idx].tstrel;
						break;
					}
				}
				if (tstrel == REL_UNKNOWN)
				{
					messid = "ERS3";
					mess1 = "\224SORRY\204- Test relation must be one of EQ, NE, GT, GE, LT, or LE";
					x_tstrel[chkidx]="R";
					goto sel_respec;
				}
			}

			select->selinfo[chkidx+base_idx].fldtyp = fldtyp;
			select->selinfo[chkidx+base_idx].fldpos = fldpos;
			select->selinfo[chkidx+base_idx].fldlen = fldlen;
			select->selinfo[chkidx+base_idx].tstrel = tstrel;


			/*
			**	VALUE	- TEST VALUE
			**
			**	Either a quoted constant value or a numeric position start 
			**	of field in the record.
			*/
			select->selinfo[chkidx+base_idx].tstval[0] = '\0';
			select->selinfo[chkidx+base_idx].tstpos = 0;

			memcpy(tstval,s[chkidx].tstval_field,TSTVAL_FIELD_LEN);
			tstval[TSTVAL_FIELD_LEN] = (char)0;

			/* Find the 1st non-space character */
			for (stvalidx=0; stvalidx<TSTVAL_FIELD_LEN && tstval[stvalidx]==' '; ++stvalidx);
			if (TSTVAL_FIELD_LEN==stvalidx)
			{
				messid = "ERS5";
				mess1 = "\224SORRY\204- The data specified is not a field position or a test value.";
				x_tstval[chkidx]="R";
				goto sel_respec;
			}
			/* Find the last non-space character */
			for (endvalidx=TSTVAL_FIELD_LEN-1; endvalidx>=stvalidx && tstval[endvalidx]==' '; --endvalidx);

			if ((tstval[stvalidx]=='"'  && tstval[endvalidx]=='"') || 
			    (tstval[stvalidx]=='\'' && tstval[endvalidx]=='\'')  )
			{
				unsigned int tstlen;

				if (stvalidx == endvalidx)
				{
					messid = "ES14";
					mess1 = "\224SORRY\204- Unbalanced quote in use";
					x_tstval[chkidx]="R";
					goto sel_respec;
				}

				tstlen = endvalidx - stvalidx - 1; /* length of data (without quotes) */

				if (tstlen < 1)
				{
					messid = "ES15";
					mess1 = "\224SORRY\204- No direct data enclosed in quotes";
					x_tstval[chkidx]="R";
					goto sel_respec;
				}

				if ('C' == fldtyp && tstlen != fldlen)
				{
					messid = "ES16";
					mess1 = "\224SORRY\204- Field length is not equal to the test value data size";
					x_tstval[chkidx]="R";
					goto sel_respec;
				}


				/* Fill tstval with nulls to ensure string is null terminated */
				memset(select->selinfo[chkidx+base_idx].tstval,0,TSTVAL_FIELD_LEN+1);
				if ('C' == fldtyp)
				{
					/*
					**	Load the test constant into tstval WITH the quotes.
					*/
					memcpy(select->selinfo[chkidx+base_idx].tstval, &tstval[stvalidx], tstlen+2);
				}
				else
				{
					char buff[TSTVAL_FIELD_LEN+1];
					unsigned int i;

					/*
					**	VALIDATE NUMERIC TEST VALUE LITERALS
					**
					**	Field consist of only digits with optionally a 
					**	leading or trailing sign
					*/
					memcpy(buff, &tstval[stvalidx+1], tstlen);
					buff[tstlen] = '\0';
					for (i=0; i<tstlen; i++)
					{
						if (isdigit((int)(buff[i])))
						{
							continue;	/* OK */
						}

						if ((i==0 || i==tstlen-1) &&
						    (buff[i] == '-' || buff[i] == '+'))
						{
							/*
							**	Found a sign in a valid position
							**	Ensure there isn't two signs
							*/
							int j;
							if (i==0)
							{
								j = tstlen-1;
							}
							else
							{
								j = 0;
							}
							if (buff[j] == '-' || buff[j] == '+')
							{
								/* found two signs */
							}
							else
							{
								continue; /* OK */
							}
						}

						messid = "ERS7";
						mess1 = "\224SORRY\204- The direct data cannot be compared with a numeric field";
						x_tstval[chkidx]="R";
						goto sel_respec;
					}

					/*
					**	Load the test constant into tstval WITHOUT the quotes.
					*/
					strcpy(select->selinfo[chkidx+base_idx].tstval, buff);
				}
			}
			else
			{
				/* Only valid value is a numeric position in the record */
				if ( 0 != field2uint(tstval, TSTVAL_FIELD_LEN, &tstpos) )
				{
					messid = "ERS5";
					mess1 = "\224SORRY\204- The data specified is not a field position or a test value.";
					x_tstval[chkidx]="R";
					goto sel_respec;
				}

				/*
				**	test position must be valid.
				**	- Greater then 0
				**	- Less then end of record - fldlen
				**	- Not eq to fldpos
				*/
				if (tstpos < 1 || tstpos > (reclen - fldlen + 1))
				{
					messid = "ERS1";
					mess1 = "\224SORRY\204- The comparison field is not imbedded within the record";
					x_tstval[chkidx]="R";
					goto sel_respec;
				}
				if (tstpos == fldpos)
				{
					messid = "ERS1";
					mess1 = "\224SORRY\204- Comparision field cannot start on same position";
					x_tstval[chkidx]="R";
					goto sel_respec;
				}

				select->selinfo[chkidx+base_idx].tstpos = tstpos;
			}

			/*
			**	CONECT	- TEST CONNECTION
			*/
			if (0==memcmp(s[chkidx].tstcon_field,"AND",TSTCON_FIELD_LEN))
			{
				tstcon = CON_AND;
			}
			else if (0==memcmp(s[chkidx].tstcon_field,"OR ",TSTCON_FIELD_LEN))
			{
				tstcon = CON_OR;
			}
			else if (0==memcmp(s[chkidx].tstcon_field,"   ",TSTCON_FIELD_LEN))
			{
				tstcon = CON_BLANK;
			}
			else
			{
				messid = "ERS4";
				mess1 = "\224SORRY\204- Criterion connector is invalid";
				x_tstcon[chkidx]="R";
				goto sel_respec;
			}

			if (chkidx+base_idx+1 == MAXSORTSELECTS && tstcon != CON_BLANK)
			{
				mess1 = "\224ERROR\204- CONECT32 MUST BE BLANK.";
				x_tstcon[chkidx]="R";
				goto sel_respec;
			}

			select->selinfo[chkidx+base_idx].tstcon = tstcon;


			if (tstcon == CON_BLANK)
			{
				/*
				**	Found a blank connect field so were done.
				*/

				select->selcnt += chkidx + 1;
				return 0;
			}
		}

		select->selcnt += 4;
		next_select = 1;

	sel_respec:
		if (next_select)
		{
		}
		else
		{
			gptype = "R ";
		}
	}
}


/*
**	Routine:	get_keys()
**
**	Function:	Issue the KEYS getparm
**
**	Description:	Issue the KEYS getparm and validate all the fields.
**
**	Arguments:
**	keys->
**	    keycnt	Key count
**	    keyinfo	Key criteria list
**
**	Globals:	None
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
*/
static int get_keys(unsigned int reclen, struct keys_getparm_s *keys)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*messid,*mess1,*mess2,*blank=" ";
	char	*issuer = ISSUER_SORT;
	unsigned int 	chkidx;
	char    *x_keys, keys_field[NUMKEYS_FIELD_LEN+1];
	char	*x_fldpos[MAXSORTKEYS], *x_fldlen[MAXSORTKEYS], *x_fldtyp[MAXSORTKEYS], *x_order[MAXSORTKEYS];
	struct keystrs
	{
		char	fldpos_field[FLDPOS_FIELD_LEN+1];
		char	fldlen_field[FLDLEN_FIELD_LEN+1];
		char	fldtyp_field[FLDTYP_FIELD_LEN+1];
		char	order_field[ORDER_FIELD_LEN+1];
	} k[MAXSORTKEYS];

	memset(k,' ',sizeof(k));
	keys_field[0]='1';
	x_keys="K";
	for (chkidx=0; chkidx<MAXSORTKEYS; ++chkidx)
	{
		x_fldpos[chkidx] = x_fldlen[chkidx] = x_fldtyp[chkidx] = x_order[chkidx] = "K";
		k[chkidx].fldtyp_field[0]='C';
		k[chkidx].order_field[0]='A';
	}

	pfkey_mask = PFKEY_16_ENABLED ;
	wswap(&pfkey_mask);
	gptype = "I ";

	mess1="Please specify Sort/Merge keys:";
	mess2=blank;
	
	messid = "0001";
	
	for(;;)
	{
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "KEYS    "; GP pfkey_recv; GP messid; GP issuer; GPNUM(2); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));

		GPCTEXT("NUMBER OF",9,2);
		GPKW(x_keys,"KEYS    ",keys_field,NUMKEYS_FIELD_LEN,9,12,"N");
		GPCTEXT("(Less than or equal to 8)",9,26);
		GPCTEXT("Key attributes:",10,2);
		GPCTEXT("Position(>0)     Length in Bytes  Sort Key Type   Sort Order(A,D)",11,11);
		GPCTEXT("Key 1:",12,2);
		GPCTEXT("Key 2:",13,2);
		GPCTEXT("Key 3:",14,2);
		GPCTEXT("Key 4:",15,2);
		GPCTEXT("Key 5:",16,2);
		GPCTEXT("Key 6:",17,2);
		GPCTEXT("Key 7:",18,2);
		GPCTEXT("Key 8:",19,2);
		GPCTEXT("* Key Type:   B=Binary, C=Character, D=Decimal, F=Floating, P=Packed",21,2);
		GPCTEXT("              Z=Zoned decimal, L=Zoned decimal sign leading, U=Unsigned",22,2);
		GPCTEXT("* Sort Order: A=Ascending, D=Descending",23,2);
                                                    
		GPKW(x_fldpos[0],"POST1   ",k[0].fldpos_field,FLDPOS_FIELD_LEN,12,11,"N");
		GPKW(x_fldpos[1],"POST2   ",k[1].fldpos_field,FLDPOS_FIELD_LEN,13,11,"N");
		GPKW(x_fldpos[2],"POST3   ",k[2].fldpos_field,FLDPOS_FIELD_LEN,14,11,"N");
		GPKW(x_fldpos[3],"POST4   ",k[3].fldpos_field,FLDPOS_FIELD_LEN,15,11,"N");
		GPKW(x_fldpos[4],"POST5   ",k[4].fldpos_field,FLDPOS_FIELD_LEN,16,11,"N");
		GPKW(x_fldpos[5],"POST6   ",k[5].fldpos_field,FLDPOS_FIELD_LEN,17,11,"N");
		GPKW(x_fldpos[6],"POST7   ",k[6].fldpos_field,FLDPOS_FIELD_LEN,18,11,"N");
		GPKW(x_fldpos[7],"POST8   ",k[7].fldpos_field,FLDPOS_FIELD_LEN,19,11,"N");
		
		GPKW(x_fldlen[0],"LENGTH1 ",k[0].fldlen_field,FLDLEN_FIELD_LEN,12,28,"N");
		GPKW(x_fldlen[1],"LENGTH2 ",k[1].fldlen_field,FLDLEN_FIELD_LEN,13,28,"N");
		GPKW(x_fldlen[2],"LENGTH3 ",k[2].fldlen_field,FLDLEN_FIELD_LEN,14,28,"N");
		GPKW(x_fldlen[3],"LENGTH4 ",k[3].fldlen_field,FLDLEN_FIELD_LEN,15,28,"N");
		GPKW(x_fldlen[4],"LENGTH5 ",k[4].fldlen_field,FLDLEN_FIELD_LEN,16,28,"N");
		GPKW(x_fldlen[5],"LENGTH6 ",k[5].fldlen_field,FLDLEN_FIELD_LEN,17,28,"N");
		GPKW(x_fldlen[6],"LENGTH7 ",k[6].fldlen_field,FLDLEN_FIELD_LEN,18,28,"N");
		GPKW(x_fldlen[7],"LENGTH8 ",k[7].fldlen_field,FLDLEN_FIELD_LEN,19,28,"N");
		
		GPKW(x_fldtyp[0],"TYPE1   ",k[0].fldtyp_field,FLDTYP_FIELD_LEN,12,45,"A");
		GPKW(x_fldtyp[1],"TYPE2   ",k[1].fldtyp_field,FLDTYP_FIELD_LEN,13,45,"A");
		GPKW(x_fldtyp[2],"TYPE3   ",k[2].fldtyp_field,FLDTYP_FIELD_LEN,14,45,"A");
		GPKW(x_fldtyp[3],"TYPE4   ",k[3].fldtyp_field,FLDTYP_FIELD_LEN,15,45,"A");
		GPKW(x_fldtyp[4],"TYPE5   ",k[4].fldtyp_field,FLDTYP_FIELD_LEN,16,45,"A");
		GPKW(x_fldtyp[5],"TYPE6   ",k[5].fldtyp_field,FLDTYP_FIELD_LEN,17,45,"A");
		GPKW(x_fldtyp[6],"TYPE7   ",k[6].fldtyp_field,FLDTYP_FIELD_LEN,18,45,"A");
		GPKW(x_fldtyp[7],"TYPE8   ",k[7].fldtyp_field,FLDTYP_FIELD_LEN,19,45,"A");
		
		GPKW(x_order[0],"ORDER1  ",k[0].order_field,ORDER_FIELD_LEN,12,61,"A");
		GPKW(x_order[1],"ORDER2  ",k[1].order_field,ORDER_FIELD_LEN,13,61,"A");
		GPKW(x_order[2],"ORDER3  ",k[2].order_field,ORDER_FIELD_LEN,14,61,"A");
		GPKW(x_order[3],"ORDER4  ",k[3].order_field,ORDER_FIELD_LEN,15,61,"A");
		GPKW(x_order[4],"ORDER5  ",k[4].order_field,ORDER_FIELD_LEN,16,61,"A");
		GPKW(x_order[5],"ORDER6  ",k[5].order_field,ORDER_FIELD_LEN,17,61,"A");
		GPKW(x_order[6],"ORDER7  ",k[6].order_field,ORDER_FIELD_LEN,18,61,"A");
		GPKW(x_order[7],"ORDER8  ",k[7].order_field,ORDER_FIELD_LEN,19,61,"A");
		
		GPENTER();
		GPPFS(&pfkey_mask);

		CALL_GETPARM2(gp_args,gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED ) 
		{
			return 16;
		}

		x_keys="K";
		for (chkidx=0; chkidx<MAXSORTKEYS; ++chkidx)
		{
			x_fldpos[chkidx] = x_fldlen[chkidx] = x_fldtyp[chkidx] = x_order[chkidx] = "K";
		}

		/*
		**	Validate KEYS field
		**
		**	The WISP WSORT utility allows KEYS=0. 
		**	This results in a "COPY" without sorting and can be used
		**	to unload an indexed file into a flat file.
		**	This should also work for SyncSort if you change to allow 0 keys.
		**	This would not be valid with a MERGE as you must have a key to merge on.
		*/
		if (keys_field[0] < '1' || keys_field[0] > '8')
		{
			messid="ER01";
			mess1="\224SORRY\204- Number of keys is invalid";
			x_keys="R";
			goto key_respec;
		}
		keys->keycnt = keys_field[0] - '0';
		
		for (chkidx=0; chkidx < keys->keycnt; ++chkidx)
		{
			/*
			**	Validate POSTx fields
			*/
			if (0 != field2uint(k[chkidx].fldpos_field, FLDPOS_FIELD_LEN, &keys->keyinfo[chkidx].fldpos) || 
				keys->keyinfo[chkidx].fldpos < 1)
			{
				messid="ER03";
				mess1="\224SORRY\204- Key position equal to 0";
				x_fldpos[chkidx]="R";
				goto key_respec;
			}
			
			if ( keys->keyinfo[chkidx].fldpos > reclen )
			{
				messid="ER04";
				mess1="\224SORRY\204- Key position greater then record size";
				x_fldpos[chkidx]="R";
				goto key_respec;
			}

			/*
			**	Validate LENGTHx fields
			*/
			if (0 != field2uint(k[chkidx].fldlen_field, FLDLEN_FIELD_LEN, &keys->keyinfo[chkidx].fldlen) || 
				keys->keyinfo[chkidx].fldlen < 1)
			{
				messid="ER05";
				mess1="\224SORRY\204- Key length equal to 0";
				x_fldlen[chkidx]="R";
				goto key_respec;
			}

			if ( keys->keyinfo[chkidx].fldpos + keys->keyinfo[chkidx].fldlen - 1 > reclen )
			{
				messid="ER06";
				mess1="\224SORRY\204- Key position plus key length exceeds record size";
				x_fldlen[chkidx]="R";
				goto key_respec;
			}

			/*
			**	Validate TYPEx fields
			*/
			if (!strchr("BCDFPZLU",k[chkidx].fldtyp_field[0]))
			{
				messid="ER09";
				mess1 = "\224SORRY\204- Key type must be one of B,C,D,F,P,Z,L,U";
				x_fldtyp[chkidx]="R";
				goto key_respec;
			}
			if (k[chkidx].fldtyp_field[0] == 'D')
			{
				wsort_abort("KEYS TYPE=D is not supported with SyncSort");
			}
			keys->keyinfo[chkidx].fldtyp = k[chkidx].fldtyp_field[0];

			switch(keys->keyinfo[chkidx].fldtyp)
			{
			case 'B':
			case 'U':
				if (keys->keyinfo[chkidx].fldlen != 2 &&
				    keys->keyinfo[chkidx].fldlen != 4 &&
				    keys->keyinfo[chkidx].fldlen != 8 )
				{
					messid="ER08";
					mess1="\224SORRY\204- The binary key length must be 2, 4 or 8 bytes";
					x_fldlen[chkidx]="R";
					goto key_respec;
				}
				break;
			case 'F':
				if (keys->keyinfo[chkidx].fldlen != 4 &&
				    keys->keyinfo[chkidx].fldlen != 8 )
				{
					messid="ER13";
					mess1="\224SORRY\204- Floating point key length must be either 4 or 8 bytes";
					x_fldlen[chkidx]="R";
					goto key_respec;
				}
				break;
			case 'P':
				if (keys->keyinfo[chkidx].fldlen > 16)
				{
					messid="ER07";
					mess1="\224SORRY\204- Packed decimal key length greater than 16 bytes";
					x_fldlen[chkidx]="R";
					goto key_respec;
				}
				break;
			case 'Z':
			case 'L':
				if (keys->keyinfo[chkidx].fldlen > 18)
				{
					messid="ER07";
					mess1="\224SORRY\204- Zoned key length greater than 18 bytes";
					x_fldlen[chkidx]="R";
					goto key_respec;
				}
				break;
			}

			/*
			**	Validate ORDERx fields
			*/
			if (k[chkidx].order_field[0] != 'A' && k[chkidx].order_field[0] != 'D')
			{
				messid="ER10";
				mess1="\224SORRY\204- Key order must be 'A' or 'D'";
				x_order[chkidx]="R";
				goto key_respec;
			}
			keys->keyinfo[chkidx].order = k[chkidx].order_field[0];

		}
		return 0;
		
	key_respec:
		
		gptype = "R ";
	}
}

/*
**	Routine:	get_reformat()
**
**	Function:	Issue the FORMAT getparm(s)
**
**	Description:	If Field 10 is filled in then another FORMAT getparm
**			is issued. These secondary getparms are identical
**			to the first except LENGTH and PAD fields are protected.
**
**			Record LENGTH field is initialized to the input file length.
**
**			Doesn't care if missing Fields.
**
**	Arguments:
**	input_recsize	The input file record length
**	reformat	The reformat info to return
**
**	Globals:	None
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
*/
static int get_reformat(unsigned int input_recsize, struct format_getparms_s *reformat)
{
#define NUMFORMATFIELDS		10
#define INPOS_FIELD_LEN		4
#define LENGHT_FIELD_LEN	4
#define OUTPOS_FIELD_LEN	4
#define PAD_FIELD_LEN		2

	int4	pfkey_mask;
	char	*gptype;
	char	*messid,*mess1,*mess2,*blank=" ";
	char	*issuer = ISSUER_SORT;
	int 	chkidx;
	char    *x_length, recsize_field[RECSIZE_FIELD_LEN+1];
	char    *x_pad,    pad_field[PAD_FIELD_LEN+1];
	char	*x_inpos[NUMFORMATFIELDS], *x_len[NUMFORMATFIELDS], *x_outpos[NUMFORMATFIELDS];
	struct
	{
		char	inpos_field[INPOS_FIELD_LEN+1];
		char	len_field[LENGHT_FIELD_LEN+1];
		char	outpos_field[OUTPOS_FIELD_LEN+1];
	} k[NUMFORMATFIELDS];
	int initial_getparm = TRUE;
	int starting_field_cnt = 0;

	reformat->pad_char = ' ';
	reformat->field_cnt = 0;
	reformat->recsize = 0;

	/* Initialize the fields */
	uint2field(input_recsize,recsize_field,RECSIZE_FIELD_LEN); /* Init to input record length */
	x_length="K";

	memset(pad_field, ' ', PAD_FIELD_LEN);
	x_pad="K";

	memset(k,' ',sizeof(k));
	for (chkidx=0; chkidx<NUMFORMATFIELDS; ++chkidx)
	{
		x_inpos[chkidx] = x_len[chkidx] = x_outpos[chkidx] = "K";
	}

	pfkey_mask = PFKEY_16_ENABLED ;
	wswap(&pfkey_mask);
	gptype = "I ";

	mess1="Please enter the output record format:";
	mess2=blank;
	
	messid = "0000";
	
	for(;;)
	{
		char	pfkey_recv[1];
		int	col;

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "FORMAT  "; GP pfkey_recv; GP messid; GP issuer; GPNUM(2); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));

		GPCTEXT("Output record",9,2);
		if (initial_getparm)
		{
			GPKW(x_length,"LENGTH  ",recsize_field,RECSIZE_FIELD_LEN,9,17,"N");
			GPKW(x_pad,"PAD     ",pad_field,PAD_FIELD_LEN,9,35,"C");
		}
		else
		{
			GPCTEXT("LENGTH   =",9,17);
			GPTEXT(recsize_field,RECSIZE_FIELD_LEN,9,17+11);

			GPCTEXT("PAD      =",9,35);
			GPTEXT(pad_field,PAD_FIELD_LEN,9,35+11);
		}
		GPCTEXT("(2 Hex digits or 1 character)",9,49);

		GPCTEXTU("Input position        Length        Output Position",11,14);
		GPCTEXT("Field 1:", 12,2);
		GPCTEXT("Field 2:", 13,2);
		GPCTEXT("Field 3:", 14,2);
		GPCTEXT("Field 4:", 15,2);
		GPCTEXT("Field 5:", 16,2);
		GPCTEXT("Field 6:", 17,2);
		GPCTEXT("Field 7:", 18,2);
		GPCTEXT("Field 8:", 19,2);
		GPCTEXT("Field 9:", 20,2);
		GPCTEXT("Field 10:",21,2);
                
		col=14;
		GPKW(x_inpos[0], "INPOS1  ",k[0].inpos_field, INPOS_FIELD_LEN,12,col,"N");
		GPKW(x_inpos[1], "INPOS2  ",k[1].inpos_field, INPOS_FIELD_LEN,13,col,"N");
		GPKW(x_inpos[2], "INPOS3  ",k[2].inpos_field, INPOS_FIELD_LEN,14,col,"N");
		GPKW(x_inpos[3], "INPOS4  ",k[3].inpos_field, INPOS_FIELD_LEN,15,col,"N");
		GPKW(x_inpos[4], "INPOS5  ",k[4].inpos_field, INPOS_FIELD_LEN,16,col,"N");
		GPKW(x_inpos[5], "INPOS6  ",k[5].inpos_field, INPOS_FIELD_LEN,17,col,"N");
		GPKW(x_inpos[6], "INPOS7  ",k[6].inpos_field, INPOS_FIELD_LEN,18,col,"N");
		GPKW(x_inpos[7], "INPOS8  ",k[7].inpos_field, INPOS_FIELD_LEN,19,col,"N");
		GPKW(x_inpos[8], "INPOS9  ",k[8].inpos_field, INPOS_FIELD_LEN,20,col,"N");
		GPKW(x_inpos[9], "INPOS10 ",k[9].inpos_field, INPOS_FIELD_LEN,21,col,"N");

		col=32;
		GPKW(x_len[0],   "LENGTH1 ",k[0].len_field,   LENGHT_FIELD_LEN,12,col,"N");
		GPKW(x_len[1],   "LENGTH2 ",k[1].len_field,   LENGHT_FIELD_LEN,13,col,"N");
		GPKW(x_len[2],   "LENGTH3 ",k[2].len_field,   LENGHT_FIELD_LEN,14,col,"N");
		GPKW(x_len[3],   "LENGTH4 ",k[3].len_field,   LENGHT_FIELD_LEN,15,col,"N");
		GPKW(x_len[4],   "LENGTH5 ",k[4].len_field,   LENGHT_FIELD_LEN,16,col,"N");
		GPKW(x_len[5],   "LENGTH6 ",k[5].len_field,   LENGHT_FIELD_LEN,17,col,"N");
		GPKW(x_len[6],   "LENGTH7 ",k[6].len_field,   LENGHT_FIELD_LEN,18,col,"N");
		GPKW(x_len[7],   "LENGTH8 ",k[7].len_field,   LENGHT_FIELD_LEN,19,col,"N");
		GPKW(x_len[8],   "LENGTH9 ",k[8].len_field,   LENGHT_FIELD_LEN,20,col,"N");
		GPKW(x_len[9],   "LENGTH10",k[9].len_field,   LENGHT_FIELD_LEN,21,col,"N");

		col=50;
		GPKW(x_outpos[0],"OUTPOS1 ",k[0].outpos_field,OUTPOS_FIELD_LEN,12,col,"N");
		GPKW(x_outpos[1],"OUTPOS2 ",k[1].outpos_field,OUTPOS_FIELD_LEN,13,col,"N");
		GPKW(x_outpos[2],"OUTPOS3 ",k[2].outpos_field,OUTPOS_FIELD_LEN,14,col,"N");
		GPKW(x_outpos[3],"OUTPOS4 ",k[3].outpos_field,OUTPOS_FIELD_LEN,15,col,"N");
		GPKW(x_outpos[4],"OUTPOS5 ",k[4].outpos_field,OUTPOS_FIELD_LEN,16,col,"N");
		GPKW(x_outpos[5],"OUTPOS6 ",k[5].outpos_field,OUTPOS_FIELD_LEN,17,col,"N");
		GPKW(x_outpos[6],"OUTPOS7 ",k[6].outpos_field,OUTPOS_FIELD_LEN,18,col,"N");
		GPKW(x_outpos[7],"OUTPOS8 ",k[7].outpos_field,OUTPOS_FIELD_LEN,19,col,"N");
		GPKW(x_outpos[8],"OUTPOS9 ",k[8].outpos_field,OUTPOS_FIELD_LEN,20,col,"N");
		GPKW(x_outpos[9],"OUTPOS10",k[9].outpos_field,OUTPOS_FIELD_LEN,21,col,"N");
		
		
		GPENTER();
		GPPFS(&pfkey_mask);

		CALL_GETPARM2(gp_args,gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED ) 
		{
			return 16;
		}

		/* Reset the attributes before error checking */
		x_length="K";
		x_pad="K";
		for (chkidx=0; chkidx<NUMFORMATFIELDS; ++chkidx)
		{
			x_inpos[chkidx] = x_len[chkidx] = x_outpos[chkidx] = "K";
		}

		if (initial_getparm)
		{
			/*
			**	Validate LENGTH field: 1 - 2048
			*/
			if (0 != field2uint(recsize_field, RECSIZE_FIELD_LEN, &reformat->recsize)  ||
			    reformat->recsize < 1 || reformat->recsize > MAXRECSIZE	)
			{
				messid="ER50";
				mess1="\224SORRY\204- LENGTH must be greater then 0 and less than or equal to 2048";
				x_length="R";
				goto format_respec;
			}

			/*
			**	Validate PAD field: 2 hex digits or one character
			*/
			if (pad_field[0] == ' ')
			{
				reformat->pad_char = pad_field[1];
			}
			else if (pad_field[1] == ' ')
			{
				reformat->pad_char = pad_field[0];
			}
			else if (isxdigit((int)(pad_field[0])) && isxdigit((int)(pad_field[1])))
			{
				char buf[3];
				char *ptr;
				unsigned long l;

				buf[0] = pad_field[0];
				buf[1] = pad_field[1];
				buf[2] = '\0';
				l = (unsigned long) strtol(buf,&ptr,16);
				reformat->pad_char = (char)l;
			}
			else
			{
				messid="ER51";
				mess1="\224SORRY\204- Invalid HEX digits specified for PAD";
				x_pad="R";
				goto format_respec;
			}
		}
		
		for (chkidx=0; chkidx < NUMFORMATFIELDS; ++chkidx)
		{
			unsigned int inpos, len, outpos;
			unsigned int i;

			if (	0==memcmp(k[chkidx].inpos_field,  "    ", INPOS_FIELD_LEN) &&
				0==memcmp(k[chkidx].len_field,    "    ", LENGHT_FIELD_LEN) &&
				0==memcmp(k[chkidx].outpos_field, "    ", OUTPOS_FIELD_LEN))
			{
				/* Skip all blank field spec */
				continue;
			}

			/*
			**	Validate INPOSx fields
			*/
			if (0 != field2uint(k[chkidx].inpos_field, INPOS_FIELD_LEN, &inpos) ||
			    inpos < 1 || inpos > input_recsize)
			{
				messid="ER52";
				mess1="\224SORRY\204- Invalid INPUT position";
				x_inpos[chkidx]="R";
				goto format_respec;
			}

			/*
			**	Validate LENGTHx fields
			*/
			if (0 != field2uint(k[chkidx].len_field, LENGHT_FIELD_LEN, &len) ||
			    len < 1 || (inpos+len-1 > input_recsize))
			{
				messid="ER53";
				mess1="\224SORRY\204- Invalid LENGTH";
				x_len[chkidx]="R";
				goto format_respec;
			}

			/*
			**	Validate OUTPOSx fields
			*/
			if (0 != field2uint(k[chkidx].outpos_field, OUTPOS_FIELD_LEN, &outpos) ||
			    outpos < 1 || (outpos+len-1 > reformat->recsize))
			{
				messid="ER55";
				mess1="\224SORRY\204- Invalid OUTPUT position";
				x_outpos[chkidx]="R";
				goto format_respec;
			}

			/*
			**	Check for overlapping fields
			**	- check each previously defined field
			*/
			for(i=0; i<reformat->field_cnt; i++)
			{
				if (outpos > (reformat->field[i].outpos+reformat->field[i].len-1))
				{
					/* OK - Beyond the end of this field */
				}
				else if ((outpos+len-1) < reformat->field[i].outpos)
				{
					/* OK - Before the start of this field */
				}
				else
				{
					messid="ER57";
					mess1="\224SORRY\204- You may not define overlapping fields";
					x_outpos[chkidx]="R";
					goto format_respec;
				}
			}

			/*
			**	Check for too many fields
			*/
			if (reformat->field_cnt >= MAXREFORMATFIELDS)
			{
				messid="ER99";
				mess1="\224SORRY\204- Maximum number of fields exceeded";
				x_inpos[chkidx]="R";
				goto format_respec;
			}

			/*
			**	Add the field info.
			*/
			reformat->field[reformat->field_cnt].inpos = inpos;
			reformat->field[reformat->field_cnt].len = len;
			reformat->field[reformat->field_cnt].outpos = outpos;
			reformat->field_cnt++;


		}

		/*
		**	Ensure at least one field specified.
		*/
		if (reformat->field_cnt == 0)
		{
			messid="ER56";
			mess1="\224SORRY\204- The output record must have at least one field !";
			x_inpos[0]="R";
			goto format_respec;
		}

		/*
		**	If Field 10 specifed then issue another getparm
		*/
		if (0!=memcmp(k[9].inpos_field, "    ", INPOS_FIELD_LEN))
		{
			initial_getparm = FALSE;
			starting_field_cnt = reformat->field_cnt; /* Save the starting count */

			/* 
			**	Reset fields for a new getparm
			*/
			gptype = "I ";
			memset(k,' ',sizeof(k));
			mess1="Please enter the output record format:";
			mess2=blank;
			messid = "0000";
	
			continue;
		}

		return 0;
		
	format_respec:
		/* 
		**	On an error need to reset the field count
		*/
		reformat->field_cnt = starting_field_cnt;
		
		gptype = "R ";
	}
}


/*
**	Routine:	get_output()
**
**	Function:	To get the output file specs for COPY.
**
**	Description:	Put up the OUTPUT getparm for COPY and validate all fields.
**
**	Arguments:
**	output->o.file		The output file 	(returned)	
**	output->o.lib		The output library 	(returned)
**	output->o.vol		The output volume 	(returned)
**	output->o.filename	The native output filename.
**	output->replace_flag	Flag if doing a replace.
**	output->o.fileorg		The file organization (C, R, or I)
**	output->o.rectype		The record type (F ov V)
**	input->infile_count
**	input->list[0].file
**	input->list[0].lib
**	input->list[0].vol
**
**	Return:		Pfkey number
**	0		Continue
**	1		Terminate without error
**	16		Exit with error
**
*/
static int get_output(struct output_getparm_s *output, 
		      struct input_getparms_s *input,
		      int reformat_flag)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*mess1,*mess2,*mess3,*mess4,*mess5,*blank=" ";
	char	*messid;
	char	*issuer = ISSUER_SORT;
	char    *termmsg1,*termmsg2;
	int	retcode;
	char	*x_file,*x_library,*x_volume,*x_replace,*x_recsize,*x_fileorg,*x_rectype;
	char    replace_field[YESNO_FIELD_LEN+1];
	char	fileorg_field[FILEORG_FIELD_LEN+1];
	char	recsize_field[RECSIZE_FIELD_LEN+1];
	char	rectype_field[RECTYPE_FIELD_LEN+1];
	int4	mode;
	char	*ptr;
	unsigned int	new_recsize;

	x_file = x_library = x_volume = x_replace = x_recsize = x_fileorg = x_rectype = "K";
	mess1 = mess2 = mess3 = mess4 = mess5 = blank;

	gptype = "I ";
	messid = "0000";
	mess3="Please specify the output file below:";

	memset(output->o.file,' ',SIZEOF_FILE);
	memcpy(output->o.lib,def_outlib,SIZEOF_LIB);
	memcpy(output->o.vol,def_outvol,SIZEOF_VOL);
	output->replace_flag = FALSE;
	uint2field(output->o.recsize, recsize_field, RECSIZE_FIELD_LEN);

	fileorg_field[0]='C';
	rectype_field[0]='F';
	termmsg1=NULL;
	termmsg2=terminate_text;
	pfkey_mask = PFKEY_16_ENABLED ;
	wswap(&pfkey_mask);

	if (output->replace_flag)
	{
		memcpy(replace_field,"YES",YESNO_FIELD_LEN);
	}
	else
	{
		memcpy(replace_field,"NO ",YESNO_FIELD_LEN);
	}
	
	for(;;)
	{
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "OUTPUT   "; GP pfkey_recv; GP messid; GP issuer; GPNUM(5); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));
			GP mess3; GPNUM(strlen(mess3));
			GP mess4; GPNUM(strlen(mess4));
			GP mess5; GPNUM(strlen(mess5));

		GPCTEXT("OUTPUT",13,2);
		GPKW(x_file,"FILE    ",output->o.file,SIZEOF_FILE,13,9,"L");
		GPCTEXT("in",13,30);
		GPKW(x_library,"LIBRARY ",output->o.lib,SIZEOF_LIB,13,33,"L");
		GPCTEXT("on",13,54);
		GPKW(x_volume,"VOLUME  ",output->o.vol,SIZEOF_VOL,13,57,"L");

		if (input->infile_count==1) /* allow_replace */
		{
			GPCTEXT("Replace input file with same name?",15,2);
			GPKW(x_replace,"REPLACE ",replace_field,YESNO_FIELD_LEN,15,50,"A");
			GPCTEXT("(YES or NO)",15,68);
		}

		if (!reformat_flag)
		{
			/*
			**	FILEORG and RECSIZE don't appear if REFORMAT=YES
			*/
			GPCTEXT("Fileorg - Consecutive?",16,2);
			GPKW(x_fileorg,"FILEORG ",fileorg_field,FILEORG_FIELD_LEN,16,50,"A");
			GPCTEXT("(C)",16,68);

			GPCTEXT("(Maximum) Record Size:",19,2);
			GPKW(x_recsize,"RECSIZE ",recsize_field,RECSIZE_FIELD_LEN,19,50,"N");

			/*
			**	Only RECTYPE=F is supported, no need to ask.
			*/
			/*
			GPCTEXT("Fixed length records?",20,2);
			GPKW(x_rectype,"RECTYPE ",rectype_field,RECTYPE_FIELD_LEN,20,50,"A");
			GPCTEXT("(F)",20,68);
			*/
		}

		if (termmsg1)
		{
 			GPCTEXT(termmsg1,22,18);
		}
		GPCTEXT(termmsg2,23,18);
		
		GPENTER();
		GPPFS(&pfkey_mask);

		CALL_GETPARM2(gp_args,gp_cnt);

		if (0==memcmp(gptype,"RD",2))
		{
			/*
			**	Updated the getparm
			*/
			return 0;
		}

		if (pfkey_recv[0] == PFKEY_16_PRESSED ) 
		{
			return 16;
		}
		if (pfkey_recv[0] == PFKEY_1_PRESSED) 
		{
			return 1;
		}

		/*
		**	Prepare for a possible error
		*/
		gptype = "R ";
		x_file = x_library = x_volume = x_replace = x_recsize = x_fileorg = x_rectype = "K";
		mess1 = mess2 = mess3 = mess4 = mess5 = blank;
		messid = "E099";
		termmsg1="Press (1)  to terminate without error status";
		termmsg2="Press (16) to terminate with error status";
		pfkey_mask = PFKEY_1_ENABLED | PFKEY_3_ENABLED | PFKEY_16_ENABLED ;
		wswap(&pfkey_mask);


		/*
		**	Validate all of the GETPARM fields 
		*/
		leftjust(output->o.file,SIZEOF_FILE);
		leftjust(output->o.lib,SIZEOF_LIB);
		leftjust(output->o.vol,SIZEOF_VOL);

		if (' '==output->o.vol[0] || ' '==output->o.lib[0] || ' '==output->o.file[0])
		{
			messid = "R001";
			mess1 = "\224SORRY\204- File specification is incomplete, please respecify.";
			
			if (' '==output->o.vol[0])
			{
				x_volume = "R";
			}
			if (' '==output->o.lib[0])
			{
				x_library = "R";
			}
			if (' '==output->o.file[0])
			{
				x_file = "R";
			}
			continue;
		}

		/*
		**	Check if volume exists
		*/
		{
			int4	start, count;
			char	recvr[22];	/* 8+8+6 */
			char	find_file[SIZEOF_FILE], find_lib[SIZEOF_LIB];


			memset(find_file, ' ', SIZEOF_FILE);
			memset(find_lib, ' ', SIZEOF_LIB);
			start = 1;
			wswap(&start);
			count = 1;
			wswap(&count);

			set_arg_count(6);
			FIND(find_file, find_lib, output->o.vol, &start, &count, recvr);

			wswap(&count);
			if (count == 0)
			{
				messid = "R042";
				mess1="\224SORRY\204- The output VOLUME specified was not found, please respecify.";
				x_volume = "R";
				continue;
			}
		}

		if (input->infile_count==1) /* allow_replace */
		{
			if (strncmp(replace_field,"YES",YESNO_FIELD_LEN) && 
			    strncmp(replace_field,"NO ",YESNO_FIELD_LEN))
			{
				mess1="\224SORRY\204- REPLACE option must be YES or NO, please respecify.";
				messid="0030";
				x_replace="R";
				continue;
			}
			else if (strncmp(replace_field,"YES",YESNO_FIELD_LEN)==0)
			{
				output->replace_flag = TRUE;

				if (	0 != memcmp(output->o.file,input->list[0].file,SIZEOF_FILE))
				{
					mess1="\224SORRY\204- Filename does not correspond with an input file, please respecify.";
					messid="0034";
					x_replace="R";
					x_file = "R";
					continue;
				}
				if (	0 != memcmp(output->o.lib, input->list[0].lib,SIZEOF_LIB))
				{
					mess1="\224SORRY\204- Library does not correspond with an input file, please respecify.";
					messid="0035";
					x_replace="R";
					x_library = "R";
					continue;
				}
				if (	0 != memcmp(output->o.vol, input->list[0].vol,SIZEOF_VOL))
				{
					mess1="\224SORRY\204- Volume does not correspond with an input file, please respecify.";
					messid="0036";
					x_replace="R";
					x_volume = "R";
					continue;
				}
			}
			else
			{
				output->replace_flag = FALSE;
			}

		}

		if (!reformat_flag)
		{
			/*
			**	FILEORG and RECSIZE don't appear if REFORMAT=YES
			*/
		
			if ('C' != fileorg_field[0])
			{
				messid="ER60";
				mess1="\224SORRY\204- Only FILEORG=C is supported, please respecify.";
				x_fileorg="R";
				continue;
			}
			output->o.fileorg = fileorg_field[0];


			/*
			**	RECSIZE ????
			**	- I don't really "get" how this works if the output record size is 
			**	  different then the input records size.
			**
			*/
			if (field2uint(recsize_field,RECSIZE_FIELD_LEN,&new_recsize))
			{
				messid="ER98";
				mess1="\224SORRY\204- RECSIZE field is invalid, please respecify.";
				x_recsize="R";
				continue;
			}
			if (new_recsize != output->o.recsize)
			{
				/*
				**	If RECSIZE has changed then give error message - not supported.
				*/
				messid="ER98";
				mess1="\224ERROR\204- Changing RECSIZE is not supported.";
				x_recsize="R";
				continue;
			}

			/*
			**	Only RECTYPE=F is supported but there is no benefit to giving
			**	an error if V is specified.
			**	Always use default 'F'.
			*/
			/*
			if ('F' != rectype_field[0])
			{
				messid="ER78";
				mess1="\224SORRY\204- Only RECTYPE=F is supported, please respecify.";
				x_rectype="R";
				continue;
			}
			output->o.rectype = rectype_field[0];
			*/
		}

		/*
		**	If the output file exists then prompt to scratch.
		**	(If overwriting then don't scratch because we need the file to do the sort.)
		*/
		if (!output->replace_flag)
		{
			int4	start, count;
			char	recvr[22];	/* 8+8+6 */

			start = 1;
			wswap(&start);
			count = 1;
			wswap(&count);

			set_arg_count(6);
			FIND(output->o.file, output->o.lib, output->o.vol, &start, &count, recvr);

			wswap(&count);
			if (count != 0)
			{
				/*
				**	If PF3 was pressed (second time thru)
				**	then scratch the file.
				*/
				if (pfkey_recv[0] == PFKEY_3_PRESSED)
				{
					set_arg_count(5);
					SCRATCH("F",output->o.file,output->o.lib,output->o.vol,&retcode);	/* Scratch the target file			*/
					wswap(&retcode);
					if (retcode != 0)
					{
						char	buff[80];
						messid = "F002";
						sprintf(buff,"\224ERROR\204- SCRATCH failed return code = %04d",retcode);
						mess1 = buff;
						x_file = "R";
						continue;
					}

				}
				else
				{
					messid = "A004";
					mess1 = "\204FILE ALREADY EXISTS.  RESPECIFY OR PRESS (3) TO SCRATCH IT.";
					x_file = "R";
					continue;
				}
			}
		}
		
		/*
		**	Generate the native filename and update the PUTPARM.
		*/

		mode = 0x00001001; /* IS_OUTPUT + IS_BACKFILL */
		ptr = WFNAME(&mode,output->o.vol,output->o.lib,output->o.file,output->o.filename);
		*ptr = (char)0;
		WL_makepath(output->o.filename);

		/*
		**	Update the PUTPARM with updated file info.
		**	The WL_use_last_prb() is an internal "hint" to GETPARM
		**	to use the last PRB.  This will prevent GETPARM from
		**	possibly updating the wrong "OUTPUT" putparm if
		**	there are multiple.
		**
		**	GETPARM does not fully correctly handle link-levels
		**	and PUTPARM chaining so this scenerio can happen:
		**	If the original "OUTPUT" putparm is not labeled then
		**	it will be deleted once used (instead of just being
		**	marked as used).  If there is another OUTPUT putparm
		**	even at a lower link-level it will be found by the
		**	RD GETPARM and incorrectly updated.  The call to 
		**	WL_use_last_prb() tell GETPARM to just update the last
		**	PUTPARM if it still exists instead of going looking
		**	for a matching PUTPARM.
		*/	
		WL_use_last_prb();
		gptype = "RD";
	}
}

static void leftjust(char *ptr, int cnt)	/* Left justify char strings. 	*/
{
	int	i,j;

	if ( *ptr != ' ' ) return;

	for(i=0; i<cnt && ptr[i]==' '; i++);			/* i = number of leading spaces.	*/

	if ( i == cnt ) return;					/* If all spaces then nothing to do.	*/

	for(j=0    ; j+i<cnt; j++)  ptr[j] = ptr[j+i];		/* Shift left thru leading spaces.	*/
	for(j=cnt-i; j  <cnt; j++)  ptr[j] = ' ';		/* Pad with trailing blanks.		*/

}

/*
**	Routine:	field2uint()
**
**	Function:	Convert a user entered number into an unsigned int.
**
**	Description:	Handles only unsigned number.
**			Can have both leading and trailing spaces.
**
**	Arguments:
**	field		The input field
**	len		The field length
**	num		The returned value of the field
**
**	Globals:	None
**
**	Return:
**	0		Success
**	1		Invalid input
**
**	Warnings:	None
**
*/
static int field2uint(const char *str, unsigned int len, unsigned int *num)
{
	unsigned int	idx;

	*num = 0;

	/*
	**	Skip over leading spaces;
	*/
	for(idx=0; idx<len && ' '==str[idx]; idx++);

	/*
	**	Empty field
	*/
	if (idx == len) return 1;

	/*
	**	Load up the number
	*/
	for(; idx<len && ' ' != str[idx]; idx++)
	{
		if (str[idx] >= '0' && str[idx] <= '9')
		{
			*num = (*num * 10) + (str[idx] - '0');
		}
		else
		{
			return 1;
		}
	}

	/*
	**	Ensure that only spaces remain
	*/
	for(; idx<len; idx++)
	{
		if (' ' != str[idx])
		{
			return 1;
		}
	}

	return 0;
}

static void uint2field(unsigned int num, char *field, unsigned int len)
{
	char buff[100];
	sprintf(buff,"%*.*u", len, len, num);
	memcpy(field, buff, len);
}


/*
**	Routine:	wswap()
**
**	Function:	swap the order of the half-words in a long-word item (for WANG routines to use)
**
**	Description:	Wang COBOL-74 used 2 x 2-byte Integer (BINARY) fields 
**			in order to make up a 4-byte Integer field.
**
**			01 MY-4-BYTE-INT.
**			   05  HIGH-2-BYTES  USAGE IS BINARY.
**			   05  LOW-2-BYTES   USAGE IS BINARY.
**			
**			On Little-Endian machines the bytes within the word 
**			need to be rearranged to form a native 32-bit integer.
**			On Big-Endian machines this routine does nothing.
**
**	Arguments:
**	lword		A Wang COBOL INT(4) field. 
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
static void wswap(void *lword)
{
	/*
	**	This is an abbreviated version of WL_swap that works
	**	only on big-endian machines and WIN32.
	*/

#ifdef WIN32
	int2 *swords;
	int2 temp;				/* used for the swap			*/
	swords = (int2 *)lword;

	temp = swords[0];			/* get high order word			*/
	swords[0] = swords[1];			/* move low to high			*/
	swords[1] = temp;			/* move high to low			*/
#endif /* WIN32 */
	
}


static void set_arg_count(int x)
{
	WVASET(&x);
}



static void error_getparm(	
	char prname[GP_PRNAME_LEN], char messid[GP_MESSID_LEN], char issuer[GP_ISSUER_LEN],
	char *msg1,char *msg2,char *msg3,char *msg4,
	char *msg5,char *msg6,char *msg7,char *msg8,
	char *msg9,char *msg10,char *msg11,char *msg12)
{                                                  
	int4	pfkey_mask;
	char	pfkey_rcvr;
	char	*cancel_msg;


	pfkey_mask = PFKEY_16_ENABLED;
	cancel_msg = "Press (16) to Cancel Processing    ";
	

	wswap( &pfkey_mask );

	gp_cnt = 0;
	GP  "R ";	
	GP  "R";	
	GP  prname;	
	GP  &pfkey_rcvr;	
	GP  messid;	
	GP  issuer;	
	GPNUM(0);
	GPCTEXT("***  ERROR Processor  ***", 9, 28);
	if (msg1 != NULL)
	{
		GPCTEXT(msg1, 11, 2);
	}
	if (msg2 != NULL)
	{
		GPCTEXT(msg2, 12, 2);
	}
	if (msg3 != NULL)
	{
		GPCTEXT(msg3, 13, 2);
	}
	if (msg4 != NULL)
	{
		GPCTEXT(msg4, 14, 2);
	}
	if (msg5 != NULL)
	{
		GPCTEXT(msg5, 15, 2);
	}
	if (msg6 != NULL)
	{
		GPCTEXT(msg6, 16, 2);
	}
	if (msg7 != NULL)
	{
		GPCTEXT(msg7, 17, 2);
	}
	if (msg8 != NULL)
	{
		GPCTEXT(msg8, 18, 2);
	}
	if (msg9 != NULL)
	{
		GPCTEXT(msg9, 19, 2);
	}
	if (msg10 != NULL)
	{
		GPCTEXT(msg10, 20, 2);
	}
	if (msg11 != NULL)
	{
		GPCTEXT(msg11, 21, 2);
	}
	if (msg12 != NULL)
	{
		GPCTEXT(msg12, 22, 2);
	}
	GPCTEXT(cancel_msg, 24, 22);
	GPNOENTER();
	GPPFS(&pfkey_mask);

	CALL_GETPARM2(gp_args,gp_cnt);

}

static void CALL_GETPARM2(char* args[], int args_count)
{
	set_arg_count(2);
	GETPARM(args, &args_count);
}


/*
**	Routine:	varstr_malloc()
**
**	Function:	Local front-end to malloc()
**
**	Description:	Call malloc() and abort if it fails.
**
**	Arguments:	
**	size		The number of bytes to malloc.
**
**	Return:		Point to the malloc-ed memory.
**
*/
#define VARSTR_CHUNK_SIZE 2048
static void *varstr_malloc(size_t size)
{
	void *ptr;
	ptr = malloc(size);
	if (ptr == NULL)
	{
		wsort_abort("MALLOC FAILED: OUT OF MEMORY");
	}
	return ptr;
}

static char* varstr_cat(struct varstr_s *buf, const char* addition)
{
	size_t addlen = strlen(addition);
	
	if (addlen+strlen(buf->str)+1 >= buf->size)	
	{
		/* Increase the size of the buff */
		char *ptr;
		ptr = buf->str;
		/* Ensure big enough to hold addition */
		buf->size += VARSTR_CHUNK_SIZE + addlen;	
		buf->str = (char*)varstr_malloc(buf->size);
		strcpy(buf->str,ptr);
		free(ptr);
	}

	strcat(buf->str, addition);

	return buf->str;
}

/*
**	Routine:	varstr_printf()
**
**	Function:	Add to a varstr with sprintf style args
**
**	Description:	
**
**	Arguments:	
**	str		The varstr_s str
**	format...	prinf style format args
**
**	Return:		The command buffer
**
*/
static void varstr_printf(struct varstr_s *str, const char* format, ... /* args */)
{
	va_list ap;

	if (format != NULL)
	{
		char	buff[2048];

		va_start(ap, format);
		vsprintf(buff, format, ap);
		va_end(ap);
		
		varstr_cat(str, buff);

	}
}

static struct varstr_s *varstr_new(void)
{
	struct varstr_s *buf;
	buf = (struct varstr_s *)varstr_malloc(sizeof(struct varstr_s));
	buf->size = VARSTR_CHUNK_SIZE;
	buf->str = (char*)varstr_malloc(buf->size);
	buf->str[0] = '\0';
	return buf;
}

static void varstr_free(struct varstr_s **buf)
{
	if (buf == NULL)
	{
		return;
	}
	if (*buf != NULL)
	{
		if ((*buf)->size != 0)
		{
			free((*buf)->str);
			(*buf)->str = NULL;
			(*buf)->size = 0;
		}
		free(*buf);
		*buf = NULL;
	}
}


/*
**	History:
**	$Log: sswsort.c,v $
**	Revision 1.1  2005/05/13 12:38:47  gsl
**	no message
**	
**	Revision 1.42  2003/05/21 20:00:57  gsl
**	no message
**	
**	
**	
**
*/
