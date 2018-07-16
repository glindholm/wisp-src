static char copyright[]="Copyright (c) 1988-1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
static char rcs_revision[]="$Revision:$";
static char rcs_state[]="$State: V4_3_06 $";

static	char 	*SORT_VERSION = "WISP SORT Program - Version 3.01.00";

/*
**	File:		wsort.c
**
**	Project:	wisp/utils
**
**	RCS:		$Source:$
**
**	Purpose:	To hold WSORT utility source file.
**
**	Routines:	
**	main()		The WSORT main routine.
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef VMS
#include <unixio.h>
#include <file.h>
#endif

#ifdef WATCOM
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#include "idsistd.h"

#define EXT_FILEXT
#include "filext.h"
#include "wcommon.h"
#include "wperson.h"
#include "wangkeys.h"

#include "vssort.h"
#include "wexit.h"
#include "level.h"
#include "wfname.h"
#include "idsisubs.h"
#include "sysdev.h"
#include "wispvers.h"
#include "wisplib.h"
#include "vwang.h"

/*
**	Structures and Defines
*/
#ifndef OSD_BLOCK_SIZE
#define OSD_BLOCK_SIZE 512
#endif

#define GP	gp_args.ptrs[gp_cnt++] = (char *)
#define	GPNUM(num)			GP &N[num]
#define	GPLEN(len)			GPNUM(len)
#define	GPAT(row,col)			GP "A";GPNUM(row);GP "A";GPNUM(col)
#define	GPFLD(fld,len,row,col)		GP fld;GPLEN(len);GPAT(row,col)
#define	GPTEXT(txt,ln,rw,cl)		GP "T";GPFLD(txt,ln,rw,cl)
#define	GPCTEXT(txt,rw,cl)		GPTEXT(txt,(strlen(txt)),rw,cl)
#define	GPKW(x,kw,rcv,ln,rw,cl,typ)	GP x;GP kw;GPFLD(rcv,ln,rw,cl);GP typ
#define	GPID(pn,pf,mn,mi)		GP pn;GP pf;GP mn; GP mi
#define	GPTYP(tp,rq)			GP tp;GP rq
#define	GPTOP(tp,rq,pn,pf,mn,mi,mc)	GPTYP(tp,rq);GPID(pn,pf,mn,mi);GPLEN(mc);
#define	GPSETUP()			init_gpint();gpcnt=0
#define	GPSTD(pn,mi,mc)			GPTOP("I ","R",pn,gppfrcvr,"0001",mi,mc)
#define GPRES(pn,mi,mc)                 GPTOP("R ","R",pn,gppfrcvr,"0001",mi,mc)
#define GPMSG(ln)                       GP ln; GPLEN(strlen(ln))

#define	GPFLV(fn,ln,vn,fr)	GPFILE(fn,fr,3);GPLIB(ln,fr,24);GPVOL(vn,fr,45)
#define	GPSYSNAME(sn,sr)		GPKW("SYSNAME ",sn,60,sr,3,"C")
#define	GPPFS(x)			GP "P";GP x
#define	GPENTER()			GP "E"
#define	GPNOENTER()			GP "N"

/*
**	Globals and Externals
*/

/*
**	Static data
*/

static	int4	N[255];
static	int4 	two=2;

static struct { char *ptrs[512]; } gp_args;
static int gp_cnt;

static char *legal_relations[]=
{
	"EQ", "NE", "GT", "GE", "LT", "LE", NULL
};

static int verbose = 0;

static char *terminate_text;
static int4 terminate_key_enabled;
static char terminate_key_pressed;

/*
**	Static Function Prototypes
*/

static int wsort(void);
static int get_input(char *ifile, char *ilib, char *ivol, char *infilename,
			int *select_flag, int *morefile_flag, int4 more_input, char *filetype, int4 *recsize);
static void record_lock(int fh);
static void free_locks(void);
static int get_lock(void);
static int get_options(int *stable_flag, int *lowspace_flag);
static int get_output(char *ofile, char *olib, char *ovol, char *outfilename, 
			int allow_replace, int *replace_flag, char *fileorg, int4 *maxrec, 
			char *outrectype, int errorgp, char *errmess1, char *errmess2, char *errmess3);
static int get_select(struct select_criteria selinfo[], int *selcnt);
static int get_keys(struct key_info keyinfo[], int *keycnt);
static int matchrel(char *p);

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
**	History:	
**	11/09/93 	borrowed from wcopy.c JEC
**
*/
main(argc,argv)
int	argc;
char	*argv[];
{
#ifdef WATCOM
	_fmode = O_BINARY;
#endif

	if ( argc > 1 )
	{
		if (0 == strcmp(argv[1],"-v"))
		{
			verbose = 1;
		}
	}

	vwang_title("WISP SORT");
	initglbs("WSORT   ");
	wisp_signal_handler();
	
	if (pfkeys12())
	{
		terminate_text = "Press (12) to terminate WSORT.";
		terminate_key_enabled = PFKEY_12_ENABLED;
		terminate_key_pressed = PFKEY_12_PRESSED;
	}
	else
	{
		terminate_text = "Press (16) to terminate WSORT.";
		terminate_key_enabled = PFKEY_16_ENABLED;
		terminate_key_pressed = PFKEY_16_PRESSED;
	}
	
	wexit(wsort());

	return 0;
}


/*
**	Routine:	wsort()
**
**	Function:	To provide a GETPARM interface like the Wang SORT utility.
**
**	Description:	To emulate the Wang SORT utility.
**
**	Arguments:	None
**
**	Globals:
**	N		The wswap'ed numbers array.
**
**	Return:
**	0		success
**
**	Warnings:	None
**
**	History:	
**	11/09/93        borrowed from WCOPY (vscopy) and modified for SORT --JEC
**
*/
static int wsort(void)
{
	char	ofile[9], olib[9], ovol[7];
	int4	infile_count;
	char    infilename_list[MAXSORTINFILES][80];
	char    infile_list[MAXSORTINFILES][9];
	char	inlib_list[MAXSORTINFILES][9];
	char	invol_list[MAXSORTINFILES][7];
	char	intype_list[MAXSORTINFILES];
	int4	inlen_list[MAXSORTINFILES];
	char	outfilename[80];
	
	int	pfkey;
	int	i;
	int4	retcode, errcode;
	int	select_flag;
	int	stable_flag;
	int4    outmaxrec;
	int	allow_replace, replace_flag;
	struct select_criteria selinfo[MAXSORTSELECTS];
	struct key_info keyinfo[MAXSORTKEYS];
	char    ofileorg[1],outrectype[1];
	int	selcnt,keycnt;
	int4	total_input_size;
	int	lowspace_flag;
	int	out_err;
	char	last_ovol[6];
	char	errmess1[256], errmess2[256], errmess3[256];
	
	retcode = 0;
	infile_count = 0;
	outmaxrec = 0;
	selcnt = 0;
	total_input_size = 0;

	lowspace_flag = 0;

	for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 				/* Initialize a swapped numbers array		*/
	{
		N[i]=i; 
		wswap(&N[i]);
	}

	memset(infile_list,0,sizeof(infile_list));
	memset(inlib_list,0,sizeof(inlib_list));
	memset(invol_list,0,sizeof(invol_list));
	memset(selinfo,' ',sizeof(selinfo));
	memset(ofile,' ',sizeof(ofile));
	memset(olib,' ',sizeof(olib));
	memset(ovol,' ',sizeof(ovol));
	
	/*
	**	OPTIONS getparm
	*/
	pfkey = get_options(&stable_flag, &lowspace_flag);
		
	if (16 == pfkey) return 16;

	{
		int	morefile_flag;
		int	more_input;
		char	ifile[9], ilib[9], ivol[7];
		char    filetype;
		int4	recsize;

		select_flag = FALSE;
		morefile_flag = FALSE;
		more_input = FALSE;

		get_defs(DEFAULTS_IL,ilib);
		get_defs(DEFAULTS_IV,ivol);

		for(;;)
		{
			memset(ifile,' ',8);

			/*
			**	INPUT getparm
			*/
			pfkey = get_input(ifile,ilib,ivol,infilename_list[infile_count],
					&select_flag,&morefile_flag,more_input,&filetype,&recsize);
			if (16 == pfkey) return 16;

			if (more_input && ' ' == ifile[0])
			{
				break;
			}

			memcpy(infile_list[infile_count],ifile,8);
			infile_list[infile_count][8] = (char)0;
			memcpy(inlib_list[infile_count],ilib,8);
			inlib_list[infile_count][8] = (char)0;
			memcpy(invol_list[infile_count],ivol,6);
			invol_list[infile_count][6] = (char)0;

			intype_list[infile_count] = filetype;
			inlen_list[infile_count] = recsize;
		        ++infile_count;

			if (lowspace_flag)
			{
				int4	fdr_mode, fdr_rc, fdr_argcnt;
				char	fdr_bs[19];

				/*
				**	Calculate the total size of the input files.
				*/
				fdr_mode = 0;
				fdr_rc = 0;
				fdr_argcnt = 7;
				wvaset(&fdr_argcnt);
				READFDR(ifile, ilib, ivol, &fdr_mode, "BS", fdr_bs, &fdr_rc);
				wswap(&fdr_rc);
				if (0 == fdr_rc)
				{
					int4	bs_tmp;
					if (0==numeric2int4(&bs_tmp, fdr_bs, 18))
					{
						total_input_size += bs_tmp;
					}
				}
			}

			if (recsize > outmaxrec) outmaxrec = recsize;

			if (morefile_flag)
			{
				more_input = TRUE;
			}
			else
			{
				break;
			}
		}
	}

	if (select_flag)
	{
		/*
		**	SELECT getparms
		*/
		pfkey = get_select(selinfo,&selcnt);
		if (16 == pfkey) return 16;
	}

	/*
	**	KEYS getparms
	*/
	pfkey = get_keys(keyinfo,&keycnt);
	if (16 == pfkey) return 16;


	if (infile_count==1)
	{
		allow_replace=1;
	}
	else
	{
		allow_replace=0;
	}

	memcpy(last_ovol, "      ", 6);

	replace_flag = 0;
	out_err = 0;
	errmess1[0] = errmess2[0] = errmess3[0] = (char)0;

	for(;;)
	{
		/* 
		**	Getparm OUTPUT info
		*/
		pfkey = get_output(ofile,olib,ovol,outfilename,allow_replace,&replace_flag,
				ofileorg,&outmaxrec,outrectype, 
				out_err, errmess1, errmess2, errmess3);
		if (16 == pfkey) return 16;
		if ( 1 == pfkey) return 0;

		out_err = 0;
		errmess1[0] = errmess2[0] = errmess3[0] = (char)0;

		if (allow_replace && replace_flag)
		{
			if (	0 != memcmp(ofile,infile_list[0],8) ||
				0 != memcmp(olib, inlib_list[0],8) ||
				0 != memcmp(ovol, invol_list[0],6)     )
			{
				out_err = 1;
				strcpy(errmess1, 
					"\224SORRY\204- REPLACE=YES, output file must match input file, please respecify.");
				continue;
			}
		}

#ifdef VMS
		if (lowspace_flag)
		{
			if (0 != memcmp(last_ovol,ovol,6))
			{
				/*
				**	Check if there is enough space on the output volume, if not then give
				**	the user a chance to make space or abort.
				*/
				int4	blocks_needed;
				uint4	blocks_available;
				char	testvol[7];

				memcpy(last_ovol,ovol,6);

				unloadpad(testvol, ovol, 6);
				blocks_available = osd_freeblocks(testvol);

				blocks_needed = (total_input_size / OSD_BLOCK_SIZE);

				if (blocks_available < blocks_needed)
				{
					out_err = 1;
					strcpy(errmess1, "\224WARNING\204- OUTPUT VOLUME IS LOW ON DISK SPACE");
					strcpy(errmess2, 
						"\204INSURE THAT THERE IS SUFFICIENT DISK SPACE FOR OUTPUT AND WORK FILES.");
					continue;
				}
			}
		}
#endif

		/*
		**	PERFORM THE SORT
		*/
		if (verbose)
		{
			vwang_shut();
			printf("WSORT: [%s] WISP Version = [%s]\n", SORT_VERSION, wisp_version());
			for (i=0; i<infile_count; i++)
			{
				printf("INPUT  FILE     = %8.8s in LIBRARY  = %8.8s on VOLUME   = %6.6s\n",
					infile_list[i], inlib_list[i], invol_list[i]);
			}
			printf("OUTPUT FILE     = %8.8s in LIBRARY  = %8.8s on VOLUME   = %6.6s\n",
				ofile, olib, ovol);
		}

		retcode = 0;

		vssort(infile_list, inlib_list, invol_list, infilename_list, intype_list, inlen_list, infile_count,
			ofile, olib, ovol, outfilename, 
			ofileorg[0], outmaxrec, outrectype[0], replace_flag,
			selinfo, selcnt, keyinfo, keycnt, stable_flag,
			&retcode, &errcode, errmess2);

		/*
		**	TEST FOR ERRORS
		*/

		if (0==errcode) 
		{
#ifdef VMS
			free_locks();
			if (replace_flag)
			{
				/*
				**	REPLACE=YES, delete the previous version. (PURGE)
				*/
				strcat(outfilename,";-1");
				delete(outfilename);
			}
#endif
			return retcode;
		}

		strcpy(errmess1,"\224ERROR\204- ERROR OCCURED DURING SORT OPERATION.");

		if (1==errcode)
		{
			out_err = 1;
		}
		else if (2 == errcode)
		{
			char	buff1[80], buff2[80];

			sprintf(buff1,"INPUT  FILE     = %8.8s in LIBRARY  = %8.8s on VOLUME   = %6.6s",
				infile_list[0], inlib_list[0], invol_list[0]);
			sprintf(buff2,"OUTPUT FILE     = %8.8s in LIBRARY  = %8.8s on VOLUME   = %6.6s",
				ofile, olib, ovol);
			err_getparm("ERROR   ", "0000", "WSORT ", errmess1, errmess2, " ",
				buff1, infilename_list[0], " ", buff2, outfilename);
			return retcode;
		}
	}
}


/*
**	Routine:	get_input()
**
**	Function:	To get the input file specs for SORT.
**
**	Description:	Put up the INPUT getparm for SORT and validate all fields.
**
**	Arguments:
**	ifile		The input file 			(returned)	
**	ilib		The input library	 	(returned)
**	ivol		The input volume	 	(returned)
**	infilename	The native input filename 	(returned)
**	select_flag	Is select records requested 	(returned)
**	morefile_flag	Is more files requested 	(returned)
**	more_input	Issues the secondary more files getparm
**	filetype	The filetype 			(returned)
**	recsize		The record size 		(returned)
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	The filetype and recsize are only used on UNIX and DOS.
**
**	History:	
**	03/02/93	Written by GSL
**	09/20/94	Rework. GSL
**	09/30/94	Add the infilename parameter. GSL
**
*/
static int get_input(char *ifile, char *ilib, char *ivol, char *infilename,
			int *select_flag, int *morefile_flag, int4 more_input, char *filetype, int4 *recsize)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*mess1,*mess2,*mess3,*mess4;
	char	*messid, *blank=" ";
	char	*x_file, *x_library, *x_volume, *x_shared, *x_select, *x_morefile, *x_filetype, *x_recsize;
	char    shared_field[3],select_field[3],morefile_field[3],filetype_field[1],recsize_field[6];
	int	shared_flag;

	mess1=mess2=mess3=mess4=blank;

	if (!more_input)
	{
		mess3="Please enter the name of the file to be sorted.";
	}
	else
	{
		mess3="Please enter the name of the next file to be sorted.";
		mess4="To end input, leave the input file blank.";
	}

	pfkey_mask = PFKEY_16_ENABLED | terminate_key_enabled;
	wswap(&pfkey_mask);
	gptype = "I ";
	messid = "0000";

	x_file = x_library = x_volume = x_shared = x_select = x_morefile = x_filetype = x_recsize = "K";

	memcpy(shared_field,"NO ",3);
	memcpy(select_field,"NO ",3);
	memcpy(morefile_field,"NO ",3);

	*filetype = 'I';
	filetype_field[0]='I';
	memcpy(recsize_field,"    0",5);
	recsize_field[5]= (char)0;
	shared_flag = 0;

	for(;;)
	{
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "INPUT   "; GP pfkey_recv; GP messid; GP "WSORT "; GPNUM(4); 
			GP mess1; GP &N[strlen(mess1)];
			GP mess2; GP &N[strlen(mess2)];
			GP mess3; GP &N[strlen(mess3)];
			GP mess4; GP &N[strlen(mess4)];

		GPCTEXT("INPUT",12,2);
		GPKW(x_file,"FILE    ",ifile,8,12,8,"L");
		GPCTEXT("in",12,28);
		GPKW(x_library,"LIBRARY ",ilib,8,12,31,"L");
		GPCTEXT("on",12,51);
		GPKW(x_volume,"VOLUME  ",ivol,6,12,54,"L");

		GPCTEXT("Is this a SHARED file ?",14,2);
		GPKW(x_shared,"SHARED  ",shared_field,3,14,47,"A");
#ifdef VMS
 		GPCTEXT("(YES or NO)",14,65);
#else
 		GPCTEXT("(NOT USED)",14,65);
#endif

		if (!more_input)
		{
			GPCTEXT("Do you want to select input records ?",15,2);
			GPKW(x_select,"SELECT  ",select_field,3,15,47,"A");
#ifdef VMS
	 		GPCTEXT("(YES or NO)",15,65);
#else
	 		GPCTEXT("(NO)",15,65);
#endif

			GPCTEXT("Do you have more input files ?",16,2);
			GPKW(x_morefile,"MOREFILE",morefile_field,3,16,47,"A");
#ifdef VMS
	 		GPCTEXT("(YES or NO)",16,65);
#else
	 		GPCTEXT("(NO)",16,65);
#endif
		}

#if defined(unix) || defined(MSDOS) || defined(WIN32)
		GPCTEXT("Is file type Indexed, Fixed, or Newline ?",18,2);
		GPKW(x_filetype,"FILETYPE",filetype_field,1,18,47,"A");
 		GPCTEXT("(I,F,N)",18,65);

		GPCTEXT("For FILETYPE=F, what is the record size ?",19,2);
		GPKW(x_recsize,"RECSIZE ",recsize_field,5,19,47,"N");
#endif
		GPCTEXT(terminate_text,22,26);

		GPENTER();
		GPPFS(&pfkey_mask);

		wvaset(&two);
		GETPARM(&gp_args,&gp_cnt);


		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}

		gptype = "R ";
		x_file = x_library = x_volume = x_shared = x_select = x_morefile = x_filetype = x_recsize = "K";
		mess1=mess2=mess3=mess4=blank;
		
		leftjust(ifile,8);
		leftjust(ilib,8);
		leftjust(ivol,6);

		if (more_input && ' ' == ifile[0])
		{
			/*
			**	On a more_input a blank file name means no more files.
			*/
			return 0;
		}

		if (' '==ivol[0] || ' '==ilib[0] || ' '==ifile[0])
		{
			messid = "R001";
			mess1 = "\224SORRY\204- File specification is incomplete, please respecify.";
			
			if (' '==ivol[0])
			{
				x_volume = "R";
			}
			if (' '==ilib[0])
			{
				x_library = "R";
			}
			if (' '==ifile[0])
			{
				x_file = "R";
			}
			continue;
		}

		/*
		**	Check if voloume and library exists
		*/
		{
			int4	vacnt, start, count;
			char	recvr[22];
			char	buff[80];

#if defined(unix) || defined(MSFS)
			if (0==wlgtrans(ivol,buff))
			{
				messid = "ER28";
				mess1="\224SORRY\204- The input VOLUME specified was not found, please respecify.";
				x_volume = "R";
				continue;
			}
#endif
			start = 1;
			wswap(&start);
			count = 1;
			wswap(&count);

			vacnt = 6;
			wvaset(&vacnt);
			FIND("        ", ilib, ivol, &start, &count, recvr);

			wswap(&count);
			if (count == 0)
			{
				messid = "R014";
#if defined(unix) || defined(MSDOS) || defined(WIN32)
				mess1 = "\224SORRY\204- The input LIBRARY was not found, please respecify.";
				x_library = "R";
#else
				mess1 = "\224SORRY\204- The input LIBRARY or VOLUME was not found, please respecify.";
				x_library = "R";
				x_volume = "R";
#endif
				continue;
			}
		}

		if (!wfexists(ifile,ilib,ivol)) 
		{
			messid = "R014";
			mess1 = "\224SORRY\204- The input FILE was not found, please respecify.";
			x_file = "R";
			continue;
		}

		/*
		**	Translate the INPUT file name.
		*/
		{
			int4	mode;
			char	*ptr;

			mode = 0;
			ptr = wfname(&mode, ivol, ilib, ifile, infilename);
			*ptr = (char)0;
		}

		if (strncmp(shared_field,"YES",3) && strncmp(shared_field,"NO ",3))
		{
			x_shared="R";
			messid="ER20";
			mess1 = "\224SORRY\204- SHARED option must be YES or NO, please respecify.";
			continue;
		}
		else if (!strncmp(shared_field,"YES",3))
		{
			shared_flag=1;
#ifdef OLD
			x_shared="R";
			messid="ER20";
			mess1 = "\224SORRY\204- SHARED option not supported, please respecify.";
			continue;
#endif
		}
		else
		{
			shared_flag=0;
		}

		if (!more_input)
		{
			if (strncmp(select_field,"YES",3) && strncmp(select_field,"NO ",3))
			{
				x_select="R";
				messid="ER20";
				mess1 = "\224SORRY\204- SELECT option must be YES or NO, please respecify.";
				continue;
			}
			else if (!strncmp(select_field,"YES",3))
			{
#ifdef VMS
				*select_flag=TRUE;
#else
				x_select="R";
				messid="ER20";
				mess1 = "\224SORRY\204- SELECT option not supported, please respecify.";
				continue;
#endif
			}
			else
			{
				*select_flag=FALSE;
			}

			if (strncmp(morefile_field,"YES",3) && strncmp(morefile_field,"NO ",3))
			{
				x_morefile ="R";
				messid="ER19";
				mess1 = "\224SORRY\204- MOREFILE option must be YES or NO, please respecify.";
				continue;
			}
			else if (!strncmp(morefile_field,"YES",3))
			{
#ifdef VMS
				*morefile_flag = TRUE;
#else
				x_morefile ="R";
				messid="ER19";
				mess1 = "\224SORRY\204- MOREFILE option not supported, please respecify.";
				continue;
#endif
			}
			else 
			{
				*morefile_flag = FALSE;
			}
		}

#if defined(unix) || defined(MSDOS) || defined(WIN32)

		/*
		**	Handle the FILETYPE and RECSIZE fields which are only used
		**	for unix and DOS.
		*/
		if ( 'C' == filetype_field[0] || 'A' == filetype_field[0] )
		{
			/*
			**	Older version supported 
			**		'C' - cisam
			**		'A' - acucobol
			**	replace with 'I'
			*/
			filetype_field[0] = 'I';
		}

		if (	filetype_field[0] != 'I' && 
			filetype_field[0] != 'F' && 
			filetype_field[0] != 'N'   )
		{
			x_filetype="R";
			messid="ER99";
			mess1 = "\224SORRY\204- FILETYPE must be \"I\" \"F\" or \"N\", please respecify.";
			continue;
		}
		else
		{
			int4	fdr_mode, fdr_retcode, fdr_argcnt;
			char	fdr_filetype;

			fdr_mode = 0;
			wswap(&fdr_mode);
			fdr_retcode = 0;
			fdr_argcnt = 7;
			wvaset(&fdr_argcnt);			
			READFDR(ifile,ilib,ivol,&fdr_mode,"FT",&fdr_filetype,&fdr_retcode);

			wswap(&fdr_retcode);
			if (0 != fdr_retcode)
			{
				messid = "R014";
				mess1 = "\224SORRY\204- Unable to access input file, please respecify.";
				x_file = "R";
				continue;
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
		*filetype=filetype_field[0];

		if ('F' == *filetype)
		{
			if (field2int4(recsize_field,5,recsize))
			{
				messid="ER98";
				mess1="\224SORRY\204- RECSIZE field is invalid, please respecify.";
				x_recsize="R";
				continue;
			}

			if (*recsize < 1)
			{
				x_recsize="R";
				messid="ER99";
				mess1 = "\224SORRY\204- RECSIZE must be specified for FILETYPE=F, please respecify.";
				continue;
			}
		}
		else if ('N' == *filetype)
		{
			*recsize = 0;
		}
#endif

		/*
		**	Call READFDR to get the record lenght.
		**
		**	NOTE: On VMS filetype is always set to 'I' so this code will be used.
		*/
		if ('I' == *filetype)
		{
			int4	fdr_mode, fdr_retcode, fdr_argcnt;
			int4	fdr_recsize;

			fdr_mode = 0;
			wswap(&fdr_mode);
			fdr_retcode = 0;
			fdr_recsize = 0;
			wswap(&fdr_recsize);
			fdr_argcnt = 7;
			wvaset(&fdr_argcnt);			
			READFDR(ifile,ilib,ivol,&fdr_mode,"RS",&fdr_recsize,&fdr_retcode);

			wswap(&fdr_retcode);
			if (0 != fdr_retcode)
			{
				messid = "R014";
				mess1 = "\224SORRY\204- Unable to access input file, please respecify.";
				x_file = "R";
				continue;
			}

			wswap(&fdr_recsize);
			*recsize = fdr_recsize;
		}

#ifdef VMS
		if (shared_flag)
		{
			int	rc;
			int	fh;
			int	openflags;

			rc = get_lock();

			if (16 == rc) return 16;
			if ( 1 == rc)
			{
				openflags = O_RDWR;
				fh = open(infilename,openflags,0,"shr=get");
				if ( -1 == fh )
				{
					messid = "LOCK";
					mess1 = "\224SORRY\204- Unable to lock input file, please respecify.";
					x_file = "R";
					continue;
				}
				record_lock(fh);
			}
		}
#endif

		return 0;
	}
}

/*
**	Routine:	get_options()
**
**	Function:	Issue the OPTIONS getparm and validate.
**
**	Description:	Put up the OPTIONS getparm for SORT and validate all fields.
**			Currently only the STABLE sort option is supported.
**			Validate all fields and report if unsupported options are requested.
**
**	Arguments:
**	stable_flag     true or false for stable sort  (returned)
**	lowspace_flag	Flag to check for low space before sort. (returned)
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	11/15/93        written JEC
**	08/24/94	Added all options plus error reporting. GSL
**
*/
static int get_options(int *stable_flag, int *lowspace_flag)
{
	int4	pfkey_mask;
	char	*gptype;
	char	message[80];
	char	*messid,*mess1,*mess2,*mess3,*mess4,*blank;
	char	*x_function, *x_memory, *x_addrout, *x_keyout, *x_stable, *x_reformat, *x_altseq, *x_lowspace;

	char	function_field[5];
	char	memory_field[4];
	char	addrout_field[3];
	char	keyout_field[3];
	char    stable_field[3];
	char	reformat_field[3];
	char	altseq_field[6];
	char	lowspace_field[3];

	pfkey_mask = PFKEY_16_ENABLED | terminate_key_enabled;
	wswap(&pfkey_mask);
	gptype = "I ";
	blank=" ";
	mess1=mess2=blank;
	
	sprintf(message,"You are now running %s",SORT_VERSION);
	mess3=message;
	mess4="Please specify the program options below:";
	messid = "0000";

	memcpy(function_field,	"SORT ",  5);
	memcpy(memory_field,	"0128",   4);
	memcpy(addrout_field,	"NO ",	  3);
	memcpy(keyout_field,	"NO ",	  3);
	memcpy(stable_field,	"NO ",	  3);
	memcpy(reformat_field,	"NO ",	  3);
	memcpy(altseq_field,	"NONE  ", 6);
	memcpy(lowspace_field,	"NO ",    3);
	
	x_function = x_memory = x_addrout = x_keyout = x_stable = x_reformat = x_altseq = x_lowspace = "K";

	for(;;)
	{
		int	row;
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "OPTIONS "; GP pfkey_recv; GP messid; GP "WSORT "; GPNUM(4); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));
			GP mess3; GPNUM(strlen(mess3));
			GP mess4; GPNUM(strlen(mess4));

		row = 12;
		GPKW(x_function,"FUNCTION",function_field,5,row,13,"A");
		GPCTEXT("(SORT)",row,34);

		row += 1;
		GPKW(x_memory,"MEMORY  ",memory_field,4,row,13,"A");
		GPCTEXT("(NOT USED)",row,34);
		
		row += 2;
		GPCTEXT("Do you want an ADDROUT output file ?",row,2);
		GPKW(x_addrout,"ADDROUT ",addrout_field,3,row,46,"A");
		GPCTEXT("(NO)",row,65);
		
		row += 1;
		GPCTEXT("Do you want a KEYOUT output file ?",row,2);
		GPKW(x_keyout,"KEYOUT  ",keyout_field,3,row,46,"A");
		GPCTEXT("(NO)",row,65);

		row += 1;
		GPCTEXT("Do you require a STABLE sort ?",row,2);
		GPKW(x_stable,"STABLE  ",stable_field,3,row,46,"A");
		GPCTEXT("(YES or NO)",row,65);
		
		row += 1;
		GPCTEXT("Do you want to REFORMAT the records ?",row,2);
		GPKW(x_reformat,"REFORMAT",reformat_field,3,row,46,"A");
		GPCTEXT("(NO)",row,65);
		
		row += 1;
		GPCTEXT("Type of ALTERNATE collating sequence ?",row,2);
		GPKW(x_altseq,"ALTSEQ  ",altseq_field,6,row,46,"A");
		GPCTEXT("(NONE)",row,65);
#ifdef VMS
		row += 1;
		GPCTEXT("Check for low disk space before sort ?",row,2);
		GPKW(x_lowspace,"LOWSPACE",lowspace_field,3,row,46,"A");
		GPCTEXT("(YES or NO)",row,65);
#endif

		GPCTEXT(terminate_text,23,26);

		GPENTER();
		GPPFS(&pfkey_mask);

		wvaset(&two);
		GETPARM(&gp_args,&gp_cnt);


		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}
	
		x_function = x_memory = x_addrout = x_keyout = x_stable = x_reformat = x_altseq = x_lowspace = "K";
		gptype = "R ";

		if (strncmp(function_field,"SORT ",5))
		{
			messid = "ER12";
			mess1 = "\224SORRY\204- Only function SORT is supported.";
			x_function="R";
			continue;
		}

		if (strncmp(addrout_field,"NO ",3))
		{
			messid = "ER15";
			mess1 = "\224SORRY\204- ADDROUT is not supported.";
			x_addrout="R";
			continue;
		}

		if (strncmp(keyout_field,"NO ",3))
		{
			messid = "ER16";
			mess1 = "\224SORRY\204- KEYOUT is not supported.";
			x_keyout="R";
			continue;
		}

		if (!strncmp(stable_field,"YES",3))
		{
			*stable_flag=TRUE;
		}
		else if (!strncmp(stable_field,"NO ",3))
		{
			*stable_flag=FALSE;
		}
		else
		{
			messid = "ER17";
			mess1 = "\224SORRY\204- STABLE option must be YES or NO.";
			x_stable="R";
			continue;
		}

		if (strncmp(reformat_field,"NO ",3))
		{
			messid = "ER18";
			mess1 = "\224SORRY\204- REFORMAT is not supported";
			x_reformat="R";
			continue;
		}

		if (strncmp(altseq_field,"NONE  ",6))
		{
			messid = "ER19";
			mess1 = "\224SORRY\204- ALTSEQ is not supported";
			x_altseq="R";
			continue;
		}

		if (!strncmp(lowspace_field,"YES",3))
		{
			*lowspace_flag=TRUE;
		}
		else if (!strncmp(lowspace_field,"NO ",3))
		{
			*lowspace_flag=FALSE;
		}
		else
		{
			messid = "ER17";
			mess1 = "\224SORRY\204- LOWSPACE option must be YES or NO.";
			x_lowspace="R";
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
**	Routine:	get_output()
**
**	Function:	To get the output file specs for COPY.
**
**	Description:	Put up the OUTPUT getparm for COPY and validate all fields.
**
**	Arguments:
**	ofile		The output file 	(returned)	
**	olib		The output library 	(returned)
**	ovol		The output volume 	(returned)
**	outfilename	The native output filename.
**	allow_replace	Flag to put up the REPLACE option if only one input file specified.
**	replace_flag	Flag if doing a replace.
**	fileorg		The file organization (C, R, or I)
**	maxrec		The maximum record size.
**	outrectype	The record type (F ov V)
**	errorgp		Error flag.
**			0 = Initial getparm
**			1 = Error getparm
**	errmess1,2,3	Error messages
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	09/23/94	Re-Written by GSL
**
*/
static int get_output(char *ofile, char *olib, char *ovol, char *outfilename, 
			int allow_replace, int *replace_flag, char *fileorg, int4 *maxrec, 
			char *outrectype, int errorgp, char *errmess1, char *errmess2, char *errmess3)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*mess1,*mess2,*mess3,*mess4,*mess5,*blank=" ";
	char	*messid;
	char    *termmsg1,*termmsg2;
	int4	retcode;
	char	buff[80];
	char	*x_file,*x_library,*x_volume,*x_replace,*x_recsize,*x_fileorg,*x_rectype;
	char    replace_field[3],fileorg_field[1],recsize_field[5],rectype_field[1];
	char    tmpstr[100];
	int4	mode;
	char	*ptr;
	int	update_putparm;

	x_file = x_library = x_volume = x_replace = x_recsize = x_fileorg = x_rectype = "K";
	mess1 = mess2 = mess3 = mess4 = mess5 = blank;

#ifndef VMS
	/*
	**	RECSIZE is not support so set to 0 to check if it gets changed later.
	*/
	*maxrec = 0;
#endif

	sprintf(tmpstr,"%05ld",(long)*maxrec);
	memcpy(recsize_field,tmpstr,5);

	if (errorgp)
	{
		gptype = "R ";
		mess1=errmess1;
		mess2=errmess2;
		mess3=errmess3;
		messid = "E099";
		fileorg_field[0]= *fileorg;
		rectype_field[0]= *outrectype;
		termmsg1="Press (1)  to terminate without error status";
		if (pfkeys12())
		{
			termmsg2="Press (12) to terminate with error status";
		}
		else
		{
			termmsg2="Press (16) to terminate with error status";
		}
		
		pfkey_mask = PFKEY_1_ENABLED | PFKEY_3_ENABLED | PFKEY_16_ENABLED | terminate_key_enabled;
	}
	else
	{
		gptype = "I ";
		mess3="Please specify the output file below:";
		memset(ofile,' ',8);
		get_defs(DEFAULTS_OL,olib);
		get_defs(DEFAULTS_OV,ovol);
		messid = "0000";
		*replace_flag = 0;
		fileorg_field[0]='C';
		rectype_field[0]='F';
		termmsg1=NULL;
		termmsg2=terminate_text;
		pfkey_mask = PFKEY_3_ENABLED | PFKEY_16_ENABLED | terminate_key_enabled;
	}
	wswap(&pfkey_mask);

	if (*replace_flag)
	{
		memcpy(replace_field,"YES",3);
	}
	else
	{
		memcpy(replace_field,"NO ",3);
	}

	update_putparm = 0;
	
	for(;;)
	{
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "OUTPUT   "; GP pfkey_recv; GP messid; GP "WSORT "; GPNUM(5); 
			GP mess1; GP &N[strlen(mess1)];
			GP mess2; GP &N[strlen(mess2)];
			GP mess3; GP &N[strlen(mess3)];
			GP mess4; GP &N[strlen(mess4)];
			GP mess5; GP &N[strlen(mess5)];

		GPCTEXT("OUTPUT",13,2);
		GPKW(x_file,"FILE    ",ofile,8,13,9,"L");
		GPCTEXT("in",13,30);
		GPKW(x_library,"LIBRARY ",olib,8,13,33,"L");
		GPCTEXT("on",13,54);
		GPKW(x_volume,"VOLUME  ",ovol,6,13,57,"L");
		if (allow_replace)
		{
			GPCTEXT("Replace input file with same name?",15,2);
			GPKW(x_replace,"REPLACE ",replace_field,3,15,50,"A");
			GPCTEXT("(YES or NO)",15,68);
		}

#ifdef VMS
		GPCTEXT("Fileorg - Consecutive, Relative or Indexed?",16,2);
		GPKW(x_fileorg,"FILEORG ",fileorg_field,1,16,50,"A");
		GPCTEXT("(C, R or I)",16,68);
#else
		GPCTEXT("Fileorg - Consecutive?",16,2);
		GPKW(x_fileorg,"FILEORG ",fileorg_field,1,16,50,"A");
		GPCTEXT("(C)",16,68);
#endif

		GPCTEXT("(Maximum) Record Size:",19,2);
		GPKW(x_recsize,"RECSIZE ",recsize_field,5,19,50,"N");

#ifdef VMS
		GPCTEXT("Fixed or Variable length records?",20,2);
		GPKW(x_rectype,"RECTYPE ",rectype_field,1,20,50,"A");
		GPCTEXT("(F or V)",20,68);
#else
		GPCTEXT("Fixed length records?",20,2);
		GPKW(x_rectype,"RECTYPE ",rectype_field,1,20,50,"A");
		GPCTEXT("(F)",20,68);
#endif

		if (termmsg1)
		{
 			GPCTEXT(termmsg1,22,18);
		}
		GPCTEXT(termmsg2,23,18);
		
		GPENTER();
		GPPFS(&pfkey_mask);

		wvaset(&two);
		GETPARM(&gp_args,&gp_cnt);

		if (update_putparm) return 0;
		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}
		if (pfkey_recv[0] == PFKEY_1_PRESSED) return 1;

		gptype = "R ";
		x_file = x_library = x_volume = x_replace = x_recsize = x_fileorg = x_rectype = "K";
		mess1 = mess2 = mess3 = mess4 = mess5 = blank;

		leftjust(ofile,8);
		leftjust(olib,8);
		leftjust(ovol,6);

		if (' '==ovol[0] || ' '==olib[0] || ' '==ofile[0])
		{
			messid = "R001";
			mess1 = "\224SORRY\204- File specification is incomplete, please respecify.";
			
			if (' '==ovol[0])
			{
				x_volume = "R";
			}
			if (' '==olib[0])
			{
				x_library = "R";
			}
			if (' '==ofile[0])
			{
				x_file = "R";
			}
			continue;
		}

#if defined(unix) || defined(MSFS)
		/*
		**	Check if volume exists
		*/
		if (0==wlgtrans(ovol,buff))
		{
			messid = "R042";
			mess1="\224SORRY\204- The output VOLUME specified was not found, please respecify.";
			x_volume = "R";
			continue;
		}
#endif

		if (allow_replace)
		{
			if (strncmp(replace_field,"YES",3) && strncmp(replace_field,"NO ",3))
			{
				mess1="\224SORRY\204- REPLACE option must be YES or NO, please respecify.";
				messid="0030";
				x_replace="R";
				continue;
			}
			else if (strncmp(replace_field,"YES",3)==0)
			{
				*replace_flag = 1;
			}
			else
			{
				*replace_flag = 0;
			}
		}
		
#ifdef VMS
		if (!strchr("CRI",fileorg_field[0]))
		{
			messid="ER60";
			mess1="\224SORRY\204- FILEORG must be C, R or I, please respecify.";
			x_fileorg="R";
			continue;
		}
#else
		if ('C' != fileorg_field[0])
		{
			messid="ER60";
			mess1="\224SORRY\204- Only FILEORG=C is supported, please respecify.";
			x_fileorg="R";
			continue;
		}
#endif
		*fileorg = fileorg_field[0];
#ifdef VMS
		if (rectype_field[0]!='F' && rectype_field[0]!='V')
		{
			messid="ER78";
			mess1="\224SORRY\204- The RECTYPE must be F or V, please respecify.";
			x_rectype="R";
			continue;
		}
#else
		if ('F' != rectype_field[0])
		{
			messid="ER78";
			mess1="\224SORRY\204- Only RECTYPE=F is supported, please respecify.";
			x_rectype="R";
			continue;
		}
#endif
		*outrectype = rectype_field[0];

		if (field2int4(recsize_field,5,maxrec))
		{
			messid="ER98";
			mess1="\224SORRY\204- RECSIZE field is invalid, please respecify.";
			x_recsize="R";
			continue;
		}

#ifndef VMS
		if (0 != *maxrec)
		{
			/*
			**	If RECSIZE has changed then give error message - not supported.
			*/
			messid="ER98";
			mess1="\224ERROR\204- RECSIZE is not supported.";
			x_recsize="R";
			continue;
		}
#endif

		/*
		**	If the output file exists then prompt to scratch.
		**	(If overwriting then don't scratch because we need the file to do the sort.)
		*/
		if (!*replace_flag && wfexists(ofile,olib,ovol))
		{
			/*
			**	If PF3 was pressed (second time thru)
			**	then scratch the file.
			*/
			if (pfkey_recv[0] == PFKEY_3_PRESSED)
			{
				int4	vacnt;

				vacnt = 5;
				wvaset(&vacnt);
				SCRATCH("F",ofile,olib,ovol,&retcode);	/* Scratch the target file			*/
				wswap(&retcode);
				if (retcode != 0)
				{
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
		
		/*
		**	Generate the native filename and update the PUTPARM.
		*/
		mode = IS_OUTPUT | IS_BACKFILL;
		ptr = wfname(&mode,ovol,olib,ofile,outfilename);
		*ptr = (char)0;
		makepath(outfilename);
#ifdef VMS
		wdellock(&mode,outfilename);
#endif

		use_last_prb();
		gptype = "RD";
		update_putparm = 1;
	}
}

/*
**	Routine:	get_select()
**
**	Function:	Issue the SELECT getparm(s)
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	selinfo		Select criteria list
**	selcnt		Select count
**
**	Globals:	None
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
static int get_select(struct select_criteria selinfo[], int *selcnt)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*messid,*mess1,*mess2,*mess3,*mess4,*mess5,*mess6,*blank=" ";
	char    prname[9];
	char    errmsg[80];
	char	tmpval[19];
	char	buff[80];
	int 	stvalidx,endvalidx;
	int 	chkidx;
	struct selstrs
	{
		char	fldpos_field[5];
		char	length_field[4];
		char	fldtyp_field[1];
		char	tstrel_field[2];
		char	value_field[18];
		char	conect_field[3];
	} 	s[4];
	char	*x_fldpos[4], *x_length[4], *x_fldtyp[4], *x_tstrel[4], *x_value[4], *x_conect[4];
	char 	kw_fldpos[4][9], kw_lenght[4][9], kw_fldtyp[4][9], kw_tstrel[4][9], kw_value[4][9], kw_conect[4][9];
	int	select_idx, base_idx;
	int	next_select;
	char	pfkey_recv[1];
	int4	tint4;

	*selcnt = 0;

	x_fldpos[0] = x_length[0] = x_fldtyp[0] = x_tstrel[0] = x_value[0] = x_conect[0] =
	x_fldpos[1] = x_length[1] = x_fldtyp[1] = x_tstrel[1] = x_value[1] = x_conect[1] =		
	x_fldpos[2] = x_length[2] = x_fldtyp[2] = x_tstrel[2] = x_value[2] = x_conect[2] = 
	x_fldpos[3] = x_length[3] = x_fldtyp[3] = x_tstrel[3] = x_value[3] = x_conect[3] = "K";

	pfkey_mask = PFKEY_16_ENABLED | terminate_key_enabled;
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
		messid = "ERR ";

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

			if (select_idx>0)
			{
				sprintf(buff,"SELECT%d  ",select_idx+1);
			}
			else
			{
				strcpy(buff,"SELECT  ");
			}
			memcpy(prname,buff,8);

			base_idx = select_idx * 4;

			for (kwidx=0; kwidx<4; ++kwidx)
			{
				sprintf(kw_fldpos[kwidx], "FLDPOS%-2d", kwidx + base_idx + 1);
				sprintf(kw_lenght[kwidx], "LENGTH%-2d", kwidx + base_idx + 1);
				sprintf(kw_fldtyp[kwidx], "FLDTYP%-2d", kwidx + base_idx + 1);
				sprintf(kw_tstrel[kwidx], "TSTREL%-2d", kwidx + base_idx + 1);
				sprintf(kw_value [kwidx],  "VALUE%-3d", kwidx + base_idx + 1);
				sprintf(kw_conect[kwidx], "CONECT%-2d", kwidx + base_idx + 1);
			}

			next_select = 0;
		}
		
		pfkey_recv[0] = '@';
		
		gp_cnt = 0;
		GP gptype; GP "R"; GP prname; GP pfkey_recv; GP messid; GP "WSORT "; GPNUM(6); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));
			GP mess3; GPNUM(strlen(mess3));
			GP mess4; GPNUM(strlen(mess4));
			GP mess5; GPNUM(strlen(mess5));
			GP mess6; GPNUM(strlen(mess6));

		GPKW(x_fldpos[0],kw_fldpos[0],s[0].fldpos_field,4,14,7,"N");
		GPKW(x_fldpos[1],kw_fldpos[1],s[1].fldpos_field,4,17,7,"N");
		GPKW(x_fldpos[2],kw_fldpos[2],s[2].fldpos_field,4,20,7,"N");
		GPKW(x_fldpos[3],kw_fldpos[3],s[3].fldpos_field,4,23,7,"N");
		
		GPKW(x_length[0],kw_lenght[0],s[0].length_field,3,14,26,"N");
		GPKW(x_length[1],kw_lenght[1],s[1].length_field,3,17,26,"N");
		GPKW(x_length[2],kw_lenght[2],s[2].length_field,3,20,26,"N");
		GPKW(x_length[3],kw_lenght[3],s[3].length_field,3,23,26,"N");
		
		GPKW(x_fldtyp[0], kw_fldtyp[0],s[0].fldtyp_field,1,14,45,"A");
		GPKW(x_fldtyp[1], kw_fldtyp[1],s[1].fldtyp_field,1,17,45,"A");
		GPKW(x_fldtyp[2], kw_fldtyp[2],s[2].fldtyp_field,1,20,45,"A");
		GPKW(x_fldtyp[3], kw_fldtyp[3],s[3].fldtyp_field,1,23,45,"A");
		
		GPKW(x_tstrel[0], kw_tstrel[0],s[0].tstrel_field,2,15,7,"A");
		GPKW(x_tstrel[1], kw_tstrel[1],s[1].tstrel_field,2,18,7,"A");
		GPKW(x_tstrel[2], kw_tstrel[2],s[2].tstrel_field,2,21,7,"A");
		GPKW(x_tstrel[3], kw_tstrel[3],s[3].tstrel_field,2,24,7,"A");
		
		GPKW(x_value[0], kw_value[0],s[0].value_field,18,15,26,"C");
		GPKW(x_value[1], kw_value[1],s[1].value_field,18,18,26,"C");
		GPKW(x_value[2], kw_value[2],s[2].value_field,18,21,26,"C");
		GPKW(x_value[3], kw_value[3],s[3].value_field,18,24,26,"C");

		GPKW(x_conect[0], kw_conect[0],s[0].conect_field,3,15,62,"A");
		GPKW(x_conect[1], kw_conect[1],s[1].conect_field,3,18,62,"A");
		GPKW(x_conect[2], kw_conect[2],s[2].conect_field,3,21,62,"A");
		GPKW(x_conect[3], kw_conect[3],s[3].conect_field,3,24,62,"A");
		
		GPENTER();
		GPPFS(&pfkey_mask);

		wvaset(&two);
		GETPARM(&gp_args,&gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}

		x_fldpos[0] = x_length[0] = x_fldtyp[0] = x_tstrel[0] = x_value[0] = x_conect[0] =
		x_fldpos[1] = x_length[1] = x_fldtyp[1] = x_tstrel[1] = x_value[1] = x_conect[1] =		
		x_fldpos[2] = x_length[2] = x_fldtyp[2] = x_tstrel[2] = x_value[2] = x_conect[2] = 
		x_fldpos[3] = x_length[3] = x_fldtyp[3] = x_tstrel[3] = x_value[3] = x_conect[3] = "K";

		for (chkidx = 0; chkidx < 4; chkidx++)
		{
			/*
			**	FLDPOS	- FIELD POSITION
			*/

			if (0 != field2int4(s[chkidx].fldpos_field, 4, &selinfo[chkidx+base_idx].fpos))
			{
				sprintf(errmsg,"\224ERROR\204- FLDPOS%-2d MUST BE A POSITIVE INTEGER",chkidx+base_idx+1);
				mess1 = errmsg;
				x_fldpos[chkidx]="R";
				goto sel_respec;
			}

			/*
			**	LENGTH	- FIELD LENGTH
			*/

			if (0 != field2int4(s[chkidx].length_field, 3, &selinfo[chkidx+base_idx].flen))
			{
				sprintf(errmsg,"\224ERROR\204- LENGTH%-2d MUST BE A POSITIVE INTEGER",chkidx+base_idx+1);
				mess1 = errmsg;
				x_length[chkidx]="R";
				goto sel_respec;
			}

			/*
			**	FLDTYP	- FIELD TYPE
			*/

			if (!strchr("BCDLPZ",s[chkidx].fldtyp_field[0]))
			{
				sprintf(errmsg,"\224ERROR\204- FLDTYP%-2d MUST BE ONE OF B, C, D, L, P, OR Z.",chkidx+base_idx+1);
				mess1 = errmsg;
				x_fldtyp[chkidx]="R";
				goto sel_respec;
			}
			else
			{
				selinfo[chkidx+base_idx].ftyp = s[chkidx].fldtyp_field[0];
			}

			if ( 'B' == selinfo[chkidx+base_idx].ftyp &&
			      2 != selinfo[chkidx+base_idx].flen  &&
			      4 != selinfo[chkidx+base_idx].flen     )
			{
				sprintf(errmsg,"\224ERROR\204- LENGTH%-2d MUST BE 2 or 4 FOR BINARY",chkidx+base_idx+1);
				mess1 = errmsg;
				x_length[chkidx]="R";
				goto sel_respec;
			}

			/*
			**	TSTREL	- TEST RELATION
			*/

			if (matchrel(s[chkidx].tstrel_field)== -1)
			{
				sprintf(errmsg,"\224ERROR\204- TSTREL%-2d MUST BE ONE OF EQ, NE, GT, GE, LT, or LE.",
					chkidx+base_idx+1);
				mess1 = errmsg;
				x_tstrel[chkidx]="R";
				goto sel_respec;
			}
			else
			{
				memcpy(selinfo[chkidx+base_idx].testrel,s[chkidx].tstrel_field,2);
			}

			/*
			**	VALUE	- TEST VALUE
			*/

			memcpy(tmpval,s[chkidx].value_field,18);
			tmpval[18] = (char)0;
			for (stvalidx=0; stvalidx<18 && tmpval[stvalidx]==' '; ++stvalidx);
			if (18==stvalidx)
			{
				sprintf(errmsg,"\224ERROR\204- VALUE%-2d must be specified",chkidx+base_idx+1);
				mess1 = errmsg;
				x_value[chkidx]="R";
				goto sel_respec;
			}
			for (endvalidx=17; endvalidx>=stvalidx && tmpval[endvalidx]==' '; --endvalidx);

			if ((tmpval[stvalidx]=='"'  && tmpval[endvalidx]=='"') || 
			    (tmpval[stvalidx]=='\'' && tmpval[endvalidx]=='\'')  )
			{
				if (stvalidx == endvalidx)
				{
					sprintf(errmsg,"\224ERROR\204- VALUE%-2d missing ending quote",chkidx+base_idx+1);
					mess1 = errmsg;
					x_value[chkidx]="R";
					goto sel_respec;
				}

				if ('C'== selinfo[chkidx+base_idx].ftyp &&
				    endvalidx - stvalidx - 1 != selinfo[chkidx+base_idx].flen)
				{
					sprintf(errmsg,"\224ERROR\204- VALUE%-2d length is not equal LENGHT",chkidx+base_idx+1);
					mess1 = errmsg;
					x_value[chkidx]="R";
					goto sel_respec;
				}

#ifdef VMS
/*
**	NEEDS TO BE VALIDATED
*/
				if (tmpval[stvalidx]=='\'') 
				{
					tmpval[stvalidx]='"';
				}
				if (tmpval[endvalidx]=='\'')
				{
					tmpval[endvalidx]='"';
				}
#endif
				
			}
			else
			{
				if ( 0 != field2int4(tmpval, 18, &tint4) )
				{
					sprintf(errmsg,"\224ERROR\204- VALUE%-2d INVALID, MISSING QUOTES",chkidx+base_idx+1);
					mess1 = errmsg;
					x_value[chkidx]="R";
					goto sel_respec;
				}
#ifdef VMS
/*
**	NEEDS TO BE VALIDATED
*/
				{
					sprintf(errmsg,"\224ERROR\204- VALUE%-2d MUST BE CONTAINED IN QUOTES",chkidx+base_idx+1);
					mess1 = errmsg;
					x_value[chkidx]="R";
					goto sel_respec;
				}
#endif
			}
			memset(selinfo[chkidx+base_idx].value,' ',18);
			memcpy(selinfo[chkidx+base_idx].value,&tmpval[stvalidx],endvalidx-stvalidx+1);

			/*
			**	CONECT	- TEST CONNECTION
			*/

			if (strncmp(s[chkidx].conect_field,"   ",3)==0)
			{
				/*
				**	Found a blank connect field so were done.
				*/

				*selcnt += chkidx + 1;
				return 0;
			}

			if (strncmp(s[chkidx].conect_field,"AND",3) && strncmp(s[chkidx].conect_field,"OR ",3))
			{
				sprintf(errmsg,"\224ERROR\204- CONECT%-2d MUST BE \"AND\", \"OR\" OR BLANK.",chkidx+base_idx+1);
				mess1 = errmsg;
				x_conect[chkidx]="R";
				goto sel_respec;
			}
			else
			{
				if (chkidx+base_idx+1 == MAXSORTSELECTS)
				{
					mess1 = "\224ERROR\204- CONECT32 MUST BE BLANK.";
					x_conect[chkidx]="R";
					goto sel_respec;
				}
				memcpy(selinfo[chkidx+base_idx].connect,s[chkidx].conect_field,3);
			}
		}

		*selcnt += 4;
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
**	keyinfo		Key criteria list
**	keycnt		Key count
**
**	Globals:	None
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	09/22/94	Updated. GSL
**
*/
static int get_keys(struct key_info keyinfo[], int *keycnt)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*messid,*mess1,*mess2,*blank=" ";
	char    errmsg[80];
	int 	chkidx;
	char    *x_keys, keys_field[1];
	char	*x_post[MAXSORTKEYS], *x_length[MAXSORTKEYS], *x_type[MAXSORTKEYS], *x_order[MAXSORTKEYS];
	struct keystrs
	{
		char	post_field[4];
		char	length_field[3];
		char	type_field[1];
		char	order_field[1];
	} k[MAXSORTKEYS];

	memset(k,' ',sizeof(k));
	keys_field[0]='1';
	x_keys="K";
	for (chkidx=0; chkidx<MAXSORTKEYS; ++chkidx)
	{
		x_post[chkidx] = x_length[chkidx] = x_type[chkidx] = x_order[chkidx] = "K";
		k[chkidx].type_field[0]='C';
		k[chkidx].order_field[0]='A';
	}

	pfkey_mask = PFKEY_16_ENABLED | terminate_key_enabled;
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
		GP gptype; GP "R"; GP "KEYS    "; GP pfkey_recv; GP messid; GP "WSORT "; GPNUM(2); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));

		GPCTEXT("NUMBER OF",9,2);
		GPKW(x_keys,"KEYS    ",keys_field,1,9,12,"N");
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
		GPCTEXT("* Key Type:   B=Binary, U=Unsigned Binary, C=Character, D=Decimal, F=Floating",21,2);
		GPCTEXT("              Z=Zoned decimal, L=Zoned decimal sign leading, P=Packed. Y=YY",22,2);
		GPCTEXT("* Sort Order: A=Ascending, D=Descending",23,2);
                                                    
		GPKW(x_post[0],"POST1   ",k[0].post_field,4,12,11,"N");
		GPKW(x_post[1],"POST2   ",k[1].post_field,4,13,11,"N");
		GPKW(x_post[2],"POST3   ",k[2].post_field,4,14,11,"N");
		GPKW(x_post[3],"POST4   ",k[3].post_field,4,15,11,"N");
		GPKW(x_post[4],"POST5   ",k[4].post_field,4,16,11,"N");
		GPKW(x_post[5],"POST6   ",k[5].post_field,4,17,11,"N");
		GPKW(x_post[6],"POST7   ",k[6].post_field,4,18,11,"N");
		GPKW(x_post[7],"POST8   ",k[7].post_field,4,19,11,"N");
		
		GPKW(x_length[0],"LENGTH1 ",k[0].length_field,3,12,28,"N");
		GPKW(x_length[1],"LENGTH2 ",k[1].length_field,3,13,28,"N");
		GPKW(x_length[2],"LENGTH3 ",k[2].length_field,3,14,28,"N");
		GPKW(x_length[3],"LENGTH4 ",k[3].length_field,3,15,28,"N");
		GPKW(x_length[4],"LENGTH5 ",k[4].length_field,3,16,28,"N");
		GPKW(x_length[5],"LENGTH6 ",k[5].length_field,3,17,28,"N");
		GPKW(x_length[6],"LENGTH7 ",k[6].length_field,3,18,28,"N");
		GPKW(x_length[7],"LENGTH8 ",k[7].length_field,3,19,28,"N");
		
		GPKW(x_type[0],"TYPE1   ",k[0].type_field,1,12,45,"A");
		GPKW(x_type[1],"TYPE2   ",k[1].type_field,1,13,45,"A");
		GPKW(x_type[2],"TYPE3   ",k[2].type_field,1,14,45,"A");
		GPKW(x_type[3],"TYPE4   ",k[3].type_field,1,15,45,"A");
		GPKW(x_type[4],"TYPE5   ",k[4].type_field,1,16,45,"A");
		GPKW(x_type[5],"TYPE6   ",k[5].type_field,1,17,45,"A");
		GPKW(x_type[6],"TYPE7   ",k[6].type_field,1,18,45,"A");
		GPKW(x_type[7],"TYPE8   ",k[7].type_field,1,19,45,"A");
		
		GPKW(x_order[0],"ORDER1  ",k[0].order_field,1,12,61,"A");
		GPKW(x_order[1],"ORDER2  ",k[1].order_field,1,13,61,"A");
		GPKW(x_order[2],"ORDER3  ",k[2].order_field,1,14,61,"A");
		GPKW(x_order[3],"ORDER4  ",k[3].order_field,1,15,61,"A");
		GPKW(x_order[4],"ORDER5  ",k[4].order_field,1,16,61,"A");
		GPKW(x_order[5],"ORDER6  ",k[5].order_field,1,17,61,"A");
		GPKW(x_order[6],"ORDER7  ",k[6].order_field,1,18,61,"A");
		GPKW(x_order[7],"ORDER8  ",k[7].order_field,1,19,61,"A");
		
		GPENTER();
		GPPFS(&pfkey_mask);

		wvaset(&two);
		GETPARM(&gp_args,&gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}

		x_keys="K";
		for (chkidx=0; chkidx<MAXSORTKEYS; ++chkidx)
		{
			x_post[chkidx] = x_length[chkidx] = x_type[chkidx] = x_order[chkidx] = "K";
		}

		/*
		**	Validate KEYS field
		*/
		if (keys_field[0] <= '0' || keys_field[0] > '8')
		{
			messid="ER01";
			mess1="\224SORRY\204- INVALID NUMBER OF KEYS";
			x_keys="R";
			goto key_respec;
		}
		*keycnt = keys_field[0] - '0';
		
		for (chkidx=0; chkidx < *keycnt; ++chkidx)
		{
			/*
			**	Validate POSTx fields
			*/
			if (0 != field2int4(k[chkidx].post_field, 4, &keyinfo[chkidx].spos) || keyinfo[chkidx].spos < 1)
			{
				sprintf(errmsg,"\224SORRY\204- POST%d MUST BE A POSITIVE INTEGER",chkidx+1);
				mess1 = errmsg;
				x_post[chkidx]="R";
				goto key_respec;
			}
			
			/*
			**	Validate LENGTHx fields
			*/
			if (0 != field2int4(k[chkidx].length_field, 3, &keyinfo[chkidx].len) || keyinfo[chkidx].len < 1)
			{
				sprintf(errmsg,"\224SORRY\204- LENGTH%d MUST BE A POSITIVE INTEGER",chkidx+1);
				mess1 = errmsg;
				x_length[chkidx]="R";
				goto key_respec;
			}

			/*
			**	Validate TYPEx fields
			*/
#ifdef VMS
			if (!strchr("CPZ",k[chkidx].type_field[0]))
			{
				sprintf(errmsg,"\224SORRY\204- TYPE%d Only C, P, or Z are supported.",chkidx+1);
				mess1 = errmsg;
				x_type[chkidx]="R";
				goto key_respec;
			}
#else
			if (!strchr("BCDFLPZN2TOUY",k[chkidx].type_field[0]))
			{
				sprintf(errmsg,"\224SORRY\204- TYPE%d MUST BE ONE OF B,C,D,F,L,P,Z,N,2,T,O,U,Y.",chkidx+1);
				mess1 = errmsg;
				x_type[chkidx]="R";
				goto key_respec;
			}
#endif
			keyinfo[chkidx].type = k[chkidx].type_field[0];

			/*
			**	Validate ORDERx fields
			*/
			if (k[chkidx].order_field[0] != 'A' && k[chkidx].order_field[0] != 'D')
			{
				sprintf(errmsg,"\224SORRY\204- ORDER%d MUST BE 'A' or 'D'.",chkidx+1);
				mess1 = errmsg;
				x_order[chkidx]="R";
				goto key_respec;
			}
			keyinfo[chkidx].order = k[chkidx].order_field[0];

		}
		return 0;
		
	      key_respec:
		
		gptype = "R ";
	}
}


static int matchrel(char *p)
{
	int idx;
	for (idx=0; legal_relations[idx]; ++idx)
	{
		if (!strncmp(p,legal_relations[idx],2))
		{
			return idx;
		}
	}
	return -1;
}

static int fh_list[MAXSORTINFILES];
static int fh_list_cnt = 0;

static void record_lock(int fh)
{
	fh_list[fh_list_cnt] = fh;
	fh_list_cnt++;
}
#ifdef VMS
static void free_locks(void)
{
	while(fh_list_cnt > 0)
	{
		fh_list_cnt--;
		close(fh_list[fh_list_cnt]);
	}
}
#endif /* VMS */

static int get_lock(void)
{
	int4	pfkey_mask;
	char	*gptype;
	char	*mess1;
	char	*messid, *blank=" ";
	char	*x_lock, *x_timeout, *x_bypass;
	char    lock_field[3],timeout_field[3],bypass_field[3];
	int	apply_lock;

	mess1 = blank;

	pfkey_mask = PFKEY_16_ENABLED | terminate_key_enabled;
	wswap(&pfkey_mask);
	gptype = "I ";
	messid = "0000";

	x_lock = x_timeout = x_bypass = "K";

	memcpy(lock_field,"YES",3);
	memcpy(timeout_field,"NO ",3);
	memcpy(bypass_field,"NO ",3);

	for(;;)
	{
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "LOCK    "; GP pfkey_recv; GP messid; GP "WSORT "; GPNUM(1); 
			GP mess1; GP &N[strlen(mess1)];

		GPCTEXT("Please enter the following options to process a file in shared mode.",9,2);

		GPCTEXT("Should a lock be placed on the file before sorting ?",11,2);
		GPCTEXT("  If YES, no changes to the file can be made during",12,2);
		GPCTEXT("  the sort,  If NO, changes to the file can be made.",13,2);

		GPKW(x_lock,"LOCK    ",lock_field,3,11,57,"A");
		GPCTEXT("(YES, NO)",11,72);

		GPCTEXT("If LOCK=YES, specify TIMEOUT and BYPASS:",15,2);

		GPCTEXT("How many seconds should the timeout be ?",17,2);
		GPKW(x_timeout,"TIMEOUT ",timeout_field,3,17,57,"A");
		GPCTEXT("(NO)",17,72);

		GPCTEXT("If the timeout expires, should the file be bypassed ?",21,2);
		GPKW(x_bypass,"BYPASS  ",bypass_field,3,21,57,"A");
		GPCTEXT("(NO)",21,72);

		GPCTEXT(terminate_text,24,26);

		GPENTER();
		GPPFS(&pfkey_mask);

		wvaset(&two);
		GETPARM(&gp_args,&gp_cnt);


		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}

		gptype = "R ";
		x_lock = x_timeout = x_bypass = "K";
		mess1 = blank;

		if (0 != memcmp(lock_field,"YES",3) && 0 != memcmp(lock_field,"NO ",3))
		{
			x_lock="R";
			messid="ER20";
			mess1 = "\224SORRY\204- LOCK option must be YES or NO, please respecify.";
			continue;
		}
		else if (0 == memcmp(lock_field,"YES",3))
		{
			apply_lock = 1;
		}
		else
		{
			apply_lock = 0;
		}

		if (0 != memcmp(timeout_field,"NO ",3))
		{
			x_timeout="R";
			messid="ER21";
			mess1 = "\224SORRY\204- TIMEOUT option not supported, please respecify.";
			continue;
		}

		if (0 != memcmp(bypass_field,"NO ",3))
		{
			x_bypass="R";
			messid="ER21";
			mess1 = "\224SORRY\204- BYPASS option not supported, please respecify.";
			continue;
		}

		if (apply_lock) return 1;

		return 0;
	}
}


/*
**	History:
**	$Log: wsort.c,v $
**	Revision 1.22  1998-09-09 16:20:28-04  gsl
**	Update version
**
**	Revision 1.21  1998-09-08 16:33:04-04  gsl
**	Add new key type Y=YY pic 99 DISPLAY based on YYPIVOTYEAR.
**
**	Revision 1.20  1997-09-24 17:38:01-04  gsl
**	Add support for pfkeys12()
**
**	Revision 1.19  1996-11-18 18:54:05-05  jockc
**	added call to vwang_title to set screen title
**
**	Revision 1.18  1996-08-29 17:15:55-07  gsl
**	Removed the link-level logic as it is now handled in initglbs()
**
**	Revision 1.17  1996-07-24 15:46:29-07  gsl
**	Fix for NT
**
**	Revision 1.16  1995-08-25 04:30:22-07  gsl
**	added call to wisp_signal_handler()
**
 * Revision 1.15  1995/06/23  15:42:48  gsl
 * changed so that on unix the SHARED= clause was ignored.
 * It used to only accept "NO " when since it does not lock it was acting
 * like SHARED=YES.
 * /
 *
 * Revision 1.14  1995/06/21  14:21:51  gsl
 * change all the PF 16's to (16) etc.
 *
 * Revision 1.13  1995/06/20  15:59:20  gsl
 * Changed so on an error it returns the RETCODE not 99.
 *
 * Revision 1.12  1995/06/14  16:33:05  gsl
 * change to use wisp_version()
 *
 * Revision 1.11  1995/05/31  11:54:12  gsl
 * Remove Duplicate ppunlink() which was causes labeled putparms 2 levels
 * up to be deleted. (TRY THAT AGAIN)
 * The linklevel was being decremented twice then an unlink was done.
 * this was causing labeled putparms one level up to be deleted
 * and unlabel putparms 2 level up to be deleted.
 *
 * Revision 1.10  1995/05/11  10:29:18  gsl
 * *** empty log message ***
 *
 * Revision 1.9  1995/05/11  10:25:15  gsl
 * added rcs revision statics
 *
 * Revision 1.8  1995/05/10  10:29:55  gsl
 * added headers and fix dor Watcom dos
 *
**
**	11/09/93        written using wcopy.c as a starting point by JEC
**	10/11/94	Re-written GSL
**
*/
