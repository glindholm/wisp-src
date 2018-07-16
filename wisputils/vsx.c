static char copyright[]="Copyright (c) 1991-1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		vsx.c
**
**	Purpose:	Loading VS BACKUP format tapes onto Unix
**
**	Routines:	
**          usage                print usage info
**          setord               compute byte order of this machine
**          savemode             save (s)tty mode
**          parseopts            parse options from command line
**          postprocess          convert .tmp's to uncompressed
**          setdebuginfo         parse the debug flag to get debug range
**          load                 load the tape(s)
**          geteot               load end of tape block 
**          prompt_new_tape      ask user to insert new tape
**          getbytes             load a certain number of bytes into workbuf
**          chk_valid_hdr        examine the file header
**          rawmode              set tty to raw mode
**          normalmode           set tty to cooked mode
**          lower                convert a string to lower case
**          pushname             push a filename onto a stack
**          skipbytes            skip over a certain number of bytes on tape
**          writefixed           write a fixed block
**          nulls2spaces         convert nulls to spaces in the buffer
**          matchopt             identify current option
**          digest               process a .tmp file
**          readtape             read some bytes from the tape
**
**
**	History:
**			History is at end of file
**
*/

static char idsi_copyright[] = "Copyright 1991,1992,1993,1994 International Digital Scientific, Inc. All Rights Reserved.";

static char VERSION[5];
static char MODDATE[20];

#ifdef AIX
/*
**	This turns on large file handling when compiled on AIX 4.2 and later. 
**	It allows for files larger then 2gig (up to 64gig).
*/
#define _LARGE_FILES
#endif

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>



#ifdef SOLARIS
#include <sys/ioctl.h>
#include <sys/mtio.h>
#endif

#ifdef AIX
#include <sys/ioctl.h>
#include <sys/devinfo.h>
#endif

#ifdef WIN32
#include <io.h>
#include <direct.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef unix
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/msg.h>
#include <termio.h>
#include <values.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <unistd.h>
#endif /* unix */

#include <signal.h>
#include <fcntl.h>
#include <errno.h>

#ifdef WIN32
#include "win32std.h"
#endif


#ifdef WIN32
#define DDS	'\\'
#else
#define DDS	'/'
#endif

#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
#define FALSE 0
#define TRUE 1

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifdef SCO					/* This must be define in the .umf file on SCO machines with CFLAGS = -DSCO	*/
#define TAPEDEV "//dev/rct0"
#endif
#ifdef ULTRIX
#define TAPEDEV "//dev/rmt0h"
#endif
#ifdef DGUX
#define TAPEDEV "//dev/rmt/0"
#endif
#ifdef NCR486
#define TAPEDEV "//dev/rmt/c0s0"
#endif
#ifdef WIN32
#define TAPEDEV "TAPE"
#endif
#ifndef TAPEDEV
#define TAPEDEV "//dev/rmt0"
#endif

static int skip_lib=0;

#define TAPE_BUFSZ 5120*20
static unsigned char tapebuf[TAPE_BUFSZ];
static unsigned char workbuf[TAPE_BUFSZ];

static char dest_dir[100];
static char arch_file[100];
static char norwd_file[100]={0};

#define RT_UNKNOWN	0
#define RT_COMPRESSED 	1
#define RT_FIXED 	2
#define RT_VARIABLE 	3

static char *rectypename[]=
{
	"Unknown",
	"C",
	"F",
	"V"
};

static enum 
{ 
	ORG_UNKNOWN	= 0, 
	ORG_CONSEC	= 1,
	ORG_INDEX  	= 2,
	ORG_PRINT  	= 3,
	ORG_PROG   	= 4,
	ORG_WORD   	= 5,
	ORG_REL		= 6
} org_type;

static char *orgtypename[]=
{
	"???",
	"CONSEC",
	"INDEXD",
	"PRINT",
	"PROG",
	"WORD",
	"REL"
};
static char *orgtypeext[]=
{
	".unknown",
	".seq",
	".index",
	".print",
	".prog",
	".word",
	".rel"
};

static int dont_unload_file = 0;
static int unload_all = 0;
static int needs_postprocessing = 0;

static unsigned short seq_num;

static int spec_rec_type = RT_COMPRESSED;

static int use_vol_name = 0;


#define FT_SRC 		0
#define FT_DAT 		1
#define FT_PRT 		2
#define FT_REL 		3
#define FT_ASK_L 	4
#define FT_ASK_F 	5

static char *filetypename[]=
{
	"Source",
	"Data",
	"Print",
	"Rel",
	"",
	""
};

static int file_type     = FT_DAT;
static int act_file_type = FT_DAT;
static int ask_file_type = FT_DAT;

#define EXT_ASK_L 1
#define EXT_ASK_F 2
#define EXT_ADD 3
#define EXT_NONE 4

#define DEF_SRC_EXT ".wcb"
#define DEF_NORM_EXT ".seq"

static int src_ext_flag=EXT_NONE;
static char src_ext[16];
static char norm_ext[16];

#define CASE_LOWER 1
#define CASE_UPPER 2

static int lname_case= CASE_LOWER;
static int fname_case= CASE_LOWER;

#define ACT_EXTRACT 1
#define ACT_INDEX 2

static int action=ACT_EXTRACT;
static int interactive=FALSE;

struct optstruct
{
	char *opt;
	int type;
	char *optdest;
	int optval;
};


static char debug_level_str[20],debug_range[20];
static int debug_level=0, debug_start=0, debug_end=0;

#define OPT_INT 1
#define OPT_STRING 2
#define OPT_BAD -1

static int eightmil=0, eightmilx=0;

#define ADDLF 1
#define NOLF 2
#define GUESSLF 3
static int add_lf=GUESSLF;
static int check_nulls=0;
static int trim_source=0;
static int process_tmps=0;

static int new_vol = TRUE;
static char expected_vol[6] = { 0 };
static short expected_seq=1;
static int eoarchive=FALSE;
static int eotape=FALSE;
static int file_continued=FALSE;
#ifdef SOLARIS
static int tape3480=FALSE;
#endif
static int bVerbose = FALSE;
static int bNoAcl = FALSE;

static char tape_rq_str[200] = {0};
static int tape_blocksz=TAPE_BUFSZ;

static char **arg_list;

static struct optstruct optlist[]=
{
	{ "-d", OPT_STRING, dest_dir, 0 },
	{ "-lnu", OPT_INT, (char *)&lname_case, CASE_UPPER },
	{ "-fnu", OPT_INT, (char *)&fname_case, CASE_UPPER },
	{ "-es", OPT_STRING, src_ext, 0 },
	{ "-en", OPT_STRING, norm_ext, 0 },
	{ "-El", OPT_INT, (char *)&src_ext_flag, EXT_ASK_L },
	{ "-Ef", OPT_INT, (char *)&src_ext_flag, EXT_ASK_F },
	{ "-ts", OPT_INT, (char *)&file_type, FT_SRC },
	{ "-td", OPT_INT, (char *)&file_type, FT_DAT },
	{ "-tl", OPT_INT, (char *)&file_type, FT_ASK_L },
	{ "-tf", OPT_INT, (char *)&file_type, FT_ASK_F },
	{ "-ua", OPT_INT, (char *)&unload_all, 1 },
	{ "-i", OPT_INT, (char *)&action, ACT_INDEX },
	{ "-q", OPT_INT, (char *)&interactive, TRUE },
	{ "-8", OPT_INT, (char *)&eightmil, TRUE },
	{ "-9", OPT_INT, (char *)&eightmilx, TRUE },
	{ "-V", OPT_INT, (char *)&bVerbose, TRUE },
	{ "-A", OPT_INT, (char *)&bNoAcl, TRUE },
	{ "-f", OPT_STRING, arch_file, 0 },
	{ "-xl", OPT_STRING, debug_level_str, 0 },
	{ "-xr", OPT_STRING, debug_range, 0 },
	{ "-lf", OPT_INT, (char*)&add_lf, ADDLF },
	{ "-nolf", OPT_INT, (char*)&add_lf, NOLF },
	{ "-null", OPT_INT, (char*)&check_nulls, TRUE },
	{ "-trim", OPT_INT, (char*)&trim_source, TRUE },
	{ "-tmp", OPT_INT, (char*)&process_tmps, TRUE },
	{ "-bs", OPT_STRING, tape_rq_str, 0 },
	{ "-nrw", OPT_STRING, norwd_file, 0 },
	{ "-v", OPT_INT, (char *)&use_vol_name, TRUE },
#ifdef SOLARIS
	{ "-3480", OPT_INT, (char*)&tape3480, TRUE },
#endif
	{ NULL, 0, NULL, 0 }
};


struct tapeheader {
	unsigned char encfhdr[4];		/* x00 Always "ENCF" */
	unsigned char vol1[6];			/* x04 */
	unsigned char userid1[3];		/* x0A */
	unsigned char filler1[4];		/* x0D */
	unsigned char sysversion[3];		/* x11 */
	unsigned char backupversion[3];		/* x14 */
	unsigned char budate[3];		/* x17 */
	unsigned char srcvol[6];		/* x1A */
	unsigned char srclib[8];		/* x22 */
	unsigned char seqnum[2];		/* x24 */
	unsigned char backsection[1];		/* x25 */
	unsigned char volseq[1];		/* x26 */
	unsigned char filler4a[4];		/* x2A */

	unsigned char fdr1format[1];		/* x2E offset 46 */
#define			FDR1FORMAT_INUSE	'1'
#define			FDR1FORMAT_NOTINUSE	'N'
	unsigned char fdr1xtntcount[1];	/* FDR1XTNTCOUNT count of extents in use */
	unsigned char fdr1org;		/* FDR1ORG 	FILE ORGANIZATION	*/
#define 		FDR1ORG_CONSEC		((unsigned char)0x01)
#define 		FDR1ORG_INDEXED		((unsigned char)0x02)
#define 		FDR1ORG_WP		((unsigned char)0x04)
#define 		FDR1ORG_RELATIVE	((unsigned char)0x08)
#define 		FDR1ORG_PROLOG		((unsigned char)0x10)
#define 		FDR1ORG_VARIABLE	((unsigned char)0x20)
#define 		FDR1ORG_PRINT		((unsigned char)0x40)
#define 		FDR1ORG_PROG		((unsigned char)0x80)
	unsigned char fdr1flags;		/* FDR1FLAGS 	FLAGS FOR STATUS 	*/
#define 		FDR1FLAGS_PRIV		((unsigned char)0x01)
#define 		FDR1FLAGS_ADMS		((unsigned char)0x02)
#define 		FDR1FLAGS_PARTIAL	((unsigned char)0x04)
#define 		FDR1FLAGS_LOG		((unsigned char)0x08)
#define 		FDR1FLAGS_ALTX		((unsigned char)0x10)
#define 		FDR1FLAGS_RECOV		((unsigned char)0x20)
#define 		FDR1FLAGS_COMPRESSED	((unsigned char)0x40)
#define 		FDR1FLAGS_UPDATE	((unsigned char)0x80)
	unsigned char fdr1x1ptr[2];
	unsigned char fdr1filename[8];	
	unsigned char fdr1filesection[1];
	unsigned char fdr1credate[3];
	unsigned char fdr1moddate[3];
	unsigned char fdr1expdate[3];
	unsigned char fdr1fpclass[1];
	unsigned char fdr1creator[3];
	unsigned char fdr1blksize[2];
	unsigned char fdr1secext[2];
	unsigned char fdr1x1strt[3];
	unsigned char fdr1x1end[3];
	unsigned char fdr1x2strt[3];
	unsigned char fdr1x2end[3];
	unsigned char fdr1x3strt[3];
	unsigned char fdr1x3end[3];

	unsigned char fdr1spare2[2];
	unsigned char fdr1nrecs[4];
	unsigned char fdr1recsize[2];


	unsigned char fdr1spare3[1];
	unsigned char fdr1eblk[3];
	unsigned char fdr1erec[2];
	unsigned char fdr1spare4[12];

	unsigned char fdr1chain[4];

	unsigned char filler8[5];

	unsigned char tapevol[6];
	unsigned char trailer[5];
};
#define BLANKSPACE 1908

struct tapetrailer 
{
	unsigned char beot[4];
	unsigned char tapevol[6];
	unsigned char userid[3];
	unsigned char filler1[1];
	unsigned char sysversion[3];
	unsigned char backupversion[3];
	unsigned char time[3];
	unsigned char date[3];
	unsigned char origvol[6];
	unsigned char nextvol[6];
	unsigned char fileseq[2];
	unsigned char flag[1];
	unsigned char filler2[5];

};

struct stack_node
{
	struct stack_node *prev;
	unsigned char *name;
};
struct stack_node *head=NULL, *stackp;

struct tapeheader *tapehdr;


#define ST_START 0
#define ST_NORMAL 1
#define ST_COMP 2
#define ST_NEXTCOMP 3
#define ST_END 4
#define ST_COMP0 5

#define ORDER_NORMAL 0
#define ORDER_REVERSE 1

static int byteorder;

/*
** these defines were setup long before idsistd and intdef were used.
** they don't really refer to 'C' longs and shorts, they refer to 
** long and short int size within the context of the VS backup.
*/
#define LONG 4
#define SHORT 2

static char oldlib[9];
static char oldvol[7];

#ifdef unix
static struct termio old,new;
#endif

#define D if(debug_level==1 && ((seq_num>=(unsigned short)debug_start) && (seq_num<=(unsigned short)debug_end)))
#define DD if(debug_level==2 && ((seq_num>=(unsigned short)debug_start) && (seq_num<=(unsigned short)debug_end)))
#define DDD if(debug_level==3 && ((seq_num>=(unsigned short)debug_start) && (seq_num<=(unsigned short)debug_end)))

#define RT_SKIPTOEND 1
#define RT_GETBLOCK 2
#define RT_GETBYTES 3
#define RT_SKIPBYTES 4

extern char *sys_errlist[];

/*--- END vsx.h ----*/


struct my_header
{
	unsigned int rectype;
	unsigned int filetype;
	unsigned int reccnt;
	unsigned int recsz;
};


static int tape_fd = -1;
static int tapebytes =0;

static int usage (void);
static void setord (void);
static int savemode (void);
static void parseopts (int c, char **v);
static int postprocess (void);
static int setdebuginfo (void);
static int load (void);
static int geteot (void);
static int prompt_new_tape (void);
static int getbytes (int cnt);
static int chk_valid_hdr (void);
static int rawmode (void);
static int normalmode (void);
static int lower (char *p);
static int pushname (char *str);
static int skipbytes (int cnt);
static int writefixed (unsigned char *buf, int recs, unsigned int size, FILE *f);
static int nulls2spaces (unsigned char *buf);
static void matchopt (char *opt, int *num, int *type, char **destval, int *val);
static int readtape(unsigned char *buf, int bytesrq, int command);
static int digest (char *name);
static void getbin(unsigned char *dest, unsigned char *src, int cnt);

#ifdef NOSTRDUP
static char *strdup(char *str)
{
        char *tmp;
        
        tmp = malloc(strlen(str)+1);
        strcpy(tmp,str);
        return tmp;
}
#endif

#ifdef SOLARIS
static void load3480(char *tapedev, int cartridge);
#endif

/*
**	Routine:	main()
**
**	Function:	main entry point for vsx
**
**	Input:		argc  - # of args 
**			argv  - arg values
**
**	Output:		loads a tape
**			
**
**	Return:		zero
**
**	Warnings:	None
**
**	History:	
**
*/

void main(int argc, char **argv)
{
        char *p,*e;
	void handler(int sig);
	
	/*
	** make a version and date stamp
	**
	*/
        memset(VERSION,0,sizeof(VERSION));
        p=strchr(rcsid,' ');
        p=strchr(++p,' ');
        e=strchr(++p,' ');
        
        memcpy(VERSION,p,e-p);
        memset(MODDATE,0,sizeof(MODDATE));
        p=strchr(++e,' ');
        memcpy(MODDATE,e,p-e);

        setbuf(stderr,NULL);
        
	/*
	** print usage info if necessary or else print version info and continue
	**
	**/
        if (argc>=2 && argv[1][0]=='-' && (argv[1][1]=='?'||argv[1][1]=='h')) 
	{
		usage();
	}
        else
	{
		printf("VSX Release V%s %s - 'vsx -h' for help\n",VERSION,MODDATE);
		printf("%s\n",copyright);
	}

	/*
	** get the byte order of this box
	*/ 
        setord();

	/*
	** save tty mode for restoring later
	**/
        savemode();
        
	/*
	** now look at what options we have
	**
	**/
        parseopts(argc,argv);

	/*
	** if we are in eightmilx mode set eightmil flag too.
	** (eightmilx is a debugging mode only.. works like eightmil except instead of
	** reading from tape we read a list of files from the disk)
	**
	**/
	if (eightmilx)
	  eightmil = TRUE;
	
	/*
	** check for -tmp flag.. if we have it we just do a postprocess (no tape load) phase
	**
	*/
	if (process_tmps)
	{
		arg_list = ++argv;
		postprocess();
		handler(0);
	}

	/*
	** check for debug info request 
	**
	**/
        setdebuginfo();
        if (debug_level) { printf("debug level %d: files %d thru %d\n",debug_level,debug_start,debug_end);}

	/*
	** print some info for user
	**
	**/
	if (strlen(norwd_file))
	{
		fprintf(stdout,"\nArchive file: %s\n",norwd_file);
	}
	else
	{
		fprintf(stdout,"\nArchive file: %s\n",arch_file);
	}
#ifdef SOLARIS
	if (tape3480)
	{
		fprintf(stderr,"Make sure 3480 tape unit is in AUTO mode.\n");
	}
#endif

	/*
	** setup int handler
	**
	**/
        signal(SIGINT,  handler );
        signal(SIGILL,  handler );
        signal(SIGSEGV, handler );
#ifdef unix
	signal(SIGEMT,  handler );
        signal(SIGBUS,  handler );
        signal(SIGQUIT, handler );
#endif
	/*
	** perform load phase
	**
	*/
        load();
        printf("end of archive reached..\f\npostprocessing:\n");
	fflush(stdout);
	fflush(stderr);

	/*
	** perform postprocessing phase
	**
	*/
        postprocess();
        handler( 0 );
        /*NOTREACHED*/
}
/*
**	Routine:	handler()
**
**	Function:	To shutdown gracefully
**
**	Description:	To print a shutdown message, close the tapefile and exit
**
**	Input:		ignored
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	at EOF
**
*/

void handler(int sig)
{
        fprintf(stderr,"\nVSX exiting.\n");
	if (tape_fd != -1)
		close(tape_fd);
        exit(0);
}
/*
**	Routine:	readtape()
**
**	Function:	To read bytes from tape
**
**	Description:	To read bytes from tape
**
**	Input:		buf       pointer to receiving buffer
**			bytesrq   number of bytes requested
**                      command   type of read to perform
**                                 RT_SKIPTOEND  - read until end of current tapefile (discard data)
**                                 RT_GETBLOCK   - read a blocksize amount of data
**                                 RT_GETBYTES   - read a certain (smaller) number of bytes
**                                 RT_SKIPBYTES  - skip a certain small number of bytes
** 
**	Output:		loads bytes into buf
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	at EOF
**
*/
static int readtape(unsigned char *buf, int bytesrq, int command)
{
        static int bufpos = 0;
        int stat, bytes;
        static int tapepos=0;
	static int fileseq= -1;
	static int eots=0;
#ifdef SOLARIS
	static int tapenum3480=0;
#endif

	/*
	** printing debug info if needed
	**
	*/	
	DD{
		fprintf(stderr,"readtape(buf,bytesrq=%d,mode=%s) tapebytes=%d, bufpos=%d/%d\n",
			bytesrq,
			command==RT_SKIPTOEND?"skip to end":
			command==RT_GETBLOCK?"get block":
			command==RT_GETBYTES?"get bytes":
			command==RT_SKIPBYTES?"skip bytes":"",
			tapebytes,bufpos,tape_blocksz);
	}
	DDD{
		fprintf(stderr,"readtape(buf,bytesrq=%d,mode=%s) tapebytes=%d, bufpos=%d/%d\n",
			bytesrq,
			command==RT_SKIPTOEND?"skip to end":
			command==RT_GETBLOCK?"get block":
			command==RT_GETBYTES?"get bytes":
			command==RT_SKIPBYTES?"skip bytes":"",
			tapebytes,bufpos,tape_blocksz);
	}
	/*
	** setup fileseq counter for debugging
	**
	*/
	if (fileseq == -1)
	{
		fileseq = debug_start;
	}
	/*
	** SKIPTOEND just read()s until EOF
	** 
	**
	*/
        if(command==RT_SKIPTOEND) 
        { 
                tapebytes=0;

                do 
                { 
                        stat=read(tape_fd,tapebuf,tape_blocksz); 
			DDD{
				printf("skipping %d %c%c%c%c\n",stat,tapebuf[0],tapebuf[1],tapebuf[2],tapebuf[3]); 
			}
                } while (stat); 

                return 0;     
        }
	/*
	** GETBLOCK returns a block of data, size unimportant.  it is called
	** when specific sizes of data are not important, we just want to
	** grab some data
	**
	*/
        if (command == RT_GETBLOCK)
        {
                int ret;
               
		/*
		** if local buffer has bytes in it, return them
		**
		*/
                if (tapebytes)
                {
                        memcpy(buf,tapebuf+bufpos,tapebytes);
                        ret=tapebytes;
                        tapebytes=0;
                }
		/*
		** otherwise read a blocksz chunk and hand it back
		**
		*/
                else
                {
                        tapebytes = read(tape_fd,tapebuf,tape_blocksz);
                        memcpy(buf,tapebuf,tapebytes);
                        ret=tapebytes;
			tapebytes=0;
                }
                return ret;
        }
	/*
	** at this point we either have a GETBYTES or a SKIPBYTES command.
	** the basic actions are the same so most of the code is shared
	**
	*/
	/*
	** first setup a loop, bytes is the number returned so far, bytesrq is the number
	** needed.
	**
	*/
        for (bytes=0; bytes < bytesrq; )
        {
		/*
		** open the tape if necessary
		**
		**/
                if (tape_fd == -1)
                {
		      openloop:
			
			/*
			** eightmilx lets a read a series of files named file1 file2 file3 etc.
			** the files must have been loaded from an 8mil or 9 track tape
			**
			*/
			if (eightmilx)
			{
				sprintf(arch_file,"file%d",fileseq++);
			}
#ifdef SOLARIS
			/*
			** now if it's a 3480 tape try to make it load the tape into the reader
			**
			*/
			if (tape3480)
			{
				load3480((strlen(norwd_file))?norwd_file:arch_file,tapenum3480);
			}
#endif
			/*
			** now open it and set eotape flag false
			**
			*/
                        tape_fd = open((strlen(norwd_file))?norwd_file:arch_file,O_RDONLY|O_BINARY);
			eotape=FALSE;
			/*
			** handle error condition
			**
			*/
                        if (tape_fd<0) 
                        {
			/*      openloop:    8/16/94-JEC-dunno why this label was here*/
				
				/*
				** ENXIO seems to indicate dev is offline.. this is of course
				** only returned by tape devices that have off and on line states
				**
				*/
				if (errno==ENXIO)
				{
					fprintf(stderr,"Place device %s ONLINE and press Return.\n",
						arch_file);
					getchar();
					goto openloop;
				}
				else
				{
					fprintf(stderr,"Error opening %s: %s (errno=%d)\n",
						arch_file,sys_errlist[errno],errno);
					handler(0);
				}
                        }
                }
		/*
		** if we have no bytes in our local buffer, read some
		**
		*/
                if (tapebytes == 0)
                {
#ifdef REV_1_64_1_1
		      loop:
#endif

			/* 
			** simple read on tape fd 
			*/
                        tapebytes = read(tape_fd,tapebuf,tape_blocksz);
			DDD{
				fprintf(stderr,"read %d bytes, bytes rqd is %d\n",tapebytes,bytesrq);
			}                       

				
			/*
			** handle special behaviors and return codes
			**
			*/
				
#ifdef ATT3B2
			/*
			** end of file reached, set tapebytes to zero 
			**
			*/
			if (tapebytes<0 && errno == ENXIO)
			{
				tapebytes=0;
			}
#endif
#ifdef DGUX
			/*
			** end of file reached, set tapebytes to zero 
			** AND close the tape fd (it must be closed and reopened to
			** get the next file)
			**
			*/
			if (tapebytes < 0 && errno== ENXIO)
			{
				tapebytes=0;
				close(tape_fd);
				tape_fd= -1;
			}
#endif
			/*
			** real error on the tape read
			**
			*/
                        if (tapebytes<0) 
                        {
                                fprintf(stderr,"Error reading %s: %s (errno=%d)\n",
					arch_file,sys_errlist[errno],errno);
                                exit(1);
                        }
			/*
			** read zero bytes. if it's not an 8mil type, expect end of tape.
			** this code is a little hairy; because we call geteot which in turn
			** calls getbytes.  not the best design, but, the geteot logic was
			** added after the fact to allow tape spanning; previously it was ignored.
			**
			*/
                        if (tapebytes == 0 && !eightmil)
                        {       
				/*
				** got end of tape block yet?
				**
				*/
				if (!eotape)
				{
					/*
					** read the eot block yet?
					** (eots is a kludge flag to prevent this block from being
					**  executed when the below call to geteot (which calls readtape)
					**  is called)
					**
					*/
					if (eots==0)
					{
						fprintf(stdout,"\nEnd of tape encountered.\n");
						eotape=TRUE;
						++eots;
						/*
						** might need to close and reopen the file
						**
						*/
						if (strlen(norwd_file))
						{
							close(tape_fd);
							tape_fd =  -1; 
						}
						/*
						** now read the eot
						*/
						geteot();
						eots=0;
#ifdef SOLARIS
						++tapenum3480;
#endif
					}
					else
					{
						return -1;
					}
				}
				else
				{
					/*
					** no EOT block found
					**
					*/
					fprintf(stderr,"\nMissing EOT block.  Check tape or try running\n");
					fprintf(stderr,"VSX again using the no-rewind device.\n");
					fprintf(stderr,"\n");
					return -1;
				}
				/*
				** now close and rewind
				**
				**/
				fprintf(stderr,"Rewinding tape\n");
				tapebytes=0;
				close(tape_fd);
				/*
				** tape we've been using is norewind, so we have to rewind it 
				** another way
				**
				*/
				if (strlen(norwd_file))
				{
					/*
					** just open and close the normal tape dev 
					**
					*/
					if (strlen(arch_file) && strcmp(arch_file, TAPEDEV))
					{
						int  rwd;
						rwd=open(arch_file,O_RDONLY|O_BINARY);
						close(rwd);
					}
					else
					{
						/*
						** otherwise, request that the  user specify the 
						** rewind tape too 
						*/
						fprintf(stderr,
							"Unable to rewind.  When using -nrw <tape>, specify the\n"
							"rewind on close device as -f <tape>.\n");
					}
				}
				tape_fd =  -1; 
                                return -1;
                        }
			/*
			** now handle 8mil case where we read only 48 bytes- it's the eotblock
			**
			*/
			if (eightmil && tapebytes == 48)
			{
				/*
				** set  eotape flag and call geteot
				**
				*/
				if (!eotape)
				{
					eotape=TRUE;
					fprintf(stderr,"\nEnd of tape encountered.\n");
					geteot();
				}
				else
				{
					/*
					** complain
					**
					*/
					fprintf(stderr,"\nMissing EOT block.  Verify tape and try again.\n");
					fprintf(stderr,"\n");
				}
				fprintf(stderr,"Rewinding tape\n");
				tapebytes=0;
				close(tape_fd);
#ifdef REV_1_64_1_1
				if (strlen(norwd_file))
				{
					if (strlen(arch_file) && strcmp(arch_file, TAPEDEV))
					{
						int  rwd;
						rwd=open(arch_file,O_RDONLY|O_BINARY);
						close(rwd);
					}
					else
					{
						fprintf(stderr,"Unable to rewind.  When using -nrw <tape>, specify the\n"
							"rewind on close device as -f <tape>.\n");
					}
				}
#endif /* REV_1_64_1_1 */
				tape_fd =  -1; 
				return -1;
			}
#ifdef REV_1_64_1_1
			if (eightmil && tapebytes == 0)
			{
				close(tape_fd);
				tape_fd = open((strlen(norwd_file))?norwd_file:arch_file,O_RDONLY|O_BINARY);
				goto loop;
			}
#endif /* REV_1_64_1_1 */
			/*
			** in 8milx mode, close file and continue
			**
			*/
			if (eightmilx && tapebytes==0)
			{
				close(tape_fd);
				tape_fd =  -1; 
				if (fileseq == debug_end)
				{
					eotape=TRUE;
					return -1;
				}
			}
			/*
			** reset bufpos, we just read a brand new buffer
			**
			*/
                        bufpos=0;
                }
		/*
		** if we have more bytes than he requested, return the number he wants
		** (minus what we may have already given him) and adjust our counters
		**
		*/
                if ((bytesrq-bytes) < tapebytes)
                {
                        if (command != RT_SKIPBYTES) 
			  memcpy(buf+bytes,tapebuf+bufpos,bytesrq-bytes);
                        bufpos += (bytesrq - bytes);     /* move bufpos. bufpos points to next avail data in our buffer */
                        tapebytes -= (bytesrq-bytes);    /* decrement byte count */
                        bytes=bytesrq;                   /* indicate user has been given bytesrq bytes */
                }
                else
                {
			/* 
			** otherwise we give him what we have and go thru the loop again 
			**
			*/
                        if (command != RT_SKIPBYTES) 
			  memcpy(buf+bytes,tapebuf+bufpos,tapebytes);
                        bytes += tapebytes;
                        tapebytes=0;
                }
        }
	/*
	** update the byte position on the tape and return it
	**
	*/
        tapepos += bytesrq;
        return tapepos;
}
/*
**	Routine:	geteot()
**
**	Function:	To load the END-OF-TAPE block
**
**	Description:	To call readtape and get eot block. Then set any 
**                      flags for next tape etc
**
**	Input:		None
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	at EOF
**
*/

static int geteot(void)
{
	struct tapetrailer x;
#ifdef EOTLOG
	FILE *eotlog;
#endif
	short seq;
	int st;
	
	/*
	** call readtape again to get additional bytes
	**
	*/
	if (!eightmil)
	{
		st = readtape((unsigned char*)&x,sizeof(x),RT_GETBYTES);
		if (st== -1)
		{
			fprintf(stderr,"Bad EOT block\n");
			eoarchive=TRUE;
			return 0;
		}
	}
	else
	{
		/*
		** in 8mil mode we already know we have 48 bytes; otherwise
		** this routine would not have been called
		**
		*/
		memcpy(&x,tapebuf,sizeof(struct tapetrailer));
	}
	/*
	** compute file sequence number
	**
	*/
	getbin((unsigned char*)&seq,x.fileseq,SHORT);
#if EOTLOG
	eotlog = fopen("eotlog","a");
	fprintf(eotlog,"BEOT     [%4.4s]\n",x.beot);
	fprintf(eotlog,"TAPEVOL  [%6.6s]\n",x.tapevol);
	fprintf(eotlog,"ORIGVOL  [%6.6s]\n",x.origvol);
	fprintf(eotlog,"NEXTVOL  [%6.6s]\n",x.nextvol);
	fprintf(eotlog,"SEQUENCE [%5.5d]\n",seq);
	fprintf(eotlog,"FLAG [%x/%c]\n",x.flag[0],x.flag[0]);
	fclose(eotlog);
#endif
	/*
	** this 0x80 byte indicates additional tapes
	**
	*/
	if (x.flag[0] == 0x80)
	{
		/*
		** copy name of next tape volume
		**
		*/
		memcpy(expected_vol,x.nextvol,6);
		++expected_seq;
		file_continued=1;
		/*
		** increment the expected tape seq and set a continuation flag
		**
		*/
		fprintf(stderr,"Backup continued on tape volume %6.6s\n",expected_vol);
	}
	else
	{
		/*
		** else no more tapes
		**
		*/
		eoarchive=TRUE;
	}

	return 0;
}
/*
**	Routine:	load()
**
**	Function:	To load an archive
**
**	Description:	A very complex routine.  It loads an archive, interpereting
**                      VS BACKUP blocking info
**
**	Input:		None
**			
**
**	Output:		files on the disk
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	at EOF
**
*/

static int load(void)
{
	/* recs    - index for reading a records in a non blocked file 
        ** fileeof - end of file flag
	** reccnt  - number of recs in current file
	** recsz   - number of bytes in current record
	** recbuf  - receiver for record data
        ** vol, lib, file - wang style path info from file header on tape
	** small   - bytecount in current "small" block
	** smallextra - extra unused bytes at end of current small block
	** med     - bytecount in current "medium" block
	** eofword - receiver of 4 byte signature at end of medium block
	** big     - bytecount in current "small" block
	** bigextra - extra unused bytes at end of current big block
	** oreccnt - total recs read so far (non blocked)
	** outpath - output file path
	** newlib  - new library (directory) is needed
	** out     - FILE * to output file
	** saverecs- used to initialize recs index (for spanned files)
	** dfpos   - used to track position on tape 
	**
	*/
        unsigned int recs,fileeof,reccnt;
        unsigned short recsz;
        unsigned char *recbuf;
        char vol[7],lib[9],file[9];
        unsigned short small,smallextra;
        unsigned int med,eofword;
        unsigned int big, bigextra;
        unsigned int oreccnt;
        char outpath[200];
        char *s;
        int newlib;
        FILE *out;
	static int saverecs;
        int dfpos;
	int bHasAcl = FALSE;
        
	/* 
	** setup the header pointer so we can examine its contents
	*/
        tapehdr=(struct tapeheader *)workbuf;

	/*
	** initialize counters and flags.
	** first block is special. big size is sizeof the tape header, extra bytes
	** at the end are ignored. file data starts at 0x800.
	**
	*/
        big=sizeof(struct tapeheader); 
	bigextra= 0x800-big;

        med=newlib=eoarchive=0;
        small=smallextra=skip_lib=0;
        eoarchive=FALSE;
        recbuf=NULL;
        
	/*
	** loop until end of archive reached
	**
	*/
        while (eoarchive==FALSE)
        {
		/*
		** is file continued from a prior tape?
		*/
		if (file_continued)
		{
#ifdef SOLARIS
			if (tape3480)
			{
				/* 
				** if its 3480, just inform user we will mount next
				**
				*/
				fprintf(stderr,"Mounting next backup tape (volume: %6.6s).\n", expected_vol);
			}
			else
			{
				/*
				** otherwise ask him to do it
				**
				*/
				prompt_new_tape();
			}
#else
			/*
			** non 3480-supporting systems always ask
			**
			*/
			prompt_new_tape();
#endif
		}
		/*
		** now grab the tape header
		**
		*/
                dfpos= getbytes(sizeof(struct tapeheader));
		/*
		** check for a valid header
		**
		*/
                if (dfpos== -1 || chk_valid_hdr()== -1)
		{
			continue;
		}
		/*
		** update big counter (big is the number of valid bytes left in the big block 
		**
		*/
                big -= sizeof(struct tapeheader);
		/*
		** now extract some fields: record size and count, file sequence number
		**
		*/
                getbin((unsigned char *)&recsz,   (unsigned char *)tapehdr->fdr1recsize,SHORT);
                getbin((unsigned char *)&reccnt,  (unsigned char *)tapehdr->fdr1nrecs,LONG);
                getbin((unsigned char *)&seq_num, (unsigned char *)tapehdr->seqnum,SHORT);

		/*
		** determine the file organization
		**
		*/

		dont_unload_file = 0;
		
		if (tapehdr->fdr1org & FDR1ORG_PROG)
		{
			org_type = ORG_PROG;
			dont_unload_file = 1;
			act_file_type = FT_DAT;
		}
		else if (tapehdr->fdr1org & FDR1ORG_PRINT)
		{
			org_type = ORG_PRINT;
			act_file_type = FT_PRT;
		}
		else if (tapehdr->fdr1org & FDR1ORG_WP)
		{
			org_type = ORG_WORD;
			act_file_type = FT_DAT;
		}
		else if (tapehdr->fdr1org & FDR1ORG_INDEXED)
		{
			org_type = ORG_INDEX;
			dont_unload_file = 1;
			act_file_type = FT_DAT;
		}
		else if (tapehdr->fdr1org & FDR1ORG_RELATIVE)
		{
			org_type = ORG_REL;
			act_file_type = FT_REL;
			dont_unload_file = 1;
		}
		else if (tapehdr->fdr1org & FDR1ORG_CONSEC)
		{
			org_type = ORG_CONSEC;

			/*
			**	These are assumptions that can be overriden later
			*/
			if (recsz==80)
			{
				act_file_type=FT_SRC;
			}
			else
			{
				act_file_type=FT_DAT;
			}
		}
		else
		{
			org_type = ORG_UNKNOWN;
			act_file_type = FT_DAT;
		}

		if (unload_all)
		{
			dont_unload_file = 0;
		}

		/*
		** now figure out what format the records are
		**
		*/
                if (tapehdr->fdr1flags & FDR1FLAGS_COMPRESSED) 
		{
			spec_rec_type = RT_COMPRESSED;
		}
		else if (tapehdr->fdr1org & FDR1ORG_VARIABLE)
		{
			spec_rec_type = RT_VARIABLE;
		}
		else
		{
			spec_rec_type = RT_FIXED;
		}
		
		/*
		**	Check for ACL block
		*/
		switch(tapehdr->fdr1filesection[0])
		{
		case '1':	/* This is the default expected value */
			bHasAcl = FALSE;
			break;
		case '5':	/* This file has an ACL */
			bHasAcl = TRUE;
			break;
		default:	/* Don't know what this is */
			bHasAcl = FALSE;
			fprintf(stderr, "\nUnrecognized fdr1filesection = \"%c\", expecting \"1\" or \"5\"\n",
				tapehdr->fdr1filesection[0]);
			break;
		}

		/*
		** reset the rec buffer (it is probably incorrect size)
		**
		*/
                if (recbuf) 
                {
                        free(recbuf);
                        recbuf=NULL;
                }
                
		/*
		** brand new file, so reset oreccnt
		**
		*/
                if (!file_continued) 
		{
			oreccnt=0;
		}
		
		/*
		** now get a record buf, then copy file lib and vol
		**
		*/
                recbuf=(unsigned char *)calloc(recsz+1,1);
                memcpy(vol,tapehdr->srcvol,6);
                memcpy(lib,tapehdr->srclib,8);
                memcpy(file,tapehdr->fdr1filename,8);
                vol[6]=lib[8]=file[8]=(char)0;

		/*
		 * Trim off trailing spaces
		 */
                s=strchr(vol,' ');
                if (s) *s=(char)0;
                s=strchr(lib,' ');
                if (s) *s=(char)0;
                s=strchr(file,' ');
                if (s) *s=(char)0;

		DD{
			printf("header=%s\n",tapehdr->encfhdr); 
			fflush(stdout);
		}
		D{
			printf("header=%4.4s backsection=%1.1d volseq=%2.2d\n",
			       tapehdr->encfhdr,tapehdr->backsection[1],tapehdr->volseq[1]); fflush(stdout);
		}

                if (use_vol_name && action==ACT_EXTRACT)
		{
                        if (0 != strcmp(vol,oldvol))
			{
				mkdir(vol,0777);
				chmod(vol,0777);
				strcpy(oldvol,vol);
			}
		}
		
		/*
		** if we are interactively querying to load the libs
		**
		*/
                if (interactive)
                {
		        /* 
			** if it's a new lib, ask if we should load it
			**
			*/
                        if (strcmp(lib,oldlib))
                        {
                                int foo,ok;
                          
				/*
				** this code is self documenting
				**
				*/
                                for (ok=FALSE; ok==FALSE; )
                                {
                                        printf("\nLibrary name: %8s    Restore this library? (y/n)-->",lib);
                                        rawmode();
                                        foo=getchar();
                                        normalmode();
                                        printf("\n");
                                        if (foo=='y'||foo=='Y') { skip_lib=FALSE; ok=TRUE; }                                    
                                        else if (foo=='n'||foo=='N') { skip_lib=TRUE; ok=TRUE; }
                                        else ok=FALSE;
                                        newlib=TRUE;
                                }
                        }
                }
	        /*
		** now we may need to ask what type the lib is also
		**
		*/
                if (file_type==FT_ASK_L && !skip_lib)
                {
                        if (strcmp(lib,oldlib))
                        {
                                int foo;

                                if (interactive)
                                {
					printf("                          Type source/normal (s/n)-->");
                                }
                                else
				{
					printf("\nLibrary name: %8s     Type source/normal (s/n)-->",lib);
				}
				
                                rawmode();
                                foo=getchar();
                                normalmode();
                                printf("\n\n");
				if (foo=='s'||foo=='S')
				{
					ask_file_type=FT_SRC;
				}
				else
				{
					ask_file_type=FT_DAT;
				}
				
                                if (foo=='x'||foo=='X') skip_lib=TRUE;
                                else skip_lib=FALSE;
                                newlib=TRUE;
                        }

			if (ORG_CONSEC == org_type)
			{
				act_file_type = ask_file_type;
			}
                }
	        /*
		** now record lib name, and truncate the spaces in lib and file
		** (volume is not really used)
		**
		*/
                if (newlib)
                {
                        strcpy(oldlib,lib);
                        newlib=FALSE;
                }
       

		/*
		** print a dot for skipped libs
		**
		*/
                if (skip_lib!=FALSE)
		{
			fprintf(stderr,".");
			fflush(stderr);
		}
		

                if (skip_lib==FALSE)
                {
			if (((seq_num-1) % 55) == 0)
			{
				printf("\f\n");
				printf("VOLUME  LIBRARY   FILE       FSEQ  FORG  RECFM  TYPE    LRECL  NRECS  Org,Flg\n");
				printf("------  --------  --------   ----  ----  -----  ------  -----  -----  -------\n");
			}

			printf("%-6s  %-8s  %-8s  %5d  %-6s  %s    %-6s   %4d  %5d  (%02X,%02X)",
			       vol, lib, file, seq_num, 
			       orgtypename[org_type], rectypename[spec_rec_type], filetypename[act_file_type], 
			       recsz, reccnt,
			       tapehdr->fdr1org, tapehdr->fdr1flags);

			if (action != ACT_EXTRACT)
			{
				printf("\n");
			}
		}
		
		/*
		** now the fun begins, time to read the file
		**
		*/

                if (action==ACT_EXTRACT && skip_lib==FALSE)
                {
                        char *use_ext;
                  
			/*
			** usually we want to lower case all the names
			*/
			if (lname_case==CASE_LOWER) 
			{
				lower(lib);
			}
                        if (fname_case==CASE_LOWER)
			{
				lower(file);
			}

			if (use_vol_name)
			{
				sprintf(outpath,"%s%c%s",vol, DDS, lib);
			}
			else
			{
				strcpy(outpath,lib);
			}
			
			mkdir(outpath,0777);
			chmod(outpath,0777);
			
			/*
			** if the recs aren't fixed, make a .tmp file instead
			**
			*/
			needs_postprocessing = 0;
			
			if (dont_unload_file)
			{
				use_ext = orgtypeext[org_type];
			}
                        else if (spec_rec_type == RT_COMPRESSED || spec_rec_type == RT_VARIABLE)
			{
				use_ext = ".tmp";
				needs_postprocessing = 1;
			}
                        else if (ORG_CONSEC == org_type)
			{
				/*
				** otherwise grab the needed extension
				**
				*/
				use_ext = (act_file_type==FT_SRC)?src_ext:norm_ext;
			}
			else
			{
				use_ext = orgtypeext[org_type];
			}

			/*
			** compose an output file name
			**
			*/
			if (use_vol_name)
			{
				sprintf(outpath,"%s%c%s%c%s%s",
					vol, DDS, lib, DDS, file, use_ext);
			}
			else
			{
				sprintf(outpath,"%s%c%s%s",
					lib, DDS, file, use_ext);
			}
			/*
			** push it onto our stack if it needs postprocessing later
			**
			*/
                        if (needs_postprocessing)
			{
				pushname(outpath);
			}
			/*
			** if its continued, print a message about it
			**
			*/
			if (file_continued)
			{
				printf("Continuing %s...\n",outpath);
			}
			/*
			** if it isn't index, print the name and info for the user to watch.
			** if it is, tell him we skipped it
			**
			*/
			if (dont_unload_file)
			{
				printf("   ==> *** SKIPPED ***\n");
			}
			else
			{
				printf("   ==> %s\n", outpath);
			}
			DD{
				printf("top %08x : big=%x/%d, bigextra=%x/%d, med=%x/%d, small=%x/%d, %x/%d\n",dfpos,
				       big,big,bigextra,bigextra,med,med,small,small,smallextra,smallextra);
			}
                }
                else if (action==ACT_INDEX && skip_lib==FALSE)
                {
			/*
			** user just wants a file list.. so print a slightly different message
			**
			*/

                }               
		/*
		** if action is extract (-i not specified) and 
		** not told to skip the lib (query lib option)
		** and file is not continued (meaning it's already open)
		** then open it
		*/
                if(action==ACT_EXTRACT && skip_lib==FALSE && !file_continued) 
		{
			out=fopen(outpath,"wb");
		}
                fileeof=0;

		/*
		** if file isn't fixed recs (it is therefor a .tmp)
		** and output file actually exists 
		** and action is extract (-i not specified) 
		** and file is not continued (meaning header was already written)
		** then write a header
		**
		*/
                if (spec_rec_type != RT_FIXED  && out!=NULL && action==ACT_EXTRACT
			&& !file_continued) /* stamp an info struct for post-processing */
                {
                        struct my_header mh;
                  
			/*
			** include record type, (max) size, and count
			** and file type 
			**
			*/
			mh.rectype  = spec_rec_type;	/* normalize_byteorder(tmpu4 = spec_rec_type); */
			mh.filetype = act_file_type;	/* normalize_byteorder(tmpu4 = act_file_type); */
			mh.recsz    = recsz;		/* normalize_byteorder(tmpu4 = recsz); */
			mh.reccnt   = reccnt;		/* normalize_byteorder(tmpu4 = reccnt); */

                        if (1 != fwrite(&mh,sizeof(struct my_header),1,out))
			{
				fprintf(stderr,"\nERROR write of temp file header failed: %s [errno=%d]\n",outpath,errno);
				perror("Writing temp file header");
				errno = 0;
			}
                }
		if (file_continued)
		{
			file_continued = FALSE;
			big=med=0;
			bigextra= 0x800-sizeof(struct tapeheader);
			med=0;
			small=smallextra=0;
			saverecs=recs;
		}
		else
		{
			saverecs = 0;
		}
		
		/*
		** now loop, reading and writing file data until EOF or EOT
		**
		*/
                while (!fileeof && !file_continued)
                {
                        DD{
                                printf("toploop %08x r%d: big=%x/%d, bigextra=%x/%d, med=%x/%d, small=%x/%d, %x/%d\n",dfpos,recs,
                                       big,big,bigextra,bigextra,med,med,small,small,smallextra,smallextra);
                        }
                        do
                        {
				/*
				** not an eightmil, so process blocking stuff
				**
				*/
                                if (!eightmil)
                                {
					/*
					** big is zero, time for a new big block
					**
					*/
                                        if (big==0)
                                        {
						/*
						** get a four byte value, this value is the number of bytes
						** in the big block
						**
						*/
                                                if (bigextra) skipbytes(bigextra);
                                                if (getbytes(LONG)<0) 
						{
							continue;
						}
                                                
						/*
						** extract the four bytes into our variable
						**
						*/
                                                getbin((unsigned char *)&big,workbuf,LONG);
						/*
						** compute the extra garbage bytes.  Max big block size is
						** 0x20000, so we subtract the big count from that and also
						** four more (the four bytes we just read above)
						**
						*/
                                                bigextra = 0x20000-big-4;
                                                DD{
                                                        printf("index: got big %x %x\n",big,bigextra);
                                                }
                                        }
					/*
					** same for medium blocks, if it's zero, read a new value
					**
					*/
                                        if (med==0)
                                        {
                                                getbytes(LONG);
                                                getbin((unsigned char *)&med,workbuf,LONG);
                                                big-=LONG;
                                                DD{
                                                        printf("index: got med %x\n",med);
                                                }
                                                
						/*
						** if the med byte count is 0xffff0090 it means the
						** file is done
						**
						*/
                                                eofword=med;
                                        }

                                        if (med == 0xffff0090) 
					{
						/* end of file marker */
						med = 0;
						break;
					}

					/*
					** in case you are wondering about this and other similarly
					** placed continues, this error is caught at a later point
					** not here, hence the continue
					**/
					if (getbytes(med)<0)
					{
						continue;
					}
					/*
					** decrement big counter since we just read med bytes
					**
					*/
					big -= med;     

#define ACL_BLOCK_SIZE	0x0422
					if (bHasAcl || ACL_BLOCK_SIZE == med)
					{

						/*	ACL = Access Control List
						**	Handle ACL block that follows header.
						**	If fdr1filesection == "5" then the file has an ACL
						**	The ACL block is x0422 (1058) bytes long and starts
						**	with the string "TACL".
						*/

						if (0==memcmp("TACL",workbuf,4))
						{
							/*
							**	Skip the ACL block
							*/
							med = 0;
							bHasAcl = FALSE;
							goto endloop;
						}
						if (bHasAcl)
						{
							fprintf(stderr, 
							   "\nError processing ACL header expecting \"TACL\" found \"%4.4s\"\n",
								workbuf);
						}
					}

					/*
					** stacked if follows:
					** first decide if we're writing the file, then take
					** actions based on if it's fixed len recs or otherwise
					**
					*/ 
					if(action==ACT_EXTRACT && 
					   skip_lib==FALSE && 
					   out!=NULL && 
					   !dont_unload_file)
					{
							
						if (spec_rec_type==RT_FIXED)
						{
                                                        int recs_this_block;
							unsigned int offset;
							/*
							** first compute the number of recs in this medium
							** block.  then loop and write each rec into the 
							** output file with no processing on it.  Some bytes
							** will be leftover at the end which are effectively 
							** skipped
							**/
							recs_this_block = 2048/recsz;
							for (offset=0;offset < med; offset+=2048)
							{
								if (oreccnt + recs_this_block > reccnt)
								{
									recs_this_block = reccnt - oreccnt;
								}
								if (recs_this_block)
								{
									writefixed(workbuf+offset,recs_this_block,recsz,out);
								}
								oreccnt += recs_this_block;
							}
						}
						else
						{
							/*
							** otherwise dump the entire medium block into the .tmp
							** file as is.. the small block processing was moved 
							** from here to the digest routine
							**/
                                                        if (fwrite(workbuf,1,med,out) != med)
							{
								fprintf(stderr,"\nERROR write to temp file failed: %s [errno=%d]\n"
									,outpath,errno);
								perror("Writing temp file");
								errno = 0;
							}
						}
					}

					/*
					** med block processed.  reset count
					**
					*/
                                        med=0;
                                }
                                else
                                {
					/* 
					** 8 mil tapes (USUALLY) have no big and medium blocking junk.
					** apparently sometimes they are written differently and
					** must be read without the -8
					*/
                                        int cnt;

					if (bHasAcl && !bNoAcl)
					{
						/*
						**	Read in the ACL block
						*/
						bHasAcl = FALSE;
						getbytes(ACL_BLOCK_SIZE);

						if (0==memcmp("TACL",workbuf,4))
						{
							/*
							**	found the ACL block
							*/
						}
						else
						{
							fprintf(stderr, 
							   "\nError processing ACL header expecting \"TACL\" found \"%4.4s\"\n",
								workbuf);
							fprintf(stderr, "Use the -A option to turn off ACL processing.\n");
						}
					}

                                        if (spec_rec_type==RT_FIXED)
                                        {
						int cur, leftover;
						
						/*
						** compute the leftover.  there are 0x800 blocks
						** into which the recs are inserted. There will be unused
						** bytes at the end of the 0x800 block
						**
						*/
						leftover = 0x800 % recsz;
						/*
						** now loop and read and write the records
						**
						*/
                                                for (cur=0, recs=saverecs; recs<reccnt; ++recs)
                                                {
							if (cur + leftover >= 0x800)
							{
								/*
								** skip the extra bytes
								**
								*/
								skipbytes(leftover);
								cur=0;
							}
                                                        if (getbytes(recsz)<0)
							{
								goto endloop;
							}
							/*
							** update cur counter
							**
							*/
							cur+=recsz;

							/*
							** now if appropriate we actually write the fixed record
							**
							*/
                                                        if(action==ACT_EXTRACT && 
                                                           skip_lib==FALSE && 
                                                           out!=NULL && 
							   !dont_unload_file)
							{
								/*
								** convert nulls to spaces if requested to
								**
								*/
								if (recsz==80 && check_nulls) nulls2spaces(workbuf);
								
								/*
								** write the record
								**
								*/
								if (fwrite(workbuf,1,recsz,out) != recsz)
								{
									fprintf(stderr,"\nERROR write failed: [errno=%d]\n",errno);
									perror("Writing out file");
									errno = 0;
								}
								
								if (add_lf==ADDLF)
								{
									fprintf(out,"\n");
								}
								else if (add_lf==GUESSLF && recsz==80)
								{
									fprintf(out,"\n");
								}
							}
                                                }
						/*
						** now skip to next tape mark for next file
						**
						*/
                                                readtape(0,0,RT_SKIPTOEND);
                                        }
                                        else /* rec type is not fixed */
                                        {
						/*
						** just loop reading arbitrary sized blocks and write them
						** to output .tmp file
						**
						*/
                                                while (0 != (cnt=readtape(workbuf,0,RT_GETBLOCK))) /* skip was 1? */
                                                {
							if (cnt== -1)
							{
								goto endloop;
							}
                                                        if(action==ACT_EXTRACT && 
                                                           skip_lib==FALSE && 
                                                           out!=NULL && 
							   !dont_unload_file)
							{
                                          			if (fwrite(workbuf,1,cnt,out) != (size_t)cnt)
								{
									fprintf(stderr,"\nERROR write failed: [errno=%d]\n",errno);
									perror("Writing out file");
									errno = 0;
								}
							}
                                                }

                                        }
					/*
					** force the eofword value
					*/
                                        eofword = 0xffff0090;
                                        
                                }

			      endloop: ;
				
                        } while (eofword != 0xffff0090 && !eoarchive && !eotape);

			/* ^^
			** keep looping until that med count is ffff0090 (or a tape mark  on an 8mil)
			** or end of archive or end of tape
			*/

                        ++fileeof;
                        continue;
                }
		/*
		** fclose the output file if we have one 
		**
		*/
                if (action==ACT_EXTRACT && skip_lib==FALSE && !file_continued) 
		{
			fclose(out);
		}
		
		if (dont_unload_file)
		{
			unlink(outpath);
		}
        }
	return 0;
}
/*
**	Routine:	trim()
**
**	Function:	To trim spaces and modcodes from end of COBOL rec
**
**	Input:		pointer to 80 char buffer
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	don't use it if your clients want their modcodes
**
**	History:	at EOF
**
*/

static int trim(unsigned char *buf)
{
        register unsigned char *p;
	
	if (check_nulls)
	  nulls2spaces(buf);

        buf[72]=(char)0;
        p=buf+71;
        while (*p==' ') --p;
        ++p;
        *p=(char)0;

	return 0;
}
/*
**	Routine:	nulls2spaces()
**
**	Function:	To replace nulls with spaces
**
**	Description:	To scan 80 bytes in a buffer and replace all nulls with spaces
**
**	Input:		buf   pointer to buffer
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	at EOF
**
*/

static int nulls2spaces(unsigned char *buf)
{
	int i;

	for (i=0; i<80; ++i) 
	{
		if (buf[i]==0x00) 
		{
			buf[i]=' ';
		}
	}

	return 0;
}

/*
** return a byte repeated
**
*/
static char *reptstr(int ch, int cnt)
{
        static char rept[128];
        
        memset(rept,ch,cnt);
        rept[cnt]=(char)0;
        return rept;
}

/*
**	Routine:	buildrec()
**
**	Function:	To uncompress a compressed record
**
**	Input:		src    - compressed record
**			dest   - receiving buffer
**                      size   - number of bytes in src
**
**	Output:		uncompressed record left in dest
**			
**
**	Return:		None
**
**	Warnings:	it's a state machine
**
**	History:	at EOF
**
*/

/* need to redo states START COMP0 COMP and NEXTCOMP */
static int bldrec(unsigned char *src, unsigned char *dest, unsigned int size)       
{
	/*
	** in_i  - input index
	** ch    - current character
	** next  - next character
	*/
        register unsigned int in_i,ch,cnt,next,out_i;
        register int state;


	/*
	** names of states for diagnostic output 
	**

        char *states[6];
        
        states[0]="start";
        states[1]="normal";
        states[2]="comp";
        states[3]="nextcomp";
        states[4]="end";
        states[5]="comp0";

	** uncomment and use if desired
	**
	*/

        state=ST_START;
        in_i=out_i=0;
        next=65535;
        
	/*
	** the state machine is mostly obvious in how it works.  the 
	** reason why I wrote the "next" logic is unclear.  It appears
	** to account for a certain anomoly sometimes seen in the compressed
	** records
	*/
        while (state != ST_END)
        {
                ch=src[in_i];

                switch (state)
                {
                      case ST_START:
                        if (ch>=0x80)
                        {
                                cnt=ch-0x80+1;
                                state=ST_COMP;
                        }
                        else 
                        {
                                next=ch+1;
                                state=ST_NORMAL;
                        }
                        ++in_i;
                        break;
                      case ST_NORMAL:
                        dest[out_i]=ch;
                        ++out_i;
                        ++in_i;
                        break;
                      case ST_COMP0:
                        if (ch>=0x80)
                        {
                                cnt=ch-0x80+1;
                                state=ST_COMP;
                        }
                        else 
                        {
                                next=ch+1;
                                state=ST_NORMAL;
                        }
                        ++in_i;
                        break;
                      case ST_COMP:
                        memcpy(&dest[out_i],reptstr(ch,cnt),cnt);
                        ++in_i;
                        out_i += cnt;
                        if (src[in_i]>=0x80) state=ST_COMP0;
                        else state=ST_NEXTCOMP;
                        break;
                      case ST_NEXTCOMP:
                        next=ch+1;
                        state=ST_NORMAL;
                        ++in_i;
                        break;
                }
                if (in_i>=size) state=ST_END;
                if (next==0) { state=ST_COMP0; next=65535; }
                else --next;
        }

	return 0;
}
/*
** call readtape to load data into workbuf
**
*/
static int getbytes(int cnt)
{
        int pos;
        
        if(cnt<=0)return 0;
        pos=readtape(workbuf,cnt,RT_GETBYTES);
        return pos;
}
/*
** call readtape to skip data
**
*/
static int skipbytes(int cnt)
{
        int tmp;
        
        if (cnt<=0)return 0;

        tmp=readtape(NULL,cnt,RT_SKIPBYTES);
        return tmp;
}
/*
** lowercase an entire string
**
*/
static int lower(char *p)
{
        while (*p)
        {
                *p = tolower(*p);
                ++p;
        }
	return 0;
}
/*
** parse the options
**
*/
static void parseopts(int c, char **v)
{
        int i,num,type,val;
        char *destval;


	strcpy(arch_file,TAPEDEV);
        strcpy(src_ext,DEF_SRC_EXT);
        strcpy(norm_ext,DEF_NORM_EXT);
        
        for (i=1;i<c;)
        {
                matchopt(v[i],&num,&type,&destval,&val);
		if (num == -1 && process_tmps) return;
                if (num == -1) fprintf(stderr,"bad option \"%s\"\n",v[i]);
                if (type==OPT_INT) *((int*)destval)=val;
                else if (type==OPT_STRING) 
                {
                        if (strlen(v[i])!=strlen(optlist[num].opt)) strcpy(destval,v[i]+strlen(optlist[num].opt));
                        else strcpy(destval,v[++i]);
                }
                ++i;
        }
	if (strlen(tape_rq_str))
	{
		tape_blocksz=atoi(tape_rq_str);
	}
#ifdef ATT3B2
	if (tape_blocksz>5120) 
	  tape_blocksz=5120;
#endif

}
/*
** match a single option
**
*/
static void matchopt(char *opt, int *num, int *type, char **destval, int *val)
{
        struct optstruct *p;
        int i;
        
        for(i=0,p=optlist;p->opt;++p,++i)
        {
                if (!strncmp(opt,p->opt,strlen(p->opt)))
                {
                        *num=i;
                        *type=p->type;
                        *destval=p->optdest;
                        *val=p->optval;
                        return;
                        
                }
                
        }
        *num= -1;
        *type = OPT_BAD;

}
/*
** get a binary value
**
*/
static void getbin(unsigned char *dest, unsigned char *src, int cnt)
{
        if (byteorder==ORDER_NORMAL)
	{
		memcpy(dest,src,cnt);
		return;
	}
        --cnt;
        while (cnt>=0)
        {
                *dest++ = *(src+cnt);
                --cnt;
        }
        return;
}
/*
** set byte order
**
*/
static void setord(void)
{
        int foo=0x12345678;
        if ((((char*)&foo)[0])==0x78) byteorder=ORDER_REVERSE;
        else byteorder=ORDER_NORMAL;
}

static int usage(void)
{
	fprintf(stderr,"VSX Release V%s %s\n",VERSION,MODDATE);
	fprintf(stderr,"%s\n",copyright);
        fprintf(stderr,"\nusage:\nvsx [-f <device or file>] [ -nrw <norewind file> ] [-e.ext] [-i] [-lnu] [-fnu]\n");
        fprintf(stderr,"   -f <archive> specify device or file containing archive (default: %s)\n",TAPEDEV);
        fprintf(stderr,"   -nrw <tape>  specify no-rewind device file\n");
        fprintf(stderr,"   -es.ext      append .ext to all source files created (normally .wcb)\n");
        fprintf(stderr,"   -en.ext      append .ext to all normal files created (normally .seq)\n");
        fprintf(stderr,"   -i           generate list of files in archive (do not extract)\n");
        fprintf(stderr,"   -lnu         generate uppercase directory names\n");
        fprintf(stderr,"   -fnu         generate uppercase file names\n");
        fprintf(stderr,"   -tl          query for filetype (source vs. nonsource) per library\n");
        fprintf(stderr,"   -q           query before restoring each library\n");
        fprintf(stderr,"   -8           archive is an 8 millimeter tape or 9-track reel tape\n");
        fprintf(stderr,"   -null        examine cobol source for embedded null values and fix\n");
        fprintf(stderr,"   -tmp <*.tmp> process .tmp files left over in case of vsx premature abort\n");
        fprintf(stderr,"   -lf          add linefeeds between records on all files\n");
        fprintf(stderr,"   -nolf        do not add linefeeds to any files\n");
        fprintf(stderr,"                (vsx normally adds linefeeds only to files with 80 byte records)\n");
        fprintf(stderr,"   -trim        trim spaces and modcodes from COBOL source\n");
        fprintf(stderr,"   -bs <size>   specify blocksize for tape reads\n");
        fprintf(stderr,"   -ua          unload all files including Indexed and Prog files\n");
        fprintf(stderr,"   -v           include volume name in output file path\n");
#ifdef SOLARIS
	fprintf(stderr,"   -3480        tape device is a Wang 3480\n");
#endif
        exit(1);
	return 0;
}

static int savemode(void)
{
#ifdef unix
        ioctl(0,TCGETA,&old);
#endif
	return 0;
}

static int rawmode(void)
{
#ifdef unix
        new=old;
        new.c_cc[VMIN]=1;
        new.c_cc[VTIME]=0;
        new.c_lflag &= ~ICANON;
        new.c_iflag = 0;
        ioctl(0,TCSETA,&new);
#endif
	return 0;
}

static int normalmode(void)
{
#ifdef unix
        ioctl(0,TCSETA,&old);
#endif
	return 0;
}

static int setdebuginfo(void)
{
        char *p;
        
        if (strlen(debug_level_str)) 
          debug_level=atoi(debug_level_str);

        if (strlen(debug_range))
        {
                p=strchr(debug_range,':');
                if (p==debug_range)
                {
                        debug_start=1;
                        debug_end=atoi(++p);
                        return 0;
                }
                else if (p)
                {
                        *p++ =(char)0;
                        debug_start=atoi(debug_range);
                        debug_end=atoi(p);
                        return 0;
                }
                else
                {
                        debug_start=atoi(debug_range);
                        debug_end=999999;
                        return 0;
                }
        }  
        else
        {
                debug_start=1;
                debug_end=999999;
                return 0;
        }               
}

static int pushname(char *str)
{
        struct stack_node *new_node;
        
        if (head==NULL)
        {
                new_node=stackp=head=(struct stack_node *)calloc(1,sizeof(struct stack_node));
                head->prev = NULL;
        }
        else
        {
                new_node=(struct stack_node *)calloc(1,sizeof(struct stack_node));
                new_node->prev = stackp;
        }
        new_node->name = (unsigned char*) strdup(str);
        stackp = new_node;

	return 0;
}

static char *getname(void)
{
        char *tmp;
        struct stack_node *save;

        if (!process_tmps)
	{
		if (stackp==NULL) return NULL;
		tmp = (char*)stackp->name;
		save = stackp;
		stackp = stackp->prev;
		free(save);
		return tmp;
	}
	else
	{
		if (*arg_list == NULL) return NULL;
		while(**arg_list=='-') ++arg_list;
		return *arg_list++;
	}
}

static int postprocess(void)
{
        char *inname,*getname(void);
        
        while (NULL != (inname=getname()))
        {
#ifdef unix
	        int dummy;
                if (fork()) 
		{
			wait(&dummy);
		}
                else
                {
                        digest(inname);
                        exit(0);
                }
#else
		digest(inname);
#endif
        }

	return 0;
}

void baddata(int sig)
{
        fprintf(stderr," file appears corrupted\n");
        exit(1);
}
/*
**	Routine:	digest()
**
**	Function:	To process a compressed record file
**
**	Input:		name   - name of file
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	at EOF
**
*/
static int digest(char *name)
{
        char *p;
	unsigned char *recbuf;
        struct my_header foo;
        FILE *in,*out;
        char outname[100];
        unsigned short small, smallextra, curlen;
        int recs, recsz,reccnt, the_size;
	int	rc;

        signal(SIGINT,  baddata );
        signal(SIGILL,  baddata );
        signal(SIGSEGV, baddata );
#ifdef unix
        signal(SIGEMT,  baddata );
        signal(SIGBUS,  baddata );
        signal(SIGQUIT, baddata );
#endif
	if (use_vol_name)
	{
		printf("%-29s",name); 
	}
	else
	{
		printf("%-22s",name); 
	}
	fflush(stderr);

	if ((in=fopen(name,"rb")) == NULL)
	{
                fprintf(stderr,"\nERROR cannot open temp file: %s [errno=%d]\n",name,errno);
		perror("Opening temp file for reading");
		errno = 0;
		return 1;
	}

        strcpy(outname,name);
        p=strchr(outname,'.');
        if (!p)
        {
                fprintf(stderr,"\nERROR bad temp filename: %s\n",outname);
                fclose(in);
		return 1;
        }

        rc = fread(&foo,sizeof(struct my_header),1,in);
	if (1 != rc)
	{
                fprintf(stderr,"\nERROR read of temp file header failed: %s [errno=%d]\n",name,errno);
		perror("Reading temp file header");
		errno = 0;
                fclose(in);
		return 1;
	}
	
        spec_rec_type 	= foo.rectype;		/* normalize_byteorder(foo.rectype); */
        act_file_type 	= foo.filetype;		/* normalize_byteorder(foo.filetype); */
        recsz 		= foo.recsz;		/* normalize_byteorder(foo.recsz); */
        reccnt 		= foo.reccnt;		/* normalize_byteorder(foo.reccnt); */

        recbuf=(unsigned char *)calloc(recsz+1,1);
        
        if (act_file_type==FT_SRC) 	strcpy(p,src_ext);
        else if (act_file_type==FT_PRT) strcpy(p,orgtypeext[ORG_PRINT]);
        else if (act_file_type==FT_REL) strcpy(p,orgtypeext[ORG_REL]);
        else 				strcpy(p,norm_ext);

	if (use_vol_name)
	{
		printf(" ==>  \n%-31s : ",outname); 
	}
	else
	{
		printf(" ==>  %-24s : ",outname); 
	}
	
	fflush(stdout);
        
        if ((out = fopen(outname,"wb"))==NULL)
        {
                fprintf(stderr,"\nERROR cannot open output: %s [errno=%d]\n",outname,errno);
		perror("Opening output file for writing");
		errno = 0;
		fclose(in);
	        free(recbuf);
		return 1;
        }

        recs=small=smallextra=0;
        while (recs<reccnt)
        {
                if (small==0)
                {
                        if (smallextra) 
			{
				the_size = smallextra;
				rc = fread(workbuf,1,the_size,in);
				if (rc != the_size)
				{
					fprintf(stderr, "\nERROR read failed: %s [errno=%d] (smallextra)\n",name,errno);
					if (feof(in))
					{
						fprintf(stderr, "Premature EOF\n");
					}
					else
					{
						perror("Reading temp file");
						errno = 0;
						fprintf(stderr, "recs=%d reccnt=%d small=%d smallextra=%d rc=%d the_size=%d\n",
							recs, reccnt,small,smallextra,rc,the_size);
					}
					fclose(in);
					fclose(out);
				        free(recbuf);
					return 1;
				}

			}
			the_size=2;
                        rc = fread(workbuf,1,the_size,in);
			if (rc != the_size)
			{
				fprintf(stderr, "\nERROR read failed: %s [errno=%d] (first bin)\n",name,errno);
				if (feof(in))
				{
					fprintf(stderr, "Premature EOF\n");
				}
				else
				{
					perror("Reading temp file");
					errno = 0;
					fprintf(stderr, "recs=%d reccnt=%d small=%d smallextra=%d rc=%d the_size=%d\n",
						recs, reccnt,small,smallextra,rc,the_size);
				}
				fclose(in);
				fclose(out);
			        free(recbuf);
				return 1;
			}
                        getbin((unsigned char *)&small,workbuf,2);
                        smallextra = 0x800 - small;
                        small -= 2;

                }
		the_size = 2;
                rc = fread(workbuf,1,the_size,in);
		if (rc != the_size)
		{
			fprintf(stderr, "\nERROR read failed: %s [errno=%d] (second bin)\n",name,errno);
			if (feof(in))
			{
				fprintf(stderr, "Premature EOF\n");
			}
			else
			{
				perror("Reading temp file");
				errno = 0;
				fprintf(stderr, "recs=%d reccnt=%d small=%d smallextra=%d rc=%d the_size=%d\n",
						recs, reccnt,small,smallextra,rc,the_size);
			}
			fclose(in);
			fclose(out);
		        free(recbuf);
			return 1;
		}
                getbin((unsigned char *)&curlen,workbuf,2);

		the_size = curlen-2;
                rc = fread(workbuf,1,the_size,in);
		if (rc != the_size)
		{
			fprintf(stderr, "\nERROR read failed: %s [errno=%d] (record)\n",name,errno);
			if (feof(in))
			{
				fprintf(stderr, "Premature EOF\n");
			}
			else
			{
				perror("Reading temp file");
				errno = 0;
				fprintf(stderr, "recs=%d reccnt=%d small=%d smallextra=%d rc=%d the_size=%d\n",
					recs, reccnt,small,smallextra,rc,the_size);
			}
			fclose(in);
			fclose(out);
		        free(recbuf);
			return 1;
		}
                small -= curlen;
                if (spec_rec_type == RT_COMPRESSED) 
		{
			bldrec(workbuf,recbuf,curlen-2);
		}
                else 
		{
			memcpy(recbuf,workbuf,recsz);
			recbuf[recsz]=(char)0;
		}
		
                ++recs;

		the_size = recsz;
		
		if (act_file_type == FT_SRC || act_file_type == FT_PRT)
		{
			recbuf[recsz]=(char)0;
		}
		
                if ((act_file_type == FT_SRC && trim_source!=FALSE) || (act_file_type == FT_PRT))
		{
			trim(recbuf);
			the_size = strlen((char*)recbuf);
		}

                rc = fwrite(recbuf, 1, the_size, out);
		if (rc != the_size)
		{
	                fprintf(stderr,"\nERROR write failed: %s [size=%d] [errno=%d] [rc=%d] [recs=%d]\n",
				outname,the_size,errno,rc,recs);
			perror("Writing output file");
			errno = 0;
			fclose(in);
			fclose(out);
		        free(recbuf);
			return 1;
		}

                if ((add_lf==ADDLF) ||
		    (add_lf==GUESSLF && act_file_type == FT_SRC && recsz==80) ||
		    (act_file_type == FT_PRT))
		{
                	fprintf(out,"\n");
		}

        }
        printf("%5d recs of %4d bytes\n",reccnt,recsz);
        
        fclose(in);
	fclose(out);
        free(recbuf);
        unlink(name);

	return 0;
}

static int writefixed(unsigned char *buf, int recs, unsigned int size, FILE *f)
{
        while (recs)
        {
		if (size==80 && check_nulls) nulls2spaces(buf);
		
                if (fwrite(buf,1,size,f) != size)
		{
			fprintf(stderr,"\nERROR write failed: [errno=%d]\n",errno);
		}
                buf += size;
                --recs;


                if ((add_lf==ADDLF) ||
		    (add_lf==GUESSLF && act_file_type == FT_SRC && size==80) ||
		    (act_file_type == FT_PRT))
		{
                	fprintf(f,"\n");
		}
        }

	return 0;
}

static const char *hexstring(const char *string, int len)
{
	static char hex[200];
	int	i;
	
	for(i=0; i<len; i++)
	{
		sprintf(&hex[i*2],"%02X", (int) string[i]);
	}
	hex[len*2] = '\0';
	return hex;
}

static void PrintVerboseVariable(const char *name, const unsigned char *var, int len)
{
	char	string[100];
	int	i;
	
	if (isprint(var[0]))
	{
		string[0] = '"';
		
		for(i=0; i<len; i++)
		{
			if (isprint(var[i]))
			{	
				string[i+1] = var[i];
			}
			else
			{
				string[i+1] = ' ';
			}
		}
		string[len+1] = '"';
		string[len+2] = '\0';
	}
	else
	{
		switch(len)
		{
		case 1:
			sprintf(string,"(%d)", (unsigned int) var[0]);
			break;
		case 2:
			sprintf(string,"(%d)", (unsigned int)(var[0] * 0x0100 + var[1]));
			break;
		case 3:
			sprintf(string,"(%d)", (unsigned int)(var[0] * 0x00010000 + var[1] * 0x00000100 + var[2]));
			break;
		case 4:
			sprintf(string,"(%d)", 
				(unsigned int)(var[0] * 0x01000000 + var[1] * 0x00010000 + var[2] * 0x0100 + var[3]));
			break;
		default:
			string[0] = '\0';
			break;
		}
	}
	
	
	printf("%-25s[%d] = x[%s] %s\n",
	       name, len, hexstring((const char *)var, len), string);
}


static int chk_valid_hdr(void)
{
	int vsxdump;

	if (bVerbose)
	{
		printf("\n\f");
		PrintVerboseVariable("tapehdr->encfhdr", tapehdr->encfhdr, 4);
		PrintVerboseVariable("tapehdr->vol1",    tapehdr->vol1, 6);
		PrintVerboseVariable("tapehdr->userid1",    tapehdr->userid1, 3);
		PrintVerboseVariable("tapehdr->filler1",    tapehdr->filler1, 4);
		PrintVerboseVariable("tapehdr->sysversion",    tapehdr->sysversion, 3);
		PrintVerboseVariable("tapehdr->backupversion",    tapehdr->backupversion, 3);
		PrintVerboseVariable("tapehdr->budate",    tapehdr->budate, 3);
		PrintVerboseVariable("tapehdr->srcvol",    tapehdr->srcvol, 6);
		PrintVerboseVariable("tapehdr->srclib",    tapehdr->srclib, 8);
		PrintVerboseVariable("tapehdr->seqnum",    tapehdr->seqnum, 2);
		PrintVerboseVariable("tapehdr->backsection",    tapehdr->backsection, 1);
		PrintVerboseVariable("tapehdr->volseq",    tapehdr->volseq, 1);
		PrintVerboseVariable("tapehdr->filler4a",    tapehdr->filler4a, 4);
		PrintVerboseVariable("tapehdr->fdr1format",    tapehdr->fdr1format, 1);
		PrintVerboseVariable("tapehdr->fdr1xtntcount",    tapehdr->fdr1xtntcount, 1);
		PrintVerboseVariable("tapehdr->fdr1org",    &tapehdr->fdr1org, 1);
		PrintVerboseVariable("tapehdr->fdr1flags",    &tapehdr->fdr1flags, 1);
		PrintVerboseVariable("tapehdr->fdr1x1ptr",    tapehdr->fdr1x1ptr, 2);
		PrintVerboseVariable("tapehdr->fdr1filename",    tapehdr->fdr1filename, 8);
		PrintVerboseVariable("tapehdr->fdr1filesection",    tapehdr->fdr1filesection, 1);
		PrintVerboseVariable("tapehdr->fdr1credate",    tapehdr->fdr1credate, 3);
		PrintVerboseVariable("tapehdr->fdr1moddate",    tapehdr->fdr1moddate, 3);
		PrintVerboseVariable("tapehdr->fdr1expdate",    tapehdr->fdr1expdate, 3);
		PrintVerboseVariable("tapehdr->fdr1fpclass",    tapehdr->fdr1fpclass, 1);
		PrintVerboseVariable("tapehdr->fdr1creator",    tapehdr->fdr1creator, 3);

		PrintVerboseVariable("tapehdr->fdr1blksize",    tapehdr->fdr1blksize, 2);
		PrintVerboseVariable("tapehdr->fdr1secext",    tapehdr->fdr1secext, 2);
		PrintVerboseVariable("tapehdr->fdr1x1strt",    tapehdr->fdr1x1strt, 3);
		PrintVerboseVariable("tapehdr->fdr1x1end",     tapehdr->fdr1x1end, 3);
		PrintVerboseVariable("tapehdr->fdr1x2strt",    tapehdr->fdr1x2strt, 3);
		PrintVerboseVariable("tapehdr->fdr1x2end",     tapehdr->fdr1x2end, 3);
		PrintVerboseVariable("tapehdr->fdr1x3strt",    tapehdr->fdr1x3strt, 3);
		PrintVerboseVariable("tapehdr->fdr1x3end",     tapehdr->fdr1x3end, 3);

		PrintVerboseVariable("tapehdr->fdr1spare2",    tapehdr->fdr1spare2, 2);
		PrintVerboseVariable("tapehdr->fdr1nrecs",    tapehdr->fdr1nrecs, 4);
		PrintVerboseVariable("tapehdr->fdr1recsize",    tapehdr->fdr1recsize, 2);
		PrintVerboseVariable("tapehdr->fdr1spare3",    tapehdr->fdr1spare3, 1);
		PrintVerboseVariable("tapehdr->fdr1eblk",    tapehdr->fdr1eblk, 3);
		PrintVerboseVariable("tapehdr->fdr1erec",    tapehdr->fdr1erec, 2);
		PrintVerboseVariable("tapehdr->fdr1spare4",    tapehdr->fdr1spare4, 12);
		PrintVerboseVariable("tapehdr->fdr1chain",    tapehdr->fdr1chain, 4);

		PrintVerboseVariable("tapehdr->filler8",    tapehdr->filler8, 5);
		PrintVerboseVariable("tapehdr->tapevol",    tapehdr->tapevol, 6);
		PrintVerboseVariable("tapehdr->trailer",    tapehdr->trailer, 5);
	}
	

	if (strncmp("ENCF",(char *)tapehdr->encfhdr,4))
	{
		close(tape_fd);
		tape_fd= -1;
		tapebytes=0;
		vsxdump=open(".vsxdump",O_WRONLY|O_CREAT|O_BINARY|0666);
		write(vsxdump,tapebuf,tapebytes);
		close(vsxdump);
		if (new_vol)
		{
			fprintf(stderr,"Volume header appears to be bad.\n");
			return -1;
		}
		else
		{
			fprintf(stderr,"Bad file header.\n");
			postprocess();
			handler(0);
		}
	}
	if (strlen(expected_vol)==0)
	  return 1;
	if (new_vol && strncmp(expected_vol,(char *)tapehdr->vol1,6))
	{
		fprintf(stderr,"Warning: Incorrect volume label. [%6.6s/%6.6s/%6.6s]\n",
			tapehdr->vol1,tapehdr->tapevol,expected_vol);
	}
	else new_vol=FALSE;
	return 1;
#ifdef NOT_USED
	char volseq;
	volseq = tapehdr->backsection[0];
	if (volseq != expected_seq)
	{
		close(tape_fd);
		tape_fd= -1;
		tapebytes=0;
		fprintf(stderr,"Tape appears to have incorrect sequence number (%d).  Expected %d.\n",
			volseq, expected_seq);
		return -1;
	}
	return 1;
#endif
}

static int prompt_new_tape(void)
{
	fprintf(stderr,"Insert next backup tape (volume: %6.6s).\n",
		expected_vol);
	fprintf(stderr,"Press return when ready\n");
	getchar();
	new_vol=TRUE;

	return 0;
}

#ifdef SOLARIS
static void load3480(char *tapedev, int cartridge)
{
	int fd;
	struct mtop mt_command;

	quickkludge:
	if ((fd = open(tapedev, O_RDONLY|O_BINARY)) < 0) {
		if (errno == EIO) {
#if 0
			fprintf(stderr, "%s: no tape loaded or drive offline\n", 
				tapedev);
#endif
			fprintf(stderr,"Please LOAD the first tape in the sequence and press Return.\n");
			getchar();
			goto quickkludge;
		}
		else {
		    perror("open() 3480 tape drive");
		}
	exit(1);
	}
	if (cartridge > 0)
	{
		mt_command.mt_op=MTOFFL;
		mt_command.mt_count = 1;

		if (ioctl(fd,MTIOCTOP,&mt_command)<0)
		{
			perror("vsx: while attempting to load next tape");
			exit(1);
		}
	}
	close(fd);
#if 0
	/*
	 * zero out the cdb
	 */ 
	 memset((char *) &cdb, 0, sizeof(cdb));
  
/*
 *  position or eject the loader
 */
	cdb.scc_cmd = 0x02;			/* byte 0 */
	cdb.scc_lun = 0;			/* byte 1 (bits 7-5) */
	cdb.t_code = eflag << 1;		/* byte 1 (bits 4-0) */
	cdb.low_count = cartridge;		/* byte 4 */
	
	/*
	 *  fill the uscsi structure
	 */
	memset((char *) &ucmd, 0, sizeof(ucmd));
	ucmd.uscsi_cdb = (char *) &cdb;
	ucmd.uscsi_cdblen = CDB_GROUP0;
	ucmd.uscsi_bufaddr = (caddr_t) NULL;
	ucmd.uscsi_buflen = 0;
	ucmd.uscsi_flags = USCSI_READ;
	
	if (ioctl(fd, USCSICMD, &ucmd) < 0) {
		perror("vsx: position tape failed");
		exit(errno);
	}


/*
 *  load the tape if we just positioned the loader and don't if we
 * just did an eject
 */
	if (!eflag) {
		/*
		 *	Load (tape)
		 */
		cdb.scc_cmd = 0x1b;			/* byte 0 */
		cdb.scc_lun = 0;			/* byte 1 (bits 7-5) */
		cdb.low_count = 1;			/* byte 4 */
	
		/*
		 *  fill the uscsi structure
		 */
		memset((char *) &ucmd, 0, sizeof(ucmd)); 
		ucmd.uscsi_cdb = (char *) &cdb;
		ucmd.uscsi_cdblen = CDB_GROUP0;
		ucmd.uscsi_bufaddr = (caddr_t) NULL;
		ucmd.uscsi_buflen = 0;
		ucmd.uscsi_flags = USCSI_READ;
	
		if (ioctl(fd, USCSICMD, &ucmd) < 0) {
			perror("vsx: load next tape failed");
			exit(errno);
		}
	}
#endif
}
#endif
/*
 * $Log: vsx.c,v $
 * Revision 1.94  1999-08-30 18:27:28-04  gsl
 * Trim spaces off the volume. This was a bug, it was constructing the
 * file name with embedded spaces.
 *
 * Revision 1.93  1998-10-29 09:30:16-05  gsl
 * Remove un-needed include file.
 *
 * Revision 1.92  1998-10-28 17:03:45-05  gsl
 * Add LARGE_FILES support for AIX.
 * Removed unneeded includes.
 *
 * Revision 1.91  1998-10-23 16:34:27-04  gsl
 * fix the gcc -Wall warnings
 *
 * Revision 1.90  1998-09-09 16:35:23-04  gsl
 * fix warning
 *
 * Revision 1.89  1998-09-04 16:25:52-04  gsl
 * Attempted to add ACL processing for 8mm tapes.
 * Not sure if correct until I get a sample tape.
 *
 * Revision 1.88  1998-09-03 18:06:32-04  gsl
 * Add support for ACL's for tapes that are not-8mm
 *
 * Revision 1.86  1998-03-18 14:53:52-05  gsl
 * Add -v option which includes the VOLUME name in the output file path
 *
 * Revision 1.85  1997-09-11 09:11:43-04  gsl
 * Finish up the improvements to recognizing file orgs and types.
 * It now fully understands the FDR1ORG and FDR1FLAGS bit masks.
 * Now it recognizes RELATIVE files and doesn't unload them.
 * The output report has been reformated to match the VS BACKUP.
 *
 * Revision 1.84  1997-09-09 21:14:32-04  gsl
 * VSX now recognizes PROG and PRINT file types.
 * The PROG are programs which are treated like INDEXD files and ingnored.
 * The PRINT are printer files and are treated like text files.
 *
 * Revision 1.83  1997-06-05 12:54:09-04  scass
 * Corrected definition for setord.  No return value so
 * changed int to void
 *
 * Revision 1.82  1995-06-13 07:19:06-04  gsl
 * fix warning
 *
 * Revision 1.81  1995/06/13  09:57:54  gsl
 * change NEED_STRDUP to NOSTRDUP on the #ifdef
 *
 * Revision 1.80  1995/05/01  09:18:38  gsl
 * Added even more verbose error messages and error checking.
 *
 * Revision 1.79  1995/04/27  11:32:46  gsl
 * added error checking around all the fread() and fwrite() rotuines.
 *
 * Revision 1.78  1995/04/27  10:21:47  gsl
 * Removed the normalize_byteorder() logic.
 * Embed the vsx.h inline. (vsx.h is now oboslete)
 * prototyped the rest of the routines.
 *
 * Revision 1.77  1995/04/26  15:51:28  gsl
 * normalize on both in and out
 *
 * Revision 1.76  1995/04/26  14:43:25  gsl
 * fix the routine normalize_byteorder() as it was broken for
 * little-endian machines.
 * This likely caused the COMPRESSED files to break.
 *
 * Revision 1.75  1995/04/25  09:58:45  gsl
 * drcs state V3_3_15
 *
 * Revision 1.74  1995/04/17  11:51:10  gsl
 * drcs state V3_3_14
 *
 * Revision 1.73  1995/02/06  14:49:13  gsl
 * fix copyright
 *
 * Revision 1.72  1995/02/02  11:06:22  gsl
 * fixed the copyright messages to Devtech
 * and fixed some compiler warnings
 *
 * Revision 1.71  1995/02/02  10:44:19  gsl
 * fix duplicate copyright
 *
 * Revision 1.70  1995/02/01  00:45:30  gsl
 * added devtech copyright
 *
 * Revision 1.69  1995/01/31  23:08:48  gsl
 * Merged in the 1.64.1.1 changes that JEC made at SFG.
 * Only two areas have changed and I enclosed then in ifdef REV_1_64_1_1
 * for now until I can figure out what they do.
 *
 * Revision 1.68  1995/01/31  00:28:13  gsl
 * fix a missing end of comment
 * VSX is now in sync
 *
 * Revision 1.67  1994/09/21  18:42:26  jockc
 * small code cleanup
 *
 * Revision 1.66  1994/09/21  18:26:49  jockc
 * take out gnu protoize sysfunc protos
 *
 * Revision 1.65  1994/09/21  18:24:46  jockc
 * code cleanup
 *
 * Revision 1.64  1994/05/03  01:34:19  jockc
 * restore var file handling
 *
 * Revision 1.63  1994/04/04  17:06:54  jockc
 * dgux 9 track tape fix
 *
 * Revision 1.62  1994/03/16  18:26:50  jockc
 * changed ifdef SOLARIS3480 to ifdef SOLARIS
 *
 * Revision 1.61  1994/03/15  20:15:45  jockc
 * move the code around a little
 *
 * Revision 1.60  1994/03/15  20:10:29  jockc
 * added code to handle 3480 autoloading tape drive.
 * changed var file handling a tad.. var files appear to look like fixed
 *
 * Revision 1.59  1993/11/08  19:04:57  jockc
 * rename loop var from open: to openloop:
 *
 * Revision 1.58  1993/10/14  17:28:20  jockc
 * fixed usage that was slightly wrong
 *
 * Revision 1.57  1993/10/14  17:23:57  jockc
 * change to use intdef.h.. also, fixed looping EOF thing
 *
 * Revision 1.56  1993/05/19  22:14:17  jockc
 * reinsert the code that lower cases the lib that was accidentally removed
 * in rev 1.55
 *
 * Revision 1.55  1992/12/01  22:52:50  jockc
 * added support for -9 debugging.. changed to print warning message if
 * user's tape contains INDEX files
 *
 * Revision 1.54  1992/08/20  21:01:50  jockc
 * added new logic to handle multi tape archives.  light testing.
 *
 * Revision 1.53  1992/08/17  20:42:54  jockc
 * first pass at multi-tape archives
 *
 * Revision 1.52  1992/06/11  16:57:42  jockc
 * removed more warnings
 *
 * Revision 1.51  1992/06/09  23:45:44  jockc
 * some changes to remove warnings from gcc and ansi C
 *
 * Revision 1.50  1992/06/04  22:32:20  jockc
 * att adds
 *
 * Revision 1.49  1992/05/21  22:49:37  jockc
 * prevent too-big blocks on NCR
 *
 * Revision 1.48  1992/04/09  17:48:04  jockc
 * pass arg to wait() for NCRE
 *
 * Revision 1.47  1992/01/22  22:34:05  jockc
 * fixup nasty 8-mil/9-track-reel bug whereby
 * data in files is repeated.. also added code
 * to do -tmp file processing
 *
 * Revision 1.46  1991/12/06  23:05:31  jockc
 * more null to space stuff..
 *
 * Revision 1.45  1991/12/06  22:11:49  jockc
 * added linefeeds for normal files, null check
 *
 * Revision 1.44  1991/12/04  21:02:15  jockc
 * fixed 2k block thing for normal files
 *
 * Revision 1.43  1991/12/03  21:00:30  jockc
 * write proper number of bytes for source files which have been trimmed to
 * less than recsz
 *
 * Revision 1.42  1991/11/26  00:27:28  jockc
 * added code for 2k blocking anomoly
 *
 * Revision 1.41  1991/11/21  18:14:39  jockc
 * changed fprintf to fwrite on output in postprocess
 *
 * Revision 1.40  1991/11/21  01:06:49  jockc
 * add lf/nolf logic
 *
 * Revision 1.39  1991/11/18  19:41:36  jockc
 * fixup extra junk at end of med block for fixed files
 *
 * Revision 1.38  1991/11/08  19:39:08  jockc
 * bug fixs: index mode trying to write tmp header on compressed files
 * archive end not detected on cartridge due to 8 mil logic reading past
 * end of tape file mark
 *
 * Revision 1.37  1991/09/06  22:08:47  jockc
 * rewrote 8 mil load stuff, comp and norm records handled
 * differently.  fixes skipped files thing.
 *
 * Revision 1.36  1991/09/05  00:20:24  jockc
 * big changes.  rewrote big portions of load() (again)
 * took out daemon aspect.  readtape now handled in main image.  readtape
 * handles 8 mil.  added 8 mil flag. updated usage().  massive cleanup
 * of unused code and variables.  some work still to be done there.
 * seperated decomp of records from load phase, moved decomp to
 * postprocessing phase.  fork before decomp to tolerate corrupt files.
 *
 * Revision 1.35  1991/04/30  00:59:41  jockc
 * fixed mis's bug.  index files were screwing things up, so
 * I hacked around it.  Index files are now skipped.  (that is
 * org byte & 0x03 == 0x02 means index file)
 *
 * Revision 1.34  1991/04/23  00:13:06  jec
 * fixed harte bug.  uninitialized var next in bldrec
 * was sending us into comp0 state at loops end
 *
 * Revision 1.1  1991/04/18  23:35:12  jockc
 * Initial revision
 *
 *
 * ---old sccs log follows---
 * 
 * D 1.32 91/03/13 13:56:42 root 32 31  00096/00067/00695
 * improved debugging, fixup enator tape big/eof thing, file org wrong byte
 * 
 * D 1.31 91/02/04 10:24:18 root 31 30  00003/00002/00759
 * cleaned up handling at end of tape if no eof mark
 * 
 * D 1.30 91/02/04 09:33:48 root 30 29  00001/00001/00760
 * D 1.29 91/02/04 09:04:34 root 29 28  00002/00005/00759
 * put verbose info back into no eof handler
 * 
 * D 1.28 91/02/03 11:16:06 root 28 27  00019/00002/00745
 * fixed datavantage extra bytes thing
 * 
 * D 1.27 91/02/01 16:05:39 root 27 26  00059/00025/00688
 * fixed aptech bug (bldrec state table.. needed to allow for NEXTCOMP instead of COMP0 )
 * fixed 2/4 byte reccnt bug
 * 
 * D 1.26 91/01/25 10:48:54 root 26 25  00001/00001/00712
 * fixed bug that appeared on SCO box.. shmat checked for less
 * than zero instead of -1
 * 
 * D 1.25 91/01/07 17:38:26 root 25 24  00009/00007/00704
 * fixed bug if big block ends right before header and grabs
 * two too many bytes (cnss tape)
 * 
 * D 1.24 91/01/07 15:30:50 jec 24 23   00002/00002/00709
 * fixed print of date
 * 
 * D 1.23 91/01/07 15:28:49 jec 23 22   00001/00001/00710
 * trying to fix auto print of version and date
 * 
 * D 1.22 91/01/07 15:22:04 jec 22 21   00024/00013/00687
 * changed printing of Noncompressed/Compressed in listing
 * added support for rectype variable
 * 
 * D 1.21 91/01/04 15:19:57 root 21 20  00013/00008/00687
 * cleaned up interactive prompting
 * 
 * D 1.20 91/01/04 14:50:29 root 20 19  00042/00007/00653
 * better handling of quit signal
 * 
 * D 1.19 91/01/04 13:02:19 root 19 18  00015/00003/00645
 * added experimental auto type detect (compressed vs noncompressed)
 * and auto source/data detect
 * 
 * D 1.18 90/12/04 19:57:07 root 18 17  00009/00002/00639
 * fixed null term record bug
 * 
 * D 1.17 90/12/04 19:01:24 root 17 16  00024/00008/00617
 * fixed bug: if get ffff0090 when expecting
 * med count, set eofflag and continue reading
 * 
 * D 1.16 90/12/04 13:44:38 root 16 15  00001/00001/00624
 * fixed bug in verbose printf listing
 * 
 * D 1.15 90/12/04 13:40:26 root 15 14  00070/00038/00555
 * made changes to support non compressed records.. need to test
 * 
 * D 1.14 90/12/03 16:34:19 root 14 13  00044/00012/00549
 * added code to support data files (not trimmed, different extensions
 * for source vs normal seq files.. default extensions added)
 * 
 * D 1.13 90/12/03 14:08:46 root 13 12  00005/00003/00556
 * more asthetic changes, and added copyright info
 * 
 * D 1.12 90/12/03 14:04:31 root 12 11  00003/00001/00556
 * more asthetic changes
 * 
 * D 1.11 90/12/03 14:00:28 root 11 10  00023/00000/00534
 * added usage(), fixed default archive, some asthetic fixes
 * 
 * D 1.10 90/12/03 13:37:49 root 10 9   00002/00002/00532
 * fixed trim code. was killing column 72-80. changed to kill 73-80, (and kill spaces)
 * 
 * D 1.9 90/11/20 11:32:17 root 9 8     00034/00335/00500
 * 2nd rewrite of load()... port to rs6000
 * 
 * D 1.8 90/11/20 10:13:38 root 8 7     00284/00058/00551
 * rewrote load, rewriting again
 * 
 * D 1.7 90/11/15 17:07:43 root 7 6     00136/00027/00473
 * finished load(), etc
 * 
 * D 1.6 90/11/14 18:39:32 root 6 5     00159/00013/00341
 * added most of load()
 * 
 * D 1.5 90/11/13 17:24:31 root 5 4     00171/00020/00183
 * improved daemon, load, buildrec...
 * need to finish load
 * 
 * D 1.4 90/11/13 12:47:17 root 4 3     00121/00006/00082
 * added daemon(), started load().. misc other funcs
 * 
 * D 1.3 90/11/12 16:19:39 root 3 2     00075/00000/00013
 * more stuff added, parseopts and matchopt
 * 
 * D 1.2 90/11/12 10:48:35 root 2 1     00004/00001/00009
 * testing sccs... 
 *
 * D 1.1 90/11/12 10:43:41 root 1 0     00010/00000/00000
 * date and time created 90/11/12 10:43:41 by root
 *
 */

