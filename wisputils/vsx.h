/*
 *  module: $RCSfile: vsx.h,v $
 * program: vsx
 *
 * $Log: vsx.h,v $
 * Revision 1.47  1992/06/09  23:46:01  jockc
 * some changes to remove warnings from gcc and ansi C[4~
 *
 * Revision 1.46  1992/06/04  22:32:49  jockc
 * changed NCR to NCR486
 *
 * Revision 1.45  1992/05/21  22:50:03  jockc
 * new read size variables
 *
 * Revision 1.44  1992/04/09  17:52:00  jockc
 * add default tapedef for ncr
 *
 * Revision 1.43  1992/03/24  18:15:21  jockc
 * changed sco def from i386 to sco
 *
 * Revision 1.42  1992/01/22  22:33:44  jockc
 * added -tmp flag to process .tmp files
 *
 * Revision 1.41  1991/12/06  22:11:23  jockc
 * added flag for checking for null bytes
 *
 * Revision 1.40  1991/12/04  21:02:52  jockc
 * adjusted flags
 *
 * Revision 1.39  1991/11/26  00:27:15  jockc
 * added 2k block switch and casts
 *
 * Revision 1.38  1991/11/21  01:04:48  jockc
 * no change
 *
 * Revision 1.37  1991/09/06  22:09:40  jockc
 * no change except rel num
 *
 * Revision 1.36  1991/09/05  00:23:13  jockc
 * cleaned out unused var stuff. other changes to support 8 mil and
 * new load routine
 *
 * Revision 1.35  1991/04/30  01:00:45  jockc
 * changes to allow skippage of index files
 *
 * Revision 1.34  1991/04/23  00:13:39  jec
 * *** empty log message ***
 *
 * Revision 1.33  1991/04/22  18:32:59  jec
 * *** empty log message ***
 *
 * Revision 1.32  1991/04/22  18:28:37  jockc
 * new ver
 *
 * Revision 1.2  1991/04/22  18:26:30  jockc
 * changing ver num
 *
 * Revision 1.1  1991/04/18  23:35:29  jockc
 * Initial revision
 *
 *
 * --old sccs log follows--
 *
 * D 1.17 91/03/13 13:56:53 root 17 16	00015/00004/00213
 * 
 * D 1.16 91/02/01 16:05:26 root 16 15	00002/00002/00215
 * fixed aptech bug (bldrec state table.. needed to allow for NEXTCOMP instead of COMP0 )
 * fixed 2/4 byte reccnt bug
 * 
 * D 1.15 91/01/25 10:59:00 root 15 14	00001/00001/00216
 * fixed default device for SCO
 * 
 * D 1.14 91/01/07 17:38:33 root 14 13	00003/00002/00214
 * 
 * D 1.13 91/01/07 15:21:51 jec 13 12	00014/00000/00202
 * changed printing of Noncompressed/Compressed in listing
 * added support for rectype variable
 * 
 * D 1.12 91/01/04 14:50:55 root 12 11	00002/00001/00200
 * better handling of quit signal, new interactive option
 * 
 * D 1.11 91/01/04 13:01:39 root 11 10	00006/00004/00195
 * added experimental auto type detect (compressed vs noncompressed)
 * 
 * D 1.10 90/12/04 19:01:37 root 10 9	00005/00002/00194
 * 
 * D 1.9 90/12/04 13:40:36 root 9 8	00013/00001/00183
 * made changes to support non compressed records.. need to test
 * 
 * D 1.8 90/12/03 16:34:32 root 8 7	00017/00006/00167
 * added code to support data files (not trimmed, different extensions
 * for source vs normal seq files.. default extensions added)
 * 
 * D 1.7 90/11/20 11:32:27 root 7 6	00012/00000/00161
 * no changes
 * 
 * D 1.6 90/11/15 17:07:53 root 6 5	00014/00005/00147
 * misc
 * 
 * D 1.5 90/11/14 18:39:40 root 5 4	00000/00000/00152
 * misc
 * 
 * D 1.4 90/11/13 17:24:07 root 4 3	00015/00005/00137
 * added state stuff for buildrec
 * 
 * D 1.3 90/11/13 12:46:53 root 3 2	00032/00000/00110
 * added structures, misc stuff
 * 
 * D 1.2 90/11/12 16:51:07 root 2 1	00110/00000/00000
 * more adding of structures and option parsing stuff
 * 
 * D 1.1 90/11/12 11:00:07 root 1 0	00000/00000/00000
 * date and time created 90/11/12 11:00:07 by root
 * 
 */

static char headerid[] = "$Id:$";

#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/msg.h>
#include <signal.h>
#include <fcntl.h>
#include <termio.h>
#include <values.h>
#include <errno.h>

#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
#define FALSE 0
#define TRUE 1

#ifdef SCO					/* This must be define in the .umf file on SCO machines with CFLAGS = -DSCO	*/
#define TAPEDEV "/dev/rct0"
#endif
#ifdef ULTRIX
#define TAPEDEV "/dev/rmt0h"
#endif
#ifdef DGUX
#define TAPEDEV "/dev/rmt/0"
#endif
#ifdef NCR486
#define TAPEDEV "/dev/rmt/c0s0"
#endif
#ifndef TAPEDEV
#define TAPEDEV "/dev/rmt0"
#endif

int skip_lib=0;

#define TAPE_BUFSZ 5120*20
unsigned char tapebuf[TAPE_BUFSZ];
unsigned char workbuf[TAPE_BUFSZ];

char dest_dir[100];
char arch_file[100];

#define FT_SRC 0
#define FT_DAT 1
#define FT_ASK_L 2
#define FT_ASK_F 3

#define RT_COMP 1
#define RT_NORM 2
#define RT_VAR 3

char *rectypenames[]=
{
	"",
	"Compressed",
	"Fixed",
	"Variable"
};

#if 0
#define HD_RT_COMP 0x22
#define HD_RT_NORM 0x01
#define HD_RT_VAR 0x21
#endif

#define ORG_MASK 0x03
#define ORG_INDEX 0x02
#define ORG_CONSEC 0x01

int org_type;

#define HD_RT_COMP 0xc0
#define HD_RT_NORM 0x80
#define HD_RT_VAR 0x88

unsigned short seq_num;

int rec_type = RT_COMP;
int spec_rec_type = RT_COMP;

int file_type= FT_SRC;
int act_file_type= FT_SRC;

#define EXT_ASK_L 1
#define EXT_ASK_F 2
#define EXT_ADD 3
#define EXT_NONE 4

#define DEF_SRC_EXT ".wcb"
#define DEF_NORM_EXT ".seq"

int src_ext_flag=EXT_NONE;
char src_ext[16];
char norm_ext[16];

#define CASE_LOWER 1
#define CASE_UPPER 2

int lname_case= CASE_LOWER;
int fname_case= CASE_LOWER;

#define ACT_EXTRACT 1
#define ACT_INDEX 2

int action=ACT_EXTRACT;
int interactive=FALSE;

struct optstruct
{
	char *opt;
	int type;
	char *optdest;
	int optval;
};


char debug_level_str[20],debug_range[20];
int debug_level=0, debug_start=0, debug_end=0;

#define OPT_INT 1
#define OPT_STRING 2
#define OPT_BAD -1

int eightmil=0;

#define ADDLF 1
#define NOLF 2
#define GUESSLF 3
int add_lf=GUESSLF;
int check_nulls=0;
int process_tmps=0;

char tape_rq_str[200] = {0};
int tape_rq_bytes=TAPE_BUFSZ;

char **arg_list;

struct optstruct optlist[]=
{
	{ "-d", OPT_STRING, dest_dir, 0},
	{ "-lnu", OPT_INT, (char *)&lname_case, CASE_UPPER },
	{ "-fnu", OPT_INT, (char *)&fname_case, CASE_UPPER },
	{ "-es", OPT_STRING, src_ext, 0},
	{ "-en", OPT_STRING, norm_ext, 0},
	{ "-El", OPT_INT, (char *)&src_ext_flag, EXT_ASK_L },
	{ "-Ef", OPT_INT, (char *)&src_ext_flag, EXT_ASK_F },
	{ "-ts", OPT_INT, (char *)&file_type, FT_SRC },
	{ "-td", OPT_INT, (char *)&file_type, FT_DAT },
	{ "-tl", OPT_INT, (char *)&file_type, FT_ASK_L },
	{ "-tf", OPT_INT, (char *)&file_type, FT_ASK_F },
	{ "-rc", OPT_INT, (char *)&rec_type, RT_COMP },
	{ "-rn", OPT_INT, (char *)&rec_type, RT_NORM },
	{ "-rv", OPT_INT, (char *)&rec_type, RT_VAR },
	{ "-i", OPT_INT, (char *)&action, ACT_INDEX },
	{ "-q", OPT_INT, (char *)&interactive, TRUE },
	{ "-8", OPT_INT, (char *)&eightmil, TRUE },
	{ "-f", OPT_STRING, arch_file, 0},
	{ "-xl", OPT_STRING, debug_level_str, 0},
	{ "-xr", OPT_STRING, debug_range, 0},
	{ "-lf", OPT_INT, (char*)&add_lf, ADDLF},
	{ "-nolf", OPT_INT, (char*)&add_lf, NOLF},
	{ "-null", OPT_INT, (char*)&check_nulls, TRUE},
	{ "-tmp", OPT_INT, (char*)&process_tmps, TRUE},
	{ "-bs", OPT_STRING, tape_rq_str, 0},
	{ NULL, 0, NULL, 0 }
};


struct tapeheader {
	char encfhdr[4];
	char vol1[6];
	char userid1[3];
	char filler1[4];
	char version[3];
	char filler2[3];
	char budate[3];
	char srcvol[6];
	char srclib[8];
	char seqnum[2];
	char filler4a[8];
/*	unsigned char foo[2]; */
	unsigned char filler4b;
	unsigned char rectype; /* swapped next two */
	char filler4c[2];
	char filename[8];	
	char filler5[1];
	char date1[3];
	char date2[3];
	char date3[3];
	char filler6[1];
	char userid2[3];
	char filler7[24];
	char reccnt[4];
	char recsz[2];
	char filler8[27];
	char tapevol[6];
	char trailer[5];
};
#define BLANKSPACE 1908

struct my_header
{
	int rectype;
	int filetype;
	int reccnt;
	int recsz;
};

struct stack_node
{
	struct stack_node *prev;
	char *name;
};
struct stack_node *head=NULL, *stackp;

struct tapeheader *tapehdr;

int eoarchive;

#define ST_START 0
#define ST_NORMAL 1
#define ST_COMP 2
#define ST_NEXTCOMP 3
#define ST_END 4
#define ST_COMP0 5

#define ORDER_NORMAL 0
#define ORDER_REVERSE 1

int byteorder;

#define LONG 4
#define SHORT 2

char oldlib[9];

struct termio old,new;

#define D if(debug_level==1)
#define DD if(debug_level==2 && ((seq_num>=(unsigned short)debug_start) && (seq_num<=(unsigned short)debug_end)))
#define DDD if(debug_level==3 && ((seq_num>=(unsigned short)debug_start) && (seq_num<=(unsigned short)debug_end)))

