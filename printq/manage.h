/*
 * Header:  manage.h
 * Program: IDSIprint
 * Purpose: primary header for screen based manager
 *      Id: $Id:$
 * 
 * $Log: manage.h,v $
 * Revision 1.22  1992/07/14  21:36:04  jockc
 * added var for saving qpos before edit
 *
 * Revision 1.21  1992/07/02  18:17:40  jockc
 * added saveqpos and saveppos
 *
 * Revision 1.20  1992/05/07  22:27:55  jockc
 * added a set of arrays that remembers the max size of a field,
 * and rearranged some stuff
 *
 * Revision 1.19  1992/04/30  19:03:52  jockc
 * add input type for config file update, config struct; changed
 * printer info layout, queue info layout
 *
 * Revision 1.18  1992/03/13  21:40:53  jockc
 * slight change of printer headings to support '|' for programs
 *
 * Revision 1.17  1992/02/17  18:56:31  jockc
 * increase size of items list to 8192 for bigger queues
 *
 * Revision 1.16  1991/12/17  22:35:34  jockc
 * added dispostional change
 *
 * Revision 1.15  1991/10/11  19:52:16  jockc
 * moved comm_status to c_comm.h where it should be
 * (duh)
 *
 * Revision 1.14  1991/10/11  19:23:17  jockc
 * define global comm_status
 *
 * Revision 1.13  1991/10/10  22:40:24  jockc
 * added defs for wwaitpid
 *
 * Revision 1.12  1991/10/08  23:27:56  jockc
 * init users_or_all to all
 *
 * Revision 1.11  1991/10/01  17:11:38  jockc
 * various stuff related to comm
 *
 * Revision 1.10  1991/08/05  23:03:25  jockc
 * changed printer scr 2 header for printer number
 *
 * Revision 1.9  1991/08/05  19:59:49  jockc
 * added copies back to qitem format stuff
 *
 * Revision 1.8  1991/08/02  23:52:15  jockc
 * support for show all users' jobs or own
 *
 * Revision 1.7  1991/07/22  23:02:26  jockc
 * added var for keep cursor position when items shift fix
 * fixed irratating warnings on OFS macro instances
 *
 * Revision 1.6  1991/05/22  20:28:30  jockc
 * changed to use up to line 19
 *
 * Revision 1.5  1991/05/22  17:08:56  jockc
 * include time for sco instead of sys/time, chopped out unused
 * and nonquoteterminated const F_HEADER1_STR
 *
 * Revision 1.4  1991/04/30  17:44:06  jockc
 * misc cleanup
 *
 * Revision 1.3  1991/04/25  21:27:43  jockc
 * prelim changes for forms screen
 *
 * Revision 1.2  1991/04/19  00:47:46  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:52:04  jockc
 * Initial revision
 *
 *
 */

#ifdef TRUE
#undef TRUE
#endif
#ifdef FALSE
#undef FALSE
#endif
#include <sys/types.h>
#ifdef i386
#include <time.h>
#else
#include <sys/time.h>
#endif
#undef OFF
#undef ON
#include <v/video.h>
#include <v/vlocal.h>
#include <v/vcap.h>
#include <v/vdata.h>

#define A_BOLD BOLD
#define A_UNDERLINE UNDERSCORE

char *str_td();
char *str_u();
char *str_fkey1();
char *str_fkey2();
char *str_fkey3();

#define A_NORM 0
#define A_HI A_BOLD
#define A_UND A_UNDERLINE
#define A_HI_UND A_BOLD|A_UNDERLINE


#define MAIN_SCREEN 0x0001
#define PRTR_SCREEN 0x0002
#define FORMS_SCREEN 0x0004
#define EDIT_ITEM 0x0008
#define AT_TOP 0x0010
#define AT_BOTTOM 0x0020

#define SCREEN struct scr
#define FKEY_ACTION struct fkey_action
FKEY_ACTION
{
	int keyval;		/* keyval returned by vgetm */
	int func;		/* return value or call function */
	int slot;		/* slot to display in */
	char *desc;		/* what to print as command for this key */
	int (*showcond)();	/* func to call to decide to show this key at screen bottom */
	 
	union act
	{
		int retval;	/* value to return */
		SCREEN *screen;
		int (*func)();	/* function to call */
	} data;
};
#define DESCWID 18

#define QEDIT struct qedit

QEDIT
{
	char pos[4];
	char class[2];
	char form[9];
	char copies[4];
	char requested_dest[9];
	char st[6],end[6];
	int banner,delete;
	char bann[4],del[4];
};

#define PEDIT struct pedit
PEDIT
{
	char name[9];
	char class[26];
};


SCREEN
{
	int scrid;

	char *title_line;
	
	char *header1_line;
	char *header2_line;
	char *prompt_line;
	
	int list_changed;
	char *list_head;
	char *(*fmtline)();
	
	FKEY_ACTION *fkeys;
	int pil,pos,tlp;
	int dmode;
};

#ifdef _IPMAN_MAIN_
char *days[]=
{
	"Sunday",
	"Monday",
	"Tuesday",
	"Wednesday",
	"Thursday",
	"Friday",
	"Saturday",
	NULL
};
char *months[]=
{
	"January",
	"February",
	"March",
	"April",
	"May",
	"June",
	"July",
	"August",
	"September",
	"October",
	"November",
	"December",
	NULL
};
int cur_width=80;
int incoming=FALSE;
SCREEN *the_screen=NULL;
int the_scrid;
int global_key;
QEDIT qed;
PEDIT ped;
int my_uid;
char vers_info[50];
int reentered;

int saveppos;
int saveqpos;

int edit_saveqpos;

#else

extern my_uid;

extern QEDIT qed;
extern PEDIT ped;

extern char *days[];
extern char *months[];
extern int cur_width;
extern int the_scrid;
extern int incoming;
extern int global_key;
extern char vers_info[20];
extern int reentered;

extern int saveqpos;
extern int saveppos;
extern int users_or_all;

extern int edit_saveqpos;

#endif


#ifdef EXT
EXT QLIST *q_head, *q_ptr;
EXT PLIST *p_head, *p_ptr;
#else
QLIST *q_head=NULL, *q_ptr;
PLIST *p_head=NULL, *p_ptr;

#endif


#define SHOW_ALL 1
#define SHOW_USERS 2

#define IQUEUE 1
#define IUSER 2
#define ITIME 4
#define IPRT 8
#define ICONF 16

#define LEAVE -1

#define ACT_RETVAL 1
#define ACT_FUNCALL 2
#define ACT_SCREEN 3
#define ACT_FUNCALLARGS 4

#define F_TITLE_STR "  ***  IDSI Print Forms Manager  ***  "
#define F_HEADER2_STR "Form   Filters                                                                  "
#define F_PROMPT_STR "Position cursor to indicate form and press PFkey to perform action "

#define QSCR 1
#define PSCR 2
#define FSCR 3

#ifdef _IPMAN_MAIN_
int screen_width=80;
int showtop,showbot,canup,candn;

char *items[8192];
int item_cnt, pos_in_list, pos_on_screen, top_line_pos;

int screen_mode;
int edit_line;
int field;
int users_or_all=SHOW_USERS;

int sloty[]=
{
	0,
	21,22,23,
	21,22,23,
	21,22,23,
	21,22,23,
	21,22,23,23
};
int slotx[]=
{
	0,
	1,1,1,
	19,19,19,
	37,37,37,
	56,56,56,
	70,70,70,70
};
int slotw[]=
{
	0,
	18,18,18,
	18,18,18,
	18,18,18,
	14,14,14,
	10,10,10,10
};
#endif

/* screen data */
#define Q_TITLE_STR "   ***  IDSI Print Queue Manager %s  ***   "
#define P_TITLE_STR "  ***  IDSI Print Printer Manager  ***  "
#define Q_PROMPT_STR "Position cursor to indicate file and press PFkey to perform action    "
#define P_PROMPT_STR "Position cursor to indicate printer and press PFkey to perform action "

/*                               1         2         3         4         5         6         7         */
/*                     01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
#define Q_HEADER1_STR  "Pos Path                               User     Form     Cls Printer  Status  "
/*                      EEE xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxx EEEEEEEE  E  EEEEEEEE xxxxxxx  */
#define Q_FMT1_STR    "%-3d %-34.34s %-8.8s %-8.8s  %c  %-8.8s %-8.8s"

#ifdef _IPMAN_MAIN_
int qfpos1[]={1,49,59,62,0};
int qflen1[]={3,8,1,8,0};
int qfmax1[]={3,FORMNAMEMAX,1,PRTNAMEMAX,0};
#endif

/*                               1         2         3         4         5         6         7         */
/*                     01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
#define Q_HEADER2_STR  "Pos Path            JobID  Copies Size   StPage  EndPage  Banner?  Disposition "
/*                      EEE xxxxxxxxxxxxxx   xxxx  EEE    xxxxxxx EEEEE   EEEEE    EEE     EEEEEEE     */
#define Q_FMT2_STR    "%-3d %15.15s   %-4d   %-3d  %-7.7s %-5d   %-5d    %3.3s     %7.7s"

#ifdef _IPMAN_MAIN_
int qfpos2[]={1,28,43,51,60,68,0};
int qflen2[]={3,5,5,5,3,7,0};
int qfmax2[]={3,5,5,5,3,7,0};
#endif

/*                               1         2         3         4         5         6         7         */
/*                     01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
#define P_HEADER1_STR  "Printer  Device                  DefForm  CurForm  Class            Status      "
/*                      xxxxxxxx Xxxxxxxxxxxxxxxxxxxxxxx xxxxxxxx EEEEEEEE EEEEEEEEEEEEEEE  xxxxxxxxx  */
#define P_FMT1_STR    "%-8.8s %c%-22.22s %-8.8s %-8.8s %-15.15s  %s"

#ifdef _IPMAN_MAIN_
int pfpos1[]={43,52,0};
int pflen1[]={8,15,0};
int pfmax1[]={FORMNAMEMAX,26,0};
#endif

#define P_HEADER2_STR "Printer  Device                 Printer #     Current Job      Model            "
#define P_FMT2_STR    "%-8s %c%-22.22s  %-4.4s        %s          %-12.12s"

#ifdef _IPMAN_MAIN_
int pfpos2[]={0};
int pflen2[]={0};
int pfmax2[]={0};

SCREEN main_screen =
{
	QSCR,
	Q_TITLE_STR,
	Q_HEADER1_STR,
	Q_HEADER2_STR,
	Q_PROMPT_STR,
	0,0,0,
	NULL,
	0,0,0,0
};
SCREEN prtr_screen =
{
	PSCR,
	P_TITLE_STR,
	P_HEADER1_STR,
	P_HEADER2_STR,
	P_PROMPT_STR,
	0,0,0,
	NULL,
	0,0,0,0
};

#define OFS(x,y) ( (char*)&((x *)0)->y - (char*)((x *)0))
int qfdat1[]=
{
	OFS(QITEM,q_pos),
	OFS(QITEM,form[0]),
	OFS(QITEM,class),
	OFS(QITEM,requested_prtr[0]),
	0
};
int qfdat2[]=
{
	OFS(QITEM,q_pos),
	OFS(QITEM,copies),
	OFS(QITEM,stpage),
	OFS(QITEM,endpage),
	OFS(QITEM,banner),
	OFS(QITEM,mode),
	0
};
int pfdat1[]=
{
	OFS(PITEM,current_form[0]),
	OFS(PITEM,class[0]),
	0
};
	
	
int get_int(),get_string(),get_bool(),get_disp();

int (*qfun1[])()={get_int,get_string,get_string,get_string,NULL};
int (*qfun2[])()={get_int,get_int,get_int,get_int,get_bool,get_disp,NULL};

int (*pfun1[])()={get_string,get_string,NULL};
int (*pfun2[])()={NULL};

int *edposarray;
int *edlenarray;
int *edmaxarray;
int (**edfunarray)();
int *eddatarray;
MASTCONFIG qconfig;
#else
extern MASTCONFIG qconfig;

extern int qfdat1[],qfdat2[],pfdat1[];

extern int qfpos1[];
extern int qfpos2[];
extern int qflen1[];
extern int qflen2[];
extern int qfmax1[];
extern int qfmax2[];

extern int (*qfun1[])();
extern int (*qfun2[])();

extern int pfpos1[];
extern int pflen1[];
extern int pfmax1[];
extern int pfmax2[];
extern int pfpos2[];
extern int pflen2[];

extern int (*pfun1[])();
extern int (*pfun2[])();

extern int *edposarray;
extern int *edlenarray;
extern int *edmaxarray;
extern int (**edfunarray)();
extern int *eddatarray;

extern SCREEN main_screen,prtr_screen,*the_screen;
extern screen_width;

extern int qfpos1[],qflen1[];

extern int sloty[],slotx[],slotw[];
extern char *items[];
extern int item_cnt, pos_in_list, pos_on_screen, top_line_pos;
extern int screen_mode;
extern int edit_line;
extern int field;

#endif

#define K_UP  1
#define K_DN  2
#define K_TOP 3
#define K_BOT 4
#define K_UPP 5
#define K_DNP 6
#define K_ENDIS 5
#define K_CHGFRM 6
#define K_ALFRM 7
#define K_HOLDREL 8
#define K_DELETE 9
#define K_CHG 10
#define K_REM 11

#define ITEM_ST_LINE 4
#define EDIT_ST_LINE 19
#define ITEM_END_LINE EDIT_ST_LINE-1
#define ITEM_LINES ITEM_END_LINE - ITEM_ST_LINE
#define MODE_NORM 1
#define MODE_EDIT 2



#ifndef WEXITSTATUS
#define WIFEXITED(x)    ( !((x) & 0xff) )
/* evaluates to the low-order 8 bits of the child exit status   */
#define WEXITSTATUS(x)  (int)(WIFEXITED(x) ? (((x) >> 8) & 0xff) : -1)
/* evaluates to a non-zero value if status returned for abnormal termination */
#endif

