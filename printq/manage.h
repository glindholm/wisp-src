/*
 * Header:  manage.h
 * Program: IDSIprint
 * Purpose: primary header for screen based manager
 *      Id: $Id:$
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
#include <v/vkeymap.h>

/*
EXFUN( int,  findqueueitem,  (struct q_item **, struct q_item **));
EXFUN( int,  findprinter,  (struct p_item **, struct p_item **));
*/
int cangotop(void), cangobot(void), cangoup(void), cangodn(void);
int doup(void), dodn(void), dotop(void), dobot(void), doholdrel(struct q_item *q), dodel(struct q_item *q);
int togglemode(void), dorem(struct q_item *q), doendis(struct p_item *p), dochgfrm(struct p_item *p);
int doalign(struct p_item *p), dopup(void), dopdn(void), dostopstart(struct q_item *q);
int doedit(char *x), notedit(void), stopedit(char *x), shalign(void), prterrp(void), doclrerr(struct p_item *p);
int candisp(void), dodisp(struct q_item *q), canhold(void), canstop(void), canmod(void), canrem(void);
int toggleshow(void), canpmod(void), doshowerr(struct p_item *p), doscrlf(void), doscrrt(void);

/* screen data */
#define Q_TITLE_STR "    ***  UniQue Print Queue Manager %s  ***    "
#define P_TITLE_STR "       ***  UniQue Printer Manager   ***       "

#define Q_PROMPT_STR "Position cursor to indicate file and press function key to perform action     	      "
#define P_PROMPT_STR "Position cursor to indicate printer and press function key to perform action 	      "        

/*                               1         2         3         4         5         6         7         */
/*                     01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
#define Q_HEADER1_STR  "Pos Path                               User     Form     Cls Printer  Status  "
/*                      EEE xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxx EEEEEEEE  E  EEEEEEEE xxxxxxx  */
#define Q_FMT1_STR    "%-3d %-34.34s %-8.8s %-8.8s  %c  %-8.8s %-8.8s"

/*                               1         2         3         4         5         6         7         */
/*                     01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
#define Q_HEADER2_STR  "Pos Path            JobID  Copies Size   StPage  EndPage  Banner?  Disposition "
/*                      EEE xxxxxxxxxxxxxx   xxxx  EEE    xxxxxxx EEEEE   EEEEE    EEE     EEEEEEE     */
#define Q_FMT2_STR    "%-3d %15.15s   %-4d   %-3d  %-7.7s %-5d   %5.5s    %3.3s     %7.7s"

/*                               1         2         3         4         5         6         7         */
/*                     01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
#define P_HEADER1_STR  "Printer  Device                  DefForm  CurForm  Class            Status      "
/*                      xxxxxxxx Xxxxxxxxxxxxxxxxxxxxxxx xxxxxxxx EEEEEEEE EEEEEEEEEEEEEEE  xxxxxxxxx  */
#define P_FMT1_STR    "%-8.8s %c%-22.22s %-8.8s %-8.8s %-15.15s  %s"
#define P_HEADER2_STR "Printer  Device                 Printer #     Current Job      Model            "
#define P_FMT2_STR    "%-8s %c%-22.22s  %-4.4s        %s          %-12.12s"

#define QSCR 1
#define PSCR 2
#define FSCR 3

#define SCREEN struct scr
#define FKEY_ACTION struct fkey_action
FKEY_ACTION
{
	char *function;
	int keyval;		/* keyval returned by vgetm */
	int altkeyval;
	int func;		/* return value or call function */
	int row,col;
        int width;
	char flabel[21];
	char nlabel[21];

	int (*showcond)();	/* func to call to decide to show this key at screen bottom */
	 
	int retval;
	int (*fn)();
	SCREEN *scrptr;
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
	int dmode, scroll, maxlen;
};
#ifdef _IPMAN_MAIN_
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
#else
extern SCREEN main_screen,prtr_screen,*the_screen;
#endif

#define SHOW_ALL 1
#define SHOW_USERS 2

#define IQUEUE 1
#define IUSER 2
#define ITIME 4
#define IPRT 8
#define ICONF 16

#define LEAVE -1
#define QUITILPMAN -2

#define ACT_RETVAL 1
#define ACT_FUNCALL 2
#define ACT_SCREEN 3
#define ACT_FUNCALLARGS 4

#define F_TITLE_STR "  ***  IDSI Print Forms Manager  ***  "
#define F_HEADER2_STR "Form   Filters                                                                  "
#define F_PROMPT_STR "Position cursor to indicate form and press PFkey to perform action          "

#define A_BOLD BOLD
#define A_UNDERLINE UNDERSCORE

char *str_td(void);
char *str_u(void);
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

#define PRTRKEY_DEF 0
#define REMKEY_DEF 1
#define ENDISKEY_DEF 2 
#define MODKEY_DEF 3
#define QUEUEKEY_DEF 4
#define OTHERKEY_DEF 5
#define EXITKEY_DEF 6
#define HOLDKEY_DEF 7
#define ALIGNKEY_DEF 8
#define UPKEY_DEF 9
#define DNKEY_DEF 10
#define PGDNKEY_DEF 11
#define PGUPKEY_DEF 12
#define DISPKEY_DEF 13
#define STOPKEY_DEF 14
#define ALLKEY_DEF 15
#define KEYCNT 16

#define KEY_ESC 1
#define KEY_FN 2
#define KEY_SPECIAL 3

struct km_to_vkey_xlat
{
	int kmval;
	int vkeyval;
	int defaultmapto;
	char *labeliffound;
	char *labelgeneric;
};


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
FKEY_ACTION pfkeys[]=
{	
	{ "EXIT",          (KM_FUNCTION_MASK|((short)1 )),(int)'x',ACT_RETVAL,      21, 1,20, 
	  {0},{"Exit"},                 0, QUITILPMAN, NULL,    NULL    },

	{ "PAGEUP",	   KM_PAGEUP,(int)'u',ACT_FUNCALL,     22, 1,20, 
	  {0},{"Page-Up"},        cangoup, 0,  dopup,    NULL    },

	{ "PAGEDOWN",	   KM_PAGEDOWN,(int)'d',ACT_FUNCALL,     23, 1,20, 
	  {0},{"Page-Down"},      cangodn, 0,  dopdn,    NULL    },


	{ "ENABLEDISABLE", (KM_FUNCTION_MASK|((short)2 )),(int)'e',ACT_FUNCALLARGS, 21,21,20, 
	  {0},{"Enable/Disable"}, canpmod, 0,  doendis,  NULL    },

	{ "SHOWPERR",	   (KM_FUNCTION_MASK|((short)3 )),(int)'s',ACT_FUNCALLARGS, 22,21,20, 
	  {0},{"Show  Error"},    prterrp, 0,  doshowerr, NULL    },

	{ "CLEARPERR",	   (KM_FUNCTION_MASK|((short)4 )),(int)'c',ACT_FUNCALLARGS, 23,21,20, 
	  {0},{"Clear Error"},    prterrp, 0,  doclrerr, NULL    },


	{ "MODIFY",	   (KM_FUNCTION_MASK|((short)6 )),(int)'m',ACT_FUNCALLARGS, 21,41,20, 
	  {0},{"Modify"},         canpmod, 0,  doedit,   NULL    },

	{ "CHANGEFORM",	   (KM_FUNCTION_MASK|((short)7 )),(int)'f',ACT_FUNCALLARGS, 22,41,20, 
	  {0},{"Change Form"},    shalign, 0,  dochgfrm, NULL    },

	{ "ALIGN",	   (KM_FUNCTION_MASK|((short)8 )),(int)'l',ACT_FUNCALLARGS, 23,41,20, 
	  {0},{"Align Form"},     shalign, 0,  doalign,  NULL    },


	{ "OTHERINFO",	   (KM_FUNCTION_MASK|((short)9 )),(int)'o',ACT_FUNCALL,     22,61,20, 
	  {0},{"Show Other"},	    0, 0,  togglemode, NULL  },

	{ "QUEUE",	   (KM_FUNCTION_MASK|((short)10 )),(int)'q',ACT_SCREEN,      23,61,20, 
	  {0},{"Show Queue"},	    0, 0,  NULL, &main_screen},


	{ "UP",	   	   KM_UP,(int)'k',ACT_FUNCALL,     21,61,20,
	  {0},{"          "},		    0, 0,  doup,	NULL      },

	{ "DOWN",	   KM_DOWN,(int)'j',ACT_FUNCALL,     -1,-1,-1, 
	  {0},{""},		    0, 0,  dodn, NULL      },

	{ "",	   	   0,(int)'\020',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doup,	NULL      },

	{ "",	   	   0,(int)'\014',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  dodn, NULL      },

	{ "",	   	   (int)'[',(int)'{',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrlf, NULL      },

	{ "",	   	   (int)']',(int)'}',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrrt, NULL      },

	{ "",	   (int)'<',(int)'t',ACT_FUNCALL,    -1,-1,-1,
	  {0},{""},        0, 0, dotop,      NULL },

	{ "",	   (int)'>',(int)'b',ACT_FUNCALL,    -1,-1,-1,
	  {0},{""},        0, 0, dobot,      NULL },

	{ NULL, 0,0,0,0,0,-1,{0},{0},0,0,NULL,NULL }	     
};
FKEY_ACTION qfkeys[]=
{	
	{ "EXIT",          (KM_FUNCTION_MASK|((short)1 )),(int)'x',ACT_RETVAL,      21, 1,20, 
	  {0},{"Exit"},                 0, QUITILPMAN,  NULL, NULL     },

	{ "PAGEUP",	   KM_PAGEUP,(int)'u',ACT_FUNCALL,     22, 1,20, 
	  {0},{"Page-Up"},        cangoup, 0, dopup,      NULL },

	{ "PAGEDOWN",	   KM_PAGEDOWN,(int)'d',ACT_FUNCALL,     23, 1,20, 
	  {0},{"Page-Down"},      cangodn, 0, dopdn,      NULL },


	{ "DISPLAY",	   (KM_FUNCTION_MASK|((short)2 )),(int)'v',ACT_FUNCALLARGS, 21,21,20, 
	  {0},{"Display File"},   candisp, 0, dodisp,     NULL },

	{ "HOLDREL",       (KM_FUNCTION_MASK|((short)3 )),(int)'h',ACT_FUNCALLARGS, 22,21,20, 
	  {0},{"Hold/Release"},   canhold, 0, doholdrel, NULL  },

	{ "STOPSTART",	   (KM_FUNCTION_MASK|((short)5 )),(int)'s',ACT_FUNCALLARGS, 23,21,20, 
	  {0},{"Stop/Start Job"}, canstop, 0, dostopstart,NULL },


	{ "MODIFY",	   (KM_FUNCTION_MASK|((short)6 )),(int)'m',ACT_FUNCALLARGS, 21,41,20, 
	  {0},{"Modify"},         notedit, 0, doedit, NULL},

	{ "REMOVE",	   (KM_FUNCTION_MASK|((short)7 )),(int)'r',ACT_FUNCALLARGS, 22,41,20, 
	  {0},{"Remove"},          canrem, 0, dorem, NULL }, 


	{ "ALLMINE",	   (KM_FUNCTION_MASK|((short)8 )),(int)'a',ACT_FUNCALL,     21,61,20, 
	  {0},{"Show All/Mine"},        0, 0, toggleshow, NULL },

	{ "OTHERINFO",	   (KM_FUNCTION_MASK|((short)9 )),(int)'o',ACT_FUNCALL,     22,61,20, 
	  {0},{"Show Other"},           0, 0, togglemode,NULL},

	{ "PRINTERS",	   (KM_FUNCTION_MASK|((short)10 )),(int)'p',ACT_SCREEN ,     23,61,20, 
	  {0},{"Show Printers"},        0, 0, NULL,  &prtr_screen},


	{ "UP",	           KM_UP,(int)'k',ACT_FUNCALL,     -1,-1,-1, 
	  {0},{""},                     0, 0, doup,       NULL },

	{ "DOWN",	   KM_DOWN,(int)'j',ACT_FUNCALL,     -1,-1,-1, 
	  {0},{""},                     0, 0, dodn,       NULL },

	{ "",	   	   0,(int)'\020',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doup,	NULL      },

	{ "",	   	   0,(int)'\014',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},			    0, 0,  dodn, NULL      },

	{ "",	   	   (int)'[',(int)'{',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrlf, NULL      },

	{ "",	   	   (int)']',(int)'}',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrrt, NULL      },

	{ "",	   (int)'<',(int)'t',ACT_FUNCALL,    -1,-1,-1,
	  {0},{""},        0, 0, dotop,      NULL },

	{ "",	   (int)'>',(int)'b',ACT_FUNCALL,    -1,-1,-1,
	  {0},{""},        0, 0, dobot,      NULL },

	{ NULL, 0,0,0,0,0,-1,{0},{0},0,0,NULL,NULL }
};
FKEY_ACTION wang_pfkeys[]=
{
	{ "EXIT",          GENERIC_PF1+VMBIAS,(int)'x',ACT_RETVAL,      21, 1,18, 
	  {"(1)"},{" Exit"},                 0, QUITILPMAN, NULL,    NULL    },

	{ "TOP",	   GENERIC_PF2+VMBIAS,(int)'t',ACT_FUNCALL,     22, 1,18, 
	  {"(2)"},{" Top"},        cangotop, 0, dotop,      NULL },

	{ "BOTTOM",	   GENERIC_PF3+VMBIAS,(int)'b',ACT_FUNCALL,     23, 1,18, 
	  {"(3)"},{" Bottom"},      cangobot, 0, dobot,      NULL },

	{ "PAGEUP",	   GENERIC_PF4+VMBIAS,(int)'u',ACT_FUNCALL,     21,19,18, 
	  {"(4)"},{" Up Pg"},        cangoup, 0,  dopup,    NULL    },

	{ "PAGEDOWN",	   GENERIC_PF5+VMBIAS,(int)'d',ACT_FUNCALL,     22,19,18, 
	  {"(5)"},{" Down Pg"},      cangodn, 0,  dopdn,    NULL    },

	{ "ENABLEDISABLE", GENERIC_PF6+VMBIAS,(int)'e',ACT_FUNCALLARGS, 23,19,18, 
	  {"(6)"},{" Enable/Disable"}, canpmod, 0,  doendis,  NULL    },

	{ "CHANGEFORM",	   GENERIC_PF7+VMBIAS,(int)'f',ACT_FUNCALLARGS, 21,37,18, 
	  {"(7)"},{" Change Form"},    shalign, 0,  dochgfrm, NULL    },

	{ "ALIGN",	   GENERIC_PF8+VMBIAS,(int)'l',ACT_FUNCALLARGS, 22,37,18, 
	  {"(8)"},{" Align  Form"},     shalign, 0,  doalign,  NULL    },

	{ "CLEARPERR",	   GENERIC_PF9+VMBIAS,(int)'c',ACT_FUNCALLARGS, 23,37,18, 
	  {"(9)"},{" Clear Error"},    prterrp, 0,  doclrerr, NULL    },

	{ "MODIFY",	   GENERIC_PF10+VMBIAS,(int)'m',ACT_FUNCALLARGS, 21,56,14, 
	  {"(10)"},{" Modify"},         canpmod, 0,  doedit,   NULL    },

	{ "OTHERINFO",	   GENERIC_PF14+VMBIAS,(int)'o',ACT_FUNCALL,     21,70,10, 
	  {"(14)"},{"Other"},	    0, 0,  togglemode, NULL  },

	{ "QUEUE",	   GENERIC_PF15+VMBIAS,(int)'q',ACT_SCREEN,      22,70,10, 
	  {"(15)"},{"Queue"},	    0, 0,  NULL, &main_screen},


	{ "UP",	   	   GENERIC_UP+VMBIAS,(int)'k',ACT_FUNCALL,     -1,-1,-1, 
	  {0},{""},		    0, 0,  doup,	NULL      },

	{ "DOWN",	   GENERIC_DOWN+VMBIAS,(int)'j',ACT_FUNCALL,     -1,-1,-1, 
	  {0},{""},		    0, 0,  dodn, NULL      },

	{ "",	   	   0,(int)'\020',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doup,	NULL      },

	{ "",	   	   0,(int)'\014',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  dodn, NULL      },

	{ "",	   	   (int)'[',(int)'{',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrlf, NULL      },

	{ "",	   	   (int)']',(int)'}',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrrt, NULL      },

	{ NULL, 0,0,0,0,0,-1,{0},{0},0,0,NULL,NULL }	     
};
FKEY_ACTION wang_qfkeys[]=
{
	{ "EXIT",          GENERIC_PF1+VMBIAS,(int)'x',ACT_RETVAL,      21, 1,18, 
	  {"(1)"},{" Exit"},                 0, QUITILPMAN,  NULL, NULL     },

	{ "TOP",	   GENERIC_PF2+VMBIAS,(int)'t',ACT_FUNCALL,     22, 1,18, 
	  {"(2)"},{" Top"},        cangotop, 0, dotop,      NULL },

	{ "BOTTOM",	   GENERIC_PF3+VMBIAS,(int)'b',ACT_FUNCALL,     23, 1,18, 
	  {"(3)"},{" Bottom"},      cangobot, 0, dobot,      NULL },

	{ "PAGEUP",	   GENERIC_PF4+VMBIAS,(int)'u',ACT_FUNCALL,     21, 19,18, 
	  {"(4)"},{" Up Pg"},        cangoup, 0, dopup,      NULL },

	{ "PAGEDOWN",	   GENERIC_PF5+VMBIAS,(int)'d',ACT_FUNCALL,     22, 19,18, 
	  {"(5)"},{" Down Pg"},      cangodn, 0, dopdn,      NULL },

	{ "DISPLAY",	   GENERIC_PF6+VMBIAS,(int)'v',ACT_FUNCALLARGS, 23, 19,18, 
	  {"(6)"},{" Display"},   candisp, 0, dodisp,     NULL },

	{ "HOLDREL",       GENERIC_PF7+VMBIAS,(int)'h',ACT_FUNCALLARGS, 21,37,18, 
	  {"(7)"},{" Hold/Release"},   canhold, 0, doholdrel, NULL  },

	{ "DELETE",	   GENERIC_PF8+VMBIAS,(int)'z',ACT_FUNCALLARGS, 22,37,18, 
	  {"(8)"},{" Delete"},          canrem, 0, dodel, NULL }, 

	{ "STOPSTART",	   GENERIC_PF9+VMBIAS,(int)'s',ACT_FUNCALLARGS, 23,37,18, 
	  {"(9)"},{" Stop/Start"}, canstop, 0, dostopstart,NULL },

	{ "MODIFY",	   GENERIC_PF10+VMBIAS,(int)'m',ACT_FUNCALLARGS, 21,56,14, 
	  {"(10)"},{" Change"},         notedit, 0, doedit, NULL},

	{ "ALLMINE",	   GENERIC_PF11+VMBIAS,(int)'a',ACT_FUNCALL,     22,56,14, 
	  {"(11)"},{" All/Mine"},        0, 0, toggleshow, NULL },

	{ "REMOVE",	   GENERIC_PF12+VMBIAS,(int)'r',ACT_FUNCALLARGS, 23,56,14, 
	  {"(12)"},{" Remove"},          canrem, 0, dorem, NULL }, 

	{ "OTHERINFO",	   GENERIC_PF14+VMBIAS,(int)'o',ACT_FUNCALL,     21,70,10, 
	  {"(14)"},{"Other"},           0, 0, togglemode,NULL},

	{ "PRINTERS",	   GENERIC_PF15+VMBIAS,(int)'p',ACT_SCREEN ,     22,70,10, 
	  {"(15)"},{"Prtrs"},        0, 0, NULL,  &prtr_screen},


	{ "UP",	           GENERIC_UP+VMBIAS,(int)'k',ACT_FUNCALL,     -1,-1,-1, 
	  {0},{""},                     0, 0, doup,       NULL },

	{ "DOWN",	   GENERIC_DOWN+VMBIAS,(int)'j',ACT_FUNCALL,     -1,-1,-1, 
	  {0},{""},                     0, 0, dodn,       NULL },

	{ "",	   	   0,(int)'\020',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doup,	NULL      },

	{ "",	   	   0,(int)'\014',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},			    0, 0,  dodn, NULL      },

	{ "",	   	   (int)'[',(int)'{',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrlf, NULL      },

	{ "",	   	   (int)']',(int)'}',ACT_FUNCALL,  -1,-1,-1, 
	  {0},{""},		    0, 0,  doscrrt, NULL      },

	{ NULL, 0,0,0,0,0,-1,{0},{0},0,0,NULL,NULL }
};

#define EDITDELLEFT_DEFAULT  KM_DELETE
#define EDITDELRIGHT_DEFAULT 0
#define EDITINSERT_DEFAULT KM_INSERT
#define EDITLEFT_DEFAULT KM_LEFT
#define EDITRIGHT_DEFAULT KM_RIGHT

struct km_to_vkey_xlat xlatkeylist[]=
{
	{ (KM_FUNCTION_MASK|((short)1 )),VKEY_F1,  GENERIC_PF1,  "F1", "^F01" },
	{ (KM_FUNCTION_MASK|((short)2 )),VKEY_F2,  GENERIC_PF2,  "F2", "^F02" },
	{ (KM_FUNCTION_MASK|((short)3 )),VKEY_F3,  GENERIC_PF3,  "F3", "^F03" },
	{ (KM_FUNCTION_MASK|((short)4 )),VKEY_F4,  GENERIC_PF4,  "F4", "^F04" },
	{ (KM_FUNCTION_MASK|((short)5 )),VKEY_F5,  GENERIC_PF5,  "F5", "^F05" },
	{ (KM_FUNCTION_MASK|((short)6 )),VKEY_F6,  GENERIC_PF6,  "F6", "^F06" },
	{ (KM_FUNCTION_MASK|((short)7 )),VKEY_F7,  GENERIC_PF7,  "F7", "^F07" },
	{ (KM_FUNCTION_MASK|((short)8 )),VKEY_F8,  GENERIC_PF8,  "F8", "^F08" },
	{ (KM_FUNCTION_MASK|((short)9 )),VKEY_F9,  GENERIC_PF9,  "F9", "^F09" },
	{ (KM_FUNCTION_MASK|((short)10)),VKEY_F10, GENERIC_PF10, "F10","^F10" },
        { (KM_UP), VKEY_UP_ARROW, GENERIC_UP, 0, 0 },
	{ (KM_DOWN), VKEY_DOWN_ARROW, GENERIC_DOWN, 0, 0 },
	{ (KM_LEFT), VKEY_LEFT_ARROW, GENERIC_LEFT, 0, 0 },
	{ (KM_RIGHT), VKEY_RIGHT_ARROW, GENERIC_RIGHT, 0, 0 },
	{ (KM_ENTER), GENERIC_ENTER, GENERIC_ENTER, 0, 0 },
	{ (KM_PAGEUP), VKEY_PREV_SCR, GENERIC_PREV_SCR, "Page-Up", "^F^P" },
	{ (KM_PAGEDOWN), VKEY_NEXT_SCR, GENERIC_NEXT_SCR, "Page-Down", "^F^N" },
	{ (KM_DELETE), VKEY_DELETE, GENERIC_DELETE, "Delete", "^FX" },
	{ (KM_INSERT), VKEY_INSERT, GENERIC_INSERT, "Insert", "^FI" },
	{ -1, -1 }
};
#else
extern FKEY_ACTION pfkeys[];
extern FKEY_ACTION qfkeys[];

extern struct km_to_vkey_xlat xlatkeylist[];

extern my_uid;

extern QEDIT qed;
extern PEDIT ped;

extern char *days[];
extern char *months[];
extern int cur_width;
extern int the_scrid;
extern int incoming;
extern int global_key;
extern char vers_info[];
extern int reentered;

extern int saveqpos;
extern int saveppos;
extern int users_or_all;

extern int edit_saveqpos;
extern int wang_mode;

#endif


#ifdef EXT
EXT char *queue_list;
EXT char *printer_list;
#else
char *queue_list=NULL;
char *printer_list=NULL;
#endif

#ifdef _IPMAN_MAIN_
int screen_width=80;
int showtop,showbot,canup,candn;

char *items[8192];
int item_cnt, pos_in_list, pos_on_screen, top_line_pos;

int screen_mode;
int edit_line;
int field;
int users_or_all=SHOW_USERS;
int wang_mode = FALSE;

int edit_delleft_main;
int edit_delleft_alt = (int)'\177'; /* Rubout */
int edit_delrt_main;
int edit_delrt_alt = (int)'\004';  /* ^d */
int edit_ins_main;
int edit_ins_alt = (int)'\005';    /* ^e */
int edit_left_main;
int edit_left_alt = (int)'\002';   /* ^b */
int edit_right_main;
int edit_right_alt = (int)'\006';  /* ^f */

#endif

#ifdef _IPMAN_MAIN_
int qfpos1[]={1,5,49,59,62,0};
int qflen1[]={3,34,8,1,8,0};
int qfmax1[]={3,QMAXPATH-1,FORMNAMEMAX-1,1,PRTNAMEMAX-1,0};
int qfedal1[]={0,1,0,0,0,0};
int qfspc1[]={0,0,0,0,0,0};
#endif

#ifdef _IPMAN_MAIN_
int qfpos2[]={1,5,28,43,51,60,68,0};
int qflen2[]={3,15,5,5,5,3,7,0};
int qfmax2[]={3,QMAXPATH-1,5,5,6,3,7,0};
int qfedal2[]={0,1,0,0,0,0,0,0};
int qfspc2[]= {0,0,0,0,0,0,0,0};

#endif

#ifdef _IPMAN_MAIN_
int pfpos1[]={10,43,52,0};
int pflen1[]={23,8,15,0};
int pfmax1[]={PIPEMAX-1,FORMNAMEMAX-1,25,0};
int pfedal[]={0,0,0,0};
int pfspc[]={1,0,0,0};
#endif

#ifdef _IPMAN_MAIN_
int pfpos2[]={0};
int pflen2[]={0};
int pfmax2[]={0};


#define OFS(x,y) (int)( (char*)&((x *)0)->y - (char*)((x *)0))

#ifdef OSF1_ALPHA
#define ZEROELEM 
#else
#define ZEROELEM [0]
#endif
int qfdat1[]=
{
	OFS(QITEM,q_pos),
	OFS(QITEM,real_path ZEROELEM),
	OFS(QITEM,form ZEROELEM),
	OFS(QITEM,class),
	OFS(QITEM,requested_prtr ZEROELEM),
	0
};

int qfdat2[]=
{
	OFS(QITEM,q_pos),
	OFS(QITEM,real_path ZEROELEM),
	OFS(QITEM,copies),
	OFS(QITEM,stpage),
	OFS(QITEM,endpage),
	OFS(QITEM,banner),
	OFS(QITEM,mode),
	0
};
int pfdat1[]=
{
	OFS(PITEM,printer_dev ZEROELEM),
	OFS(PITEM,current_form ZEROELEM),
	OFS(PITEM,class ZEROELEM),
	0
};
	
	
int get_int(int *pval, int field_width, int row, int col, int max),get_string(char *string, int wsize, int row, int col, int max),get_bool(int *pval, int size, int row, int col, int dummy),get_disp(int *pval, int size, int row, int col, int dummy);

int (*qfun1[])()={get_int,get_string,get_string,get_string,get_string,NULL};
int (*qfun2[])()={get_int,get_string,get_int,get_int,get_int,get_bool,get_disp,NULL};

int (*pfun1[])()={get_string,get_string,get_string,NULL};
int (*pfun2[])()={NULL};

int *edposarray;
int *edlenarray;
int *edmaxarray;
int (**edfunarray)();
int *eddatarray;
int *ededitallowed;
int *edspcallowed;

MASTCONFIG qconfig;
char *fileread;

char erasefield[81]={ 0 };
char dispprog[128];

#else

extern int edit_delleft_main;
extern int edit_delleft_alt;
extern int edit_delrt_main;
extern int edit_delrt_alt;
extern int edit_ins_main;
extern int edit_ins_alt;
extern int edit_left_main;
extern int edit_left_alt;
extern int edit_right_main;
extern int edit_right_alt;

extern char *fileread;

extern MASTCONFIG qconfig;

extern int qfdat1[],qfdat2[],pfdat1[];

extern int qfpos1[];
extern int qfpos2[];
extern int qflen1[];
extern int qflen2[];
extern int qfmax1[];
extern int qfmax2[];
extern int qfedal1[];
extern int qfedal2[];
extern int qfspc1[];
extern int qfspc2[];

extern int (*qfun1[])();
extern int (*qfun2[])();

extern int pfpos1[];
extern int pflen1[];
extern int pfmax1[];
extern int pfmax2[];
extern int pfpos2[];
extern int pflen2[];
extern int pfedal[];
extern int pfspc[];

extern int (*pfun1[])();
extern int (*pfun2[])();

extern int *edposarray;
extern int *edlenarray;
extern int *edmaxarray;
extern int (**edfunarray)();
extern int *eddatarray;
extern int *ededitallowed;
extern int *edspcallowed;

extern screen_width;

extern char *items[];
extern int item_cnt, pos_in_list, pos_on_screen, top_line_pos;
extern int screen_mode;
extern int edit_line;
extern int field;
extern char erasefield[];
extern char dispprog[];

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

/*
 * $Log: manage.h,v $
 * Revision 1.30  1993/10/12  23:24:45  jockc
 * change to erase unused slots for fkeys
 *
 * Revision 1.29  1993/09/30  23:01:07  jockc
 * still had the class length wrong
 *
 * Revision 1.28  1993/09/30  00:44:21  jockc
 * small adjustment to edit data sizes
 *
 * Revision 1.27  1993/09/15  01:12:06  jockc
 * change q_fmt2_str a little to put ALL instead of 99999
 *
 * Revision 1.26  1993/09/14  22:28:30  jockc
 * last elem in qfkeys structs missing an int item (-1)
 *
 * Revision 1.25  1993/09/13  23:04:21  jockc
 * added dispprog[] var
 *
 * Revision 1.24  1993/09/13  15:26:46  jockc
 * *** empty log message ***
 *
 * Revision 1.24  1993/09/10  18:28:34  jockc
 * new key defs added
 *
 * Revision 1.23  1993/08/13  20:13:34  jockc
 * changes for alpha/c89, begin stuff for key mapping etc. moved
 * log to end
 *
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
