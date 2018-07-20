/*
**	Copyright (c) Shell Stream Software LLC, All rights reserved.
**
**	$Id:$
**
**	WISP fragments for include into sub85.c 
**
**	This file is divided into three fragments, each is enclosed 
**	within #ifdef/#endif
**
**	WISP_SUB85_HEADER	- Header info
**	WISP_SUB85_LIBTABLE	- LIBTABLE entries
**	WISP_SUB85_ROUTINES	- Supporting routines
*/

#ifdef WISP_SUB85_HEADER
/************************************************************************/

/*
	To reduce the size of the RTS you can remove any of the following
	"#define" statements and the corresponding files will not be
	included.

	ACP	 The Wang ACP routines.
	NETCAP	 The Netron Cap routines.
	EDE	 The EDE routines.
	KCSI	 The KCSI utilties (Control Report Inquiry Datentry Create)
*/

#include <string.h>

#ifdef unix
#define ACP
#endif

#define NETCAP

/*
**  Include the KCSI header files
*/
#ifdef KCSI
#define KCSI_SUB85_HEADER
#include "kcsi_sub85_inc.c"
#endif /* KCSI */

int wispfrontend( char* name, int num_args, Argument args[], int initial, ... );
int wispfrontend2( char* name, int num_args, Argument args[], int initial, ... );


/************************************************************************/
#endif /* WISP_SUB85_HEADER */


#ifdef WISP_SUB85_LIBTABLE
/************************************************************************/
/*
	The first entry must be UPPERCASE.
*/

/*
**  VSSUBS & USERSUBS
*/
	{ "BELL", 	wispfrontend, NULL },
	{ "BITPACK",	wispfrontend, NULL },
	{ "BITUNPK",	wispfrontend, NULL },
	{ "CANCEL",	wispfrontend, NULL },
	{ "CEXIT",	wispfrontend, NULL },
	{ "COBLINK",	wispfrontend, NULL },
	{ "DATE",	wispfrontend, NULL },
	{ "DATE2",	wispfrontend, NULL },
	{ "DATE4",	wispfrontend, NULL },
	{ "DATE6",	wispfrontend, NULL },
	{ "DAY",	wispfrontend, NULL },
	{ "EXTRACT",	wispfrontend, NULL },
	{ "EXTRACT2",	wispfrontend, NULL },
	{ "FIND",	wispfrontend, NULL },
	{ "GETPARM",	wispfrontend, NULL },
	{ "HEXPACK",	wispfrontend, NULL },
	{ "HEXUNPK",	wispfrontend, NULL },
	{ "LINK",	wispfrontend2, NULL },
	{ "LOGOFF",	wispfrontend, NULL },
	{ "MESSAGE",	wispfrontend, NULL },
	{ "PAUSE",	wispfrontend, NULL },
	{ "PRINT",	wispfrontend, NULL },
	{ "PUTPARM",	wispfrontend, NULL },
	{ "READFDR",	wispfrontend, NULL },
	{ "READFDR4",	wispfrontend, NULL },
	{ "READVTOC",	wispfrontend, NULL },
	{ "RENAME",	wispfrontend, NULL },
	{ "SCRATCH",	wispfrontend, NULL },
	{ "SCREEN",	wispfrontend, NULL },
	{ "SEARCH",	wispfrontend, NULL },
	{ "SET",	wispfrontend, NULL },
	{ "SET2",	wispfrontend, NULL },
	{ "SETFILE",	wispfrontend, NULL },
	{ "SORT",	wispfrontend, NULL },
	{ "SORTCALL",	wispfrontend, NULL },
	{ "SORTLINK",	wispfrontend, NULL },
	{ "STRING",	wispfrontend, NULL },		
	{ "SUBMIT",	wispfrontend, NULL },
	{ "UPDATFDR",	wispfrontend, NULL },
	{ "WSXIO",	wispfrontend, NULL },

/*
**  WISPSUBS
*/
	{ "ACUGARGS", 	wispfrontend, NULL },
	{ "ACUNARGS", 	wispfrontend, NULL },
	{ "ACUPARGS", 	wispfrontend, NULL },
	{ "BIT_OFF",	wispfrontend, NULL },
	{ "BIT_ON",	wispfrontend, NULL },
	{ "BIT_TEST",	wispfrontend, NULL },
	{ "FILECOPY",	wispfrontend, NULL },
	{ "FXZONE",	wispfrontend, NULL },
	{ "GETPARMBUILD",wispfrontend, NULL },
	{ "INITWISP2",	wispfrontend, NULL },
	{ "INITWISP3",	wispfrontend, NULL },
	{ "ISDBFILE",	wispfrontend2, NULL },
	{ "LBIT_OFF",	wispfrontend, NULL },
	{ "LBIT_ON",	wispfrontend, NULL },
	{ "LINKPROC",	wispfrontend, NULL },
	{ "MWCONV",	wispfrontend, NULL },
	{ "NOHELP",	wispfrontend, NULL },
	{ "ONHELP",	wispfrontend, NULL },
	{ "RETCODE",	wispfrontend, NULL },
	{ "SET8BIT",	wispfrontend, NULL },
	{ "SETFILE2",	wispfrontend, NULL },
	{ "SETRETCODE", wispfrontend, NULL },
	{ "SETSUBMIT",  wispfrontend, NULL },
	{ "SLEEPONE",   wispfrontend, NULL },
	{ "SORTINFO",	wispfrontend, NULL },
	{ "UPPER",	wispfrontend, NULL },
	{ "USEHARDLINK",wispfrontend, NULL },
	{ "USESOFTLINK",wispfrontend, NULL },
	{ "VWANG",	wispfrontend, NULL },
	{ "W4WAPI",	wispfrontend, NULL },
	{ "W99TOX",	wispfrontend, NULL },
	{ "WACCEPT",	wispfrontend, NULL },
	{ "WANSI2WANG",	wispfrontend, NULL },
	{ "WCHAIN",	wispfrontend, NULL },
	{ "WDISPLAY",	wispfrontend, NULL },
	{ "WFCLOSE",	wispfrontend, NULL },
	{ "WFILECHK2",  wispfrontend, NULL },
	{ "WFILECHK3",	wispfrontend, NULL },
	{ "WFNAME",     wispfrontend, NULL },
	{ "WFNAME2",    wispfrontend, NULL },
	{ "WFOPEN3",	wispfrontend, NULL },
	{ "WFOPEN4",	wispfrontend, NULL },
	{ "WFWAIT",	wispfrontend, NULL },
	{ "WGETCURPOS",	wispfrontend, NULL },
	{ "WGETFILEXT", wispfrontend, NULL },
	{ "WISPEXIT",	wispfrontend, NULL },
	{ "WISPHELP",	wispfrontend, NULL },
	{ "WISPPLAT",	wispfrontend, NULL },
	{ "WISPSHUT",	wispfrontend, NULL },
	{ "WISPSORT",	wispfrontend, NULL },
	{ "WISPSYNC",	wispfrontend, NULL },
	{ "WS_CLOSE",	wispfrontend, NULL },
	{ "WS_READ",	wispfrontend, NULL },
	{ "WS_READ_ALT",	wispfrontend, NULL },
	{ "WS_REWRITE",	wispfrontend, NULL },
	{ "WSCREEN",	wispfrontend, NULL },
	{ "WSETFACBLINK", wispfrontend, NULL },
	{ "WSETFILEXT", wispfrontend, NULL },
	{ "WSTOP",	wispfrontend, NULL },
	{ "WTITLE",	wispfrontend, NULL },
	{ "WVASET",     wispfrontend, NULL },
	{ "WWANG2ANSI",	wispfrontend, NULL },
	{ "X4DBFILE",	wispfrontend2, NULL },
	{ "X4DBFILE2",	wispfrontend2, NULL },

/*
** The following are obsolete API routines used by 
** older versions of WISP. They are included for
** backwards compatablity.
*/
	{ "GETWFILEXT", wispfrontend, NULL },	/* getwfilext	-> WGETFILEXT */
	{ "SETPROGID",  wispfrontend, NULL },	/* Obsolete */
	{ "SETTRIGPROG",wispfrontend, NULL },	/* Obsolete */
	{ "SETWFILEXT", wispfrontend, NULL },	/* setwfilext	-> WSETFILEXT */
	{ "SETWISPFILEXT", wispfrontend, NULL }, /* setwispfilext-> WSETFILEXT */
	{ "W2ROWCOL",	wispfrontend, NULL },	/* w2rowcol 	-> WGETCURPOS */
	{ "WEXITH",     wispfrontend, NULL },	/* wexith 	-> WISPEXIT */
	{ "WFILECHK",   wispfrontend, NULL },	/* Obsolete */
	{ "WFOPEN",	wispfrontend, NULL },	/* Obsolete */
	{ "WFOPEN2",	wispfrontend, NULL },	/* Obsolete */
	{ "WMEMCPY",	wispfrontend, NULL },	/* Obsolete */
	{ "WPAUSE",	wispfrontend, NULL },	/* wpause	-> PAUSE */
	{ "WRENAME",	wispfrontend, NULL },	/* wrename 	-> RENAME */
	{ "WSETSTAT",	wispfrontend, NULL },	/* Obsolete */
	{ "XX2BYTE",	wispfrontend, NULL },	/* xx2byte 	-> W99TOX */

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
	{ "WSCLOSE",	wispfrontend, NULL },
	{ "WSFNM",	wispfrontend, NULL },
	{ "WSFNS",	wispfrontend, NULL },
#endif
/*
** The following are ACP routines 
*/
#ifdef ACP
	{ "BREAKACP",	wispfrontend, NULL },
	{ "CHECKACP",	wispfrontend, NULL },
	{ "CLOSEACP",	wispfrontend, NULL },
	{ "GETACP",	wispfrontend, NULL },
	{ "OPENACP",	wispfrontend, NULL },
	{ "READACP",	wispfrontend, NULL },
	{ "SETACP",	wispfrontend, NULL },
	{ "WRITEACP",	wispfrontend, NULL },
#endif

/*
** The following are EDE routines
*/
#ifdef EDE
	{ "A_WSLINK",	wispfrontend, NULL },
	{ "DYLINK",	wispfrontend, NULL },
	{ "DYUNLINK",	wispfrontend, NULL },
	{ "EDCLRSCR",	wispfrontend, NULL },
	{ "EDDRKSCR",	wispfrontend, NULL },
	{ "EDEXIT",	wispfrontend, NULL },
	{ "EDLOAD",	wispfrontend, NULL },
	{ "EDLTESCR",	wispfrontend, NULL },
	{ "EDNARSCR",	wispfrontend, NULL },
	{ "EDWIDSCR",	wispfrontend, NULL },
	{ "GCALC",	wispfrontend, NULL },
	{ "GCALEND",	wispfrontend, NULL },
	{ "GCLOCK",	wispfrontend, NULL },
	{ "GENVEC",	wispfrontend, NULL },
	{ "GNOTEPAD",	wispfrontend, NULL },
	{ "GPUZZLE",	wispfrontend, NULL },
	{ "MENUCONT",	wispfrontend, NULL },
	{ "MENUEXIT",	wispfrontend, NULL },
	{ "MENUGO",	wispfrontend, NULL },
	{ "MENUINFO",	wispfrontend, NULL },
	{ "MENUITEM",	wispfrontend, NULL },
	{ "MENUKILL",	wispfrontend, NULL },
	{ "MENULOAD",	wispfrontend, NULL },
	{ "MENUMODE",	wispfrontend, NULL },
	{ "MENUREST",	wispfrontend, NULL },
	{ "MENUSAVE",	wispfrontend, NULL },
	{ "NOPFKEYS",	wispfrontend, NULL },
	{ "PFKEYSON",	wispfrontend, NULL },
	{ "POPAREA",	wispfrontend, NULL },
	{ "PUSHAREA",   wispfrontend, NULL },
	{ "PUSHSCRN",	wispfrontend, NULL },
	{ "RETRACE",	wispfrontend, NULL },
	{ "TRACEEND",	wispfrontend, NULL },
	{ "TRACEGO",	wispfrontend, NULL },
	{ "VIDLINE",	wispfrontend, NULL },
	{ "VIDMODE",	wispfrontend, NULL },
	{ "VIDMOVE",	wispfrontend, NULL },
	{ "VIDTEXT",	wispfrontend, NULL },
#endif /* EDE */

/*
** This includes the KCSI utility routines
*/
#ifdef KCSI
#define KCSI_SUB85_LIBTABLE
#include "kcsi_sub85_inc.c"
#endif /* KCSI */

/************************************************************************/
#endif /* WISP_SUB85_LIBTABLE */



#ifdef WISP_SUB85_ROUTINES
/************************************************************************/

typedef short 		 int2; 
typedef int  		 int4;
typedef unsigned short  uint2;  
typedef unsigned int   	uint4;


extern void WL_reversebytes();
extern int  WL_bytenormal();
#define WERRCODE(x)	((int4)(x))
extern void WL_werrlog();
extern void WL_werrlog_error();
extern void WL_wtrace(); /* (const char* routine, const char* code, const char* format, ... ); */
extern void WL_wexit();
extern void VL_set_isdebug_true();
extern void VL_set_isdebug_false();
extern char *WL_upper_string();
extern void WL_setdbfile();
extern void WL_setdbfile_attr();
extern int wisp_nativescreens();
extern void WL_set_va_count();

/*
**  VSSUBS & USERSUBS
*/
extern int BELL();
extern int BITPACK(); 
extern int BITUNPK();
extern int CANCEL();
extern int CEXIT();
extern int COBLINK();
extern int WISPDATE();	/* DATE ->	WISPDATE */
extern int DATE2();
extern int DATE4();
extern int DATE6();
extern int DAY();
extern int EXTRACT();
extern int EXTRACT2();
extern int FIND();
extern int GETPARM();
extern int HEXPACK();
extern int HEXUNPK();
extern int LINK2();	/* LINK		-> LINK2 */
extern int LOGOFF();
extern int MESSAGE();
extern int PAUSE();
extern int PRINT();
extern int PUTPARM();
extern int READFDR();
extern int READFDR4();
extern int READVTOC();
extern int RENAME();
extern int SCRATCH();
extern int SCREEN();
extern int SEARCH();
extern int SET();
extern int SET2();
extern int SETFILE();
extern int SORT();
extern int SORTCALL();
extern int SORTLINK();
extern int STRING();
extern int SUBMIT();
extern int UPDATFDR();
extern int WSXIO();

/*
**  WISPSUBS
*/
extern int ACUGARGS();
extern int ACUNARGS();
extern int ACUPARGS();
extern int BIT_OFF();
extern int BIT_ON();
extern int BIT_TEST();
extern int FILECOPY();
extern int FXZONE();
extern int getparmbuild();
extern int INITWISP2();
extern int INITWISP3();
extern int ISDBFILE();
extern int LBIT_OFF();
extern int LBIT_ON();
extern int LINKPROC();
extern int MWCONV();
extern int NOHELP();
extern int ONHELP();
extern int RETCODE();
extern int SET8BIT();
extern int SETFILE2();
extern int SETPROGID();
extern int SETRETCODE();
extern int SETSUBMIT();
extern int SLEEPONE();
extern int SORTINFO();
extern int UPPER();
extern int USEHARDLINK();
extern int USESOFTLINK();
extern int vwang();
extern int W4WAPI();
extern int W99TOX();
extern int WACCEPT();
extern int WANSI2WANG();
extern int WCHAIN();
extern int WDISPLAY();
extern int WFCLOSE();
extern int WFILECHK2();
extern int WFILECHK3();
extern int WFNAME();
extern int WFNAME2();
extern int WFOPEN3();
extern int WFOPEN4();
extern int WFWAIT();
extern int WGETCURPOS();
extern int WGETFILEXT();
extern int WISPEXIT();
extern int WISPHELP();
extern int WISPPLAT();
extern int WISPSHUT();
extern int WISPSORT();
extern int WISPSYNC();
extern int WS_CLOSE();
extern int WS_READ();
extern int WS_READ_ALT();
extern int WS_REWRITE();
extern int WSCREEN();
extern int WSETFACBLINK();
extern int WSETFILEXT();
extern int WSTOP();
extern int WTITLE();
extern int WVASET();
extern int WWANG2ANSI();
extern int X4DBFILE();
extern int X4DBFILE2();

/*
** Obsolete
*/
extern int SETTRIGPROG();
extern int WFILECHK();
extern int WFOPEN();
extern int WFOPEN2();
extern int WMEMCPY();
extern int WSETSTAT();

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
extern int WSCLOSE();
extern int WSFNM();
extern int WSFNS();
#endif

/*
** The following are ACP routines
*/
#ifdef ACP
extern int OPENACP();
extern int CLOSEACP();
extern int READACP();
extern int WRITEACP();
extern int BREAKACP();
extern int CHECKACP();
extern int GETACP();
extern int SETACP();
#endif

/*
** The following are EDE routines
*/
#ifdef EDE
extern int A_WSLINK();
extern int DYLINK();
extern int DYUNLINK();
extern int EDCLRSCR();
extern int EDDRKSCR();
extern int EDEXIT();
extern int EDLOAD();
extern int EDLTESCR();
extern int EDNARSCR();
extern int EDWIDSCR();
extern int GENVEC();
extern int MENUCONT();
extern int MENUEXIT();
extern int MENUGO();
extern int MENUINFO();
extern int MENUITEM();
extern int MENUKILL();
extern int MENULOAD();
extern int MENUMODE();
extern int MENUREST();
extern int MENUSAVE();
extern int NOPFKEYS();
extern int PFKEYSON();
extern int POPAREA();
extern int PUSHAREA();
extern int PUSHSCRN();
extern int RETRACE();
extern int TRACEEND();
extern int TRACEGO();
extern int VIDLINE();
extern int VIDMODE();
extern int VIDMOVE();
extern int VIDTEXT();
extern int gcalc();
extern int gcalend();
extern int gclock();
extern int gen_ncpfkey();
extern int gnotepad();
extern int gpuzzle();
extern int nc_pop_menu();
extern int ws_bar_menu();
#endif /* EDE */


struct	WISPPROCTABLE {
	char	*name;		/* subroutine name in UPPERCASE */
	int	(*routine)();	/* pointer to function to call */
};

struct	WISPPROCTABLE WISPTABLE[] = {
/*
**  VSSUBS & USERSUBS
*/
	{ "BELL",	BELL },
	{ "BITPACK",	BITPACK },
	{ "BITUNPK",	BITUNPK },
	{ "CANCEL",	CANCEL },	
	{ "CEXIT",	CEXIT },	
	{ "COBLINK",	COBLINK },	
	{ "DATE",	WISPDATE },
	{ "DATE2",	DATE2 },
	{ "DATE4",	DATE4 },
	{ "DATE6",	DATE6 },
	{ "DAY",	DAY },
	{ "EXTRACT",	EXTRACT },	
	{ "EXTRACT2",	EXTRACT2 },	
	{ "FIND",	FIND },
	{ "GETPARM",	GETPARM },
	{ "HEXPACK",	HEXPACK },
	{ "HEXUNPK",	HEXUNPK },
	{ "LINK",	LINK2 },	/* NOTE: Translate LINK ==> LINK2 */
	{ "LOGOFF",	LOGOFF },
	{ "MESSAGE",	MESSAGE },
	{ "PAUSE",	PAUSE },
	{ "PRINT",	PRINT },
	{ "PUTPARM",	PUTPARM },
	{ "READFDR",	READFDR },
	{ "READFDR4",	READFDR4 },
	{ "READVTOC",	READVTOC },
	{ "RENAME",	RENAME },
	{ "SCRATCH",	SCRATCH },
	{ "SCREEN",	SCREEN },
	{ "SEARCH",	SEARCH },
	{ "SET",	SET },
	{ "SET2",	SET2 },
	{ "SETFILE",	SETFILE},
	{ "SORT",	SORT },
	{ "SORTCALL",	SORTCALL },
	{ "SORTLINK",	SORTLINK },
	{ "STRING",	STRING },
	{ "SUBMIT",	SUBMIT },
	{ "UPDATFDR",	UPDATFDR },
	{ "WSXIO",	WSXIO },

/*
**  WISPSUBS
*/
	{ "ACUGARGS",	ACUGARGS },
	{ "ACUNARGS",	ACUNARGS },
	{ "ACUPARGS",	ACUPARGS },
	{ "BIT_OFF",	BIT_OFF },
	{ "BIT_ON",	BIT_ON },
	{ "BIT_TEST",	BIT_TEST },
	{ "FILECOPY",	FILECOPY },
	{ "FXZONE",	FXZONE },
	{ "GETPARMBUILD",getparmbuild },
	{ "INITWISP2",	INITWISP2 },
	{ "INITWISP3",	INITWISP3 },
	{ "ISDBFILE",	ISDBFILE },
	{ "LBIT_OFF",	LBIT_OFF },
	{ "LBIT_ON",	LBIT_ON },
	{ "LINKPROC",	LINKPROC },
	{ "MWCONV",	MWCONV },
	{ "NOHELP",	NOHELP },
	{ "ONHELP",	ONHELP },
	{ "RETCODE",	RETCODE },
	{ "SET8BIT",	SET8BIT },
	{ "SETFILE2",	SETFILE2 },
	{ "SETRETCODE", SETRETCODE },
	{ "SETSUBMIT",	SETSUBMIT },
	{ "SLEEPONE",	SLEEPONE },
	{ "SORTINFO",	SORTINFO },
	{ "UPPER",	UPPER },
	{ "USEHARDLINK",USEHARDLINK },
	{ "USESOFTLINK",USESOFTLINK },
	{ "VWANG",	vwang },
	{ "W4WAPI",	W4WAPI },
	{ "W99TOX",	W99TOX },
	{ "WACCEPT",	WACCEPT },
	{ "WANSI2WANG", WANSI2WANG },	
	{ "WCHAIN",	WCHAIN },
	{ "WDISPLAY",	WDISPLAY },
	{ "WFCLOSE",	WFCLOSE },
	{ "WFILECHK2",  WFILECHK2 },
	{ "WFILECHK3",	WFILECHK3 },
	{ "WFNAME",     WFNAME },
	{ "WFNAME2",    WFNAME2 },
	{ "WFOPEN3",	WFOPEN3 },
	{ "WFOPEN4",	WFOPEN4 },
	{ "WFWAIT",	WFWAIT },
	{ "WGETCURPOS",	WGETCURPOS },
	{ "WGETFILEXT", WGETFILEXT },
	{ "WISPEXIT",	WISPEXIT },
	{ "WISPHELP",	WISPHELP },
	{ "WISPPLAT",	WISPPLAT },
	{ "WISPSHUT",	WISPSHUT },
	{ "WISPSORT",	WISPSORT },
	{ "WISPSYNC",	WISPSYNC },
	{ "WS_CLOSE",		WS_CLOSE },
	{ "WS_READ",		WS_READ },
	{ "WS_READ_ALT",	WS_READ_ALT },
	{ "WS_REWRITE",		WS_REWRITE },
	{ "WSCREEN",	WSCREEN },
	{ "WSETFACBLINK", WSETFACBLINK },
	{ "WSETFILEXT", WSETFILEXT },
	{ "WSTOP",	WSTOP },
	{ "WTITLE",	WTITLE },
	{ "WVASET", 	WVASET },
	{ "WWANG2ANSI", WWANG2ANSI },	
	{ "X4DBFILE",	X4DBFILE },
	{ "X4DBFILE2",	X4DBFILE2 },

/*
** The following are obsolete API routines used by 
** older versions of WISP. They are included for
** backwards compatablity.
*/
	{ "GETWFILEXT", WGETFILEXT },	/* getwfilext	-> WGETFILEXT */
	{ "SETPROGID",  SETPROGID },	/* Obsolete */
	{ "SETTRIGPROG",SETTRIGPROG },	/* Obsolete */
	{ "SETWFILEXT", WSETFILEXT },	/* setwfilext	-> WSETFILEXT */
	{ "SETWISPFILEXT", WSETFILEXT}, /* setwispfilext-> WSETFILEXT */
	{ "W2ROWCOL",	WGETCURPOS },	/* w2rowcol 	-> WGETCURPOS */
	{ "WEXITH",     WISPEXIT },	/* WEXITH 	-> WISPEXIT */
	{ "WFILECHK",   WFILECHK },	/* Obsolete */
	{ "WFOPEN",	WFOPEN },	/* Obsolete */
	{ "WFOPEN2",	WFOPEN2 },	/* Obsolete */
	{ "WMEMCPY",    WMEMCPY },	/* Obsolete */
	{ "WPAUSE",     PAUSE },	/* wpause	-> PAUSE */
	{ "WRENAME",	RENAME },	/* wrename 	-> RENAME */
	{ "WSETSTAT",	WSETSTAT },	/* Obsolete */
	{ "XX2BYTE",	W99TOX },	/* xx2byte 	-> W99TOX */

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
	{ "WSCLOSE",	WSCLOSE },
	{ "WSFNM",	WSFNM },
	{ "WSFNS",	WSFNS },
#endif
/*
** The following are ACP routines 
*/
#ifdef ACP
	{ "BREAKACP",	BREAKACP },
	{ "CHECKACP",	CHECKACP },
	{ "CLOSEACP",	CLOSEACP },
	{ "GETACP",	GETACP   },
	{ "OPENACP",	OPENACP  },
	{ "READACP",	READACP  },
	{ "SETACP",	SETACP   },
	{ "WRITEACP",	WRITEACP },
#endif
/*
** The following are EDE routines
*/
#ifdef EDE
	{ "A_WSLINK",	A_WSLINK },
	{ "DYLINK",	DYLINK 	},
	{ "DYUNLINK",	DYUNLINK },
	{ "EDCLRSCR",	EDCLRSCR },
	{ "EDDRKSCR",	EDDRKSCR },
	{ "EDEXIT",	EDEXIT 	},
	{ "EDLOAD",	EDLOAD 	},
	{ "EDLTESCR",	EDLTESCR },
	{ "EDNARSCR",	EDNARSCR },
	{ "EDWIDSCR",	EDWIDSCR },
	{ "GCALC",	gcalc },
	{ "GCALEND",	gcalend },
	{ "GCLOCK",	gclock },
	{ "GENVEC",	GENVEC },
	{ "GEN_NCPFKEY",gen_ncpfkey},
	{ "GNOTEPAD",	gnotepad },
	{ "GPUZZLE",	gpuzzle },
	{ "MENUCONT",	MENUCONT },
	{ "MENUEXIT",	MENUEXIT },
	{ "MENUGO",	MENUGO 	},
	{ "MENUINFO",	MENUINFO },
	{ "MENUITEM",	MENUITEM },
	{ "MENUKILL",	MENUKILL },
	{ "MENULOAD",	MENULOAD },
	{ "MENUMODE",	MENUMODE },
	{ "MENUREST",	MENUREST },
	{ "MENUSAVE",	MENUSAVE },
	{ "NC_POP_MENU",nc_pop_menu},
	{ "NOPFKEYS",	NOPFKEYS },
	{ "PFKEYSON",	PFKEYSON },
	{ "POPAREA",	POPAREA },
	{ "PUSHAREA",	PUSHAREA },
	{ "PUSHSCRN",	PUSHSCRN },
	{ "RETRACE",	RETRACE },
	{ "TRACEEND",	TRACEEND },
	{ "TRACEGO",	TRACEGO },
	{ "VIDLINE",	VIDLINE },
	{ "VIDMODE",	VIDMODE },
	{ "VIDMOVE",	VIDMOVE },
	{ "VIDTEXT",	VIDTEXT },
	{ "WS_BAR_MENU",ws_bar_menu},
#endif /* EDE */

/*
** Terminate with a NULL
*/
	{ NULL,		NULL }
};


int wispfrontend( char* name, int num_args, Argument args[], int initial, ... )
{
#define MAXARGS 128
	int i;
	int (*fn)();
	struct WISPPROCTABLE *p;

	for (p = WISPTABLE; p->name && 0 != strcmp(p->name,name); ++p);
	if (p->name && 0 == strcmp(p->name,name))
	{
		fn = p->routine;
	}
 	else
	{
		return 1;
	}

	if (num_args > MAXARGS)
	{		
		WL_werrlog_error(WERRCODE(104),"SUB85","WFRONTEND", 
			"MAXARGS exceeded [num_args = %d]",num_args);
		
		num_args = MAXARGS;
	}

	WL_set_va_count(num_args);

	if (num_args > 0)
	{
		long	tempbin[MAXARGS];					/* Place to store binary values to reverse bytes*/
		void 	*arglist[MAXARGS]; 

		memset((char *)arglist, '\0', sizeof(arglist));

		for (i=0; i < num_args; ++i)
		{
			if ( !WL_bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				WL_reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
				arglist[i] = (void *)&tempbin[i];				/* Put addr of temp in arglist	*/
			}
			else
			{
				arglist[i] = (void *)args[i].a_address;
			}
		}

		/*
		**	Call the function.
		*/
		(*fn)(
		      arglist[0],      arglist[1],	arglist[2],      arglist[3],
		      arglist[4],      arglist[5],    	arglist[6],      arglist[7],
		      arglist[8],      arglist[9],  	arglist[10],     arglist[11],
		      arglist[12],     arglist[13],   	arglist[14],     arglist[15],
		      arglist[16],     arglist[17],   	arglist[18],     arglist[19],
		      arglist[20],     arglist[21],   	arglist[22],     arglist[23],
		      arglist[24],     arglist[25],	arglist[26],     arglist[27],
		      arglist[28],     arglist[29],	arglist[30],     arglist[31],
		      arglist[32],     arglist[33],	arglist[34],     arglist[35],
		      arglist[36],     arglist[37],	arglist[38],     arglist[39],
		      arglist[40],     arglist[41],	arglist[42],     arglist[43],
		      arglist[44],     arglist[45],	arglist[46],     arglist[47],
		      arglist[48],     arglist[49],	arglist[50],     arglist[51],
		      arglist[52],     arglist[53],	arglist[54],     arglist[55],
		      arglist[56],     arglist[57],	arglist[58],     arglist[59],
		      arglist[60],     arglist[61],	arglist[62],     arglist[63],
		      arglist[64],     arglist[65],	arglist[66],     arglist[67],
		      arglist[68],     arglist[69],	arglist[70],     arglist[71],
		      arglist[72],     arglist[73],	arglist[74],     arglist[75],
		      arglist[76],     arglist[77],	arglist[78],     arglist[79],
		      arglist[80],     arglist[81],	arglist[82],     arglist[83],
		      arglist[84],     arglist[85],	arglist[86],     arglist[87],
		      arglist[88],     arglist[89],	arglist[90],     arglist[91],
		      arglist[92],     arglist[93],	arglist[94],     arglist[95],
		      arglist[96],     arglist[97],	arglist[98],     arglist[99],
		      arglist[100],    arglist[101],	arglist[102],    arglist[103],
		      arglist[104],    arglist[105],	arglist[106],    arglist[107],
		      arglist[108],    arglist[109],	arglist[110],    arglist[111],
		      arglist[112],    arglist[113],	arglist[114],    arglist[115],
		      arglist[116],    arglist[117],	arglist[118],    arglist[119],
		      arglist[120],    arglist[121],	arglist[122],    arglist[123],
		      arglist[124],    arglist[125],	arglist[126],    arglist[127]);

		if ( !WL_bytenormal() )
		{
			for (i=0; i < num_args; ++i)
			{
				if ( args[i].a_type == BinarySigned ||
				     args[i].a_type == BinaryUnsigned )
				{
					WL_reversebytes(&tempbin[i], args[i].a_length);		/* Undo the byte reverse	*/
					memcpy(args[i].a_address, &tempbin[i], args[i].a_length);	/* Restore argument	*/
				}
			}
		}
	}
	else 
	{
		/*
		**	Call the function with no arguments
		*/
		(*fn)();
	}	
	return 0;
}

/*
	wispfrontend2 - Acts just like wispfrontend except it puts both the address and the length on the stack. So for each arg
		     there 2 entries on the stack.
*/	
int wispfrontend2( char* name, int num_args, Argument args[], int initial, ... )
{
#define MAXARGS2 64
	int i,l;
	int (*fn)();
	struct WISPPROCTABLE *p;

	for (p = WISPTABLE; p->name && 0 != strcmp(p->name,name); ++p);
	if (p->name && 0 == strcmp(p->name,name))
	{
		fn = p->routine;
	}
 	else
	{
		return 1;
	}

	if (num_args > MAXARGS2)
	{
		WL_werrlog_error(WERRCODE(104),"SUB85","WFRONTEND2", 
			"MAXARGS exceeded [num_args = %d]",num_args);

		num_args = MAXARGS2;
	}

	WL_set_va_count(num_args);

	if (num_args > 0)
	{
		long	tempbin[MAXARGS2];					/* Place to store binary values to reverse bytes*/
		void 	*arglist[MAXARGS2*2]; 

		memset((char *)arglist, '\0', sizeof(arglist));

		for (i=0,l=0; i < num_args; ++i)
		{
			if ( !WL_bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				WL_reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
				arglist[l++] = (void *)&tempbin[i];				/* Put addr of temp in arglist	*/
			}
			else
			{
				arglist[l++] = (void *)args[i].a_address; 
			}

			arglist[l++] = (void *)args[i].a_length;				/* Put the length on the stack	*/
		}

		/*
		**	Call the function.
		*/
		(*fn)(
		      arglist[0],	arglist[1],	arglist[2],      arglist[3],
		      arglist[4],	arglist[5],   	arglist[6],      arglist[7],
		      arglist[8],	arglist[9],   	arglist[10],     arglist[11],
		      arglist[12],	arglist[13],  	arglist[14],     arglist[15],
		      arglist[16],	arglist[17],  	arglist[18],     arglist[19],
		      arglist[20],	arglist[21],  	arglist[22],     arglist[23],
		      arglist[24],	arglist[25],	arglist[26],     arglist[27],
		      arglist[28],	arglist[29],	arglist[30],     arglist[31],
		      arglist[32],	arglist[33],	arglist[34],     arglist[35],
		      arglist[36],	arglist[37],	arglist[38],     arglist[39],
		      arglist[40],	arglist[41],	arglist[42],     arglist[43],
		      arglist[44],	arglist[45],	arglist[46],     arglist[47],
		      arglist[48],	arglist[49],	arglist[50],     arglist[51],
		      arglist[52],	arglist[53],	arglist[54],     arglist[55],
		      arglist[56],	arglist[57],	arglist[58],     arglist[59],
		      arglist[60],	arglist[61],	arglist[62],     arglist[63],
		      arglist[64],	arglist[65],	arglist[66],     arglist[67],
		      arglist[68],	arglist[69],	arglist[70],     arglist[71],
		      arglist[72],	arglist[73],	arglist[74],     arglist[75],
		      arglist[76],	arglist[77],	arglist[78],     arglist[79],
		      arglist[80],	arglist[81],	arglist[82],     arglist[83],
		      arglist[84],	arglist[85],	arglist[86],     arglist[87],
		      arglist[88],	arglist[89],	arglist[90],     arglist[91],
		      arglist[92],	arglist[93],	arglist[94],     arglist[95],
		      arglist[96],	arglist[97],	arglist[98],     arglist[99],
		      arglist[100],	arglist[101],	arglist[102],    arglist[103],
		      arglist[104],	arglist[105],	arglist[106],    arglist[107],
		      arglist[108],	arglist[109],	arglist[110],    arglist[111],
		      arglist[112],	arglist[113],	arglist[114],    arglist[115],
		      arglist[116],	arglist[117],	arglist[118],    arglist[119],
		      arglist[120],	arglist[121],	arglist[122],    arglist[123],
		      arglist[124],	arglist[125],	arglist[126],    arglist[127]);


		if ( !WL_bytenormal() )
		{
			for (i=0; i < num_args; ++i)
			{
				if ( args[i].a_type == BinarySigned ||
				     args[i].a_type == BinaryUnsigned )
				{
					WL_reversebytes(&tempbin[i], args[i].a_length);		/* Undo the byte reverse	*/
					memcpy(args[i].a_address, &tempbin[i], args[i].a_length);	/* Restore argument	*/
				}
			}
		}
	}
	else 
	{
		/*
		**	Call the function with no arguments
		*/
		(*fn)();
	}	

	return 0;
}

/*
**	Routine:	wisp_acu_cobol()
**
**	Function:	To identify if we are in an Acucobol RTS.
**
**	Description:	Two versions exist.
**			The one in sub85.c returns TRUE.
**			The one in acustubs.c returns FALSE.
**
**	Arguments:	None
**
**	Return:		
**	1		TRUE	- in an Acucobol RTS
**	0		FALSE	- not in an Acucobol RTS
**
*/
int wisp_acu_cobol()
{
	return 1; /* TRUE */
}

/*
**	Routine:	WL_call_acucobol()
**
**	Function:	To "call" an Acucobol COBOL routine from 'C'.
**
**	Description:	This routine builds the correct calling sequence to call
**			an Acucobol COBOL routine from 'C'.
**			Currently this is used for SOFTLINK.
**
**	Arguments:
**	filespec	The full file spec of the COBOL routine to call.
**	parmcnt		The number of parameters to pass.
**	parms		Array of pointers to the parms.
**	lens		Array of lengths of the parms
**	rc		The return code from the "cobol" routine (A_call_err). 
**
**	Globals:
**	A_call_err	The error code from Acucobol
**
**	Return:		None
**
**	Warnings:	None
**
*/
void WL_call_acucobol( char* filespec, int4 parmcnt, char *parms[], int4 lens[], int* rc )
{
#define MAX_CALL_ARGS	32

#if !defined(WISP_ACU50) && !defined(WISP_ACU51) && !defined(WISP_ACU52)
extern short *Astdlib_A_call_err();
#define  A_call_err  (*Astdlib_A_call_err()) 
#endif /* 6.0 - 9.0 */

#if defined(WISP_ACU50) || defined(WISP_ACU51) 
	extern	short A_call_err;
#endif

#if defined(WISP_ACU52)
#ifndef A_call_err
#ifdef  _WINDOWS 
extern short *Astdlib_A_call_err();
#define  A_call_err  (*Astdlib_A_call_err()) 
#else 
extern short *I59752();
#define  A_call_err  (*I59752()) 
#endif 
#endif /* !A_call_err */
#endif /* WISP_ACU52 */

#ifdef WIN32
#ifdef ACUCANCEL42
extern void acu_cancel(char *);
#else
#if defined(WISP_ACU60) || defined(WISP_ACU62)
extern void PASCAL acu_cancel(char *); 
#endif
#endif
#else
extern void acu_cancel();
#endif
	Argument args[MAX_CALL_ARGS];
	int	argcnt, i;

	*rc = 0;
	if (parmcnt > MAX_CALL_ARGS)
	{
		*rc = -1;
		return;
	}

	argcnt = (int)parmcnt;

	for(i=0; i<argcnt; i++)
	{
		args[i].a_address = parms[i];
		args[i].a_length  = (int)lens[i];
	}

	if (0 != cobol(filespec, argcnt, args))
	{
		char msg[100];

		int cec = cobol_exit_code ( msg, sizeof(msg) -1 );
		msg[sizeof(msg)-1] = '\0';

		*rc = (int)A_call_err;

		WL_wtrace("WL_call_acucobol", "Failed", "Call cobol(\"%s\") returned [%d] [%s] cec=[%d]", 
			filespec, *rc, msg, cec);
	}

	/*
	**	Cancel the program
	*/
	acu_cancel(filespec);

	return;
}

static int wisp_wexit = 0;

void WL_shutexitcobol(int exit_code)				/* Called by wisp from WL_wexit()  */
{
	extern void stop_runtime();
	
	/*
	**	If already processing WISP exit logic then just return.
	*/
	if (wisp_wexit)
	{
		return;
	}

	/*
	**	This prevents a COBOL and WISP exit loop.
	*/
	wisp_wexit = 1;

	/*
	**	Call stop_runtime() to have cobol stop the runtime.
	*/
	stop_runtime(exit_code,0,0,0,0,0);			/* Acucobol routine to shutdown */
}

/*
	To prepare the terminal for C screen I/O, at the top of your C routine 
	call w_reset_term() to place the terminal in its default state.

	To prepare the terminal for COBOL screen I/O, before your C routine returns 
	call w_set_term() to place the terminal back into the "COBOL" state. 
*/

void WL_start_cobol_screen_handler()
{
	w_set_term();
}

void WL_shutdown_cobol_screen_handler()
{
	w_reset_term();
}

/*
**	AShutdown is called by ACUCOBOL at shutdown.
**	If error_halt==0 then a normal STOP RUN otherwise error.
*/
void AShutdown( int error_halt )
{
	/*
	**	If a normal exit from the runtime then just return.
	*/
	if (0==error_halt)
	{
		return;
	}
	
	/*
	**	This is an abnormal exit (0 != error_halt).
	**
	**	If we are not already processing WL_wexit() then go thur the WISP exit logic.
	*/
	if (0 == wisp_wexit)
	{
		if (!wisp_nativescreens())
		{
			
			WL_werrlog(WERRCODE(104),"Terminating on an error detected by ACUCOBOL.",0,0,0,0,0,0,0);
#ifdef unix
			WL_werrlog(WERRCODE(104),"Use the -e runtime option to capture the error message.",0,0,0,0,0,0,0);
#endif
		}
		
		wisp_wexit = 1;

		WL_wexit(error_halt);
	}
}
/*
**	Rename the AShutdown() in sub.c to OLD_AShutdown() 
**	to prevent conflict with out custom AShutdown().
**	This is a shortcut so WISP does not need to distribute
**	a custom version of sub.c.
*/
#define AShutdown OLD_AShutdown

int exam_args( int argc, char *argv[] )
{
	int idx;
	int gotdashd = 0;

	/* Start looking at argv[1] the second arg */
	for (idx=1; idx<argc; idx++)
	{
		if (argv[idx] && 0==strcmp(argv[idx],"-d"))
		{
			gotdashd = 1;
		}
	}
	if (gotdashd)
	{
		VL_set_isdebug_true();
	}
	else
	{
		VL_set_isdebug_false();
	}
	return 0;
}

/*
**	Rename the exam_args() in sub.c to OLD_exam_args() 
**	to prevent conflict with out custom exam_args().
**	This is a shortcut so WISP does not need to distribute
**	a custom version of sub.c.
*/
#define exam_args OLD_exam_args

/*
**	Routine:	ISDBFILE()
**
**	Function:	Check if a file is a Database file
**
**	Description:	This routine uses Agetenv() to check the $A_CONFIG file
**			to see if this file is a database file.
**
**	Arguments:
**	select_name	The COBOL select name of the file. It should be terminated
**			by a space or a period or by length.
**	answer		The answer flag 
**				'Y' yes
**				'N' no
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	04/06/94	Written by GSL
**
*/
int ISDBFILE(char* select_name, int4 l1, char* answer, int4 l2)
{
	static	int first = 1;
	static	char	default_host[40];
	int	i;
	char	*ptr;
	char	test_host[80];
	char	buff[80];
	char	*Agetenv();

	if (first)
	{
		/*
		**	The first time thru get the default host, if not set assume VISION.
		*/
		if ((ptr = Agetenv("DEFAULT_HOST")))
		{
			strcpy(default_host,ptr);
			WL_upper_string(default_host);
		}
		else
		{
			strcpy(default_host,"VISION");
		}
		first = 0;
	}

	for(i=0; i < l1; i++)
	{
		if (select_name[i] <= ' ' ||
		    select_name[i] == '.' ||
		    select_name[i] >  'z'   ) break;

		test_host[i] = select_name[i];
		if (test_host[i] == '-') test_host[i] = '_';
	}
	test_host[i] = (char)0;
	strcat(test_host,"_HOST");
	WL_upper_string(test_host);

	if ((ptr = Agetenv(test_host)))
	{
		strcpy(buff,ptr);
		WL_upper_string(buff);
	}
	else
	{
		strcpy(buff,default_host);
	}

	if (	(0 == strcmp(buff,"INFORMIX"))	||
		(0 == strcmp(buff,"ORACLE"))	||
		(0 == strcmp(buff,"MSSQL"))	||
		(0 == strcmp(buff,"ODBC"))	||
		(0 == strcmp(buff,"DB2"))	||
		(0 == strcmp(buff,"SYBASE"))	  )
	{
		*answer = 'Y';
	}
	else
	{
		*answer = 'N';
	}
	return 0;
}

/*
**	Routine:	X4DBFILE()
**
**	Function:	To set the IS_DBFILE flag in a files status word.
**
**	Description:	This routine uses ISDBFILE() to check the ACUCONFIG file
**			to see if this file is a database file if it is it will set
**			flag in the status word.
**
**	Arguments:
**	select_name	The COBOL select name of the file. It should be terminated
**			by a space or a period or by length.
**	select_status	The 4 byte status for the file.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	04/06/94	Written by GSL
**
*/
int X4DBFILE(char* select_name, int4 l1, int* select_status, int4 l2)
{
	char	answer;
	int4	one = 1;

	ISDBFILE(select_name, l1, &answer, one);

	if ('Y' == answer)
	{
		WL_setdbfile(select_status,1);
	}
	else
	{
		WL_setdbfile(select_status,0);
	}
	return 0;
}
int X4DBFILE2(char* select_name, int4 l1, char* file_attributes, int4 l2)
{
	char	answer;
	int4	one = 1;

	ISDBFILE(select_name, l1, &answer, one);

	if ('Y' == answer)
	{
		WL_setdbfile_attr(file_attributes,1);
	}
	else
	{
		WL_setdbfile_attr(file_attributes,0);
	}
	return 0;
}

/*
** Include KCSI interface routines
*/
#ifdef KCSI
#define KCSI_SUB85_ROUTINES
#include "kcsi_sub85_inc.c"
#endif /* KCSI */

/************************************************************************/
#endif /* WISP_SUB85_ROUTINES */


#undef WISP_SUB85_HEADER
#undef WISP_SUB85_LIBTABLE
#undef WISP_SUB85_ROUTINES
