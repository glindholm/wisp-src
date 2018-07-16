/*
	sub85_acu51.c		

	This is the WISP compatable version of sub85.c for Acucobol 5.1
	and earlier. 

	If you are using Acucobol 5.1 or earlier you will need to 
	rename this file to sub85.c.

	All of the WISP added code is enclosed in
	"#ifdef WISP" statements.
*/
#define WISP

/* sub85.c - RM/COBOL-85 compatible 'C' routine interface */

/* Copyright (c) 1995-2000 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */


/* THIS FILE IS #INCLUDED FROM sub.c.  BECAUSE SYSTEM HEADER FILES	*/
/* SHOULD BE INCLUDED BEFORE sub.h, AND BECAUSE THIS FILE IS INCLUDED	*/
/* AFTER sub.h, YOU REALLY SHOULDN'T INCLUDE ANY SYSTEM HEADER FILES	*/
/* FROM THIS FILE.  */

/* The following LIBTABLE should be modified to contain the names and	*/
/* function addresses of 'C' routines you wish to link into the runtime	*/
/* system.  This table is searched for each CALL statement to see if a	*/
/* matching routine name is found.  If so, then the corresponding 	*/
/* 'C' function is called.  Note that the table must be terminated by	*/
/* NULL pointers and that the routine names should be all upper case.	*/

/* Each 'C' routine receives 4 parameters.  The first is a pointer to 	*/
/* the name it was called by.  The second is the number of USING 	*/
/* arguments the CALL statement contained.  The third is a pointer to	*/
/* an array of ARGUMENT_ENTRY structures (see "sub.h").  Each array	*/
/* element describes one of the USING arguments.  The final parameter	*/
/* is 1 if this routine is being called for the first time or has been	*/
/* CANCELLED since its last CALL.  Otherwise this parameter is zero.	*/

#ifdef WISP
/************************************************************************/
static char wisp_copyright[]="Copyright (c) 1989-2002 NeoMedia Technologies, All rights reserved.";
static char wisp_rcsid[]="$Id:$";
/*
	To reduce the size of the RTS you can remove any of the following
	"#define" statements and the corresponding files will not be
	included.

		ACP	 The Wang ACP routines.
		NETCAP	 The Netron Cap routines.
		EDE	 The EDE routines.
		CRID	 Control Report Inquiry Datentry (by default they are not included) 
*/

#include <string.h>

typedef short 		 int2; 
typedef int  		 int4;
typedef unsigned short  uint2;  
typedef unsigned int   	uint4;

#ifdef unix
#define ACP
#endif

#define NETCAP
#define EDE

char	WISPFILEXT[39];			/* Define the file extension variable.	*/
char	WISPRETURNCODE[3];		/* Define the return code field.	*/

extern int va_set();
extern void reversebytes();
extern int bytenormal();
extern void werrlog();
extern void wexit();
extern void set_isdebug_true();
extern void set_isdebug_false();
extern char *upper_string();
extern void setdbfile();
extern int nativescreens();

int 	wfrontend();
int	wfrontend2();

/*
**  Include the CRID header files
*/
#ifdef CRID
#include "crid.h"
#endif /* CRID */

/************************************************************************/
#endif /* WISP */

/* int	call_system P_((char *, int, Argument[], int)); */
/* void	wstoasc P_((Argument *, char *)); */

int	call_system();
void	wstoasc();


struct	PROCTABLE LIBTABLE[] = {
	{ "SYSTEM", 	call_system },
#ifdef WISP
/************************************************************************/
/*
	The first entry must be UPPERCASE.
*/
	{ "ACUGARGS", 	wfrontend },
	{ "ACUNARGS", 	wfrontend },
	{ "ACUPARGS", 	wfrontend },
	{ "BELL", 	wfrontend },
	{ "BITPACK",	wfrontend },
	{ "BITUNPK",	wfrontend },
	{ "BIT_OFF",	wfrontend },
	{ "BIT_ON",	wfrontend },
	{ "BIT_TEST",	wfrontend },
	{ "CANCEL",	wfrontend },
	{ "CEXIT",	wfrontend },
	{ "COBLINK",	wfrontend },
	{ "DATE",	wfrontend },
	{ "DATE2",	wfrontend },
	{ "DATE4",	wfrontend },
	{ "DAY",	wfrontend },
	{ "EXTRACT",	wfrontend },
	{ "FILECOPY",	wfrontend },
	{ "FIND",	wfrontend },
	{ "FXZONE",	wfrontend },
	{ "GETPARM",	wfrontend },
	{ "GETPARMBUILD",wfrontend },
	{ "GETWFILEXT", wfrontend },
	{ "GRECLEN",	wfrontend },
	{ "HEXPACK",	wfrontend },
	{ "HEXUNPK",	wfrontend },
	{ "INITWISP",	wfrontend },
	{ "INITWISP2",	wfrontend },
	{ "LBIT_OFF",	wfrontend },
	{ "LBIT_ON",	wfrontend },
	{ "LINK",	wfrontend2 },
	{ "LINKPROC",	wfrontend },
	{ "LOGOFF",	wfrontend },
	{ "MESSAGE",	wfrontend },
	{ "MWCONV",	wfrontend },
	{ "NOHELP",	wfrontend },
	{ "ONHELP",	wfrontend },
	{ "PRINT",	wfrontend },
	{ "PUTPARM",	wfrontend },
	{ "READFDR",	wfrontend },
	{ "READFDR4",	wfrontend },
	{ "READVTOC",	wfrontend },
	{ "RETCODE",	wfrontend },
	{ "SCRATCH",	wfrontend },
	{ "SCREEN",	wfrontend },
	{ "SEARCH",	wfrontend },
	{ "SET",	wfrontend },
	{ "SET8BIT",	wfrontend },
	{ "SETFILE",	wfrontend },
	{ "SETPROGID", wfrontend },
	{ "SETRETCODE", wfrontend },
	{ "SETRUNNAME", wfrontend },
	{ "SETSUBMIT",  wfrontend },
	{ "SETTRIGPROG",  wfrontend },
	{ "SETWFILEXT", wfrontend },
	{ "SETWISPFILEXT", wfrontend },
	{ "SORT",	wfrontend },
	{ "SORTCALL",	wfrontend },
	{ "SORTINFO",	wfrontend },
	{ "SORTLINK",	wfrontend },
	{ "STRING",	wfrontend },		
	{ "SUBMIT",	wfrontend },
	{ "UPDATFDR",	wfrontend },
	{ "UPPER",	wfrontend },
	{ "USEHARDLINK",wfrontend },
	{ "USESOFTLINK",wfrontend },
	{ "VEXIT",      wfrontend },
	{ "VWANG",	wfrontend },
	{ "W2ROWCOL",	wfrontend },
	{ "W4WAPI",	wfrontend },
	{ "WACCEPT",	wfrontend },
	{ "WANSI2WANG",	wfrontend },
	{ "WCHAIN",	wfrontend },
	{ "WDISPLAY",	wfrontend },
	{ "WEXITH",     wfrontend },
	{ "WFILECHK",   wfrontend },
	{ "WFILECHK2",  wfrontend },
	{ "WFNAME",     wfrontend },
	{ "WFOPEN",	wfrontend },
	{ "WFOPEN2",	wfrontend },
	{ "WFOPEN3",	wfrontend },
	{ "WFCLOSE",	wfrontend },
	{ "WFWAIT",	wfrontend },
	{ "WFSWAIT",	wfrontend },
	{ "WISPEXIT",	wfrontend },
	{ "WISPHELP",	wfrontend },
	{ "WISPPLAT",	wfrontend },
	{ "WISPSHUT",	wfrontend },
	{ "WISPSORT",	wfrontend },
	{ "WISPSYNC",	wfrontend },
	{ "WMEMCPY",	wfrontend },
	{ "WPAUSE",	wfrontend },
	{ "WRENAME",	wfrontend },
	{ "WS132",	wfrontend },
	{ "WS80",	wfrontend },
	{ "WSCREEN",	wfrontend },
	{ "WSETSTAT",	wfrontend },
	{ "WSTOP",	wfrontend },
	{ "WSXIO",	wfrontend },
	{ "WTITLE",	wfrontend },
	{ "WVASET",     wfrontend },
	{ "WWANG2ANSI",	wfrontend },
	{ "XX2BYTE",	wfrontend },
	{ "X4DBFILE",	wfrontend2 },
	{ "ISDBFILE",	wfrontend2 },
/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
	{ "WSCLOSE",	wfrontend },
	{ "WSFNM",	wfrontend },
	{ "WSFNS",	wfrontend },
#endif
/*
** The following are ACP routines 
*/
#ifdef ACP
	{ "OPENACP",	wfrontend },
	{ "CLOSEACP",	wfrontend },
	{ "READACP",	wfrontend },
	{ "WRITEACP",	wfrontend },
	{ "BREAKACP",	wfrontend },
	{ "CHECKACP",	wfrontend },
	{ "GETACP",	wfrontend },
	{ "SETACP",	wfrontend },
#endif

/*
** The following are EDE routines
*/
#ifdef EDE
	{ "A_WSLINK",	wfrontend },
	{ "EDLOAD",	wfrontend },
	{ "EDEXIT",	wfrontend },
	{ "EDCLRSCR",	wfrontend },
	{ "EDDRKSCR",	wfrontend },
	{ "EDLTESCR",	wfrontend },
	{ "EDWIDSCR",	wfrontend },
	{ "EDNARSCR",	wfrontend },
	{ "TRACEGO",	wfrontend },
	{ "TRACEEND",	wfrontend },
	{ "RETRACE",	wfrontend },
	{ "PFKEYSON",	wfrontend },
	{ "NOPFKEYS",	wfrontend },
	{ "MENUSAVE",	wfrontend },
	{ "MENUREST",	wfrontend },
	{ "MENULOAD",	wfrontend },
	{ "MENUITEM",	wfrontend },
	{ "MENUGO",	wfrontend },
	{ "MENUCONT",	wfrontend },
	{ "DYLINK",	wfrontend },
	{ "DYUNLINK",	wfrontend },
	{ "MENUKILL",	wfrontend },
	{ "MENUEXIT",	wfrontend },
	{ "MENUMODE",	wfrontend },
	{ "VIDMOVE",	wfrontend },
	{ "VIDMODE",	wfrontend },
	{ "VIDLINE",	wfrontend },
	{ "VIDTEXT",	wfrontend },
	{ "PUSHSCRN",	wfrontend },
	{ "PUSHAREA",   wfrontend },
	{ "POPAREA",	wfrontend },
	{ "MENUINFO",	wfrontend },
	{ "GCALC",	wfrontend },
	{ "GCALEND",	wfrontend },
	{ "GCLOCK",	wfrontend },
	{ "GNOTEPAD",	wfrontend },
	{ "GPUZZLE",	wfrontend },
	{ "GENVEC",	wfrontend },
#endif /* EDE */

/*
** The OLD WISP menuing system (obsolete)
*/
#ifdef ORIGMENU
	{ "MENU",	wfrontend },
#endif /* ORIGMENU */

/*
** This includes the CRID utility routines
*/
#ifdef CRID
#include "cridtbl.c"
#endif /* CRID */

/*
** Terminate with a NULL
*/
/************************************************************************/
#endif /* WISP */

	{ NULL,		NULL }
	};



/* Implementation of SYSTEM routine */

/* The following structure accesses the COLOR-MAP configuration	value	*/
/* maintained by the runtime system.  We pull the EXIT value out of 	*/
/* this table to set the default colors to be used by the SYSTEM call.	*/
/* The "w_foregrnd" and "w_backgrnd" variables are used to communicate	*/
/* the chosen colors to the window manager prior to setting the		*/
/* terminal to its standard operating mode.  */

typedef struct {
	char	foregrnd;
	char	backgrnd;
} COLORMAP;

extern	COLORMAP	colormap[19];
extern	char		w_foregrnd, w_backgrnd;

extern	int		Asystem();
extern	void		A_moveleft();


#define	EXIT_COLOR	4
#define	MAXCMD		256

int
call_system( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	char		command[ MAXCMD+1 ];
	unsigned	size;

	/* Check to see that we received reasonable parameters */

	if ( ( num_args != 1 && num_args != 2 ) || Numeric( args[0].a_type ) )
		return Halt;

	/* load USING parameter into local buffer and NULL terminate */

	size = a_size( args[0] ) > MAXCMD ? MAXCMD : a_size( args[0] );
	A_moveleft( command, args[0].a_address, size );
	command[ size ] = 0;

	/* set terminal to normal mode (unless two arguments used) */

	if ( num_args == 1 ) {
		w_foregrnd = colormap[ EXIT_COLOR ].foregrnd;
		w_backgrnd = colormap[ EXIT_COLOR ].backgrnd;
		resetunit();
	}

	/* execute command and set return code to exit status */

	return_code = Asystem( command );

	/* set terminal back to COBOL state and return */

#ifdef	ACU_ALWAYS_INIT
	w_foregrnd = w_backgrnd = 0;
	setunit();
#else
	if ( num_args == 1 ) {
		w_foregrnd = w_backgrnd = 0;
		setunit();
	}
#endif
	return Okay;

}   /* call_system */



/* wstoasc - this routine simply takes an Argument and copies to a 'C'	*/
/* string, adding a NULL terminator.  It is provided for RM/COBOL-85 	*/
/* compatibility.  */

void
wstoasc( arg, dest )
Argument	*arg;
char		*dest;
{
	register char		*src;
	register unsigned	count;

	count = (unsigned) arg->a_length;
	for( src = arg->a_address; 
			count-- && *src >= ' ' && *src <= '~'; 
			*dest++ = *src++ );
	*dest = 0;

}   /* wstoasc */


#ifdef WISP
/************************************************************************/
extern SYSTEM();
extern ACUGARGS();
extern ACUNARGS();
extern ACUPARGS();
extern BELL();
extern BITPACK();
extern BITUNPK();
extern COBLINK();
extern CANCEL();
extern CEXIT();
extern WISPDATE();
extern DATE2();
extern DATE4();
extern DAY();
extern EXTRACT();
extern FILECOPY();
extern FIND();
extern FXZONE();
extern GETPARM();
extern getparmbuild();
extern PUTPARM();
extern HEXPACK();
extern HEXUNPK();
extern LINK2();
extern LINKPROC();
extern LOGOFF();
extern MESSAGE();
extern NOHELP();
extern ONHELP();
extern PRINT();
extern RETCODE();
extern READFDR();
extern READFDR4();
extern READVTOC();
extern SCRATCH();
extern SCREEN();
extern SEARCH();
extern SET();
extern SET8BIT();
extern SETFILE();
extern SETSUBMIT();
extern SETTRIGPROG();
extern SORT();
extern SORTCALL();
extern SORTINFO();
extern SORTLINK();
extern STRING();
extern SUBMIT();
extern UPDATFDR();
extern UPPER();
extern USEHARDLINK();
extern USESOFTLINK();
extern WACCEPT();
extern WCHAIN();
extern WDISPLAY();
extern WISPEXIT();
extern WISPHELP();
extern WISPPLAT();
extern WISPSHUT();
extern WISPSYNC();
extern WISPSORT();
extern WSTOP();
extern WSXIO();
extern WTITLE();
extern wrename();
extern w2rowcol();
extern wscreen();
extern mwconv();
extern initwisp();
extern initwisp2();
extern bit_on();
extern bit_off();
extern lbit_on();
extern lbit_off();
extern bit_test();
extern wfname();
extern wfopen();
extern wfopen2();
extern wfopen3();
extern wfclose();
extern wfwait();
extern wfswait();
extern wmemcpy();
extern wvaset();
extern setwfilext();
extern getwfilext();
extern setretcode();
extern wexith();
extern vexit();
extern wfilechk();
extern wfilechk2();
extern vwang();
extern greclen();
extern setrunname();
extern setwispfilext();
extern setprogid();
extern wpause();
extern wsetstat();
extern xx2byte();
extern x4dbfile();
extern ISDBFILE();
extern ws80();
extern ws132();
extern WANSI2WANG();
extern WWANG2ANSI();
extern W4WAPI();

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
extern WSCLOSE();
extern WSFNM();
extern WSFNS();
#endif

/*
** The following are ACP routines
*/
#ifdef ACP
extern OPENACP();
extern CLOSEACP();
extern READACP();
extern WRITEACP();
extern BREAKACP();
extern CHECKACP();
extern GETACP();
extern SETACP();
#endif

/*
** The following are EDE routines
*/
#ifdef EDE
extern ws_bar_menu();
extern gen_ncpfkey();
extern nc_pop_menu();
extern A_WSLINK();
extern EDLOAD();
extern EDEXIT();
extern EDCLRSCR();
extern EDDRKSCR();
extern EDLTESCR();
extern EDWIDSCR();
extern EDNARSCR();
extern TRACEGO();
extern TRACEEND();
extern RETRACE();
extern PFKEYSON();
extern NOPFKEYS();
extern MENUSAVE();
extern MENUREST();
extern MENULOAD();
extern MENUITEM();
extern MENUGO();
extern MENUCONT();
extern DYLINK();
extern DYUNLINK();
extern MENUKILL();
extern MENUEXIT();
extern MENUMODE();
extern VIDMOVE();
extern VIDMODE();
extern VIDLINE();
extern VIDTEXT();
extern PUSHSCRN();
extern PUSHAREA();
extern POPAREA();
extern MENUINFO();
extern gcalc();
extern gcalend();
extern gclock();
extern gnotepad();
extern gpuzzle();
extern GENVEC();
#endif /* EDE */

#ifdef ORIGMENU
extern menu();
#endif /* ORIGMENU */

/*
	The first entry is UPPERCASE.
	The second is case sensitive.
*/
struct	PROCTABLE WISPTABLE[] = {
	{ "ACUGARGS",	ACUGARGS },
	{ "ACUNARGS",	ACUNARGS },
	{ "ACUPARGS",	ACUPARGS },
	{ "BELL",	BELL },
	{ "BITPACK",	BITPACK },
	{ "BITUNPK",	BITUNPK },
	{ "BIT_TEST",	bit_test },
	{ "BIT_ON",	bit_on },
	{ "BIT_OFF",	bit_off }, 
	{ "CANCEL",	CANCEL },	
	{ "CEXIT",	CEXIT },	
	{ "COBLINK",	COBLINK },	
	{ "DATE",	WISPDATE },
	{ "DATE2",	DATE2 },
	{ "DATE4",	DATE4 },
	{ "DAY",	DAY },
	{ "EXTRACT",	EXTRACT },	
	{ "FILECOPY",	FILECOPY },
	{ "FIND",	FIND },
	{ "FXZONE",	FXZONE },
	{ "GETPARM",	GETPARM },
	{ "GETPARMBUILD",getparmbuild },
	{ "HEXPACK",	HEXPACK },
	{ "HEXUNPK",	HEXUNPK },
	{ "LINK",	LINK2 },	/* NOTE: Translate LINK ==> LINK2 */
	{ "LINKPROC",	LINKPROC },
	{ "LOGOFF",	LOGOFF },
	{ "MESSAGE",	MESSAGE },
	{ "MWCONV",	mwconv },
	{ "NOHELP",	NOHELP },
	{ "ONHELP",	ONHELP },
	{ "PRINT",	PRINT },
	{ "PUTPARM",	PUTPARM },
	{ "READFDR",	READFDR },
	{ "READFDR4",	READFDR4 },
	{ "READVTOC",	READVTOC },
	{ "RETCODE",	RETCODE },
	{ "SCRATCH",	SCRATCH },
	{ "SCREEN",	SCREEN },
	{ "SEARCH",	SEARCH },
	{ "SET",	SET },
	{ "SET8BIT",	SET8BIT },
	{ "SETFILE",	SETFILE},
	{ "SETSUBMIT",	SETSUBMIT },
	{ "SETTRIGPROG",SETTRIGPROG },
	{ "SORT",	SORT },
	{ "SORTCALL",	SORTCALL },
	{ "SORTINFO",	SORTINFO },
	{ "SORTLINK",	SORTLINK },
	{ "STRING",	STRING },
	{ "SUBMIT",	SUBMIT },
	{ "UPDATFDR",	UPDATFDR },
	{ "WISPEXIT",	WISPEXIT },
	{ "WISPHELP",	WISPHELP },
	{ "WISPPLAT",	WISPPLAT },
	{ "WISPSHUT",	WISPSHUT },
	{ "WISPSORT",	WISPSORT },
	{ "WISPSYNC",	WISPSYNC },
	{ "WS132",	ws132 },
	{ "WS80",	ws80 },
	{ "WSTOP",	WSTOP },
	{ "WSXIO",	WSXIO },
	{ "WTITLE",	WTITLE },
	{ "WRENAME",	wrename },
	{ "INITWISP",	initwisp },
	{ "INITWISP2",	initwisp2 },
	{ "LBIT_ON",	lbit_on },
	{ "LBIT_OFF",	lbit_off },
	{ "MWCONV",	mwconv },
	{ "UPPER",	UPPER },
	{ "USEHARDLINK",USEHARDLINK },
	{ "USESOFTLINK",USESOFTLINK },
	{ "WACCEPT",	WACCEPT },
	{ "WFILECHK",   wfilechk },
	{ "WFILECHK2",  wfilechk2 },
	{ "WFOPEN",	wfopen },
	{ "WFOPEN2",	wfopen2 },
	{ "WFOPEN3",	wfopen3 },
	{ "WFCLOSE",	wfclose },
	{ "WCHAIN",	WCHAIN },
	{ "WDISPLAY",	WDISPLAY },
	{ "WFNAME",     wfname },
	{ "WFWAIT",	wfwait },
	{ "WVASET", 	wvaset },
	{ "W2ROWCOL",	w2rowcol },
	{ "WSCREEN",	wscreen },
	{ "WSETSTAT",	wsetstat },
	{ "SETWFILEXT", setwfilext },
	{ "GETWFILEXT", getwfilext },
	{ "SETRUNNAME", setrunname },
	{ "SETWISPFILEXT", setwispfilext},
	{ "GRECLEN",    greclen },
	{ "SETPROGID",  setprogid },
	{ "SETRETCODE", setretcode },
	{ "WEXITH",     wexith },
	{ "WFSWAIT",    wfswait },
	{ "WMEMCPY",    wmemcpy },
	{ "WPAUSE",     wpause },
	{ "WANSI2WANG", WANSI2WANG },	
	{ "WWANG2ANSI", WWANG2ANSI },	
	{ "VEXIT",      vexit },
	{ "VWANG",	vwang },
	{ "XX2BYTE",	xx2byte },
	{ "X4DBFILE",	x4dbfile },
	{ "ISDBFILE",	ISDBFILE },
	{ "W4WAPI",	W4WAPI },

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
	{ "OPENACP",	OPENACP  },
	{ "CLOSEACP",	CLOSEACP },
	{ "READACP",	READACP  },
	{ "WRITEACP",	WRITEACP },
	{ "BREAKACP",	BREAKACP },
	{ "CHECKACP",	CHECKACP },
	{ "GETACP",	GETACP   },
	{ "SETACP",	SETACP   },
#endif
/*
** The following are EDE routines
*/
#ifdef EDE
	{ "WS_BAR_MENU",ws_bar_menu},
	{ "GEN_NCPFKEY",gen_ncpfkey},
	{ "NC_POP_MENU",nc_pop_menu},
	{ "A_WSLINK",	A_WSLINK },
	{ "EDLOAD",	EDLOAD 	},
	{ "EDEXIT",	EDEXIT 	},
	{ "EDCLRSCR",	EDCLRSCR },
	{ "EDDRKSCR",	EDDRKSCR },
	{ "EDLTESCR",	EDLTESCR },
	{ "EDWIDSCR",	EDWIDSCR },
	{ "EDNARSCR",	EDNARSCR },
	{ "TRACEGO",	TRACEGO },
	{ "TRACEEND",	TRACEEND },
	{ "RETRACE",	RETRACE },
	{ "PFKEYSON",	PFKEYSON },
	{ "NOPFKEYS",	NOPFKEYS },
	{ "MENUSAVE",	MENUSAVE },
	{ "MENUREST",	MENUREST },
	{ "MENULOAD",	MENULOAD },
	{ "MENUITEM",	MENUITEM },
	{ "MENUGO",	MENUGO 	},
	{ "MENUCONT",	MENUCONT },
	{ "DYLINK",	DYLINK 	},
	{ "DYUNLINK",	DYUNLINK },
	{ "MENUKILL",	MENUKILL },
	{ "MENUEXIT",	MENUEXIT },
	{ "MENUMODE",	MENUMODE },
	{ "VIDMOVE",	VIDMOVE },
	{ "VIDMODE",	VIDMODE },
	{ "VIDLINE",	VIDLINE },
	{ "VIDTEXT",	VIDTEXT },
	{ "PUSHSCRN",	PUSHSCRN },
	{ "PUSHAREA",	PUSHAREA },
	{ "POPAREA",	POPAREA },
	{ "MENUINFO",	MENUINFO },
	{ "GCALC",	gcalc },
	{ "GCALEND",	gcalend },
	{ "GCLOCK",	gclock },
	{ "GNOTEPAD",	gnotepad },
	{ "GPUZZLE",	gpuzzle },
	{ "GENVEC",	GENVEC },
#endif /* EDE */
/*
** The OLD wisp menu system (obsolete)
*/
#ifdef ORIGMENU
	{ "MENU", 	menu },
#endif /* ORIGMENU */

/*
** Terminate with a NULL
*/
	{ NULL,		NULL }
};


wfrontend( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
#define MAXARGS 128
	int i;
	int (*fn)();
	struct PROCTABLE *p;

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
		fprintf(stderr,"\n\r\n\r sub85.c MAXARGS exceeded [num_args = %d] \n\r\n\r",num_args);
		num_args = MAXARGS;
	}

	wvaset(&num_args);

	if (num_args > 0)
	{
		long	tempbin[MAXARGS];					/* Place to store binary values to reverse bytes*/
		void 	*arglist[MAXARGS]; 

		memset((char *)arglist, '\0', sizeof(arglist));

		for (i=0; i < num_args; ++i)
		{
			if ( !bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
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

		if ( !bytenormal() )
		{
			for (i=0; i < num_args; ++i)
			{
				if ( args[i].a_type == BinarySigned ||
				     args[i].a_type == BinaryUnsigned )
				{
					reversebytes(&tempbin[i], args[i].a_length);		/* Undo the byte reverse	*/
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
	wfrontend2 - Acts just like wfrontend except it puts both the address and the length on the stack. So for each arg
		     there 2 entries on the stack.
*/	
wfrontend2( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
#define MAXARGS2 64
	int i,l;
	int (*fn)();
	struct PROCTABLE *p;

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
		fprintf(stderr,"\n\r\n\r sub85.c MAXARGS2 exceeded [num_args = %d] \n\r\n\r",num_args);
		num_args = MAXARGS2;
	}

	wvaset(&num_args);

	if (num_args > 0)
	{
		long	tempbin[MAXARGS2];					/* Place to store binary values to reverse bytes*/
		void 	*arglist[MAXARGS2*2]; 

		memset((char *)arglist, '\0', sizeof(arglist));

		for (i=0,l=0; i < num_args; ++i)
		{
			if ( !bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
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


		if ( !bytenormal() )
		{
			for (i=0; i < num_args; ++i)
			{
				if ( args[i].a_type == BinarySigned ||
				     args[i].a_type == BinaryUnsigned )
				{
					reversebytes(&tempbin[i], args[i].a_length);		/* Undo the byte reverse	*/
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
**	Routine:	call_acucobol()
**
**	Function:	To "call" an Acucobol COBOL routine from 'C'.
**
**	Description:	This routine builds the correct calling sequence to call
**			an Acucobol COBOL routine from 'C'.
**			Currently this is used for the MSDOS implementation of LINK.
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
**	History:	
**	12/22/92	Written by GSL
**
*/
void call_acucobol( filespec, parmcnt, parms, lens, rc )
char *filespec;
int4 parmcnt;
char *parms[];
int4 lens[];
int *rc;
{
#define MAX_CALL_ARGS	32
extern	short A_call_err;

#ifndef A_COBOL_DECLARED
/* In 4.3 Acucobol started declaring cobol() in sub.h */
extern	int cobol();
#endif /* A_COBOL_DECLARED */

#ifdef WIN32
#ifdef ACUCANCEL42
extern void acu_cancel(char *);
#else
extern void PASCAL acu_cancel(char *);
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

	if (cobol(filespec, argcnt, args))
	{
		*rc = (int)A_call_err;
	}

	/*
	**	Cancel the program
	*/
	acu_cancel(filespec);

	return;
}

static int wisp_wexit = 0;

void shutexitcobol(exit_code)					/* Called by wisp from wexit()  */
int exit_code;
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

void start_cobol_screen_handler()
{
	w_set_term();
}

void shutdown_cobol_screen_handler()
{
	w_reset_term();
}

/*
	AShutdown is called by ACUCOBOL at shutdown.
	If error_halt==0 then a normal STOP RUN otherwise error.

	Shutdown() was renamed to AShutdown() for version 2.4.x
*/
void
AShutdown( error_halt )
int	error_halt;
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
	**	If we are not already processing wexit() then go thur the WISP exit logic.
	*/
	if (0 == wisp_wexit)
	{
		if (!nativescreens())
		{
			werrlog(102,"Terminating on an error detected by ACUCOBOL.",0,0,0,0,0,0,0);
#ifdef unix
			werrlog(102,"Use the -e runtime option to capture the error message.",0,0,0,0,0,0,0);
#endif
		}
		
		wisp_wexit = 1;

		wexit(error_halt);
	}
}
void Shutdown(error_halt)
int error_halt;
{
	AShutdown(error_halt);
}

#define Shutdown OLD_Shutdown
#define AShutdown OLD_Shutdown

int
exam_args( argc, argv )
int argc;
char *argv[];
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
		set_isdebug_true();
	}
	else
	{
		set_isdebug_false();
	}
	return 0;
}

#define exam_args OLD_exam_args

/*
**	Routine:	ISDBFILE()
**
**	Function:	Check if a file is a Database file
**
**	Description:	This routine uses Agetenv() to check the ACUCONFIG file
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
int ISDBFILE(select_name, l1, answer, l2)
char *select_name;
int4 l1;
char *answer;
int4 l2;
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
		if (ptr = Agetenv("DEFAULT_HOST"))
		{
			strcpy(default_host,ptr);
			upper_string(default_host);
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
	upper_string(test_host);

	if (ptr = Agetenv(test_host))
	{
		strcpy(buff,ptr);
		upper_string(buff);
	}
	else
	{
		strcpy(buff,default_host);
	}

	if (	(0 == strcmp(buff,"INFORMIX"))	||
		(0 == strcmp(buff,"ORACLE"))	||
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
**	Routine:	x4dbfile()
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
int x4dbfile(select_name, l1, select_status, l2)
char *select_name;
int4 l1;
int *select_status;
int4 l2;
{
	char	answer;
	int4	one = 1;

	ISDBFILE(select_name, l1, &answer, one);

	if ('Y' == answer)
	{
		setdbfile(select_status,1);
	}
	else
	{
		setdbfile(select_status,0);
	}
	return 0;
}

/*
** Include CRID interface routines
*/
#ifdef CRID
#include "crid85.c"

/*
 *	Acucobol 5.0 does not have v_make_vers which is needed for
 *	older CRID.
 */
#ifdef CRID_V_MAKE_VERS
short v_make_vers;
#endif /* CRID_V_MAKE_VERS */
#endif /* CRID */

/************************************************************************/
#endif /* WISP */


