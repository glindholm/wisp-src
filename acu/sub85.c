/*
	sub85.c		This is the WISP compatable version of sub85.c.
			All of the WISP added code is enclosed in
			"#ifdef WISP" statements.

	ACUCOBOL VERSION 2.0
	The "system" call routine has been changed by Acucobol to
	"Asystem" it was "system". To make this sub85.c compatable
	for both 2.0 and 1.5 we have surrounded the "Asystem" call
	in "#ifdef ACUCOBOL2". To use the 2.0 version you need to
	add the line "#define ACUCOBOL2".
*/
#define WISP

/* sub85.c - RM/COBOL-85 compatible 'C' routine interface */

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
/*
	To reduce the size of the RTS you can remove any of the following
	"#define" statements and the corresponding files will not be
	included.

		ACP	 The Wang ACP routines.
		NETCAP	 The Netron Cap routines.
		WISPTEST Wisp test routines.  (by default they are not included)
		EDE	 The EDE routines.
		CRID	 Control Report Inquiry Datentry (by default they are not included) 
*/

#define ACP
#define NETCAP
#define EDE


char	WISPFILEXT[39];			/* Define the file extension variable.	*/
char	WISPRETURNCODE[3];		/* Define the return code field.	*/


#ifndef DGUX
#ifdef m88k
int __stdinb;
int __stderrb;
int __stdoutb;
#endif /* m88k */
#endif /* DGUX */
   
extern int va_set();

int 	wfrontend();
int	wfrontend2();

/*
**  Include the CRID header files
*/
#ifdef CRID
#include "crid.h"
#endif /* CRID */

#endif /* WISP */

/*
	The first entry must be UPPERCASE.
*/
int	call_system();
struct	PROCTABLE LIBTABLE[] = {
	{ "SYSTEM", 	call_system },
#ifdef WISP
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
	{ "DAY",	wfrontend },
	{ "EXTRACT",	wfrontend },
	{ "FILECOPY",	wfrontend },
	{ "FIND",	wfrontend },
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
	{ "MENU",	wfrontend },
	{ "MESSAGE",	wfrontend },
	{ "MWCONV",	wfrontend },
	{ "NOHELP",	wfrontend },
	{ "ONHELP",	wfrontend },
	{ "PRINT",	wfrontend },
	{ "PUTPARM",	wfrontend },
	{ "READFDR",	wfrontend },
	{ "READVTOC",	wfrontend },
	{ "RETCODE",	wfrontend },
	{ "SCRATCH",	wfrontend },
	{ "SCREEN",	wfrontend },
	{ "SEARCH",	wfrontend },
	{ "SET",	wfrontend },
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
	{ "VEXIT",      wfrontend },
	{ "VWANG",	wfrontend },
	{ "WACCEPT",	wfrontend },
	{ "WCHAIN",	wfrontend },
	{ "WDISPLAY",	wfrontend },
	{ "WDINIT",	wfrontend },
	{ "WDFINISH",	wfrontend },	
	{ "WEXITH",     wfrontend },
	{ "WFILECHK",   wfrontend },
	{ "WFNAME",     wfrontend },
	{ "WFOPEN",	wfrontend },
	{ "WFOPEN2",	wfrontend },
	{ "WFOPEN3",	wfrontend },
	{ "WFCLOSE",	wfrontend },
	{ "WFWAIT",	wfrontend },
	{ "WFSWAIT",	wfrontend },
	{ "WISPEXIT",	wfrontend },
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
	{ "WSXIO",	wfrontend },
	{ "W2ROWCOL",	wfrontend },
	{ "WVASET",     wfrontend },
	{ "XX2BYTE",	wfrontend },
/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
	{ "WSCLOSE",	wfrontend },
	{ "WSFNM",	wfrontend },
	{ "WSFNS",	wfrontend },
#endif
/*
** The following are TEST routines
*/
#ifdef WISPTEST
	{ "MESSUP",	wfrontend },
	{ "QASCROLL",	wfrontend },
	{ "QATSTLST",	wfrontend },
	{ "QAGP255",	wfrontend },
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
** This includes the CRID utility routines
*/
#ifdef CRID
#include "cridtbl.c"
#endif /* CRID */

/*
** Terminate with a NULL
*/
#endif /* WISP */
	{ NULL,		NULL }
	};

#ifdef WISP
 extern ACUGARGS();
 extern ACUNARGS();
 extern ACUPARGS();
 extern SYSTEM();
 extern BELL();
 extern BITPACK();
 extern BITUNPK();
 extern COBLINK();
 extern CANCEL();
 extern CEXIT();
 extern DATE();
 extern DAY();
 extern EXTRACT();
 extern FILECOPY();
 extern FIND();
 extern GETPARM();
 extern getparmbuild();
 extern PUTPARM();
 extern HEXPACK();
 extern HEXUNPK();
 extern LINK();
 extern LINKPROC();
 extern LOGOFF();
 extern MESSAGE();
 extern NOHELP();
 extern ONHELP();
 extern PRINT();
 extern RETCODE();
 extern READFDR();
 extern READVTOC();
 extern SCRATCH();
 extern SCREEN();
 extern SEARCH();
 extern SET();
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
 extern WACCEPT();
 extern WCHAIN();
 extern WDISPLAY();
 extern WISPEXIT();
 extern WISPSHUT();
 extern WISPSYNC();
 extern WISPSORT();
 extern WSXIO();
 extern menu();
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
 extern wdinit();
 extern wdfinish();
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
 extern vwang();
 extern greclen();
 extern setrunname();
 extern setwispfilext();
 extern setprogid();
 extern wpause();
 extern wsetstat();
 extern xx2byte();
 extern ws80();
 extern ws132();

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
 extern WSCLOSE();
 extern WSFNM();
 extern WSFNS();
#endif
/*
** The following are WISPTEST routines
*/
#ifdef WISPTEST
 extern MESSUP();
 extern QASCROLL();
 extern QATSTLST();
 extern QAGP255();
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
	{ "DATE",	DATE },
	{ "DAY",	DAY },
	{ "EXTRACT",	EXTRACT },	
	{ "FILECOPY",	FILECOPY },
	{ "FIND",	FIND },
	{ "GETPARM",	GETPARM },
	{ "GETPARMBUILD",getparmbuild },
	{ "HEXPACK",	HEXPACK },
	{ "HEXUNPK",	HEXUNPK },
	{ "LINK",	LINK },
	{ "LINKPROC",	LINKPROC },
	{ "LOGOFF",	LOGOFF },
	{ "MENU", 	menu },
	{ "MESSAGE",	MESSAGE },
	{ "MWCONV",	mwconv },
	{ "NOHELP",	NOHELP },
	{ "ONHELP",	ONHELP },
	{ "PRINT",	PRINT },
	{ "PUTPARM",	PUTPARM },
	{ "READFDR",	READFDR },
	{ "READVTOC",	READVTOC },
	{ "RETCODE",	RETCODE },
	{ "SCRATCH",	SCRATCH },
	{ "SCREEN",	SCREEN },
	{ "SEARCH",	SEARCH },
	{ "SET",	SET },
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
	{ "WISPSHUT",	WISPSHUT },
	{ "WISPSORT",	WISPSORT },
	{ "WISPSYNC",	WISPSYNC },
	{ "WS132",	ws132 },
	{ "WS80",	ws80 },
	{ "WSXIO",	WSXIO },
	{ "WRENAME",	wrename },
	{ "INITWISP",	initwisp },
	{ "INITWISP2",	initwisp2 },
	{ "LBIT_ON",	lbit_on },
	{ "LBIT_OFF",	lbit_off },
	{ "MWCONV",	mwconv },
	{ "UPPER",	UPPER },
	{ "WACCEPT",	WACCEPT },
	{ "WFILECHK",   wfilechk },
	{ "WFOPEN",	wfopen },
	{ "WFOPEN2",	wfopen2 },
	{ "WFOPEN3",	wfopen3 },
	{ "WFCLOSE",	wfclose },
	{ "WCHAIN",	WCHAIN },
	{ "WDISPLAY",	WDISPLAY },
	{ "WDINIT",	wdinit },
	{ "WDFINISH",	wdfinish },
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
	{ "VEXIT",      vexit },
	{ "VWANG",	vwang },
	{ "XX2BYTE",	xx2byte },
/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
	{ "WSCLOSE",	WSCLOSE },
	{ "WSFNM",	WSFNM },
	{ "WSFNS",	WSFNS },
#endif
/*
** The following are  WISPTEST routines
*/
#ifdef WISPTEST
	{ "MESSUP",	MESSUP },
	{ "QASCROLL",	QASCROLL },
	{ "QATSTLST",	QATSTLST },
	{ "QAGP255",	QAGP255 },
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
** Terminate with a NULL
*/
	{ NULL,		NULL }
};

#define MAXARGS 128

struct 
{ 
	char *arglist[MAXARGS]; 
} argtable;

#define MAXARGS2 64

struct 
{ 
	char *arglist[MAXARGS2*2]; 
} argtable2;

#endif /* WISP */



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

extern	COLORMAP	colormap[];
extern	char		w_foregrnd, w_backgrnd;

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

#ifdef ACUCOBOL2
	/* execute command and set return code to exit status */

	return_code = Asystem( command );
#else
	/* execute command */

	system( command );
#endif

	/* set terminal back to COBOL state and return */

	if ( num_args == 1 ) {
		w_foregrnd = w_backgrnd = 0;
		setunit();
	}
	return Okay;

}   /* call_system */



/* wstoasc - this routine simply takes an Argument and copies to a 'C'	*/
/* string, adding a NULL terminator.  It is provided for RM/COBOL 	*/
/* compatibility.  */

wstoasc( arg, dest )
Argument	*arg;
char		*dest;
{
	register char		*src;
	register unsigned	count;

	count = arg->a_length;
	for( src = arg->a_address; 
			count-- && *src >= ' ' && *src <= '~'; 
			*dest++ = *src++ );
	*dest = 0;

}   /* wstoasc */



#ifdef WISP

wfrontend( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	char **argp;
	int i,l,j;
	int (*fn)();
	struct PROCTABLE *p;

	for (p = WISPTABLE; strcmp(p->name,name); ++p);
	if (!strcmp(p->name,name))
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

	if (num_args)
	{
		long	tempbin[MAXARGS];					/* Place to store binary values to reverse bytes*/

		for (i=0; i < num_args; ++i)
		{
			if ( !bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
				argtable.arglist[i] = (char *)&tempbin[i];			/* Put addr of temp in argtable	*/
			}
			else
			{
				argtable.arglist[i] = args[i].a_address;
			}
		}
#ifdef NOTUSED
		(*fn)(argtable);	/* This doesn't work on all systems */
#endif
		(*fn)(
		      argtable.arglist[0],      argtable.arglist[1],	argtable.arglist[2],      argtable.arglist[3],
		      argtable.arglist[4],      argtable.arglist[5],    argtable.arglist[6],      argtable.arglist[7],
		      argtable.arglist[8],      argtable.arglist[9],  	argtable.arglist[10],     argtable.arglist[11],
		      argtable.arglist[12],     argtable.arglist[13],   argtable.arglist[14],     argtable.arglist[15],
		      argtable.arglist[16],     argtable.arglist[17],   argtable.arglist[18],     argtable.arglist[19],
		      argtable.arglist[20],     argtable.arglist[21],   argtable.arglist[22],     argtable.arglist[23],
		      argtable.arglist[24],     argtable.arglist[25],	argtable.arglist[26],     argtable.arglist[27],
		      argtable.arglist[28],     argtable.arglist[29],	argtable.arglist[30],     argtable.arglist[31],
		      argtable.arglist[32],     argtable.arglist[33],	argtable.arglist[34],     argtable.arglist[35],
		      argtable.arglist[36],     argtable.arglist[37],	argtable.arglist[38],     argtable.arglist[39],
		      argtable.arglist[40],     argtable.arglist[41],	argtable.arglist[42],     argtable.arglist[43],
		      argtable.arglist[44],     argtable.arglist[45],	argtable.arglist[46],     argtable.arglist[47],
		      argtable.arglist[48],     argtable.arglist[49],	argtable.arglist[50],     argtable.arglist[51],
		      argtable.arglist[52],     argtable.arglist[53],	argtable.arglist[54],     argtable.arglist[55],
		      argtable.arglist[56],     argtable.arglist[57],	argtable.arglist[58],     argtable.arglist[59],
		      argtable.arglist[60],     argtable.arglist[61],	argtable.arglist[62],     argtable.arglist[63],
		      argtable.arglist[64],     argtable.arglist[65],	argtable.arglist[66],     argtable.arglist[67],
		      argtable.arglist[68],     argtable.arglist[69],	argtable.arglist[70],     argtable.arglist[71],
		      argtable.arglist[72],     argtable.arglist[73],	argtable.arglist[74],     argtable.arglist[75],
		      argtable.arglist[76],     argtable.arglist[77],	argtable.arglist[78],     argtable.arglist[79],
		      argtable.arglist[80],     argtable.arglist[81],	argtable.arglist[82],     argtable.arglist[83],
		      argtable.arglist[84],     argtable.arglist[85],	argtable.arglist[86],     argtable.arglist[87],
		      argtable.arglist[88],	argtable.arglist[89],	argtable.arglist[90],     argtable.arglist[91],
		      argtable.arglist[92],     argtable.arglist[93],	argtable.arglist[94],     argtable.arglist[95],
		      argtable.arglist[96],     argtable.arglist[97],	argtable.arglist[98],     argtable.arglist[99],
		      argtable.arglist[100],    argtable.arglist[101],	argtable.arglist[102],    argtable.arglist[103],
		      argtable.arglist[104],    argtable.arglist[105],	argtable.arglist[106],    argtable.arglist[107],
		      argtable.arglist[108],    argtable.arglist[109],	argtable.arglist[110],    argtable.arglist[111],
		      argtable.arglist[112],    argtable.arglist[113],	argtable.arglist[114],    argtable.arglist[115],
		      argtable.arglist[116],    argtable.arglist[117],	argtable.arglist[118],    argtable.arglist[119],
		      argtable.arglist[120],    argtable.arglist[121],	argtable.arglist[122],    argtable.arglist[123],
		      argtable.arglist[124],    argtable.arglist[125],	argtable.arglist[126],    argtable.arglist[127]);

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
	char **argp;
	int i,l,j;
	int (*fn)();
	struct PROCTABLE *p;

	for (p = WISPTABLE; strcmp(p->name,name); ++p);
	if (!strcmp(p->name,name))
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

	if (num_args)
	{
		long	tempbin[MAXARGS2];					/* Place to store binary values to reverse bytes*/

		for (i=0,l=0; i < num_args; ++i)
		{
			if ( !bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
				argtable2.arglist[l++] = (char *)&tempbin[i];			/* Put addr of temp in argtable	*/
			}
			else
			{
				argtable2.arglist[l++] = args[i].a_address; 
			}

			argtable2.arglist[l++] = (char *) args[i].a_length;		/* Put the length on the stack		*/
		}

#ifdef NOTUSED
		(*fn)(argtable2);	/* This doesn't work on all systems */
#endif
		(*fn)(
		      argtable2.arglist[0],	argtable2.arglist[1],	argtable2.arglist[2],      argtable2.arglist[3],
		      argtable2.arglist[4],	argtable2.arglist[5],   argtable2.arglist[6],      argtable2.arglist[7],
		      argtable2.arglist[8],	argtable2.arglist[9],   argtable2.arglist[10],     argtable2.arglist[11],
		      argtable2.arglist[12],	argtable2.arglist[13],  argtable2.arglist[14],     argtable2.arglist[15],
		      argtable2.arglist[16],	argtable2.arglist[17],  argtable2.arglist[18],     argtable2.arglist[19],
		      argtable2.arglist[20],	argtable2.arglist[21],  argtable2.arglist[22],     argtable2.arglist[23],
		      argtable2.arglist[24],	argtable2.arglist[25],	argtable2.arglist[26],     argtable2.arglist[27],
		      argtable2.arglist[28],	argtable2.arglist[29],	argtable2.arglist[30],     argtable2.arglist[31],
		      argtable2.arglist[32],	argtable2.arglist[33],	argtable2.arglist[34],     argtable2.arglist[35],
		      argtable2.arglist[36],	argtable2.arglist[37],	argtable2.arglist[38],     argtable2.arglist[39],
		      argtable2.arglist[40],	argtable2.arglist[41],	argtable2.arglist[42],     argtable2.arglist[43],
		      argtable2.arglist[44],	argtable2.arglist[45],	argtable2.arglist[46],     argtable2.arglist[47],
		      argtable2.arglist[48],	argtable2.arglist[49],	argtable2.arglist[50],     argtable2.arglist[51],
		      argtable2.arglist[52],	argtable2.arglist[53],	argtable2.arglist[54],     argtable2.arglist[55],
		      argtable2.arglist[56],	argtable2.arglist[57],	argtable2.arglist[58],     argtable2.arglist[59],
		      argtable2.arglist[60],	argtable2.arglist[61],	argtable2.arglist[62],     argtable2.arglist[63],
		      argtable2.arglist[64],	argtable2.arglist[65],	argtable2.arglist[66],     argtable2.arglist[67],
		      argtable2.arglist[68],	argtable2.arglist[69],	argtable2.arglist[70],     argtable2.arglist[71],
		      argtable2.arglist[72],	argtable2.arglist[73],	argtable2.arglist[74],     argtable2.arglist[75],
		      argtable2.arglist[76],	argtable2.arglist[77],	argtable2.arglist[78],     argtable2.arglist[79],
		      argtable2.arglist[80],	argtable2.arglist[81],	argtable2.arglist[82],     argtable2.arglist[83],
		      argtable2.arglist[84],	argtable2.arglist[85],	argtable2.arglist[86],     argtable2.arglist[87],
		      argtable2.arglist[88],	argtable2.arglist[89],	argtable2.arglist[90],     argtable2.arglist[91],
		      argtable2.arglist[92],	argtable2.arglist[93],	argtable2.arglist[94],     argtable2.arglist[95],
		      argtable2.arglist[96],	argtable2.arglist[97],	argtable2.arglist[98],     argtable2.arglist[99],
		      argtable2.arglist[100],	argtable2.arglist[101],	argtable2.arglist[102],    argtable2.arglist[103],
		      argtable2.arglist[104],	argtable2.arglist[105],	argtable2.arglist[106],    argtable2.arglist[107],
		      argtable2.arglist[108],	argtable2.arglist[109],	argtable2.arglist[110],    argtable2.arglist[111],
		      argtable2.arglist[112],	argtable2.arglist[113],	argtable2.arglist[114],    argtable2.arglist[115],
		      argtable2.arglist[116],	argtable2.arglist[117],	argtable2.arglist[118],    argtable2.arglist[119],
		      argtable2.arglist[120],	argtable2.arglist[121],	argtable2.arglist[122],    argtable2.arglist[123],
		      argtable2.arglist[124],	argtable2.arglist[125],	argtable2.arglist[126],    argtable2.arglist[127]);


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
		(*fn)();
	}	

	return 0;
}

static int wisp_wexit = 0;

shutexitcobol(exit_code)					/* Called by wisp from wexit()  */
int exit_code;
{
	wisp_wexit = 1;
	stop_runtime(exit_code,0,0,0,0,0);			/* Acucobol routine to shutdown */
}

/*
	Shutdown is called by ACUCOBOL at shutdown.
	If error_halt==0 then a normal STOP RUN otherwise error.
*/
void
Shutdown( error_halt )
int	error_halt;
{
	if (error_halt && !wisp_wexit)
	{
		werrlog(102,"Terminating on an ACUCOBOL error",0,0,0,0,0,0,0);
		wexit(error_halt);
	}
	return;
}

#define Shutdown OLD_Shutdown

/*
** Include CRID interface routines
*/
#ifdef CRID
#include "crid85.c"
#endif /* CRID */

#endif /* WISP */


