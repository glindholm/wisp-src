/* 
	Copyright (c) 1996 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

/*
	wrunmf:		This routine is used to build a Micro Focus COBOL/2 
			Run Time System (RTS or RTE).

*/

/*
	To reduce the size of the RTS you can remove any of the following
	"#define" statements and the corresponding files will not be
	included.

		ACP	 The Wang ACP routines.
		NETCAP	 The Netron Cap routines.
		EDE	 The EDE routines.
		W4W	 The WISP FOR WINDOWS routines.
*/

#define ACP
#define NETCAP
#define EDE
#define W4W


/*
	Wrunmf() is a dummy routine which is never called.
	It exists to force the linker (ld) to include the
	listed routines into the RTS.
*/

int wrunmf_dummy = 1;

wrunmf()
{
	if (wrunmf_dummy) return;

/*
	The following are a list of all "C" routines that could be called from 
	a WISPed COBOL program.

	To add additional routines to the the RTS you can follow this example 
	and add a reference to the routine.
*/

	ACUGARGS();
	ACUNARGS();
	ACUPARGS();
	BELL();
	BITPACK();
	BITUNPK();
	CANCEL();
	CEXIT();
	COBLINK();
	DATE();
	DATE2();
	DAY();
	EXTRACT();
	FILECOPY();
	FIND();
	FXZONE();
	GETPARM();
	getparmbuild();
	HEXPACK();
	HEXUNPK();
	LINK();
	LINKGARG();
	LINKMF();
	LINKPROC();
	LOGOFF();
	MESSAGE();
	NOHELP();
	ONHELP();
	PRINT();
	PUTPARM();
	RETCODE();
	READFDR();
	READVTOC();
	SCRATCH();
	SCREEN();
	SEARCH();
	SET();
	SETFILE();
	SETSUBMIT();
	SETTRIGPROG();
	SET8BIT();
	SORT();
	SORTCALL();
	SORTINFO();
	SORTLINK();
	STRING();
	SUBMIT();
	UPDATFDR();
	UPPER();
	USEHARDLINK();
	USESOFTLINK();
	WACCEPT();
	WCHAIN();
	WDISPLAY();
	WISPEXIT();
	WISPPLAT();
	WISPSHUT();
	WISPSYNC();
	WISPSORT();
	WSTOP();
	WSXIO();
	wrename();
	w2rowcol();
	wscreen();
	mwconv();
	initwisp();
	initwisp2();
	bit_on();
	bit_off();
	lbit_on();
	lbit_off();
	bit_test();
	lbit_test();
	wfname();
	wfopen();
	wfopen2();
	wfopen3();
	wfclose();
	wfwait();
	wfswait();
	wmemcpy();
	wvaset();
	setwfilext();
	getwfilext();
	setretcode();
	wexith();
	vexit();
	wfilechk();
	wfilechk2();
	vwang();
	setrunname();
	setwispfilext();
	setprogid();
	wpause();
	wsetstat();
	xx2byte();
	ws80();
	ws132();

	WANSI2WANG();
	WWANG2ANSI();
	WTITLE();
	
/*
** The following routines are ACP routines
*/

#ifdef ACP
	OPENACP();
	CLOSEACP();
	READACP();
	WRITEACP();
	BREAKACP();
	CHECKACP();
	GETACP();
	SETACP();
#endif

/*
** The following are NETRON CAP specific routines
*/

#ifdef NETCAP
	WSCLOSE();
	WSFNM();
	WSFNS();
#endif
	

/*
** The following are EDE routines
*/

#ifdef EDE
	 ws_bar_menu();
	 EDLOAD();
	 EDEXIT();
	 EDCLRSCR();
	 EDDRKSCR();
	 EDLTESCR();
	 EDWIDSCR();
	 EDNARSCR();
	 TRACEGO();
	 TRACEEND();
	 RETRACE();
	 PFKEYSON();
	 NOPFKEYS();
	 MENUSAVE();
	 MENUREST();
	 MENULOAD();
	 MENUITEM();
	 MENUGO();
	 MENUCONT();
	 DYLINK();
	 DYUNLINK();
	 MENUKILL();
	 MENUEXIT();
	 MENUMODE();
	 VIDMOVE();
	 VIDMODE();
	 VIDLINE();
	 VIDTEXT();
	 vpopscr();
	 vpushscr();
	 gcalc();
	 gcalend();
	 gclock();
	 gnotepad();
	 gpuzzle();
	 GENVEC();
	nc_pop_menu();
	gen_ncpfkey();
#endif

/*
** The following are W4W routines
*/
#ifdef W4W
	W4WAPI();
#endif /* W4W */

/*
** The OLD WISP menu system (obsolete)
*/
#ifdef ORIGMENU
	menu();
#endif
}
