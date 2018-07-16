			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	wrunmf:		This routine is used to build a Micro Focus COBOL/2 
			Run Time System (RTS or RTE).


		$ cob -xe "" -o wrunmf wrunmf.c wispmf.o -lwtest -lwisp -lilp -lvideo
*/

/*
	To reduce the size of the RTS you can remove any of the following
	"#define" statements and the corresponding files will not be
	included.

		ACP	 The Wang ACP routines.
		NETCAP	 The Netron Cap routines.
		WISPTEST Wisp test routines.
		EDE	 The EDE routines.
*/

#define ACP
#define NETCAP

/*
 *	#define WISPTEST
 */

#define EDE


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

	LINKGARG();
	ACUGARGS();
	ACUNARGS();
	ACUPARGS();
	BELL();
	BITPACK();
	BITUNPK();
	COBLINK();
	CANCEL();
	CEXIT();
	DATE();
	DAY();
	EXTRACT();
	FILECOPY();
	FIND();
	findlong();
	FXZONE();
	GETPARM();
	getparmbuild();
	PUTPARM();
	HEXPACK();
	HEXUNPK();
	LINK();
	LINKPROC();
	LOGOFF();
	MESSAGE();
	NOHELP();
	ONHELP();
	PRINT();
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
	SORT();
	SORTCALL();
	SORTINFO();
	SORTLINK();
	STRING();
	SUBMIT();
	UPDATFDR();
	UPPER();
	WACCEPT();
	WCHAIN();
	WDISPLAY();
	WISPEXIT();
	WISPSHUT();
	WISPSYNC();
	WISPSORT();
	WSXIO();
	menu();
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
	wfname();
	wfopen();
	wfopen2();
	wfopen3();
	wfclose();
	wdinit();
	wdfinish();
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
	vwang();
	greclen();
	setrunname();
	setwispfilext();
	setprogid();
	wpause();
	wsetstat();
	xx2byte();
	ws80();
	ws132();

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
** The following are WISPTEST routines
*/

#ifdef WISPTEST
	MESSUP();
	QASCROLL();
	QATSTLST();
	QAGP255();
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
}
