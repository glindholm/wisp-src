/*  mswinsub.c - modifications to the windows runtime should go here  */
/*  $Id: mswinsub.c 57329 2010-01-13 21:05:11Z rzack $  */

/* Copyright (c) 1995-2007 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */

#ifdef	ACU_SOURCE_FILENAME
#undef	ACU_SOURCE_FILENAME
#endif	/* ACU_SOURCE_FILENAME */
#define	ACU_SOURCE_FILENAME	"lib/mswinsub.c"
const char what_lib_mswinsub_c_str[] = "@(#) " ACU_SOURCE_FILENAME " $Date: 2010-01-13 21:05:11 +0000 (Wed, 13 Jan 2010) $$Rev: 57329 $";

#include <stdio.h>
#include <windows.h>

/* The following variables may be used by your 'C' subroutines.  The 	*/
/* first one (the current instance) is initialized by the start-up 	*/
/* code.  The last one (the handle of the runtime's window) is created	*/
/* after the start-up code and after the configuration file has been	*/
/* read.  It will remain NULL if you use either the "-b" runtime	*/
/* flag or the NO-CONSOLE configuration option.  In these cases, the	*/
/* runtime does not create its own window - you must provide any window	*/
/* that you wish to use.  */

/* Note that you should reference hAcuInstance and hAcuWnd as if they	*/
/* were regular variables, even though they have the strange definition	*/
/* shown here.  The implementation may change in the future, but the	*/
/* use of the names hAcuWnd and hAcuInstance will not change.  */

extern	HINSTANCE	*Aui_hAcuInstance( void );
extern	HWND		*Aui_hAcuWnd( void );

#define	hAcuInstance	(*Aui_hAcuInstance())
#define	hAcuWnd		(*Aui_hAcuWnd())

extern	HDC		hAcuSpoolerDC;	/* display context for spooler */


/* windows_startup - this is called only when running under Microsoft	*/
/* Windows.  It is passed the same parameters as WinMain - see the	*/
/* Microsoft documentation for details.  Use this routine to register	*/
/* any window classes that you need.  Return 0 if an error occurs and	*/
/* you need to shut-down, otherwise return 1.  */

int
windows_startup(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
  int nCmdShow)
{
	return 1;
}

