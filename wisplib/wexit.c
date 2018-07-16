			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wexit.c
*/

#include "werrlog.h"
#include "wglobals.h"

#ifdef		ROUTINE
#undef          ROUTINE
#endif
#define		ROUTINE		65400

wexit(num)
long	num;
{
	int	exit_code;
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	LINKCOMPCODE = 16;								/* Abnormal termination of LINK		*/

#ifdef VMS
	if ( num == 0 ) exit_code = 1;							/* Change to VMS style exit code	*/
	else		exit_code = 0;
	if (LINKPARM) VMSPARGS();							/* If LINK then restore args		*/
#else					/* unix and MSDOS */
	if ( 2 == sizeof(int) )								/* int == short (16 bits; like MSDOS).	*/
	{
		exit_code = (int)( num % 10000L );					/* Preserve the last 4 decimal digits.	*/
	}
	else										/* int == long (32 bits; unix or VMS).	*/
	{
		exit_code = num;
	}

#ifdef unix
	if (LINKPARM) ACUPARGS();							/* If LINK then restore args		*/
#endif

	wexith();
	vexit();
#endif

	shutexitcobol(exit_code);		/* this is the COBOL specific routine to shutdown cobol and exit		*/
}

