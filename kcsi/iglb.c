static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "kcsifunc.h"

static char sccsid[]="@(#)iglb.c	1.2 1/27/93";

char inq_tokens[81];
int  inq_token = 0;
char *inq_inqs;

void inq_globals()
{
	/* Dummy routine for linker */
}


/*
**	History:
**	$Log: iglb.c,v $
**	Revision 1.3.2.1  2002/11/12 15:56:25  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.7  2002/10/24 15:48:32  gsl
**	Make globals unique
**	
**	Revision 1.6  2002/10/23 21:07:27  gsl
**	make global name unique
**	
**	Revision 1.5  2002/10/17 17:17:18  gsl
**	Removed VAX VMS code
**	
**	Revision 1.4  2002/07/25 15:20:28  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:45:37  gsl
**	drcs update
**	
**
**
*/
