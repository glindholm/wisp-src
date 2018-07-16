/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

#include "kcsifunc.h"


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
**	Revision 1.8  2003/02/04 19:19:09  gsl
**	fix header
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
