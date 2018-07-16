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

#include <string.h>
#include "cobioblk.h"
#include "kcsifunc.h"



void INIDIO(char *blk)
{
	memset(blk ,0,IO_BLOCK_LEN);
	memset(blk,'0',COBOL_BLOCK_LEN);
	memset(blk,' ',STATUS_POS);
}


/*
**	History:
**	$Log: inidio.c,v $
**	Revision 1.4  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.3  1996/09/17 23:45:38  gsl
**	drcs update
**	
**
**
*/
