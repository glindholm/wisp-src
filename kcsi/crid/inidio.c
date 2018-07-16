static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <string.h>
#include "cobioblk.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)inidio.c	1.1 6/27/92";


void INIDIO(char *blk)
{
	memset(blk ,0,IO_BLOCK_LEN);
	memset(blk,'0',COBOL_BLOCK_LEN);
	memset(blk,' ',STATUS_POS);
}


/*
**	History:
**	$Log: inidio.c,v $
**	Revision 1.3  1996-09-17 19:45:38-04  gsl
**	drcs update
**
**
**
*/
