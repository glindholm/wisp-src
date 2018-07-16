static char copyright[]="Copyright (c) 1988-1997 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";
/*---
mfstub.c

Stubs for routines used by ACUCOBOL

*/

#include "intdef.h"

/*
**      Routine:        ISDBFILE()
**
**      Function:       Stub for routine used by ACUCOBOL port.
**
**      Description:    
**
**      Arguments:	None
**
**      Globals:        None
**
**      Return:         None
**
**      Warnings:       None
**
**      History:
**      07/11/97        Written by SMC
**
*/
int ISDBFILE(char *select_name, int4 l1, char *answer, int4 l2)
{
	*answer = 'N';

        return 0;
}

/*
**      Routine:        x4dbfile()
**
**      Function:       Stub for routine used by ACUCOBOL port.
**
**      Description:    
**
**      Arguments:	None
**
**      Globals:        None
**
**      Return:         None
**
**      Warnings:       None
**
**
**      History:
**      07/11/97        Written by SMC
**
*/
int x4dbfile(char *select_name, int4 l1, int4 *select_status, int4 l2)
{
	extern void setdbfile(int4 *mode, int flag);
	
        setdbfile(select_status,0);

        return 0;
}

/*
**	History:
**	$Log: mfstub.c,v $
**	Revision 1.3  1997/07/15 19:33:50  gsl
**	fix
**	
**	Revision 1.2  1997-07-14 11:17:42-04  scass
**	Corrected imporper use of long.  Changed to int4 for porting.
**
**	Revision 1.1  1997-07-14 10:23:46-04  scass
**	Initial revision
**
**	Revision 1.5  1996-09-17 19:34:01-04  gsl
**	drcs update
**
**
**
*/
