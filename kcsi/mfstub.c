/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
**
**
** 
** CVS
**
**
**
**
******************************************************************************
*/

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
**      Routine:        X4DBFILE()
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
*/
int X4DBFILE(char *select_name, int4 l1, int4 *select_status, int4 l2)
{
	extern void WL_setdbfile(int4 *mode, int flag);
	
        WL_setdbfile(select_status,0);

        return 0;
}
int X4DBFILE2(char *select_name, int4 l1, char *file_attributes, int4 l2)
{
	extern void WL_setdbfile_attr(char *file_attributes, int flag);
	
        WL_setdbfile_attr(file_attributes,0);

        return 0;
}

/*
**	History:
**	$Log: mfstub.c,v $
**	Revision 1.7  2003/03/06 21:33:50  gsl
**	Add X4DBFILE2() for use with file ATTR
**	
**	Revision 1.6  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.5  2002/07/25 14:06:27  gsl
**	x4dbfile -> X4DBFILE
**	
**	Revision 1.4  2002/07/11 20:29:21  gsl
**	Fix WL_ globals
**	
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
