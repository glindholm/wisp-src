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

#ifdef KCSI_MFX
extern int cobinit (void);
#endif

extern int RPTMAIN(void);

int main(int argc, char *argv[])
{
#ifdef KCSI_MFX
	cobinit();
#endif

	RPTMAIN();

	return 0;
}

/*
**	History:
**	$Log: report.c,v $
**	Revision 1.5  2003/05/07 17:52:53  gsl
**	-Wall
**	
**	Revision 1.4  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.3  1999/03/05 15:00:35  gsl
**	Add cobinit() for MFX
**	
**	Revision 1.2  1996-09-17 19:45:47-04  gsl
**	drcs update
**
**
**
*/
