/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


/*----
This is a top end for the LPI and MF cobol versions
of inquiry. It saves the name used to invoke
the program (upper or lower case or whatever)
and then calls the main routine
------*/

#include <string.h>

#ifdef KCSI_MFX
extern int cobinit (void);
#endif

extern int INQMAIN(void);

extern char inq_progname[];

int main(int argc, char *argv[])
{
	strcpy(inq_progname,argv[0]);

#ifdef KCSI_MFX
	cobinit();
#endif

	INQMAIN();

	return 0;
}

/*
**	History:
**	$Log: inquiry.c,v $
**	Revision 1.6  2003/05/07 17:57:37  gsl
**	-Wall
**	
**	Revision 1.5  2003/05/07 17:52:53  gsl
**	-Wall
**	
**	Revision 1.4  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.3  1999/03/05 14:59:40  gsl
**	Add cobinit() for MFX
**	
**	Revision 1.2  1996-09-17 19:45:38-04  gsl
**	drcs update
**
**
**
*/
