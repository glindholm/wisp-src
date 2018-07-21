/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

#ifdef KCSI_MFX
extern int cobinit (void);
#endif

extern int DTEMAIN(void);


int main(int argc, char *argv[])
{
#ifdef KCSI_MFX
	cobinit();
#endif

	DTEMAIN();

	return 0;
}


/*
**	History:
**	$Log: datentry.c,v $
**	Revision 1.5  2003/05/07 17:52:53  gsl
**	-Wall
**	
**	Revision 1.4  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.3  1999/03/05 15:00:14  gsl
**	Add cobinit() for MFX
**	
**	Revision 1.2  1996-09-17 19:45:32-04  gsl
**	drcs update
**
**
**
*/
