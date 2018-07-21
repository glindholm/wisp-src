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

extern int CTRLMAIN(void);


int main(int argc, char *argv[])
{
#ifdef KCSI_MFX
	cobinit();
#endif

	CTRLMAIN();

	return 0;
}

/*
**	History:
**	$Log: control.c,v $
**	Revision 1.5  2003/05/07 17:52:53  gsl
**	-Wall
**	
**	Revision 1.4  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.3  1999/03/05 14:47:00  gsl
**	add cobinit() for MFX
**	
**	Revision 1.2  1996-09-17 19:45:27-04  gsl
**	drcs update
**
**
**
*/
