static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

static char sccsid[]="@(#)datentry.c	1.1 2/4/93";

main()
{
#ifdef KCSI_MFX
	cobinit();
#endif

	DTEMAIN();
}


/*
**	History:
**	$Log: datentry.c,v $
**	Revision 1.3  1999/03/05 15:00:14  gsl
**	Add cobinit() for MFX
**	
**	Revision 1.2  1996-09-17 19:45:32-04  gsl
**	drcs update
**
**
**
*/
