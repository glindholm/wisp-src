static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

static char sccsid[]="@(#)control.c	1.1 2/4/93";

main()
{
#ifdef KCSI_MFX
	cobinit();
#endif

	CTRLMAIN();
}

/*
**	History:
**	$Log: control.c,v $
**	Revision 1.3  1999/03/05 14:47:00  gsl
**	add cobinit() for MFX
**	
**	Revision 1.2  1996-09-17 19:45:27-04  gsl
**	drcs update
**
**
**
*/
