static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*----
This is a top end for the LPI and MF cobol versions
of inquiry. It saves the name used to invoke
the program (upper or lower case or whatever)
and then calls the main routine
------*/

static char sccsid[]="@(#)inquiry.c	1.2 9/2/94";

extern char inq_progname[];

main(argc,argv)
int argc;
char *argv[];
{
	strcpy(inq_progname,argv[0]);

#ifdef KCSI_MFX
	cobinit();
#endif

	INQMAIN();
}

/*
**	History:
**	$Log: inquiry.c,v $
**	Revision 1.3  1999-03-05 09:59:40-05  gsl
**	Add cobinit() for MFX
**
**	Revision 1.2  1996-09-17 19:45:38-04  gsl
**	drcs update
**
**
**
*/
