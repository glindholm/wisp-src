static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#ifdef MSDOS
/*
**	sortx.c		This routine exists only to change the module name
**			to make Intel Code Builder not complain about 
**			duplicate module names.  This is caused because
**			ACUCOBOL run386.lib contains a module named "sort".
*/

#define MSDOS_SORTX
#include "sort.c"

#else /* MSDOS */
static int dummy_sortx;
#endif

/*
**	History:
**	$Log: sortx.c,v $
**	Revision 1.5  1996-08-19 18:32:58-04  gsl
**	drcs update
**
**
**
*/
