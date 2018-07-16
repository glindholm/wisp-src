static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


#include "vscrglb.h"

#ifdef	CISAM_FILES
#define	SPLITKEYS	1
#define	KEYSOVERLAP	1
#endif

static char sccs_id[]="@(#)vscrspky.c	1.1 8/15/93";

void set_split_keys(void)
{
#ifdef	SPLITKEYS
	cr_split_keys = 1;
#ifdef	KEYSOVERLAP
	keys_overlap = 1;
#else
	keys_overlap = 0;
#endif
#else
	cr_split_keys = 0;
	keys_overlap = 0;
#endif
}
	
/*
**	History:
**	$Log: vscrspky.c,v $
**	Revision 1.3  1996-10-02 12:12:33-04  gsl
**	Add standard headers
**	Fix prototypes
**
**
**
*/
