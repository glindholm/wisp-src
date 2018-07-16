static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


#include "vscrglb.h"

#ifdef	CISAM_FILES
#define	SPLITKEYS	1
#define	KEYSOVERLAP	1
#endif

static char sccs_id[]="@(#)vscrspky.c	1.1 8/15/93";

void CR_set_split_keys(void)
{
#ifdef	SPLITKEYS
	cr_split_keys = 1;
#ifdef	KEYSOVERLAP
	CR_keys_overlap = 1;
#else
	CR_keys_overlap = 0;
#endif
#else
	cr_split_keys = 0;
	CR_keys_overlap = 0;
#endif
}
	
/*
**	History:
**	$Log: vscrspky.c,v $
**	Revision 1.3.2.1  2002/11/12 15:56:43  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.5  2002/10/24 14:20:30  gsl
**	Make globals unique
**	
**	Revision 1.4  2002/07/25 15:20:22  gsl
**	Globals
**	
**	Revision 1.3  1996/10/02 16:12:33  gsl
**	Add standard headers
**	Fix prototypes
**	
**
**
*/
