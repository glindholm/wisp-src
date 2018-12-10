/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/



#include "vscrglb.h"

#ifdef	CISAM_FILES
#define	SPLITKEYS	1
#define	KEYSOVERLAP	1
#endif


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
**	Revision 1.6  2003/02/04 19:19:08  gsl
**	fix header
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
