/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define	_VSCRGLB_C
#include "vscrglb.h"

#include "create.h"
#include "vwang.h"



void kcsi_create_main(void)
{
	char the_title[80];

	sprintf(the_title,"CREATE %s.%s", create_version(), create_platform());
	vwang_title(the_title);

	cr_blk = NULL;
	cr_errlist = 0;

	GP_init_gpint();

	WL_set_va_count(2);
	EXTRACT("IL",create_inlib);

	WL_set_va_count(2);
	EXTRACT("IV",create_invol);

	WL_set_va_count(2);
	EXTRACT("OL",create_outlib);

	WL_set_va_count(2);
	EXTRACT("OV",create_outvol);

	for(;;)
	{
		int rc;

		/* 
		** init_cr_out() - Initialize cr_out
		*/
		memset(&cr_out,0,sizeof(cr_out));
		memset(cr_out.ofile._name,' ',8);
		memcpy(cr_out.ofile._library,create_outlib,8);
		memcpy(cr_out.ofile._volume,create_outvol,6);
		strcpy(cr_out.ofile._prname,"NEWFILE ");

		rc = cr_get_output_spec(create_version(),create_platform());
		if (rc == 1)
			continue;
		if(rc == 16)
			break;
		cr_get_blocks();
		cr_create_file();
		rc = cr_eoj();
		if(rc == 16)
			break;
	}
	cr_free_blks();
}



/*
**	History:
**	$Log: vscrglb.c,v $
**	Revision 1.7  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.6  2002/10/24 17:29:39  gsl
**	Set the title
**	
**	Revision 1.5  2002/10/24 14:20:31  gsl
**	Make globals unique
**	
**	Revision 1.4  2002/10/21 18:29:20  gsl
**	cleanup
**	
**	Revision 1.3  2002/10/17 21:22:19  gsl
**	move kcsi_create_main() from vscrmain.c to vscrglb.c
**	The only thing left in vscmain.c is main()
**	
**	Revision 1.2  1996/10/02 16:10:05  gsl
**	Add standard headers
**	
**
**
*/
