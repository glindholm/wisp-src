/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


#include "vscrglb.h"
#include "kcsifunc.h"


int cr_eoj(void)
{
	int rc;
	long pf;
	char records[15];
	char errors[15];

	WL_wpload();
	GP_pfkeys = GP_PF_01|GP_PF_16;
	WL_wswap(&GP_pfkeys);

	GPSETUP();
	GPSTD("EOJ     ","CREATE");

	GPCTEXT("         in          on        created with",10,2);
	GPCTEXT(cr_out.ofile._name,10,2);
	GPCTEXT(cr_out.ofile._library,10,14);
	GPCTEXT(cr_out.ofile._volume,10,26);
	sprintf(records,"%ld",cr_out.records);
	GPCTEXT(records,10,46);
	GPCTEXT("records.",10,60);
	GPCTEXT("PFKey (1)  to create another file.",12,20);
	GPCTEXT("PFKey (16) to exit.",14,20);
	if(cr_out.errors != 0L)
		{
		sprintf(errors,"%ld",cr_out.errors);
		GPCTEXT("Warning",20,2);
		GPCTEXT("records not created due to key errors.",20,25);
		GPCTEXT(errors,20,10);
		}
	GPNOENTER();
	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	rc = pf;
	return(rc);
}

/*
**	History:
**	$Log: vscreoj.c,v $
**	Revision 1.8  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.7  2002/10/24 14:20:32  gsl
**	Make globals unique
**	
**	Revision 1.6  2002/07/12 17:17:02  gsl
**	Global unique WL_ changes
**	
**	Revision 1.5  2002/07/10 21:06:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.4  1996/10/03 00:11:40  gsl
**	Fixed pfkey tags for w4w suppport
**	
**	Revision 1.3  1996-10-02 09:08:38-07  gsl
**	Add standard headers
**
**
**
*/
