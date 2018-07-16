static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include "vscrglb.h"
#include "kcsifunc.h"

static char sccs_id[]="@(#)vscreoj.c	1.1 8/15/93";

int cr_eoj(void)
{
	int rc;
	long pf;
	char records[15];
	char errors[15];

	wpload();
	gppfkeys = GP_PF_01|GP_PF_16;
	wswap(&gppfkeys);

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
	GPPFS(&gppfkeys);
	pf = display_and_read_gp();
	rc = pf;
	return(rc);
}

/*
**	History:
**	$Log: vscreoj.c,v $
**	Revision 1.4  1996-10-02 20:11:40-04  gsl
**	Fixed pfkey tags for w4w suppport
**
**	Revision 1.3  1996-10-02 09:08:38-07  gsl
**	Add standard headers
**
**
**
*/
