static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <string.h>
#include "vseglb.h"
#include "vsescr.h"

/*----
Is passed an array of field values and a 1924 byte area for the blah blah blah
screen, and build and displayable screen
------*/
void vsescr(VSEFLD *fld, char *scr)
{
	while(fld->src)
		{
		vsefld(fld,scr);
		++fld;
		}
}

void vsescr_init(char *scr)
{
	memset(scr+sizeof(vse_default_oa),0,1920);
}

/*----
Loads a field into the screen with a few tricks
If the col is 1 then a fac is not loaded
If the fac is 0 then a DIMFAC is used
If the length is zero then strlen of the src is used
If the field ends in col 80 then a DIM_AC is not appended
If the new fac would overlay an existing fac then a fac
is not loaded.
------*/
void vsefld(VSEFLD *fld, char *scr)
{
	int offset,len,fac;

	scr += sizeof(vse_default_oa);
	offset = ((fld->row - 1) * VSE_SCREEN_WIDTH) + (fld->col - 1);
	if(offset < 0)
		offset = 0;
	scr += offset;
	if(fld->col > 1)
		{
		--scr;
		*scr = (fld->fac)?fld->fac:DIM_FAC;
		++scr;
		}
	memcpy(scr,fld->src,len = (fld->len)?fld->len:strlen(fld->src));

	if( (fld->col + len - 1) < VSE_SCREEN_WIDTH )
	{
		/*
		**	Add in the trailing FAC unless already has one.
		*/
		scr += len;
		fac = *scr;
		if(!(fac&FAC_BIT))
		{
			*scr = DIM_FAC;
		}
	}
}


void vseunscr(VSEFLD *fld, char *scr)
{
	while(fld->src)
		{
		vseunfld(fld,scr);
		++fld;
		}
}

/*----
UnLoads a screen field into the field with a few tricks
If the col is 1 then dimfac is assumed and is not unloaded
If the fac is 0 then a DIMFAC is assumed
If the length is zero then strlen of the src is used
If the field ends in col 80 then a DIM_FAC is not appended
------*/
void vseunfld(VSEFLD *fld, char *scr)
{
	int offset,len,fac;

	if(!fld->obj)
		return;
	scr += sizeof(vse_default_oa);
	offset = ((fld->row - 1) * VSE_SCREEN_WIDTH) + (fld->col - 1);
	if(offset < 0)
		offset = 0;
	scr += offset;
	if(fld->col == 1)
		return;
	--scr;
	fac = *scr;
	++scr;
	len = (fld->len)?fld->len:strlen(fld->src);
	if (len + fld->col - 1 > VSE_SCREEN_WIDTH)
	{
		len = VSE_SCREEN_WIDTH - fld->col + 1;
	}
	memcpy(fld->obj,scr,len);
}

/*
**	History:
**	$Log: vsescr.c,v $
**	Revision 1.10  1996/09/03 22:24:10  gsl
**	drcs update
**	
**
**
*/
