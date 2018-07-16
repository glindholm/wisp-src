/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


#include <string.h>
#include "vseglb.h"
#include "vsescr.h"

/*----
Is passed an array of field values and a 1924 byte area for the blah blah blah
screen, and build and displayable screen
------*/
void vsescr(VSEFLD *fld, unsigned char *scr)
{
	while(fld->src)
		{
		vsefld(fld,scr);
		++fld;
		}
}

void vsescr_init(unsigned char *scr)
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
void vsefld(VSEFLD *fld, unsigned char *scr)
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


void vseunscr(VSEFLD *fld, unsigned char *scr)
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
void vseunfld(VSEFLD *fld, unsigned char *scr)
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
**	Revision 1.12  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.11  2002/08/01 15:49:36  gsl
**	type warnings
**	
**	Revision 1.10  1996/09/03 22:24:10  gsl
**	drcs update
**	
**
**
*/
