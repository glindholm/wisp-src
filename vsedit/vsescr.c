#include "vseglb.h"
#include "vsescr.h"

/* General information: The total length of an editable line is 72 columns,
   not 80 and the code has been changed to reflect this. Between columns 72
   and 80 a MOD code (modification code) is displayed under certain situations.
   Modifications by CIS: 07/20/93 AJA */

/*----
Is passed an array of field values and a 1924 byte area for the blah blah blah
screen, and build and displayable screen
------*/
vsescr(fld,scr)
VSEFLD *fld;
char *scr;
{
	while(fld->src)
		{
		vsefld(fld,scr);
		++fld;
		}
}

vsescr_init(scr)
char *scr;
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
vsefld(fld,scr)
VSEFLD *fld;
char *scr;
{
	int col,row,offset,len,fac;

	scr += sizeof(vse_default_oa);
	offset = ((fld->row - 1) * 80) + (fld->col - 1);
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
	if( 72 > (fld->col + len))
		{
		scr += len;
		fac = *scr;
		if(!(fac&FAC_BIT))
			*scr = DIM_FAC;
		}

	/* Set the fac in column 72 (the end of the line).
	   Added by CIS: 07/20/93 AJA */
	else if ( 80 > (fld->col + len) )
	{
		scr += len;
		*scr = language_case();
		*(++scr) = DIM_FAC;
	}
}


vseunscr(fld,scr)
VSEFLD *fld;
char *scr;
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
vseunfld(fld,scr)
VSEFLD *fld;
char *scr;
{
	int col,row,offset,len,fac;

	if(!fld->obj)
		return;
	scr += sizeof(vse_default_oa);
	offset = ((fld->row - 1) * 80) + (fld->col - 1);
	if(offset < 0)
		offset = 0;
	scr += offset;
	if(fld->col == 1)
		return;
	--scr;
	fac = *scr;
	++scr;
	len = (fld->len)?fld->len:strlen(fld->src);
	if (len + fld->col > 80)
	{
		len = 80-(fld->col);
	}
	memcpy(fld->obj,scr,len);
}

