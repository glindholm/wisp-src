/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/



#include <stdio.h>
#include "vscrglb.h"

/* routines to check if everything in a block is completed */

/*----
 A block is complete if one of the following conditions is true:

1.	Block count is > 1 and count is reached.

2.	Any field that is  file or sequence has exhausted its input.


------*/


static int file_is_done(CR_FLD *fld);
static int ix_is_done(CR_FLD *fld);
static int rel_is_done(CR_FLD *fld);


static int field_is_done(CR_FLD *fld)
{
	if(fld->type == CFIELD_PAD)
		return(0);
	if(fld->type == CFIELD_STRING)
		return(0);
	if(fld->type == CFIELD_SEQUENCE)
	{
		if(	(fld->repeat)			||
			(fld->curval <= fld->endval)	)
		{
			return(0);
		}
		else
		{
			return(1);
		}
	}
	if(fld->type == CFIELD_FILE)
	{
		return(file_is_done(fld));
	}

	/* This should never happen */
	return 0;
}

static int file_is_done(CR_FLD *fld)
{
	if(fld->kfb->_org[0] == 'I')
		return(ix_is_done(fld));
	else
		return(rel_is_done(fld));
}

/*----
An index file is done if an at_end condition exists, or the 
key is exhausted.
------*/
static int ix_is_done(CR_FLD *fld)
{
	if(fld->kfb->_status != 0)
		return(1);
	return(0);
}

/*----
A relative or sequential file is done, if an at_end condition exists, or the
record range is exhausted
------*/
static int rel_is_done(CR_FLD *fld)
{
	if(fld->kfb->_status != 0)
		return(1);
	if(	(fld->endval > 0) 		&&
		(fld->curval > fld->endval)	)
		return(1);
	return(0);
}

int CR_block_is_done(CR_BLK *blk)
{
	if(blk->count > 0)
		{
		if(blk->counter >= blk->count)
			return(1);
		}
	if(ll_select((LLTYPE*)blk->fld, field_is_done))
		return(1);
	return(0);
}

/*
**	History:
**	$Log: vscrbchk.c,v $
**	Revision 1.7  2003/02/20 19:29:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.6  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.5  2002/10/23 20:39:05  gsl
**	make global name unique
**	
**	Revision 1.4  1999/09/13 19:54:38  gsl
**	fix missing return code
**	
**	Revision 1.3  1996-10-02 12:07:26-04  gsl
**	Add standard headers
**	Fix prototypes
**
**
**
*/
