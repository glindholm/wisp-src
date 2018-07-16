/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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

			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"
#include "vmodules.h"
#include "vmove.h"
#include "vraw.h"


/*						Static data definitions.							*/

static int out_flag;									/* Flag when output is actually done.	*/

static int vmv_do(int line, int col, int op);						/* Prep to do the move.			*/
static int vmv_move(int line, int column);						/* Do normal move.			*/
static int vmv_slew(int nl, int ol, int nc, int oc);					/* Slew by n rows and n cols.		*/
static int vmv_same(int ol, int oc, int cnt);						/* Check if attributes the same.	*/
static int vmv_out(char *string, int arg1, int arg2, int arg3);				/* Allow only three args.		*/
static int vmv_traverse_scroll_region(int ol, int nl);					/* ol = old line,  nl = new line.	*/


/*						Subroutine entry point.								*/

int VL_vmove(int line, int column)								/* Move to a location on the screen.	*/
{
	int ret = OPTIMIZED;
	enum e_vop vopt = voptlevel();

	out_flag = FALSE;								/* Assume no actual output to be done.	*/

	if ((line >= MAX_LINES_PER_SCREEN) || (column >= vedge(line)) || (line < 0) || (column < 0))
	{
		/*
		**	Position is invalid
		*/
		vre("vmove(%d,%d)-Position is invalid, no move performed",line,column);
		return FAILURE;
	}

	if (OFF == VL_vmov_op)
	{
		/*
		**     First time thru - no optimization
		*/
		vopt = VOP_OFF;
		VL_vmov_op = ON;								/* Optimization can now be done		*/
	}

	switch(vopt)
	{
	case VOP_OFF:
	case VOP_TRACKING_ONLY:
	case VOP_DATA_ONLY:
		/*
		**	No optimization
		*/
		ret = vmv_do(line,column,OFF);
		break;

	case VOP_DATA_AND_CONTROLS:
		/*
		**	Simple optimization - if already equal don't do.
		**	Escape optimization - find smallest escape code.
		*/
		if ((line != vcur_lin) || (column != vcur_col))
		{
			ret = vmv_do(line,column,ON);
		}
		break;

	case VOP_DEFER_MOTION_ONLY:
	case VOP_DEFER_MODE:
		/*
		**	Simple optimization - if already equal don't do.
		*/
		if ((line != vcur_lin) || (column != vcur_col))
		{
			/*
			**	Defer this action
			*/
			vdefer(VDEFER_MOTION_ONLY);
			vcur_lin = line;
			vcur_col = column;
		}
		break;
	}

	return(ret);									/* Let the caller know how it went.	*/
}

static int vmv_do(int line, int col, int op)						/* Prep to do the move.			*/
{
	vdefer_restore();

	return VL_vgoto(line,col,vcur_lin,vcur_col,op);
}

/*					Subroutine to do calculative moves.							*/

int VL_vgoto(int nl, int nc, int ol, int oc, int op)					/* Got to (nl,nc) from (ol,oc).		*/
{
	register int ret, i;								/* Working registers.			*/
	enum e_vop save_op;
	
	out_flag = FALSE;								/* Assume no real output to do.		*/
	i = MAX_LINES_PER_SCREEN;							/* Copy to make next line shorter.	*/
	if ((nl >= i) || (nc >= vedge(nl)) || (ol >= i) || (oc >= vedge(ol)) || (nl < 0) || (nc < 0))	/* Valid position?	*/
	{
		vre("VL_vgoto(%d,%d,%d,%d)-Position invalid, no move performed",nl,nc,ol,oc);	/* Tell user he messed up. 	*/
		return FAILURE;
	}
	ret = SUCCESS;									/* Assume success.			*/

	if (op && (nl == ol) && (nc == oc)) 
	{
		ret = OPTIMIZED;							/* Already there so don't do a thing.	*/
	}
	else if (vdeferred() == VDEFER_OFF)						/* Any output at all?			*/
	{
		if (nl < ol || nc < oc || 
		    nl-ol > 3 || nc-oc > 3)
		{
			/*
			**	If we have to go backwards then do a real move not a slew.
			**	OR If moving more then 3 characters then too far to slew.
			*/
			op = OFF;
		}
#ifdef	DIRECTVID
		if (vrawdirectio())
		{
			/*
			**	Direct I/O must always go thru vmv_move() so we force optimization off.
			*/
			op = OFF;
		}
#endif
		
		VL_vbuffering_start();						/* Turn on logical buffering.		*/

		/*
		**	Do not defer the following move so turn down the opitization level.
		*/
		save_op = voptlevel();
		if (save_op > VOP_DATA_AND_CONTROLS)
		{
			voptimize(VOP_DATA_AND_CONTROLS);				/* Turn down opt level			*/
		}

		if (!op || vmv_traverse_scroll_region(ol,nl))				/* Can we do a quick move?		*/
		{
			vmv_move(nl,nc);						/* No, then do a regular move.		*/
			ret = SUCCESS;							/* Normal move is just successful.	*/
		}
		else 
		{
			vmv_slew(nl,ol,nc,oc);						/* Slew to new location.		*/
			ret = OPTIMIZED;						/* Report the optimization.		*/
		}

		if (save_op > VOP_DATA_AND_CONTROLS)
		{
			voptimize(save_op);						/* Restore optimization			*/
		}

		VL_vbuffering_end();							/* Restore automatic buffering.		*/
	}

	vcur_lin = nl;
	vcur_col = nc;	

	if (out_flag)									/* Was there actual output?		*/
	{
		VL_tcur_lin = vcur_lin;							/* Yes, then update true position too.	*/
		VL_tcur_col = vcur_col;
	}

	return(ret);
}

/*					Subroutine to perform normal ANSII moves.						*/

static int vmv_move(int line, int column)						/* Do normal move.			*/
{

#ifdef unix
	char *tparm();
#define PARMFUNC tparm
#else
	char *VL_vcparm();
#define PARMFUNC VL_vcparm
#endif

#ifdef	DIRECTVID
	if (vrawdirectio())
	{
		vrawmove(line,column);							/* Set memory pointers in vrawdos.c.	*/
	}
	else
#endif
	{
		vcontrol(PARMFUNC(VL_vcapvalue(CURSOR_ADDRESS),line,column));		/* Move to the position.		*/
	}
	
	out_flag=TRUE;
	return(SUCCESS);								/* Hopefully nothing went wrong.	*/
}


/*					Subroutine to slew to a location.							*/

static int vmv_slew(int nl, int ol, int nc, int oc)					/* Slew by n rows and n cols.		*/
{
	register int nrows, ncols, temp;						/* Temporary storage.			*/
	unsigned char *cm;								/* Pointer to character map element.	*/

	nrows = nl - ol;								/* Compute delta position to slew.	*/
	ncols = nc - oc;
	if ((nrows == 0) && (ncols == 0)) return(SUCCESS);				/* Return if nothing to do.		*/
	cm = &vchr_map[vml(ol)][oc];							/* Calculate index in case we need it.	*/


	if      ((ncols == 1) && vmv_same(ol,oc,1)) vmv_out("%c",*cm,0,0);		/* Going right one position?		*/
	else if ((ncols == 2) && vmv_same(ol,oc,2)) vmv_out("%c%c",*cm, *(cm+1),0);	/* Going right two positions?	*/
	else if ((ncols == 3) && vmv_same(ol,oc,3)) vmv_out("%c%c%c", *cm, *(cm+1), *(cm+2));	  /* Three positions?	*/
	else if (ncols >   0) 
	{
		vmv_move(nl,nc);							/* more than 1 positions.	*/
		return(SUCCESS);
	}

	temp = VL_vb_pure;									/* Save the current pureness.		*/
	VL_vb_pure = TRUE;									/* Now actually output linefeeds.	*/

	if      (nrows ==  1) vmv_out("\n",0,0,0);					/* Going down 1 position.		*/
	else if (nrows ==  2) vmv_out("\n\n",0,0,0);					/* Going down 2 positions?		*/
	else if (nrows ==  3) vmv_out("\n\n\n",0,0,0);					/* Going down 3 positions.		*/
	else if (nrows >   0) 
	{
		vmv_move(nl,nc);							/* more than 1 positions.	*/
	}
	VL_vb_pure = temp;									/* Now restore pureness state.		*/

	return(SUCCESS);								/* Return that we did it.		*/
}

static int vmv_same(int ol, int oc, int cnt)						/* Check if attributes the same.	*/
{
	register int i, k, same;							/* Working registers.			*/

	k = vml(ol);									/* Compute index into map.		*/

	same = TRUE;									/* Assume the attributes are the same.	*/
	for (i = 0; i < cnt; i++)							/* Loop through all positions.		*/
	{
		if (vatr_map[k][oc+i] != (vcur_atr | vchr_set)) same = FALSE;		/* Are attributes the same?		*/
	}
	return(same);									/* Return the conclusion.		*/
}
	
/*					Subroutine to encode the string to be output.						*/

static int vmv_out(char *string, int arg1, int arg2, int arg3)				/* Allow only three args.		*/
{
	char temp[20];									/* Temporary working string.		*/

	sprintf(temp,string,arg1,arg2,arg3);						/* Encode the output string ANSI?	*/

	vcontrol(temp);									/* Output it.				*/
	out_flag = TRUE;								/* Output was done.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

/*				Subroutine to determine if a move goes across the scroll area.					*/

static int vmv_traverse_scroll_region(int ol, int nl)					/* ol = old line,  nl = new line.	*/
{
	register int ret;								/* Working register.			*/

	ret = FALSE;									/* Assume not a problem.		*/
	if ( (ol >= vrol_top) && (ol <= vrol_bot) )					/* Is the old position in scroll area?	*/
	{
		if ( (nl < vrol_top) || (nl > vrol_bot) ) ret = TRUE;			/* Yes, then is the new line outside?	*/
	}

	return(ret);									/* Now report to the caller.		*/
}

/*
**	History:
**	$Log: vmove.c,v $
**	Revision 1.22  2003/06/20 15:04:28  gsl
**	VL_ globals
**	
**	Revision 1.21  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.20  2002/07/17 21:06:03  gsl
**	VL_ globals
**	
**	Revision 1.19  2002/07/16 13:40:21  gsl
**	VL_ globals
**	
**	Revision 1.18  2002/07/15 20:56:40  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.17  2002/07/15 20:16:11  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.16  2002/07/15 17:10:04  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  1997/07/12 21:34:43  gsl
**	Fix VL_vgoto() so that it didn't defer any move. This was done by
**	reducing the optimization level.
**	
**	Revision 1.14  1997-07-09 11:46:29-04  gsl
**	Fix the directio logic to work with COSTAR on WIN32.
**	Localize a lot of the data and routines.
**	Simplfy the move optimization logic.
**
**	Revision 1.13  1996-08-02 16:32:36-04  jockc
**	removed debug code, removed call to vmv_ud (update tracking info)
**	from ifdef DIRECTVID section (that is being done inside of vrawntcn)..
**	changed ifndef MSDOS to ifndef DIRECTVID
**
**	Revision 1.12  1996-07-26 10:09:22-07  jockc
**	added some logging for debug of windows nt console
**
**	Revision 1.11  1996-07-17 11:40:23-07  jockc
**	changed ifdef MSDOS around vrawmove to ifdef DIRECTVID
**
**	Revision 1.10  1996-03-28 14:23:27-08  gsl
**	fix prototypes
**
 * Revision 1.9  1996/03/12  13:27:05  gsl
 * fix vbuffering()).
 *
**
**
*/
