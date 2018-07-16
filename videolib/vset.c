static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include internal definitions.	*/
#include "vdata.h"
#include "vcap.h"
#include "vmodules.h"
#include "vraw.h"


static int vs(int vs_item, int vs_state, char *string);					/* vset output.				*/

/*						Static data.									*/


/*						Subroutine entry point.								*/

int vset(int item, int state)								/* Set item to given state.		*/
{
	extern int force_200;								/* Reference flag to force VT200 mode.	*/
	register int ret;								/* Working registers.			*/

	if ((state < 0) || (state > 1))							/* Is this a valid state code?		*/
	{
		ret = FAILURE;								/* No, so flag a failure.		*/
		vre("vset(%d,%d)-Invalid selection state.",item,state);			/* Give error message as appropriate.	*/
	}
	else if ((item < 0) && (item > VSET_TABLE_SIZE))				/* Is the item code valid?		*/
	{
		ret = FAILURE;								/* No, then return with failure.	*/
		vre("vset(%d,%d)-Invalid selection item.",item,state);			/* Give error message as appropriate.	*/
	}
	else if (item == VSET_KEYPAD) 
		ret = (state ? vs(item, 1, kpapmd_esc) : vs(item, 0, kpnorm_esc));	/* Applications keypad is special case.	*/
	else if (item == VSET_PRINTER) 
		ret = (state ? vs(item, 1, prnton_esc) : vs(item, 0, prntoff_esc));
	else if (item == VSET_CURSOR) 
		ret = (state ? vset_cursor_on() : vset_cursor_off());

	return(ret);									/* Return with status code.		*/
}

int vset_cursor_on(void) 
{
#ifdef	DIRECTVID
	if (vrawdirectio())
	{
		vrawcursor( VISIBLE );
		vcur_set[VSET_CURSOR] = VISIBLE;
		return SUCCESS;
	}
#endif
	return vs(VSET_CURSOR, VISIBLE, cursron_esc);
}
int vset_cursor_off(void)
{
#ifdef	DIRECTVID
	if (vrawdirectio())
	{
		vrawcursor( INVISIBLE );
		vcur_set[VSET_CURSOR] = INVISIBLE;
		return SUCCESS;
	}
#endif
	return vs(VSET_CURSOR, INVISIBLE, cursroff_esc);
}



/*				Output string attending to optimization.							*/

static int vs(int vs_item, int vs_state, char *string)					/* vset output.				*/
{
	static int vset_op[VSET_TABLE_SIZE] = { 0 };				/* Terminal setup elements start in off state.	*/
	int ret;									/* Working registers.			*/

	
	ret = OPTIMIZED;								/* Assume we will be optimized.		*/

	if (string && !*string)
	{
		/*
		**	Null string 
		**
		**	This was added because a number of the vset() items are actually null.
		**	Specifically the ORIGIN is null and the call to vha() cause video
		**	to think it was at origin when it wasn't.
		*/

		vset_op[vs_item] = ON;							/* Set the one shot op flag on.		*/
		ret = SUCCESS;								/* Set successful.			*/
	}
	else if ((!vset_op[vs_item]) || (voptlevel() <= VOP_DATA_ONLY))			/* Should we try to optimize?		*/
	{
		vset_op[vs_item] = ON;							/* Set the one shot op flag on.		*/
		vdefer_restore();							/* Restore from deferred action.	*/
#ifdef DIRECTVID
		if (vrawdirectio())
		{
			/* Do nothing */
		}
		else
#endif
		{
			vcontrol(string);						/* Output the requested state.		*/
		}
		
		ret = SUCCESS;								/* Set successful.			*/
	}

	else if (voptlevel() <= VOP_DEFER_MOTION_ONLY)					/* Should we optimize this control?	*/
	{
		if (!(vset_op[vs_item] && (vcur_set[vs_item] == vs_state)))		/* Nicely optimized?			*/
		{
			vdefer_restore();						/* Restore from deferred action.	*/
#ifdef DIRECTVID
			if (vrawdirectio())
			{
				/* Do nothing */
			}
			else
#endif
			{
				vcontrol(string);					/* Output the requested state.		*/
			}

			ret = SUCCESS;							/* Set successful.			*/
		}
	}

	else if (!(vset_op[vs_item] && (vcur_set[vs_item] == vs_state))) vdefer_save();	/* Nicely optimized?			*/

	vcur_set[vs_item] = vs_state;							/* Remember for next time.		*/
	return(SUCCESS);								/* And all is well.			*/
}

/*
**	History:
**	$Log: vset.c,v $
**	Revision 1.12  1997/07/09 16:23:52  gsl
**	Add support for direct IO
**	Add support for COSTAR on WIN32
**	Removed 13 out of 16 set options which were unused and unavailable on
**	must terminals, simplified must of the logic.
**	
**	Revision 1.11  1996-08-02 16:30:35-04  jockc
**	changed ifdef MSDOS to ifdef DIRECTVID
**
**	Revision 1.10  1996-07-17 13:55:26-07  jockc
**	changed ifdef MSDOS around vrawcursor to ifdef DIRECTVID
**
**	Revision 1.9  1996-03-11 14:59:47-08  gsl
**	Fix the bug seen on a vline() call where the line was drawn in the wrong
**	location (specifically row 23) causing the screen to scroll and
**	everything became mis-placed.
**	The problem was the handling of restoring the ORIGIN, the code that does
**	this is not implemented and does nothing, however vs() in vset() calls vha()
**	to tell video that the origin has been restored and we are now at 0,0.
**	vha() then does a vmove() to return you to where you should be.
**	However since restoring the origin never moved you in first place vha()
**	caused the position to be all screwed up.
**	This only would show up sometimes because vmove() will usually do an
**	absolute move to the desired position, HOWEVER if you are close to the
**	desired position vmove() will "cursor" you over which fails if you
**	are not where you think you are.
**	The fix I did was to add null string processing to vs(). If vs() is call
**	with a null string then it does not issue the vcontrol() or call vha().
**
**
**
*/
