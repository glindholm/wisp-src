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

/*						Static data.									*/

static vs_item;										/* Scope reference for item.		*/
static vs_state;									/* Scope reference for state.		*/

/*						Subroutine entry point.								*/

int vset(item,state) int item, state;							/* Set item to given state.		*/
{
	extern int force_200;								/* Reference flag to force VT200 mode.	*/
	register int ret;								/* Working registers.			*/

	vs_item = item;									/* Set scope reference for item.	*/
	vs_state = state;								/* Set scope reference for state.	*/
	if ((state < 0) || (state > 1))							/* Is this a valid state code?		*/
	{
		ret = FAILURE;								/* No, so flag a failure.		*/
		vre("vset(%d,%d)-Invalid selection state.",item,state);			/* Give error message as appropriate.	*/
	}
	else if ((item < 0) && (item > INT_SET_TABLE_SIZE))				/* Is the item code valid?		*/
	{
		ret = FAILURE;								/* No, then return with failure.	*/
		vre("vset(%d,%d)-Invalid selection item.",item,state);			/* Give error message as appropriate.	*/
	}
	else if ((item == TERMINAL) && (state == ANSI) && force_200) ret = vs(vt2207bit_esc);	/* VT220 / 7 bit mode.		*/
	else if ((item == TERMINAL) && (state == ANSI)) ret = vs(termansi_esc);		/* ANSI selection is a special case.	*/
	else if (item == KEYPAD) ret = (state ? vs(kpapmd_esc) : vs(kpnorm_esc));	/* Applications keypad is special case.	*/
	else if (item == SCROLL) ret = (state ? vs(scrlsmooth_esc) : vs(scrljump_esc));	/* Smooth or jump scroll?		*/
	else if (item == ORIGIN) ret = (state ? vs(origscrl_esc) : vs(origtos_esc));	/* Origin top of screen or scroll area?	*/
	else if (item == AUTO_WRAP) ret = (state ? vs(arapon_esc) : vs(arapoff_esc));
	else if (item == AUTO_REPEAT) ret = (state ? vs(arepton_esc) : vs(areptoff_esc));
	else if (item == INTERLACE) ret = (state ? vs(ilaceon_esc) : vs(ilaceoff_esc));
	else if (item == AUTO_PRINT) ret = (state ? vs(aprnton_esc) : vs(aprntoff_esc));
	else if (item == PRINTER) ret = (state ? vs(prnton_esc) : vs(prntoff_esc));
	else if (item == NEW_LINE_MODE) ret = (state ? vs(nlmdon_esc) : vs(nlmdoff_esc));
	else if (item == KEYBOARD) ret = (state ? vs(keylckon_esc) : vs(keylckoff_esc));
	else if (item == INSERT_MODE) ret = (state ? vs(insmdon_esc) : vs(insmdoff_esc));
#ifdef	MSDOS
	else if (item == CURSOR) ret = vrawcursor( state );
#else	/* VMS or unix */
	else if (item == CURSOR) ret = (state ? vs(cursron_esc) : vs(cursroff_esc));
#endif	/* VMS or unix */
	else if (item == PRINT_TERMINATOR) ret = (state ? vs(ptermff_esc) : vs(ptermnone_esc));
	else if (item == PRINT_EXTENT) ret = (state ? vs(pextscrl_esc) : vs(pextfull_esc));

	return(ret);									/* Return with status code.		*/
}

/*				Output string attending to optimization.							*/

vs(string,arg) char string[]; long arg;							/* vset output.				*/
{
	char temp[MAX_ESC*2];								/* Working string.			*/
	register int ret;								/* Working registers.			*/

	ret = OPTIMIZED;								/* Assume we will be optimized.		*/

	if ((vs_item == ORIGIN) && (vs_state == SCROLL_AREA)) vre("vset(ORIGIN,SCROLL_AREA) not yet implemented.");

	else if ((!vset_op[vs_item]) || (optimization <= DATA_ONLY))			/* Should we try to optimize?		*/
	{
		vset_op[vs_item] = ON;							/* Set the one shot op flag on.		*/
		vdefer(RESTORE);							/* Restore from deferred action.	*/
#ifndef MSDOS	/* VMS or unix */
		sprintf(temp,string,arg);						/* Encode to a single string.		*/
		vcontrol(temp);								/* Output the requested state.		*/
#endif	/* VMS or unix */
		if (vs_item == ORIGIN) vha();						/* Now adjust for cursor home.		*/
		ret = SUCCESS;								/* Set successful.			*/
	}

	else if (optimization <= DATA_CONTROLS_AND_MOTION)				/* Should we optimize this control?	*/
	{
		if (!(vset_op[vs_item] && (vcur_set[vs_item] == vs_state)))		/* Nicely optimized?			*/
		{
			vdefer(RESTORE);						/* Restore from deferred action.	*/
#ifndef MSDOS	/* VMS or unix */
			sprintf(temp,string,arg);					/* Encode to a single string.		*/
			vcontrol(temp);							/* Output the requested state.		*/
#endif	/* VMS or unix */
			if (vs_item == ORIGIN) vha();					/* Now adjust for cursor home.		*/
			ret = SUCCESS;							/* Set successful.			*/
		}
	}

	else if (!(vset_op[vs_item] && (vcur_set[vs_item] == vs_state))) vdefer(SAVE);	/* Nicely optimized?			*/

	vcur_set[vs_item] = vs_state;							/* Remember for next time.		*/
	return(SUCCESS);								/* And all is well.			*/
}
