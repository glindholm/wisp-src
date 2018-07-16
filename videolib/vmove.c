			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"

#define JUST_TEXT  0									/* Output string is just text.		*/
#define ABSOLUTE   1									/* Move to absolute location on screen.	*/
#define GO_UP      2									/* Slew one position.			*/
#define SLEW_UP    6									/* Slew n positions.			*/
#define SLEW_DOWN  7
#define SLEW_RIGHT 8
#define SLEW_LEFT  9

/*						Static data definitions.							*/

static char lead_in;									/* Flag controls writing of quick data.	*/
static int out_flag;									/* Flag when output is actually done.	*/

/*						Subroutine entry point.								*/

int vmove(line,column) int line, column;						/* Move to a location on the screen.	*/
{
	register int ret;								/* Return status.			*/

	out_flag = FALSE;								/* Assume no actual output to be done.	*/
	ret = OPTIMIZED;								/* Assume we will be optimized.		*/

	if ((line >= MAX_LINES_PER_SCREEN) || (column >= vedge(line)) || (line < 0) || (column < 0))	/* Valid position?	*/
	{
		ret = FAILURE;								/* Nope, so flag the failure.		*/
		vre("vmove(%d,%d)-Position is invalid, no move performed",line,column);	/* Tell the user why. 			*/
	}

	else if ((!vmov_op) || (optimization <= DATA_ONLY))				/* Is the local control flag off?	*/
	{
		vdefer(RESTORE);							/* Yes, then restore from holding.	*/
		ret = vmv_do(line,column,(vmov_op && optimization));			/* Go do the move.			*/
		vmov_op = TRUE;								/* It's a one shot affair.		*/
	}

	else if (optimization == DATA_AND_CONTROLS)					/* Optimize controls?			*/
	{
		if ((line != vcur_lin) || (column != vcur_col))				/* Any changes?				*/
		{
			vdefer(RESTORE);						/* Restore what we were doing.		*/
			ret = vmv_do(line,column,ON);					/* Do the change.			*/
		}
	}

	else if ((line != vcur_lin) || (column != vcur_col)) vdefer(MOTION_ONLY);	/* Else we defer this action.		*/

	if (!(ret == FAILURE)) vmv_ud(line,column);					/* Update position tracking.		*/
	return(ret);									/* Let the caller know how it went.	*/
}

int vmv_do(line, col, op) int line, col, op;						/* Prep to do the move.			*/
{
#ifdef	MSDOS
	vrawmove(line,col);								/* Set memory pointers in vrawdos.c.	*/
	vmv_ud(line,col);								/* Update tracking flags.		*/
	return(SUCCESS);								/* Unconditionally successfull.		*/
#else	/* VMS or unix */
	vgoto(line,col,vcur_lin,vcur_col,vmv_dist(line,col,vcur_lin,vcur_col,op),op);	/* Call the internal go to routine.	*/
	return(SUCCESS);								/* Unconditionally successfull.		*/
#endif	/* VMS or unix */
}

#ifndef MSDOS	/* VMS or unix */
/*					Subroutine to do calculative moves.							*/

vgoto(nl,nc,ol,oc,dis,op) int nl, nc, ol, oc, dis, op;					/* Got to (nl,nc) from (ol,oc).		*/
{
	register int ret, i;								/* Working registers.			*/

	out_flag = FALSE;								/* Assume no real output to do.		*/
	i = MAX_LINES_PER_SCREEN;							/* Copy to make next line shorter.	*/
	if ((nl >= i) || (nc >= vedge(nl)) || (ol >= i) || (oc >= vedge(ol)) || (nl < 0) || (nc < 0))	/* Valid position?	*/
	{
		ret = FAILURE;								/* Nope, so flag the failure.		*/
		vre("vgoto(%d,%d,%d,%d)-Position invalid, no move performed",nl,nc,ol,oc);	/* Tell user he messed up. 	*/
	}
	ret = SUCCESS;									/* Assume success.			*/
	if (op && (nl == ol) && (nc == oc)) ret = OPTIMIZED;				/* Already there so don't do a thing.	*/
	else if (!deferred)								/* Any output at all?			*/
	{
		vbuffering(LOGICAL);							/* Turn on logical buffering.		*/

		if (!op || (vmv_max(nl,nc,ol) <= dis) || vmv_traverse(ol,nl))		/* Can we do a quick move?		*/
		{
			vmv_move(nl,nc,ol);						/* No, then do a regular move.		*/
			ret = SUCCESS;							/* Normal move is just successful.	*/
		}

		else if (lead_in == 0)							/* A leading new line or return?	*/
		{
			vmv_slew(nl,ol,nc,oc);						/* No, then slew to new location.	*/
			ret = OPTIMIZED;						/* Report the optimization.		*/
		}

		else 									/* Yes, then do a slew.			*/
		{
			vmv_out(JUST_TEXT,"%c",lead_in);				/* Do the return or new line.		*/
			if (lead_in == '\n') vcur_lin = vcur_lin+1;			/* Remember that we've moved.		*/
			vcur_col = 0;							/* And we're at the left hand side.	*/
			if (lead_in == '\n') vmv_slew(nl,(ol+1),nc,0);			/* Slew from the start of next line.	*/
			else vmv_slew(nl,ol,nc,0);					/* Call the slewer.			*/
			ret = OPTIMIZED;						/* Report the optimization.		*/
		}

		vbuffering(AUTOMATIC);							/* Restore automatic buffering.		*/
	}
	vmv_ud(nl,nc);									/* Update tracking flags.		*/
	return(ret);
}

/*					Subroutine to perform normal ANSII moves.						*/

vmv_move(line,column,from_l) int line, column, from_l;					/* Do normal move.			*/
{
#ifdef unix
	char *tparm();
#define PARMFUNC tparm
#else
	char *vcparm();
#define PARMFUNC vcparm
#endif
	
	vcontrol(PARMFUNC(vcapdef[CURSOR_ADDRESS],line,column));			/* Move to the position.		*/
	if (!vmv_compatible(from_l,line))						/* Moving from double wide line?	*/
	{
		vcontrol(PARMFUNC(vcapdef[CURSOR_ADDRESS],line,column));		/* Yes so move again (VT100 bug).	*/
	}
	out_flag=TRUE;
	return(SUCCESS);								/* Hopefully nothing went wrong.	*/
}


/*					Subroutine to slew to a location.							*/

vmv_slew(nl, ol, nc, oc) int nl, ol, nc, oc;						/* Slew by n rows and n cols.		*/
{
	register int nrows, ncols, temp;						/* Temporary storage.			*/
	unsigned char *cm;								/* Pointer to character map element.	*/
#ifdef unix
	char *tparm();
#define PARMFUNC tparm
#else
	char *vcparm();
#define PARMFUNC vcparm
#endif

	nrows = nl - ol;								/* Compute delta position to slew.	*/
	ncols = nc - oc;
	if ((nrows == 0) && (ncols == 0)) return(SUCCESS);				/* Return if nothing to do.		*/
	cm = &vchr_map[vml(ol)][oc];							/* Calculate index in case we need it.	*/

	if      (ncols == -1) vmv_out(JUST_TEXT,"\b");					/* Going left 1 position.		*/
	else if	(ncols == -2) vmv_out(JUST_TEXT,"\b\b");				/* Going left 2 positions?		*/
	else if (ncols == -3) vmv_out(JUST_TEXT,"\b\b\b");				/* Going left 3 positions?		*/
	else if (ncols <   0) 
	{
		vcontrol(PARMFUNC(vcapdef[CURSOR_ADDRESS],nl,nc));			/* more than 3 positions.	*/
		out_flag=TRUE;
		return(SUCCESS);
	}
	else if ((ncols == 1) && vmv_same(ol,oc,1)) vmv_out(JUST_TEXT,"%c",*cm);	/* Going right one position?		*/
	else if ((ncols == 2) && vmv_same(ol,oc,2)) vmv_out(JUST_TEXT,"%c%c",*cm, *(cm+1));	/* Going right two positions?	*/
	else if ((ncols == 3) && vmv_same(ol,oc,3)) vmv_out(JUST_TEXT,"%c%c%c", *cm, *(cm+1), *(cm+2));	  /* Three positions?	*/
	else if (ncols >   0) 
	{
		vcontrol(PARMFUNC(vcapdef[CURSOR_ADDRESS],nl,nc));			/*  more than 1 position.	*/
		out_flag=TRUE;
		return(SUCCESS);
	}

	temp = vb_pure;									/* Save the current pureness.		*/
	vb_pure = TRUE;									/* Now actually output linefeeds.	*/

	if      (nrows == -1) { vcontrol(vcapdef[CURSOR_UP]); out_flag=TRUE; }		/* Going up 1 position.			*/
	else if (nrows <   0)
	{
		vcontrol(PARMFUNC(vcapdef[CURSOR_ADDRESS],nl,nc));			/*  more than 1 position?	*/
		out_flag=TRUE;
	}
	else if (nrows ==  1) vmv_out(JUST_TEXT,"\n");					/* Going down 1 position.		*/
	else if (nrows ==  2) vmv_out(JUST_TEXT,"\n\n");				/* Going down 2 positions?		*/
	else if (nrows ==  3) vmv_out(JUST_TEXT,"\n\n\n");				/* Going down 3 positions.		*/
	else if (nrows >   0) 
	{
		vcontrol(PARMFUNC(vcapdef[CURSOR_ADDRESS],nl,nc));			/*  more than 1 position.	*/
		out_flag=TRUE;
	}
	vb_pure = temp;									/* Now restore pureness state.		*/

	return(SUCCESS);								/* Return that we did it.		*/
}

/*					Calculate how many characters to do a move.						*/

int vmv_dist(nl,nc,ol,oc,op) int ol, oc, nl, nc, op;					/* From old to new location.		*/
{
	register int dist, horiz, vert;							/* Working registers.			*/

	if (op && (ol == nl) && (oc == nc)) return(0);					/* Nothing to do.			*/

	dist = vmv_max(nl,nc,ol);							/* Get the size of a worst case move.	*/

	if (!op) return(dist);								/* That is it if not optimizing.	*/

	if (!vmv_traverse(ol,nl))							/* Can we slew and stay in scroll area?	*/
	{
		if (((ol+1) < MAX_LINES_PER_SCREEN) && (vmv_compatible(nl,ol+1)))	/* Can we scroll down?			*/
		{
			if ((vert = vmv_horiz(nc,0,ol+1)+vmv_vert(nl,ol+1)+2) < dist)	/* Yes, we can use a newline.		*/
 			dist = vert;							/* Record the distance.			*/
			lead_in = '\n';							/* Tag the leadin character.		*/
		}

		if (vmv_compatible(nl,ol))						/* Do we have compatible line sizes?	*/
		{
			if ((vert = vmv_horiz(nc,0,ol)+vmv_vert(nl,ol)+1) < dist)	/* Can we use a return?			*/
			{
				dist = vert;						/* Record the distance.			*/
				lead_in = '\015';					/* Tag a return leadin character.	*/
			}
			horiz = vmv_horiz(nc,oc,ol);					/* How many chars in horizontal dirn?	*/
			vert  = vmv_vert(nl,ol);					/* How many chars in veritcal dirn?	*/
			if ((horiz+vert) < dist) 					/* Faster to go via slewing?		*/
			{
				dist = horiz + vert;					/* Yes so select direct slew.		*/
				lead_in = 0;						/* And reset the lead-in character.	*/
			}
		}
	}

	return(dist);									/* Return the distance.			*/
}

vmv_horiz(nc,oc,ol) int nc,oc,ol;							/* Determine horizontal distance.	*/
{
	register int horiz;								/* Horizontal distance in chars.	*/

	horiz = vmvsd(nc,oc);								/* Get the slew distance.		*/
	if ((nc < oc) && ((oc-nc) < 4)) horiz = oc - nc;				/* Can we do it with back spaces?	*/
	if ((oc < nc) && ((nc-oc) < 4) && vmv_same(ol,oc,nc-oc)) horiz = nc - oc;	/* Can we do it from the map?		*/
	return(horiz);

}

vmv_same(ol,oc,cnt) int ol, oc, cnt;							/* Check if attributes the same.	*/
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
	
vmv_vert(nl,ol) int nl,ol;								/* Determine vertical distance.		*/
{
	register int vert;								/* Vertical distance.			*/

	vert = vmvsd(nl,ol);								/* Get the slew distance.		*/
	if ((nl > ol) && ((nl-ol) < 4)) vert = nl - ol;					/* Can we do it with line feeds?	*/
	return(vert);									/* Return vertical distance.		*/
}

/*				Subroutine to calculate how many characters a slew will take					*/

vmvsd(n,o) int n, o;								/* Determine the distance a slew takes.	*/
{
	register int i;									/* Working register.			*/

	if (o == n) return(0);								/* Return if no slew to do.		*/

	if (n <= o) i = vmvsx(o-n);							/* Get the positive slew count.		*/
	else i = vmvsx(n-o);								/* Get the negative slew count.		*/

	return(i);									/* Return the distance to the caller.	*/
}

vmvsx(i) int i;
{
	register int j;									/* Working register.			*/
	if      (i == 1) j = 3;								/* Takes 3 digits for 0 or 1.		*/
	else if (i < 10) j = 4;								/* How many digits will it take?	*/
	else             j = 5;
	return(j);
}

/*				Subroutine to calculate the worst case move size.						*/

vmv_max(nl,nc,ol) int nl,nc,ol;								/* Calculate max worst case move.	*/
{
	register int dist;								/* Working register.			*/

	dist = 6;									/* Minimum move escape sequence is 6.	*/
	if (nl >=  9) dist++;								/* Add one if line 10 or beyond.	*/
	if (nc >=  9) dist++;								/* Add one if column 10 or beyond.	*/
	if (nc >= 99) dist++;								/* Add one more if column beyond 99.	*/
	if (!vmv_compatible(nl,ol)) dist = dist*2;					/* Are the lines compatible widths?	*/
	return(dist);									/* Return the value he/she wants.	*/
}

/*					Subroutine to determine if line sizes are compatible.					*/

vmv_compatible(i,j) int i,j;
{

	if ((vlin_atr[i] == SINGLE_WIDTH) && (vlin_atr[j] == SINGLE_WIDTH)) return(TRUE);	/* Lines are compatible.	*/
	if ((vlin_atr[i] != SINGLE_WIDTH) && (vlin_atr[j] != SINGLE_WIDTH)) return(TRUE);	/* Lines are compatible.	*/

	return(FALSE);									/* How about that, not compatible.	*/
}

/*					Subroutine to encode the string to be output.						*/

int vmv_out(how, string,arg1,arg2,arg3) int how; char *string; int arg1, arg2, arg3;	/* Allow only three args.		*/
{
	char temp[20];									/* Temporary working string.		*/
	int r,c;									/* Temporary variables.			*/

	if ((!vmovebias) || (how == JUST_TEXT)) sprintf(temp,string,arg1,arg2,arg3);	/* Encode the output string ANSI?	*/
	else										/* Do a biased encoding.		*/
	{
		if (how == ABSOLUTE)
		{
			r = (arg1 + vmovebias - 1);
			c = (arg2 + vmovebias - 1);
		}
		else if (how == GO_UP)
		{
			r = (vcur_lin + vmovebias) - 1;
			c = (vcur_col + vmovebias);
		}
		else if (how == SLEW_UP)
		{
			r = (vcur_lin + vmovebias) - arg1;
			c = (vcur_col + vmovebias);
		}
		else if (how == SLEW_DOWN)
		{
			r = (vcur_lin + vmovebias) + arg1;
			c = (vcur_col + vmovebias);
		}
		else if (how == SLEW_RIGHT)
		{
			r = (vcur_lin + vmovebias);
			c = (vcur_col + vmovebias) + arg1;
		}
		else if (how == SLEW_LEFT)
		{
			r = (vcur_lin + vmovebias);
			c = (vcur_col + vmovebias) - arg1;
		}
		sprintf(temp,mvrowcol_esc,r,c);						/* Encode the position.			*/
	}
	vcontrol(temp);									/* Output it.				*/
	out_flag = TRUE;								/* Output was done.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

/*				Subroutine to determine if a move goes across the scroll area.					*/

int vmv_traverse(ol,nl) int ol,nl;							/* ol = old line,  nl = new line.	*/
{
	register int ret;								/* Working register.			*/

	ret = FALSE;									/* Assume not a problem.		*/
	if ( (ol >= vrol_top) && (ol <= vrol_bot) )					/* Is the old position in scroll area?	*/
	{
		if ( (nl < vrol_top) || (nl > vrol_bot) ) ret = TRUE;			/* Yes, then is the new line outside?	*/
	}

	return(ret);									/* Now report to the caller.		*/
}
#endif	/* VMS or unix */

/*				Subroutine to update the tracking flags.							*/

int vmv_ud(line,column) register int line, column;					/* Update global position values.	*/
{
	vcur_lin = line;								/* Remember where we are.		*/
	vcur_col = column;
	if (out_flag)									/* Was there actual output?		*/
	{
		tcur_lin = vcur_lin;							/* Yes, then update true position too.	*/
		tcur_col = vcur_col;
	}
	return(SUCCESS);								/* Return to the caller.		*/
}
