			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			    Copyright (c) 1990				*/
			/*    An unpublished work by Gregory L. Adams.  All rights reserved.	*/
			/************************************************************************/

/*						Include standard header files.							*/

#include <v/video.h>									/* Include video definitions.		*/
#include <v/vlocal.h>									/* Include local definitions.		*/
#include <v/vdata.h>									/* Include video data base.		*/
#include <v/vcap.h>

/*						Subroutine entry point.								*/

int wsmove(line,column) int line, column;						/* Move to a location on the screen.	*/
{
#ifdef	MSDOS
	vrawmove(line,column);
#else	/* VMS or unix */

	unsigned char *am;								/* Pointer to the attribute map.	*/
	register int i,k,m,ok;								/* Working parameters.			*/
	unsigned char temp[MAX_ESC];							/* Temporary working string.		*/
	unsigned char *vcparm();
	
	if (!vmov_op)
	{
		vcontrol(vcparm(vcapdef[CURSOR_ADDRESS],line,column));			/* Move to the position.		*/
		vmov_op = TRUE;								/* Now we can optimize.			*/
	}

	else if ((line == vcur_lin) && (column == vcur_col));				/* Anything to move?			*/

	else if (line == vcur_lin)							/* On the same line?			*/
	{
		if ((m = column - vcur_col) > 0) 					/* Going right?				*/
		{
			if (m <= 3) 							/* Just moving one position?		*/
			{
				k = vml(line);						/* Get the map index.			*/
				am = &vatr_map[k][column-m];				/* Point to what we should have output.	*/
				ok = TRUE;						/* Assume we can do this.		*/
				for (i = 0; i < m; i++)					/* Check the rendition for all chars.	*/
				{
					if (*(am++) != vcur_atr|vchr_set) ok = FALSE;	/* Did the rendition change?		*/
				}
				if (!ok)						/* Do we have to do a real slew?	*/
				{
					strcpy(temp,vcparm(vcapdef[CURSOR_ADDRESS],vcur_lin,vcur_col+m));
#if 0
					if (!vmovebias) sprintf(temp,mvright_esc,m);	/* Yes do a real slew.			*/
					else sprintf(temp,mvrowcol_esc,vcur_lin+vmovebias,vcur_col+m+vmovebias);
#endif
				}
				else
				{
					memcpy(temp,&vchr_map[k][column-m],m);		/* No, then output the map characters.	*/
					temp[m] = '\000';				/* Store a null terminaltor.		*/
				}
			}
			else
			  strcpy(temp,vcparm(vcapdef[CURSOR_ADDRESS],vcur_lin,vcur_col+m));
#if 0
			  if (!vmovebias) sprintf(temp,mvright_esc,m);		/* Slew to the right.			*/
			  else sprintf(temp,mvrowcol_esc,vcur_lin+vmovebias,vcur_col+m+vmovebias);
#endif
		}
		else									/* Going left?				*/
		{
			m = -m;								/* Make positive.			*/
			if (m > 3) 							/* Large distance?			*/
			{
				strcpy(temp,vcparm(vcapdef[CURSOR_ADDRESS],vcur_lin,vcur_col-m));
#if 0			       
				if (!vmovebias) sprintf(temp,mvleft_esc,m);		/* Slew to the left.			*/
				else sprintf(temp,mvrowcol_esc,vcur_lin+vmovebias,vcur_col-m+vmovebias);
#endif
			}
			else
			{
				for (i = 0; i < m; i++) temp[i] = '\b';			/* Store backspaces.			*/
				temp[i] = '\000';					/* Store a null.			*/
			}
		}
		vcontrol(temp);								/* Output it.				*/
	}

	else if ((column == 0) && (line >= vcur_lin))					/* Go to the start of a line?		*/
	{
		vcontrol("\015");							/* Output a carriage return.		*/
		for (i = 1; i <= line-vcur_lin; i++) vcontrol("\012");			/* Output a linefeed.			*/
	}

	else										/* Just do a pure ANSI move.		*/
	{
		vcontrol(vcparm(vcapdef[CURSOR_ADDRESS],line,column));
#if 0
		if (!vmovebias) sprintf(temp,mvrowcol_esc,line+1,column+1);		/* Move to the position.		*/
		else sprintf(temp,mvrowcol_esc,line+vmovebias,column+vmovebias);	/* Move to the position.		*/
		vcontrol(temp);								/* Output the data.			*/
#endif
	}

#endif	/* VMS or unix */

	vcur_lin = line;								/* Remember where we are.		*/
	vcur_col = column;
	return(SUCCESS);								/* Return that we did it.		*/
}
