static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*					Include required header files.								*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "video.h"									/* Get the standard VIDEO definitions.	*/
#include "vlocal.h"									/* Get the local video definitions.	*/
#include "vmodules.h"
#include "vprint.h"
#include "vraw.h"
#include "vdata.h"

/*					Static data.										*/

static int tab_stop(int c);
static void vpr_scroll(int sl, int el);							/* Scroll the map.			*/
static int vpr_nl_map(void);
static int vpr_put_map(char c, char* holdbuf);
static void dumpbuf(char *holdbuf);
static int vpr_inc_col(int row, int* pCol);
static int vpr_true(void);


/*					Subroutine entry point.									*/

int vputc(char ch)									/* Put a single character.		*/
{
	return(vprint("%c",ch));							/* Return after printing.		*/
}

int vprint(char *format, ...)
{
	va_list args;
	
	register int i;
	char ascii[PRINT_BUFFER_SIZE];							/* Expanded "pure" ascii string.	*/
	char holdbuf[PRINT_BUFFER_SIZE];						/* Output hold buffer			*/
	
	va_start(args,format);
	vsprintf(ascii,format,args);							/* Convert to a pure string.		*/
	va_end(args);

	if (voptlevel() == VOP_OFF) 
	{
		vrawprint(ascii);		       					/* Are we to track what is going on?	*/
	}
	else
	{
		/*
		**	Note on optimization:
		**
		**	Printing text is never deferred however, some control 
		**	characters are processed here and they may be deferred.
		**	Printing text may be optimized if the data is already in 
		**	the maps, in this case it is changed into a move which
		**	may be deferred.
		*/

		vbuffering_start();							/* Turn on logical buffering.		*/
		synch_required = TRUE;							/* Now output has been done.		*/

		/*
		**	Start deferring motion  (Doesn't matter what voptlevel() we're at.)
		*/
		vdefer(VDEFER_MOTION_ONLY);
		
		holdbuf[0] = '\0';
		
		/*
		**	Put the characters into the maps
		*/
		for (i = 0; (i < PRINT_BUFFER_SIZE) && (ascii[i] != 0); i++)
		{
			switch (ascii[i])
			{
			case '\n': 							/* New line?				*/
				dumpbuf(holdbuf);
				vpr_nl_map();
				break;

			case '\r': 							/* Carriage return?			*/
				dumpbuf(holdbuf);
				vcur_col = 0;						/* Set to the start of the row.		*/
				break;

			case '\b': 							/* Back space?				*/
				dumpbuf(holdbuf);
				vcur_col -= 1;
				if (vcur_col < 0) vcur_col = 0;
				break;

			case '\a': 							/* Bell.				*/
				/* Nothing goes into the the maps - just let it be output */
				dumpbuf(holdbuf);
				vrawputc('\a');
				break;

			case '\t': 							/* Horizontal tab?			*/
				do
				{
					/*
					**	Put at least one space then spaces until tab-stop.
					*/
					vpr_put_map(' ',holdbuf);
				} while (!tab_stop(vcur_col)) ;
				
				break;

			default:							/* Assume printable character		*/
				vpr_put_map(ascii[i],holdbuf);
				break;
			}
		}

		dumpbuf(holdbuf);

		/*
		**	If op level does not defer motions the restore now.
		*/

		if (voptlevel() < VOP_DEFER_MOTION_ONLY)
		{
			vdefer_restore();
		}
		

		vbuffering_end();
	}

	return SUCCESS;
}
/*					Put a character in the character map.							*/

static int vpr_put_map(char c, char* holdbuf)
{
	register unsigned char *chr, *atr, *cng;					/* Working pointers.			*/
	register int i;									/* Working integers.			*/
	char char_str[2];
	int col;

	col = vcur_col + strlen(holdbuf);
	i = vml(vcur_lin);								/* Get array position.			*/
	chr = &vchr_map[i][col];							/* Point to character map element.	*/
	atr = &vatr_map[i][col];							/* Point to attribute map element.	*/
	cng = &vmap_cng[i][col];							/* Point to change map element.		*/

	if ((*chr == c) && (*atr == (vcur_atr | vchr_set)))				/* Is the map changing?			*/
	{
		dumpbuf(holdbuf);
		
		*cng = 0;								/* No change.				*/
		vpr_inc_col(vcur_lin, &vcur_col);					/* Increment the column number.		*/
	}

	else if (!visible(c,vcur_atr) && !visible(*chr,*atr))				/* Visible now and before?		*/
	{
		dumpbuf(holdbuf);

		*chr = c;								/* Store the current char.		*/
		*atr = vcur_atr | vchr_set;						/* Store the current attributes.	*/
		vpr_inc_col(vcur_lin, &vcur_col);					/* Increment the column number.		*/
	}

	else										/* Map changed so...			*/
	{
		*chr = c;								/* Store the current char.		*/
		*atr = vcur_atr | vchr_set;						/* Store the current attributes.	*/

		char_str[0] = c;
		char_str[1] = '\0';
		strcat(holdbuf, char_str);
		
		if (voptlevel() <= VOP_TRACKING_ONLY)
		{
			dumpbuf(holdbuf);
		}
	}

	return(SUCCESS);								/* Return the results.			*/
}

static void dumpbuf(char *holdbuf)
{
	if (!*holdbuf) return;
	
	vdefer_restore();

	vrawprint(holdbuf);

	vcur_col += strlen(holdbuf);
	if (vcur_col >= vedge(vcur_lin))
	{
		vcur_col = vedge(vcur_lin)-1;
	}

	vpr_true();
	vdefer(VDEFER_MOTION_ONLY);
	holdbuf[0] = '\0';
}

/*				Record the occurance of a new-line character.							*/

static int vpr_nl_map(void)
{
	if (vcur_lin+1 >= MAX_LINES_PER_SCREEN) 
	{
		vpr_scroll(0,MAX_LINES_PER_SCREEN-1);					/* Scroll the full screen.	*/
	}
	else if (vcur_lin == vrol_bot) 
	{
		vpr_scroll(vrol_top,vrol_bot);						/* Scroll the scroll region.		*/
	}
	else
	{
		vcur_lin += 1;								/* Set to start of next line.		*/

		if (!vb_pure) 
		{
			vcur_col = 0;							/* Is \n a line-feed or a new-line?	*/
		}
	}

	return(SUCCESS);								/* What could possibly go wrong?	*/
}


/*					Subroutine to scroll the map.								*/

static void vpr_scroll(int sl, int el)							/* Scroll the map.			*/
{
	vcur_lin = el;
	vcur_col = 0;
	vdefer_restore();

	if (vscroll_frwd_avail())
	{
		vmap(SCROLL_UP,sl,0,el,0);						/* Now scroll the map up		*/
		vrawputc('\n');								/* Output new-line or line-feed.	*/
	}
	else
	{
		vmap(SCROLL_UP,sl,0,el,0);						/* Now scroll the map up		*/
		vrefresh(HARD_REFRESH);
	}

	vpr_true();
	vdefer(VDEFER_MOTION_ONLY);
}

/*					Record the occurance of a horizontal tab.						*/

static int tab_stop(int c)
{
/*	NOTE: This assumes 8 column tabs are set on the terminal. Need a better algorithm (later).				*/
	if ((c ==  8) || (c == 16) || (c == 24) || (c == 32)) return(TRUE);		/* Fast routine for checking for	*/
	if ((c == 40) || (c == 48) || (c == 56) || (c == 64)) return(TRUE);		/*  modulo 8.				*/
	if ((c == 72) || (c == 80) || (c == 88) || (c == 96)) return(TRUE);
	if ((c == 104) || (c == 112) || (c == 120) || (c == 128)) return(TRUE);
	if (c >= (vscr_wid-1)) return(TRUE);						/* Assume a tab at edge of screen.	*/
	return(FALSE);	
}

/*					Subroutine to increment the current column.						*/

static int vpr_inc_col(int row, int* pCol)
{
	*pCol += 1;
	if (*pCol >= vedge(row)) *pCol = vedge(row)-1;
	return(SUCCESS);
}


/*					Coordinate true location with current location.						*/

static int vpr_true(void)
{
	tcur_lin = vcur_lin;								/* No, then remember that the current	*/
	tcur_col = vcur_col;								/*   position is the true position.	*/

	return(SUCCESS);
}

/*
**	History:
**	$Log: vprint.c,v $
**	Revision 1.20  1998/04/16 16:06:13  gsl
**	Only do the vdefer_restore if op level doesn't defer motion.
**	This is to fix problem Harte reported trk#403 where screens
**	were being restored char-by-char no optimization.
**	
**	Revision 1.19  1997-07-12 17:43:14-04  gsl
**	Major changes in how optimization is done (or not done)
**	Now hold regular characters into a hold buffer (holdbuf) and only
**	output them when the neede to.
**
**	Revision 1.18  1997-07-09 11:51:37-04  gsl
**	Fix for directio and COSTAR for WIN32.
**	Change to use new video.h interfaces
**	Simplfied the handling of special characters in print stream.
**
**	Revision 1.17  1997-01-09 19:58:38-05  gsl
**	Make local routines static add fix the scroll logic
**
**	Revision 1.16  1996-10-11 15:16:15-07  gsl
**	drcs update
**
**
**
*/
