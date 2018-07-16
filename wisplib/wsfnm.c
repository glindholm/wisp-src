/*
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
*/

/*
**	File:		wsfnm.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Emulation of NetronCAP system subroutine
**
**	Routines:	
*/

		/****************************************************************************************
		*	WSFNM.C	- Emulation of NetronCAP system subroutine 				*
		*                                                                    			*
		*    This routine is used to handle all virtual 'Long screen' screen I/O.		*
		*    Concept whereby one logical screen is defined by the programmer, which is split	*
		*    into multiple physical screens when executed.  The 'enter' gets the user from the	*
		*    first physical screen to the next, and so on.  The actual action key (eg. add 	*
		*    record, delete, modify) takes effect on the last physical screen.			*
		*											*
		*    WSFNM function used by CAPscreen:							*
		*	AM	Passes a long screen.  Calls DS and AS in WSFNS and displays 24 line	*
		*		fragments of the screen.						*
		*											*
		*	OM	Initializes by defining the length of the screen, number of lines that	*
		*               remain constant at top and bottom of screen, sets message to tell user	*
		*		how to move within the screen.						*
		*											*
		*	PM	Passes through a PF call to WSFNS.					*
		*											*
		*    The calling format from COBOL programs is as follows:				*
		*                                                      TYPE  LEN  			*
		*	For functions AM -								*
		*	CALL "WSFNM" using 	Z1-FN,		   	C     2   function to perform	*
		*				Z1-REC,			C  X 400  screen record image	*
		*				Z1-LINE,		B     2   posn cursor at row	*
		*				Z1-POS,			B     2   posn cursor at column *
		*				Z1-BEEPFLAG		C     1   id to beep or not.	*
		*	For functions OM -								*
		*	CALL "WSFNM" using 	Z1-FN,		   	C     2   function to perform	*
		*				Z1-SCREENLINES,		B     2   # of lines in screen	*
		*				Z1-SFIRST,		B     2	  1st line scroll reg.	*
		*				Z1-SLAST,		B     2   Last line scroll reg.	*
		*				Z1-MESSAGE,		C    80   Constant message 	*
		*				Z1-MESSAGELINE		B     2   Not used - Line to	*
		*								   print message	*
		*	For function PM -								*
		*	CALL "WSFNM" using 	Z1-FN,		   	C     2   function to perform	*
		*				Z1-PF,			B     2   return PFkey pressed	*
		*				Z1-ROW,			B     2   current cursor row	*
		*				Z1-COL,			B     2   current cursor column *
		*                                                                    			*
		****************************************************************************************/

/*
**	Includes
*/

#include <stdio.h>									/* Allow standard I/O.			*/
#include <stdarg.h>									/* Allow variable number of arguments	*/
#include <stdlib.h>
#include <string.h>

#include "idsistd.h"
#include "vwang.h"
#include "werrlog.h"
#include "wglobals.h"
#include "wisplib.h"
#include "wsfns.h"

/*
**	Globals and Externals
*/
extern char WLNC_pfkey[3], WLNC_order_area[4];						/* Define global in WSFNS so accessible	*/
extern int  WLNC_toggle;								/* Define global in WSFNS so accessible */
extern char WLNC_cur_toggle_val;							/* Var to hold current toggle value.	*/

/*
**	Static data
*/
static unsigned char *wsfnm_cscn;							/* Current pos in long screen pointer.	*/
static short screen_len, scrl_len;							/* Values for each screen setup.	*/
static short st_scrl, end_scrl;

/*
**	Static Function Prototypes
*/
static void do_erase();
static void blank_msg_line();


int WSFNM(char* z1_fn, ...)
{
	va_list		the_args;							/* A pointer to traverse the stack.	*/
	int		arg_count, i;							/* Ptrs and local copies of passed param.*/
	unsigned char	*z1_rec, *z1_beepfl;						/* Input/output params from WSFNM call.	*/
        short           *z1_pf, *z1_col, *z1_row; 
	short		*z1_screenlines, *z1_slast, *z1_sfirst, *z1_line, *z1_pos;
	short		realline;
	char 		func_type[3];							/* Local string copy of function type.	*/
	int		another_screen, snum, l_used;
	short		cur_scrl_len;
	char		wsfns_func[3];

	WL_wtrace("WSFNM","ENTRY","Entry into WSFNM Function=[%2.2s]",z1_fn);

	WLNC_check_first_time_netroncap();						/* Check environment if first time in.	*/
	wisp_set_progname("");								/* Set the progname name to spaces.	*/
	wisp_set_screenname("");							/* Set the screen name to spaces.	*/

	va_start(the_args, z1_fn);							/* Set pointer to top of stack.		*/
	arg_count = WL_va_count();							/* How many args are there ?		*/
	if (arg_count == 0)								/* No arguments passed.			*/
	{
		werrlog(WERRCODE(85504),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call. Record message.	*/
		return(16);
	}
	/* z1_fn = va_arg(the_args, unsigned char *); */				/* Get function type from arg stack.	*/
	arg_count--;									/* One less argument.			*/
	for (i = 0; i < 2; i++)  func_type[i] = *z1_fn++;				/* Copy function to variable.		*/
	func_type[i] = '\0';								/* Null terminate the string.		*/
	WL_wtrace("WSFNM","ENTRY","Entry into WSFNM(%2.2s)",func_type);
	if (memcmp(func_type,"OM",2) == 0)						/* Is it function OM?			*/
	{
		if (arg_count != 5)
		{
			werrlog(WERRCODE(85502),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call. Record message.	*/
			return(16);
		}
		z1_screenlines = va_arg(the_args, short *);
		screen_len = *z1_screenlines;						/* Assing number of lines in screen.	*/
		z1_sfirst = va_arg(the_args, short *);
		st_scrl = *z1_sfirst;							/* Assign start of scroll region.	*/
		z1_slast = va_arg(the_args, short *);
		end_scrl = *z1_slast;							/* Assign end of scroll region.		*/
		scrl_len = 24 - ((st_scrl-1) + (screen_len - end_scrl));		/* Compute the # lines in scroll region.*/
	}
	else if (memcmp(func_type,"AM",2) == 0)						/* Is it function AM?			*/
	{
		if (arg_count != 4)
		{
			werrlog(WERRCODE(85502),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call. Record message.	*/
			return(16);
		}
		z1_rec = va_arg(the_args, unsigned char*);				/* Get the pointer to the screen.	*/
		z1_line = va_arg(the_args, short *);					/* Get the virtual row position.	*/
		if (*z1_line == 0) realline = 0;					/* If zero, don't force a cursor pos.	*/
		else if (*z1_line < st_scrl) realline = *z1_line;			/* Are we to position in the header?	*/
		else if (*z1_line > end_scrl) realline = 24 - (screen_len - *z1_line);	/* Are we to position in the trailer?	*/
		else if (*z1_line <= screen_len)					/* Compute position if on virt screen.	*/
		{
			 realline = st_scrl + ((*z1_line - st_scrl) - (((*z1_line - st_scrl) / scrl_len) * scrl_len));
		}
		else
		{
			werrlog(WERRCODE(85508),func_type,*z1_line,0,0,0,0,0,0);		/* Invalid row. Record message.	*/
			return(16);
		}

		z1_pos = va_arg(the_args, short *);					/* Get the column to position to.	*/
		if ((*z1_pos < 0) || (*z1_pos > 80))
		{
			werrlog(WERRCODE(85506),func_type,*z1_pos,0,0,0,0,0,0);		/* Invalid column. Record message.	*/
			return(16);
		}

		z1_beepfl = va_arg(the_args, unsigned char*);				/* Assign flag to beep. 		*/

		WLNC_hfio((unsigned char)WRITE_ALL,z1_rec);				/* Display header and footer.		*/

		if ((*z1_line < st_scrl) || (*z1_line > end_scrl)) 			/* Use the first scroll region?		*/
		{
			snum = 1;							/* Assign the current page umber.	*/
			wsfnm_cscn = &z1_rec[(st_scrl-1)*80];				/* Yes, then select that position.	*/
		}
		else									/* No, then compute which screen.	*/
		{
			snum = 1 + ((*z1_line - st_scrl) / scrl_len);			/* Assign the current page umber.	*/
			wsfnm_cscn = &z1_rec[(((((*z1_line - st_scrl) / scrl_len) * scrl_len) + st_scrl) - 1) * 80];
		}

		cur_scrl_len = scrl_len;
		another_screen = 1;
		while (another_screen)
		{
			strcpy(wsfns_func,"DS");					/* Save screen for PF30 func.		*/
			WL_set_va_count(6);
			WSFNS(wsfns_func, wsfnm_cscn, &realline, z1_pos, z1_beepfl, &cur_scrl_len);
			strcpy(wsfns_func,"AS");					/* Display and read screen.		*/
			WL_set_va_count(7);
			WSFNS(wsfns_func, wsfnm_cscn, &realline, z1_pos, z1_beepfl, &st_scrl, &cur_scrl_len);
			if (memcmp(WLNC_pfkey,"02",2) == 0) 	 			/* Go to top of scroll region by PF2.	*/
			{
				snum = 1;						/* Set to first page.			*/
				wsfnm_cscn = &z1_rec[(st_scrl-1)*80];			/* Set ptr to beginning of scroll reg.	*/
				cur_scrl_len = scrl_len;				/* Set back to full scroll region.	*/
			}
			else if (memcmp(WLNC_pfkey,"00",2) != 0)			/* Return processing to COBOL program.	*/
			{								/* so far in screen.			*/
				another_screen = 0;					/* No more screens to display or not a	*/
			}
			else /* ENTER KEY PRESSED */
			{								/* Calculate the number of lines used	*/
				l_used = (st_scrl - 1 ) + (snum*scrl_len) + (screen_len - end_scrl);
				snum++;							/* Increment number of display screen.	*/
				if (l_used >= screen_len)				/* At the end of the screen.		*/
				{
					another_screen = 0;				/* Return to caller.			*/
				}
				else							/* Set to correct page of scroll region.*/
				{
					blank_msg_line();				/* Do erase of message line.		*/
					wsfnm_cscn = &z1_rec[((snum - 1)*(scrl_len*80) + (st_scrl-1)*80)];
					i = screen_len - l_used;			/* Calculate lines in current screen.	*/
					if (i < scrl_len)				/* If last page is less than scroll	*/
					{						/* region.				*/
						cur_scrl_len = i;			/* Set to what it actually left.	*/
						do_erase(i);				/* Do erase of bottom of scroll region.	*/
					}
				}
				*z1_beepfl = ' ';					/* Set so doesn't beep on next screen.	*/
				*z1_pos = 0;						/* Set so positions on 1st mod field.	*/
				*z1_line = 0;
			}
			if (WLNC_toggle)
			{
				WLNC_hfio((unsigned char)WRITE_ALL,z1_rec);		/* Display header and footer.		*/
			}
		}
		WLNC_hfio((unsigned char)READ_ALL,z1_rec);				/* Restore header and footer info.	*/
	}
	else if (memcmp(func_type,"PM",2) == 0)						/* Is it function PM?			*/
	{										/* Return cursor position and PFkey.	*/
		strcpy(wsfns_func,"PF");
		z1_pf = va_arg(the_args, short *);					/* Get address of pfkey passed.		*/
		z1_row = va_arg(the_args, short *);					/* Get address of cursor row.		*/
		z1_col = va_arg(the_args, short *);					/* Get address of cursor column.	*/
		WL_set_va_count( 4 );
		WSFNS(wsfns_func,z1_pf,z1_row,z1_col);					/* Make a call to WSFNS.		*/
		return(0);
	}
	return(0);
}

void WLNC_hfio(function, scn_ptr)							/* Call VWANG to display header and	*/
unsigned char function;									/* Function to be performed.		*/
unsigned char *scn_ptr;									/* footer areas.			*/
{
	char		*terminate_list;						/* Parameters for vwang			*/
	unsigned char	*ldispa, l_numl;						/* Local pointers for vwang.		*/
	char		wcc, save_oa[4];						/* WANG order area for screen display	*/
	unsigned char 	vw_mod[2];							/* Parameters for vwang PFkey pressed.	*/
	int		st_row, i;

	st_row = 1;									/* Set the starting header line #.	*/
	wcc = POSITION_CURSOR;								/* Assign Write Control Character	*/

	WLNC_order_area[ROW_NUMBER_BYTE]	= (unsigned char)st_row;	   	/* Starting screen line number for data.*/
	WLNC_order_area[WCC_BYTE]            = wcc;					/* Set the WCC byte.			*/
	WLNC_order_area[CURSOR_COL_BYTE]     = (unsigned char)1;			/* Position cursor at column.		*/
	WLNC_order_area[CURSOR_ROW_BYTE]     = (unsigned char)1;			/* Position cursor at row.		*/

	ldispa = (scn_ptr - 4);								/* Point to display area minus 4 bytes.	*/
	for (i = 0; i < 4; i++) save_oa[i] = ldispa[i];					/* Save 4 bytes of program data area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = WLNC_order_area[i];				/* Concatenate order area with screen.	*/
	l_numl = st_scrl - 1;								/* Set the number of lines to display.	*/

	if (WLNC_toggle)
	{
		ldispa[4] = WLNC_cur_toggle_val;					/* Move current toggle value into 	*/
	}										/*  (1,1) header area of screen.	*/
	terminate_list = "A";
	vwang(&function, ldispa, &l_numl, terminate_list, WLNC_pfkey, vw_mod);

	WLNC_pfkey[2] = '\0';								/* Null terminate the string.		*/
	for (i = 0; i < 4; i++) WLNC_order_area[i] = ldispa[i];				/* Save the returned vwang order area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = save_oa[i];					/* Restore program data area.		*/

	st_row = st_scrl + scrl_len;							/* Set the starting footer line #.	*/
	WLNC_order_area[ROW_NUMBER_BYTE]	= (unsigned char)st_row;	   	/* Starting screen line number for data.*/

	ldispa = &scn_ptr[end_scrl*80];							/* Set to address of footer area.	*/
	ldispa = (ldispa - 4);								/* Point to display area minus 4 bytes.	*/
	for (i = 0; i < 4; i++) save_oa[i] = ldispa[i];					/* Save 4 bytes of program data area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = WLNC_order_area[i];				/* Concatenate order area with screen.	*/
	l_numl = screen_len - end_scrl;							/* Set the number of lines to display.	*/
	if (WLNC_toggle)
	{
		int temp;

		temp = (l_numl * 80) + 3;						/* Calculate position in footer.	*/
		ldispa[temp] = WLNC_cur_toggle_val;					/*  Move current toggle val into (24,80)*/
		WLNC_set_toggle_value();
	}

	if (EDE_using())
	{
		gen_ncpfkey(0,(char**)&ldispa,l_numl*80,NULL,NULL);			/* Set up PFkey window if have EDE.	*/
	}
											/* Don't need st_win and end_win because*/
	vwang(&function, ldispa, &l_numl, terminate_list, WLNC_pfkey, vw_mod);		/* temp screen is just pfkey area.	*/

	WLNC_pfkey[2] = '\0';								/* Null terminate the string.		*/
	for (i = 0; i < 4; i++) WLNC_order_area[i] = ldispa[i];				/* Save the returned vwang order area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = save_oa[i];					/* Restore program data area.		*/
}

static void do_erase(num_scrl_lines)							/* Call VWANG to erase bottom of scroll	*/
int num_scrl_lines;									/* region.				*/
{
	unsigned char	function;							/* Parameters for vwang			*/
	unsigned char	*edispa, e_numl;						/* Local pointers for vwang.		*/
	char		wcc;								/* WANG order area for screen display	*/
	unsigned char 	vw_mod[2];							/* Parameters for vwang PFkey pressed.	*/
	int		st_row;
	char 		blscreen[1924];							/* Generate a blank screen to pass to	*/
											/* VWANG.				*/
	memset(blscreen,' ',sizeof(blscreen));						/* Init the screen to all balnks.	*/
	wcc = (char)(UNLOCK_KEYBOARD | POSITION_CURSOR);				/* Assign Write Control Character	*/
	st_row = st_scrl + num_scrl_lines;						/* Calculate the starting row.		*/

	blscreen[ROW_NUMBER_BYTE]	= (char)st_row;	   				/* Starting screen line number for data.*/
	blscreen[WCC_BYTE]            = wcc;						/* Set the WCC byte.			*/
	blscreen[CURSOR_COL_BYTE]     = (char)1;					/* Position cursor at column.		*/
	blscreen[CURSOR_ROW_BYTE]     = (char)1;					/* Position cursor at row.		*/

	edispa = (unsigned char *)blscreen;						/* Point to display area plus 4 bytes.	*/
	e_numl = scrl_len - num_scrl_lines;						/* Set the number of lines to display.	*/

	function = WRITE_ALL;								/* Display blank lines in scroll region.*/
	vwang(&function, edispa, &e_numl, NULL, WLNC_pfkey, vw_mod);
	WLNC_pfkey[2] = '\0';								/* Null terminate the string.		*/
}

static void blank_msg_line()								/* Call VWANG to erase message line.	*/
{
	unsigned char	function;							/* Parameters for vwang			*/
	char		terminate_list[2];
	unsigned char	*edispa, e_numl;						/* Local pointers for vwang.		*/
	char		wcc;								/* WANG order area for screen display	*/
	unsigned char 	vw_mod[2];							/* Parameters for vwang PFkey pressed.	*/
	int		st_row;
	char		blank_msg[80+4+1];						/* Blank message line for VWANG.	*/
	unsigned char	*cptr;
	int		idx;

	/*
	**	NOTE:   I do not dig why we read the message line then
	**		set it to blanks up to the first FAC (after column 1)
	**		then rewrite it -- why not just set to blanks and write ???
	*/

	strcpy((char *)terminate_list,"A");
	memset((char *)blank_msg,' ',sizeof(blank_msg));				/* Init the line to all blanks.		*/
	blank_msg[sizeof(blank_msg)-1] = '\0';						/* Null terminate the string.		*/
	wcc = POSITION_CURSOR;								/* Assign Write Control Character	*/
	st_row = st_scrl + scrl_len;							/* Calculate the message starting row.	*/

	if (st_row > 24) return;							/* No MESSAGE LINE so return.		*/

	blank_msg[ROW_NUMBER_BYTE]	= (unsigned char)st_row;	   		/* Starting screen line number for data.*/
	blank_msg[WCC_BYTE]            = wcc;						/* Set the WCC byte.			*/
	blank_msg[CURSOR_COL_BYTE]     = (unsigned char)1;				/* Position cursor at column.		*/
	blank_msg[CURSOR_ROW_BYTE]     = (unsigned char)1;				/* Position cursor at row.		*/

	edispa = (unsigned char *)blank_msg;						/* Point to msg line plus order area.	*/
	e_numl = 1;									/* Set the number of lines to read.	*/

	function = READ_ALL;			    					/* Read message line from screen.	*/
	vwang(&function, edispa, &e_numl, terminate_list, WLNC_pfkey, vw_mod);

	cptr = edispa;									/* Point to message line.		*/
	cptr += 4;									/* Step past order area.		*/
	cptr[0] = ' ';									/* Set the FAC to space.		*/
	for(idx=1; idx<80 && ((cptr[idx] & FAC_CHARACTER) != FAC_CHARACTER); idx++)
	{
		cptr[idx] = ' ';							/* Set message line to blank.		*/
	}

	function = WRITE_ALL;								/* Display blank message line.		*/
	e_numl = 1;									/* Set the number of lines to read.	*/
	vwang(&function, edispa, &e_numl, terminate_list, WLNC_pfkey, vw_mod);
}

/*
**	History:
**	$Log: wsfnm.c,v $
**	Revision 1.27  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.26  2003/02/21 20:29:21  gsl
**	Switch to stdarg.h
**	
**	Revision 1.25  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.24  2002/12/10 20:54:07  gsl
**	use WERRCODE()
**	
**	Revision 1.23  2002/12/09 21:09:34  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.22  2002/08/01 15:07:35  gsl
**	type warnings
**	
**	Revision 1.21  2002/08/01 14:45:10  gsl
**	type warnings
**	
**	Revision 1.20  2002/07/16 16:24:51  gsl
**	Globals
**	
**	Revision 1.19  2002/07/12 20:40:42  gsl
**	Global unique WL_ changes
**	
**	Revision 1.18  2002/07/12 17:01:05  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.17  2002/07/11 20:29:19  gsl
**	Fix WL_ globals
**	
**	Revision 1.16  2002/07/10 21:05:37  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  1996/07/15 17:21:43  gsl
**	Fix argument warning
**	
**	Revision 1.14  1996-07-11 16:56:10-07  gsl
**	fix includes and return values for NT
**
**	Revision 1.13  1995-08-22 06:58:23-07  gsl
**	fix warnings
**
 * Revision 1.12  1995/08/14  09:50:16  scass
 * Added test for message line > 24.  If is then doesn't
 * try to display it.  Assumes there is no message line.
 *
 * Revision 1.11  1995/07/21  11:33:00  gsl
 * Fixed bug in blank_msg_line() in a while() loop which was setting
 * the line to blanks until a FAC was found -- if no fac found it
 * would run off the end.
 *
**
**
*/
