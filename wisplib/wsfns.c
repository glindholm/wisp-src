/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


		/****************************************************************************************
		*	WSFNS.C	- Emulation of NetronCAP system subroutine for NISSI			*
		*                                                                    			*
		*    This routine is used to handle all screen I/O for NISSI Technologies.		*
		*    WSFNS function used by CAPscreen:							*
		*	DS (Display Screen)		Saves the screen image for use with PF30	*
		*					restart/refress function.			*
		*	AS (Accept Screen)		Displays the stored screen, and accepts the	*
		*					modified screen.				*
		*	PF				Returns the cursor position and the PFkey	*
		*					pressed by the user.				*
		*    Other WSFNS functions:								*
		*	DI (Display immediate)		Puts up a screen then returns to processing	*
		*					without waiting for user to press a PFkey.	*
		*	AI (Accept immediate)		Accepts the screen without first rewriting it.	*
		*	ST (Start CRT)			Checks to see if a PFkey has been pressed.  If	*
		*					it has, its value will be returned in Z1-PF.	*
		*					If not, a -1 will be returned. -2 is keyboard	*
		*					locked by rewrite, -3 is HELP, or -10 is 	*
		*					screen damage alert.				*
		*											*
		*    The calling format from COBOL programs is as follows:				*
		*                                                      TYPE  LEN  			*
		*	For functions DS, AS, DI, AI -							*
		*	CALL "WSFNS" using 	Z1-FN,		   	C     2   function to perform	*
		*				Z1-REC,			C    1920 screen record image	*
		*				Z1-LINE,		B     2   posn cursor at row	*
		*				Z1-POS,			B     2   posn cursor at column *
		*				Z1-BEEPFLAG		C     1   id to beep or not.	*
		*	For function PF -								*
		*	CALL "WSFNS" using 	Z1-FN,		   	C     2   function to perform	*
		*				Z1-PF,			B     2   return PFkey pressed	*
		*				Z1-ROW,			B     2   current cursor row	*
		*				Z1-COL,			B     2   current cursor column *
		*	For function ST -								*
		*	CALL "WSFNS" using 	Z1-FN,		   	C     2   function to perform	*
		*				Z1-PF,			B     2   return PFkey pressed	*
		*				Z1-FILESTATUS,		C     2   file status on start	*
		*                                                                    			*
		****************************************************************************************/

#include <stdio.h>									/* Allow standard I/O.			*/
#include <stdlib.h>
#include <stdarg.h>									/* Allow variable number of arguments	*/
#include <ctype.h>
#include <string.h>

#include "idsistd.h"
#include "vwang.h"
#include "werrlog.h"
#include "wglobals.h"
#include "wsfns.h"
#include "wisplib.h"
#include "wexit.h"
#include "level.h"
#include "wmalloc.h"
#include "vssubs.h"

char WLNC_pfkey[3], WLNC_order_area[4];							/* Define global so accessible by all	*/
int  WLNC_toggle = 0;									/*   Flag to indicate toggle function.	*/
char WLNC_cur_toggle_val;								/*   Var to hold current toggle value.	*/

static void save_screen();
static void get_screen();

static int netroncap = 0;								/*   Flag so vwang knows the origin.	*/
static int wsfns_first = 1;								/*   Flag for first time in.		*/

static unsigned char *dsptr;								/* Ptr to holding screen before mods.	*/
static int mem_allocated = 0;								/* Memory not allocated yet.		*/
static int call_fl;									/* Set to who called WSFNS: COBOL or C.	*/

int WSFNS(char* z1_fn, ...)
{
#define		ROUTINE		85700

	va_list		the_args;							/* A pointer to traverse the stack.	*/
	int		arg_count, i_pf, i;						/* Ptrs and local copies of passed param.*/
	unsigned char	*z1_rec, *z1_beepfl;						/* Input/output paramsfrom WSFNS call.	*/
	short		*z1_line, *z1_pos, *z1_col, *z1_row, one_short;			/* Input/output paramsfrom WSFNS call.	*/
	short		*z1_pf;
	char		*terminate_list;						/* Parameters for vwang			*/
	unsigned char	*ldispa;							/* Local pointers for vwang.		*/
	unsigned char	l_numl, l_slen;
	char		wcc, save_oa[4];						/* WANG order area for screen display	*/
	char 		func_type[3];							/* Local string copy of function type.	*/
	unsigned char 	vw_mod[2], function;						/* Parameters for vwang PFkey pressed.	*/

	WL_wtrace("WSFNS","ENTRY","Entry into WSFNS Function=[%2.2s]",z1_fn);

	WLNC_check_first_time_netroncap();						/* Check environment if first time in.	*/
	wisp_set_progname("");								/* Set the progname name to spaces.	*/
	wisp_set_screenname("");							/* Set the screen name to spaces.	*/
	one_short = 1;
	terminate_list = "A";								/* Assign A for ALL PFkeys.		*/
	va_start(the_args, z1_fn);							/* Set pointer to top of stack.		*/
	arg_count = WL_va_count();							/* How many args are there ?		*/
	if (arg_count == 0)								/* No arguments passed.			*/
	{
		werrlog(WERRCODE(85704),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call.  Record error msg.	*/
		return(16);
	}
	/* z1_fn = va_arg(the_args, unsigned char*); */					/* Get function type from arg stack.	*/
	arg_count--;									/* One less argument.			*/
	for (i = 0; i < 2; i++)  func_type[i] = *z1_fn++;				/* Copy function to variable.		*/
	func_type[i] = '\0';								/* Null terminate the string.		*/
	WL_wtrace("WSFNS","ENTRY","Entry into WSFNS(%2.2s)",func_type);
	if ( 	(memcmp(func_type,"DS",2) == 0) ||					/* Is it function WSFNS Display Screen?	*/
		(memcmp(func_type,"AS",2) == 0) ||					/*	Accept Screen?			*/
		(memcmp(func_type,"DI",2) == 0) ||					/*	Display Immediate?		*/
		(memcmp(func_type,"AI",2) == 0) )					/*	Accept Immediate?		*/
	{
		if (arg_count < 4)
		{
			werrlog(WERRCODE(85704),func_type,arg_count,0,0,0,0,0,0);	/* Invalid call.  Record error msg.	*/
			return(16);
		}
		z1_rec = va_arg(the_args, unsigned char*);
		z1_line = va_arg(the_args, short*);					/* Get the row to position to.		*/
		if ((*z1_line < 0) || (*z1_line > 24))					/* Is the row valid?			*/
		{
			werrlog(WERRCODE(85708),func_type,*z1_line,0,0,0,0,0,0);
			return(16);
		}
		z1_pos = va_arg(the_args, short*);					/* Get the column to position to.	*/
		if ((*z1_pos < 0) || (*z1_pos > 80))					/* Is the column valid?			*/
		{
			werrlog(WERRCODE(85706),func_type,*z1_pos,0,0,0,0,0,0);
			return(16);
		}
		z1_beepfl = va_arg(the_args, unsigned char*);				/* Assign flag to beep. 		*/
		if (arg_count > 4)							/* Extra parameters passed from WSFNM.	*/
		{
			short *short_ptr;
			if (memcmp(func_type,"DS",2) == 0)
			{
				short_ptr = va_arg(the_args, short *);			/* Get the number of lines in screen.	*/
				l_slen = (unsigned char) *short_ptr;
				z1_row = &one_short;
			}
			else
			{
				z1_row = va_arg(the_args, short *);			/* Get starting display line.		*/
				short_ptr = va_arg(the_args, short *);			/* Get the number of lines to display.	*/
				l_numl = (unsigned char) *short_ptr;
			}
			call_fl = 1;							/* Was called from C: WSFNM.		*/
		}
		else
		{
			z1_row = &one_short;						/* Set because call is from Cobol so	*/
			l_numl = 24;							/* display full screen defaults.	*/
			l_slen = 24;
			call_fl = 0;							/* Was called from COBOL.		*/
		}
		if (memcmp(func_type,"DS",2) == 0)
		{
			save_screen(z1_rec,(int)l_slen);				/* Save screen so if want to start over.*/
			return(0);							/* Return to COBOL caller.		*/
		}
		wcc = (char)(UNLOCK_KEYBOARD | POSITION_CURSOR);			/* Assign Write Control Character	*/
		if (*z1_beepfl != ' ')  wcc |= SOUND_ALARM;				/* If not blank then beep.		*/
		netroncap = 1;								/* Set so vwang won't use pseudo blanks.*/
restart:										/* Start position if PF30 (start over)	*/
		WLNC_order_area[ROW_NUMBER_BYTE]	= (unsigned char)*z1_row;   	/* Starting screen line number for data.*/
		WLNC_order_area[WCC_BYTE]            = wcc;				/* Set the WCC byte.			*/
		WLNC_order_area[CURSOR_COL_BYTE]     = (unsigned char)*z1_pos;		/* Position cursor at column.		*/
		WLNC_order_area[CURSOR_ROW_BYTE]     = (unsigned char)*z1_line;		/* Position cursor at row.		*/

		ldispa = (z1_rec - 4);							/* Point to display area minus 4 bytes.	*/
		for (i = 0; i < 4; i++) save_oa[i] = ldispa[i];				/* Save 4 bytes of program data area.	*/
	 	for (i = 0; i < 4; i++) ldispa[i] = WLNC_order_area[i];			/* Concatenate order area with screen.	*/

		if (memcmp(func_type,"AS",2) == 0)					/* Accept Screen WSFNS function.	*/
		{
			if (WLNC_toggle && !call_fl)					/* If toggle processing and not WSFNM.	*/
			{
				ldispa[4] = WLNC_cur_toggle_val;			/* Move current toggle value into 	*/
				ldispa[1923] = WLNC_cur_toggle_val;			/*  (1,1) and (24,80) posn's on screen.	*/
				WLNC_set_toggle_value();				/* Toggle the value.			*/
			}
			if (call_fl)							/* Just want to write and read scroll	*/
			{								/*  region from WSFNM call.		*/
				function = WRITE_ALL;
				vwang(&function, ldispa, &l_numl, terminate_list, WLNC_pfkey, vw_mod);
				function = READ_ALL;
				vwang(&function, ldispa, &l_numl, terminate_list, WLNC_pfkey, vw_mod);
			}
			else
			{
				unsigned char *hlddispa, *cptr;
				int st_win,end_win;					/* Start and end of pfkey window.	*/
                                         
				st_win = 0;
				end_win = 0;
				hlddispa = ldispa;					/* Save the current local screen pntr.	*/
				if (EDE_using())
				{
					gen_ncpfkey(1,(char**)&ldispa,1924,&st_win,&end_win);	/* Generate pop up pfkey window with	*/
				}
											/* a temp screen to display.		*/
				function = DISPLAY_AND_READ_ALTERED;
				vwang(&function, ldispa, &l_numl, terminate_list, WLNC_pfkey, vw_mod);

				if (vw_mod[0] == 'M' && (!getenv("PFKEYWINDOW"))) 	/* If mods were made than need to get*/
				{							/* them back into original screen.	*/
					memcpy(hlddispa,ldispa,80*st_win); 		/* Copy top of temp screen to orig.	*/ 
					if (end_win < 24)				/* If there is a bottom screen area.	*/
					{
						cptr = ldispa + 4 + (80 * end_win);	/* Set ptr to bottom area.		*/
						hlddispa = hlddispa + 4 + (80 * end_win);  /* Set ptr to bottom of orig.	*/
						memcpy(hlddispa,cptr,(80*(24 - end_win))); 
											/* Copy temp screen to orig.		*/ 
					}
				}
			}
		}
		else if (memcmp(func_type,"DI",2) == 0)					/* Display Immediate WSFNS function.	*/
		{
			if (WLNC_toggle)						/* If toggle processing set.		*/
			{
				ldispa[4] = WLNC_cur_toggle_val;			/* Move current toggle value into 	*/
				ldispa[1923] = WLNC_cur_toggle_val;			/*  (1,1) and (24,80) posn's on screen.	*/
				WLNC_set_toggle_value();				/* toggle the value.			*/
			}
			function = WRITE_ALL;
			vwang(&function, ldispa, &l_numl, terminate_list, WLNC_pfkey, vw_mod);
		}
		else if (memcmp(func_type,"AI",2) == 0)					/* Accept Immediate WSFNS function.	*/
		{
			function = READ_ALL;
			vwang(&function, ldispa, &l_numl, terminate_list, WLNC_pfkey, vw_mod);
		}
		WLNC_pfkey[2] = '\0';							/* Null terminate the string.		*/
		for (i = 0; i < 4; i++) WLNC_order_area[i] = ldispa[i];			/* Save the returned vwang order area.	*/
		for (i = 0; i < 4; i++) ldispa[i] = save_oa[i];				/* Restore program data area.		*/

		if (memcmp(WLNC_pfkey,"30",2) == 0)					/* Want to start input over.		*/
		{
			get_screen(z1_rec,(int)l_numl);					/* Get the orig. starting screen.	*/
			goto restart;							/* Call VWANG again will no mods.	*/
		}
		if (memcmp(WLNC_pfkey,"31",2) == 0)					/* Spawn a process.			*/
		{
			COBLINK("DOCPROG ");
		}
		netroncap = 0;								/* Set back for normal screens.		*/
	}
	else if (memcmp(func_type,"PF",2) == 0)						/* Is it function PF?			*/
	{										/* Return cursor position and PFkey.	*/
		if (arg_count != 3)
		{
			werrlog(WERRCODE(85702),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call.  Record error msg.	*/
			return(16);
		}
		z1_pf  = va_arg(the_args, short *);					/* Get address of pfkey passed.		*/
		z1_row = va_arg(the_args, short *);					/* Get address of cursor row.		*/
		z1_col = va_arg(the_args, short *);					/* Get address of cursor column.	*/
		i_pf = atoi(WLNC_pfkey);						/* convert PFkey to an integer.		*/
		*z1_pf  = (short)i_pf;							/* Put value into return cobol ptr.	*/
		*z1_col = (short)WLNC_order_area[CURSOR_COL_BYTE];			/* Init for cobol return column.	*/
		*z1_row = (short)WLNC_order_area[CURSOR_ROW_BYTE];			/* Init for cobol return row.		*/
		return(0);
	}
	return(0);
}

static void save_screen(save_rec,scn_len)						/* Save the starting screen config.	*/
unsigned char *save_rec;
int 	scn_len;
{
	int 	nchr;

	nchr = scn_len * 80;								/* Calculate number of chars in screen.	*/

	if (mem_allocated)								/* Memory allocated yet?		*/
	{
		free(dsptr);								/* Free up the old one.			*/
		mem_allocated = 0;
	}

	if (!(dsptr = (unsigned char *)wisp_malloc((unsigned)nchr)))			/* Get memory.				*/
	{
		werrlog(WERRCODE(85710),nchr,0,0,0,0,0,0,0);				/* Oops, cannot get memory.		*/
		wexit(WERRCODE(85710));							/* Unconditionally fatal.		*/
	}
	mem_allocated = 1;								/* Now it is allocated.			*/

	memcpy(dsptr,save_rec,nchr);							/* Save screen record.			*/
}

static void get_screen(curr_rec,num_lines)						/* Copy saved screen to current screen.	*/
unsigned char *curr_rec;
int num_lines;
{
	int 	cnt;

	if (!mem_allocated) vwang_bell(1);						/* Beep if no screen to refresh.	*/
	else
	{
		cnt = num_lines * 80;							/* Set # lines on current screen	*/
		memcpy((char *)curr_rec,(char *)dsptr,cnt);				/* Copy current screen only.		*/
	}
}


void WLNC_check_first_time_netroncap()							/* Check environment if first time in.	*/
{
	char		*tptr;								/* Ptr to toggle environment variable.	*/

	if (wsfns_first)								/* If first time in then see if env.	*/
	{										/* variable is set.			*/
		tptr = getenv("WSFNS_DISPLAY_TOGGLE");
		if (tptr)								/* If environment var exists...		*/
		{
			if (strcmp(tptr,"YES") == 0)					/* If environment var = YES		*/
			{
				WLNC_toggle = 1;					/* Set flag so will toggle on display.	*/
				WLNC_set_toggle_value();
			}
			else	WLNC_toggle = 0;					/* Don't toggle or display lower or 	*/
		}									/*  upper.				*/
		else	WLNC_toggle = 0;						/* Don't toggle or display lower or 	*/
		wsfns_first = 0;							/*  upper.				*/
	}
}

void WLNC_set_toggle_value()								/* Set the toggle value according to	*/
{											/*  the current link level.		*/
	char toggle_mask[27];
	int cllevel;									/* Var for current link level.		*/
	char save_toggle_val;

	save_toggle_val = WLNC_cur_toggle_val;						/* Save original value.			*/
	strcpy(toggle_mask,"abcdefghijklmnopqrstuvwxyz");
	cllevel = WL_linklevel();							/* Set the current link level.		*/
	cllevel--;									/* Subtract one so is 0 based.		*/
	WLNC_cur_toggle_val = toggle_mask[cllevel];
	if (islower((int)save_toggle_val))							/* Now toggle the value.		*/
	{
		WLNC_cur_toggle_val = toupper(WLNC_cur_toggle_val);
	}
}

int WLNC_use_netroncap(void)
{
	return netroncap;
}
/*
**	History:
**	$Log: wsfns.c,v $
**	Revision 1.33  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.32  2003/02/21 20:29:21  gsl
**	Switch to stdarg.h
**	
**	Revision 1.31  2003/02/04 18:43:32  gsl
**	fix -Wall warnings
**	
**	Revision 1.30  2003/02/04 17:23:12  gsl
**	Fix -Wall warnings
**	
**	Revision 1.29  2003/01/31 19:08:36  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.28  2003/01/29 21:50:08  gsl
**	Switch to use vssubs.h
**	
**	Revision 1.27  2002/12/10 20:54:07  gsl
**	use WERRCODE()
**	
**	Revision 1.26  2002/12/09 21:09:34  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.25  2002/08/01 15:31:28  gsl
**	type warnings
**	
**	Revision 1.24  2002/08/01 14:45:10  gsl
**	type warnings
**	
**	Revision 1.23  2002/07/16 16:24:50  gsl
**	Globals
**	
**	Revision 1.22  2002/07/12 20:40:42  gsl
**	Global unique WL_ changes
**	
**	Revision 1.21  2002/07/11 20:29:19  gsl
**	Fix WL_ globals
**	
**	Revision 1.20  2002/07/11 15:21:44  gsl
**	Fix WL_ globals
**	
**	Revision 1.19  2002/07/10 21:05:37  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.18  2002/07/02 21:15:37  gsl
**	Rename wstrdup
**	
**	Revision 1.17  2002/06/25 15:21:55  gsl
**	Change to use wmalloc()
**	
**	Revision 1.16  1996/08/19 22:33:23  gsl
**	drcs update
**	
**
**
*/
