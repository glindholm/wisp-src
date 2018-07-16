			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

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

#include <varargs.h>									/* Allow variable number of arguments	*/

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif
#ifndef VMS	/* unix or MSDOS */
#include <malloc.h>
#endif

#include <v/video.h>
#include "vwang.h"
#include "werrlog.h"
#include "wglobals.h"

char	*getenv();

char NC_pfkey[3], NC_order_area[4];							/* Define global so accessible by all	*/
int netroncap = 0;									/* Flag so vwang knows the origin.	*/

static unsigned char *dsptr;								/* Ptr to holding screen before mods.	*/
static int mem_allocated = 0;								/* Memory not allocated yet.		*/
static int call_fl;									/* Set to who called WSFNS: COBOL or C.	*/

WSFNS(va_alist)
va_dcl
{
#define		ROUTINE		85700

	va_list		the_args;							/* A pointer to traverse the stack.	*/
	int		arg_count, i_pf, i;						/* Ptrs and local copies of passed param.*/
	unsigned char	*z1_fn, *z1_rec, *z1_beepfl;					/* Input/output paramsfrom WSFNS call.	*/
	short		*z1_line, *z1_pos, *z1_col, *z1_row, one_short;			/* Input/output paramsfrom WSFNS call.	*/
	short		*z1_pf;
	unsigned char	function, *terminate_list;					/* Parameters for vwang			*/
	unsigned char	*ldispa;							/* Local pointers for vwang.		*/
	unsigned char	l_numl, l_slen;
	char		wcc, save_oa[4];						/* WANG order area for screen display	*/
	char 		func_type[3];							/* Local string copy of function type.	*/
	char 		vw_mod[2];							/* Parameters for vwang PFkey pressed.	*/

	werrlog(ERRORCODE(1),"?",0,0,0,0,0,0,0);					/* Say we are here.			*/
	*wisp_progname = CHAR_NULL;							/* Set the progname name to spaces.	*/
	*wisp_screen = CHAR_NULL;							/* Set the screen name to spaces.	*/
	one_short = 1;
	terminate_list = (unsigned char *)"A";						/* Assign A for ALL PFkeys.		*/
	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/
	if (arg_count == 0)								/* No arguments passed.			*/
	{
		werrlog(ERRORCODE(4),func_type,arg_count,0,0,0,0,0,0);			/* Invalid call.  Record error msg.	*/
		return(16);
	}
	va_start(the_args);								/* Go back to the top of the stack.	*/
	z1_fn = va_arg(the_args, unsigned char*);					/* Get function type from arg stack.	*/
	arg_count--;									/* One less argument.			*/
	for (i = 0; i < 2; i++)  func_type[i] = *z1_fn++;				/* Copy function to variable.		*/
	func_type[i] = '\0';								/* Null terminate the string.		*/
	werrlog(ERRORCODE(1),func_type,0,0,0,0,0,0,0);					/* Say we are here.			*/
	if ( 	(strcmp(func_type,"DS") == 0) ||					/* Is it function WSFNS Display Screen?	*/
		(strcmp(func_type,"AS") == 0) ||					/*	Accept Screen?			*/
		(strcmp(func_type,"DI") == 0) ||					/*	Display Immediate?		*/
		(strcmp(func_type,"AI") == 0) )						/*	Accept Immediate?		*/
	{
		if (arg_count < 4)
		{
			werrlog(ERRORCODE(4),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call.  Record error msg.	*/
			return(16);
		}
		z1_rec = va_arg(the_args, unsigned char*);
		z1_line = va_arg(the_args, short*);					/* Get the row to position to.		*/
		if ((*z1_line < 0) || (*z1_line > 24))					/* Is the row valid?			*/
		{
			werrlog(ERRORCODE(8),func_type,*z1_line,0,0,0,0,0,0);
			return(16);
		}
		z1_pos = va_arg(the_args, short*);					/* Get the column to position to.	*/
		if ((*z1_pos < 0) || (*z1_pos > 80))					/* Is the column valid?			*/
		{
			werrlog(ERRORCODE(6),func_type,*z1_pos,0,0,0,0,0,0);
			return(16);
		}
		z1_beepfl = va_arg(the_args, unsigned char*);				/* Assign flag to beep. 		*/
		if (arg_count > 4)							/* Extra parameters passed from WSFNM.	*/
		{
			short *short_ptr;
			if (strcmp(func_type,"DS") == 0)
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
		if (strcmp(func_type,"DS") == 0)
		{
			save_screen(z1_rec,(int)l_slen);				/* Save screen so if want to start over.*/
			return(0);							/* Return to COBOL caller.		*/
		}
		wcc = UNLOCK_KEYBOARD | POSITION_CURSOR;				/* Assign Write Control Character	*/
		if (*z1_beepfl != ' ')  wcc |= SOUND_ALARM;				/* If not blank then beep.		*/
		netroncap = 1;								/* Set so vwang won't use pseudo blanks.*/
restart:										/* Start position if PF30 (start over)	*/
		NC_order_area[ROW_NUMBER_BYTE]	= (unsigned char)*z1_row;	   	/* Starting screen line number for data.*/
		NC_order_area[WCC_BYTE]            = wcc;				/* Set the WCC byte.			*/
		NC_order_area[CURSOR_COL_BYTE]     = (unsigned char)*z1_pos;		/* Position cursor at column.		*/
		NC_order_area[CURSOR_ROW_BYTE]     = (unsigned char)*z1_line;		/* Position cursor at row.		*/

		ldispa = (z1_rec - 4);							/* Point to display area minus 4 bytes.	*/
		for (i = 0; i < 4; i++) save_oa[i] = ldispa[i];				/* Save 4 bytes of program data area.	*/
	 	for (i = 0; i < 4; i++) ldispa[i] = NC_order_area[i];			/* Concatenate order area with screen.	*/

		if (strcmp(func_type,"AS") == 0)					/* Accept Screen WSFNS function.	*/
		{
			if (call_fl)							/* Just want to write and read scroll	*/
			{								/*  region from WSFNM call.		*/
				function = WRITE_ALL;
				vwang(&function, ldispa, &l_numl, terminate_list, NC_pfkey, vw_mod);
				function = READ_ALL;
				vwang(&function, ldispa, &l_numl, terminate_list, NC_pfkey, vw_mod);
			}
			else
			{
				unsigned char *hlddispa, *cptr;
				int st_win,end_win;					/* Start and end of pfkey window.	*/
                                         
				hlddispa = ldispa;					/* Save the current local screen pntr.	*/
				gen_ncpfkey(1,&ldispa,1924,&st_win,&end_win);		/* Generate pop up pfkey window with	*/
											/* a temp screen to display.		*/
				function = DISPLAY_AND_READ_ALTERED;
				vwang(&function, ldispa, &l_numl, terminate_list, NC_pfkey, vw_mod);

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
		else if (strcmp(func_type,"DI") == 0)					/* Display Immediate WSFNS function.	*/
		{
			function = WRITE_ALL;
			vwang(&function, ldispa, &l_numl, terminate_list, NC_pfkey, vw_mod);
		}
		else if (strcmp(func_type,"AI") == 0)					/* Accept Immediate WSFNS function.	*/
		{
			function = READ_ALL;
			vwang(&function, ldispa, &l_numl, terminate_list, NC_pfkey, vw_mod);
		}
		NC_pfkey[2] = '\0';							/* Null terminate the string.		*/
		for (i = 0; i < 4; i++) NC_order_area[i] = ldispa[i];			/* Save the returned vwang order area.	*/
		for (i = 0; i < 4; i++) ldispa[i] = save_oa[i];				/* Restore program data area.		*/

		if (strcmp(NC_pfkey,"30") == 0)						/* Want to start input over.		*/
		{
			get_screen(z1_rec,(int)l_numl);					/* Get the orig. starting screen.	*/
			goto restart;							/* Call VWANG again will no mods.	*/
		}
		if (strcmp(NC_pfkey,"31") == 0)						/* Spawn a process.			*/
		{
			COBLINK("DOCPROG ");
		}
		netroncap = 0;								/* Set back for normal screens.		*/
	}
	else if (strcmp(func_type,"PF") == 0)						/* Is it function PF?			*/
	{										/* Return cursor position and PFkey.	*/
		if (arg_count != 3)
		{
			werrlog(ERRORCODE(2),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call.  Record error msg.	*/
			return(16);
		}
		z1_pf  = va_arg(the_args, short *);					/* Get address of pfkey passed.		*/
		z1_row = va_arg(the_args, short *);					/* Get address of cursor row.		*/
		z1_col = va_arg(the_args, short *);					/* Get address of cursor column.	*/
		i_pf = atoi(NC_pfkey);							/* convert PFkey to an integer.		*/
		*z1_pf  = (short)i_pf;							/* Put value into return cobol ptr.	*/
		*z1_col = (short)NC_order_area[CURSOR_COL_BYTE];			/* Init for cobol return column.	*/
		*z1_row = (short)NC_order_area[CURSOR_ROW_BYTE];			/* Init for cobol return row.		*/
		return(SUCCESS);
	}
	return(SUCCESS);
}

static save_screen(save_rec,scn_len)							/* Save the starting screen config.	*/
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

	if (!(dsptr = (unsigned char *)malloc((unsigned)nchr)))				/* Get memory.				*/
	{
		werrlog(ERRORCODE(10),nchr,0,0,0,0,0,0,0);				/* Oops, cannot get memory.		*/
		wexit(ERRORCODE(10));							/* Unconditionally fatal.		*/
	}
	mem_allocated = 1;								/* Now it is allocated.			*/

	memcpy(dsptr,save_rec,nchr);							/* Save screen record.			*/
}

static get_screen(curr_rec,num_lines)							/* Copy saved screen to current screen.	*/
unsigned char *curr_rec;
int num_lines;
{
	int 	cnt;

	if (!mem_allocated) vbell();							/* Beep if no screen to refresh.	*/
	else
	{
		cnt = num_lines * 80;							/* Set # lines on current screen	*/
		memcpy((char *)curr_rec,(char *)dsptr,cnt);				/* Copy current screen only.		*/
	}
}
