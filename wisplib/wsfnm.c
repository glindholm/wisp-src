			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

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

#include <varargs.h>									/* Allow variable number of arguments	*/
#include <v/video.h>
#include "vwang.h"
#include "werrlog.h"
#include "wglobals.h"

extern char NC_pfkey[3], NC_order_area[4];						/* Define global in WSFNS so accessible	*/

static short screen_len, scrl_len;							/* Values for each screen setup.	*/
unsigned char *wsfnm_cscn;								/* Current pos in long screen pointer.	*/
static short st_scrl, end_scrl;

#define		ROUTINE		85500

WSFNM(va_alist)
va_dcl
{
	va_list		the_args;							/* A pointer to traverse the stack.	*/
	int		arg_count, i;							/* Ptrs and local copies of passed param.*/
	unsigned char	*z1_fn, *z1_rec, *z1_beepfl;					/* Input/output params from WSFNM call.	*/
        short           *z1_pf, *z1_col, *z1_row; 
	short		*z1_screenlines, *z1_slast, *z1_sfirst, *z1_line, *z1_pos;
	short		realline;
	char 		func_type[3];							/* Local string copy of function type.	*/
	int		another_screen, snum, l_used;
	short		cur_scrl_len;
	char		wsfns_func[3];
	long		vcount;

	werrlog(ERRORCODE(1),"?",0,0,0,0,0,0,0);					/* Say we are here.			*/
	*wisp_progname = CHAR_NULL;							/* Set the progname name to spaces.	*/
	*wisp_screen = CHAR_NULL;							/* Set the screen name to spaces.	*/

	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/
	if (arg_count == 0)								/* No arguments passed.			*/
	{
		werrlog(ERRORCODE(4),func_type,arg_count,0,0,0,0,0,0);			/* Invalid call. Record message.	*/
		return(16);
	}
	va_start(the_args);								/* Go back to the top of the stack.	*/
	z1_fn = va_arg(the_args, unsigned char *);					/* Get function type from arg stack.	*/
	arg_count--;									/* One less argument.			*/
	for (i = 0; i < 2; i++)  func_type[i] = *z1_fn++;				/* Copy function to variable.		*/
	func_type[i] = '\0';								/* Null terminate the string.		*/
	werrlog(ERRORCODE(1),func_type,0,0,0,0,0,0,0);					/* Say we are here.			*/
	if (strcmp(func_type,"OM") == 0)						/* Is it function OM?			*/
	{
		if (arg_count != 5)
		{
			werrlog(ERRORCODE(2),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call. Record message.	*/
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
	else if (strcmp(func_type,"AM") == 0)						/* Is it function AM?			*/
	{
		if (arg_count != 4)
		{
			werrlog(ERRORCODE(2),func_type,arg_count,0,0,0,0,0,0);		/* Invalid call. Record message.	*/
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
			werrlog(ERRORCODE(8),func_type,*z1_line,0,0,0,0,0,0);		/* Invalid row. Record message.	*/
			return(16);
		}

		z1_pos = va_arg(the_args, short *);					/* Get the column to position to.	*/
		if ((*z1_pos < 0) || (*z1_pos > 80))
		{
			werrlog(ERRORCODE(6),func_type,*z1_pos,0,0,0,0,0,0);		/* Invalid column. Record message.	*/
			return(16);
		}

		z1_beepfl = va_arg(the_args, unsigned char*);				/* Assign flag to beep. 		*/

		hfio((unsigned char)WRITE_ALL,z1_rec);					/* Display header and footer.		*/

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
			vcount = 7;
			wvaset( &vcount );
			strcpy(wsfns_func,"DS");					/* Save screen for PF30 func.		*/
			WSFNS(wsfns_func,wsfnm_cscn,&realline,z1_pos,z1_beepfl,&cur_scrl_len);
			strcpy(wsfns_func,"AS");					/* Display and read screen.		*/
			WSFNS(wsfns_func,wsfnm_cscn,&realline,z1_pos,z1_beepfl,&st_scrl,&cur_scrl_len);
			if (strcmp(NC_pfkey,"02") == 0) 				/* Go to top of scroll region by PF2.	*/
			{
				snum = 1;						/* Set to first page.			*/
				wsfnm_cscn = &z1_rec[(st_scrl-1)*80];			/* Set ptr to beginning of scroll reg.	*/
				cur_scrl_len = scrl_len;				/* Set back to full scroll region.	*/
			}
			else if (strcmp(NC_pfkey,"00") != 0)				/* Return processing to COBOL program.	*/
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
		}
		hfio((unsigned char)READ_ALL,z1_rec);					/* Restore header and footer info.	*/
	}
	else if (strcmp(func_type,"PM") == 0)						/* Is it function PM?			*/
	{										/* Return cursor position and PFkey.	*/
		strcpy(wsfns_func,"PF");
		z1_pf = va_arg(the_args, short *);					/* Get address of pfkey passed.		*/
		z1_row = va_arg(the_args, short *);					/* Get address of cursor row.		*/
		z1_col = va_arg(the_args, short *);					/* Get address of cursor column.	*/
		vcount = 4;
		wvaset( &vcount );
		WSFNS(wsfns_func,z1_pf,z1_row,z1_col);					/* Make a call to WSFNS.		*/
		return(SUCCESS);
	}
	return(SUCCESS);
}

static hfio(function, scn_ptr)								/* Call VWANG to display header and	*/
	unsigned char function;								/* Function to be performed.		*/
	unsigned char *scn_ptr;								/* footer areas.			*/
{
	unsigned char	*terminate_list;						/* Parameters for vwang			*/
	unsigned char	*ldispa, l_numl;						/* Local pointers for vwang.		*/
	char		wcc, save_oa[4];						/* WANG order area for screen display	*/
	char 		vw_mod[2];							/* Parameters for vwang PFkey pressed.	*/
	int		st_row, i;

	st_row = 1;									/* Set the starting header line #.	*/
	wcc = POSITION_CURSOR;								/* Assign Write Control Character	*/

	NC_order_area[ROW_NUMBER_BYTE]	= (unsigned char)st_row;	   		/* Starting screen line number for data.*/
	NC_order_area[WCC_BYTE]            = wcc;					/* Set the WCC byte.			*/
	NC_order_area[CURSOR_COL_BYTE]     = (unsigned char)1;				/* Position cursor at column.		*/
	NC_order_area[CURSOR_ROW_BYTE]     = (unsigned char)1;				/* Position cursor at row.		*/

	ldispa = (scn_ptr - 4);								/* Point to display area minus 4 bytes.	*/
	for (i = 0; i < 4; i++) save_oa[i] = ldispa[i];					/* Save 4 bytes of program data area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = NC_order_area[i];				/* Concatenate order area with screen.	*/
	l_numl = st_scrl - 1;								/* Set the number of lines to display.	*/

	terminate_list = (unsigned char *)"A";
	vwang(&function, ldispa, &l_numl, terminate_list, NC_pfkey, vw_mod);

	NC_pfkey[2] = '\0';								/* Null terminate the string.		*/
	for (i = 0; i < 4; i++) NC_order_area[i] = ldispa[i];				/* Save the returned vwang order area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = save_oa[i];					/* Restore program data area.		*/

	st_row = st_scrl + scrl_len;							/* Set the starting footer line #.	*/
	NC_order_area[ROW_NUMBER_BYTE]	= (unsigned char)st_row;	   		/* Starting screen line number for data.*/

	ldispa = &scn_ptr[end_scrl*80];							/* Set to address of footer area.	*/
	ldispa = (ldispa - 4);								/* Point to display area minus 4 bytes.	*/
	for (i = 0; i < 4; i++) save_oa[i] = ldispa[i];					/* Save 4 bytes of program data area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = NC_order_area[i];				/* Concatenate order area with screen.	*/
	l_numl = screen_len - end_scrl;							/* Set the number of lines to display.	*/

	gen_ncpfkey(0,&ldispa,l_numl*80);						/* Set up PFkey window if have EDE.	*/
											/* Don't need st_win and end_win because*/
	vwang(&function, ldispa, &l_numl, terminate_list, NC_pfkey, vw_mod);		/* temp screen is just pfkey area.	*/

	NC_pfkey[2] = '\0';								/* Null terminate the string.		*/
	for (i = 0; i < 4; i++) NC_order_area[i] = ldispa[i];				/* Save the returned vwang order area.	*/
	for (i = 0; i < 4; i++) ldispa[i] = save_oa[i];					/* Restore program data area.		*/
}

static do_erase(num_scrl_lines)								/* Call VWANG to erase bottom of scroll	*/
int num_scrl_lines;									/* region.				*/
{
	unsigned char	function;							/* Parameters for vwang			*/
	unsigned char	*edispa, e_numl;						/* Local pointers for vwang.		*/
	char		wcc;								/* WANG order area for screen display	*/
	char 		vw_mod[2];							/* Parameters for vwang PFkey pressed.	*/
	int		st_row;
	char 		blscreen[1924];							/* Generate a blank screen to pass to	*/
											/* VWANG.				*/
	memset((char *)blscreen,' ',sizeof(blscreen));					/* Init the screen to all balnks.	*/
	wcc = UNLOCK_KEYBOARD | POSITION_CURSOR;					/* Assign Write Control Character	*/
	st_row = st_scrl + num_scrl_lines;						/* Calculate the starting row.		*/

	blscreen[ROW_NUMBER_BYTE]	= (unsigned char)st_row;	   		/* Starting screen line number for data.*/
	blscreen[WCC_BYTE]            = wcc;						/* Set the WCC byte.			*/
	blscreen[CURSOR_COL_BYTE]     = (unsigned char)1;				/* Position cursor at column.		*/
	blscreen[CURSOR_ROW_BYTE]     = (unsigned char)1;				/* Position cursor at row.		*/

	edispa = (unsigned char *)blscreen;						/* Point to display area plus 4 bytes.	*/
	e_numl = scrl_len - num_scrl_lines;						/* Set the number of lines to display.	*/

	function = WRITE_ALL;								/* Display blank lines in scroll region.*/
	vwang(&function, edispa, &e_numl, NULL, NC_pfkey, vw_mod);
	NC_pfkey[2] = '\0';								/* Null terminate the string.		*/
}

static blank_msg_line()									/* Call VWANG to erase message line.	*/
{
	unsigned char	function, terminate_list[2];					/* Parameters for vwang			*/
	unsigned char	*edispa, e_numl;						/* Local pointers for vwang.		*/
	char		wcc;								/* WANG order area for screen display	*/
	char 		vw_mod[2];							/* Parameters for vwang PFkey pressed.	*/
	int		st_row;
	char		blank_msg[84+1];						/* Blank message line for VWANG.	*/
	unsigned char	*cptr;

	strcpy((char *)terminate_list,"A");
	memset((char *)blank_msg,' ',sizeof(blank_msg));				/* Init the line to all blanks.		*/
	blank_msg[sizeof(blank_msg)-1] = '\0';						/* Null terminate the string.		*/
	wcc = POSITION_CURSOR;								/* Assign Write Control Character	*/
	st_row = st_scrl + scrl_len;							/* Calculate the message starting row.	*/

	blank_msg[ROW_NUMBER_BYTE]	= (unsigned char)st_row;	   		/* Starting screen line number for data.*/
	blank_msg[WCC_BYTE]            = wcc;						/* Set the WCC byte.			*/
	blank_msg[CURSOR_COL_BYTE]     = (unsigned char)1;				/* Position cursor at column.		*/
	blank_msg[CURSOR_ROW_BYTE]     = (unsigned char)1;				/* Position cursor at row.		*/

	edispa = (unsigned char *)blank_msg;						/* Point to msg line plus order area.	*/
	e_numl = 1;									/* Set the number of lines to read.	*/

	function = READ_ALL;			    					/* Read message line from screen.	*/
	vwang(&function, edispa, &e_numl, terminate_list, NC_pfkey, vw_mod);

	cptr = edispa;									/* Point to message line.		*/
	cptr += 4;									/* Step past order area.		*/
	*cptr++ = ' ';									/* Set the FAC to space.		*/
	while ((*cptr & FAC_CHARACTER) != FAC_CHARACTER) *cptr++ = ' ';			/* Set message line to blank.		*/

	function = WRITE_ALL;								/* Display blank message line.		*/
	vwang(&function, edispa, &e_numl, terminate_list, NC_pfkey, vw_mod);
}
