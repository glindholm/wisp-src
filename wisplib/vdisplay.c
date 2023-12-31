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

/*
**	File:		vdisplay.c
**
**	Project:	wisp/lib
**
**	Purpose:	Replacement for Wang DISPLAY untility
**
**	Routines:	
**	WL_vdisplay()		Setup processing of file.
**	get_line()		Compose an input line
**	display_buffer()	Display correct size of buffer
**	eof_trailer()		Write the start of file header
**	eop_indicator()		Write the page break indicator
**	str_insert()		Insert a string in another string
**	show_help()		Show the help screen
**	rep_limit()		Report the limit of upward scroll
**	clear_window()		Subroutine to clear the help area
**	work_message()		Display the working message
**	roll_up()		Scroll up one line
**	roll_down()		Scroll down one line
**	roll_left_right()	Scroll left or right
**	set_scroll_buffer()	Set pointer to buffer to be at the current scroll column
**	add_tab_cols()
**	on_screen()		Is the text on the screen
**	stx()			Get string index in another string
**	in_file() 		Search for string text within file
**	scr_refresh()		Refresh screen from line buffers
**	adjust_buffers()	Adjust buffer address to reflect what is really on the screen
**	full_screen_refresh()
**	derror()		Error message reporting routine
**	gets0()
**	get_pos()		Get the position of a record
**	mem_err()		Memory error - could not allocate
**
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#ifdef unix
#include <unistd.h>
#endif

#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vcap.h"

#include "idsistd.h"
#include "werrlog.h"
#include "wperson.h"
#include "scnfacs.h"
#include "wanguid.h"
#include "idsisubs.h"
#include "costar.h"
#include "osddefs.h"
#include "wexit.h"
#include "wisplib.h"
#include "filext.h"
#include "wispcfg.h"
#include "vwang.h"
#include "wsb.h"
#include "wmalloc.h"
#include "screen.h"

#ifdef INCLUDE_VDISPIDX
# include "vdispidx.h"
#endif

/*
**	Structures and Defines
*/
#define EDIT_VMODE		(VMODE_REVERSE|VMODE_UNDERSCORE)
#define MESSAGE_VMODE		(VMODE_REVERSE)
#define HIGH_MESSAGE_VMODE	(VMODE_REVERSE|VMODE_UNDERSCORE)

#define CHAR_CTRL_U	'\025'
#define CHAR_DEL	'\177'
#define CHAR_BELL	'\007'

#define LNFD		0x0A
#define FF		0x0C
#define CHAR_CR		0x0D

#define SEARCH_TEXT_SIZE	64							/* Maximum length of a search string.	*/
#define SEARCH_NUM_SIZE		4							/* Maximum length of a number string.	*/
#define POSITION_TABLE_SIZE	1000							/* Number of lines for reverse scroll.	*/
#define PAGE_SCROLL_LINES	18							/* Number of lines for a page scroll.	*/
#define PAGE_SCROLL_COLS	20							/* Number of lines for a column scroll. */
#define BUFFER_SIZE		265							/* Maximum buffer size.			*/
#define TAB_SPACE_COUNT		8							/* Number of spaces equal to tab char.	*/

#define WINDOW_TOP	9								/* Top line of window area.		*/
#define WINDOW_ROWS	10								/* Number of lines in window area.	*/

#define CHECK_INTERVAL	500								/* Number of lines between checking	*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

static int errmsg_active = FALSE;							/* Error message is on the screen.	*/
static int rvrs_scroll_avail = TRUE;							/* Set reverse scroll available.	*/
static int frwd_scroll_avail = TRUE;							/* Set forward scroll available.	*/
static int ff_flag = FALSE;								/* Flag for FF found.			*/
static int stream_active;								/* Flag for stream mode display.	*/
static int status_active;								/* Status message is on the screen.     */
static int frow, fcol;									/* Current row in file.			*/
static unsigned char *ssave;								/* Save the status unerneath.		*/

static char srch_ch_map[256];
static int srch_len;
static FILE *input_file;								/* File control pointers.		*/

static int use_costar_flag = 0;								/* Is COSTAR active?			*/

static int edit_vmode = EDIT_VMODE;
static int message_vmode = MESSAGE_VMODE;
static int high_message_vmode = HIGH_MESSAGE_VMODE;

static int display_high_chars = 0;							/* Display 8-bit chars flag		*/
static int nativecharmap = 0;		/* Do CHARMAP substitution (1=Native char set, 0=Wang char set)		*/

/*
**	Static Function Prototypes
*/

static int get_line(register char *lb,int4 *fpos_ptr,int4 *fpoff_ptr,int bufsize,FILE *input_file);
static void display_buffer(char *lb);							/* Display correct size of buffer.	*/
static void eof_trailer(const char *file_name);						/* Write the start of file header.	*/
static void eop_indicator(char *lb, int bufsize);					/* Write the page break indicator.	*/
static void str_insert(char *target, const char *source);				/* Insert a string in another string.	*/
static void show_help(const char *file_name, int options12);				/* Show the help screen.		*/
static void rep_limit(void);								/* Report the limit of upward scroll.	*/
static void clear_window(char *lbuf[],int roll_cnt,int bufsize);			/* Subroutine to clear the help area.	*/
static int work_message(int on);							/* Display the working message.		*/
static int roll_up(char *lbuf[],int4 fpos_tab[],int4 fpoff_tab[],int4 *pt,int4 *pto,int bufsize,FILE *input_file,
                   int *tx,int *lc,int send_out,int roll_cnt);
static void roll_down(char *lbuf[],int4 fpos_tab[],int4 fpoff_tab[],int4 *pt,int4 *pto,int bufsize,FILE *input_file,
                 int *tx,int *lc,int *eofile,int roll_cnt);
static void roll_left_right(char *lbuf[], int4 fpos_tab[], int bufsize,FILE *input_file,int *eorec,int roll_cnt);
static void set_scroll_buffer(int roll_cnt, char *lb, int bufsize);
static int add_tab_cols(int col);
static int on_screen(char *search_text, char *lbuf[], int4 fpos_tab[],int4 *pt, int bufsize,
			FILE *input_file,int *tx,int *lc,int *eofile,int *eorec,int *roll_cnt);
static int stx(char *string, char *mask);						/* Get string index in another string.	*/
static int in_file(char *search_text,char **lbuf,int4 fpos_tab[],int4 fpoff_tab[],int4 *pt,int4 *pto,
                   int bufsize,FILE *input_file,int *tx,int *lc,int *eofile,int *eorec,int *roll_cnt);
static void scr_refresh(char *lbuf[],int4 fpos_tab[],int lines,int bufsize,int roll_cnt);
static void adjust_buffers(char *lbuf[],int4 fpos_tab[],int4 fpoff_tab[],int direction);
static void full_screen_refresh(char *lbuf[], int row,int col);
static void derror(char *text);								/* Error message reporting routine	*/
static int gets0(char *string, int count);
static int4 get_pos(FILE *input_file);							/* Get the position of a record.	*/
static void mem_err(void);								/* Memory error - could not allocate	*/
static int check_empty(const char *file_name);						/* See if the file is empty.		*/
static void display_stat(int dwidth);							/* Display cursor row, col and width.	*/



int WL_vdisplay(const char *file_name,int record_size)					/* VIDEO file display subroutine.	*/
{
	char *lb, *lbuf[MAX_LINES_PER_SCREEN];						/* Pointer to the line input buffer.	*/
	int bufsize;									/* Size of the line buffer.		*/
	int eofile;									/* End of file flag.			*/
	int eorec;									/* End of record flag.			*/
	enum e_vop op_save;								/* Save optimization level.		*/
	int win_active;									/* Flag to track overlay window.	*/
	int options12_active;								/* The pfkeys12() options window active	*/
	int err_just_removed, win_just_removed;						/* Flags indicating just removed.	*/
	int viewing;									/* Flag that viewing is takeing place.	*/
	int restart_requested;								/* Flag that a restart is requested.	*/
	int new_size_requested;								/* Flag that a new size is requested.	*/
	int first_time;									/* First time help control flag.	*/
	int4 fpos_tab[MAX_LINES_PER_SCREEN];						/* Position of each line of the screen.	*/
	int4 fpoff_tab[MAX_LINES_PER_SCREEN];						/* Posn offset of each line of screen.	*/
	int4 postab[POSITION_TABLE_SIZE];						/* Allocate the position table.		*/
	int4 pofftab[POSITION_TABLE_SIZE];						/* Allocate the position offset table.	*/
	int tx;										/* Current table index.			*/
	int lc;										/* Count of lines in position table.	*/
	int text_defined;								/* Search text defined flag.		*/
	int search_abort, term_status;							/* Search has been aborted		*/
	char search_text[SEARCH_TEXT_SIZE+2];						/* Storage for the searc text.		*/
	char search_num[SEARCH_NUM_SIZE+2];						/* Storage for the number text.		*/
	int scrl_col_cnt;								/* Scroll column index count.		*/
	register int j,c;								/* Some working registers.		*/
	int i, retcd;									/* Return code from print routines.	*/
	int	check_time;
	int4	def_bgchange, def_bgcolor;
	int	file_ok;

	WL_wtrace("VDISPLAY","ENTRY","Entry into VDISPLAY File=[%s] Recsize=[%d]", file_name, record_size);

	stream_active = FALSE;								/* Set default mode.			*/
	status_active = FALSE;
	ssave = NULL;

	vwang_init_video();
	
	/*
	**	Note: Text data will either be in the "Native" character set
	**	or in the "Wang" character set.
	*/
	vwang_load_charmap(0);
	if (WL_get_wisp_option("NATIVECHARMAP"))
	{
		nativecharmap = 1;
	}
	else
	{
		display_high_chars = wispdisplay8bit();
	}
	
	use_costar_flag = use_costar();
	if (use_costar_flag)
	{
		edit_vmode = costar_edit_vmode();
		message_vmode = costar_message_vmode();
		high_message_vmode = costar_high_message_vmode();
	}

	/*
	**	Check if the file exists and is a regular file.
	*/
	file_ok = (WL_isafile(file_name) && WL_fcanread(file_name));


	if (!file_ok || check_empty(file_name))					/* Check to see if empty file.		*/
	{
		HWSB	hWsb;
		int	pfkey, currow, curcol;
		char 	templine[80];
		char	*ptr;

		currow = 0;
		curcol = 0;
		hWsb = wsb_new();

		wsb_add_text(hWsb,2,26,"Active Subprogram is DISPLAY");
		ptr="                                      ";
		wsb_add_field(hWsb,3,21,UNDER_TEXT,ptr,strlen(ptr));
		ptr="SORRY";
		wsb_add_field(hWsb,6,37,BLINK_TEXT,ptr,strlen(ptr));
		memset(templine,0,sizeof(templine));
		sprintf(templine,"File:  %s",file_name);
		wsb_add_text(hWsb,9,20,templine);

		if (!file_ok)
		{
			wsb_add_text(hWsb,11,20,"File does not exist or can not be read");
		}
		else
		{
			wsb_add_text(hWsb,11,20,"File specified contains zero records");
		}
		
		wsb_add_text(hWsb,14,25,"Press (ENTER) to end DISPLAY");

		wsb_display_and_read(hWsb, "00X", &pfkey, &currow, &curcol);

		wsb_delete(hWsb);
		
		return(SUCCESS);
	}

#ifdef INCLUDE_VDISPIDX
	{
	int type;
	type = vdisp_file_type(file_name);
	if (type == VD_ERR_OPEN || type == VD_ERR_READ)
	{
		werrlog(WERRCODE(66506),file_name,0,0,0,0,0,0,0);
		return(FAILURE);							/* It is fatal so return with error.	*/
	}
	if (type!=VD_TYPE_TEXT)
	{
		vdispidx(file_name,type);
		return SUCCESS;
	}
	}
#endif

	if (record_size < 0)								/* Is record size valid.		*/
	{										/* Oops, invalid record size.		*/
		werrlog(WERRCODE(66502),record_size,0,0,0,0,0,0,0);
		VL_vexit();								/* Unconditional exit.			*/
		wexit(WERRCODE(66502));
	}

	if (record_size > BUFFER_SIZE)							/* Is record size greater than buffer.	*/
	{										/* Invalid buffer size.			*/
		werrlog(WERRCODE(66504),BUFFER_SIZE,record_size,0,0,0,0,0,0,0);
		VL_vexit();								/* Unconditional exit.			*/
		wexit(WERRCODE(66504));
	}

	VL_vcapload();

	frwd_scroll_avail = VL_vscroll_frwd_avail();
	rvrs_scroll_avail = VL_vscroll_rvrs_avail();

	WL_wpload();

	first_time = TRUE;								/* First time only true the 1st time.	*/
	op_save = VL_voptimize(VOP_DEFER_MOTION_ONLY);					/* Select deferred optimization.	*/

newsize:
	text_defined = FALSE;

	VL_vscreen(VSCREEN_NARROW);							/* Select a narrow screen unless VMS.	*/
	
	/*
	**	NOTE:	On UNIX and WIN32 the record_size will ALWAYS come in as 0 because
	**		there is no "record size".  This is to indicate a "stream" file
	**		that uses NewLines to determine record size.  In this case we always
	**		set the buffer size to be the maximum.
	*/

 	if (record_size != 0) bufsize = record_size;					/* Determine the buffer size.		*/
 	else bufsize = BUFFER_SIZE;							/* Else, variable length.		*/

	WL_get_defs(DEFAULTS_BGCHANGE,(char*)&def_bgchange);
	if (def_bgchange)								/* Set the screen background if		*/
	{										/* not done already.			*/
		WL_get_defs(DEFAULTS_BGCOLOR,(char*)&def_bgcolor);
		if (def_bgcolor) VL_vscreen(VSCREEN_LIGHT);
		else VL_vscreen(VSCREEN_DARK);
	}
	VL_verase(FULL_SCREEN);

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Allocate a screens worth of buffers. */
	{
		if ((lbuf[i] = wisp_malloc(bufsize+1+8)) == 0) mem_err();		/* Allocate a line buffer.		*/
	}

	if ((input_file = fopen(file_name,FOPEN_READ_BINARY)) == NULL)		       	/* Did the file open?			*/
	{                                                             
		werrlog(WERRCODE(66506),file_name,0,0,0,0,0,0,0);
		return(FAILURE);							/* It is fatal so return with error.	*/
	}

#ifdef NOT_YET
	blocks_loaded=0;			/* Number of blocks loaded */
	curr_block=0;				/* The current block in use (first == 0) */
	curr_pos=0;				/* The current position in curr_block (offset) */
	curr_byte=0;				/* The current byte (offset) */
	file_loaded=0;				/* Is the file fully loaded */
	highest_byte=0;				/* The highest byte loaded */

	loadblock();									/* Load in the first block of data	*/
#endif

again:
	fseek(input_file,0,0);

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Initialize screen buffers.		*/
	{
		lb = lbuf[i];								/* Make a copy of its pointer.		*/
		memset(lb,' ',bufsize);							/* Copy spaces to buffer.		*/
		lb[bufsize] = CHAR_NULL;						/* Default a null at the end of line.	*/
		fpos_tab[i] = 0;							/* Set all lines to start of file.	*/
		fpoff_tab[i] = 0;
	}

	for (i = 0; i < POSITION_TABLE_SIZE; i++)
	{
		postab[i] = 0;								/* Set it all to zero.			*/
		pofftab[i] = 0;
	}
	tx = 0;										/* Set the table index to the start.	*/
	lc = 0;										/* There are no lines in the table.	*/
	scrl_col_cnt = 0;								/* Have not scrolled left or right yet. */

	VL_vbuffering_start();		       						/* Logical buffering on.		*/
	VL_vmode(VMODE_CLEAR);								/* Clear all renditions.		*/
	VL_vcharset(VCS_DEFAULT);								/* Default character set.		*/
	VL_vmove(0,0);									/* Home the cursor.			*/
	VL_vset_cursor_on();								/* Turn it on.				*/

	eofile = FALSE;									/* Not end of file yet.			*/
	eorec = FALSE;									/* Assume not end of record yet.	*/
	if (record_size > 0)								/* If record size specified.		*/
	{
		if (record_size <= VL_vscr_wid) eorec = TRUE;				/* Record_size less then full screen.	*/
	}
											/* Test first char to see what file type.*/
	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Read in the first screen's worth.	*/
	{
		eofile = get_line(lbuf[i],&fpos_tab[i],&fpoff_tab[i],bufsize,input_file); /* Get the string.			*/

		if (eofile)
		{
			if (*lbuf[i] != '\0')
			{
				display_buffer(lbuf[i]);				/* Display the line.			*/
				if (i != MAX_LINES_PER_SCREEN-1) VL_vprint("\n");		/* Move to the next line if appro.	*/
			}
			break;
		}

		display_buffer(lbuf[i]);						/* Display the line.			*/
		if (i != MAX_LINES_PER_SCREEN-1) VL_vprint("\n");				/* Move to the next line if appro.	*/
	}

	if (eofile)									/* End of file, so blank buffers.	*/
	{
		/*
		**	On a file that is less then one screen long (24 lines) we mark the end of it with an fpos of -1.
		**	We then display an "end of file" banner on the next line.
		*/
		if (i < MAX_LINES_PER_SCREEN)
		{
			fpos_tab[i] = -1;						/* Indicate end of file position.	*/
			fpoff_tab[i] = 0;
			eof_trailer(file_name);						/* Write out the eof banner		*/
		}

		for (j = i+1; j < MAX_LINES_PER_SCREEN; j++)				/* Loop through the rest of the lines.	*/
		{
			lb = lbuf[j];							/* Make the working pointer.		*/
			*lb = CHAR_NULL;						/* Set it to a null character.		*/
		}
	}

	if (first_time)									/* Is this the first time in program?	*/
	{
		derror(" Depress the HELP key for information on how this program works.");
		first_time = FALSE;							/* No longer the first time.		*/
	}
	win_active = FALSE;								/* No need to activate help.		*/
	options12_active = FALSE;
	win_just_removed = FALSE;							/* Window has not just been removed.	*/
	err_just_removed = FALSE;							/* Error message not just removed.	*/

	VL_vmove(0,0);									/* Move to the start of the text.	*/
	frow = 0;									/* Init the file row and column.	*/
	fcol = 0;
	restart_requested = FALSE;							/* Assume restart will not be needed.	*/
	new_size_requested = FALSE;							/* Assume new size will not be needed.	*/
	viewing = TRUE;									/* Flag that we are viewing a file.	*/
	while (viewing)									/* Repeat until PF16 depressed.		*/
	{
		if (status_active)							/* Do we want status visible?		*/
		{
			display_stat(bufsize);						/* Display cursor row, col and width.	*/
		}

		c = VL_vgetm();								/* Get a character.			*/

		/*
		**	Translate pfkeys when in pfkeys12() mode.
		**	
		**	12 -> 16	Exit key
		**	11 -> 17	Options key (Toggle between HELP and OPTIONS window)
		**
		**	Options key mode
		**	6  -> 11	Change width
		**	7  -> 12	Toggle status
		**	8  -> 13	Stream mode
		**	9  -> 14	Print screen
		**	10 -> 15	Print file
		**	11 -> help	Toggle to HELP
		*/
		if (WL_pfkeys12())
		{
			if (c == fn12_key)
			{
				c = fn16_key;
			}
			else if (options12_active)
			{
				switch(c)
				{
				case fn6_key:	c = fn11_key;	break;
				case fn7_key:	c = fn12_key;	break;
				case fn8_key:	c = fn13_key;	break;
				case fn9_key:	c = fn14_key;	break;
				case fn10_key:	c = fn15_key;	break;
				case fn11_key:	c = help_key;	break;
				}
			}
			else if (c == fn11_key)
			{
				c = fn17_key;
			}
		}
		

		if (win_active)								/* Is anything over written?		*/
		{
			clear_window(lbuf,scrl_col_cnt,bufsize);			/* Clear away the area.			*/
			win_active = FALSE;						/* Help is no longer active.		*/
			options12_active = FALSE;
			win_just_removed = TRUE;					/* Window has just been removed.	*/
		}

		if (errmsg_active)							/* Is there an error message visible?	*/
		{
			j = MAX_LINES_PER_SCREEN-1;					/* Determine the text location.		*/
			VL_vstate(VSTATE_SAVE);						/* Save what is going on.		*/
			VL_vmode(VMODE_CLEAR);						/* Clear the current mode.		*/
			VL_vcharset(VCS_DEFAULT);						/* Default character set.		*/
			VL_vmove(MAX_LINES_PER_SCREEN-1, 0);				/* Move to error area.			*/
			lb = lbuf[j];							/* Get pointer to buffer.		*/
			set_scroll_buffer(scrl_col_cnt,lb,bufsize);			/* Set ptr and display buffer.		*/
			VL_vstate(VSTATE_RESTORE);						/* Restore where we were.		*/
			errmsg_active = FALSE;						/* Now no error on the screen.		*/
			err_just_removed = TRUE;					/* Error message just removed.		*/
		}

		if (status_active)							/* Is there a status visible?		*/
		{
			if (ssave != NULL)
			{
				VL_vrss(ssave);
				ssave = NULL;
			}
		}

		if ((c == fn1_key) || (c == home_key))					/* Toggle home/reverse home.		*/
		{
			if (VL_vcur_col == 0 && VL_vcur_lin == 0)				/* If cursor in home position go oppsite*/
			{
				frow = frow + MAX_LINES_PER_SCREEN - 1;
				fcol = fcol + VL_vscr_wid - 1;
				VL_vmove(MAX_LINES_PER_SCREEN-1,VL_vscr_wid-1);		/* Else go to reverse home.		*/
			}
			else
			{
				frow = frow - VL_vcur_lin;
				fcol = fcol - VL_vcur_col;
				VL_vmove(0,0);						/* Home if not home.			*/
			}
		}

		else if (c == fn16_key) viewing = FALSE;				/* Does he want to exit?		*/

		else if (c == down_arrow_key)						/* Down arrow?				*/
		{
			if (VL_vcur_lin != MAX_LINES_PER_SCREEN-1)
			{
				VL_vmove(VL_vcur_lin+1,VL_vcur_col);				/* Position cursor?		*/
				frow++;
			}
			else								/* No, so scroll a line.		*/
			{
				eofile = roll_up(lbuf,fpos_tab,fpoff_tab,postab,pofftab,
                                                 bufsize,input_file,&tx,&lc,1,scrl_col_cnt);
				if (eofile) derror("End of file encountered, no action performed.");
				else if (!frwd_scroll_avail)				/* In no forward scroll then need to	*/
				{							/* display manually.			*/
					full_screen_refresh(lbuf,MAX_LINES_PER_SCREEN-1,VL_vcur_col);
					frow++;
				}
				else frow++;
			}
		}

		else if (c == up_arrow_key)						/* Up arrow?				*/
		{
			if (VL_vcur_lin != 0)
			{
				VL_vmove(VL_vcur_lin-1,VL_vcur_col);				/* Move cursor?				*/
				frow--;
			}
			else if (fpos_tab[0] == 0) derror("Start of file encountered, no action performed.");
			else if (lc == 0)						/* Anywhere to go?			*/
			{
				rep_limit();						/* Report the scroll limit is reached.	*/
				win_active = TRUE;					/* Window is now active.		*/
			}
			else 
			{
				roll_down(lbuf,fpos_tab,fpoff_tab,postab,pofftab,bufsize,
                                          input_file,&tx,&lc,&eofile,scrl_col_cnt);
				if (!rvrs_scroll_avail)					/* In no reverse scroll then need to	*/
				{							/* display manually.			*/
					full_screen_refresh(lbuf,0,VL_vcur_col);		/* Move cursor to top of screen.	*/
				}
				frow--;
			}
		}

		else if (c == left_arrow_key)						/* Left arrow?				*/
		{
			if (VL_vcur_col != 0)
			{
				VL_vmove(VL_vcur_lin,VL_vcur_col-1);				/* Move cursor?				*/
				fcol--;
			}
			else if (scrl_col_cnt == 0) derror("Left edge of file encountered, no action performed.");
			else
			{
				scrl_col_cnt--;						/* Roll right one column.		*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
				fcol--;
			}
		}

		else if (c == right_arrow_key)
		{
			if (VL_vcur_col < VL_vscr_wid-1)
			{
				VL_vmove(VL_vcur_lin,VL_vcur_col+1);				/* Move cursor?				*/
				fcol++;
			}
			else if (eorec)
			{								/* Verify if has a record size.		*/
				if (record_size) derror("Right edge of file encountered, no action performed.");
				else derror("Right edge of buffer encountered, no action performed.");
			}
			else
			{
				scrl_col_cnt++;						/* Roll left one column.		*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
				fcol++;
			}
		}

		else if (c == fn4_key)							/* Previous screen?			*/
		{
			if (fpos_tab[0] == 0 && VL_vcur_lin == 0) derror("Already at start of file, no action performed.");
			else if (fpos_tab[0] == 0)
			{
				VL_vmove(0,VL_vcur_col);					/* Move cursor to top of screen.	*/
				frow = 0;
			}
			else if (lc == 0)						/* Anywhere to go?			*/
			{
				rep_limit();						/* Report the scroll limit is reached.	*/
				win_active = TRUE;					/* Window is now active.		*/
			}
			else
			{
				for (i = 0; i < PAGE_SCROLL_LINES; i++)			/* Loop through and scroll.		*/
				{
					if (lc > 0)					/* If the lines in scroll buffer	*/
					{
						roll_down(lbuf,fpos_tab,fpoff_tab,postab,pofftab,bufsize,
                                                          input_file,&tx,&lc,&eofile,scrl_col_cnt);
						frow--;
					}
				}

				if (!rvrs_scroll_avail)					/* In no reverse scroll then need to	*/
				{							/* display manually.			*/
					full_screen_refresh(lbuf,0,VL_vcur_col);		/* Move cursor to top of screen.	*/
				}
			}
		}

		else if (c == fn5_key)							/* Next screen?				*/
		{
			if (eofile)							/* If already on last screen in file.	*/
			{
				int eofl;

				eofl = 0;
				if (0==fpos_tab[0])					/* If first line of file is in row 1	*/
				{
					eofl = 1;					/* Start scanning in row 2		*/
				}

				while (eofl < MAX_LINES_PER_SCREEN && 0<fpos_tab[eofl]) eofl++; /* Find last line in file.	*/
				eofl -=1;						/* We are one line past end so backup	*/
				if (VL_vcur_lin == eofl) derror("At end of file, no action performed.");
				else
				{
					int cl;

					cl = VL_vcur_lin;					/* Save prior line position		*/
					VL_vmove(eofl,VL_vcur_col);				/* Move cursor to end of file.		*/
					frow = frow + (VL_vcur_lin - cl);
				}
			}
			else 
			{
				for (i = 0; (i < PAGE_SCROLL_LINES) && !eofile; i++)	/* Scroll that many times.		*/
				{
					eofile = roll_up(lbuf,fpos_tab,fpoff_tab,postab,pofftab,
                                                         bufsize,input_file,&tx,&lc,1,scrl_col_cnt);
					if (!eofile)
					{
						frow++;
					}
				}

				if (!frwd_scroll_avail)
				{
					full_screen_refresh(lbuf,MAX_LINES_PER_SCREEN-1,VL_vcur_col); /* Move cursor to bottom	*/
				}
			}
		}

#if defined(unix)
		else if (c == fn6_key)							/* Change width of screen.		*/
		{
			int cl;

			cl = VL_vcur_lin;
			VL_vmove(0,0);							/* Home before changing.		*/
			VL_vdefer_restore();						/* Restore from deferred actions.	*/
			if (VL_vscr_wid == 132) VL_vscreen(VSCREEN_NARROW);			/* Set narrow?				*/
			else VL_vscreen(VSCREEN_WIDE);					/* No, then set wide.			*/

			sleep(1);							/* Delay to allow screen width change	*/

			scr_refresh(lbuf,fpos_tab,MAX_LINES_PER_SCREEN,bufsize,scrl_col_cnt); /* Refresh the screen.	*/
			VL_vmove(0,0);							/* Home for consistency.		*/
			frow = frow - cl;
			fcol = 0;
		}
#endif

		else if ((c == fn7_key) || (c == fn8_key))				/* Search for text in a file?		*/
		{
			search_abort = FALSE;

			if (c == fn7_key)						/* Define search text?			*/
			{
				VL_vstate(VSTATE_SAVE);					/* Save where we are in the text.	*/
				VL_vmove(0,0);						/* Home.				*/
				VL_vmode(message_vmode);					/* Use the reverse rendition.		*/
				VL_vprint(" Search for: ");
				VL_vmode(edit_vmode);
				VL_vprint("                                                                ");
				VL_verase(TO_EOL);						/* Erase to the end of the line.	*/
				VL_vmove(0,13);						/* Move to the start of input area.	*/
				i = gets0(search_text,SEARCH_TEXT_SIZE);		/* Get the string to search for.	*/
				if (i == CHAR_CR) text_defined = TRUE;			/* The search text is now defined.	*/
				else if (i == fn1_key || i == fn16_key)			/* Search was aborted			*/
				{
					text_defined = FALSE;
					search_abort = TRUE;
				}
				else text_defined = FALSE;				/* Unless input was aborted.		*/
				if (search_text[0] == CHAR_NULL) text_defined = FALSE;	/* Not defined if nothing entered.	*/
				if (errmsg_active)					/* Is there an error message visible?	*/
				{
					j = MAX_LINES_PER_SCREEN-1;			/* Determine the text location.		*/
					VL_vmode(VMODE_CLEAR);				/* Clear the current mode.		*/
					VL_vcharset(VCS_DEFAULT);				/* Default character set.		*/
					VL_vmove(MAX_LINES_PER_SCREEN-1, 0);		/* Move to error area.			*/
					lb = lbuf[j];					/* Get pointer to buffer.		*/
					set_scroll_buffer(scrl_col_cnt,lb,bufsize);	/* Set ptr and display buffer.		*/
					errmsg_active = FALSE;				/* Now no error on the screen.		*/
				}
				memset(srch_ch_map,0,sizeof(srch_ch_map));
				srch_len=0;
				if ( text_defined )
				{
					lb = &search_text[0];				/* Point to the search text.		*/
					while (*lb != CHAR_NULL)			/* Now convert to upper case.		*/
					{
						if (!isalpha((int)*lb))
						{
							srch_ch_map[(int)*lb]=TRUE;
						}
						else
						{
							srch_ch_map[tolower(*lb)]=TRUE;
							srch_ch_map[toupper(*lb)]=TRUE;
						}
						++srch_len;
						++lb;
					}
				}
				VL_vmove(0,0);						/* Return to the start of the line.	*/
				VL_vmode(VMODE_CLEAR);					/* Reset the clear rendition.		*/
				lb = lbuf[0];						/* Get pointer to buffer.		*/
				set_scroll_buffer(scrl_col_cnt,lb,bufsize);		/* Set ptr and display buffer.		*/
				VL_vstate(VSTATE_RESTORE);					/* Restore where we were.		*/
				VL_vcontrol_flush();					/* Show everything we've done.		*/
			}

			if (search_abort) { /* just fall through */ }
			else if (!text_defined) derror("A search string has not been defined, use FIND function to define one.");
			else if (on_screen(search_text,lbuf,fpos_tab,postab,bufsize,	/* Is it on the current screen?		*/
						input_file,&tx,&lc,&eofile,&eorec,&scrl_col_cnt));
			else if (eofile) derror("Already at end of file, no action performed.");
			else if ((term_status = 
				  in_file(search_text,lbuf,fpos_tab,fpoff_tab,
					  postab,pofftab,bufsize,input_file,
                                          &tx,&lc,&eofile,&eorec,&scrl_col_cnt)))
			{
				if (term_status == 2) derror("Search aborted.");	/* Search function has been aborted.	*/
			}
			else derror("String not found.");				/* Not in file so ring the bell.	*/
		}

		else if (c == fn2_key)							/* Top of file?				*/
		{
			viewing = FALSE;						/* Stop viewing.			*/
			restart_requested = TRUE;					/* Ask for a restart.			*/
			frow = 0;
			fcol = 0;
		}

		else if (c == fn3_key)							/* Bottom of file?			*/
		{
			if (eofile) derror("Already at end of file, no action performed.");
			else
			{
				int aborted,on;

				aborted = FALSE;
				on = 0;							/* Working counters.			*/
				check_time = 0;
				VL_vset_cursor_off();					/* Don't need the cursor for a while.	*/
				while (!eofile)						/* Loop to find the end of the file.	*/
				{
					eofile = roll_up(lbuf,fpos_tab,fpoff_tab,postab,pofftab,
                                                         bufsize,input_file,&tx,&lc,0,scrl_col_cnt);
					frow++;

					if (check_time++ > CHECK_INTERVAL)		/* Time to update the message?		*/
					{
						check_time = 0;				/* Reset the counter.			*/
						on = work_message(on);			/* Output flashing working message.	*/

						if (VL_vcheck() != 0)
						{
							aborted = TRUE;
							break;				/* Break out of working loop.		*/
						}
					}
				}

				if (eofile) frow--;

				ff_flag = FALSE; /* Temp fix to misc highlighting bug */

				VL_vstate(VSTATE_SAVE);					/* Save what we're doing.		*/
				scr_refresh(lbuf,fpos_tab,MAX_LINES_PER_SCREEN,bufsize,scrl_col_cnt); /* Refresh screen.	*/
				VL_vstate(VSTATE_RESTORE);					/* Restore what we were doing.		*/
				VL_vset_cursor_on();					/* Now restore the flashing cursor.	*/

				if (aborted) derror("Move to bottom of file aborted.  Positioned at aborted screen.");
			}
		}

		else if (c == fn9_key)							/* Previous screen to left?		*/
		{
			/*
			**	Don't just move the cursor, do the scroll
			*/
			if (scrl_col_cnt == 0)
			{
				if (VL_vcur_col > 0 )
				{
					VL_vmove(VL_vcur_lin,0);				/* Move cursor to beginning of line.	*/
					fcol = 0;
				}
				else derror("Already at left edge of file, no action performed.");
			}
			else
			{
				int hscrl_cnt;

				hscrl_cnt = scrl_col_cnt;
				scrl_col_cnt -= PAGE_SCROLL_COLS;			/* Roll right defined number of cols.	*/
				if (scrl_col_cnt < 0) scrl_col_cnt = 0;			/* Can only scroll to beg. of line.	*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
				fcol = fcol - (hscrl_cnt - scrl_col_cnt);
			}
		}

		else if (c == fn10_key || c == tab_key)					/* Next screen right?			*/
		{
			/*
			**	Don't just move the cursor, do the scroll
			*/
			if (eorec)
			{								/* Verify if has a record size.		*/
				if (record_size) derror("Right edge of file encountered, no action performed.");
				else derror("Right edge of buffer encountered, no action performed.");
			}
			else 
			{
				int hscrl_cnt;

				hscrl_cnt = scrl_col_cnt;
				scrl_col_cnt += PAGE_SCROLL_COLS;			/* Roll left defined number of cols.	*/
				if (scrl_col_cnt > (bufsize - VL_vscr_wid)) scrl_col_cnt = bufsize - VL_vscr_wid;
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
				fcol = fcol + (scrl_col_cnt - hscrl_cnt);
			}
		}

		else if (c == fn11_key)							/* Change current buffer size?		*/
		{									/*  (record length)			*/
			int	cl;

			cl = VL_vcur_lin;
			text_defined = FALSE;
			VL_vstate(VSTATE_SAVE);						/* Save where we are in the text.	*/
			VL_vmove(0,0);							/* Home.				*/
			VL_vmode(message_vmode);						/* Use the reverse rendition.		*/
			VL_vprint(" Enter record length: ");
			VL_vmode(edit_vmode);
			VL_vprint("                                                                ");
			VL_verase(TO_EOL);							/* Erase to the end of the line.	*/
			VL_vmove(0,23);							/* Move to the start of input area.	*/
			i = gets0(search_num,SEARCH_NUM_SIZE);				/* Get the record length.		*/
			if (i == CHAR_CR) text_defined = TRUE;				/* The value is now defined.		*/
			else if (i == fn1_key || i == fn16_key)				/* Change was aborted			*/
			{
				text_defined = FALSE;
				search_abort = TRUE;
			}
			else text_defined = FALSE;					/* Unless input was aborted.		*/
			if (search_num[0] == CHAR_NULL) text_defined = FALSE;		/* Not defined if nothing entered.	*/
			if ( text_defined )
			{
				int4	tsize;
				if (field2int4(search_num, strlen(search_num), &tsize))
				{
					werrlog(WERRCODE(66514),search_num,0,0,0,0,0,0,0,0);
				}
				else if (tsize > BUFFER_SIZE || tsize < 0)		/* Zero is valid; means variable	*/
				{
					werrlog(WERRCODE(66514),search_num,0,0,0,0,0,0,0,0);
				}
				else if (strlen(search_num) > SEARCH_NUM_SIZE)
				{
					werrlog(WERRCODE(66514),search_num,0,0,0,0,0,0,0,0);
				}
				else
				{
					VL_verase(FULL_SCREEN);				/* Make sure is really erased.		*/
					record_size = tsize;				/* Set the new record size.		*/
					viewing = FALSE;				/* Stop viewing.			*/
					new_size_requested = TRUE;			/* Asked for a new record size.		*/
				}
			}

			VL_vmove(0,0);							/* Return to the start of the line.	*/
			VL_vmode(VMODE_CLEAR);						/* Reset the clear rendition.		*/
			frow = frow - cl;				
			fcol = 0;
			lb = lbuf[0];							/* Get pointer to buffer.		*/
			set_scroll_buffer(scrl_col_cnt,lb,bufsize);			/* Set ptr and display buffer.		*/
			VL_vstate(VSTATE_RESTORE);					/* Restore where we were.		*/
			VL_vcontrol_flush();						/* Show everything we've done.		*/
		}

		else if (c == fn12_key)							/* Toggle the display of the status line*/
		{
			if (status_active)
			{
				status_active = FALSE;					/* If on then turn off.			*/
				if (ssave != NULL)
				{
					VL_vrss(ssave);						
					ssave = NULL;
				}
			}
			else	status_active = TRUE;
		}

		else if (c == fn13_key)							/* Toggle the stream mode display	 */
		{
			if (stream_active) stream_active = FALSE;			/* If on then turn off.			*/
			else	stream_active = TRUE;

			viewing = FALSE;						/* Stop viewing.			*/
			new_size_requested = TRUE;					/* Ask for a new record size.		*/
			frow = 0;
			fcol = 0;

			if (stream_active && 0 == record_size)
			{
				record_size = 80;
			}
		}

		else if (c == fn14_key)							/* Print current screen?		*/
		{
			char *text, *ttxt;
			char filelibvol[23];
			int size;
			char errstr[80], temp[5];
			int	linesize;

			linesize = (VL_vscr_wid < bufsize) ? VL_vscr_wid : bufsize;

			size = ((linesize+1+8) * MAX_LINES_PER_SCREEN);			/* Compute the size.			*/
			if ((text = wisp_malloc(size)) == 0) mem_err();			/* Allocate a screen buffer.		*/

			ttxt = text;
			for (i = 0; i < MAX_LINES_PER_SCREEN; i++)			/* Init screen buffer to print. */
			{
				if (*lbuf[i])
				{
					memcpy(ttxt,lbuf[i]+scrl_col_cnt,linesize);
				}
				else
				{
					memset(ttxt,' ',linesize);
				}
				ttxt += linesize;
			}
			size = ttxt - text;						/* Get the size of the buffer to send	*/
											/*  to the printer.			*/
			memset(filelibvol,' ',22);
			memset(filelibvol,'#',2);
			memcpy(&filelibvol[2],WL_wanguid3(),3);
			memset(&filelibvol[5],' ',22-5); /* fix bug */
			retcd = 0;
			retcd = WL_di_write_file(text,size,linesize,filelibvol,filelibvol);
			if (!retcd)
			{
				strcpy(errstr,"Current screen has been spooled to printer.");
			}
			else
			{
				strcpy(errstr,"Error ");
				sprintf(temp,"(%d)",retcd);
				strcat(errstr,temp);
				strcat(errstr," when printing screen.");
			}
			derror(errstr);
			free(text);							/* Free memory allocated.		*/
		}

		else if (c == fn15_key)							/* Print file screen?			*/
		{
			char errstr[80], temp[5];
			char	def_prt_mode, def_prt_class;
			int4	def_prt_form;

			WL_get_defs(DEFAULTS_PM,&def_prt_mode);
			WL_get_defs(DEFAULTS_PC,&def_prt_class);
			WL_get_defs(DEFAULTS_FN,&def_prt_form);

			retcd = 0;
			WL_wprint(file_name,def_prt_mode,"DS",1,def_prt_class,def_prt_form,&retcd);
			if (!retcd)
			{
				strcpy(errstr,file_name);
				strcat(errstr," has been spooled to printer.");
			}
			else
			{
				strcpy(errstr,"Error ");
				sprintf(temp,"(%d)",retcd);
				strcat(errstr,temp);
				strcat(errstr," when printing ");
				strcat(errstr,file_name);
				strcat(errstr,".");
			}
			derror(errstr);
		}

		else if (c == fn17_key && WL_pfkeys12())				/* Option screen 			*/
		{
			show_help(file_name,1);
			win_active = TRUE;
			options12_active = TRUE;
		}
		
		else if (c == help_key)							/* Does he/she want help?		*/
		{
			show_help(file_name,0);						/* Yes, so show it.			*/
			win_active = TRUE;						/* Now help is active.			*/
		}

		else if (!(win_just_removed || err_just_removed)) derror("Invalid key.");

		win_just_removed = FALSE;						/* Now window is gone.			*/
		err_just_removed = FALSE;						/* Now messages are gone.		*/
	}

	if (ssave != NULL)
	{
		VL_vrss(ssave);
		ssave = NULL;
	}
	VL_vbuffering_end();								/* Dump all buffers.			*/
	if (restart_requested) goto again;						/* Start again if requested.		*/

#ifdef NOT_YET
	freeblocks();
#endif
	fclose(input_file);								/* Close the file we were viewing.	*/

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++) free(lbuf[i]);			/* Loop through all screen lines.	*/
	VL_vmove(0,0);									/* Home before changing.		*/
	VL_vdefer_restore();								/* Restore from deferred states.	*/
	if (new_size_requested) goto newsize;						/* Start again with new record size.	*/
	VL_vscreen(VSCREEN_NARROW);							/* Restore to a narrow screen.		*/
#ifdef unix
	sleep(1);									/* Delay to allow screen width change	*/
#endif
	VL_voptimize(op_save);								/* Restore optimization.		*/
	VL_vcontrol_flush();								/* Dump the buffer.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

/*
**	Routine:	get_line()
**
**	Function:	Compose an input line.
**
**	Description:	{Full detailed description}...
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
static int get_line(register char *lb,int4 *fpos_ptr,int4 *fpoff_ptr,int bufsize,FILE *input_file)
{
	char 	*l_lb;
	int	inpos, outpos, tcnt, num;
	int	eoline, eofile;								/* End of line and file flags.		*/
	int	c, cnt_off;
	unsigned char the_char;

	eofile = FALSE;									/* Assume not end of file.		*/
	eoline = FALSE;									/* Assume not end of line.		*/

	l_lb = lb;									/* Set ptr to beginning of line.	*/
	outpos = 0;									/* Set the output position in buffer.	*/
	*fpos_ptr = get_pos(input_file);						/* Get the position in the file.	*/

	cnt_off = 0;									/* Init to 0 for position offset in file*/
	for (inpos = 0; outpos < bufsize; inpos++)					 /* Loop until full.			 */
	{
		if ((c = fgetc(input_file)) == EOF) 
		{
			eofile = TRUE;							/* Get the next character.		*/
			break;								/* Exit loop				*/
		}
		else									/* There is data left to read.		*/
		{
			the_char = c;

			/*
			**	In STREAM MODE all control characters are displayed instead
			**	of being acted on.
			*/

			if (!stream_active && (c == LNFD || c == FF || c == CHAR_CR))
			{
				eoline = TRUE;						/* Is newline character or page break?	*/
				if (c == CHAR_CR)
				{
					c = fgetc(input_file);				/* Read the next char.			*/
					cnt_off++;
					if (c != LNFD)
					{
						ungetc(c,input_file);			/* Put char back in file.		*/
						cnt_off--;
					}
				}
				else if (c == FF)
				{
					if (inpos > 0)					/* If have stuff in buffer.		*/
					{
						ungetc(c,input_file);			/* Put char back in file.		*/
						cnt_off--;
					}
					else
					{
						lb = l_lb;				/* Set to beginning of current buffer.	*/
						eop_indicator(lb,bufsize);		/* Init form feed indicator.		*/
						ff_flag = TRUE;
						c = fgetc(input_file);			/* Read the next char.			*/
						cnt_off++;
						if (c != LNFD)
						{
							ungetc(c,input_file);		/* Put char back in file.		*/
							cnt_off--;
						}
						*fpoff_ptr = cnt_off;			/* Set offset for current line in file.	*/
						return(eofile);				/* Return the end of file status.	*/
					}
				}
				break;							/* Exit the loop			*/
			}
			else if (!stream_active && c == '\t')				/* Is it a tab character?		*/
			{
				num = add_tab_cols(outpos);				/* Get number of chars for tab char.	*/
				if (num + outpos >= bufsize)				/* Check if have room in buffer.	*/
				{
					num = bufsize - outpos;				/* Set to available characters left.	*/
				}

				for (tcnt = 0; tcnt < num; tcnt++)			/* Put appropriate number of spaces	*/
				{							/* into the buffer.			*/
					*(lb++) = ' ';
					outpos++;					/* Increment out buffer position.	*/
				}
			}
			else if (!stream_active && c == '\b')
			{
				*(lb++) = c;						/* Insert back space characters.	*/
				outpos++;						/* Increment out buffer position.	*/
			}
			else
			{
				/*
				**	If NATIVECHARMAP then sub from ansi thru wang to term.
				**	This takes presidence over DISPLAY8BIT.
				*/
				if (nativecharmap)	/* Using Native character set */
				{
					vwang_ansi2wang(&the_char, 1);
					vwang_wang2term(&the_char, 1);
				}
				else 			/* Using Wang character set */
				{
					if (the_char >= 128 && !display_high_chars)
					{
						/*
						**	If an 8-bit char and we are not displaying them
						**	then replace with a '.'.
						*/
						the_char = '.';
					}
					else
					{
						vwang_wang2term(&the_char, 1);
					}
				}

				if (vwang_terminal_control_char(the_char))
				{
					/*
					**	Control characters display as '.'
					*/

					*(lb++) = '.';
				}
				else
				{
					*(lb++) = the_char;
				}

				outpos++;						/* Increment out buffer position.	*/
			}
		}
	}

	memset(lb,' ',bufsize - outpos);
	l_lb[bufsize] = CHAR_NULL;
	*lb = CHAR_NULL;								/* Default a null at the end of line.	*/

	*fpoff_ptr = cnt_off;								/* Set offset for current line in file.	*/
	return(eofile);									/* Return the end of file status.	*/
}

static void display_buffer(char *lb)							/* Display correct size of buffer.	*/
{
	char *clb;
	char csave;									/* Variable to hold saved character.	*/
	int pos, np, erasefl, md_save;

	clb = lb;									/* Set to start of buffer.		*/
	erasefl = FALSE;
	np = 0;
	for (pos = 0; pos < VL_vscr_wid; pos++)						/* Do until at correct position for	*/
	{										/* the current screen size.		*/
		if (*clb++ == CHAR_NULL)
		{
			erasefl = TRUE;							/* Make sure erases to end of line.	*/
			pos = VL_vscr_wid;							/* Set so gets out of loop.		*/
		}
		else np++;
	}
	csave = lb[np];									/* Save the char at last pos on screen. */ 
	lb[np] = CHAR_NULL;								/* Stuff a null so displays correctly.	*/
	md_save = VL_vcur_atr;								/* Save the current mode.		*/
	if (ff_flag)
	{
		VL_vmode(VMODE_BOLD);
		ff_flag = FALSE;
	}
	VL_vprint("%s",lb);								/* Display the line.			*/
	if ((VL_vcur_col < VL_vscr_wid-1) || erasefl ) VL_verase(TO_EOL);			/* Erase to the end of the line.	*/
	lb[np] = csave;									/* Put character back into the buffer.	*/
	VL_vmode(md_save);									/* Put back to what it was.		*/
}

static void eof_trailer(const char *file_name)						/* Write the start of file header.	*/
{
	unsigned int	pos;
	unsigned int	size;
	const char *prefix = "End of file: ";
	char lb[256];

	memset(lb,'-',VL_vscr_wid);							/* Store spaces across the line.	*/
	lb[VL_vscr_wid] = '\0';

	size = strlen(file_name) + strlen(prefix);
	if (size >= (unsigned)VL_vscr_wid)
	{
		pos = 2;
	}
	else
	{
		pos = (VL_vscr_wid - size)/2;
		pos = (pos < 2) ? 2 : pos;
	}

	str_insert(&lb[pos],prefix);							/* Insert header message.		*/
	str_insert(&lb[pos+strlen(prefix)],file_name);					/* Insert the file name.		*/
	lb[VL_vscr_wid] = '\0';								/* Null terminate the string.		*/
	VL_vmode(VMODE_CLEAR);								/* Clear the rendition flag.		*/
	display_buffer(lb);								/* Display the line.			*/
}

static void eop_indicator(char *lb, int bufsize)					/* Write the page break indicator.	*/
{
	memset(lb,'-',bufsize);								/* Copy dashes to buffer.		*/
	lb[bufsize-1] = '\0';								/* Null terminate the string.		*/
}

static void str_insert(char *target, const char *source)				/* Insert a string in another string.	*/
{
	while(*source != CHAR_NULL) *(target++) = *(source++);				/* Loop until end of string null.	*/
}

static void show_help(const char *file_name, int options12)				/* Show the help screen.		*/
{
	int lhs;									/* Define location of the help window.	*/
	int columns;									/* Define size of the help window.	*/
	char string[65];								/* Working string.			*/
	char l_filen[256], *lptr;							/* Local copy of file_name.		*/
	register int i;									/* Working counter.			*/
	int len;

	strcpy(l_filen,file_name);							/* Copy file_name to local copy.	*/

	columns = 62;									/* The help area is 62 columns wide.	*/
	lhs = (VL_vscr_wid - columns)/2;							/* Center the help screen.		*/

	VL_vstate(VSTATE_SAVE);								/* Save the current screen state.	*/
	VL_vmove(WINDOW_TOP-1,lhs-1);							/* Draw the outline box.		*/
	VL_vline(VLINE_VERTICAL,WINDOW_ROWS+2);
	VL_vmove(WINDOW_TOP-1,lhs-1+columns);
	VL_vline(VLINE_VERTICAL,WINDOW_ROWS+2);
	VL_vmove(WINDOW_TOP-1,lhs-1);
	VL_vline(VLINE_HORIZONTAL,columns+1);
	VL_vmove(WINDOW_TOP+WINDOW_ROWS,lhs-1);
	VL_vline(VLINE_HORIZONTAL,columns+1);

	for (i = 0; i < 64; i++) string[i] = ' ';					/* Initialize the working string.	*/
	str_insert(&string[2],"Displaying file");					/* Insert the file indicator.		*/
	len = strlen(l_filen);
	lptr = l_filen;									/* Set the pointer.			*/
	for (i = 41; i <= len; i++)							/* Chop off file name from left if too	*/
	{										/* int4 for display purposes.		*/
		lptr++;									/* Set ptr over one char.		*/
	}
	str_insert(&string[18],lptr);							/* Insert the file name.		*/
	string[61] = CHAR_NULL;								/* Store a trailing null.		*/

	i = WINDOW_TOP;									/* Point the help area.			*/

	VL_vmode(high_message_vmode);							/* Do it in reverse underscore,		*/

	VL_vmove(i++,lhs);
	VL_vprint("%s",string);								/* Print the file name.			*/

	VL_vmode(VMODE_CLEAR);								/* Clear all renditions.		*/
	VL_vmode(message_vmode);								/* Do it in reverse.			*/

	VL_vmove(i++,lhs);	VL_vprint(				"  Use the arrow keys to scroll up, down, left and right.     ");

	if (options12)
	{
		VL_vmove(i++,lhs);	VL_vprint(			"   (1) - Home cursor   (2) - File top      (3) - Bottom      ");
		VL_vmove(i++,lhs);	VL_vprint(			"   (4) - Prev screen   (5) - Next screen                     ");
		if (stream_active)
		{
			VL_vmove(i++,lhs);	VL_vprint(		"   (6) - Record size   (7) - Toggle status (8) - Record Mode ");
		}
		else
		{
			VL_vmove(i++,lhs);	VL_vprint(		"   (6) - Change width  (7) - Toggle status (8) - Stream Mode ");
		}
		VL_vmove(i++,lhs);	VL_vprint(			"   (9) - Print screen (10) - Print file                      ");
		VL_vmove(i++,lhs);	VL_vprint(			"  (11) - Options      (12) - Exit                            ");
		VL_vmove(i++,lhs);	VL_vprint(			"                                                             ");
	}
	else
	{
		VL_vmove(i++,lhs);	VL_vprint(			"   (1) - Home cursor   (2) - File top       (3) - Bottom     ");
#if defined(WIN32)
		VL_vmove(i++,lhs);	VL_vprint(			"   (4) - Prev screen   (5) - Next screen                     ");
#else
		if (VL_vscr_wid == 132)
		{
			VL_vmove(i++,lhs);	VL_vprint(		"   (4) - Prev screen   (5) - Next screen    (6) - 80 col     ");
		}
		else 
		{
			VL_vmove(i++,lhs);	VL_vprint(		"   (4) - Prev screen   (5) - Next screen    (6) - 132 col    ");
		}
#endif
		VL_vmove(i++,lhs);	VL_vprint(			"   (7) - Find string   (8) - Find next                       ");
		VL_vmove(i++,lhs);	VL_vprint(			"   (9) - Scroll left  (10) - Scroll right                    ");

		if (WL_pfkeys12())
		{
			VL_vmove(i++,lhs);	VL_vprint(		"  (11) - Options      (12) - Exit                            ");
			VL_vmove(i++,lhs);	VL_vprint(		"                                                             ");
		}
		else
		{
			if (stream_active)
			{
				VL_vmove(i++,lhs);	VL_vprint(	"  (11) - Record size  (12) - Toggle status (13) - Record Mode");
			}
			else
			{
				VL_vmove(i++,lhs);	VL_vprint(	"  (11) - Change width (12) - Toggle status (13) - Stream Mode");
			}
			VL_vmove(i++,lhs);	VL_vprint(		"  (14) - Print screen (15) - Print file    (16) - Exit       ");
		}
	}
	
	VL_vmove(i++,lhs);	VL_vprint(				"  Depress any key to remove this window.                     ");
	VL_vmove(i++,lhs);	VL_vprint(				"  Use the HELP key to bring this window back.                ");

	VL_vstate(VSTATE_RESTORE);								/* Reset rendition to normal.		*/
}

static void rep_limit(void)								/* Report the limit of upward scroll.	*/
{
	int columns;									/* Define size of the help window.	*/
	int lhs;									/* Define location of the help window.	*/
	register int i;									/* Working counter.			*/

	columns = 60;									/* Width of the report area.		*/
	VL_vbell();									/* Let him/her have an audio alert.	*/
	while (VL_vcheck() != 0);								/* Flush all type ahead.		*/

	lhs = (VL_vscr_wid - columns)/2;							/* Center the help screen.		*/

	VL_vstate(VSTATE_SAVE);								/* Save the current screen state.	*/
	VL_vmove(WINDOW_TOP-1,lhs-1);							/* Draw the outline box.		*/
	VL_vline(VLINE_VERTICAL,WINDOW_ROWS+2);
	VL_vmove(WINDOW_TOP-1,lhs-1+columns);
	VL_vline(VLINE_VERTICAL,WINDOW_ROWS+2);
	VL_vmove(WINDOW_TOP-1,lhs-1);
	VL_vline(VLINE_HORIZONTAL,columns+1);
	VL_vmove(WINDOW_TOP+WINDOW_ROWS,lhs-1);
	VL_vline(VLINE_HORIZONTAL,columns+1);

	i = WINDOW_TOP;									/* Point the help area.			*/
	VL_vmode(high_message_vmode);							/* Do it in reverse underscore,		*/
	VL_vmove(i++,lhs);
	VL_vprint("                Reverse Scroll Limit Reached               ");		/* Print the message header.		*/

	VL_vmode(VMODE_CLEAR);								/* Clear all renditions.		*/
	VL_vmode(message_vmode);								/* Do it in reverse.			*/
	VL_vmove(i++,lhs);
	VL_vprint("  It is not possible to scroll backwards beyond this point ");
	VL_vmove(i++,lhs);
	VL_vprint("  because the reverse movement limit has been reached.  In ");
	VL_vmove(i++,lhs);
	VL_vprint("  order to view text in front of this screen, use option 2 ");
	VL_vmove(i++,lhs);
	VL_vprint("  to go to the start of this file and then scroll forward. ");
	VL_vmove(i++,lhs);
	VL_vprint("                                                           ");
	VL_vmove(i++,lhs);
	VL_vprint("                                                           ");
	VL_vmove(i++,lhs);
	VL_vprint("                                                           ");
	VL_vmove(i++,lhs);
	VL_vprint("  Depress any key to remove this message...                ");
	VL_vstate(VSTATE_RESTORE);								/* Reset rendition to normal.		*/
}

static void clear_window(char *lbuf[],int roll_cnt,int bufsize)				/* Subroutine to clear the help area.	*/
											/* Pointer to the line input buffers.	*/
{
	register int i,j;								/* Working registers.			*/
	char *lb;									/* Working pointer.			*/

	if ((j = WINDOW_TOP - 1) >= MAX_LINES_PER_SCREEN) j = j - MAX_LINES_PER_SCREEN; /* Get the first line.			*/

	VL_vstate(VSTATE_SAVE);								/* Save all the event info.		*/
	VL_vmode(VMODE_CLEAR);								/* Clear all renditions.		*/
	VL_vcharset(VCS_DEFAULT);								/* Clear the current character set.	*/

	for (i = WINDOW_TOP-1; i < WINDOW_TOP + WINDOW_ROWS + 1; i++)			/* Loop through the whole help area.	*/
	{
		VL_vmove(i,0);								/* Move to the start of the line.	*/
		lb = lbuf[j];								/* Point to this line buffer.		*/
		set_scroll_buffer(roll_cnt,lb,bufsize);					/* Set ptr and display buffer.		*/
		j++;									/* Move to the next line.		*/
		if (j >= MAX_LINES_PER_SCREEN) j = 0;					/* Did we wrap around?			*/
	}
	VL_vstate(VSTATE_RESTORE);								/* Restore where we were.		*/
}

/*
**	Routine:	work_message
**
**	Function:	Flash the working message.
**
**	Description:	
**			on = 0		Start the process; Draw the box
**			on = -1		Flash off	(Clear the center of the box)
**			on = 1		Flash on	(Write "Working" in the box)
**
**	Returns:	The next value of "on" to use.
*/
static int work_message(int on)								/* Display the working message.		*/
{
	int lhs;									/* Left hand side of display.		*/
	register int i=0;								/* Working register.			*/

	VL_vstate(VSTATE_SAVE);								/* Save wher we are.			*/

	lhs = (VL_vscr_wid - 16)/2;							/* center the message			*/

	if (on == 0)									/* First time?				*/
	{
		VL_vmove(WINDOW_TOP,lhs-2);						/* Draw the outline box.		*/
		VL_vline(VLINE_VERTICAL,3);
		VL_vmove(WINDOW_TOP,lhs+13);
		VL_vline(VLINE_VERTICAL,3);
		VL_vmove(WINDOW_TOP,lhs-2);
		VL_vline(VLINE_HORIZONTAL,16);
		VL_vmove(WINDOW_TOP+2,lhs-2);
		VL_vline(VLINE_HORIZONTAL,16);
	}

	if (on <= 0)
	{
		VL_vmove(WINDOW_TOP+1,lhs-1);						/* Move to the clear area.		*/
		VL_vmode(VMODE_CLEAR);							/* Clear it out.			*/
		VL_vprint("              ");						/* Output spaces.			*/
		i = 1;									/* Turn on next time.			*/
	}

	if (on >= 0)									/* On or off?				*/
	{
		VL_vmove(WINDOW_TOP+1,lhs);						/* Move to the info area.		*/
		VL_vmode(message_vmode);							/* Select something noticeable.		*/
		VL_vprint(" Working... ");							/* Display the working message.		*/
		i = -1;									/* Turn off the next time.		*/
	}

	VL_vstate(VSTATE_RESTORE);								/* Restore where we were.		*/
	VL_vcontrol_flush();								/* Show what is going on.		*/
	return(i);									/* Return the new state.		*/
}

/*
**	Routine:	roll_up()
**
**	Function:	Scroll the display up one line.
**
**	Description:	
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
static int roll_up(char *lbuf[],int4 fpos_tab[],int4 fpoff_tab[],int4 *pt,int4 *pto,int bufsize,FILE *input_file,
                   int *tx,int *lc,int send_out,int roll_cnt)
{
	int4 temp_position, temp_pos_offset;
	int eofile;									/* End of file flag.			*/
	char temp_line[BUFFER_SIZE+8];
				     
	eofile = FALSE;									/* Assume not end of file.		*/
	temp_pos_offset = 0;
	
	eofile = get_line(temp_line,&temp_position,&temp_pos_offset,bufsize,input_file); /* Get another line.			*/
	if (!eofile || (eofile && *temp_line != '\0'))					/* Are we at the end of file?		*/
	{
		if (send_out)								/* Do they want output?			*/
		{
			VL_vstate(VSTATE_SAVE);						/* Save where we are.			*/
			if (frwd_scroll_avail)
			{
				VL_vmove(MAX_LINES_PER_SCREEN-1,VL_vcur_col);			/* Move to the bottom line.		*/
				VL_vprint("\n");						/* Scroll forward.			*/
				set_scroll_buffer(roll_cnt,temp_line,bufsize);		/* Set ptr and display buffer.		*/
			}
			VL_vstate(VSTATE_RESTORE);						/* Restore where we were.		*/
		}

		pt[*tx] = fpos_tab[0];							/* Remember position of line off top.	*/
		pto[*tx] = fpoff_tab[0];
		*tx = *tx + 1;								/* Increment the table index.		*/
		if (*tx >= POSITION_TABLE_SIZE) *tx = 0;				/* Handle if it wrapped around.		*/
		*lc = *lc + 1;								/* Increment the entry counter.		*/
		if (*lc > POSITION_TABLE_SIZE) *lc = POSITION_TABLE_SIZE;		/* Don't let it overflow.		*/

		adjust_buffers(lbuf,fpos_tab,fpoff_tab,1);				/* Adjust data buffers up one.		*/
		memcpy(lbuf[MAX_LINES_PER_SCREEN-1],temp_line,bufsize);

		fpos_tab[MAX_LINES_PER_SCREEN - 1] = temp_position;			/* Remember the new position.		*/
		fpoff_tab[MAX_LINES_PER_SCREEN - 1] = temp_pos_offset;

	}
	return(eofile);									/* Return the end of file indicator.	*/
}

static void roll_down(char *lbuf[],int4 fpos_tab[],int4 fpoff_tab[],int4 *pt,int4 *pto,int bufsize,FILE *input_file,
                 int *tx,int *lc,int *eofile,int roll_cnt)
{
	char *nlb, *temp_line;								/* Temporary storage.			*/
	int4 temp_position, temp_pos_offset;
	int4 temp_fp, temp_fpo;
	register int e, i;

	if (*lc != 0)									/* Can we still scroll backwards?	*/
	{
		VL_vstate(VSTATE_SAVE);							/* Save where we were.			*/
		if (rvrs_scroll_avail)
		{
			VL_vmove(0,0);							/* Move to the top of the screen.	*/
			VL_vnewline(VLF_REVERSE);						/* Scroll backwards.			*/
		}
		*tx = *tx - 1;								/* Decrement the index.			*/
		if (*tx < 0) *tx = POSITION_TABLE_SIZE-1;				/* Wrap around as appropriate.		*/
		*lc = *lc - 1;								/* Decrement the backup line count.	*/
		temp_fp = fpos_tab[MAX_LINES_PER_SCREEN - 1];				/* Remember where the file was.		*/
		temp_fpo = fpoff_tab[MAX_LINES_PER_SCREEN - 1];
		e = fseek(input_file,pt[*tx],0);					/* Seek the previous record.		*/
		temp_pos_offset = pto[*tx];						/* Set the current offset in record.	*/

		if (!e)									/* If was a successful seek.		*/
		{
			if ((temp_line = (char*)wisp_malloc(bufsize+1+8)) == 0) mem_err(); /* Allocate a line buffer	*/
			get_line(temp_line,&temp_position,&temp_pos_offset,bufsize,input_file); /* Get another line.		*/
			if (rvrs_scroll_avail) set_scroll_buffer(roll_cnt,temp_line,bufsize); /* Set ptr and display buffer.	*/
			adjust_buffers(lbuf,fpos_tab,fpoff_tab,2);			/* Adjust data buffers down one.	*/
			nlb = lbuf[0];							/* Get address of first buffer.		*/
			for (i = 0; i < bufsize; i++) *nlb++ = temp_line[i];		/* Copy the string.			*/
			fpos_tab[0] = temp_position;					/* Remember the new position.		*/
			free(temp_line);
		}
		VL_vstate(VSTATE_RESTORE);							/* Restore where we were.		*/

		e = fseek(input_file,temp_fp,0);					/* Reset to read bottom line again.	*/
		*eofile = FALSE;							/* Cannot be at end of file anymore.	*/
	}
}

static void roll_left_right(char *lbuf[], int4 fpos_tab[], int bufsize,FILE *input_file,int *eorec,int roll_cnt)
											/* Scroll left or right.		*/
{
	char *lb;
	register int i, c;
	int sc,sl;									/* Variable to save current cursor pos. */
	int cont;									/* Continue flag.			*/

	sl = VL_vcur_lin;									/* Save the current cursor position.	*/
	sc = VL_vcur_col;

	*eorec = FALSE;									/* Assume not at end of record.		*/
	cont = TRUE;
	i = 0;										/* Set to current top of screen ptr.	*/
	c = 0;
	VL_vbuffering_start();
	while (cont)									/* Read in a new screen's worth.	*/
	{
		lb = lbuf[c];								/* Make a working copy.			*/
		VL_vmove(c,0);								/* Move to beginning of the line.	*/
		set_scroll_buffer(roll_cnt,lb,bufsize);					/* Set ptr and display buffer.		*/
		i++;									/* Increment current line counter.	*/
		c++;									/* Increment # of lines scrolled.	*/
		if ((c >= MAX_LINES_PER_SCREEN) || (fpos_tab[i] == -1)) cont = FALSE;
	}
	VL_vmove(sl,sc);									/* Put cursor in original position.	*/
	VL_vbuffering_end();
	if (roll_cnt >= bufsize - VL_vscr_wid) *eorec = TRUE;				/* Set end of buffer/record.		*/
}

static void set_scroll_buffer(int roll_cnt, char *lb, int bufsize)			/* Set pointer to buffer to be at the	*/
											/* current scroll column.		*/
{
	int i;

	for (i = 0; i < roll_cnt; i++) lb++;						/* Scroll the buffer according to data. */
	display_buffer(lb);								/* Display the line.			*/
}

/*
**	Routine:	add_tab_cols()
**
**	Function:	Calc number of spaces to substitue for a tab.
**
**	Description:	This routine is past a column position and it returns the
**			number of spaces that need to be substituted for a tab character
**			that occurs in that column.  The number returned can be 1 to 8.
**
**	Arguments:
**	col		The column position (1-255)
**
**	Globals:	None
**
**	Return:		Number of spaces needed to substitute.
**
**	Warnings:	None
**
**	History:	
**	09/23/93	Modified by GSL
**
*/
static int add_tab_cols(int col)
{
	return(TAB_SPACE_COUNT - (col % TAB_SPACE_COUNT));
}

static int on_screen(char *search_text, char *lbuf[], int4 fpos_tab[],int4 *pt, int bufsize,
			FILE *input_file,int *tx,int *lc,int *eofile,int *eorec,int *roll_cnt)
											/* Is the text on the screen.		*/
{
	char *lb;									/* Temporary storage.			*/
	register int i, j;								/* Working registers.			*/
	int first_in;									/* Flag so will step past if first line.*/
	int k;										/* Tab count so display cursor correctly*/

	i = VL_vcur_lin;									/* Set to the current line.		*/
	j = *roll_cnt + VL_vcur_col;							/* Set to the current column in buffer. */
	if ((j+1) >= bufsize)								/* At the edge of the buffer?		*/
	{
		j = 0;									/* Yes, so start on the next line.	*/
		if ((i = i+1) >= MAX_LINES_PER_SCREEN) return(FALSE);			/* Off bottom of the screen?		*/
	}
	first_in = TRUE;								/* Set because checking first line in.	*/
	while (i < MAX_LINES_PER_SCREEN)						/* Loop through every line on screen.	*/
	{
		lb = lbuf[i];								/* Get pointer to the buffer.		*/
		if (j > 0 || first_in)							/* Set ptr to step past occurance.	*/
		{
			for (k = 0; k < j; k++) lb++;					/* Step to current occurance.		*/
			lb++;								/* Step past current occurance.		*/
			j++; 
			first_in = FALSE;						/* Set so will on do if j > 0.		*/
		}
		/*stb(temp,lb,j,bufsize);*/						/* Load the temporary string.		*/
		if ((k = stx(lb,search_text)) >= 0)					/* Is the string in this string?	*/
		{
			int clin;

			clin = j + k;							/* Assign position on current screen.	*/
			if (clin >= *roll_cnt + VL_vscr_wid)				/* Is to right of current screen.	*/
			{								/* Set up scroll column count.		*/
				while (clin >= *roll_cnt + VL_vscr_wid) *roll_cnt = *roll_cnt + VL_vscr_wid;
				if (*roll_cnt > bufsize - VL_vscr_wid) *roll_cnt = bufsize - VL_vscr_wid;
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,eorec,*roll_cnt);
				clin = clin - *roll_cnt;				/* Set cursor position on new screen.	*/
			}
			else if (clin < *roll_cnt)					/* Is to left of current screen.	*/
			{								/* Set up scroll column count.		*/
				while (clin < *roll_cnt) *roll_cnt = *roll_cnt - VL_vscr_wid;
				if (*roll_cnt < 0) *roll_cnt = 0;			/* Can only scroll to beg. of line.	*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,eorec,*roll_cnt);
			}
			if (clin > VL_vscr_wid) clin = clin - *roll_cnt;			/* Set so is on current screen.		*/

			frow += i - VL_vcur_lin;
			fcol = *roll_cnt + clin;

			VL_vmove(i,clin);							/* Move to where the string is.		*/
			return(TRUE);							/* And we're all done.			*/
		}
		i++;									/* Move to the next line		*/
		j = 0;									/* Move to the left hand column.	*/
	}
	return(FALSE);									/* The string is not on the screen.	*/
}


static int stx(char *string, char *mask)						/* Get string index in another string.	*/
{
	int m_idx, s_idx, cmpval;
	int m_len, s_len;
	int save;

	m_len = srch_len - 1;
	s_len = strlen(string);
	save = s_idx = m_idx = m_len;
	while (s_idx < s_len)
	{
		while ( m_idx >= 0 )
		{
			cmpval =  mask[m_idx] ^ string[s_idx] ;
			if (cmpval && (cmpval != 0x20)) break;
			cmpval = mask[m_idx] & 0xdf;
			if (cmpval < 'A' || cmpval >'Z')
				if (mask[m_idx] != string[s_idx]) break;
			--m_idx; --s_idx;
		}
		if (m_idx < 0) return s_idx + 1;
		else
		{
			while (srch_ch_map[(int)string[s_idx]] && (save-s_idx < m_len)) --s_idx;
			save = s_idx += m_len + 1;
			m_idx = m_len;
		}
	}
	return -1;
}

/*
**	Routine:	in_file()
**	
**	Function:	To see if the search_text exists in the file.
**
**	Description:	This routine is only called after on_screen() has been called, so we known the text is not on the
**			currently displayed screen.  It will then roll_up thru the file one line at a time until it finds
**			a line with a match.  It then fudges up a call to on_screen(), since we know that the search_text is
**			now on the screen.  If the search_text is not found in the file it restores the previous position.
**
*/
											/* Is the text in the input file?	*/
static int in_file(char *search_text,char **lbuf,int4 fpos_tab[],int4 fpoff_tab[],int4 *pt,int4 *pto,
                   int bufsize,FILE *input_file,int *tx,int *lc,int *eofile,int *eorec,int *roll_cnt) 
{
	char lbuf_save[MAX_LINES_PER_SCREEN][BUFFER_SIZE+8];				/* Save the current screen.		*/
	int4 fp_save[MAX_LINES_PER_SCREEN];						/* Save the current pointers.		*/
	int4 pt_save[POSITION_TABLE_SIZE];						/* Save the position table.		*/
	int tx_save, lc_save;								/* Save the index pointers.		*/
	int lin_save,col_save;								/* Position save locations.		*/
	int4 pos_save;									/* Save current file pointer.		*/
	int found;									/* Flag to indicate text found.		*/
	int lines_read;									/* Lines read in search.		*/
	char *lb;							/* Temporary working buffer.		*/
	register int i,k;								/* Working registers.			*/
	register int check_time;
	int	on, l_eof;

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Save the current data buffers.	*/
	{
		memcpy(lbuf_save[i],lbuf[i],bufsize);					/* Save the line buffer.		*/
		fp_save[i] = fpos_tab[i];						/* Save the file position value.	*/
	}
	for (i = 0; i < POSITION_TABLE_SIZE; i++) pt_save[i] = pt[i];			/* Save the position table.		*/
	tx_save = *tx;									/* Save the position table index.	*/
	lc_save = *lc;									/* Save the position table counter.	*/
	pos_save = get_pos(input_file);							/* Save where we are in the file.	*/
	lin_save = VL_vcur_lin;								/* Remember the cursor position.	*/
	col_save = VL_vcur_col;

	VL_vset_cursor_off();								/* Don't need the cursor for a while.	*/
	on = 0;										/* Working counters.			*/
	check_time = 0;
	found = FALSE;									/* Haven't found the string yet.	*/
	lines_read = 0;									/* No lines read yet.			*/

	l_eof = *eofile;
	while (!l_eof && !found)							/* Loop to find the end of the file.	*/
	{
		l_eof = roll_up(lbuf,fpos_tab,fpoff_tab,pt,pto,
                                bufsize,input_file,tx,lc,0,*roll_cnt);			/* Read a line.				*/

		ff_flag = FALSE; /* Temp fix to misc highlighting bug */

		if (!l_eof)								/* If not end of file?			*/
		{
			lines_read++;							/* Count the lines read.		*/
			k = MAX_LINES_PER_SCREEN-1;
			lb = lbuf[k];							/* Get ptr to line.			*/
			/*stb(temp,lb,0,bufsize);*/					/* Get the working string.		*/
			if ((found = stx(lb,search_text)) >= 0)				/* Is the string on this line?		*/
			{
				int	lines_scrolled;
				found = TRUE;						/* Found the string.			*/

				frow += (MAX_LINES_PER_SCREEN - lin_save - 2) + lines_read;

				lines_scrolled = 0;
				if (lines_read > PAGE_SCROLL_LINES)			/* If we've read most of a page..	*/
				{
					for(i=0;i<MAX_LINES_PER_SCREEN-12;i++)		/* Position match at row 12		*/
					{
						/*
						**	The search_text was found and is currently on the last line.
						**	Scroll it up to row 12.
						**
						**	NOTE:	Use the real eofile instead of local since we are really moving
						**		ahead in the file.
						*/
						*eofile = roll_up(lbuf,fpos_tab,fpos_tab,pt,pto,
                                                                  bufsize,input_file,tx,lc,0,*roll_cnt);
						if (*eofile) break;
						lines_read++;
					}
					lines_scrolled = i;
				}
				scr_refresh(lbuf,fpos_tab,lines_read,bufsize,*roll_cnt);	/* Yes, then write the string.	*/
				VL_vmove(MAX_LINES_PER_SCREEN-(2+lines_scrolled),VL_vscr_wid-1); /* Move to the end of previous line. */
				on_screen(search_text,lbuf,fpos_tab,pt,bufsize,input_file,
						tx,lc,eofile,eorec,roll_cnt);
			}
			else								/* Not found on this line so...		*/
			{
				found = FALSE;						/* Set not found flag.			*/
				if (check_time++ > CHECK_INTERVAL)			/* Time to update the message?		*/
				{
					check_time = 0;					/* Reset the counter.			*/
					on = work_message(on);				/* Output flashing working message.	*/

					if (VL_vcheck() != 0)				/* Check of a keypress interupt		*/
					{
						for (i = 0; i < MAX_LINES_PER_SCREEN; i++) /* Restore the current data buffers. */
						{
							memcpy(lbuf[i],lbuf_save[i],bufsize);	/* Restore the line buffer.	*/
							fpos_tab[i] = fp_save[i];	/* Restore the file position value.	*/
						}
						for (i = 0; i < POSITION_TABLE_SIZE; i++) 
							pt[i] = pt_save[i];		/* Restore the position table.		*/
						*tx = tx_save;				/* Restore the position table index.	*/
						*lc = lc_save;				/* Restore the position table counter.	*/
						fseek(input_file,pos_save,0);		/* Restore the file position pointer.	*/
						VL_vmove(lin_save,col_save);		/* Now move back to where we were.	*/
						found = 2;				/* Set so will display correct message. */
						break;					/* Break out of working loop.		*/
					}
				}
			}
		}
		else									/* No string, at end of file.		*/
		{
			for (i = 0; i < MAX_LINES_PER_SCREEN; i++)			/* Restore the current data buffers.	*/
			{
				memcpy(lbuf[i],lbuf_save[i],bufsize);			/* Restore the line buffer.		*/
				fpos_tab[i] = fp_save[i];				/* Restore the file position value.	*/
			}
			for (i = 0; i < POSITION_TABLE_SIZE; i++) pt[i] = pt_save[i];	/* Restore the position table.		*/
			*tx = tx_save;							/* Restore the position table index.	*/
			*lc = lc_save;							/* Restore the position table counter.	*/
			fseek(input_file,pos_save,0);					/* Restore the file position pointer.	*/
			VL_vmove(lin_save,col_save);					/* Now move back to where we were.	*/
			*eofile = FALSE;						/* No longer end of file.		*/
		}
	}

	clear_window(lbuf,*roll_cnt,bufsize);						/* Clear away the area.			*/
	VL_vset_cursor_on();								/* Now restore the flashing cursor.	*/
	return(found);
}

static void scr_refresh(char *lbuf[],int4 fpos_tab[],int lines,int bufsize,int roll_cnt)
											/* Refresh screen from line buffers.	*/
{
	register int i, j, k;								/* Working registers.			*/
	char *lb;

	j = 0;										/* Point j to the top of the screen.	*/
	k = 0;										/* Assume start at top of screen.	*/

	if (lines < MAX_LINES_PER_SCREEN-1)						/* Write the whole screen?		*/
	{
		VL_vmove(MAX_LINES_PER_SCREEN-1,0);					/* No so move to the bottom.		*/
		for (i = 0; i < lines; i++) VL_vprint("\n");				/* Scroll up.				*/
		k = MAX_LINES_PER_SCREEN - lines;					/* Determine where to start.		*/
		j = k;
	}

	for (i = k; i < MAX_LINES_PER_SCREEN; i++)					/* Now display the file.		*/
	{
		VL_vmove(i,0);								/* Move to the start of the line.	*/
		lb = lbuf[j];								/* Get pointer to current buffer.	*/
		set_scroll_buffer(roll_cnt,lb,bufsize);					/* Set pointer to current scroll column.*/
		j++;									/* Move to the next line.		*/
		if (j >= MAX_LINES_PER_SCREEN) j = 0;					/* Handle the wrap around.		*/
	}
	VL_vmove(MAX_LINES_PER_SCREEN-1,VL_vscr_wid-1);					/* Move to the standard cursor pos.	*/
}

/*
**	Routine:	adjust_buffers()
**
**	Function:	Adjust buffer address to reflect what is really on the screen.
**
**	Description:	
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
static void adjust_buffers(char *lbuf[],int4 fpos_tab[],int4 fpoff_tab[],int direction)
{
	register int i=0;
	char *tbufa;

	if (direction == 1)								/* Move buffers forward one.		*/
	{
		tbufa = lbuf[0];							/* Get the address of first buffer.	*/
		for (i = 0; i < MAX_LINES_PER_SCREEN-1; i++)				/* Do for each buffer line.		*/
		{
			lbuf[i] = lbuf[i+1];						/* Assign the new address.		*/
			fpos_tab[i] = fpos_tab[i+1];					/* Assign the file position value.	*/
			fpoff_tab[i] = fpoff_tab[i+1];
		}
	}
	else if (direction == 2)							/* Move buffers backward one.		*/
	{
		tbufa = lbuf[MAX_LINES_PER_SCREEN-1];					/* Get the address of last buffer.	*/
		for (i = MAX_LINES_PER_SCREEN-1; i > 0; i--)				/* Do for each buffer line.		*/
		{
			lbuf[i] = lbuf[i-1];						/* Assign new address.			*/
			fpos_tab[i] = fpos_tab[i-1];					/* Assign the file position value.	*/
			fpoff_tab[i] = fpoff_tab[i-1];
		}
	}
	lbuf[i] = tbufa;								/* last/first buffer pts to first/last. */
	fpos_tab[i] = 0;								/* Set so no position.			*/
	fpoff_tab[i] = 0;
}

static void full_screen_refresh(char *lbuf[], int row,int col)
{
	int	i;
	VL_verase(FULL_SCREEN);
	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Display screen's worth.		*/
	{
		display_buffer(lbuf[i]);						/* Display the line.			*/
		if (i != MAX_LINES_PER_SCREEN-1) VL_vprint("\n");				/* Move to next line if appro.	*/
	}
	VL_vmove(row,col);									/* Reposition the cursor		*/
}

static void derror(char *text)								/* Error message reporting routine	*/
{
	int col, lin, mod, set;								/* Save locations.			*/
	
	lin = VL_vcur_lin;									/* Save what we're doing.		*/
	col = VL_vcur_col;
	set = VL_vchr_set;
	mod = VL_vcur_atr;

	VL_vmode(VMODE_CLEAR);								/* Clear all renditions.		*/
	VL_vmode(high_message_vmode);							/* Select the reverse rendition.	*/
	VL_vcharset(VCS_DEFAULT);								/* Assume the default position.		*/
	VL_vmove(MAX_LINES_PER_SCREEN-1,0);						/* Move to the error location.		*/
	if (text[0] != ' ') VL_vbell();							/* Ring the bells.			*/
	VL_vprint("%s",text);								/* Print the error message.		*/

	while (VL_vcur_col < VL_vscr_wid-1) VL_vprint(" ");					/* Space to the end of line.		*/

	VL_vmove(lin,col);									/* Restore where we were.		*/
	VL_vmode(mod);
	VL_vcharset(set);

	while (VL_vcheck() != 0);								/* Purge all type ahead.		*/
	errmsg_active = TRUE;								/* Now we have an error message.	*/
}

static int gets0(char *string, int count)
{
	int lin,col,set,mod;									/* Save locations.		*/
	int i;											/* Working variables.		*/
	int c;
	int term;										/* Terminator.			*/

	i = 0;											/* Assume no chars yet.		*/
	term = 0;										/* No terminator depressed yet. */

	if (count)										/* Does he want any chars?	*/
	{
		VL_vmode(VMODE_CLEAR);
		VL_vmode(edit_vmode);

		while ((i < count) && !term)							/* Repeat until term or full.	*/
		{
			c = VL_vgetm();								/* Get the meta character.	*/

			if (errmsg_active)							/* Error message?		*/
			{
				lin = VL_vcur_lin;							/* Save what we are doing.	*/
				col = VL_vcur_col;
				set = VL_vchr_set;
				mod = VL_vcur_atr;
				VL_vmove(MAX_LINES_PER_SCREEN-1,0);				/* Erase the error message.	*/
				VL_vmode(VMODE_CLEAR);
				VL_vcharset(VCS_DEFAULT);
				VL_verase(TO_EOL);
				VL_vmode(edit_vmode);
				VL_vcharset(set);
				VL_vmove(lin,col);
			}

			if (c == return_key || c == enter_key) term = c = CHAR_CR;		/* Return terminator.		*/

			else if (c == delete_key)						/* Is it a delete?		*/
			{
				if (i)								/* Anything to rub out?		*/
				{
					VL_vprint("\010 \010");					/* Rub out the last char.	*/
					i = i - 1;						/* Set counter back one char.	*/
				}
			}

			else if ( c == fn1_key || c == fn16_key )
			{
				term = c;
			}

			else if (c >= 128)							/* Is this an special char?	*/
			{
				derror("Invalid character, character ignored.");
			}

			else if (c == CHAR_CTRL_U)						/* A control U?			*/
			{
				while (i)							/* Continue until all gone.	*/
				{
					VL_vprint("\010 \010");					/* Rub out the last char.	*/
					i = i - 1;						/* Set counter back one char.	*/
				}
			}

			else if (c < ' ')							/* Is it a control char?	*/
			{
				derror("Control characters cannot be used in a search string, character ignored.");
			}

			else
			{
				VL_vputc((char)c);							/* Echo the character.		*/
				string[i++] = (char)c;						/* Record the string.		*/
			}
		}
	}

	string[i] = CHAR_NULL;									/* Terminate the string.	*/
	return(term);										/* Return the terminator.	*/
}

/*	Get the postion of a file.												*/
/*	The algorithm used in vdisplay() is based on saving the file position at the start of records. I.e. get_pos() is only	*/
/*	called when the current position is at the start of a record. This algorighm requires only the standard I/O routines	*/
/*	ftell() and fseek() to handle both variable length (stream) and fixed length files (the record length is passed to	*/
/*	vdisplay() when it is called). Under VAX/VMS, the VAX-C run-time library automatically processes RMS indexed files	*/
/*	returning meaningfull positions to the start of records based on the sorted primary key. Hence, display works for VMS	*/
/*	VMS indexed files too.	For UNIX, depending on how indexed files are implemented, this routine (get_pos()) might have	*/
/*	to updated so that get_pos() returns the file position of the next record to be read (i.e. the primary key is scanned	*/
/*	and used to determine the positon of the next record). Provided this routine (get_pos()) functions correctly, vdisplay	*/
/*	will be able to display virtually any file.										*/

static int4 get_pos(FILE *input_file)							/* Get the position of a record.	*/
{
	return(ftell(input_file));							/* Return the record position.		*/
}

static void mem_err(void)								/* Memory error - could not allocate	*/
{											/* the needed memory.			*/
	werrlog(WERRCODE(66508),0,0,0,0,0,0,0,0);
	VL_vexit();									/* Unconditional exit.			*/
	wexit(WERRCODE(66508));
}


#ifdef NOT_YET

/*
	The following set of routines replace fgetc ungetx fseek and ftell.  They bring the data into memory blocks and
	manage these blocks.
	
*/

int xfgetc();
int xungetc();
int xfseek();
int4 xftell();

static int	blocks_loaded=0;			/* Number of blocks loaded */
static int	curr_block=0;				/* The current block in use (first == 0) */
static int	curr_pos=0;				/* The current position in curr_block (offset) */
static int4	curr_byte=0;				/* The current byte (offset) */
static int	file_loaded=0;				/* Is the file fully loaded */
static int	highest_byte=0;				/* The highest byte loaded */

#define MAX_MEMBLOCKS	1000
struct
{
	char	*blockptr;				/* Ptr to the block */
	int4	highbyte;				/* The highest byte in this block */
	int4	msize;					/* The malloc size */
	int4	usize;					/* The used size */
} memblock[MAX_MEMBLOCKS];


static int xfgetc()
{
	int	c;

	if (curr_pos >= memblock[curr_block].usize)
	{
		curr_block++;
		curr_pos = 0;
	}

	if (curr_block>=blocks_loaded)
	{
		if (file_loaded)
		{
			return(EOF);
		}
		loadblock();								/* Load the next block			*/
	}

	if (curr_pos >= memblock[curr_block].usize)
	{
		return(EOF);
	}

	c = (int) memblock[curr_block].blockptr[curr_pos];
	curr_pos++;
	curr_byte++;
	return(c);	
}
static int xungetc()
{
	curr_pos--;
	curr_byte--;
	return(0);
}
static int xfseek(dummy,byte_pos)
FILE	*dummy;
int4	byte_pos;
{
	int	blk;

	for (blk=0; blk<blocks_loaded; blk++)
	{
		if ( byte_pos < memblock[blk].highbyte )
		{
			break;
		}
	}
	/* Let's assume this always works */

	curr_block = blk;
	curr_byte = byte_pos;

	if (blk==0)
	{
		curr_pos = curr_byte;
	}
	else
	{
		curr_pos = memblock[curr_block].usize - (memblock[curr_block].highbyte-curr_byte);
	}
	return(0);
}
static int4 xftell()
{
	return(curr_byte);
}

static loadblock()
{
	int	c;
	char *ptr;
	int4	cnt;
	unsigned mallsize;

	if (file_loaded) return;

	if (blocks_loaded>=MAX_MEMBLOCKS)
	{
		file_loaded = 1;							/* Force a file loaded			*/
		return;
	}

	if (blocks_loaded == 0)
	{
		mallsize = (32 * 1024) - 100;						/* First time read 32K	(fast start)	*/
	}
	else
	{
		mallsize = (256 * 1024) - 100;						/* Next use 256K			*/
	}

	if (ptr = wisp_malloc(mallsize))							/* Malloc a memory block		*/
	{
		memblock[blocks_loaded].blockptr = ptr;
	}
	else
	{
		mem_err();
	}

	cnt = 0;
	for(cnt=0;cnt<mallsize;cnt++)							/* Load one block into memory		*/
	{
		c = fgetc(input_file);
		if (c == EOF)
		{
			file_loaded =1;
			break;
		}
		else
		{
			*ptr++ = c;
		}
	}

	memblock[blocks_loaded].msize = mallsize;
	memblock[blocks_loaded].usize = cnt;
	highest_byte += cnt;
	memblock[blocks_loaded].highbyte = highest_byte;
	blocks_loaded++;
}
static freeblocks()
{
	int	blk;

	for (blk=0; blk<blocks_loaded; blk++)
	{
		free(memblock[blk].blockptr);
	}
}

#endif	/*  NOT_YET	*/

static int check_empty(const char *file_name)						/* See if the file is empty.		*/
{
	int 	rc;
	FILE	*fh;

	if ((fh = fopen(file_name,FOPEN_READ_BINARY)) == NULL)				/* Did the file open?			*/
	{
		werrlog(WERRCODE(66506),file_name,0,0,0,0,0,0,0);
		return(1);								/* It is fatal so return with error.	*/
	}
	fgetc(fh);
	rc = feof(fh);
	fclose(fh);
	return(rc);
}

static void display_stat(int dwidth)							/* Display cursor row, col and width.	*/
{
	int dlen;
	char tstr[30];

	dlen = 14;									/* Set the display length.		*/

	if (ssave == NULL)
	{
		ssave = VL_vsss(MAX_LINES_PER_SCREEN-2, VL_vscr_wid-dlen, 1, dlen);		/* Save what is on the screen.		*/
	}

	VL_vstate(VSTATE_SAVE);
	VL_vmode(message_vmode);								/* Select the reverse rendition.	*/
	VL_vcharset(VCS_DEFAULT);								/* Assume the default position.		*/
	VL_vmove(MAX_LINES_PER_SCREEN-2, VL_vscr_wid - dlen);					/* Move to the status location.		*/
	sprintf(tstr,"(%d,%d) %d",frow+1,fcol+1,dwidth);				/* Display as 1 based.			*/
	VL_vprint("%-14.14s",tstr);
	VL_vstate(VSTATE_RESTORE);
}

/*
**	History:
**	$Log: vdisplay.c,v $
**	Revision 1.66  2003/06/20 15:37:45  gsl
**	VL_ globals
**	
**	Revision 1.65  2003/04/07 15:56:57  gsl
**	Unneed ftell() declaration
**	
**	Revision 1.64  2003/03/12 18:18:10  gsl
**	FIx -Wall warnings
**	
**	Revision 1.63  2003/02/20 23:14:34  gsl
**	Add OPTIONS get to DISPLAY utility that gets the record size RECSIZE
**	
**	Revision 1.62  2003/02/04 18:43:32  gsl
**	fix -Wall warnings
**	
**	Revision 1.61  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.60  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.59  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.58  2002/12/10 20:54:10  gsl
**	use WERRCODE()
**	
**	Revision 1.57  2002/12/10 17:09:15  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.56  2002/12/09 21:09:33  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.55  2002/07/17 17:26:01  gsl
**	fix vbuffering_end problem
**	
**	Revision 1.54  2002/07/16 14:11:47  gsl
**	VL_ globals
**	
**	Revision 1.53  2002/07/16 13:40:19  gsl
**	VL_ globals
**	
**	Revision 1.52  2002/07/15 20:16:01  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.51  2002/07/15 17:52:50  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.50  2002/07/10 21:05:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.49  2002/07/10 04:27:35  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.48  2002/07/09 04:13:57  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.47  2002/07/02 21:15:30  gsl
**	Rename wstrdup
**	
**	Revision 1.46  2002/06/25 15:21:54  gsl
**	Change to use wmalloc()
**	
**	Revision 1.45  2001/10/15 13:55:17  gsl
**	change vwang_set_videocap() to vwang_init_video()
**	
**	Revision 1.44  2001-09-25 12:19:25-04  gsl
**	Remove unneeded ifdefs
**
**	Revision 1.43  1998-10-15 11:11:10-04  gsl
**	The eof_trailer() was signal 11'ing when the filename was longer then the
**	current record size.  It has been fixed to use the screen size instead of
**	the record size and it now has a private buffer which is long.
**
**	Revision 1.42  1998-07-09 16:40:20-04  gsl
**	fix adjust_buffers() forward scroll overflow, was doing 0-23 instead of 0-22.
**
**	Revision 1.41  1998-06-19 11:01:48-04  gsl
**	Added a check to see if the file exists and can be read before
**	testing if empty.
**
**	Revision 1.40  1998-05-21 13:26:40-04  gsl
**	CHange to load costar VL_vmode's dynamically
**
**	Revision 1.39  1998-04-22 15:51:37-04  gsl
**	Fix warnings
**
**	Revision 1.38  1998-01-07 13:31:56-05  gsl
**	Fix non-ascii support so if NATIVECHARMAP is not set then it
**	will assume Wang character set encoding
**
**	Revision 1.37  1997-12-19 10:34:56-05  gsl
**	Change to use vwang_terminal_control_char() routines
**
**	Revision 1.36  1997-12-18 21:00:06-05  gsl
**	Replace the DISPLAYCHARMAP with NATIVECHARMAP processing.
**
**	Revision 1.35  1997-12-15 15:53:53-05  gsl
**	Add support for CHARMAP substitution with the DISPLAYCHARMAP option.
**
**	Revision 1.34  1997-10-31 14:37:19-05  gsl
**	Change "Empty" message to use WSB routines
**
**	Revision 1.33  1997-10-23 16:15:42-04  gsl
**	Changed the file_name to "const"
**
**	Revision 1.32  1997-10-17 10:47:54-04  gsl
**	Add support for pfkeys12
**
**	Revision 1.31  1997-07-16 21:22:00-04  gsl
**	Fix the eof_trailer() to center the filename as long names were getting
**	truncated off the right hand side
**
**	Revision 1.30  1997-07-14 08:26:32-04  gsl
**	Change to use new video.h interfaces
**
**	Revision 1.29  1997-01-11 18:21:37-05  gsl
**	Add a VL_verase(FULL_SCREEN) after the VL_vscreen() because it may not
**	clear the screen.
**
**	Revision 1.28  1996-11-18 15:37:11-08  gsl
**	Force scrolling off for WIN32 and MSDOS
**
**	Revision 1.27  1996-11-13 17:21:01-08  gsl
**	changed to use VL_vcontrol_flush()
**
**	Revision 1.26  1996-11-04 15:53:47-08  gsl
**	Add a call to vwang_set_videocap()
**
**	Revision 1.25  1996-10-08 17:27:11-07  gsl
**	replaced getenv() with wispdisplay8bit()
**
**	Revision 1.24  1996-09-10 08:47:43-07  gsl
**	Fix uninitialized variable cnt_off
**
**	Revision 1.23  1996-07-18 16:32:16-07  gsl
**	Enclose costar code in ifdef COSTAR
**
**	Revision 1.22  1996-07-17 15:06:50-07  gsl
**	Fixed compiler warnings
**
**	Revision 1.21  1996-07-17 14:53:17-07  gsl
**	Fix prototypes
**
**	Revision 1.20  1996-07-17 09:22:35-07  gsl
**	fix compile warning
**
**	Revision 1.19  1996-07-15 10:10:43-07  gsl
**	Fixed for NT
**
**	Revision 1.18  1996-07-11 16:22:15-07  gsl
**	Fix to use FOPEN mode defines for NT
**
**	Revision 1.17  1995-11-06 13:50:58-08  gsl
**	Add support to DISPLAY to display 8bit "foreign" characters.
**	This is turned on with WISP_DISPLAY_8BIT=YES in the environment
**
 * Revision 1.16  1995/07/20  10:12:32  gsl
 * minor cosmetic fix for W4W
 *
 * Revision 1.15  1995/04/28  07:08:04  scass
 * Change MAX_BUFFER_SIZE from 255 to 265 to accomodate
 * record length pf 256.
 *
 * Revision 1.14  1995/04/25  09:54:08  gsl
 * drcs state V3_3_15
 *
 * Revision 1.13  1995/04/24  16:30:44  gsl
 * fix PFKEY tag
 *
 * Revision 1.12  1995/04/17  11:47:17  gsl
 * drcs state V3_3_14
 *
 * Revision 1.11  1995/04/06  08:04:43  gsl
 * change VL_vmode() calls to work with costar
 *
 * Revision 1.10  1995/04/05  13:38:32  gsl
 * change to use new video.h defines
 *
 * Revision 1.9  1995/04/05  13:14:44  gsl
 * add standard headers
 *
**
**
*/
