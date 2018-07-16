			/************************************************************************/
			/*									*/
			/*		WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	vdisplay.c
*/

/*					Include files and definitions.								*/

#include <stdio.h>
#include <ctype.h>

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif
#ifndef VMS	/* unix or MSDOS */
#include <malloc.h>
#include <string.h>
#include <memory.h>
#endif

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>
#include <v/vcap.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wperson.h"
#include "scnfacs.h"
#include "wanguid.h"

#ifdef INCLUDE_VDISPIDX
# include "vdispidx.h"
#endif

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
#define BUFFER_SIZE		255							/* Maximum buffer size.			*/
#define TAB_SPACE_COUNT		8							/* Number of spaces equal to tab char.	*/

#define WINDOW_TOP	9								/* Top line of window area.		*/
#define WINDOW_ROWS	10								/* Number of lines in window area.	*/

#ifdef unix
#define CHECK_INTERVAL	500								/* Number of lines between checking	*/
#else
	/*
	**	VMS & MSDOS are SLOW compared to most of the UNIX boxes. 
	*/
#define CHECK_INTERVAL	200								/* Number of lines between checking	*/
#endif

#ifdef MSDOS
char *wanguid3();
#endif

#ifdef VMS
static int first_char = TRUE;
#endif

extern char WISPFILEXT[39];
static int errmsg_active = FALSE;							/* Error message is on the screen.	*/
static int rvrs_scroll_avail = TRUE;							/* Set reverse scroll available.	*/
static int frwd_scroll_avail = TRUE;							/* Set forward scroll available.	*/
static int ff_flag = FALSE;								/* Flag for FF found.			*/
static int stream_active;								/* Flag for stream mode display.	*/
static int status_active;								/* Status message is on the screen.     */
static int frow, fcol;									/* Current row in file.			*/
static char *ssave;									/* Save the status unerneath.		*/

static char srch_ch_map[256];
static int srch_len;
static FILE *input_file;								/* File control pointers.		*/

static int get_line();
static int display_buffer();
static int eof_trailer();
static int show_help();
static int rep_limit();
static int clear_window();
static int work_message();
static int roll_up();
static int roll_down();
static int roll_left_right();
static int set_scroll_buffer();
static int stx();
static int in_file();
static int scr_refresh();
static int full_screen_refresh();
static int derror();
static int gets0();
static int mem_err();
static int check_empty();
static int display_stat();
static int eop_indicator();
static int str_insert();
static int add_tab_cols();
static int adjust_buffers();
static int get_pos();

vdisplay(file_name,record_size)								/* VIDEO file display subroutine.	*/
char *file_name;
int record_size;
{
#define		ROUTINE		66500
	unsigned char *lb, *lbuf[MAX_LINES_PER_SCREEN];					/* Pointer to the line input buffer.	*/
	int bufsize;									/* Size of the line buffer.		*/
	int eofile;									/* End of file flag.			*/
	int eorec;									/* End of record flag.			*/
	int op_save;									/* Save optimization level.		*/
	int win_active;									/* Flag to track overlay window.	*/
	int err_just_removed, win_just_removed;						/* Flags indicating just removed.	*/
	int viewing;									/* Flag that viewing is takeing place.	*/
	int restart_requested;								/* Flag that a restart is requested.	*/
	int new_size_requested;								/* Flag that a new size is requested.	*/
	int first_time;									/* First time help control flag.	*/
	int4 fpos_tab[MAX_LINES_PER_SCREEN];						/* Position of each line in the file.	*/
	int4 postab[POSITION_TABLE_SIZE];						/* Allocate the position table.		*/
	int tx;										/* Current table index.			*/
	int lc;										/* Count of lines in position table.	*/
	int text_defined;								/* Search text defined flag.		*/
	int search_abort, term_status;							/* Search has been aborted		*/
	unsigned char search_text[SEARCH_TEXT_SIZE+2];					/* Storage for the searc text.		*/
	char search_num[SEARCH_NUM_SIZE+2];						/* Storage for the number text.		*/
	int scrl_col_cnt;								/* Scroll column index count.		*/
	register int j,c;								/* Some working registers.		*/
	int i, retcd;									/* Return code from print routines.	*/
	int	check_time;
	int4	def_bgchange, def_bgcolor;

	int type;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	stream_active = FALSE;								/* Set default mode.			*/
	status_active = FALSE;
	ssave = NULL;

	if (check_empty(file_name))							/* Check to see if empty file.		*/
	{
		char *screen, function, lines, term[2], no_mod;
		char templine[80];
		int pos;

		if ((screen = malloc(1924+8)) == 0) mem_err();				/* Able to get memory?			*/
		wsc_init(screen,0,0);							/* Initialize the screen layout.	*/

		wput(screen,2,26,PLAIN_TEXT,"Active Subprogram is DISPLAY");
		wput(screen,3,21,UNDER_TEXT,"                                      ");
		wput(screen,6,37,BLINK_TEXT,"SORRY");					/* Layout the screen.			*/
		memset(templine,0,sizeof(templine));
		sprintf(templine,"File:  %s",file_name);
		wput(screen,9,20,PLAIN_TEXT,templine);
		wput(screen,11,20,PLAIN_TEXT,"File specified contains zero records");
		wput(screen,14,25,PLAIN_TEXT,"Press PF16 to end DISPLAY");

		function = 3;								/* Use display and read.		*/
		lines = 24;
		vwang(&function,screen,&lines,"A",term,&no_mod);			/* Call Wang emulation to fill screen.	*/

		free(screen);
		return(SUCCESS);
	}
#ifdef INCLUDE_VDISPIDX
	type = vdisp_file_type(file_name);
	if (type == VD_ERR_OPEN || type == VD_ERR_READ)
	{
		werrlog(ERRORCODE(6),file_name,0,0,0,0,0,0,0);
		return(FAILURE);							/* It is fatal so return with error.	*/
	}
	if (type!=VD_TYPE_TEXT)
	{
		vdispidx(file_name,type);
		return 0;
	}
#endif
	if ((record_size < 0) || (record_size > 512))					/* Is record size valid.		*/
	{										/* Oops, invalid record size.		*/
		werrlog(ERRORCODE(2),record_size,0,0,0,0,0,0,0);
		vexit();								/* Unconditional exit.			*/
		wexit(ERRORCODE(2));
	}
#ifdef VMS
	if (record_size != 0 && record_size > BUFFER_SIZE) record_size = BUFFER_SIZE;	/* If an executable then use maximum.	*/
#endif
	if (record_size > BUFFER_SIZE)							/* Is record size greater than buffer.	*/
	{										/* Invalid buffer size.			*/
		werrlog(ERRORCODE(4),BUFFER_SIZE,record_size,0,0,0,0,0,0,0);
		vexit();								/* Unconditional exit.			*/
		wexit(ERRORCODE(4));
	}

	vcapload();

	if ( vcapdef[LINES] && *((int *)vcapdef[LINES]) == 25 )				/* is there a 25th line			*/
	{
		frwd_scroll_avail = FALSE;						/* Can't use forward scroll		*/
	}
	if (vcapnull(rvrsidx_esc, "SCROLL_REVERSE",0)) 
	{
		rvrs_scroll_avail = FALSE;						/* There is no reverse scroll		*/
	}

	wpload();

	first_time = TRUE;								/* First time only true the 1st time.	*/
	op_save = voptimize(DATA_CONTROLS_AND_MOTION);					/* Select deferred optimization.	*/

newsize:
	text_defined = FALSE;

#ifdef VMS
	if ((record_size > 0) && (record_size <= 80)) vscreen(NARROW);			/* Set the narrow parameters.		*/
	else vscreen(WIDE);								/* Set the wide parameters.		*/
#else
	vscreen(NARROW);								/* Select a narrow screen unless VMS.	*/
#endif

	/*
	**	NOTE:	On UNIX and MSDOS the record_size will ALWAYS come in as 0 because
	**		there is no "record size".  This is to indicate a "stream" file
	**		that uses NewLines to determine record size.  In this case we always
	**		set the buffer size to be the maximum.
	*/

	if (record_size) bufsize = record_size;						/* Determine the buffer size.		*/
	else bufsize = BUFFER_SIZE;							/* Else, variable length.		*/

	get_defs(DEFAULTS_BGCHANGE,(char*)&def_bgchange);
	if (def_bgchange)								/* Set the screen background if		*/
	{										/* not done already.			*/
		get_defs(DEFAULTS_BGCOLOR,(char*)&def_bgcolor);
		if (def_bgcolor) vscreen(LIGHT);
		else vscreen(DARK);
	}

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Allocate a screens worth of buffers. */
	{
		if ((lbuf[i] =(unsigned char*)malloc(bufsize+1+8)) == 0) mem_err();	/* Allocate a line buffer.		*/
	}

#ifdef MSDOS
	if ((input_file = fopen(file_name,"rb")) == NULL)				/* Did the file open?			*/
#else /* !MSDOS */
	if ((input_file = fopen(file_name,"r")) == NULL)				/* Did the file open?			*/
#endif /* !MSDOS */
	{
		werrlog(ERRORCODE(6),file_name,0,0,0,0,0,0,0);
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
	}

	for (i = 0; i < POSITION_TABLE_SIZE; i++) postab[i] = 0;			/* Set it all to zero.			*/
	tx = 0;										/* Set the table index to the start.	*/
	lc = 0;										/* There are no lines in the table.	*/
	scrl_col_cnt = 0;								/* Have not scrolled left or right yet. */

	vbuffering(LOGICAL);								/* Logical buffering on.		*/
	vmode(CLEAR);									/* Clear all renditions.		*/
	vcharset(DEFAULT);								/* Default character set.		*/
	vmove(0,0);									/* Home the cursor.			*/
	vset(CURSOR,ON);								/* Turn it on.				*/

	eofile = FALSE;									/* Not end of file yet.			*/
	eorec = FALSE;									/* Assume not end of record yet.	*/
	if (record_size > 0)								/* If record size specified.		*/
	{
		if (record_size <= vscr_wid) eorec = TRUE;				/* Record_size less then full screen.	*/
	}
											/* Test first char to see what file type.*/
	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Read in the first screen's worth.	*/
	{
		eofile = get_line(lbuf[i],&fpos_tab[i],bufsize,input_file);		/* Get the string.			*/
		if (eofile)
		{
			if (*lbuf[i] != '\0')
			{
				display_buffer(lbuf[i]);				/* Display the line.			*/
				if (i != MAX_LINES_PER_SCREEN-1) vprint("\n");		/* Move to the next line if appro.	*/
			}
			break;
		}

		display_buffer(lbuf[i]);						/* Display the line.			*/
		if (i != MAX_LINES_PER_SCREEN-1) vprint("\n");				/* Move to the next line if appro.	*/
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
			eof_trailer(lb,file_name,bufsize);				/* Write out the eof banner		*/
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
	win_just_removed = FALSE;							/* Window has not just been removed.	*/
	err_just_removed = FALSE;							/* Error message not just removed.	*/

	vmove(0,0);									/* Move to the start of the text.	*/
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

		c = vgetm();								/* Get a character.			*/

		if (win_active)								/* Is anything over written?		*/
		{
			clear_window(lbuf,scrl_col_cnt,bufsize);			/* Clear away the area.			*/
			win_active = FALSE;						/* Help is no longer active.		*/
			win_just_removed = TRUE;					/* Window has just been removed.	*/
		}

		if (errmsg_active)							/* Is there an error message visible?	*/
		{
			j = MAX_LINES_PER_SCREEN-1;					/* Determine the text location.		*/
			vstate(SAVE);							/* Save what is going on.		*/
			vmode(CLEAR);							/* Clear the current mode.		*/
			vcharset(DEFAULT);						/* Default character set.		*/
			vmove(MAX_LINES_PER_SCREEN-1, 0);				/* Move to error area.			*/
			lb = lbuf[j];							/* Get pointer to buffer.		*/
			set_scroll_buffer(scrl_col_cnt,lb,bufsize);			/* Set ptr and display buffer.		*/
			vstate(RESTORE);						/* Restore where we were.		*/
			errmsg_active = FALSE;						/* Now no error on the screen.		*/
			err_just_removed = TRUE;					/* Error message just removed.		*/
		}

		if (status_active)							/* Is there a status visible?		*/
		{
			if (ssave != NULL)
			{
				vrss(ssave);
				ssave = NULL;
			}
		}

		if ((c == fn1_key) || (c == home_key))					/* Toggle home/reverse home.		*/
		{
			if (vcur_col == 0 && vcur_lin == 0)				/* If cursor in home position go oppsite*/
			{
				frow = frow + MAX_LINES_PER_SCREEN - 1;
				fcol = fcol + vscr_wid - 1;
				vmove(MAX_LINES_PER_SCREEN-1,vscr_wid-1);		/* Else go to reverse home.		*/
			}
			else
			{
				frow = frow - vcur_lin;
				fcol = fcol - vcur_col;
				vmove(0,0);						/* Home if not home.			*/
			}
		}

		else if (c == fn16_key) viewing = FALSE;				/* Does he want to exit?		*/

		else if (c == down_arrow_key)						/* Down arrow?				*/
		{
			if (vcur_lin != MAX_LINES_PER_SCREEN-1)
			{
				vmove(vcur_lin+1,vcur_col);				/* Position cursor?		*/
				frow++;
			}
			else								/* No, so scroll a line.		*/
			{
				eofile = roll_up(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,1,scrl_col_cnt);
				if (eofile) derror("End of file encountered, no action performed.");
				else if (!frwd_scroll_avail)				/* In no forward scroll then need to	*/
				{							/* display manually.			*/
					full_screen_refresh(lbuf,MAX_LINES_PER_SCREEN-1,vcur_col);
					frow++;
				}
				else frow++;
			}
		}

		else if (c == up_arrow_key)						/* Up arrow?				*/
		{
			if (vcur_lin != 0)
			{
				vmove(vcur_lin-1,vcur_col);				/* Move cursor?				*/
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
				roll_down(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,&eofile,scrl_col_cnt);
				if (!rvrs_scroll_avail)					/* In no reverse scroll then need to	*/
				{							/* display manually.			*/
					full_screen_refresh(lbuf,0,vcur_col);		/* Move cursor to top of screen.	*/
				}
				frow--;
			}
		}

		else if (c == left_arrow_key)						/* Left arrow?				*/
		{
			if (vcur_col != 0)
			{
				vmove(vcur_lin,vcur_col-1);				/* Move cursor?				*/
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
			if (vcur_col < vscr_wid-1)
			{
				vmove(vcur_lin,vcur_col+1);				/* Move cursor?				*/
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
			if (fpos_tab[0] == 0 && vcur_lin == 0) derror("Already at start of file, no action performed.");
			else if (fpos_tab[0] == 0)
			{
				vmove(0,vcur_col);					/* Move cursor to top of screen.	*/
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
						roll_down(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,&eofile,scrl_col_cnt);
						frow--;
					}
				}

				if (!rvrs_scroll_avail)					/* In no reverse scroll then need to	*/
				{							/* display manually.			*/
					full_screen_refresh(lbuf,0,vcur_col);		/* Move cursor to top of screen.	*/
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
				if (vcur_lin == eofl) derror("At end of file, no action performed.");
				else
				{
					int cl;

					cl = vcur_lin;					/* Save prior line position		*/
					vmove(eofl,vcur_col);				/* Move cursor to end of file.		*/
					frow = frow + (vcur_lin - cl);
				}
			}
			else 
			{
				for (i = 0; (i < PAGE_SCROLL_LINES) && !eofile; i++)	/* Scroll that many times.		*/
				{
					eofile = roll_up(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,1,scrl_col_cnt);
					if (!eofile)
					{
						frow++;
					}
				}

				if (!frwd_scroll_avail)
				{
					full_screen_refresh(lbuf,MAX_LINES_PER_SCREEN-1,vcur_col); /* Move cursor to bottom	*/
				}
			}
		}

#ifndef MSDOS
		else if (c == fn6_key)							/* Change width of screen.		*/
		{
			int cl;

			cl = vcur_lin;
			vmove(0,0);							/* Home before changing.		*/
			vdefer(RESTORE);						/* Restore from deferred actions.	*/
			if (vscr_wid == 132) vscreen(NARROW);				/* Set narrow?				*/
			else vscreen(WIDE);						/* No, then set wide.			*/
#ifdef unix
			sleep(1);							/* Delay to allow screen width change	*/
#endif
			scr_refresh(lbuf,fpos_tab,MAX_LINES_PER_SCREEN,bufsize,scrl_col_cnt); /* Refresh the screen.	*/
			vmove(0,0);							/* Home for consistency.		*/
			frow = frow - cl;
			fcol = 0;
		}
#endif

		else if ((c == fn7_key) || (c == fn8_key))				/* Search for text in a file?		*/
		{
			search_abort = FALSE;

			if (c == fn7_key)						/* Define search text?			*/
			{
				vstate(SAVE);						/* Save where we are in the text.	*/
				vmove(0,0);						/* Home.				*/
				vmode(REVERSE);						/* Use the reverse rendition.		*/
				vprint(" Search for:                                                                 ");
				verase(TO_EOL);						/* Erase to the end of the line.	*/
				vmove(0,13);						/* Move to the start of input area.	*/
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
					vmode(CLEAR);					/* Clear the current mode.		*/
					vcharset(DEFAULT);				/* Default character set.		*/
					vmove(MAX_LINES_PER_SCREEN-1, 0);		/* Move to error area.			*/
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
						if (!isalpha(*lb))
						{
							srch_ch_map[*lb]=TRUE;
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
				vmove(0,0);						/* Return to the start of the line.	*/
				vmode(CLEAR);						/* Reset the clear rendition.		*/
				lb = lbuf[0];						/* Get pointer to buffer.		*/
				set_scroll_buffer(scrl_col_cnt,lb,bufsize);		/* Set ptr and display buffer.		*/
				vstate(RESTORE);					/* Restore where we were.		*/
				vcontrol(DUMP_OUTPUT);					/* Show everything we've done.		*/
			}

			if (search_abort) { /* just fall through */ }
			else if (!text_defined) derror("A search string has not been defined, use FIND function to define one.");
			else if (on_screen(search_text,lbuf,fpos_tab,postab,bufsize,	/* Is it on the current screen?		*/
						input_file,&tx,&lc,&eofile,&eorec,&scrl_col_cnt));
			else if (eofile) derror("Already at end of file, no action performed.");
			else if (term_status = in_file(search_text,lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,
						&eofile,&eorec,&scrl_col_cnt))
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
				vset(CURSOR,OFF);					/* Don't need the cursor for a while.	*/
				while (!eofile)						/* Loop to find the end of the file.	*/
				{
					eofile = roll_up(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,0,scrl_col_cnt);
					frow++;

					if (check_time++ > CHECK_INTERVAL)		/* Time to update the message?		*/
					{
						check_time = 0;				/* Reset the counter.			*/
						on = work_message(on);			/* Output flashing working message.	*/

						if (vcheck() != 0)
						{
							aborted = TRUE;
							break;				/* Break out of working loop.		*/
						}
					}
				}

				ff_flag = FALSE; /* Temp fix to misc highlighting bug */

				vstate(SAVE);						/* Save what we're doing.		*/
				scr_refresh(lbuf,fpos_tab,MAX_LINES_PER_SCREEN,bufsize,scrl_col_cnt); /* Refresh screen.	*/
				vstate(RESTORE);					/* Restore what we were doing.		*/
				vset(CURSOR,ON);					/* Now restore the flashing cursor.	*/

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
				if (vcur_col > 0 )
				{
					vmove(vcur_lin,0);				/* Move cursor to beginning of line.	*/
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
				if (scrl_col_cnt > (bufsize - vscr_wid)) scrl_col_cnt = bufsize - vscr_wid;
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
				fcol = fcol + (scrl_col_cnt - hscrl_cnt);
			}
		}

		else if (c == fn11_key)							/* Change current buffer size?		*/
		{									/*  (record length)			*/
			int tsize, cl;

			cl = vcur_lin;
			text_defined = FALSE;
			vstate(SAVE);							/* Save where we are in the text.	*/
			vmove(0,0);							/* Home.				*/
			vmode(REVERSE);							/* Use the reverse rendition.		*/
			vprint(" Enter record length:                                                                 ");
			verase(TO_EOL);							/* Erase to the end of the line.	*/
			vmove(0,23);							/* Move to the start of input area.	*/
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
				tsize = atoi(search_num);
				if (tsize > BUFFER_SIZE || tsize < 0)			/* Zero is valid; means variable	*/
				{
					werrlog(ERRORCODE(14),search_num,0,0,0,0,0,0,0,0);
				}
				else if (strlen(search_num) > SEARCH_NUM_SIZE)
				{
					werrlog(ERRORCODE(14),search_num,0,0,0,0,0,0,0,0);
				}
				else if (isalpha(search_num[0]) || isalpha(search_num[1]) ||
					 isalpha(search_num[2]) || isalpha(search_num[3]) )
				{
					werrlog(ERRORCODE(14),search_num,0,0,0,0,0,0,0,0);
				}
				else
				{
					verase(FULL_SCREEN);				/* Make sure is really erased.		*/
					record_size = tsize;				/* Set the new record size.		*/
					viewing = FALSE;				/* Stop viewing.			*/
					new_size_requested = TRUE;			/* Asked for a new record size.		*/
				}
			}

			vmove(0,0);							/* Return to the start of the line.	*/
			vmode(CLEAR);							/* Reset the clear rendition.		*/
			frow = frow - cl;				
			fcol = 0;
			lb = lbuf[0];							/* Get pointer to buffer.		*/
			set_scroll_buffer(scrl_col_cnt,lb,bufsize);			/* Set ptr and display buffer.		*/
			vstate(RESTORE);						/* Restore where we were.		*/
			vcontrol(DUMP_OUTPUT);						/* Show everything we've done.		*/
		}

		else if (c == fn12_key)							/* Toggle the display of the status line*/
		{
			if (status_active)
			{
				status_active = FALSE;					/* If on then turn off.			*/
				if (ssave != NULL)
				{
					vrss(ssave);						
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
		}

		else if (c == fn14_key)							/* Print current screen?		*/
		{
			unsigned char *text, *ttxt;
			char filelibvol[23];
			int size;
			char errstr[80], temp[5];
			int	linesize;

			linesize = (vscr_wid < bufsize) ? vscr_wid : bufsize;

			size = ((linesize+1+8) * MAX_LINES_PER_SCREEN);			/* Compute the size.			*/
			if ((text = (unsigned char *)malloc(size)) == 0) mem_err();	/* Allocate a screen buffer.		*/

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
			memcpy(&filelibvol[2],(char *)wanguid3(),3);
			memset(&filelibvol[5],' ',22-5); /* fix bug */
			retcd = 0;
			retcd = di_write_file((char*)text,size,linesize,filelibvol,filelibvol);
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

			get_defs(DEFAULTS_PM,(char*)&def_prt_mode);
			get_defs(DEFAULTS_PC,(char*)&def_prt_class);
			get_defs(DEFAULTS_FN,(char*)&def_prt_form);

			retcd = 0;
			wprint(file_name,def_prt_mode,"DS",1,def_prt_class,def_prt_form,&retcd);
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

		else if ((c == help_key) && !win_just_removed)				/* Does he/she want help?		*/
		{
			show_help(file_name);						/* Yes, so show it.			*/
			win_active = TRUE;						/* Now help is active.			*/
		}

		else if (!(win_just_removed || err_just_removed)) derror("Invalid key.");

		win_just_removed = FALSE;						/* Now window is gone.			*/
		err_just_removed = FALSE;						/* Now messages are gone.		*/
	}

	if (ssave != NULL)
	{
		vrss(ssave);
		ssave = NULL;
	}
	vbuffering(AUTOMATIC);								/* Dump all buffers.			*/
	if (restart_requested) goto again;						/* Start again if requested.		*/

#ifdef NOT_YET
	freeblocks();
#endif
	fclose(input_file);								/* Close the file we were viewing.	*/

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++) free(lbuf[i]);			/* Loop through all screen lines.	*/
	vmove(0,0);									/* Home before changing.		*/
	vdefer(RESTORE);								/* Restore from deferred states.	*/
	if (new_size_requested) goto newsize;						/* Start again with new record size.	*/
	vscreen(NARROW);								/* Restore to a narrow screen.		*/
#ifdef unix
	sleep(1);									/* Delay to allow screen width change	*/
#endif
	voptimize(op_save);								/* Restore optimization.		*/
	vcontrol(DUMP_OUTPUT);								/* Dump the buffer.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

static int get_line(lb,fpos_ptr,bufsize,input_file)					/* Compose an input line.		*/
register unsigned char *lb;
int4 *fpos_ptr;
int bufsize;
FILE *input_file;
{
	unsigned char *l_lb;
	int4 get_pos();								       /* Get file postion routine.	       */
	int	inpos, outpos, tcnt, num, err;
	int	eoline, eofile;								/* End of line and file flags.		*/
	register int c;

	eofile = FALSE;									/* Assume not end of file.		*/
	eoline = FALSE;									/* Assume not end of line.		*/

	l_lb = lb;									/* Set ptr to beginning of line.	*/
	outpos = 0;									/* Set the output position in buffer.	*/
	*fpos_ptr = get_pos(input_file);						/* Get the position in the file.	*/
#ifdef VMS
	first_char = TRUE;
#endif

	for (inpos = 0; outpos < bufsize; inpos++)					 /* Loop until full.			 */
	{
		if ((c = fgetc(input_file)) == EOF) 
		{
			eofile = TRUE;							/* Get the next character.		*/
			break;								/* Exit loop				*/
		}
		else									/* There is data left to read.		*/
		{
#ifdef VMS
			if (first_char && c == CHAR_CR)					/* Fix for BEFORE ADVANCING		*/ 
			{								/*  (VFC record format)			*/
				c = fgetc(input_file);					/* Read the next char.			*/
				first_char = FALSE;
			}
#endif
			if (c >= ' ' && c <= '~')					/* A displayable character?		*/
			{
				*(lb++) = c;
				outpos++;						/* Increment out buffer position.	*/
			}
			else if (stream_active)
			{
				*(lb++) = '.';						/* Is a non-displayable character.	*/
				outpos++;						/* Increment out buffer position.	*/
			}
			else if (c == LNFD || c == FF || c == CHAR_CR)
			{
				eoline = TRUE;						/* Is newline character or page break?	*/
				if (c == CHAR_CR)
				{
					c = fgetc(input_file);				/* Read the next char.			*/
					if (c != LNFD) ungetc(c,input_file);		/* Put char back in file.		*/
				}
				else if (c == FF)
				{
					if (inpos > 0)					/* If have stuff in buffer.		*/
					{
						ungetc(c,input_file);			/* Put char back in file.		*/
					}
					else
					{
						lb = l_lb;				/* Set to beginning of current buffer.	*/
						eop_indicator(lb,bufsize);		/* Init form feed indicator.		*/
						ff_flag = TRUE;
						c = fgetc(input_file);			/* Read the next char.			*/
						if (c != LNFD) ungetc(c,input_file);	/* Put char back in file.		*/
						return(eofile);				/* Return the end of file status.	*/
					}
				}
				break;							/* Exit the loop			*/
			}
			else if (c == '\t')						/* Is it a tab character?		*/
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
			else if (c == CHAR_DEL);					/* Ignore delete character.		*/
			else if (c == '\b')
			{
				*(lb++) = c;						/* Insert back space characters.	*/
				outpos++;						/* Increment out buffer position.	*/
			}
			else if (c == CHAR_BELL)
			{
				*(lb++) = c;						/* Insert bell characters.		*/
				outpos++;						/* Increment out buffer position.	*/
			}
			else
			{
				*(lb++) = '.';						/* else is a non-displayable character. */
				outpos++;						/* Increment out buffer position.	*/
			}
		}
	}

	memset(lb,' ',bufsize - outpos);
	l_lb[bufsize] = CHAR_NULL;

	*lb = CHAR_NULL;								/* Default a null at the end of line.	*/
	return(eofile);									/* Return the end of file status.	*/
}

static display_buffer(lb)								/* Display correct size of buffer.	*/
unsigned char *lb;									/* Stuff NULL into correct pos.		*/
{
	unsigned char *clb;
	char csave;									/* Variable to hold saved character.	*/
	int pos, np, erasefl, md_save;

	clb = lb;									/* Set to start of buffer.		*/
	erasefl = FALSE;
	np = 0;
	for (pos = 0; pos < vscr_wid; pos++)						/* Do until at correct position for	*/
	{										/* the current screen size.		*/
		if (*clb++ == CHAR_NULL)
		{
			erasefl = TRUE;							/* Make sure erases to end of line.	*/
			pos = vscr_wid;							/* Set so gets out of loop.		*/
		}
		else np++;
	}
	csave = lb[np];									/* Save the char at last pos on screen. */ 
	lb[np] = CHAR_NULL;								/* Stuff a null so displays correctly.	*/
	md_save = vcur_atr;								/* Save the current mode.		*/
	if (ff_flag)
	{
		vmode(BOLD);
		ff_flag = FALSE;
	}
	vprint("%s",lb);								/* Display the line.			*/
	if ((vcur_col < vscr_wid-1) || erasefl ) verase(TO_EOL);			/* Erase to the end of the line.	*/
	lb[np] = csave;									/* Put character back into the buffer.	*/
	vmode(md_save);									/* Put back to what it was.		*/
}

static eof_trailer(lb,file_name,bufsize)						/* Write the start of file header.	*/
unsigned char *lb, *file_name;
int bufsize;
{
	memset(lb,'-',bufsize);								/* Store spaces across the line.	*/
	str_insert(&lb[20],"This is the end of ");					/* Insert header message.		*/
	str_insert(&lb[39],file_name);							/* Insert the file name.		*/
	vmode(CLEAR);									/* Clear the rendition flag.		*/
	display_buffer(lb);								/* Display the line.			*/
}

static eop_indicator(lb,bufsize)							/* Write the page break indicator.	*/
unsigned char *lb;
int bufsize;
{
	memset(lb,'-',bufsize);								/* Copy dashes to buffer.		*/
	lb[bufsize-1] = '\0';								/* Null terminate the string.		*/
}

static str_insert(target,source)							/* Insert a string in another string.	*/
unsigned char *target, *source;
{
	while(*source != CHAR_NULL) *(target++) = *(source++);				/* Loop until end of string null.	*/
}

static show_help(file_name)								/* Show the help screen.		*/
char *file_name;
{
	int lhs;									/* Define location of the help window.	*/
	int columns;									/* Define size of the help window.	*/
	unsigned char string[65];							/* Working string.			*/
	char l_filen[256], *lptr;							/* Local copy of file_name.		*/
	register int i;									/* Working counter.			*/
	int len;

	strcpy(l_filen,file_name);							/* Copy file_name to local copy.	*/

	columns = 62;									/* The help area is 62 columns wide.	*/
	lhs = (vscr_wid - columns)/2;							/* Center the help screen.		*/

	vstate(-1);									/* Save the current screen state.	*/
	vmove(WINDOW_TOP-1,lhs-1);							/* Draw the outline box.		*/
	vline(VERTICAL,WINDOW_ROWS+2);
	vmove(WINDOW_TOP-1,lhs-1+columns);
	vline(VERTICAL,WINDOW_ROWS+2);
	vmove(WINDOW_TOP-1,lhs-1);
	vline(HORIZONTAL,columns+1);
	vmove(WINDOW_TOP+WINDOW_ROWS,lhs-1);
	vline(HORIZONTAL,columns+1);

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
	vmode(REVERSE|UNDERSCORE);							/* Do it in reverse underscore,		*/
	vmove(i++,lhs);
	vprint("%s",string);								/* Print the file name.			*/

	vmode(CLEAR);									/* Clear all renditions.		*/
	vmode(REVERSE);									/* Do it in reverse.			*/
	vmove(i++,lhs);
	vprint("  Use the arrow keys to scroll up, down, left and right.     ");
	vmove(i++,lhs);
	vprint("   (1) - Home cursor   (2) - File top       (3) - Bottom     ");
	vmove(i++,lhs);
#ifdef MSDOS
	vprint("   (4) - Prev screen   (5) - Next screen                     ");
#else
	if (vscr_wid == 132)	vprint("   (4) - Prev screen   (5) - Next screen    (6) - 80 col     ");
	else			vprint("   (4) - Prev screen   (5) - Next screen    (6) - 132 col    ");
#endif
	vmove(i++,lhs);
	vprint("   (7) - Find string   (8) - Find next                       ");
	vmove(i++,lhs);
	vprint("   (9) - Scroll left  (10) - Scroll right                    ");
	vmove(i++,lhs);
	if (stream_active)
	{
		vprint("  (11) - Record size  (12) - Toggle status (13) - Record Mode");
	}
	else
	{
		vprint("  (11) - Change width (12) - Toggle status (13) - Stream Mode");
	}
	vmove(i++,lhs);
	vprint("  (14) - Print screen (15) - Print file    (16) - Exit       ");
	vmove(i++,lhs);
	vprint("  Depress any key to remove this window.                     ");
	vmove(i++,lhs);
	vprint("  Use the HELP key to bring this window back.                ");
	vstate(1);									/* Reset rendition to normal.		*/
}

static rep_limit()									/* Report the limit of upward scroll.	*/
{
	int columns;									/* Define size of the help window.	*/
	int lhs;									/* Define location of the help window.	*/
	register int i;									/* Working counter.			*/

	columns = 60;									/* Width of the report area.		*/
	vbell();									/* Let him/her have an audio alert.	*/
	while (vcheck() != 0);								/* Flush all type ahead.		*/

	lhs = (vscr_wid - columns)/2;							/* Center the help screen.		*/

	vstate(-1);									/* Save the current screen state.	*/
	vmove(WINDOW_TOP-1,lhs-1);							/* Draw the outline box.		*/
	vline(VERTICAL,WINDOW_ROWS+2);
	vmove(WINDOW_TOP-1,lhs-1+columns);
	vline(VERTICAL,WINDOW_ROWS+2);
	vmove(WINDOW_TOP-1,lhs-1);
	vline(HORIZONTAL,columns+1);
	vmove(WINDOW_TOP+WINDOW_ROWS,lhs-1);
	vline(HORIZONTAL,columns+1);

	i = WINDOW_TOP;									/* Point the help area.			*/
	vmode(REVERSE|UNDERSCORE);							/* Do it in reverse underscore,		*/
	vmove(i++,lhs);
	vprint("                Reverse Scroll Limit Reached               ");		/* Print the message header.		*/

	vmode(CLEAR);									/* Clear all renditions.		*/
	vmode(REVERSE);									/* Do it in reverse.			*/
	vmove(i++,lhs);
	vprint("  It is not possible to scroll backwards beyond this point ");
	vmove(i++,lhs);
	vprint("  because the reverse movement limit has been reached.  In ");
	vmove(i++,lhs);
	vprint("  order to view text in front of this screen, use option 2 ");
	vmove(i++,lhs);
	vprint("  to go to the start of this file and then scroll forward. ");
	vmove(i++,lhs);
	vprint("                                                           ");
	vmove(i++,lhs);
	vprint("                                                           ");
	vmove(i++,lhs);
	vprint("                                                           ");
	vmove(i++,lhs);
	vprint("  Depress any key to remove this message...                ");
	vstate(1);									/* Reset rendition to normal.		*/
}

static clear_window(lbuf,roll_cnt,bufsize)						/* Subroutine to clear the help area.	*/
unsigned char *lbuf[];									/* Pointer to the line input buffers.	*/
int roll_cnt, bufsize;
{
	register int i,j;								/* Working registers.			*/
	unsigned char *lb;								/* Working pointer.			*/

	if ((j = WINDOW_TOP - 1) >= MAX_LINES_PER_SCREEN) j = j - MAX_LINES_PER_SCREEN; /* Get the first line.			*/

	vstate(-1);									/* Save all the event info.		*/
	vmode(CLEAR);									/* Clear all renditions.		*/
	vcharset(DEFAULT);								/* Clear the current character set.	*/

	for (i = WINDOW_TOP-1; i < WINDOW_TOP + WINDOW_ROWS + 1; i++)			/* Loop through the whole help area.	*/
	{
		vmove(i,0);								/* Move to the start of the line.	*/
		lb = lbuf[j];								/* Point to this line buffer.		*/
		set_scroll_buffer(roll_cnt,lb,bufsize);					/* Set ptr and display buffer.		*/
		j++;									/* Move to the next line.		*/
		if (j >= MAX_LINES_PER_SCREEN) j = 0;					/* Did we wrap around?			*/
	}
	vstate(1);									/* Restore where we were.		*/
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
static int work_message(on) int on;							/* Display the working message.		*/
{
	int lhs;									/* Left hand side of display.		*/
	register int i;									/* Working register.			*/

	vstate(-1);									/* Save wher we are.			*/

	lhs = (vscr_wid - 16)/2;							/* center the message			*/

	if (on == 0)									/* First time?				*/
	{
		vmove(WINDOW_TOP,lhs-2);						/* Draw the outline box.		*/
		vline(VERTICAL,3);
		vmove(WINDOW_TOP,lhs+13);
		vline(VERTICAL,3);
		vmove(WINDOW_TOP,lhs-2);
		vline(HORIZONTAL,16);
		vmove(WINDOW_TOP+2,lhs-2);
		vline(HORIZONTAL,16);
	}

	if (on <= 0)
	{
		vmove(WINDOW_TOP+1,lhs-1);						/* Move to the clear area.		*/
		vmode(CLEAR);								/* Clear it out.			*/
		vprint("              ");						/* Output spaces.			*/
		i = 1;									/* Turn on next time.			*/
	}

	if (on >= 0)									/* On or off?				*/
	{
		vmove(WINDOW_TOP+1,lhs);						/* Move to the info area.		*/
		vmode(REVERSE);								/* Select something noticeable.		*/
		vprint(" Working... ");							/* Display the working message.		*/
		i = -1;									/* Turn off the next time.		*/
	}

	vstate(1);									/* Restore where we were.		*/
	vcontrol(DUMP_OUTPUT);								/* Show what is going on.		*/
	return(i);									/* Return the new state.		*/
}

static int roll_up(lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,send_out,roll_cnt)		/* Scroll up one line.			*/
unsigned char *lbuf[];
int4 fpos_tab[], *pt;
int bufsize, *tx, *lc, send_out, roll_cnt;
FILE *input_file;
{
	unsigned char *nlb;								/* Temporary storage.			*/
	int4 temp_position;
	int i, eofile;									/* End of file flag.			*/
	unsigned char temp_line[BUFFER_SIZE+8];
				     
	eofile = FALSE;									/* Assume not end of file.		*/

	eofile = get_line(temp_line,&temp_position,bufsize,input_file);			/* Get another line.			*/
	if (!eofile || (eofile && *temp_line != '\0'))					/* Are we at the end of file?		*/
	{
		if (send_out)								/* Do they want output?			*/
		{
			vstate(SAVE);							/* Save where we are.			*/
			if (frwd_scroll_avail)
			{
				vmove(MAX_LINES_PER_SCREEN-1,vcur_col);			/* Move to the bottom line.		*/
				vprint("\n");						/* Scroll forward.			*/
				set_scroll_buffer(roll_cnt,temp_line,bufsize);		/* Set ptr and display buffer.		*/
			}
			vstate(RESTORE);						/* Restore where we were.		*/
		}

		pt[*tx] = fpos_tab[0];							/* Remember position of line off top.	*/
		*tx = *tx + 1;								/* Increment the table index.		*/
		if (*tx >= POSITION_TABLE_SIZE) *tx = 0;				/* Handle if it wrapped around.		*/
		*lc = *lc + 1;								/* Increment the entry counter.		*/
		if (*lc > POSITION_TABLE_SIZE) *lc = POSITION_TABLE_SIZE;		/* Don't let it overflow.		*/

		adjust_buffers(lbuf,fpos_tab,1);					/* Adjust data buffers up one.		*/
		memcpy(lbuf[MAX_LINES_PER_SCREEN-1],temp_line,bufsize);

		fpos_tab[MAX_LINES_PER_SCREEN -1] = temp_position;			/* Remember the new position.		*/
	}
	return(eofile);									/* Return the end of file indicator.	*/
}

static roll_down(lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,eofile,roll_cnt)		/* Scroll down one line.		*/
unsigned char *lbuf[];
int4 fpos_tab[], *pt;
int bufsize, *tx, *lc, *eofile, roll_cnt;
FILE *input_file;
{
	unsigned char *nlb, *temp_line;							/* Temporary storage.			*/
	int4 temp_position, temp_fp;
	register int e, i;

	if (*lc != 0)									/* Can we still scroll backwards?	*/
	{
		vstate(SAVE);								/* Save where we were.			*/
		if (rvrs_scroll_avail)
		{
			vmove(0,0);							/* Move to the top of the screen.	*/
			vnewline(REVERSE);						/* Scroll backwards.			*/
		}
		*tx = *tx - 1;								/* Decrement the index.			*/
		if (*tx < 0) *tx = POSITION_TABLE_SIZE-1;				/* Wrap around as appropriate.		*/
		*lc = *lc - 1;								/* Decrement the backup line count.	*/
		temp_fp = fpos_tab[MAX_LINES_PER_SCREEN -1];				/* Remember where the file was.		*/
		e = fseek(input_file,pt[*tx],0);					/* Seek the previous record.		*/

		if (!e)									/* If was a successful seek.		*/
		{
			if ((temp_line = (unsigned char*)malloc(bufsize+1+8)) == 0) mem_err(); /* Allocate a line buffer	*/
			get_line(temp_line,&temp_position,bufsize,input_file);		/* Get another line.			*/
			if (rvrs_scroll_avail) set_scroll_buffer(roll_cnt,temp_line,bufsize); /* Set ptr and display buffer.	*/
			adjust_buffers(lbuf,fpos_tab,2);				/* Adjust data buffers down one.	*/
			nlb = lbuf[0];							/* Get address of first buffer.		*/
			for (i = 0; i < bufsize; i++) *nlb++ = temp_line[i];		/* Copy the string.			*/
			fpos_tab[0] = temp_position;					/* Remember the new position.		*/
			free(temp_line);
		}
		vstate(RESTORE);							/* Restore where we were.		*/

		e = fseek(input_file,temp_fp,0);					/* Reset to read bottom line again.	*/
		*eofile = FALSE;							/* Cannot be at end of file anymore.	*/
	}
}

static int roll_left_right(lbuf,fpos_tab,bufsize,input_file,eorec,roll_cnt)		/* Scroll left or right.		*/
unsigned char *lbuf[];
int4 fpos_tab[];
int bufsize, *eorec, roll_cnt;
FILE *input_file;
{
	unsigned char *lb;
	register int i, j, c;
	int sc,sl;									/* Variable to save current cursor pos. */
	int cont;									/* Continue flag.			*/

	sl = vcur_lin;									/* Save the current cursor position.	*/
	sc = vcur_col;

	*eorec = FALSE;									/* Assume not at end of record.		*/
	cont = TRUE;
	i = 0;										/* Set to current top of screen ptr.	*/
	c = 0;
	vbuffering(LOGICAL);
	while (cont)									/* Read in a new screen's worth.	*/
	{
		lb = lbuf[c];								/* Make a working copy.			*/
		vmove(c,0);								/* Move to beginning of the line.	*/
		set_scroll_buffer(roll_cnt,lb,bufsize);					/* Set ptr and display buffer.		*/
		i++;									/* Increment current line counter.	*/
		c++;									/* Increment # of lines scrolled.	*/
		if ((c >= MAX_LINES_PER_SCREEN) || (fpos_tab[i] == -1)) cont = FALSE;
	}
	vmove(sl,sc);									/* Put cursor in original position.	*/
	vbuffering(AUTOMATIC);
	if (roll_cnt >= bufsize - vscr_wid) *eorec = TRUE;				/* Set end of buffer/record.		*/
}

static int set_scroll_buffer(roll_cnt,lb,bufsize)					/* Set pointer to buffer to be at the	*/
unsigned char *lb;									/* current scroll column.		*/
int roll_cnt, bufsize;
{
	int i, remdr, cnt;

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
static int add_tab_cols(col)
int col;
{
	return(TAB_SPACE_COUNT - (col % TAB_SPACE_COUNT));
}

on_screen(search_text,lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,eofile,eorec,roll_cnt)	/* Is the text on the screen.		*/
unsigned char *search_text, *lbuf[];
int4 fpos_tab[], *pt;
int bufsize, *tx, *lc, *eofile, *eorec, *roll_cnt;
FILE *input_file;
{
	unsigned char *lb, temp[BUFFER_SIZE+8];						/* Temporary storage.			*/
	register int i, j;								/* Working registers.			*/
	int first_in;									/* Flag so will step past if first line.*/
	int k;										/* Tab count so display cursor correctly*/

	i = vcur_lin;									/* Set to the current line.		*/
	j = *roll_cnt + vcur_col;							/* Set to the current column in buffer. */
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
			if (clin >= *roll_cnt + vscr_wid)				/* Is to right of current screen.	*/
			{								/* Set up scroll column count.		*/
				while (clin >= *roll_cnt + vscr_wid) *roll_cnt = *roll_cnt + vscr_wid;
				if (*roll_cnt > bufsize - vscr_wid) *roll_cnt = bufsize - vscr_wid;
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,eorec,*roll_cnt);
				clin = clin - *roll_cnt;				/* Set cursor position on new screen.	*/
			}
			else if (clin < *roll_cnt)					/* Is to left of current screen.	*/
			{								/* Set up scroll column count.		*/
				while (clin < *roll_cnt) *roll_cnt = *roll_cnt - vscr_wid;
				if (*roll_cnt < 0) *roll_cnt = 0;			/* Can only scroll to beg. of line.	*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,eorec,*roll_cnt);
			}
			if (clin > vscr_wid) clin = clin - *roll_cnt;			/* Set so is on current screen.		*/

			vmove(i,clin);							/* Move to where the string is.		*/
			return(TRUE);							/* And we're all done.			*/
		}
		i++;									/* Move to the next line		*/
		j = 0;									/* Move to the left hand column.	*/
	}
	return(FALSE);									/* The string is not on the screen.	*/
}

static int stb(out,in,j,cnt)								/* Build a string from the buffers.	*/
unsigned char *out, *in;
int j, cnt;
{
	while (j < cnt)									/* Loop to end of line.			*/
	{
		*(out++) = toupper(*(in++));						/* Copy and upcase the character.	*/
		j++;									/* Count along eh.			*/
	}
	*(out++) = CHAR_NULL;								/* Store a trailing null.		*/
}

static int stx(string,mask)								/* Get string index in another string.	*/
register unsigned char *mask, *string;
{
	register m_idx, s_idx, cmpval;
	register int m_len, s_len;
	int save;

	m_len = srch_len - 1;
	s_len = strlen((char *)string);
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
			while (srch_ch_map[string[s_idx]] && (save-s_idx < m_len)) --s_idx;
			save = s_idx += m_len + 1;
			m_idx = m_len;
		}
	}
	return -1;
}

/*
**	Routine:	in_file
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
static int in_file(search_text,lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,eofile,eorec,roll_cnt) 
unsigned char *search_text, **lbuf;
int4 fpos_tab[], *pt;
int bufsize, *tx, *lc, *eofile, *eorec, *roll_cnt;
FILE *input_file;
{
	unsigned char lbuf_save[MAX_LINES_PER_SCREEN][BUFFER_SIZE+8];			/* Save the current screen.		*/
	int4 fp_save[MAX_LINES_PER_SCREEN];						/* Save the current pointers.		*/
	int4 pt_save[POSITION_TABLE_SIZE];						/* Save the position table.		*/
	int tx_save, lc_save;								/* Save the index pointers.		*/
	int lin_save,col_save;								/* Position save locations.		*/
	int4 pos_save;									/* Save current file pointer.		*/
	int found;									/* Flag to indicate text found.		*/
	int lines_read;									/* Lines read in search.		*/
	int4 get_pos();									/* Get position returns a int4.		*/
	unsigned char *lb, temp[BUFFER_SIZE+8];						/* Temporary working buffer.		*/
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
	lin_save = vcur_lin;								/* Remember the cursor position.	*/
	col_save = vcur_col;

	vset(CURSOR,OFF);								/* Don't need the cursor for a while.	*/
	on = 0;										/* Working counters.			*/
	check_time = 0;
	found = FALSE;									/* Haven't found the string yet.	*/
	lines_read = 0;									/* No lines read yet.			*/

	l_eof = *eofile;
	while (!l_eof && !found)							/* Loop to find the end of the file.	*/
	{
		l_eof = roll_up(lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,0,*roll_cnt); /* Read a line.				*/

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
						*eofile = roll_up(lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,0,*roll_cnt);
						if (*eofile) break;
						lines_read++;
					}
					lines_scrolled = i;
				}
				scr_refresh(lbuf,fpos_tab,lines_read,bufsize,*roll_cnt);	/* Yes, then write the string.	*/
				vmove(MAX_LINES_PER_SCREEN-(2+lines_scrolled),vscr_wid-1); /* Move to the end of previous line. */
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

					if (vcheck() != 0)				/* Check of a keypress interupt		*/
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
						vmove(lin_save,col_save);		/* Now move back to where we were.	*/
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
			vmove(lin_save,col_save);					/* Now move back to where we were.	*/
			*eofile = FALSE;						/* No longer end of file.		*/
		}
	}

	clear_window(lbuf,*roll_cnt,bufsize);						/* Clear away the area.			*/
	vset(CURSOR,ON);								/* Now restore the flashing cursor.	*/
	return(found);
}

static scr_refresh(lbuf,fpos_tab,lines,bufsize,roll_cnt)				/* Refresh screen from line buffers.	*/
unsigned char *lbuf[];
int4 fpos_tab[];
int lines, bufsize, roll_cnt;
{
	register int i, j, k;								/* Working registers.			*/
	unsigned char *lb;

	j = 0;										/* Point j to the top of the screen.	*/
	k = 0;										/* Assume start at top of screen.	*/

	if (lines < MAX_LINES_PER_SCREEN-1)						/* Write the whole screen?		*/
	{
		vmove(MAX_LINES_PER_SCREEN-1,0);					/* No so move to the bottom.		*/
		for (i = 0; i < lines; i++) vprint("\n");				/* Scroll up.				*/
		k = MAX_LINES_PER_SCREEN - lines;					/* Determine where to start.		*/
		j = k;
	}

	for (i = k; i < MAX_LINES_PER_SCREEN; i++)					/* Now display the file.		*/
	{
		vmove(i,0);								/* Move to the start of the line.	*/
		lb = lbuf[j];								/* Get pointer to current buffer.	*/
		set_scroll_buffer(roll_cnt,lb,bufsize);					/* Set pointer to current scroll column.*/
		j++;									/* Move to the next line.		*/
		if (j >= MAX_LINES_PER_SCREEN) j = 0;					/* Handle the wrap around.		*/
	}
	vmove(MAX_LINES_PER_SCREEN-1,vscr_wid-1);					/* Move to the standard cursor pos.	*/
}

static adjust_buffers(lbuf,fpos_tab,direction)						/* Adjust buffer address to reflect what*/
unsigned char *lbuf[];									/* is really on the screen.		*/
int4 fpos_tab[];
int direction;
{
	register int i, j;
	unsigned char *tbufa;

	if (direction == 1)								/* Move buffers forward one.		*/
	{
		tbufa = lbuf[0];							/* Get the address of first buffer.	*/
		for (i = 0; i < MAX_LINES_PER_SCREEN; i++)				/* Do for each buffer line.		*/
		{
			lbuf[i] = lbuf[i+1];						/* Assign the new address.		*/
			fpos_tab[i] = fpos_tab[i+1];					/* Assign the file position value.	*/
		}
		i--;									/* Set so is last buffer on screen.	*/
	}
	else if (direction == 2)							/* Move buffers backward one.		*/
	{
		tbufa = lbuf[MAX_LINES_PER_SCREEN-1];					/* Get the address of last buffer.	*/
		for (i = MAX_LINES_PER_SCREEN-1; i > 0; i--)				/* Do for each buffer line.		*/
		{
			lbuf[i] = lbuf[i-1];						/* Assign new address.			*/
			fpos_tab[i] = fpos_tab[i-1];					/* Assign the file position value.	*/
		}
	}
	lbuf[i] = tbufa;								/* last/first buffer pts to first/last. */
	fpos_tab[i] = 0;								/* Set so no position.			*/
}

static full_screen_refresh(lbuf,row,col)
unsigned char *lbuf[MAX_LINES_PER_SCREEN];
{
	int	i;
	verase(FULL_SCREEN);
	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Display screen's worth.		*/
	{
		display_buffer(lbuf[i]);						/* Display the line.			*/
		if (i != MAX_LINES_PER_SCREEN-1) vprint("\n");				/* Move to next line if appro.	*/
	}
	vmove(row,col);									/* Reposition the cursor		*/
}

static derror(text) char *text;								/* Error message reporting routine	*/
{
	register int i;									/* Working register.			*/
	int col, lin, mod, set;								/* Save locations.			*/
	
	lin = vcur_lin;									/* Save what we're doing.		*/
	col = vcur_col;
	set = vchr_set;
	mod = vcur_atr;

	vmode(CLEAR);									/* Clear all renditions.		*/
	vmode(REVERSE);									/* Select the reverse rendition.	*/
	vcharset(DEFAULT);								/* Assume the default position.		*/
	vmove(MAX_LINES_PER_SCREEN-1,0);						/* Move to the error location.		*/
	if (text[0] != ' ') vbell();							/* Ring the bells.			*/
	vprint("%s",text);								/* Print the error message.		*/

	while (vcur_col < vscr_wid-1) vprint(" ");					/* Space to the end of line.		*/

	vmove(lin,col);									/* Restore where we were.		*/
	vmode(mod);
	vcharset(set);

	while (vcheck() != 0);								/* Purge all type ahead.		*/
	errmsg_active = TRUE;								/* Now we have an error message.	*/
}

static int gets0(string,count)
char *string;
int count;
{
	int lin,col,set,mod;									/* Save locations.		*/
	int i;											/* Working variables.		*/
	int c;
	int term;										/* Terminator.			*/

	i = 0;											/* Assume no chars yet.		*/
	term = 0;										/* No terminator depressed yet. */

	if (count)										/* Does he want any chars?	*/
	{
		while ((i < count) && !term)							/* Repeat until term or full.	*/
		{
			c = vgetm();								/* Get the meta character.	*/

			if (errmsg_active)							/* Error message?		*/
			{
				lin = vcur_lin;							/* Save what we are doing.	*/
				col = vcur_col;
				set = vchr_set;
				mod = vcur_atr;
				vmove(MAX_LINES_PER_SCREEN-1,0);				/* Erase the error message.	*/
				vmode(CLEAR);
				vcharset(DEFAULT);
				verase(TO_EOL);
				vmode(mod);
				vcharset(set);
				vmove(lin,col);
			}

			if (c == return_key || c == enter_key) term = c = CHAR_CR;		/* Return terminator.		*/

			else if (c == delete_key)						/* Is it a delete?		*/
			{
				if (i)								/* Anything to rub out?		*/
				{
					vprint("\010 \010");					/* Rub out the last char.	*/
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
					vprint("\010 \010");					/* Rub out the last char.	*/
					i = i - 1;						/* Set counter back one char.	*/
				}
			}

			else if (c < ' ')							/* Is it a control char?	*/
			{
				derror("Control characters cannot be used in a search string, character ignored.");
			}

			else
			{
				vputc((char)c);							/* Echo the character.		*/
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

static int4 get_pos(input_file) FILE *input_file;					/* Get the position of a record.	*/
{
	long ftell();									/* ftell returns a int4eger.	*/

	return(ftell(input_file));							/* Return the record position.		*/
}

static int mem_err()									/* Memory error - could not allocate	*/
{											/* the needed memory.			*/
	werrlog(ERRORCODE(8),0,0,0,0,0,0,0,0);
	vexit();									/* Unconditional exit.			*/
	wexit(ERRORCODE(8));
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
	unsigned char	*blockptr;			/* Ptr to the block */
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
	unsigned char *ptr;
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

	if (ptr = (unsigned char *) malloc(mallsize))					/* Malloc a memory block		*/
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

static int check_empty(file_name)							/* See if the file is empty.		*/
char *file_name;
{
	int 	rc;
	FILE	*fh;

#ifdef MSDOS
	if ((fh = fopen(file_name,"rb")) == NULL)					/* Did the file open?			*/
#else /* !MSDOS */
	if ((fh = fopen(file_name,"r")) == NULL)					/* Did the file open?			*/
#endif /* !MSDOS */
	{
		werrlog(ERRORCODE(6),file_name,0,0,0,0,0,0,0);
		return(FAILURE);							/* It is fatal so return with error.	*/
	}
	fgetc(fh);
	rc = feof(fh);
	fclose(fh);
	return(rc);
}

static display_stat(dwidth)								/* Display cursor row, col and width.	*/
int dwidth;
{
	register int i;									/* Working register.			*/
	char *vsss();
	int dlen;
	char tstr[30];

	dlen = 14;									/* Set the display length.		*/

	if (ssave == NULL)
	{
		ssave = vsss(MAX_LINES_PER_SCREEN-2, vscr_wid-dlen, 1, dlen);		/* Save what is on the screen.		*/
	}

	vstate(SAVE);
	vmode(REVERSE);									/* Select the reverse rendition.	*/
	vcharset(DEFAULT);								/* Assume the default position.		*/
	vmove(MAX_LINES_PER_SCREEN-2, vscr_wid - dlen);					/* Move to the status location.		*/
	sprintf(tstr,"(%d,%d) %d",frow+1,fcol+1,dwidth);				/* Display as 1 based.			*/
	vprint("%-14.14s",tstr);
	vstate(RESTORE);
}
