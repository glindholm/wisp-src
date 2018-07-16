			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
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

#include "werrlog.h"
#include "wperson.h"
#include "scnfacs.h"

#define CHAR_CTRL_U	'\025'
#define CHAR_DEL	'\177'
#define CHAR_BELL	'\007'

#define LNFD		0x0A
#define FF		0x0C
#define CHAR_CR		0x0D

#define SEARCH_TEXT_SIZE	64							/* Maximum length of a search string.	*/
#define POSITION_TABLE_SIZE	1000							/* Number of lines for reverse scroll.	*/
#define PAGE_SCROLL_LINES	18							/* Number of lines for a page scroll.	*/
#define PAGE_SCROLL_COLS	20							/* Number of lines for a column scroll.	*/
#define BUFFER_SIZE		255							/* Maximum buffer size.			*/
#define TAB_SPACE_COUNT		8							/* Number of spaces equal to tab char.	*/

#define WINDOW_TOP	9								/* Top line of window area.		*/
#define WINDOW_ROWS	9								/* Number of lines in window area.	*/

#ifdef VMS
	/*
	**	VMS is SLOW compared to most of the UNIX boxes. 
	*/
#define CHECK_INTERVAL	200								/* Number of lines between checking	*/
#else
#define CHECK_INTERVAL	500								/* Number of lines between checking	*/
#endif

#ifdef MSDOS
char *wanguid3();
#endif

extern char WISPFILEXT[39];
static int errmsg_active = FALSE;							/* Error message is on the screen.	*/
static int rvrs_scroll_avail = TRUE;							/* Set reverse scroll available.	*/
static int frwd_scroll_avail = TRUE;							/* Set forward scroll available.	*/
static int ff_flag = FALSE;								/* Flag for FF found.			*/

static char srch_ch_map[256];
static int srch_len;
static FILE *input_file;								/* File control pointers.		*/

vdisplay(file_name,record_size) 							/* VIDEO file display subroutine.	*/
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
	int first_time;									/* First time help control flag.	*/
	long int fpos_tab[MAX_LINES_PER_SCREEN];					/* Position of each line in the file.	*/
	long int postab[POSITION_TABLE_SIZE];						/* Allocate the position table.		*/
	int tx;										/* Current table index.			*/
	int lc;										/* Count of lines in position table.	*/
	int text_defined;								/* Search text defined flag.		*/
	int search_abort, term_status;							/* Search has been aborted		*/
	unsigned char search_text[SEARCH_TEXT_SIZE+2];					/* Storage for the searc text.		*/
	int scrl_col_cnt;								/* Scroll column index count.		*/
	register int j,c;								/* Some working registers.		*/
	int i, retcd;									/* Return code from print routines.	*/
	int	check_time;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	if ((record_size < 0) || (record_size > 512))					/* Is record size valid.		*/
	{										/* Oops, invalid record size.		*/
		werrlog(ERRORCODE(2),record_size,0,0,0,0,0,0,0);
		vexit();								/* Unconditional exit.			*/
		wexit(ERRORCODE(2));
	}
	if (record_size > BUFFER_SIZE)							/* Is record size greater than buffer.	*/
	{										/* Invalid buffer size.			*/
		werrlog(ERRORCODE(4),BUFFER_SIZE,record_size,0,0,0,0,0,0,0);
		vexit();								/* Unconditional exit.			*/
		wexit(ERRORCODE(4));
	}
	vcapload();

	if ( *((int *)vcapdef[LINES]) == 25 )						/* is there a 25th line			*/
	{
		frwd_scroll_avail = FALSE;						/* Can't use forward scroll		*/
	}
	if (vcapnull(rvrsidx_esc, "SCROLL_REVERSE",0)) 
	{
		rvrs_scroll_avail = FALSE;						/* There is no reverse scroll		*/
	}

	wpload();
	first_time = TRUE;								/* First time only true the 1st time.	*/
	text_defined = FALSE;								/* Search text is not yet defined.	*/
	op_save = voptimize(DATA_CONTROLS_AND_MOTION);					/* Select deferred optimization.	*/
#ifdef VMS
	if ((record_size > 0) && (record_size <= 80)) vscreen(NARROW);			/* Set the narrow parameters.		*/
	else vscreen(WIDE);								/* Set the wide parameters.		*/
#else
	vscreen(NARROW);								/* Select a narrow screen unless VMS.	*/
#endif
	if (defaults.bgchange)								/* Set the screen background if		*/
	{										/* not done already.			*/
		if (defaults.bgcolor) vscreen(LIGHT);
		else vscreen(DARK);
	}
	if (record_size) bufsize = record_size;						/* Determine the buffer size.		*/
	else bufsize = BUFFER_SIZE - 1;							/* Else, variable length.		*/

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Allocate a screens worth of buffers.	*/
	{
		if ((lbuf[i] = malloc(bufsize+1+8)) == 0) mem_err();			/* Allocate a line buffer.		*/
	}

	if ((input_file = fopen(file_name,"r")) == NULL)				/* Did the file open?			*/
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
	scrl_col_cnt = 0;								/* Have not scrolled left or right yet.	*/

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
		if (eofile) break;

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
	restart_requested = FALSE;							/* Assume restart will not be needed.	*/
	viewing = TRUE;									/* Flag that we are viewing a file.	*/
	while (viewing)									/* Repeat until PF16 depressed.		*/
	{
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

		if ((c == fn1_key) || (c == home_key)) 					/* Toggle home/reverse home.		*/
		{
			if ((vcur_col != 0) || (vcur_lin != 0)) vmove(0,0);		/* Home if not home.			*/
			else vmove(MAX_LINES_PER_SCREEN-1,vscr_wid-1);			/* Else go to reverse home.		*/
		}

		else if (c == fn16_key) viewing = FALSE;				/* Does he want to exit?		*/

		else if (c == down_arrow_key) 						/* Down arrow?				*/
		{
			if (vcur_lin != MAX_LINES_PER_SCREEN-1) vmove(vcur_lin+1,vcur_col);	/* Position cursor?		*/
			else								/* No, so scroll a line.		*/
			{
				eofile = roll_up(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,1,scrl_col_cnt);
				if (eofile) derror("End of file encountered, no action performed.");
				else if (!frwd_scroll_avail)				/* In no forward scroll then need to	*/
				{							/* display manually.			*/
					full_screen_refresh(lbuf,MAX_LINES_PER_SCREEN-1,vcur_col);
				}
			}
		}

		else if (c == up_arrow_key)						/* Up arrow?				*/
		{
			if (vcur_lin != 0) vmove(vcur_lin-1,vcur_col);			/* Move cursor?				*/
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
			}
		}

		else if (c == left_arrow_key)						/* Left arrow?				*/
		{
			if (vcur_col != 0) vmove(vcur_lin,vcur_col-1);			/* Move cursor?				*/
			else if (scrl_col_cnt == 0) derror("Left edge of file encountered, no action performed.");
			else
			{
				scrl_col_cnt--;						/* Roll right one column.		*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
			}
		}

		else if (c == right_arrow_key)
		{
			if (vcur_col < vscr_wid-1) vmove(vcur_lin,vcur_col+1); 		/* Move cursor?				*/
			else if (eorec)
			{								/* Verify if has a record size.		*/
				if (record_size) derror("Right edge of file encountered, no action performed.");
				else derror("Right edge of buffer encountered, no action performed.");
			}
			else
			{
				scrl_col_cnt++;						/* Roll left one column.		*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
			}
		}

		else if (c == fn4_key)							/* Previous screen?			*/
		{
			if (fpos_tab[0] == 0 && vcur_lin == 0) derror("Already at start of file, no action performed.");
			else if (fpos_tab[0] == 0) vmove(0,vcur_col);			/* Move cursor to top of screen.	*/
			else if (lc == 0) 						/* Anywhere to go?			*/
			{
				rep_limit();						/* Report the scroll limit is reached.	*/
				win_active = TRUE;					/* Window is now active.		*/
			}
			else
			{
				for (i = 0; i < PAGE_SCROLL_LINES; i++)			/* Loop through and scroll.		*/
				{
					roll_down(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,&eofile,scrl_col_cnt);
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
				else vmove(eofl,vcur_col);				/* Move cursor to end of file.		*/
			}
			else 
			{
				for (i = 0; (i < PAGE_SCROLL_LINES) && !eofile; i++)	/* Scroll that many times.		*/
				{
					eofile = roll_up(lbuf,fpos_tab,postab,bufsize,input_file,&tx,&lc,1,scrl_col_cnt);
				}

				if (!frwd_scroll_avail)
				{
					full_screen_refresh(lbuf,MAX_LINES_PER_SCREEN-1,vcur_col); /* Move cursor to bottom	*/
				}
			}
		}

		else if (c == fn6_key)							/* Change width of screen.		*/
		{
			vmove(0,0);							/* Home before changing.		*/
			vdefer(RESTORE);						/* Restore from deferred actions.	*/
			if (vscr_wid == MAX_COLUMNS_PER_LINE) vscreen(NARROW);		/* Set narrow?				*/
			else vscreen(WIDE);						/* No, then set wide.			*/
			scr_refresh(lbuf,fpos_tab,MAX_LINES_PER_SCREEN,bufsize,scrl_col_cnt); /* Refresh the screen.	*/
			vmove(0,0);							/* Home for consistency.		*/
		}

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

		else if (c == fn9_key) 							/* Previous screen to left?		*/
		{
#ifdef OLD
			if (vcur_col >= PAGE_SCROLL_COLS) vmove(vcur_lin,vcur_col - PAGE_SCROLL_COLS); /* Move cursor?		*/
			else 
#endif
			/*
			**	Don't just move the cursor, do the scroll
			*/
			if (scrl_col_cnt == 0)
			{
				if (vcur_col > 0 ) vmove(vcur_lin,0);			/* Move cursor to beginning of line.	*/
				else derror("Already at left edge of file, no action performed.");
			}
			else
			{
				scrl_col_cnt -= PAGE_SCROLL_COLS;			/* Roll right defined number of cols.	*/
				if (scrl_col_cnt < 0) scrl_col_cnt = 0;			/* Can only scroll to beg. of line.	*/
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
			}
		}

		else if (c == fn10_key || c == tab_key)					/* Next screen right?			*/
		{
#ifdef OLD
			if (vcur_col + PAGE_SCROLL_COLS < vscr_wid-1) vmove(vcur_lin,vcur_col + PAGE_SCROLL_COLS);/* Move cursor?*/
			else if (eorec && (vcur_col > vscr_wid - PAGE_SCROLL_COLS) && (vcur_col < vscr_wid-1) )
			{								/* Move cursor to edge of screen.	*/
				vmove(vcur_lin,vscr_wid - 1);
			}
			else 
#endif
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
				scrl_col_cnt += PAGE_SCROLL_COLS;			/* Roll left defined number of cols.	*/
				if (scrl_col_cnt > (bufsize - vscr_wid)) scrl_col_cnt = bufsize - vscr_wid;
				roll_left_right(lbuf,fpos_tab,bufsize,input_file,&eorec,scrl_col_cnt);
			}
		}
 
		else if (c == fn14_key)							/* Print current screen?		*/
		{
			unsigned char *text, *ttxt;
			char filelibvol[22];
			int size;
			char errstr[80], temp[5];
			int	linesize;

			linesize = (vscr_wid < bufsize) ? vscr_wid : bufsize;

			size = ((linesize+1+8) * MAX_LINES_PER_SCREEN);			/* Compute the size.			*/
			if ((text = malloc(size)) == 0) mem_err();			/* Allocate a screen buffer.		*/

			ttxt = text;
			for (i = 0; i < MAX_LINES_PER_SCREEN; i++) 			/* Init screen buffer to print.	*/
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

			retcd = 0;
			retcd = di_write_file(text,size,linesize,filelibvol,filelibvol);
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

			retcd = 0;
			wprint(file_name,defaults.prt_mode,"DS",1,defaults.prt_class,defaults.prt_form,&retcd);
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

		else if ((c == help_key) && !win_just_removed) 				/* Does he/she want help?		*/
		{
			show_help(file_name);						/* Yes, so show it.			*/
			win_active = TRUE;						/* Now help is active.			*/
		}

		else if (!(win_just_removed || err_just_removed)) derror("Invalid key.");

		win_just_removed = FALSE;						/* Now window is gone.			*/
		err_just_removed = FALSE;						/* Now messages are gone.		*/
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
	vscreen(NARROW);								/* Restore to a narrow screen.		*/
	voptimize(op_save);								/* Restore optimization.		*/
	vcontrol(DUMP_OUTPUT);								/* Dump the buffer.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

static int get_line(lb,fpos_ptr,bufsize,input_file)					/* Compose an input line.		*/
register unsigned char *lb;
long int *fpos_ptr;
int bufsize;
FILE *input_file;
{
	unsigned char *l_lb;
	long int get_pos();								/* Get file postion routine.		*/
	int 	i, j, num, err;
	int 	eoline, eofile;								/* End of line and file flags.		*/
	register int c;

	eofile = FALSE;									/* Assume not end of file.		*/
	eoline = FALSE;									/* Assume not end of line.		*/

	l_lb = lb;									/* Set ptr to beginning of line.	*/
	*fpos_ptr = get_pos(input_file);						/* Get the position in the file.	*/

	for (i = 0; i < bufsize; i++)							/* Loop until full.			*/
	{
		if ((c = fgetc(input_file)) == EOF) 
		{
			eofile = TRUE;							/* Get the next character.		*/
			break;								/* Exit loop				*/
		}
		else 									/* There is data left to read.		*/
		{
			if (c >= ' ' && c <= '~') *(lb++) = c;				/* A displayable character?		*/
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
					if (i > 0)					/* If have stuff in buffer.		*/
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
				num = add_tab_cols(&i);					/* Put appropriate number of spaces	*/
				for (j = 0; j < num; j++) *(lb++) = ' ';		/* into the buffer.			*/
				i--;							/* Because for loop will add one.	*/
			}
			else if (c == CHAR_DEL);					/* Ignore delete character.		*/
			else if (c == '\b') *(lb++) = c;				/* Insert back space characters.	*/
			else if (c == CHAR_BELL) *(lb++) = c;				/* Insert bell characters.		*/
			else *(lb++) = ' ';						/* else is a non-displayable character.	*/
		}
	}
#ifdef OLD
	for (j = i; j < bufsize; j++) *(lb++) = ' ';					/* If not at end, fill with blanks.	*/
#endif
	memset(lb,' ',bufsize-i);
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
	for (pos = 0; pos < vscr_wid; pos++)						/* Do until at correct position for 	*/
	{										/* the current screen size.		*/
		if (*clb++ == CHAR_NULL)
		{
			erasefl = TRUE;							/* Make sure erases to end of line.	*/
			pos = vscr_wid;							/* Set so gets out of loop.		*/
		}
		else np++;
	}
	csave = lb[np];									/* Save the char at last pos on screen.	*/ 
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
	unsigned char string[62];							/* Working string.			*/
	char l_filen[256], *lptr;							/* Local copy of file_name.		*/
	register int i;									/* Working counter.			*/
	int len;

	strcpy(l_filen,file_name);							/* Copy file_name to local copy.	*/

	columns = 60;									/* The help area is 60 columns wide.	*/
	if (vscr_wid == MAX_COLUMNS_PER_LINE) lhs = 34;					/* Center the help screen.		*/
	else lhs = 10;

	vstate(-1);									/* Save the current screen state.	*/
	vmove(WINDOW_TOP-1,lhs-1);							/* Draw the outline box.		*/
	vline(VERTICAL,WINDOW_ROWS+2);
	vmove(WINDOW_TOP-1,lhs-1+columns);
	vline(VERTICAL,WINDOW_ROWS+2);
	vmove(WINDOW_TOP-1,lhs-1);
	vline(HORIZONTAL,columns+1);
	vmove(WINDOW_TOP+WINDOW_ROWS,lhs-1);
	vline(HORIZONTAL,columns+1);

	for (i = 0; i < 62; i++) string[i] = ' ';					/* Initialize the working string.	*/
	str_insert(&string[2],"Displaying file");					/* Insert the file indicator.		*/
	len = strlen(l_filen);
	lptr = l_filen;									/* Set the pointer.			*/
	for (i = 41; i <= len; i++)							/* Chop off file name from left if too	*/
	{										/* long for display purposes.		*/
		lptr++;									/* Set ptr over one char.		*/
	}
	str_insert(&string[18],lptr);							/* Insert the file name.		*/
	string[59] = CHAR_NULL;								/* Store a trailing null.		*/

	i = WINDOW_TOP;									/* Point the help area.			*/
	vmode(REVERSE|UNDERSCORE);							/* Do it in reverse underscore,		*/
	vmove(i++,lhs);
	vprint("%s",string);								/* Print the file name.			*/

	vmode(CLEAR);									/* Clear all renditions.		*/
	vmode(REVERSE);									/* Do it in reverse.			*/
	vmove(i++,lhs);
	vprint("  Use the arrow keys to scroll up, down, left and right.   ");
	vmove(i++,lhs);
	vprint("   (1) - Home cursor   (2) - File top      (3) - Bottom    ");
	vmove(i++,lhs);
	if (vscr_wid == MAX_COLUMNS_PER_LINE) vprint("   (4) - Prev screen   (5) - Next screen   (6) - 80 col    ");
					 else vprint("   (4) - Prev screen   (5) - Next screen   (6) - 132 col   ");
	vmove(i++,lhs);
	vprint("   (7) - Find string   (8) - Find next                     ");
	vmove(i++,lhs);
	vprint("   (9) - Scroll left  (10) - Scroll right                  ");
	vmove(i++,lhs);
	vprint("  (14) - Print screen (15) - Print file   (16) - Exit      ");
	vmove(i++,lhs);
	vprint("  Depress any key to remove this window.                   ");
	vmove(i++,lhs);
	vprint("  Use the HELP key to bring this window back.              ");
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

	if (vscr_wid == MAX_COLUMNS_PER_LINE) lhs = 34;					/* Center the help screen.		*/
	else lhs = 10;

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

	if ((j = WINDOW_TOP - 1) >= MAX_LINES_PER_SCREEN) j = j - MAX_LINES_PER_SCREEN;	/* Get the first line.			*/

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
**			on = -1		Flash off 	(Clear the center of the box)
**			on = 1		Flash on	(Write "Working" in the box)
**
**	Returns:	The next value of "on" to use.
*/
static int work_message(on) int on;							/* Display the working message.		*/
{
	int lhs;									/* Left hand side of display.		*/
	register int i;									/* Working register.			*/

	vstate(-1);									/* Save wher we are.			*/

	lhs = 32;									/* Assume at column 32.			*/
	if (vscr_wid == MAX_COLUMNS_PER_LINE) lhs = 58;					/* On wide screens put at column 58.	*/

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
long int fpos_tab[], *pt;
int bufsize, *tx, *lc, send_out, roll_cnt;
FILE *input_file;
{
	unsigned char *nlb;								/* Temporary storage.			*/
	long int temp_position;
	int i, eofile;									/* End of file flag.			*/
	unsigned char temp_line[BUFFER_SIZE+8];

	eofile = FALSE;									/* Assume not end of file.		*/

	eofile = get_line(temp_line,&temp_position,bufsize,input_file);			/* Get another line.			*/
	if (!eofile) 									/* Are we at the end of file?		*/
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
#ifdef OLD
		nlb = lbuf[MAX_LINES_PER_SCREEN - 1];					/* Get address of last buffer.		*/
		for (i = 0; i < bufsize; i++) *nlb++ = temp_line[i];			/* Copy the string.			*/
#endif
		memcpy(lbuf[MAX_LINES_PER_SCREEN-1],temp_line,bufsize);

		fpos_tab[MAX_LINES_PER_SCREEN -1] = temp_position;			/* Remember the new position.		*/
	}
	return(eofile);									/* Return the end of file indicator.	*/
}

static roll_down(lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,eofile,roll_cnt)		/* Scroll down one line.		*/
unsigned char *lbuf[];
long int fpos_tab[], *pt;
int bufsize, *tx, *lc, *eofile, roll_cnt;
FILE *input_file;
{
	unsigned char *nlb, *temp_line;							/* Temporary storage.			*/
	long int temp_position, temp_fp;
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
			if ((temp_line = malloc(bufsize+1+8)) == 0) mem_err();		/* Allocate a line buffer.		*/
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
long int fpos_tab[];
int bufsize, *eorec, roll_cnt;
FILE *input_file;
{
	unsigned char *lb;
	register int i, j, c;
	int sc,sl;									/* Variable to save current cursor pos.	*/
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

	for (i = 0; i < roll_cnt; i++) lb++;						/* Scroll the buffer according to data.	*/
	display_buffer(lb);								/* Display the line.			*/
}

static int add_tab_cols(col_cnt)							/* Add number of spaces to column count	*/
int *col_cnt;										/* for current occurance of tab char.	*/
{
	int remdr;									/* Remainder of division calculation.	*/

	if ((remdr = *col_cnt % TAB_SPACE_COUNT) == 0) 					/* Is it a full tab?			*/
	{
		*col_cnt = *col_cnt + TAB_SPACE_COUNT;					/* Yes, so add full value of tab.	*/
		return(TAB_SPACE_COUNT);						/* Return number of spaces for tab.	*/
	} 
	else										/* Else calculate adjustment.		*/
	{
		*col_cnt = *col_cnt + TAB_SPACE_COUNT - remdr;				/* Find out what tab really did.	*/
		return(TAB_SPACE_COUNT - remdr);					/* Return number of spaces for tab.	*/
	}
}

on_screen(search_text,lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,eofile,eorec,roll_cnt) 	/* Is the text on the screen.		*/
unsigned char *search_text, *lbuf[];
long int fpos_tab[], *pt;
int bufsize, *tx, *lc, *eofile, *eorec, *roll_cnt;
FILE *input_file;
{
	unsigned char *lb, temp[BUFFER_SIZE];						/* Temporary storage.			*/
	register int i, j;								/* Working registers.			*/
	int first_in;									/* Flag so will step past if first line.*/
	int k;										/* Tab count so display cursor correctly*/

	i = vcur_lin;									/* Set to the current line.		*/
	j = *roll_cnt + vcur_col;							/* Set to the current column in buffer.	*/
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
			for (k = 0; k < j; k++)	lb++;					/* Step to current occurance.		*/
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
	while (j < cnt)			 						/* Loop to end of line.			*/
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
long int fpos_tab[], *pt;
int bufsize, *tx, *lc, *eofile, *eorec, *roll_cnt;
FILE *input_file;
{
	unsigned char lbuf_save[MAX_LINES_PER_SCREEN][BUFFER_SIZE];			/* Save the current screen.		*/
	long int fp_save[MAX_LINES_PER_SCREEN];						/* Save the current pointers.		*/
	long int pt_save[POSITION_TABLE_SIZE];						/* Save the position table.		*/
	int tx_save, lc_save;								/* Save the index pointers.		*/
	int lin_save,col_save;								/* Position save locations.		*/
	long int pos_save;								/* Save current file pointer.		*/
	int found;									/* Flag to indicate text found.		*/
	int lines_read;									/* Lines read in search.		*/
	long int get_pos();								/* Get position returns a long.		*/
	unsigned char *lb, temp[BUFFER_SIZE];						/* Temporary working buffer.		*/
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
		l_eof = roll_up(lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,0,*roll_cnt);	/* Read a line.				*/

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
						**	NOTE:   Use the real eofile instead of local since we are really moving
						**	 	ahead in the file.
						*/
						*eofile = roll_up(lbuf,fpos_tab,pt,bufsize,input_file,tx,lc,0,*roll_cnt);
						if (*eofile) break;
						lines_read++;
					}
					lines_scrolled = i;
				}
				scr_refresh(lbuf,fpos_tab,lines_read,bufsize,*roll_cnt);	/* Yes, then write the string.	*/
				vmove(MAX_LINES_PER_SCREEN-(2+lines_scrolled),vscr_wid-1); /* Move to the end of previous line.	*/
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
						for (i = 0; i < MAX_LINES_PER_SCREEN; i++) /* Restore the current data buffers.	*/
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
						found = 2;				/* Set so will display correct message.	*/
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
long int fpos_tab[];
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
long int fpos_tab[];
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
	lbuf[i] = tbufa;								/* last/first buffer pts to first/last.	*/
	fpos_tab[i] = 0;								/* Set so no position.			*/
}

static full_screen_refresh(lbuf,row,col)
unsigned char *lbuf[MAX_LINES_PER_SCREEN];
{
	int	i;
	verase(FULL_SCREEN);
	for (i = 0; i < MAX_LINES_PER_SCREEN; i++) 					/* Display screen's worth.		*/
	{
		display_buffer(lbuf[i]);						/* Display the line.			*/
		if (i != MAX_LINES_PER_SCREEN-1) vprint("\n"); 				/* Move to next line if appro.	*/
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
	term = 0;										/* No terminator depressed yet.	*/

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
/*	VMS indexed files too.  For UNIX, depending on how indexed files are implemented, this routine (get_pos()) might have	*/
/*	to updated so that get_pos() returns the file position of the next record to be read (i.e. the primary key is scanned	*/
/*	and used to determine the positon of the next record). Provided this routine (get_pos()) functions correctly, vdisplay	*/
/*	will be able to display virtually any file.										*/

static long int get_pos(input_file) FILE *input_file;					/* Get the position of a record.	*/
{
	long ftell();								/* ftell returns a long integer.	*/

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
long xftell();

static int	blocks_loaded=0;			/* Number of blocks loaded */
static int	curr_block=0;				/* The current block in use (first == 0) */
static int	curr_pos=0;				/* The current position in curr_block (offset) */
static long	curr_byte=0;				/* The current byte (offset) */
static int	file_loaded=0;				/* Is the file fully loaded */
static int	highest_byte=0;				/* The highest byte loaded */

#define MAX_MEMBLOCKS	1000
struct
{
	unsigned char	*blockptr;			/* Ptr to the block */
	long	highbyte;				/* The highest byte in this block */
	long	msize;					/* The malloc size */
	long	usize;					/* The used size */
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
long	byte_pos;
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
static long xftell()
{
	return(curr_byte);
}

static loadblock()
{
	int	c;
	unsigned char *ptr;
	long	cnt;
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

