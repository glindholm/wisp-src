			/************************************************************************/
			/*		             Copyright (c) 1993				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include required header files.							*/

#include <stdio.h>									/* Include Standard I/O header.		*/

#ifdef VMS										/* Include VMS specific headers.	*/
#include <descrip.h>									/* Include descriptor layout for VMS.	*/
#include <ssdef.h>									/* Include VMS system messages.		*/
#endif

#ifndef unix										/* Include DOS & VMS headers.		*/
#include <stdlib.h>									/* Include the standard library.	*/
#endif

#ifndef VMS										/* Include the Unix and DOS headers.	*/
#include <malloc.h>									/* Include memory allocation headers.	*/
#endif

#include "video.h"									/* Include the video headers.		*/
#include "vlocal.h"									/* Include video local headers.		*/
#include "vdata.h"									/* Include the video data base.		*/
#include "vform.h"									/* Include the forms headers.		*/

#include <ctype.h>									/* Get character type macros.		*/
#include <math.h>									/* Get math macros.			*/

/*						Static and Global Data Definitions.						*/

static int rts_first = TRUE;								/* First time flag.			*/
static int ask_keys_on = TRUE;								/* Assume help keys to show.		*/
static int first_row = 0;								/* First row and column.		*/
static int first_column = 0;
static int first_error = 0;
static char last_form_displayed[HP_FORM_NAME_SIZE+2];					/* Remember who we last displayed.	*/
static int show_screen();
static int process_input();
static int in_field();
static int in_mod_field();
static int show_window();
static int next_field();
static int find_field();
static int last_field();
static int is_visible();
static int echo();
static int show_field();
static int return_data();

/*						Subroutine entry point.								*/

int vform(form,data,message) struct video_form *form; unsigned char *data, *message;	/* Forms information.			*/
{
	int term;
	int i;

	vbuffering(LOGICAL);								/* Buffer logically.			*/

	if (rts_first) 									/* First time in program?		*/
	{
		rts_first = FALSE;							/* Clear the first time flag.		*/
		vdefer(RESTORE);							/* Restore from any deferred states.	*/
		vstate(0);								/* Set to a known state.		*/
		verase(FULL_SCREEN);							/* Erase the screen.			*/
		vscreen(DARK|NARROW);							/* Set screen to standard state.	*/
		for (i = 0; i < HP_FORM_NAME_SIZE; i++) last_form_displayed[i] = ' ';	/* No form yet displayed.		*/
		highest_line_written = -1;						/* Nothing written yet.			*/
	}

	if (isdebug())									/* Is a debugger running.		*/
	{
		vstate(SAVE_DEBUG);							/* Yes then save the debuggers term.	*/
		if (!rts_first)								/* Is there something on the screen?	*/
		{
/*			vdefer(RESTORE);	*/					/* Restore from any defered states.	*/
/*			vrefresh(HARD_REFRESH);	*/					/* Restore to initial state.		*/
/*			vcontrol(DUMP_OUTPUT);	*/					/* Actually show the screen.		*/
		}
	}

	show_window(form,message);							/* Show the error message window.	*/

	if (form->control & FORM_READ_ONLY) i = FALSE;
	else if ((veqfn(form->name,last_form_displayed)) && (form->start_row == (highest_line_written - form->text_rows + 1)))
	{
		i = FALSE;
	}
	else i = TRUE;									/* Show it.				*/
	if (!show_screen(form,data,i)) return(FAILURE);					/* Display the screen.			*/

	vbuffering(AUTOMATIC);

	if (form->control & FORM_WRITE_ONLY) term = 0;					/* Don't process input.			*/
	else term = process_input(form,data);						/* Now process the input.		*/

	for (i = 0; i < form->fields; i++) form->field[i].row = form->field[i].row - form->start_row;	/* Debias 		*/
	vmode(0);
	vcharset(0);
	if (isdebug()) vstate(RESTORE_DEBUG);						/* Restore the debugger's state.	*/
	return(term);									/* Return with success (for now).	*/
}

static int show_screen(form,data,d) struct video_form *form; unsigned char *data; int d;
{
	register int i, j;								/* Working counters.			*/
	unsigned char *dataptr;								/* Working pointer.			*/

	first_row = 0;									/* Assume we don't know where the first	*/
	first_column = 0;								/*  row and column are.			*/

	vbuffering(LOGICAL);								/* Turn on buffering if not on.		*/

	if (d && (form->start_row+form->text_rows) > MAX_LINES_PER_SCREEN)		/* Error check.				*/
	{
		vroll(freeze_top+1,23);							/* Set the scroll area.			*/
		vmove(23,0);
		for (i = 0; i < form->text_rows; i++) vprint("\n");			/* Scroll the screen.			*/
		vroll(0,23);
		form->start_row = form->start_row - form->text_rows;			/* Decrement the start of form.		*/
	}

	vmove(form->start_row,0);							/* Move to the start of the screen.	*/
	vmode(CLEAR);									/* Select the normal rendition.		*/

	if (d) highest_line_written = form->start_row - 1;				/* Start is now highest written.	*/
	if (d)
	{
		for (i = 0; i < form->text_rows; i++)					/* Loop through each line.		*/
		{
			vprint("%s",form->text[i]);					/* Print the text.			*/
			if (vcur_col < vscr_wid-1) verase(TO_EOL);			/* Erase to the end of the line.	*/
			if (i != form->text_rows-1) vprint("\n");			/* Go to the next line.			*/
			highest_line_written++;						/* Count the highest line written.	*/
		}
	}

	if (d && form->clear_screen)							/* Clear the screen?			*/
	{
		for (i = form->start_row + form->text_rows; i < MAX_LINES_PER_SCREEN; i++)
		{
			vprint("\n");							/* Move to the next line.		*/
			verase(TO_EOL);							/* Erase it.				*/
		}
	}

	if (d) for (i = 0; i < form->lines; i++)					/* Load the lines.			*/
	{
		vmove(form->line[i].row+form->start_row, form->line[i].column);		/* Move to where the line starts.	*/
		vline(form->line[i].type,form->line[i].length);				/* Draw the line.			*/
	}
	vcharset(0);									/* Remove the line character set.	*/

	first_error = FALSE;								/* No error field yet.			*/
	dataptr = data;									/* Point to the start of the data.	*/
	for (i = 0; i < form->fields; i++)						/* Loop through each field.		*/
	{
		form->field[i].row = form->field[i].row+form->start_row;		/* Bias the row.			*/
		if (form->field[i].type && (first_row == 0) && (first_column == 0))	/* Have we determined the first field?	*/
		{
			first_row = form->field[i].row;					/* No, then remember it.		*/
			first_column = form->field[i].column;
		}
		if (!first_error && form->field[i].error_code)				/* Is this the first error field?	*/
		{
			first_row = form->field[i].row;					/* No, then remember it.		*/
			first_column = form->field[i].column;
			first_error = TRUE;						/* Now set the error found flag.	*/
			vbell();							/* Ring the bells.			*/
		}
		show_field(form,i,dataptr);						/* Show the field.			*/
		dataptr = dataptr + form->field[i].length;				/* Increment to the next data area.	*/
	}
	vmode(CLEAR);									/* Clean up.				*/
	vcharset(0);									/* Normal character set.		*/
	vbuffering(AUTOMATIC);								/* Dump the buffer to show the screen.	*/
	vputlen(last_form_displayed,form->name,HP_FORM_NAME_SIZE);			/* Store the form name.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

static int process_input(form,data) struct video_form *form; unsigned char *data;	/* Process input.			*/
{
	register int i,j,k;								/* Working registers.			*/
	int filling;									/* Screen filling control flag.		*/
	int good;									/* Good character flag.			*/
	int f;										/* Last field flag.			*/
	int term;

	if ((form->keys_on) && (highest_line_written < MAX_LINES_PER_SCREEN-2)) 	/* Put the help keys up.		*/
	{
		vkeyhelp(ON);
		ask_keys_on = TRUE;
	} 
	else										/* Unless they don't want them.		*/
	{
		vkeyhelp(OFF);
		ask_keys_on = FALSE;
	}

	vmove(first_row,first_column);							/* Move to the first field or wherever.	*/
	f = in_field(form,-1,first_row,first_column);					/* Set the field tracker.		*/

	filling = TRUE;									/* Start filling the screen.		*/
	while (filling)									/* Repeat until an exit event.		*/
	{
		k = vml(vcur_lin);							/* Get table index.			*/
		good = TRUE;								/* Assume character will be good.	*/

		if (vcur_lin >= 22) vkeyhelp(OFF);					/* Turn off the help keys.		*/
		else if (ask_keys_on) vkeyhelp(ON);					/* Turn them on?			*/

		i = vgetm();								/* Get a meta character.		*/

		if (is_visible(i))							/* Is it a printing character.		*/
		{
			if ((f = in_mod_field(form,f,vcur_lin,vcur_col)) < 0)		/* Are we in a modifyable field?	*/
			{
				vbell();						/* No, then beep.			*/
				f = find_field(form);					/* Move to the next field.		*/
				if (f >= 0) echo(i,f,form);				/* Echo it.				*/
			}
			else echo(i,f,form);						/* Yes, then echo as appropriate.	*/
			if ((j = in_mod_field(form,f,vcur_lin,vcur_col)) < 0) 		/* Did we move out of field space?	*/
			{
				f = next_field(form,f);					/* Yes, then move to the next field.	*/
			}
		}
		else if ((i == return_key) || (i == enter_key))				/* A return key?			*/
		{
			return_data(form,data);						/* Return the data.			*/
			term = 0;							/* Terminated on return key.		*/
			filling = FALSE;						/* All done.				*/
		}
		else if (j = vfnkey(i))							/* A function key?			*/
		{
			if (((j >=1) && (j <= 8)) || (j == 10))				/* Always return on PF key.		*/
			{
				return_data(form,data);					/* Return the data.			*/
				term = j;						/* Return the function key.		*/
				filling = FALSE;					/* Done.				*/
			}
			else vbell();							/* Not a valid key.			*/
		}
		else if (i == up_arrow_key)						/* Going up?				*/
		{
			if (vcur_lin == 0) vbell();					/* Error.				*/
			else vslew(-1,0);						/* Move up one line.			*/
		}
		else if (i == down_arrow_key)						/* Down?				*/
		{
			if (vcur_lin >= MAX_LINES_PER_SCREEN-1) vbell();		/* Error.				*/
			else vslew(1,0);						/* Move down one line.			*/
		}
		else if (i == left_arrow_key)						/* Go left.				*/
		{
			if (vcur_col == 0) vbell();					/* Cannot go left.			*/
			else vslew(0,-1);						/* Go left one line.			*/
		}
		else if (i == right_arrow_key)						/* Go right.				*/
		{
			if (vcur_col >= vscr_wid-1) vbell();				/* Error.				*/
			else vslew(0,1);						/* Go right one position.		*/
		}
		else if (i == tab_key) 							/* Tab to the next field.		*/
		{
			if ((f = in_field(form,f,vcur_lin,vcur_col)) >= 0)		/* In a field now?			*/
			{
				if ((f = next_field(form,f)) < 0) vbell();		/* Yes, move to the next one.		*/
			}
			else if ((f = find_field(form)) < 0) vbell();			/* Beep if there isn't one.		*/
		}

		else if ((i == backtab_key) || (i == 8))				/* Move to the last field.		*/
		{
			if ((f = in_field(form,f,vcur_lin,vcur_col)) < 0)		/* In a field?				*/
			{
				if ((f = find_field(form)) < 0) vbell();		/* No, so bell if there isn't one.	*/
			}
			if (f >= 0)							/* In a field?				*/
			{
				if ((f = last_field(form,f)) < 0) vbell();		/* Yes, move to the last one.		*/
			}
		}

		else if (i == help_key)
		{
			if (vcur_lin >= 22) vbell();					/* Beep if in low key area.		*/
			else
			{
				vkeyhelp(TOGGLE_KEY_DISPLAY);				/* Give help.				*/
				if (ask_keys_on) ask_keys_on = FALSE;
				else ask_keys_on = TRUE;
			}
		}

		else if (i == clear_field_key);

		else if (i == clear_before_key);

		else if (i == clear_after_key);

		else if (i == home_key);

		else if (i == delete_key)							/* Delete or backspace?		*/
		{
			f = in_field(form,f,vcur_lin,vcur_col);					/* Are we in a field?		*/
			if ((f >= 0) && (vcur_col > form->field[f].column)) 			/* Are we at the start of it?	*/
			{
				vslew(0,-1);							/* No, then slew back.		*/
				vputc(' ');							/* Output a space.		*/
				vslew(0,-1);							/* And back again.		*/
			}
			else vbell();								/* No, then just beep.		*/
		}

		else if (i == insert_key);

		else if (i == remove_key);

		else if (i == cancel_key);

		else vbell();								/* Unknown key so just beep.		*/

	}
	return(term);
}

static int next_field(form,f) struct video_form *form; int f;				/* Select the next field.		*/
{
	int i,j;									/* Working registers.			*/
	int found;									/* Field found flag.			*/

	for (i = f+1; i < form->fields; i++)						/* Loop through each field.		*/
	{
		if (form->field[i].type && (i < form->fields))				/* Is this the field.			*/
		{
			vmove(form->field[i].row,form->field[i].column);		/* Move to the start of the field.	*/
			return(i);							/* Return.				*/
		}
	}

	for (i = 0; i < f; i++)								/* Loop from the start.			*/
	{
		if (form->field[i].type && (i < form->fields))				/* Is this the field.			*/
		{
			vmove(form->field[i].row,form->field[i].column);		/* Move to the start of the field.	*/
			return(i);							/* Return.				*/
		}
	}

	return(f);									/* Return where we were to caller.	*/
}

static int find_field(form) struct video_form *form;					/* Find the next field in order.	*/
{
	register int i,j;								/* Working integers.			*/
	int f;										/* Last field counter.			*/

	for (j = vcur_col+1; j < vscr_wid-1; j++)					/* First check the current line.	*/
	{
		if (((f = in_field(form,-1,vcur_lin,j)) >= 0) && form->field[f].type)	/* Are we in a field yet?		*/
		{
			vmove(form->field[f].row,form->field[f].column);		/* Move to the field.			*/
			return(f);							/* Return to the caller.		*/
		}
	}

	for (i = vcur_lin+1; i < MAX_LINES_PER_SCREEN-1; i++)				/* Loop through lines to the end.	*/
	{
		for (j = 0; j < vscr_wid-1; j++)					/* Loop through each column.		*/
		{
			if (((f = in_field(form,-1,i,j)) >= 0) && form->field[f].type)	/* Are we in a field?			*/
			{
				vmove(form->field[f].row,form->field[f].column);	/* Move to the field.			*/
				return(f);						/* Return to the caller.		*/
			}
		}
	}

	for (i = 0; i < vcur_lin; i++)							/* Loop from start to line before.	*/
	{
		for (j = 0; j < vscr_wid-1; j++)					/* Loop through each column.		*/
		{
			if (((f = in_field(form,-1,i,j)) >= 0) && form->field[f].type)	/* Are we in a field?			*/
			{
				vmove(form->field[f].row,form->field[f].column);	/* Move to the field.			*/
				return(f);						/* Return to the caller.		*/
			}
		}
	}

	for (j = 0; j < vcur_col; j++)							/* Check from start of current line.	*/
	{
		if (((f = in_field(form,-1,vcur_lin,j)) >= 0) && form->field[f].type)	/* Are we in a field yet?		*/
		{
			vmove(form->field[f].row,form->field[f].column);		/* Move to the field.			*/
			return(f);							/* Return to the caller.		*/
		}
	}

	return(-1);									/* No field found.			*/
}

static int last_field(form,f) struct video_form *form; int f;				/* Select the last field.		*/
{
	int i,j;									/* Working registers.			*/
	int found;									/* Field found flag.			*/

	for (i = f-1; i >= 0; i--)							/* Loop through each field.		*/
	{
		if (form->field[i].type && (i < form->fields))				/* Is this the field.			*/
		{
			vmove(form->field[i].row,form->field[i].column);		/* Move to the start of the field.	*/
			return(i);							/* Return.				*/
		}
	}

	for (i = form->fields-1; i > f; i--)						/* Loop from the start.			*/
	{
		if (form->field[i].type && (i < form->fields))				/* Is this the field.			*/
		{
			vmove(form->field[i].row,form->field[i].column);		/* Move to the start of the field.	*/
			return(i);							/* Return.				*/
		}
	}

	return(f);									/* Return where we were to caller.	*/
}

static int is_visible(i) int i;								/* Check if a character is visible.	*/
{
	if ((i < 040) || (i > 0376)) return(FALSE);					/* Not visible.				*/
	return(SUCCESS);								/* Visible.				*/
}

static int in_mod_field(form,last,r,c) struct video_form *form; int last, r, c;		/* Check if we are in a field.		*/
{
	register int i,j;								/* Working registers.			*/

	i = in_field(form,last,r,c);							/* Are we in a field?			*/
	if (form->field[i].type) return(i);						/* Is it modifyable?			*/
	return(-1);									/* No, so return invalid.		*/
}

static int in_field(form,last,r,c) struct video_form *form; int last, r, c;		/* Check if we are in a field.		*/
{
	register int i,j;								/* Working registers.			*/
	int passed;									/* Have we passed the field.		*/

	if (last >= 0)									/* Are we already in a field?		*/
	{
		i = last;
		if ((form->field[i].row == r) && ((c >= form->field[i].column)
				&& (c < (form->field[i].column + form->field[i].length)))) return(i);
	}

	for (i = 0, passed = FALSE; (i < form->fields) && !passed; i++)			/* Loop through all fields.		*/
	{
		if (form->field[i].row  > r) passed = TRUE;				/* Don't keep looping if passed.	*/
		if (form->field[i].row == r)						/* On this line?			*/
		{
			if ((c >= form->field[i].column) && (c < (form->field[i].column + form->field[i].length)))
			{
				return(i);						/* In field so return field position.	*/
			}
		}
	}
	return(-1);
}

static int echo(i,j,form) int i,j; struct video_form *form;				/* Echo a character.			*/
{
	register int k;									/* Working register.			*/
	unsigned char c;								/* Working character.			*/

	if (form->field[j].error_code) vmode(form->error_rendition);			/* Error rendition?			*/
	else vmode(form->field[j].rendition);						/* Select the field's rendition.	*/
	c = (unsigned char) i;								/* Cast to a character.			*/

	if (form->field[j].secure) 							/* Don't echo if secure.		*/
	{
		vputc(' ');								/* Output a space.			*/
		vdefer(RESTORE);							/* Do it now.				*/
		k = vml(vcur_lin);							/* Determine the map index.		*/
		vchr_map[k][vcur_col-1] = c;						/* Store it in the map ** dangerous **	*/
	}
	else vputc(c);									/* Output the character.		*/

	return(SUCCESS);								/* All done.				*/
}

static int show_field(form,n,dataptr) struct video_form *form; int n; unsigned char *dataptr;
{
	register int i,j,k;
	unsigned char *dp;

	dp = dataptr;									/* Point to the data.			*/
	vmove(form->field[n].row,form->field[n].column);				/* Move to the start of the field.	*/
	if (form->field[n].error_code) vmode(form->error_rendition);			/* Error rendition?			*/
	else vmode(form->field[n].rendition);						/* Select the field's rendition.	*/

	for (j = 0; j < form->field[n].length; j++)					/* Output each character.		*/
	{
		if (form->field[n].secure)						/* Echo blanks?				*/
		{
			vputc(' ');							/* Output a space.			*/
			vdefer(RESTORE);						/* Do it now.				*/
			k = vml(vcur_lin);						/* Determine the map index.		*/
			vchr_map[k][vcur_col-1] = *dp++;				/* Store it in the map ** dangerous **	*/
		}
		else vputc(*dp++);							/* Output the character.		*/
	}
}

static int return_data(form,data) struct video_form *form; unsigned char *data;		/* Return data to the user.		*/
{
	register int i,j,k;								/* Working registers.			*/
	unsigned char *dp;								/* Working pointer.			*/

	vkeyhelp(OFF);									/* Turn the keys off.			*/
	dp = data;									/* Point to the data.			*/

	for (i = 0; i < form->fields; i++)						/* Loop through each field.		*/
	{
		k = vml(form->field[i].row);						/* Get index into the map.		*/
		for (j = form->field[i].column; j < (form->field[i].column + form->field[i].length); j++)
		{
			*dp++ = vchr_map[k][j];						/* Transfer back the character.		*/
		}
	}
	return(SUCCESS);								/* Return to the caller.		*/
}

static int show_window(form,message) struct video_form *form; unsigned char message[];
{
	int i,j;

	vmove(form->window_line,0);							/* Move to the error window line.	*/
	vmode(form->window_rendition);							/* Yes, then set the rendition.		*/

	for (i = 0; ((message[i] != CHAR_NULL) && (i < 80)); i++)
	{
		if (message[i] >= ' ') vputc(message[i]);				/* Output printing characters.		*/
		else if ((message[i] == '\033') && (message[i+1] == '&') && (message[i+2] == 'd'))	/* Is this an escape?	*/
		{
			i = i+3;							/* Move past the escape sequence.	*/
			if (message[i] == 'C') vmode(BLINK | BOLD | REVERSE);		/* Put on reverse blink?		*/
			else if (message[i] == '@') vmode(BOLD);			/* Turn off all renditions.		*/
			else
			{
				vre_window("vform:show_window: Invalid enhancement escape sequence, term = %o",message[i]);
				vexit(0); exit(0);
			}
		}
		else									/* Something invalid in the output.	*/
		{
			vre_window("vform:show_window: Invalid character (%o) in output.",message[i]);
			vexit(0); exit(0);
		}
	}
	while(vcur_col < vscr_wid-1) vputc(' ');					/* Pad to the end.			*/
	vmode(CLEAR);									/* Set back to normal background.	*/
	return(SUCCESS);
}
