/*
******************************************************************************
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
******************************************************************************
*/

/*
**	File:		vsedfnd.c
**
**	Project:	wisp/vsedit
**
**	Purpose:	FIND function
**
**	Routines:	
*/

/*
**	Includes
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "idsistd.h"
#include "idsisubs.h"
#include "vsedfnd.h"
#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "vsedit.h"
#include "vsedmod.h"
#include "vseutl.h"

/*
**	Structures and Defines
*/

#define CHAR_NULL (char)0
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

/*
**	Globals and Externals
*/

/*
**	Static data
*/

static char save_oa[4];
static int screen_error;

static int backward;

static int4 fnd_number, fnd_quoted;
static TEXT *fnd_txt, *start_txt;
static int4 fnd_row;

static int4 found;
static int fnd_start_col = 0;
static int4 g_start_line,g_end_line;

static char fnd_field[81], emsg_field[81];
static char repl_field[81], chng_field[81];
static char start_field[17], end_field[17];


static VSESCR_FLDS(ed_fnd_base_flds) = {
{LEN(0)		ROW(1)	COL(2)	VALUE("Enter the text string or line number to be found, or select:")},
{LEN(0)		ROW(1)	COL(77)	ULINEVALUE("Find")},
{LEN(0)		ROW(2)	COL(2)	VALUE("(1) Exit  (2) Set Tabs or Case Options (8) Search Backwards")},
{LEN(0)		ROW(3)	COL(2)	BRIGHT(emsg_field)},
{LEN(0)		ROW(4)	COL(2)	VALUE("Find =")},
{LASTITEM}
};
static VSESCR_FLDS(ed_fnd_upper_flds) = {
{LEN(0)		ROW(4)	COL(9)	USING(fnd_field)},
{LASTITEM}
};

static VSESCR_FLDS(ed_fnd_uplow_flds) = {
{LEN(0)		ROW(4)	COL(9)	UPLOW(fnd_field)},
{LASTITEM}
};

static char std_chng_first_line[81] = "(ENTER) Change text in Range  (1) Exit  (2) Set tabs or Case options";
static char chng_first_line[81];

static VSESCR_FLDS(ed_change_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE(chng_first_line)},
{LEN(0)	ROW(1)	COL(75)	ULINEVALUE("Change")},
{LEN(0) ROW(2)  COL(2)  VALUE("Old  =")},
{LEN(0) ROW(2)  COL(9)  USING(chng_field)},
{LEN(0) ROW(3)  COL(2)  VALUE("New  =")},
{LEN(0) ROW(3)  COL(9)  USING(repl_field)},

{LEN(0)	 ROW(4)	COL(2)	VALUE("Start =")},
{LEN(16) ROW(4)	COL(10)	USING(start_field)},
{LEN(0)	 ROW(4)	COL(29)	VALUE("End   =")},
{LEN(16) ROW(4)	COL(37)	USING(end_field)},

{LASTITEM}
};

static VSESCR_FLDS(ed_change_uplow_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE(chng_first_line)},
{LEN(0)	ROW(1)	COL(75)	ULINEVALUE("Change")},
{LEN(0) ROW(2)  COL(2)  VALUE("Old  =")},
{LEN(0) ROW(2)  COL(9)  UPLOW(chng_field)},
{LEN(0) ROW(3)  COL(2)  VALUE("New  =")},
{LEN(0) ROW(3)  COL(9)  UPLOW(repl_field)},

{LEN(0)	 ROW(4)	COL(2)	VALUE("Start =")},
{LEN(16) ROW(4)	COL(10)	USING(start_field)},
{LEN(0)	 ROW(4)	COL(29)	VALUE("End   =")},
{LEN(16) ROW(4)	COL(37)	USING(end_field)},

{LASTITEM}
};

static VSESCR_FLDS(ed_change_prompt_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Press (ENTER) to change this occurrence and continue, or select:")},
{LEN(0)	ROW(1)	COL(75)	ULINEVALUE("Change")},
{LEN(0) ROW(2)  COL(2)  VALUE("(1) Exit    (8) Find next occurrence   (10) Change remaining occurrences")},
{LEN(0) ROW(3)  COL(2)  VALUE("Old  =")},
{LEN(0) ROW(3)  COL(9)  VALUE(chng_field)},
{LEN(0) ROW(4)  COL(2)  VALUE("New  =")},
{LEN(0) ROW(4)  COL(9)  VALUE(repl_field)},
{LASTITEM}
};

static VSESCR_FLDS(chars_will_be_lost) = {
{LEN(0)	ROW(1) COL(2) VALUE("\224WARNING\204- Non-blank characters will be moved off end of line.")},
{LEN(0)	ROW(2) COL(2) VALUE("(ENTER) Change this occurrence   (1) Exit  (8) Find next occurrence")},
{LASTITEM}
};

/*
**	Static Function Prototypes
*/

static void vse_ed_fnd_val(void);
static int in_quotes(char *str);
static int4 is_line_number(char *str);
static void vse_ed_fnd_text(int backward);
static void fnd_on_screen(TEXT *txt, int col);
static int vse_ed_src_rpl(void);
static int replace_str(int col, int fndlen, TEXT *txt, char *repstr);
static void setup_search(char *srch_txt, int exact);
static int search(char *string, char *mask, int spos, int exact);
static int alldigits(char *str, int len);
static int case_exact(void);


void restart_find(void)
{
	NULL_FIELD(save_oa);
	screen_error = 0;
	backward = 0;
	fnd_number = 0;
	fnd_quoted = 0;
	fnd_txt = start_txt = NULL;
	fnd_row = 0;
	found = 0;
	fnd_start_col = 0;
	g_start_line = g_end_line = 0;
	CLEAR_STRING(fnd_field);
	CLEAR_STRING(emsg_field);
	CLEAR_STRING(chng_field);
	CLEAR_STRING(repl_field);
	CLEAR_STRING(start_field);
	CLEAR_STRING(end_field);
}

void vse_ed_find(void)
{
	found=0;

	fnd_row = get_line_and_index((int)(ed_oa[OA_CURSOR_ROW]),NULL);
	if (fnd_row < 0)
	{
		fnd_row = 0;
	}

	fnd_txt = NULL;
	CLEAR_FIELD(emsg_field);
	ed_oa[OA_CURSOR_ROW] = ed_oa[OA_COL] = 0;

	for(;vse_edit_pick!=1;)
	{
		screen_error = 0;

		vse_untrunc(fnd_field,vse_edit_width);

		vsescr_init(ed_scr);
		vse_ed_load_full_lines();
		strcpy(ed_pfs,"00010208X");
		vsescr(ed_line_flds,ed_scr);
		vsescr(ed_fnd_base_flds,ed_scr);
		if (mode_upper())	vsescr(ed_fnd_upper_flds,ed_scr);
		else			vsescr(ed_fnd_uplow_flds,ed_scr);
		vse_ed_add_numbers();

		d_and_r_ed(TAB_TO_FIELD);
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		
		switch(vse_edit_pick)
		{
		case 1:
			fnd_start_col = 0;
			break;
		case 2:
			vse_set();
			break;
		case 0:
		case 8:
			if (mode_upper())	vseunscr(ed_fnd_upper_flds,ed_scr);
			else			vseunscr(ed_fnd_uplow_flds,ed_scr);
			vse_ed_fnd_val();
			if(!screen_error)
			{
				vse_ed_fnd_text((int)(8==vse_edit_pick));
			}
			break;
		}
	}
}

static void vse_ed_fnd_val(void)
{
	int qt;

	fnd_quoted = fnd_number = 0;
	vse_trunc(fnd_field);
	if( (qt = in_quotes(fnd_field)) == -1)
	{
		screen_error = 1;
		strcpy(emsg_field,"Both quotes required");
		return;
	}
	if(qt == -2)
	{
		screen_error = 1;
		strcpy(emsg_field,"SORRY - Cannot search for an empty text string.");
		return;
	}
	if(qt == 1)
	{
		fnd_quoted = 1;
		return;
	}
	fnd_number = is_line_number(fnd_field);
}

/*----------------------------------------------------------------------*/

int vse_ed_change(void)
{
	int	rc;

	screen_error = 0;

	/*
	**	Find Starting line number
	*/
	fnd_row = get_line_and_index((int)(ed_oa[OA_CURSOR_ROW]),&g_start_line);
	if (fnd_row < 0)
	{
		g_start_line = 0;
		CLEAR_FIELD(start_field);
	}
	else
	{
		sprintf(start_field,"%06ld          ",(long)g_start_line);
	}

	ed_oa[OA_CURSOR_ROW] = ed_oa[OA_COL] = 0;


	for(;;)
	{
		if (screen_error)
		{
			strcpy(chng_first_line, emsg_field);
		}
		else
		{
			strcpy(chng_first_line, std_chng_first_line);
		}

		vse_untrunc(chng_field,vse_edit_width);
		vse_untrunc(repl_field,vse_edit_width);

		vsescr_init(ed_scr);
		vse_ed_load_full_lines();
		strcpy(ed_pfs,"000102X");
		vsescr(ed_line_flds,ed_scr);
		if (mode_upper())	vsescr(ed_change_flds,ed_scr);
		else			vsescr(ed_change_uplow_flds,ed_scr);
		vse_ed_add_numbers();
	
		d_and_r_ed(TAB_TO_FIELD);
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
		{
			return 0;
		}
		if(2 == vse_edit_pick)
		{
			vse_set();
			continue;
		}

		if (mode_upper())	vseunscr(ed_change_flds,ed_scr);
		else			vseunscr(ed_change_uplow_flds,ed_scr);

		screen_error = 0;

		vse_trunc(chng_field);
		vse_trunc(repl_field);
		if (0 == strlen(chng_field))
		{
			screen_error = 1;
			strcpy(emsg_field,"\224SORRY\204- Cannot search for an empty text string.");
		}

		if (!screen_error)
		{
			if ((rc = validate_range(start_field, &g_start_line, end_field, &g_end_line)))
			{
				strcpy(emsg_field,vse_err(rc));
				screen_error = 1;
			}
		}

		if(!screen_error)
		{
			if (0==vse_ed_src_rpl())
			{
				return 0;
			}
			screen_error = 1;
			strcpy(emsg_field,"\224SORRY\204 - The Old string was not found.");
		}
	}
}

/*----
Checks a string for enclosing quotes.
Returns 1 string is quoted
	0 string is not quoted
	-1 string has one quote but not the other error
	-2 string is zero length (quoted or not)
------*/
static int in_quotes(char *str)
{
	int oquote, cquote, len;

	len = strlen(str);
	if(0 == len)
		return(-2);

	oquote = *str;
	if( (oquote == '\'')  ||
	    (oquote == '\"')   )
		;
	else
		oquote = 0;

	str += len;
	--str;
	cquote = *str;
	if( (cquote == '\'') ||
	    (cquote == '\"')  )
		;
	else
		cquote = 0;
	if(!(oquote + cquote))		/* Not quoted */
		return(0);
	if(oquote != cquote)		/* Quotes aren't the same */
		return(-1);
	if(len < 3)			/* Quotes but not enough length */
		return(-2);
	return 1;
}

/*----
A line number is up to 6 consecutive digits
------*/
static int4 is_line_number(char *str)
{
	int len;
	int4 result;
	char	temp[20];

	if (fnd_quoted)
	{
		return atoi(str+1);
	}
	if((len = strlen(str)) > VSE_NUM_WIDTH)
		return(0);

	strcpy(temp,str);
	WL_upper_string(temp);
	if (0==strcmp(temp,"FIRST"))
	{
		return text_first->lineno;
	}
	else if (0==strcmp(temp,"LAST"))
	{
		return text_last->lineno;
	}

	result = 0;
	while(len--)
		{
		if(!isdigit((int)*str))
			return(0);
		result *= 10;
		result += ((*str) - '0');
		++str;
		}
	return(result);
}

static void vse_ed_fnd_text(int backward)
{
	TEXT *txt;
	int col, num;
	char srch_txt[81];
	char *orientation;
	int	exact;

	exact = case_exact();
	
	orientation = (backward) ? "first" : "last";
	
	if (fnd_txt && fnd_start_col)
		;
	if (fnd_txt)
	{
		if (fnd_start_col)
		{
			/*
			**	If we have a starting col then use the same fnd_txt again.
			*/
		}
		else
		{
			fnd_txt = (found)?(backward?fnd_txt->prev:fnd_txt->next):fnd_txt;
		}
	}
	else
	{
		fnd_txt = ed_txt[fnd_row];
	}

	if (fnd_txt == NULL)
	{ 
		if (found)
		  sprintf(emsg_field,"SORRY - The %s occurrence of text is already displayed.", orientation);
		else
		  strcpy(emsg_field,"SORRY - The specified text was not found.");
	}

	txt = fnd_txt;
	col = -1;
	num=0;
	memset(srch_txt,0,sizeof(srch_txt));
	if (fnd_quoted)
	  strncpy(srch_txt,fnd_field+1,strlen(fnd_field)-2);
	else
	  strcpy(srch_txt,fnd_field);

	if (999999 == fnd_number || fnd_number == text_last->lineno)
	{
		file_bottom();
		CLEAR_FIELD(emsg_field);
		vse_edit_pick=1;
		return;
	}
	else if (fnd_number && !fnd_quoted)
	{
		for (txt=text_first; txt;  txt=txt->next)
		{
		  if (fnd_number <= txt->lineno || !txt->next)
		  {
			  fnd_txt=txt;
			  col=0;
			  CLEAR_FIELD(emsg_field);
			  vse_edit_pick=1;
			  break;
		  }
		}
	}
	else
	{
 	    setup_search(srch_txt,exact);
	    while(txt)
	    {
		col = search(txt->text,srch_txt,fnd_start_col,exact);

		if(-1 != col)
		{
			fnd_start_col = col+strlen(srch_txt);
			fnd_txt = txt;
			++found;
			CLEAR_FIELD(emsg_field);
			break;
		}
		fnd_start_col = 0;
		txt = backward?txt->prev:txt->next;
		if ((!txt) || (-1==col))
		{
			if (found)
			  sprintf(emsg_field,"SORRY - The %s occurrence of text is already displayed.", orientation);
			else
			  strcpy(emsg_field,"SORRY - The specified text was not found.");
		}
		else
		{
			CLEAR_FIELD(emsg_field);
		}
	    }
	}
	
/*now position the screen */
	if (txt && (-1 != col))
	  fnd_on_screen(txt,col);
}

static void fnd_on_screen(TEXT *txt, int col)
{
	int idx;
	for(idx = 0; idx < VSE_EDIT_ROWS; ++idx)
		{
		if(ed_txt[idx] == txt)
			break;
		}
	if(idx < VSE_EDIT_ROWS)
		;
	else
		{
		scr_first = txt;
		idx = 0;
		}
	vse_save_row = ed_oa[OA_CURSOR_ROW] = idx + VSE_FIRST_SCREEN_ROW;
	col += VSE_FIRST_SCREEN_COL;
	vse_save_col = ed_oa[OA_COL] = col < VSE_SCREEN_WIDTH ? col : VSE_SCREEN_WIDTH;
}

static int vse_ed_src_rpl(void)
{
	TEXT 	*txt;
	int 	col,startpos,foundone;
	char 	srch_txt[81];
	int	exact;
	int	multi_line_change;
	int	do_all_replace;
	
	exact = case_exact();
	col = -1;
	foundone = 0;
	do_all_replace = 0;

	strcpy(srch_txt,chng_field);
	
	setup_search(srch_txt,exact);

	multi_line_change = (g_start_line != g_end_line);

	for(txt=text_first; txt; txt=txt->next)
	{
		if ( txt->lineno > g_end_line )
		{
			break;
		}

		if ( txt->lineno >= g_start_line )
		{
			startpos = 0;
			while ((col = search(txt->text,chng_field,startpos,exact)) != -1)
			{
				int	do_replace;

				foundone = 1;
				fnd_txt = txt;
				startpos = col+strlen(repl_field);

				if (multi_line_change && !do_all_replace)
				{
					fnd_on_screen(txt,col);
	
					screen_error=0;
					vsescr_init(ed_scr);
					vse_ed_load_full_lines();
					strcpy(ed_pfs,"00010810X");
					vsescr(ed_line_flds,ed_scr);
					vsescr(ed_change_prompt_flds,ed_scr);
					vse_ed_add_numbers();

					d_and_r_ed(TAB_TO_FIELD);
					int4_from_str2(ed_pfcode,&vse_edit_pick);
					switch(vse_edit_pick)
					{
					case 1:
						return 0;
					case 8:
						do_replace = 0;
						break;
					case 10:
						do_all_replace = 1;
						break;
					default:
						do_replace = 1;
					}
				}
				else
				{
					/*
					**	For a single line change all way do the replace.
					*/
					do_replace = 1;
				}

				if (do_replace || do_all_replace)
				{
					if (-1 == replace_str(col,strlen(chng_field),txt,repl_field))
					{
						return 0;
					}
				}
			}
		}
	}		
 
	if(!foundone)
	{
		return 1;
	}

	return 0;
}

static int replace_str(int col, int fndlen, TEXT *txt, char *repstr)
{
	int maxlen, newlen, replen;
	char *buf;
	char *text;

	text = txt->text;
	replen = strlen(repstr);
	newlen = strlen(text) - fndlen + replen;

	maxlen = vse_edit_width;

	if (newlen > maxlen)
	{
		fnd_on_screen(txt,col);
	
		screen_error=0;
		vsescr_init(ed_scr);
		vse_ed_load_full_lines();
		strcpy(ed_pfs,"000108X");
		vsescr(ed_line_flds,ed_scr);
		vsescr(chars_will_be_lost,ed_scr);
		vse_ed_add_numbers();

		d_and_r_ed(TAB_TO_FIELD);
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
			return -1;
		else if(8 == vse_edit_pick)
			return 0;

		newlen = maxlen;
		if (replen+col > maxlen)
		{
			replen = maxlen-col;
		}
	}

	vse_file_changed=1;

	buf=malloc(newlen+1);
	memcpy(buf,text,col);
	memcpy(&buf[col],repstr,replen);
	memcpy(&buf[col+replen],&text[col+fndlen],newlen - col - replen);
	buf[newlen] = (char)0;
	free(text);
	txt->text=buf;

	add_modcode(txt);

	return 0;
}

static char srch_ch_map[256];
static int srch_len;

static void setup_search(char *srch_txt, int exact)
{
	char *p;
	
	memset(srch_ch_map,0,sizeof(srch_ch_map));
	srch_len=0;
	p = srch_txt;				/* Point to the search text.		*/
	while (*p != CHAR_NULL)				/* Now convert to upper case.		*/
	{
		if (!isalpha((int)*p))
		{
			srch_ch_map[(int)*p]=TRUE;
		}
		else
		{
			if (exact)
			{
				srch_ch_map[(int)*p]=TRUE;
			}
			else
			{
				srch_ch_map[tolower(*p)]=TRUE;
				srch_ch_map[toupper(*p)]=TRUE;
			}
		}
		++srch_len;
		++p;
	}
}

static int search(char *string, char *mask, int spos, int exact)
{
	int m_idx, s_idx, cmpval;
	int m_len, s_len;
	int save;

	m_len = srch_len - 1;
	s_len = strlen(string);
	save = s_idx = spos + m_len;
	m_idx = m_len;
	while (s_idx < s_len)
	{
		if (exact)
		{
			while ( m_idx >= 0 )
			{
				/*
				**	XOR to compare.
				**		0x00 = exact match
				*/
				if ( mask[m_idx] ^ string[s_idx] ) break;
				--m_idx; --s_idx;
			}
		}
		else
		{
			while ( m_idx >= 0 )
			{
				/*
				**	XOR to compare.
				**		0x00 = exact match
				**		0x20 = Upper/Lower case difference
				**			(could also be random)
				*/
				cmpval =  mask[m_idx] ^ string[s_idx];

				if (cmpval) /* cmpval != 0x00 (not exact match) */
				{
					/*
					**	If the cmpval is not 0x20 or the char is not alpha then not a match.
					*/
					if (cmpval != 0x20) break;
					if (!isalpha((unsigned int)mask[m_idx])) break;
				}
				--m_idx; --s_idx;
			}
		}

		if (m_idx < 0) 
		{
			return s_idx + 1;
		}
		else
		{
			while (srch_ch_map[(unsigned)string[s_idx]] && (save-s_idx < m_len)) --s_idx;
			save = s_idx += m_len + 1;
			m_idx = m_len;
		}
	}
	return -1;
}

/*
**	Routine:	validate_range()
**
**	Function:	Validates the Start - End line number range.
**
**	Description:	Convert the start and end into line numbers
**			and validate that start <= end.  If no end is
**			given then end_line will be set equal to start.
**			Accepts keywords FIRST, LAST, and ALL.
**			If there is text then FIRST, LAST and ALL will
**			return actual line numbers otherwise it will
**			return 1 and 999999 for first and last.
**
**	Arguments:
**	start_field	The user entered START
**	start_line	The returned starting line number.
**	end_field	The user entered END
**	end_line	The returned ending line number.
**
**	Globals:
**	text_first
**	text_last
**
**	Return:
**	0		Range is valid
**	RESP_START	Bad start
**	RESP_END	Bad end
**	RESP_RANGE	Bad range
**	RESP_EMPTY	Range is empty (Only if there is text)
**
**	Warnings:	None
**
**	History:	
**	03/08/94	Written by GSL
**
*/
int validate_range(char *start_field, int4 *start_line, char *end_field, int4 *end_line)
{

	if (validate_linenum(start_field,start_line))
	{
		return RESP_START;
	}

	if (validate_linenum(end_field,end_line))
	{
		return RESP_END;
	}

	switch(*start_line)
	{
	case -1:/* blank */
		return RESP_START;

	case -2: /* ALL */
		if (-1 != *end_line)
		{
			return RESP_END;
		}

		*start_line = 1;
		*end_line = 999999;
		return 0;

	case -3: /* FIRST */
		*start_line = 1;
		break;

	case -4: /* LAST */
		*start_line = 999999;
		break;
	}


	switch(*end_line)
	{
	case -1: /* blank */
		*end_line = *start_line;
		break;

	case -2: /* ALL */
		return RESP_END;

	case -3: /* FIRST */
		*end_line = 1;
		break;

	case -4: /* LAST */
		*end_line = 999999;
		break;
	}

	if (*start_line > *end_line)
	{
		return RESP_RANGE;
	}

	return 0;
}

#ifdef OLD
int get_txt_range(start_lineno, start_txt, end_lineno, end_txt)
int4	start_lineno, end_lineno;
TEXT	**start_txt, **end_txt;
{
	TEXT	*txt;

	*start_txt = *end_txt = NULL;

	if (1==start_lineno && 999999==end_lineno && text_first)
	{
		*start_txt = text_first;
		*end_txt = text_last;
		return 0;
	}

	for(txt=text_first; txt; txt=txt->next)
	{
		if (txt->lineno > end_lineno)
		{
			break;
		}

		if (!*start_txt && txt->lineno >= start_lineno)
		{
			*start_txt = txt;
		}

		*end_txt = txt;
	}

	if (! *start_txt)
	{
		return RESP_EMPTY;
	}

	return 0;
}
#endif /* OLD */

/*
**	Routine:	validate_linenum()
**
**	Function:	Validates a user entered line number.
**
**	Description:	Converts the user entered string into a line number.
**			Valid range is 0 - 999999 plus following special values.
**			(blank)		-1
**			ALL		-2
**			FIRST		-3
**			LAST		-4
**
**	Arguments:
**	num_string	The user entered line number.
**	num		The returned line number.
**
**	Globals:	None
**
**	Return:
**	0		Number is valid
**	1		Invalid
**
**	Warnings:	None
**
**	History:	
**	03/09/94	Written by GSL
**
*/
int validate_linenum(char *num_string, int4 *num)
{
	char	temp[17];

	memcpy(temp,num_string,16);
	temp[16] = (char)0;

	leftjust(temp,16);
	vse_trunc(temp);

	if ( 0==strlen(temp) )
	{
		*num = -1;
		return 0;
	}

	if ( 0==strcmp(temp,"ALL") )
	{
		*num = -2;
		return 0;
	}

	if ( 0==strcmp(temp,"FIRST") )
	{
		*num = -3;
		return 0;
	}

	if ( 0==strcmp(temp,"LAST") )
	{
		*num = -4;
		return 0;
	}

	if (!alldigits(temp, strlen(temp)))
	{
		return 1;
	}

	*num = (int4) atol(temp);

	if (*num < 0 || *num > 999999)
	{
		return 1;
	}

	return 0;
}

static int alldigits(char *str, int len)
{
	while(len--)
	{
		if (!isdigit((int)*str)) return 0;
		str++;
	}
	return 1;
}



/*
**	Routine:	get_line_and_index()
**
**	Function:	Retreives current positions in the file and on the screen.
**
**	Description:	Returns the line number and index into ed_txt of the current
**			record.  The current record is the one the cursor in on.  If
**			the cursor is before the first or after the last record
**			displayed then the current is the first or last displayed.
**
**	Arguments:
**	row		The screen row (ed_oa[OA_CURSOR_ROW])
**	line		The returned line number of the current record. (If not NULL.)
**
**	Globals:
**	ed_oa		The edit screen order area
**	ed_txt		The edit screen text array.
**
**	Return:		
**	0-19		The index into ed_txt of the current record.
**	-1		Error
**
**	Warnings:	None
**
**	History:	
**	03/09/93	Written by GSL
**
*/
int get_line_and_index(int row, int4 *line)
{
	int	index;

	if (!text_first)
	{
		if (line)
		{
			*line = 0;
		}
		return -1;
	}

	index = row; /* ed_oa[OA_CURSOR_ROW];	*/
	if(index < VSE_FIRST_SCREEN_ROW)
	{
		index = VSE_FIRST_SCREEN_ROW;
	}
	index -= VSE_FIRST_SCREEN_ROW;
	while(index > -1)
	{
		/*
		**	Search backwards for a record.
		*/
		if(ed_txt[index])
		{
			if (line != NULL)
			{
				*line = ed_txt[index]->lineno;
			}
			return index;
		}
		index--;
	}

	return -1;
}

static int case_exact(void)
{
	return (0==strcmp(vse_gp_defaults_case,"EXACT")) ? 1 : 0;
}

/*
**	History:
**	$Log: vsedfnd.c,v $
**	Revision 1.16  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.15  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.14  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.13  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.12  2002/07/11 14:34:00  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.11  1996/07/18 20:48:44  gsl
**	fix for NT
**	
**	Revision 1.10  1995-04-25 02:59:18-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.9  1995/04/25  09:25:54  gsl
 * fix pfkey tags
 *
 * Revision 1.8  1995/04/17  11:51:38  gsl
 * drcs state V3_3_14
 *
 * Revision 1.7  1995/04/10  08:53:13  gsl
 * fix some compiler warnings and add standard headres
 * thats headers.
 *
**
**
*/
