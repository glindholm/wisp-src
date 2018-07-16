#include <stdio.h>
#include <ctype.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"

static long fnd_number, fnd_quoted;
static TEXT *fnd_txt, *start_txt;
static long fnd_row;

static char fnd_field[81],emsg_field[81];

static VSESCR_FLDS(ed_fnd_flds) = {
{LEN(72)	ROW(3)	COL(7)	USING(fnd_field)},
{LEN(0)	ROW(1)	COL(2)	VALUE("Enter the text to find and press ENTER or")},
{LEN(0)	ROW(2)	COL(2)	VALUE("(1) Exit  (2) Set Tabs (8) Search Backwards")},
{LEN(0)	ROW(3)	COL(2)	VALUE("Find")},
{LEN(0)	ROW(4)	COL(2)	FROM(emsg_field)},
{LASTITEM}
};

static char save_oa[4];
static screen_error;

vse_ed_fnd()
{
	fnd_row = ed_oa[3];
	
	if(fnd_row < 5)
		fnd_row = 5;
	fnd_row -= 5;
	while(fnd_row > -1)
		{
		if(ed_txt[fnd_row])
			break;
		--fnd_row;
		}
	init_fnd_fields();
	ed_oa[3] = ed_oa[2] = 0;
	for(;;)
	{
	screen_error = 0;
	vse_ed_fnd_init();
	d_and_r_ed();
	strcpy(emsg_field,"");
	long_from_str2(ed_pfcode,&vse_edit_pick);
	if(1L == vse_edit_pick)
		break;
	vseunscr(ed_fnd_flds,ed_scr);
	vse_ed_fnd_val();
	if(!screen_error)
		{
		vse_ed_fnd_dispatch();
		}
	}
}

init_fnd_fields()
{
	untrunc(fnd_field,80);
	fnd_txt = NULL;
}

vse_ed_fnd_val()
{
	int qt;
	long is_line_number();

	fnd_quoted = fnd_number = 0L;
	trunc(fnd_field);
	if( (qt = in_quotes(fnd_field)) == -1)
		{
		screen_error = 1;
		strcpy(emsg_field,"Both quotes required");
		return;
		}
	if(qt == -2)
		{
		screen_error = 1;
		strcpy(emsg_field,"Enter a search text");
		return;
		}
	if(qt == 1)
		{
		fnd_quoted = 1;
		return;
		}
	fnd_number = is_line_number(fnd_field);
}

/*----
Checks a string for enclosing quotes.
Returns 1 string is quoted
	0 string is not quoted
	-1 string has one quote but not the other error
	-2 string is zero length (quoted or not)
------*/
in_quotes(str)
char *str;
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
}

/*----
A line number is up to 6 consecutive digits
------*/
long is_line_number(str)
char *str;
{
	int len;
	long result;

	if((len = strlen(str)) > 6)
		return(0L);
	result = 0;
	while(len--)
		{
		if(!isdigit(*str))
			return(0L);
		result *= 10;
		result += ((*str) - '0');
		++str;
		}
	return(result);
}

vse_ed_fnd_dispatch()
{
	switch(vse_edit_pick)
		{
		case 2:
			vse_naf();
			break;
		case 8:
			vse_ed_fnd_text(1);
			break;
		case 0:
			vse_ed_fnd_text(0);
			break;
		}
}

vse_ed_fnd_text(backward)
int backward;
{
	TEXT *txt;
	int col;

	if(fnd_txt != NULL)
		fnd_txt = backward?fnd_txt->prev:fnd_txt->next;
	else
		fnd_txt = ed_txt[fnd_row];

	txt = fnd_txt;
	col = -1;
	while(txt)
		{
		if(fnd_number)
			col = does_number_match(txt,fnd_number);
		else
			col = does_text_match(txt->text,fnd_field);
		if(-1 != col)
			{
			fnd_txt = txt;
			break;
			}
		txt = backward?txt->prev:txt->next;
		}
	if(-1 == col)
		{
		strcpy(emsg_field,"Text not found");
		return;
		}
/*now position the screen */
	fnd_on_screen(txt,col);
}

fnd_on_screen(txt,col)
TEXT *txt;
int col;
{
	int idx;
	for(idx = 0; idx < 20; ++idx)
		{
		if(ed_txt[idx] == txt)
			break;
		}
	if(idx < 20)
		;
	else
		{
		scr_first = txt;
		idx = 0;
		}
	ed_oa[3] = idx + 5;
	if(vse_numbering)
		col += 9;
	else
		col += 2;
	ed_oa[2] = col;
}


does_number_match(txt,lineno)
TEXT *txt;
long lineno;
{
	if(txt->lineno == lineno)
		return(0);
	return(-1);
}

does_text_match(str1,str2)
char *str1,*str2;
{
	char buf1[81],buf2[81];
	char *strstr(),*ptr;
	int len;

	strcpy(buf2,str2);
	str2 = buf2;
	if(!strcmp(vse_gp_defaults_case,"ANY  "))
		{
		strcpy(buf1,str1);
		strupr(buf1);
		str1 = buf1;
		strupr(buf2);
		}
	if(fnd_quoted)
		{
		strcpy(buf2,&buf2[1]);
		len = strlen(buf2);
		--len;
		buf2[len] = 0;
		}
		
	ptr = strstr(str1,str2);
	if(!ptr)
		return(-1);
	else
		return(ptr - str1);
}

vse_ed_fnd_init()
{

	vsescr_init(ed_scr);

/* 
   This routine from the main screen is only used to get the
   text correctly loaded
*/
	init_vse_ed_lines();
	strcpy(ed_pfs,"00010208X");
	untrunc(fnd_field);
	ed_fnd_flds[0].fac = language_case();
	vsescr(ed_line_flds,ed_scr);
	vsescr(ed_fnd_flds,ed_scr);
	vse_ed_add_numbers();
}

strupr(str)
char *str;
{
	while(*str)
		{
		if(isalpha(*str))
			*str = toupper(*str);
		++str;
		}
}

