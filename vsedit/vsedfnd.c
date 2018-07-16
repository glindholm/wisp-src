#include <stdio.h>
#include <ctype.h>

#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "idsistd.h"

#define CHAR_NULL (char)0
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

static int backward;

static int4 fnd_number, fnd_quoted;
static TEXT *fnd_txt, *start_txt;
static int4 fnd_row;

static int4 found;

static char fnd_field[81],emsg_field[81];

static char srch_field[81],repl_field[81], start_field[7],end_field[7];


static VSESCR_FLDS(ed_fnd_flds) = {
{LEN(72)	ROW(3)	COL(7)	USING(fnd_field)},
{LEN(0)	ROW(1)	COL(2)	VALUE("Enter the text to find and press ENTER or")},
{LEN(0)	ROW(2)	COL(2)	VALUE("(1) Exit  (2) Set Tabs (8) Search Backwards")},
{LEN(0)	ROW(3)	COL(2)	VALUE("Find")},
{LEN(0)	ROW(4)	COL(2)	BRIGHT(emsg_field)},
{LASTITEM}
};
static VSESCR_FLDS(ed_replace_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("(ENTER) Change text in Range  (1) Exit  (2) Set tabs or Case options")},
{LEN(0) ROW(2)  COL(2)  VALUE("Old = ")},
{LEN(70) ROW(2)  COL(8)  UPLOW(fnd_field)},
{LEN(0) ROW(3)  COL(2)  VALUE("New = ")},
{LEN(70) ROW(3)  COL(8)  UPLOW(repl_field)},
{LEN(0) ROW(4) COL(2) VALUE("Start = ")},
{LEN(6) ROW(4) COL(10) USING(start_field)},
{LEN(0) ROW(4) COL(20) VALUE("End = ")},
{LEN(6) ROW(4) COL(26) USING(end_field)},
{LASTITEM}
};
static VSESCR_FLDS(chars_will_be_lost) = {
{LEN(0)	ROW(1) COL(2) VALUE("Warning - Non-blank characters will be moved off end of line.")},
{LEN(0)	ROW(2) COL(2) VALUE("(Enter) Change this occurance   (1) Exit  (8) Find next occurance")},
{LASTITEM}
};

static int setup_search();
static int search();
static char save_oa[4];
static screen_error;

vse_ed_fnd()
{
	fnd_row = ed_oa[3];
	found=0;
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
	for(;vse_edit_pick!=1;)
	{
		screen_error = 0;
		vse_ed_fnd_init();
		d_and_r_ed(TAB_TO_FIELD);
/*		strcpy(emsg_field,"");*/
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
		  break;
		vseunscr(ed_fnd_flds,ed_scr);
		vse_ed_fnd_val();
		if(!screen_error)
		{
			vse_ed_fnd_dispatch();
		}
	}
}
vse_ed_srch_repl()
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
	screen_error=0;
	vsescr_init(ed_scr);
	vse_ed_load_full_lines();
	strcpy(ed_pfs,"000102X");
	ed_fnd_flds[0].fac = language_case();
	vsescr(ed_line_flds,ed_scr);
	CLEAR_FIELD(fnd_field);
	CLEAR_FIELD(repl_field);
	CLEAR_FIELD(start_field);
	CLEAR_FIELD(end_field);
	vsescr(ed_replace_flds,ed_scr);
	vse_ed_add_numbers();
	vse_ed_add_mod();
	
	d_and_r_ed(TAB_TO_FIELD);
	int4_from_str2(ed_pfcode,&vse_edit_pick);
	if(1 == vse_edit_pick)
	  return;
	vseunscr(ed_replace_flds,ed_scr);
	vse_ed_fnd_val();
	trunc(repl_field);
	if(!screen_error)
	{
		vse_ed_src_rpl();
	}
}
init_fnd_fields()
{
	untrunc(fnd_field,80);
	fnd_txt = NULL;
	CLEAR_FIELD(emsg_field);
}

vse_ed_fnd_val()
{
	int qt;
	int4 is_line_number();

	fnd_quoted = fnd_number = 0;
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
	return 1;
}

/*----
A line number is up to 6 consecutive digits
------*/
int4 is_line_number(str)
char *str;
{
	int len;
	int4 result;

	if (fnd_quoted)
	{
		return atoi(str+1);
	}
	if((len = strlen(str)) > 6)
		return(0);
	result = 0;
	while(len--)
		{
		if(!isdigit(*str))
			return(0);
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
		vse_set();
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
	int col, num;
	FILE *x,*fopen();
	char srch_txt[81];
	
	if(fnd_txt != NULL)
		fnd_txt = (found)?(backward?fnd_txt->prev:fnd_txt->next):fnd_txt;
	else
		fnd_txt = ed_txt[fnd_row];

	if (fnd_txt == NULL)
	{ 
		if (found)
		  strcpy(emsg_field,"Sorry - The last occurrence of text is already displayed.");
		else
		  strcpy(emsg_field,"Sorry - The specified text was not found.");
	}

	txt = fnd_txt;
	col = -1;
	num=0;
	memset(srch_txt,0,sizeof(srch_txt));
	if (fnd_quoted)
	  strncpy(srch_txt,fnd_field+1,strlen(fnd_field)-2);
	else
	  strcpy(srch_txt,fnd_field);

	setup_search(srch_txt);
	if (fnd_number && !fnd_quoted)
	{
		for (txt=text_first; txt;  txt=txt->next)
		  if (fnd_number==txt->lineno)
		  {
			  fnd_txt=txt;
			  col=7;
			  CLEAR_FIELD(emsg_field);
			  vse_edit_pick=1;
			  break;
		  }
	}
	else
	while(txt)
	{
		col = search(txt->text,srch_txt,0);

		if(-1 != col)
		{
			fnd_txt = txt;
			++found;
			CLEAR_FIELD(emsg_field);
			break;
		}
		txt = backward?txt->prev:txt->next;
		if ((!txt) || (-1==col))
		{
			if (found)
			  strcpy(emsg_field,"Sorry - The last occurrence of text is already displayed.");
			else
			  strcpy(emsg_field,"Sorry - The specified text was not found.");
		}
		else
		{
			CLEAR_FIELD(emsg_field);
		}
	}
	
/*now position the screen */
	if (txt && (-1 != col))
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
	vse_save_row = ed_oa[3] = idx + 5;
/*	if(vse_numbering)*/
		col += 9;
/*	else
		col += 2;*/
	vse_save_col = ed_oa[2] = col < 80 ? col : 80;
}


does_number_match(txt,lineno)
TEXT *txt;
int4 lineno;
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
	vse_ed_load_full_lines();
	strcpy(ed_pfs,"00010208X");
	untrunc(fnd_field,80);
	ed_fnd_flds[0].fac = language_case();
	vsescr(ed_line_flds,ed_scr);
	vsescr(ed_fnd_flds,ed_scr);
	vse_ed_add_numbers();
	vse_ed_add_mod();
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

vse_ed_src_rpl()
{
	TEXT *txt;
	int col,repl,startpos;
	char srch_txt[81];
	
	memset(srch_txt,0,sizeof(srch_txt));
	if (fnd_quoted)
	  strncpy(srch_txt,fnd_field+1,strlen(fnd_field)-2);
	else
	  strcpy(srch_txt,fnd_field);
	
	setup_search(srch_txt);
	col = -1;
	txt = ed_txt[fnd_row];
	
	while(txt)
	{
		startpos = 0;
		while ((col = search(txt->text,fnd_field,startpos)) != -1 && repl != -1)
		{
			startpos = col+strlen(repl_field);
			repl=replace_str(col,strlen(fnd_field),txt,repl_field);
			fnd_txt = txt;
		}
		txt=txt->next;
	}		
 
	if(-1 == col)
	{
		strcpy(emsg_field,"Text not found");
		return;
	}
}
replace_str(col,fndlen,txt,repstr)
int col,fndlen;
TEXT  *txt, *repstr;
{
	int maxlen, newlen;
	char *buf;
	char *text;

	text = txt->text;
	
	newlen= strlen(text)-fndlen+strlen(repstr);
	maxlen = vse_numbering?73:72;

	if (newlen > maxlen)
	{
		screen_error=0;
		vsescr_init(ed_scr);
		vse_ed_load_full_lines();
		strcpy(ed_pfs,"000108X");
		vsescr(ed_line_flds,ed_scr);
		vsescr(chars_will_be_lost,ed_scr);
		vse_ed_add_numbers();
		vse_ed_add_mod();

		fnd_on_screen(txt,col);
	
		d_and_r_ed(TAB_TO_FIELD);
		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
		  return -1;
		else if(8 == vse_edit_pick)
		  return 0;
	}
	vse_file_changed=1;
	buf=calloc(newlen+1,1);
	memcpy(buf,text,col);
	strcat(buf,repstr);
	memcpy(buf+col+strlen(repstr),text+col+fndlen,newlen-col-fndlen);
	free(text);
	txt->text=buf;
	return 0;
}
static char srch_ch_map[256];
static int srch_len;

static int setup_search(srch_txt)	
char *srch_txt;
{
	char *p;
	
	memset(srch_ch_map,0,sizeof(srch_ch_map));
	srch_len=0;
	p = srch_txt;				/* Point to the search text.		*/
	while (*p != CHAR_NULL)				/* Now convert to upper case.		*/
	{
		if (!isalpha(*p))
		{
			srch_ch_map[*p]=TRUE;
		}
		else
		{
			srch_ch_map[tolower(*p)]=TRUE;
			srch_ch_map[toupper(*p)]=TRUE;
		}
		++srch_len;
		++p;
	}
}
static int search(string,mask,spos)	
register unsigned char *mask, *string;
int spos;
{
	register m_idx, s_idx, cmpval;
	register int m_len, s_len;
	int save;

	m_len = srch_len - 1;
	s_len = strlen((char *)string);
	save = s_idx = spos + m_len;
	m_idx = m_len;
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

