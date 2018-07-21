/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/


#include <ctype.h>
#include <string.h>
#include <assert.h>

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"

static void compress_name(char *name, unsigned int len);


int strpos(const char* src, const char* srch)
{
	const char* ptr;
	int	len;

	ptr = src;
	len = strlen(srch);

	for(;;)
	{
		ptr = strchr(ptr,srch[0]);
		if (!ptr) return(-1);

		if (0==memcmp(ptr,srch,len)) return(ptr-src);
		ptr++;
	}
}

	
/* Edit a source line, find the search string and replace it with *repl	*/
int stredt(char* src, const char* srch, const char* repl)
{
	int i;
	char tstring[512];
	char *sptr;

	if ( (i = strpos(src,srch)) != -1)						/* It's ok to go ahead			*/
	{
		src += i;								/* point to location of search string	*/
		i = strlen(srch);
		sptr = src + i;								/* Skip over the search string . . .	*/
		strcpy(tstring,sptr);							/* copy end of line into temp string	*/
		strcpy(src,repl);							/* put replacement string in place	*/
		strcat(src,tstring);							/* put end line back into source	*/
		i = 0;									/* edit was ok				*/
	}
	return(i);									/* return the value 0 or -1		*/
}

char* make_fac(char* fac, const char* src)		/* take a source variable name and make a FAC variable name		*/
{
	return gen_data_name(fac, FAC_OF_PREFIX, src, NULL);
}

char*  make_oa(char* oa, const char* src)		/* take a source variable name and make an ORDER-AREA variable name.	*/
{
	return gen_data_name(oa, ORDER_AREA_PREFIX, src, NULL);
}

#define MAX_COBOL_WORD_LEN	30

/* take a source variable name and prefix and make a new variable name.		*/
char* make_fld(char *dst, const char* src, const char* prfx)	
{
	return gen_data_name(dst, prfx, src, NULL);
}

char* gen_data_name(char* dest, const char* prefix, const char* base, const char* suffix)
{
	static int depth = 0;	/* recursive depth */
	const char* l_prefix = "";
	const char* l_suffix = "";
	char	l_base[80];
	char	temp[80];
	
	depth++;

	if (prefix)
	{
		l_prefix = prefix;
	}
	if (suffix)
	{
		l_suffix = suffix;
	}

	strcpy(l_base, base);
	compress_name(l_base, MAX_COBOL_WORD_LEN - strlen(l_prefix) - strlen(l_suffix));

	sprintf(temp,"%s%s%s", l_prefix, l_base, l_suffix);

#ifdef OLD
	if (strlen(temp) > MAX_COBOL_WORD_LEN)
	{
		int	max_base_len;
		int	len;
		int	i;
		char	l_base[MAX_COBOL_WORD_LEN+1];

		strncpy(l_base,base,MAX_COBOL_WORD_LEN);
		l_base[MAX_COBOL_WORD_LEN] = '\0';
		
		max_base_len = MAX_COBOL_WORD_LEN - strlen(l_prefix) - strlen(l_suffix);

		/*
		**	Start by removing hyphens (from the end)
		*/
		len = strlen(l_base);
		for(i=len-1; len > max_base_len && i >= 0; i--)
		{
			if ( '-' == l_base[i] )
			{
				stredt(&l_base[i],"-","");
				len--;
			}
		}

		/*
		**	Next remove vowels (from the end)
		*/
		for(i=len-1; len > max_base_len && i >= 0; i--)
		{
			if (strchr("AEIOU",l_base[i]))
			{
				l_base[i] = '*';
				len--;
			}
		}
		while(0 == stredt(l_base,"*",""))
		{
			/* loop removing "*" */
		}

		/*
		**	Next truncate
		*/
		if ((int)strlen(l_base) > max_base_len)
		{
			l_base[max_base_len] = '\0';
		}
		
		sprintf(temp, "%s%s%s", l_prefix, l_base, l_suffix);

		assert(strlen(temp) <= MAX_COBOL_WORD_LEN);

		write_log("WISP",'I',"GENNAME", "Generated data name [%s] has been compress from [%s%s%s]",
			  temp, l_prefix, base, l_suffix);
	}
#endif /* OLD */

	/*
	**	Ensure generated name doesn't conflict with a user symbol.
	*/
	if (is_symbol(temp))
	{
		if (depth > 1)
		{
			write_log("WISP",'E',"GENNAME", "Error Generated Unique data name [%s] from [%s%s%s]",
				  temp, l_prefix, base, l_suffix);
		}
		else
		{
			/*
			**	Make Unique with "-Q"
			*/
			gen_data_name(temp, l_prefix, &temp[strlen(l_prefix)], "-Q");
			write_log("WISP",'I',"GENNAME", "Generated data name [%s] has been munged to not conflict with symbol [%s%s%s]",
				  temp, l_prefix, base, l_suffix);
		}
	}

	strcpy(dest,temp);
	
	depth--;

	return dest;
}


char* sp_trunc(char* text)							/* Truncate a string at the first space		*/
{
	int i;
	i = strpos(text," ");							/* find the space				*/
	if ( i != -1 ) text[i] = '\0';						/* replace with a null				*/
	return text;
}

int uppercase(char* str)								/* Shift string to uppercase		*/
{
	for(;*str;str++)  *str = toupper(*str);
	return 0;
}

int lowercase(char* str)								/* Shift string to uppercase		*/
{
	for(;*str;str++)  *str = tolower(*str);
	return 0;
}

int paracmp(const char *str, char strtab[MAX_PARAGRAPHS][40], int tabcnt)
{
	int	i;
	for(i=0; i<tabcnt; i++)
	{
		if (0==strcmp(str,strtab[i])) return(1);
	}
	return(0);
}

int instrlist(const char *test_str, char* strlist[], int list_cnt)
{
	int	i;
	for(i=0; i<list_cnt; i++)
	{
		if (0==strcmp(test_str, strlist[i])) return(1);
	}
	return(0);
}

int noncase_eq(const char* str1, const char *str2)
{
	while(*str1 && *str2 && toupper(*str1)==toupper(*str2))
	{
		str1++;
		str2++;
	}

	if (*str1 || *str2) return(0);
	return(1);
}

int strchrcnt(char *string,char chr)
{
	int	cnt;

	for(cnt=0; (string = strchr(string,chr)); cnt++) {string++;}

	return( cnt );
}

/*
**	Compress a cobol user defined name to len characters by
**	remove "-" characters then truncating
*/
static void compress_name(char *name, unsigned int len)
{
	int shrink = strlen(name) - len;	/* Number of characters to be removed */

	if (shrink > 0)
	{
		char *ptr;
		
		/* Remove "-" from the end */
		while (shrink>0 && (ptr = strrchr(name, '-')) != NULL)
		{
			stredt(ptr, "-", "");
			shrink--;
		}

		name[len] = '\0';	/* Truncate the rest */
	}
}

/*
**	Use make_prog_name() for all the get_prog_xxx() routines.
**	This will truncate the results required length.
**	Don't use gen_data_name() because these are required 
**	before working-storage and the value gen_data_name() generates
**	may be different after working-storage is loaded into symbol table.
*/
static char *make_prog_name(char *the_name, int fnum, const char* prefix)
{
	char fname[80];
	
	strcpy(fname,prog_files[fnum]);

	if (strlen(fname)+strlen(prefix) > MAX_COBOL_WORD_LEN)
	{
		/* Shrink fname */
		compress_name(fname, MAX_COBOL_WORD_LEN - strlen(prefix));
	}

	strcpy(the_name,prefix);
	strcat(the_name,fname);

	return the_name;
}

const char *get_prog_vname(int fnum)
{
	static char the_name[40];
	
	if (is_literal(prog_vnames[fnum]) || (!prog_vnames[fnum][0]))
	{
		return make_prog_name(the_name, fnum, "V-");
	}
	else
	{
		return prog_vnames[fnum];
	}

}

const char *get_prog_lname(int fnum)
{
	static char the_name[40];
	
	if (is_literal(prog_lnames[fnum]) || (!prog_lnames[fnum][0]))
	{
		return make_prog_name(the_name, fnum, "L-");
	}
	else
	{
		return prog_lnames[fnum];
	}

}

const char *get_prog_fname(int fnum)
{
	static char the_name[40];
	
	if (is_literal(prog_fnames[fnum]) || (!prog_fnames[fnum][0]))
	{
		return make_prog_name(the_name, fnum, "F-");
	}
	else
	{
		return prog_fnames[fnum];
	}

}

const char *get_prog_nname(int fnum)
{
	static char the_name[40];

	return make_prog_name(the_name, fnum, "N-");
}

const char *get_prog_prname(int fnum)
{
	static char the_name[40];

	return make_prog_name(the_name, fnum, "PR-");
}

const char *get_prog_status(int fnum)
{
	static char the_name[40];

	/* Status is an Attribute */
	return make_prog_name(the_name, fnum, "ATTR-");
	/* return make_prog_name(the_name, fnum, "S-"); */
}

void safemove(char *dest, const char *src, int len)				/* safemove (memcpy) handles overlapping move	*/
{
	if (dest > src)								/* will overlap so copy backwards from end      */
	{
		for(;len>0;--len)
		{
			dest[len-1] = src[len-1];				/* copy bytes offset len-1 through zero         */
		}
	}
	else
	{
		for(;len>0;--len)						/* copy "frontwards" since no overlap will occur*/
		{
			*dest++ = *src++;
		}
	}
}

int is_quote(char data)
{
	if (DOUBLE_QUOTE == data || data == SINGLE_QUOTE)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

int is_literal(const char* literal)
{
	unsigned int len = strlen(literal);

	if (len >= 2 && is_quote(literal[0]))
	{
		if (literal[0] == literal[len-1]) /* Check that quotes match */
		{
			return 1;
		}
	}
	return 0;
}

/*
**	Routine:	remove_quotes()
**
**	Function:	Remove the quotes from a literal
**
**	Description:	Remove the leading and trailing quotes if present.
**			Either single or duoble quotes can be used.
**			Quotes must be matched.
**
**	Arguments:	The literal
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**
*/
char *remove_quotes(char *literal)
{
	unsigned int len = strlen(literal);

	if (len >= 2 && is_quote(literal[0]))
	{
		if (literal[0] == literal[len-1]) /* Check that quotes match */
		{
			literal[len-1] = '\0'; /* truncate off the trailing quote */
			safemove(literal, &literal[1], len-1); 
		}
	}

	return literal;
}

/*
**	History:
**	$Log: wt_utils.c,v $
**	Revision 1.25  2003/03/06 21:51:09  gsl
**	Change to use ATTR instead of MODE
**	
**	Revision 1.24  2003/03/03 22:08:39  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.23  2003/02/04 17:33:18  gsl
**	fix copyright header
**	
**	Revision 1.22  2002/08/12 20:13:50  gsl
**	quotes and literals
**	
**	Revision 1.21  2002/08/09 20:41:57  gsl
**	Ensure last char of a generated name isn't a "-" char
**	
**	Revision 1.20  2002/07/25 17:53:34  gsl
**	Fix FAC-OF- prefix
**	
**	Revision 1.19  2002/06/20 23:07:46  gsl
**	native
**	
**	Revision 1.18  2002/06/19 22:50:00  gsl
**	Gen ORDER-AREA-xxx instead of O-A-xxx
**	
**	Revision 1.17  2002/06/18 23:47:12  gsl
**	Change FAC OF to FAC-OF-xxx instead of F-xxx
**	
**	Revision 1.16  1998/03/27 18:38:17  gsl
**	fix warnings
**	
**	Revision 1.15  1998-03-26 09:25:17-05  gsl
**	Changed the get_prog_xxx() routines to not use the gen_data_name()
**	because these need to word before working-storage is processed.
**
**	Revision 1.14  1998-03-25 16:41:25-05  gsl
**	Fix typo
**
**	Revision 1.13  1998-03-25 16:29:01-05  gsl
**	Finish the gen_data_name() routines to remove from the end and to
**	preserve the prefix when munged.
**
**	Revision 1.12  1998-03-23 13:39:37-05  gsl
**	Add gen_data_name() as a replacement for make_fld().
**	Move OLD to old.c
**
**	Revision 1.11  1998-02-10 15:09:51-05  gsl
**	Added get_prog_xxxx() for fname, vname, lname, nname, prname, status.
**
**	Revision 1.10  1996-08-30 21:56:26-04  gsl
**	drcs update
**
**
**
*/
