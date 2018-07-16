static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <ctype.h>
#include <string.h>
#include <assert.h>

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"

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
	return gen_data_name(fac, "F-", src, NULL);
}

char*  make_oa(char* oa, const char* src)		/* take a source variable name and make an ORDER-AREA variable name.	*/
{
	return gen_data_name(oa, "O-A-", src, NULL);
}

#define MAX_COBOL_WORD_LEN	30

/* take a source variable name and prefix and make a new variable name.		*/
char* make_fld(char *dst, const char* src, const char* prfx)	
{
	return gen_data_name(dst, prfx, src, NULL);
	
#ifdef OLD
	int i;
	char	*ptr;

	strcpy(dst,prfx);						/* start the string with the prefix			*/
	strcat(dst,src);						/* add in the old field name				*/

	if ((i = strpos(dst,"(")) == -1)				/* Make sure length is not counting possible (INDEX)	*/
	{
		i = strlen(dst);					/* count length of the variable name			*/
	}

	ptr = dst;
	ptr += strlen(prfx);						/* move ptr to character after prefix			*/
	for( i = i-MAX_COBOL_WORD_LEN; i > 0;)
	{
		if( 0 == stredt(ptr,"-",""))				/* remove dashes till right size			*/
			i--;
		else
			break;
	}

	if ((i = strpos(dst,"(")) == -1)				/* Make sure length is not counting possible (INDEX)	*/
	{
		i = strlen(dst);					/* count length of the variable name			*/
	}

	while( i > MAX_COBOL_WORD_LEN )
	{
		dst[--i] = ' ';
		stredt(dst," ","");
	}
	return dst;
#endif /* OLD */
}

char* gen_data_name(char* dest, const char* prefix, const char* base, const char* suffix)
{
	const char* l_prefix = "";
	const char* l_suffix = "";
	char	temp[80];
	
	if (prefix)
	{
		l_prefix = prefix;
	}
	if (suffix)
	{
		l_suffix = suffix;
	}

	sprintf(temp,"%s%s%s", l_prefix, base, l_suffix);

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

	/*
	**	Ensure generated name doesn't conflict with a user symbol.
	*/
	if (is_symbol(temp))
	{
		/*
		**	Make Unique with "-Q"
		*/
		gen_data_name(temp, l_prefix, &temp[strlen(l_prefix)], "-Q");

		write_log("WISP",'I',"GENNAME", "Generated data name [%s] has been munged to not conflict with symbol [%s%s%s]",
			  temp, l_prefix, base, l_suffix);
	}

	strcpy(dest,temp);
	
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

int paracmp(str,strtab,tabcnt)
char	*str;
char	strtab[MAX_PARAGRAPHS][40];
int	tabcnt;
{
	int	i;
	for(i=0; i<tabcnt; i++)
	{
		if (0==strcmp(str,strtab[i])) return(1);
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
**	Use make_prog_name() for all the get_prog_xxx() routines.
**	This will truncate the results required length.
**	Don't use gen_data_name() because these are required 
**	before working-storage and the value gen_data_name() generates
**	may be different after working-storage is loaded into symbol table.
*/
static char *make_prog_name(char *new, int fnum, const char* prefix)
{
	char buff[80];
	
	strcpy(buff,prefix);
	strcat(buff,prog_files[fnum]);
	buff[MAX_COBOL_WORD_LEN] = '\0';
	strcpy(new,buff);

	return new;
}

const char *get_prog_vname(int fnum)
{
	static char the_name[40];
	
	if ((prog_vnames[fnum][0] == '\"') || (!prog_vnames[fnum][0]))
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
	
	if ((prog_lnames[fnum][0] == '\"') || (!prog_lnames[fnum][0]))
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
	
	if ((prog_fnames[fnum][0] == '\"') || (!prog_fnames[fnum][0]))
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

	return make_prog_name(the_name, fnum, "S-");
}

/*
**	History:
**	$Log: wt_utils.c,v $
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
