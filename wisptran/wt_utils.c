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

#define EXT extern
#include "wisp.h"

#define	ALPHA_FAC	129
#define NUM_FAC		130
#define PROT_FAC	140

int strpos(src,srch)
char	*src;
char	*srch;
{
	char	*ptr;
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

	

int stredt(src,srch,repl)								/* Edit a source line, find the search	*/
char *src,*srch,*repl;									/* string and replace it with *repl	*/
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

make_fac(fac,src)				/* take a source variable name and make a FAC variable name			*/
char *fac,*src;
{
	make_fld(fac,src,"F-");						/* do it						*/
	return 0;
}

make_oa(oa,src)					/* take a source variable name and make an ORDER-AREA variable name.		*/
char *oa,*src;
{
	make_fld(oa,src,"O-A-");					/* do it!						*/
	return 0;
}


make_fld(dst,src,prfx)				/* take a source variable name and prefix and make a new variable name.		*/
char *dst,*src,*prfx;
{
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
	for( i = i-30; i > 0;)
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

	while( i > 30 )
	{
		dst[--i] = ' ';
		stredt(dst," ","");
	}
	return 0;
}

#define HT '\t'										/* Define horizontal tab.		*/
#define SP ' '										/* Define space character.		*/
#ifdef NULL
#undef NULL
#endif
#define NULL '\0'


int trim(string) char string[];								/* Trim a string of trailing blanks.	*/

{
    register int i;									/* Working register storage.		*/

    for (i = strlen(string)-1; (i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == NULL)); i--) string[i] = NULL;

    return(i+1);									/* Return the string length.		*/
}

int strlast(char *string, char srch)							/* determine if the srch char is the	*/
											/* last non-whitespace			*/
{
	register int i;									/* Working register storage.		*/
	for 	(i = strlen(string)-1;
		(i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == NULL) || (string[i] == '\n'));
		(i--));
	if (string[i] == srch)	return(1);						/* it is the last char			*/
	else			return(0);						/* not found				*/
}


sp_trunc(text)									/* Truncate a string at the first space		*/
char *text;
{
	int i;
	i = strpos(text," ");							/* find the space				*/
	if ( i != -1 ) text[i] = '\0';						/* replace with a null				*/
	return 0;
}


/*				Squeeze a line until it is equal to the_length							*/

int wsqueeze(the_line,length)
char *the_line;
int  length;
{
	int i,j;

	i = strlen(the_line);							/* First examine the line.			*/

	if (the_line[i-1] == '\n') i--;						/* Ignore the newline.				*/

	if (i <= length) return(0);						/* no need to squeeze the line			*/

	do
	{
		j = stredt(&the_line[12],"  "," ");				/* change 2 spaces to 1 space after col 12	*/
	} while ((j != -1) && (--i > length));					/* till no more or length is ok			*/

	if (i > length) return(-1);						/* Couldn't do it.				*/
	else return(1);								/* say we did it				*/
}

uppercase(str)										/* Shift string to uppercase		*/
char *str;
{
	for(;*str;str++)  *str = toupper(*str);
	return 0;
}

lowercase(str)										/* Shift string to uppercase		*/
char *str;
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

int noncase_eq(str1,str2)
char	*str1, *str2;
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
**	History:
**	$Log: wt_utils.c,v $
**	Revision 1.10  1996-08-30 21:56:26-04  gsl
**	drcs update
**
**
**
*/
