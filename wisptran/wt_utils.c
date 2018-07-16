			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern

#include "wisp.h"

#define	ALPHA_FAC	129
#define NUM_FAC		130
#define PROT_FAC	140

fparse(fnam,fext,flib)							/* Take a filename and determine if it has already	*/
char *fnam,*fext,*flib;							/* got an extension on it. if not, add one.		*/
{									/* add the library name also				*/
	char 	fullname[133], filename[133], liblist[128];
	char	*p;
	char	*end, *ptr;
	int	i;

#ifdef unix
	for( p=fnam; *p; *p=tolower(*p),p++ );				/* Shift filename to lowercase				*/
	for( p=fext; *p; *p=tolower(*p),p++ );				/* Shift extension to lowercase				*/
#endif

	fullname[0] = 0;
	filename[0] = 0;

	strcat(filename,fnam);						/* Add the file name to the fullname			*/

	if ((strpos(fnam,".") == -1) && fext[0])			/* Does the filename already have an extension?		*/
	{
		strcat(filename,".");					/* no, so add a period					*/
		strcat(filename,fext);					/* and add the extension				*/
	}

	strcpy(fullname,filename);

	if ((flib[0] != 0) && (flib[0] != '\n'))			/* Were we passed a libname ?				*/
	{
#ifdef VMS
		strcpy(fullname,flib);					/* Yes, start the fullname with it			*/
		if ((strpos(flib,":") == -1) && (strpos(flib,"[") == -1))	
		{							/* If it has no colon or [, it is a logical name,	*/
			strcat(fullname,":");				/* so we should add a colon as a separator		*/
		}
		strcat(fullname,filename);
#else /* NOT VMS */
		strcpy(liblist,flib);
		end = liblist;
		end += strlen(liblist);
		for(ptr = liblist;ptr<end;)
		{
			for(i=0;*ptr && *ptr != ':';ptr++,i++)
				fullname[i] = *ptr;
			fullname[i] = '\0';
			ptr++;
#ifdef unix
			strcat(fullname,"/");
#endif /* unix */
#ifdef MSDOS
			strcat(fullname,"\\");
#endif /* MSDOS */
			strcat(fullname,filename);
			if (access(fullname,0)==0) break;
		}
#endif /* NOT VMS */
	}

	strcpy(fnam,fullname);						/* Copy the new complete filename			*/

	return(0);
}


int keyword(the_word,the_list)						/* search a list of keywords to see if the_word is in	*/
char *the_word;								/* the list						*/
char *the_list[];
{
	int i;
	i = 0;

	do								/* this routine will return keyword numbers starting	*/
	{								/* with 1, not 0.					*/
		if (!strcmp(the_word,the_list[i++])) return(i);		/* found a match					*/
	} while (*the_list[i]);						/* look till null string				*/
	return(0);							/* no match						*/
}

int strpos(src,srch)							/* search a string for the occurence of another string	*/
char *src,*srch;							/* src is the string to search, srch is the match	*/
{
	int i;
	char *tsrc,*tsrch;

	i = 0;									/* start position counter			*/
	do
	{
		tsrc = src;							/* copy the pointer				*/
		tsrch = srch;

		do
		{
			if (*tsrch != *tsrc) break;				/* no match					*/
			tsrch++;
			tsrc++;
		} while (*tsrch && *tsrc);					/* till null					*/
		if (!*tsrch) return(i);						/* a match					*/
		if (!*tsrc) return(-1);						/* out of space					*/
		i++;
	} while (*(++src));							/* till null					*/
	return(-1);								/* didn't match					*/
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




/* 	Search through the various field lists until you find the given field, then return it's pointer.			*/

item_record *find_item(the_name)
char *the_name;
{
	int i;
	item_record *this_item;

	for (i=0; i<num_screens; i++)							/* first search each screen...		*/
	{
		this_item = screen_item[i];						/* Get ptr to first field		*/
		do
		{
			if (!strcmp(the_name,this_item->name))				/* found it!				*/
			{
				return(this_item);					/* Return the pointer.			*/
			}
			this_item = this_item->next_item;				/* check next item			*/
		} while (this_item);							/* until no more items			*/
	}
	return(0);									/* Not found.				*/
}

int file_index(the_name)								/* Scan the file list for a file.	*/
char *the_name;
{
	int i;

	if (!prog_cnt) return(-1);							/* No files, just exit.			*/

	for (i=prog_cnt-1; i > -1; i--) if (!strcmp(prog_files[i],the_name)) break;	/* look for the file name		*/

	return(i);									/* Will be -1 or the file number.	*/
}

int crt_file_index(the_name)								/* Scan the file list for a file.	*/
char *the_name;
{
	int i;

	if (!crt_fcount) return(-1);							/* No files, just exit.			*/

	for (i=crt_fcount-1; i > -1; i--) if (!strcmp(crt_file[i],the_name)) break;	/* look for the file name		*/

	return(i);									/* Will be -1 or the file number.	*/
}


make_fac(fac,src)				/* take a source variable name and make a FAC variable name			*/
char *fac,*src;
{
	make_fld(fac,src,"F-");						/* do it						*/
}

make_oa(oa,src)					/* take a source variable name and make an ORDER-AREA variable name.		*/
char *oa,*src;
{
	make_fld(oa,src,"O-A-");					/* do it!						*/
}


make_fld(dst,src,prfx)				/* take a source variable name and prefix and make a new variable name.		*/
char *dst,*src,*prfx;
{
	int i,j;
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

int strlast(string,srch)								/* determine if the srch char is the	*/
char string[];										/* last non-whitespace			*/
char srch;
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
