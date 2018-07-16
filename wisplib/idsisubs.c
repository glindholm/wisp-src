static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	General C subroutines used at IDSI
**	==================================
**	
**	Don't include any WISP specific routines !!!!
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "idsisubs.h"
#include "paths.h"

#ifndef FALSE
#define FALSE 0
#define TRUE !FALSE
#endif

char *upper_string(char *str)								/* Convert a string to uppercase.	*/
{
	for (; *str; str++)
	{
		*str = toupper(*str);
	}
	return( str );
}

char *upper_mem(char *str, int cnt)							/* Convert a string to uppercase.	*/
{
	for (; cnt; str++, cnt--)
	{
		*str = toupper(*str);
	}
	return( str );
}

char *lower_string(char *str)								/* Convert a string to lowercase.	*/
{
	for (; *str; str++)
	{
		*str = tolower(*str);
	}
	return( str );
}

char *lower_mem(char *str, int cnt)							/* Convert a string to lowercase.	*/
{
	for (; cnt; str++, cnt--)
	{
		*str = tolower(*str);
	}
	return( str );
}

char *compressit(char *str)							/* Compress out spaces.				*/
										/* " AB  C   D  " -> "ABCD"			*/
{
	char *tmp, *p;
	char buf[80];

	if (!strlen(str)) return str;
	tmp = buf;
	for (p=str; *p; ++p)
	{
		if (*p != ' ') *tmp++ = *p;
	}
	*tmp = '\0';	
	strcpy(str,buf);
	return(str);
}

int upcase(char *ptr, int cnt)							/* Change cnt bytes of mem to uppercase.	*/
{
	register int ch;

	while (cnt) 
	{                                         
		ch = *ptr;
		*ptr = toupper(ch);
		++ptr; --cnt;
	}
	return 0;
}

int unnull(char *ptr, int cnt)							/* Change nulls to spaces in cnt bytes of mem	*/
{
	register int ch;

	while (cnt) 
	{
		ch = *ptr;
		if (ch==(char)0) *ptr=' ';
		++ptr; --cnt;
	}
	return 0;
}

int dispchars(char *ptr, int cnt)			/* Change non-display characters to spaces in cnt bytes of mem	*/
{
	while (cnt) 
	{
		if (*ptr < ' ' || *ptr > '~') *ptr=' ';
		++ptr; --cnt;
	}
	return 0;
}

int isspaces(char *p)								/* Is this string all spaces.			*/
{
	if (*p == (char)0) return TRUE;
	while (*p) 
	{
		if (*p++ != ' ') return FALSE;
	}
	return TRUE;
}

int memccpyx(char *dest, const char *src, char ch, int cnt)			/* Copies cnt bytes or up to ch character	*/
{
	int i;									/* Return number of char copied.		*/

	i = 0;
	while (cnt && *src != ch)
	{
		*dest++ = *src++;
		--cnt;
		i++;
	} 
	return(i);
}


void leftjust(char *ptr, int cnt)							/* Left justify char strings. 	*/
{
	int	i,j;

	if ( *ptr != ' ' ) return;

	for(i=0; i<cnt && ptr[i]==' '; i++);						/* i = number of leading spaces.	*/

	if ( i == cnt ) return;								/* If all spaces then nothing to do.	*/

	for(j=0    ; j+i<cnt; j++)  ptr[j] = ptr[j+i];					/* Shift left thru leading spaces.	*/
	for(j=cnt-i; j  <cnt; j++)  ptr[j] = ' ';					/* Pad with trailing blanks.		*/

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


/*
	loadpad		Load dest with src and pad with spaces up to size.
			Src can be null terminated or space padded.
*/
void loadpad(char *dest, const char *src, int size)
{
	for(;size>0 && *src && *src != ' ';size-- ) *dest++ = *src++;

	for(;size>0;size--) *dest++ = ' ';
}

/*
	unloadpad	Load dest with src and null terminate.
			Src is at most size bytes int4, it can be null terminated or space padded.
*/
void unloadpad(char *dest, const char *src, int size)
{
	for(;size>0 && *src && *src != ' ';size-- ) *dest++ = *src++;
	*dest = '\0';
}



int strpos(const char *src, const char *srch)				/* search a string for the occurence of another string	*/
									/* src is the string to search, srch is the match	*/
{
	int i;
	const char *tsrc,*tsrch;

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
	

int stredt(char *src, const char *srch, const char *repl)				/* Edit a source line, find the search	*/
											/* string and replace it with *repl	*/
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

/*
**	Routine:	splitpath()
**
**	Function:	To separate the file path from the file spec.
**
**	Description:	Copy the full file spec into a static data area then put a NULL where the file name starts.
**			
**				UNIX:	/usr/project/sub1/filex.c	-> /usr/project/sub1
**				VMS:	USER$DISK:[LIBSTUFF]FILEX.C	-> USER$DISK:[LIBSTUFF]
**				DOS:	C:\HOME\WORK\FILEX.C		-> C:\HOME\WORK
**
**	Arguments:	filepath	the full file spec
**
**	Return:		Pointer to the path portion (this may be a NULL string).
**
**	Warnings:	A second call to splitpath will invalidate the returned pointer.
**
**	History:	07/17/92	Written by GSL
**
*/

char *splitpath(const char *filepath)
{
static	char	buff[256];
	char	*ptr;

	strcpy(buff,filepath);

#if defined(unix) || defined(MSFS)
	ptr = strrchr(buff, DIR_SEPARATOR);
	if (ptr)
	{
		*ptr = '\0';								/* NULL the last slash			*/
	}
	else
	{
		buff[0] = '\0';
	}
#endif
#ifdef VMS
	ptr = strrchr(buff,']');
	if (ptr)
	{
		ptr[1] = '\0';								/* NULL after the ']'			*/
	}
	else
	{
		ptr = strrchr(buff,':');
		if (ptr)
		{
			ptr[1] = '\0';							/* NULL after the ':'			*/
		}
		else
		{
			buff[0] = '\0';
		}
	}
#endif

	return(buff);
}

/*
**	Routine:	splitname()
**
**	Function:	To separate the file name from the file spec.
**
**	Description:	Strip off the path and the extension and save name in static data area.
**			
**				UNIX:	/usr/project/sub1/filex.c	-> filex
**				VMS:	USER$DISK:[LIBSTUFF]FILEX.C	-> FILEX
**				DOS:	C:\HOME\WORK\FILEX.C		-> FILEX
**
**	Arguments:	filepath	the full file spec
**
**	Return:		Pointer to the name portion (this may be a NULL string).
**
**	Warnings:	A second call to splitname will invalidate the returned pointer.
**
**	History:	07/17/92	Written by GSL
**
*/

char *splitname(char *filepath)
{
static	char	buff[256];
	char	*ptr;

#if defined(unix) || defined(MSFS)
	ptr = strrchr(filepath, DIR_SEPARATOR);
	if (ptr)
	{
		ptr++;									/* point after the last slash		*/
	}
	else
	{
		ptr = filepath;
	}
#endif
#ifdef VMS
	ptr = strrchr(filepath,']');
	if (ptr)
	{
		ptr++;
	}
	else
	{
		ptr = strrchr(filepath,':');
		if (!ptr)
		{
			ptr = filepath;
		}
	}
#endif

	strcpy(buff,ptr);								/* Load buff starting at filename	*/

	ptr = strrchr(buff,'.');							/* Remove extension			*/
	if (ptr)
	{
		*ptr = '\0';
	}

	return(buff);
}

/*
**	Routine:	splitext()
**
**	Function:	To separate the file extension from the file spec.
**
**	Description:	Strip off the path and name then save the extension in static data area.
**			
**				UNIX:	/usr/project/sub1/filex.c	-> .c
**				VMS:	USER$DISK:[LIBSTUFF]FILEX.C	-> .C
**				DOS:	C:\HOME\WORK\FILEX.C		-> .C
**
**	Arguments:	filepath	the full file spec
**
**	Return:		Pointer to the extension portion with leading period (this may be a NULL string).
**
**	Warnings:	A second call to splitext will invalidate the returned pointer.
**
**	History:	07/17/92	Written by GSL
**
*/

char *splitext(char *filepath)
{
static	char	buff[64];
	char	*ptr;

	ptr = strrchr(filepath,'.');
#if defined(unix) || defined(MSFS)
	if (ptr)
	{
		if (strchr(ptr,DIR_SEPARATOR)) ptr = "";
	}
	else
	{
		ptr = "";
	}
#endif
#ifdef VMS
	if (ptr)
	{
		if (strchr(ptr,']')) ptr = "";
		if (strchr(ptr,':')) ptr = "";
	}
	else
	{
		ptr = "";
	}
#endif

	strcpy(buff,ptr);
	return(buff);
}

/*
**	Routine:	hasext()
**
**	Function:	Returns if the file spec has an extension.
**
**	Description:	Find the last '.' then see if we've gone pass the file portion.
**			
**	Arguments:	filepath	the full file spec
**
**	Return:		1 	file has extension
**			0	file does not have extension
**
**	Warnings:	None
**
**	History:	07/27/92	Written by GSL
**
*/

int hasext(char *filepath)
{
	char	*ptr;

	ptr = strrchr(filepath,'.');
	if (!ptr) return(0);

#if defined(unix) || defined(MSFS)
	if (0==strchr(ptr,DIR_SEPARATOR)) return(1);
#endif
#ifdef VMS
	if (0==strchr(ptr,']')) return(1);
	if (0==strchr(ptr,':')) return(1);
#endif

	return(0);
}


/*
**	Routine:	buildfilepath()
**
**	Function:	To build a filepath from a file and path.
**
**	Description:	This routine concatenates the file and path together to form
**			a full filepath.  It handles O/S specific dir separators.
**			Dest and path can be the same.
**
**	Arguments:
**	dest		The destination to put the filepath. Must be large enough to hold result.
**	path		The path with out a file and without trailing separator.
**	file		The filename.
**
**	Globals:	None
**
**	Return:		Dest.
**
**	Warnings:	None
**
**	History:	
**	12/28/92	Written by GSL
**
*/
char *buildfilepath(char *dest, const char *path, const char *file)
{
	/*
	**	If dest and path are the same then we are concatinating to path so don't copy.
	*/
	if (dest != path)
	{
		/*
		**	If a path then copy it otherwise make dest a null string.
		*/
		if (path)
		{
			strcpy(dest,path);
		}
		else
		{
			dest[0] = (char)0;
		}
	}

	/*
	**	If dest is a null string then don't need a leading separator
	*/
	if (dest[0])
	{
#if defined(unix) || defined(MSFS)
		strcat(dest,DIR_SEPARATOR_STR);
#endif
	}

	strcat(dest,file);

	return dest;
}

/*
**	Routine:	numeric2int4()
**
**	Function:	Convert a numeric (PIC 9) to int4.
**
**	Description:	Convert a COBOL style PIC 9(x) numeric into an int4.
**			The source must consist of only '0' - '9' characters.
**
**	Arguments:
**	result		The int4 value returned
**	source		The numeric source
**	sourcelen	The lenght of source
**
**	Globals:	None
**
**	Return:
**	0		Success
**	1		Invalid source
**
**	Warnings:	None
**
**	History:	
**	09/16/94	Written by GSL
**
*/
int numeric2int4(int4 *result, char *source, int sourcelen)
{
	int	idx;
	int4	value;

	*result = 0;
	value = 0;

	for(idx=0; idx<sourcelen; idx++)
	{
		if (source[idx] < '0' || source[idx] > '9') return 1;

		value = (value * 10) + (source[idx] - '0');
	}

	*result = value;

	return 0;
}

/*
**	Routine:	field2int4()
**
**	Function:	Convert a user entered number into an int4.
**
**	Description:	Handles only unsigned number.
**			Can have both leading and trailing spaces.
**
**	Arguments:
**	field		The input field
**	len		The field length
**	num		The returned value of the field
**
**	Globals:	None
**
**	Return:
**	0		Success
**	1		Invalid input
**
**	Warnings:	None
**
**	History:	
**	09/19/94	Written by GSL
**
*/
int field2int4(char *str, int len, int4 *num)
{
	int	idx;

	*num = 0;

	/*
	**	Skip over leading spaces;
	*/
	for(idx=0; idx<len && ' '==str[idx]; idx++);

	/*
	**	Empty field
	*/
	if (idx == len) return 1;

	/*
	**	Load up the number
	*/
	for(; idx<len && ' ' != str[idx]; idx++)
	{
		if (str[idx] >= '0' && str[idx] <= '9')
		{
			*num = (*num * 10) + (str[idx] - '0');
		}
		else
		{
			return 1;
		}
	}

	/*
	**	Ensure that only spaces remain
	*/
	for(; idx<len; idx++)
	{
		if (' ' != str[idx])
		{
			return 1;
		}
	}

	return 0;
}
/*
**	History:
**	$Log: idsisubs.c,v $
**	Revision 1.13  1997-08-18 15:52:48-04  gsl
**	Change prototypes to add const keyword where needed
**
**	Revision 1.12  1996-08-23 16:59:42-04  gsl
**	buildfilepath() madepath and file parms const
**
**	Revision 1.11  1996-08-19 15:32:23-07  gsl
**	drcs update
**
**
**
*/
