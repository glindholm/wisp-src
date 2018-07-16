/*
**	General C subroutines used at IDSI
**	==================================
**	
**	Don't include any WISP specific routines !!!!
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "idsistd.h"

#ifndef FALSE
#define FALSE 0
#define TRUE !FALSE
#endif

char *upper_string(str)									/* Convert a string to uppercase.	*/
char *str;
{
	for (; *str; str++)
	{
		*str = toupper(*str);
	}
	return( str );
}

char *upper_mem(str,cnt)								/* Convert a string to uppercase.	*/
char *str;
int   cnt;
{
	for (; cnt; str++, cnt--)
	{
		*str = toupper(*str);
	}
	return( str );
}

char *lower_string(str)									/* Convert a string to lowercase.	*/
char *str;
{
	for (; *str; str++)
	{
		*str = tolower(*str);
	}
	return( str );
}

char *lower_mem(str,cnt)								/* Convert a string to lowercase.	*/
char *str;
int   cnt;
{
	for (; cnt; str++, cnt--)
	{
		*str = tolower(*str);
	}
	return( str );
}

char *compressit(str)								/* Compress out spaces.				*/
char *str;									/* " AB  C   D  " -> "ABCD"			*/
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

int upcase(ptr,cnt)								/* Change cnt bytes of mem to uppercase.	*/
char *ptr;
int cnt;
{
	register int ch;

	while (cnt) 
	{                                         
		ch = *ptr;
		*ptr = toupper(ch);
		++ptr; --cnt;
	}
}

int unnull(ptr,cnt)								/* Change nulls to spaces in cnt bytes of mem	*/
char *ptr;
int cnt;
{
	register int ch;

	while (cnt) 
	{
		ch = *ptr;
		if (ch==(char)0) *ptr=' ';
		++ptr; --cnt;
	}
}

int dispchars(ptr,cnt)						/* Change non-display characters to spaces in cnt bytes of mem	*/
unsigned char *ptr;
int cnt;
{
	while (cnt) 
	{
		if (*ptr < (unsigned char) ' ' ||
		    *ptr > (unsigned char) '~'    ) *ptr=' ';
		++ptr; --cnt;
	}
}

int isspaces(p)									/* Is this string all spaces.			*/
register char *p;
{
	if (*p == (char)0) return TRUE;
	while (*p) 
	{
		if (*p++ != ' ') return FALSE;
	}
	return TRUE;
}

int memccpyx(dest,src,ch,cnt)							/* Copies cnt bytes or up to ch character	*/
char *dest, *src;
char ch;
int cnt;
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


void leftjust(ptr,cnt)									/* Left justify char strings. 	*/
char	*ptr;
int	cnt;
{
	char	*p1, *p2;
	int	i,j;

	if ( *ptr != ' ' ) return;

	for(i=0; i<cnt && ptr[i]==' '; i++);						/* i = number of leading spaces.	*/

	if ( i == cnt ) return;								/* If all spaces then nothing to do.	*/

	for(j=0    ; j+i<cnt; j++)  ptr[j] = ptr[j+i];					/* Shift left thru leading spaces.	*/
	for(j=cnt-i; j  <cnt; j++)  ptr[j] = ' ';					/* Pad with trailing blanks.		*/

}


safemove(dest,src,len)								/* safemove (memcpy) handles overlapping move	*/
char	*dest, *src;
int4	len;
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
loadpad(dest,src,size)
char	*dest;
char	*src;
int	size;
{
	for(;size>0 && *src && *src != ' ';size-- ) *dest++ = *src++;

	for(;size>0;size--) *dest++ = ' ';
}

/*
	unloadpad	Load dest with src and null terminate.
			Src is at most size bytes int4, it can be null terminated or space padded.
*/
unloadpad(dest,src,size)
char	*dest;
char	*src;
int	size;
{
	for(;size>0 && *src && *src != ' ';size-- ) *dest++ = *src++;
	*dest = '\0';
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

char *splitpath(filepath)
char *filepath;
{
static	char	buff[256];
	char	*ptr;

	strcpy(buff,filepath);

#ifdef unix
	ptr = strrchr(buff, '/');
	if (ptr)
	{
		*ptr = '\0';								/* NULL the last slash			*/
	}
	else
	{
		buff[0] = '\0';
	}
#endif
#ifdef MSDOS
	ptr = strrchr(buff, '\\');
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

char *splitname(filepath)
char *filepath;
{
static	char	buff[256];
	char	*ptr;

#ifdef unix
	ptr = strrchr(filepath, '/');
	if (ptr)
	{
		ptr++;									/* point after the last slash		*/
	}
	else
	{
		ptr = filepath;
	}
#endif
#ifdef MSDOS
	ptr = strrchr(buff, '\\');
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
	ptr = strrchr(buff,']');
	if (!ptr)
	{
		ptr = strrchr(buff,':');
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

char *splitext(filepath)
char *filepath;
{
static	char	buff[64];
	char	*ptr;

	ptr = strrchr(filepath,'.');
#ifdef unix
	if (ptr)
	{
		if (strchr(ptr,'/')) ptr = "";
	}
	else
	{
		ptr = "";
	}
#endif
#ifdef MSDOS
	if (ptr)
	{
		if (strchr(ptr,'\\')) ptr = "";
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

int hasext(filepath)
char *filepath;
{
	char	*ptr;

	ptr = strrchr(filepath,'.');
	if (!ptr) return(0);

#ifdef unix
	if (0==strchr(ptr,'/')) return(1);
#endif
#ifdef MSDOS
	if (0==strchr(ptr,'\\')) return(1);
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
char *buildfilepath(dest,path,file)
char *dest;
char *path;
char *file;
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
#ifdef unix
		strcat(dest,"/");
#endif /* unix */
#ifdef MSDOS
		strcat(dest,"\\");
#endif /* MSDOS */
	}

	strcat(dest,file);

	return dest;
}
