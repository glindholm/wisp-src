static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"
#include "werrlog.h"
#include "wglobals.h"
#include <ctype.h>
#include <errno.h>
#ifdef UNIX
#include <sys/types.h>
#include <fcntl.h>
#endif
PRNTNAME(wang_file, wang_lib) 
char *wang_lib, *wang_file;
{

 	static int 	l_ws,checkbyte(),l_wswap();
	char 	*vol, *evol;
	static int  	sz, sz1, sz2;
	static char	wang_vol[7];
	static long 	*mode;
	char	prefix[7];								/* The up to 4 char prefix.		*/
	char	tempname[20];
	char	path[80];
	char	*ptr, *fptr;
	int	i;
	long	argcnt, starter, counter, filecount;
	char	recv[22], rtype, template[8];
	char	foundfile[8], foundseq[4];
	int     tseq;									/* Tempfile sequence number.		*/
	int	fdesc;

	l_ws = noswap_words;
	wpload();
	tseq = 0;
	ptr = wang_file;								/* Skip over leading #'s		*/
        memset(wang_vol,'\0',7);
	get_defs(DEFAULTS_SV,wang_vol);
	vol = wang_file;
	evol=(char *)strchr(wang_file,' ');
	if (evol)
		sz=evol-vol;
	else
		sz=6;    

	noswap_words=checkbyte();

	sz2=8-sz;
	if (*ptr == '#' || *ptr == '%') { ptr++; }
	if (*ptr == '#' || *ptr == '%') { ptr++; }

	if (*ptr == '#')								/* THIS WOULD CAUSE SERIOUS PROBLEMS	*/
	{										/* AS THE GENERATED NAME WOULD NOW START*/
		*ptr = '$';								/* WITH '#'. CHANGE TO '$'		*/
	}

	strcpy(prefix,"      ");							/* Extract 4 char prefix.		*/
	for( i=0; i<sz && *ptr != '\0' && *ptr != ' '; i++, ptr++ )
	{
		prefix[i] = *ptr;
	}
	prefix[i] = '\0';
	                                                                             
	memset( template, ' ', 8 );							/* Build template for FIND.		*/
	memcpy( template, prefix, strlen(prefix) );
	memset( &template[strlen(prefix)], '*', sz2 );

	starter=1;									/* Call FIND to get filecount		*/
	counter=1;
	filecount=0;
	l_wswap(&starter,&counter,&filecount);
	rtype = 'A';
	argcnt=8;
	wvaset( &argcnt );
	FIND( template, wang_lib, wang_vol, &starter, &counter, recv, &filecount, &rtype );
	l_wswap(&starter,&counter,&filecount);

	if ( filecount == 0 ) 								/* If none then start at Zero.		*/
	{
		tseq = 0;
	}
	else
	{
		starter = filecount;							/* Get the last file****.		*/
		counter = 1;
		filecount = 0;
		l_wswap(&starter,&counter,&filecount);
		argcnt=8;
		wvaset( &argcnt );
		FIND( template, wang_lib, wang_vol, &starter, &counter, recv, &filecount, &rtype );
		l_wswap(&starter,&counter,&filecount);

		if  ( filecount == 0 )							/* If error then start at Zero.	  	*/
		{
			tseq = 0;
		}
		else
		{
			int scale,num,digit;

			memcpy( foundfile, &recv[14], 8 );				/* Get file filename.			*/
			for( fptr= (&foundfile[7]); *fptr==' '; fptr--);		/* Find end of filename.		*/

			scale = 1;
			num = 0;

			sz1 = 7-sz;
			for( i=sz1; i>=0; i-- )						/* Fill foundseq. 			*/
			{
				if ( ! isdigit(*fptr) )					/* If invalid then start at zero.	*/
				{
					num = -1;
					break;
				}

				digit = *fptr - '0';
				num += digit * scale;
				scale *= 10;
				foundseq[i] = *fptr--;
			}

			if ( num == -1 )
			{
				tseq = 0;
			}
			else
			{
				tseq = num + 1;
			}
		}
	}

	for(;;)
	{
		if (tseq > 9999 && sz2 == 4) tseq=0;						/* If too large then wrap around.	*/
		if (tseq > 999 && sz2 == 3) tseq=0;						/* If too large then wrap around.	*/
		if (tseq > 99 && sz2 == 2) tseq=0;						/* If too large then wrap around.	*/
		if (tseq > 9 && sz2 == 1) tseq=0;						/* If too large then wrap around.	*/

		sprintf( tempname, "%s%02d", prefix, tseq );				/* Build the tempfile name		*/

		memset( wang_file, ' ', 8 );
		memcpy( wang_file, tempname, strlen(tempname) );

		starter=1;									/* Call FIND to get filecount		*/
		counter=1;
		filecount=0;
		l_wswap(&starter,&counter,&filecount);
		rtype = 'A';
		argcnt=8;
		wvaset( &argcnt );
		FIND( tempname, wang_lib, wang_vol, &starter, &counter, recv, &filecount, &rtype );
		l_wswap(&starter,&counter,&filecount);

		if  ( filecount > 0 )							/* If error then start at Zero.	  	*/
		{
			tseq += 1;							/*   try next sequence number.		*/
		}
		else
		{
			break;
		}
	}

	noswap_words = l_ws;								/* Return Swap word.			*/
}

static int checkbyte()
{
	static int first = 1;
	static int normal;
	long	l;
	char	*p;
	char	messstr[80];

	if (first)
	{
		first = 0;
		l = (long) 0x01020304;
		p = (char *)&l;
		if      ( *p == (char) 0x01 )
		{
			normal = 1;
		}
		else if ( *p == (char) 0x04 )
		{
			normal = 0;
		}
		else
		{
			printf( "\n\r UNKNOWN BYTE-ORDER <%8.8x> = <%2.2x> <%2.2x> <%2.2x> <%2.2x> \n\r",
				l, p[0], p[1], p[2], p[3] );
			wexit(1);
		}
	}

	return( normal );
}

static int l_wswap(starter,counter,filecount)
long	 *starter, *counter, *filecount;
{
	if (!noswap_words)
	{
		wswap(starter);
		wswap(counter);
		wswap(filecount);
	}
}
/*
**	History:
**	$Log: prntname.c,v $
**	Revision 1.8  1996-07-23 14:12:56-04  gsl
**	drcs update
**
**
**
*/
