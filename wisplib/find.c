static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		find.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Find the file or files which match the template
**
**	Routines:	
**	FIND()
*/

/*
**	Includes
*/

#include <varargs.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#if defined(MSDOS) || defined(_MSC_VER)
#include <io.h>
#endif

#if defined(unix) || defined(MSDOS) || defined(WIN32)
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#endif /* unix || MSDOS || WIN32 */

#include "idsistd.h"
#include "wfiles.h"
#include "movebin.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wfname.h"
#include "idsisubs.h"
#include "wexit.h"
#include "wisplib.h"
#include "filext.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Function Prototypes
*/

static int haswc(char* p);
static void osd_find(char* the_file,char* the_lib,char* the_vol,
		     int4 l_starter, int4* l_counter_p,
		     char* receiver, int4* l_file_count_p);
static void build_list(char* vspec, char* lspec, char* fspec);
static void build_lib_list( char* vol, char* vol_path, char* lspec, char* fspec);
static void build_file_list( char* vol, char* vol_path, char* lib, char* fspec);
static void pass_back( char* vol, char* lib, char* fil);



void FIND(va_alist)
va_dcl
{
#define		ROUTINE		19000

	va_list	the_args;
	int	arg_count;
	char	*the_file,*the_lib,*the_vol,*receiver,*receiver_type;
	int4	*starter,*counter,*file_count;
	int4	l_starter,l_counter,l_file_count;

	l_starter = l_counter = l_file_count = 0;
	va_start(the_args);								/* Point to the top of the stack.	*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/
	va_start(the_args);								/* Go back to the top.			*/

	the_file = va_arg(the_args, char*);						/* Get the file name address.		*/ 
	arg_count--;
	the_lib = va_arg(the_args, char*);						/* Get the library name address.	*/
	arg_count--;
	the_vol = va_arg(the_args, char*);						/* Get the volume name address.		*/
	arg_count--;
	starter = va_arg(the_args, int4*);						/* Get address of the start count.	*/
	arg_count--;
	counter = va_arg(the_args, int4*);						/* Get address of the number to list.	*/
	arg_count--;
	receiver = va_arg(the_args, char*);						/* Get address of the receiver area.	*/
	arg_count--;

	file_count = 0;									/* Set the pointer to never never land.	*/
	l_file_count = -1;
	if (arg_count)									/* Are there any more out there ?	*/
	{
		file_count = va_arg(the_args, int4*);					/* Get address of the file count.	*/
		arg_count--;
		l_file_count = 0;
	}
	receiver_type = NULL;
	if (arg_count)
	{
		receiver_type = va_arg(the_args, char*);				/* Get receiver type if specified.	*/

		if ( *receiver_type=='F' )
		{
			werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);				/* Not yet supported.			*/
			wexit(ERRORCODE(2));
		}
	}


	GETBIN(&l_starter,starter,4);							/* copy to local values			*/
	GETBIN(&l_counter,counter,4);
	
	wswap(&l_starter);								/* swap order of the words		*/
	wswap(&l_counter);								/* swap order of the words		*/

	wtrace("FIND","ARGS","File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] Start=%ld Cnt=%ld",
	       the_file, the_lib, the_vol, (long)l_starter, (long)l_counter);

	leftjust(the_file,8);
	leftjust(the_lib,8);
	leftjust(the_vol,6);

	if ( the_vol[0] == ' ' )							/* Not a valid call to FIND.		*/
	{
		wtrace("FIND","BADARG", "Volume is blank");

		l_counter = 0;
		l_file_count = 0;
	}
	else osd_find( the_file, the_lib, the_vol, l_starter, &l_counter, receiver, &l_file_count );

	wtrace("FIND","RETURN", "Files returned=%ld  Files found=%ld", (long)l_counter, (long)l_file_count); 

	wswap(&l_counter);								/* swap order of the words		*/
	wswap(&l_file_count);								/* swap order of the words		*/

	PUTBIN(counter,&l_counter,4);
	if ( file_count ) PUTBIN(file_count,&l_file_count,4);				/* If supplied this argument in list.	*/
}


#ifdef VMS

#include <descrip.h>
#include <rmsdef.h>
#include <fscndef.h>
#include <ssdef.h>
#include <lnmdef.h>

static osd_find( the_file, the_lib, the_vol, l_starter, l_counter_p, receiver, l_file_count_p )
char	*the_file;
char	*the_lib;
char	*the_vol;
int	l_starter;
int	*l_counter_p;
char	*receiver;
int	*l_file_count_p;
{

	char	l_file[9], l_lib[9], l_vol[7];
	char	f_file[9], f_lib[9];
	char	lastfile[9], lastlib[9];
	char 	*l_buf,*lib_buf;
	uint4  	status,status2;
	int  	i,j;
	int 	cnt, pos;
	char 	*end_name;
	int4 	mode;
	int	lib_search;
	unsigned short reslen;
	char	buff[80];
	char template[80],result[256],*context;

	struct itemlist
	{
		short	clen;
		short	code;
		char	*ptr;
	}	items[3];

	struct
	{
		short	clen;
		short	code;
		char	*bufr;
		char	*retaddr;
	}	logitem[3];


#include "find.d"

	l_buf = receiver;

	memcpy( l_file, the_file, 8);
	memcpy( l_lib,  the_lib,  8);
	memcpy( l_vol,  the_vol,  6);

	/*
	**	If a volume search then return just the volume, no widecard matching is done.
	*/
	if (l_lib[0] == ' ' && l_file[0] == ' ')					/* Volume search not supported		*/
	{
		memset(l_buf,' ',22);							/* Blank of one entry			*/

		if ( l_starter == 1 )
		{
			memcpy(l_buf,l_vol,6);						/* for each file, use the same volume	*/
			*l_counter_p = 1;						/* Set the return count.		*/
		}
		else
		{
			*l_counter_p = 0;						/* Set the return count.		*/
		}

		if ( *l_file_count_p == 0 )
		{
			*l_file_count_p = 1;						/* If the pointer is set, return value.	*/
		}
		return;
	}	

	/*
	**	If FILE and VOLUME with no library use [000000] as the library.
	*/
	if (l_lib[0] == ' ')								/* VOLUME:FILE -> VOLUME:[000000]FILE	*/
	{
		if (l_vol[0] != ' ') /*** This will always be TRUE ***/			/* If there is a volume provided...	*/
		{
			reslen = sizeof(result);					/* Set up to get it's logical name.	*/
			logitem[0].clen = sizeof(result);				/* Then we can determine if it is a	*/
			logitem[0].code = LNM$_STRING;					/* device or a path. If it's a device,	*/
			logitem[0].bufr = result;					/* like DUA0, use [000000] to get to the*/
			logitem[0].retaddr = (char *)&reslen;				/* root directory. If a path, use it to	*/
			logitem[1].clen = 0;						/* Access the end of the path. If it's a*/
			logitem[1].code = 0;						/* Path with no unresolved part, leave	*/
											/* The library alone.			*/
			while (l_vol[l_desc.dsc$w_length-1] == ' ') l_desc.dsc$w_length--; /* Reduce length by spaces.		*/

			status = sys$trnlnm(0,&lnm_file,&l_desc,0,logitem);		/* Look in LNM$FILE_DEV.		*/
											/* If not a logical, or it translates to*/
											/* a path with a ".]" on end, then use	*/
											/* "000000" as the lib path.		*/
			if (status != SS$_NORMAL || (status == SS$_NORMAL && result[reslen-2] == '.'))
			{
				strcpy( l_lib, "000000  ");
			}
		}
	}

	mode = 0;

	if (l_file[0] == ' ')								/* If file is blank then lib search.	*/
	{
		setwispfilext("DIR");							/* Build lib template as ...		*/
		end_name = wfname(&mode, l_vol, "000000  ", l_lib, template);		/* VOLUME:[000000]LIB.DIR		*/
		*end_name = (char)0;							/* Null terminate string.		*/
		lib_search = 1;
	}
	else										/* If file search then			*/
	{
		if ( WISPFILEXT[0] == ' ' || WISPFILEXT[0] == '\0' )
			setwispfilext("*");						/* Build template as ....		*/
		end_name = wfname(&mode, l_vol, l_lib, l_file, template);		/* VOLUME:[LIB]FILE.*			*/
		*end_name = (char)0;							/* Null terminate string.		*/
		lib_search = 0;
	}
                                                         
	context = 0;

	pos = 1;								/* Position in list of files found.		*/
	cnt = 0;								/* Count of files loaded into receiver.		*/

	lastfile[0] = (char)0;
	lastlib[0] = (char)0;

	/*
	**	Loop using FIND_FILE to find all the files in this directory.
	**	For each file found test if valid, if not skip to next file.
	**	If file is valid and in the START/COUNT range add it to the reciever.
	*/
	for(;;)
	{
		t_desc.dsc$w_length = strlen(template);				/* Set the length in descriptor.		*/
		status = LIB$FIND_FILE(&t_desc,&r_desc,&context,0,0,0,0);	/* look for a file				*/
		if (status != RMS$_NORMAL) 
		{
			if (status == RMS$_PRV)
			{
				continue;					/* Try next file				*/
			}
			else
			{
				break;						/* Exit the loop				*/
			}
		}

		/*
		**	Test if a valid file name.
		*/
		items[0].code = FSCN$_DIRECTORY;				/* Extract the directory			*/
		items[0].clen = 0;
		items[0].ptr  = 0;
		items[1].code = FSCN$_NAME;					/* Extract the file name			*/
		items[1].clen = 0;
		items[1].ptr  = 0;
		items[2].code = 0;
		items[2].clen = 0;
		items[2].ptr  = 0;

		status2 = SYS$FILESCAN( &r_desc,items,0 );			/* Parse up the filespec			*/
		if ( status2 != SS$_NORMAL )					/* if FILESCAN failed				*/
		{
			werrlog(ERRORCODE(8),result,status2,0,0,0,0,0,0);
			continue;						/* Skip this file and try next one		*/
		}
		memset(f_file,' ',8);
		memset(f_lib,' ',8);

		if ( ! lib_search )
		{
			/*
			**	The library is going to come back as
			**		[MYLIB] or <MYLIB>
			*/
			if (items[0].clen > 2)
			{
				memcpy(buff, items[0].ptr + 1, items[0].clen - 2);
				buff[items[0].clen-2] = (char)0;
			
				if (strlen(buff) > 8)
				{
					continue;				/* LIBRARY Too big, skip to next file		*/
				}
				memcpy(f_lib, buff, strlen(buff));
			}

			if (items[1].clen > 8)
			{
				continue;					/* FILE Too big, skip to next file		*/
			}
			memcpy(f_file, items[1].ptr, items[1].clen);

			if (0==memcmp(lastfile,f_file,8) && 0==memcmp(lastlib,f_lib,8))
			{
				continue;					/* Same as last file/lib, skip to next file	*/
			}
			memcpy(lastfile,f_file,8);
			memcpy(lastlib,f_lib,8);
		}
		else /* LIBRARY FIND */
		{
			/* NOTE: The library is returned in the file position because we are searching	*/
			/*       for  [000000]?lib?.DIR							*/

			if (items[1].clen > 8)
			{
				continue;					/* FILE Too big, skip to next file		*/
			}
			memcpy(f_file, items[1].ptr, items[1].clen);
		}

		/*
		**	A valid file has been found.
		**	If in return range then load into return buffer.
		*/
		if ((pos >= l_starter) && (cnt < *l_counter_p))			/* if in return range				*/
		{ 
			cnt++;							/* Add 1 to the returned entry count.		*/

			memcpy(l_buf,l_vol,6);					/* for each file, use the same volume		*/
			l_buf += 6;						/* Point to the library				*/
			memcpy(l_buf,f_lib,8);
			l_buf += 8;
			memcpy(l_buf,f_file,8);
			l_buf += 8;

			if (cnt == *l_counter_p)				/* If we have filled the reciever		*/
			{
				if (*l_file_count_p != 0)			/* and we don't need a total count		*/
				{
					break;					/* Exit the loop				*/
				}
			}
		}
		pos++;
	}

	*l_counter_p = cnt;							/* Set the return count.			*/

	if ( *l_file_count_p == 0 )
	{
		*l_file_count_p = pos - 1;					/* If the pointer is set, return value.		*/
	}
 		
	status = LIB$FIND_FILE_END(&context);					/* free the file context			*/
}
#endif	/* VMS */


#if defined(unix) || defined(MSDOS) || defined(WIN32)
/*
**	The same osd_find() routines are used for unix and MSDOS
*/

#include "wperson.h"
#include "wdefines.h"
#include "paths.h"

char *nextfile();

static char *rec_ptr;									/* Ptr to reciever area			*/
static char find_ext[39];								/* FIND copy of WISPFILEXT		*/

#define LISTFILES 0
#define LISTLIBS 1
#define LISTVOLS 2
#define BLANKNAME "        "

static	int4	search_mode;
static	int4	found_count;								/* Total matches found.			*/
static	int4	stop_count;								/* 0 = Count all.			*/
static	int4	returned_count;								/* Number matches returned.		*/
static	int4	requested_count;							/* Number matches requested.		*/
static	int4	item_start;								/* First match to return		*/
static	int4	item_count;								/* Current match position		*/


static void osd_find(char* the_file,char* the_lib,char* the_vol,
		     int4 l_starter, int4* l_counter_p,
		     char* receiver, int4* l_file_count_p)
{
	char *fspec;									/* pointers to the "passed" strings	*/
	char *lspec;
	char *vspec;

	char passed_file[50];								/* possible extension			*/
	char passed_vol[9];
	char passed_lib[9];

	search_mode 	= 0;
	found_count 	= 0;
	returned_count 	= 0;
	item_count 	= 0;                                                                                                   
	requested_count = *l_counter_p;
	item_start 	= l_starter;
	rec_ptr 	= receiver;

	if ( *l_file_count_p == 0 )							/* Do we count all matches.		*/
	{
		stop_count = 0;								/* Count all matches (Don't stop)	*/
	}
	else
	{
		stop_count = l_starter + *l_counter_p - 1;				/* Calculate stopping point		*/
	}

	wpload();

	memset(passed_vol,(char)0,sizeof(passed_vol));
	memset(passed_file,(char)0,sizeof(passed_file));
	memset(passed_lib,(char)0,sizeof(passed_lib));
	memccpyx(passed_vol,the_vol,' ',6);
	memccpyx(passed_lib,the_lib,' ',8);
	memccpyx(passed_file,the_file,' ',8);

	vspec = compressit(passed_vol);							/* Make compressed null-term strings.	*/
	lspec = compressit(passed_lib);
	fspec = compressit(passed_file);

	if ( fspec[0] == '#' ) fspec[0] = '%';						/* Substitute leading #	-> %		*/
	if ( lspec[0] == '#' ) lspec[0] = '%';

	search_mode = LISTFILES;							/* Determine search mode		*/
	if (isspaces(fspec)) search_mode = LISTLIBS;
	if (isspaces(lspec) && isspaces(fspec)) search_mode = LISTVOLS;

	leftjust(WISPFILEXT,39);

	memcpy(find_ext,WISPFILEXT,39);							/* grab a copy of WISPFILEXT		*/
	*(strchr(find_ext,' ')) = NULL_CHAR;						/* Make find_ext null terminated	*/

	if ( WISPFILEXT[0] != ' ' )							/* If ext then add to fspec.		*/
	{
		strcat(fspec,".");
		strcat(fspec,find_ext);
	}
	memset(WISPFILEXT,' ',sizeof(WISPFILEXT));					/* Clear the extension			*/


	build_list(vspec,lspec,fspec);							/* Generate to list.			*/


	*l_counter_p = returned_count;							/* Set number entries returned		*/
	if ( stop_count == 0 )								/* If no stop count then return		*/
		*l_file_count_p = found_count;						/*   the number found			*/

}

											/* Build the list starting by matching	*/
static void build_list(char* vspec, char* lspec, char* fspec)							/* the VOLUMES.				*/
{
	logical_id *p;
	char	*vol, *vol_path;

	if ( haswc(vspec) )
	{
											/* MATCH THE VOLUME			*/
		for (p=get_logical_list(); p; p=(logical_id *)p->next)			/* for loop to find matching volume 	*/
		{
			vol      = p->logical;
			vol_path = p->translate;
											/* Compare to logical list (LGMAP) 	*/
											/* Volume '.' will only match vspec '.' */
											/* it does not match wildcards.		*/
			if ( strcmp(vol,".")!=0 && wcmatch(vspec,vol,FALSE,'*','?') )
			{

				if (search_mode==LISTVOLS)
				{
					++found_count;					/* Count the number matches found.	*/
					pass_back(vol,BLANKNAME,BLANKNAME);		/* Load into receiver.			*/
				}
				else							/* If not LISTVOLS then build lib-list	*/
				{
					build_lib_list(vol,vol_path,lspec,fspec);	/* Get libs and files on this volume 	*/
				}

				if (stop_count && found_count >= stop_count ) break;	/* If we found enough then stop.	*/
			}
		}
	}	
	else										/* NO WILDCARDS IN VOLUME		*/
	{
		char	vol_buf[80];

		wlgtrans(vspec,vol_buf);

		if ( fexists(vol_buf) )							/* See if it exists			*/
		{

			if (search_mode==LISTVOLS)
			{
				++found_count;						/* Count the number matches found.	*/
				pass_back(vspec,BLANKNAME,BLANKNAME);			/* Load into receiver.			*/
			}
			else								/* If not LISTVOLS then build lib-list	*/
			{
				build_lib_list(vspec,vol_buf,lspec,fspec);		/* Get libs and files on this volume 	*/
			}
		}
	}
}

											/* Build the list of LIBRARY under VOL	*/
static void build_lib_list( char* vol, char* vol_path, char* lspec, char* fspec)
{
	char	*vol_context = NULL;
	char	*libname;
	char 	tmpbuf[200];
	struct stat statbuf;

	vol_context = NULL;

	if (search_mode==LISTFILES && isspaces(lspec))					/* If a filesearch and lspec is blank	*/
	{
		libname = ".";								/* Use libname="."			*/
		build_file_list(vol,vol_path,libname,fspec);				/* Build the files under libname="."	*/
	}
	else if (! haswc(lspec) )							/* NO WILDCARDS IN LIBRARY		*/
	{
		char	lib_buf[40];

		strcpy(lib_buf,lspec);
		lower_string(lib_buf);

		buildfilepath(tmpbuf,vol_path,lib_buf);					/* Concat the vol and lib		*/

		if ( fexists(tmpbuf) )							/* See if it exists			*/
		{
			if ( stat(tmpbuf,&statbuf)==0 )					/* make sure we can access it 		*/
			{
				 if (statbuf.st_mode & S_IFDIR)				/* is it a dir? 			*/
				{

					if (search_mode==LISTLIBS)			/* If were looking for libs		*/
					{
						++found_count;				/* Count number matches found.		*/
						pass_back(vol,lib_buf,BLANKNAME);	/* Load into receiver			*/
					}
					else						/* If not LISTLIB then  build filelist */
					{
						build_file_list(vol,vol_path,lib_buf,fspec);
					}
				}
			}

		}
	}
	else while ((libname = nextfile(vol_path,&vol_context)) != NULL)
	{
											/* Lib "." does not match wildcards	*/
											/* Lib ".." never matches		*/
											/* See if it matches			*/
		if ( ( strcmp(libname,".")==0 && strcmp(lspec, ".")==0 ) ||
		     ( strlen(libname) <= 8 &&
		       strcmp(libname,".")!=0 && 
		       strcmp(libname,"..")!=0 && 
		       wcmatch(lspec,libname,FALSE,'*','?') )               )
		{
			buildfilepath(tmpbuf,vol_path,libname);				/* Make path to call stat		*/
			if ( stat(tmpbuf,&statbuf)==0 )					/* make sure we can access it 		*/
			{
				 if (statbuf.st_mode & S_IFDIR)				/* is it a dir? 			*/
				{

					if (search_mode==LISTLIBS)			/* If were looking for libs		*/
					{
						++found_count;				/* Count number matches found.		*/
						pass_back(vol,libname,BLANKNAME);	/* Load into receiver			*/
					}
					else						/* If not LISTLIB then  build filelist */
					{
						build_file_list(vol,vol_path,libname,fspec);
					}
					if (stop_count && found_count >= stop_count ) break; /* If we found enough then stop.	*/
				}
			}
		}

	}
	nextfile(NULL,&vol_context);							/* Reset nextfile for reading dir	*/
}

											/* Build list of FILEs under LIB	*/
static void build_file_list( char* vol, char* vol_path, char* lib, char* fspec)
{
	char 	*lib_context = NULL;
	char 	*filename;
	char 	dirpath[128];
	char	file_noext[80];								/* The filename without extension.	*/
	char	*cmp_file;								/* Compare file string			*/
	char	*ptr;
	char 	tmpbuf[200];
	char	lastfile[9];

	lib_context = NULL;

	lastfile[0] = (char)0;

	buildfilepath(dirpath,vol_path,lib);
	while ((filename = nextfile(dirpath,&lib_context)) != NULL)
	{

											/* FILE "." and ".." never match	*/
		if ( strcmp(filename,".")==0 || strcmp(filename,"..")==0 )
		{
			continue;
		}

		strcpy(file_noext,filename);						/* Strip the file extention.		*/
		ptr = osd_ext(file_noext);						/* Point to the ext (after the dot (.))	*/
		if ( ptr ) 
		{
			if (strcmp(ptr,"idx")==0 && find_ext[0] == NULL_CHAR)
			{
				continue;						/* Don't find .idx files (for cisam)	*/
			}
			*(--ptr) = NULL_CHAR;
		}

		if (strlen(file_noext) > 8) continue;					/* Filename too big			*/

		if (0==strcmp(lastfile,file_noext))					/* Don't do duplicate names		*/
		{
			continue;
		}
		strcpy(lastfile,file_noext);

		if ( find_ext[0] == NULL_CHAR )						/* If no file extension			*/
			cmp_file = file_noext;						/*    then Match any extension.		*/
		else
			cmp_file = filename;						/*    Else Match found extension	*/


		if ( wcmatch(fspec,cmp_file,FALSE,'*','?') )
		{
			struct 	stat statbuf;

			buildfilepath(tmpbuf,dirpath,filename);				/* Make path to call stat		*/
			if ( stat(tmpbuf,&statbuf)==0 )				    	/* make sure we can access it 		*/
			{
				 if (!(statbuf.st_mode & S_IFDIR))			/* make sure it's not a dir 		*/
				{
					 ++found_count;

					pass_back(vol,lib,file_noext);

					if (stop_count && found_count >= stop_count ) break;
				}
			}
		}
	}
	nextfile(NULL,&lib_context);							/* Reset nextfile for reading dir	*/
}

static void pass_back( char* vol, char* lib, char* fil)								/* Load the results into reciever	*/
{
#define REC struct receiver_struct
struct receiver_struct {
	char volume[6];
	char library[8];
	char file[8];
	};

	item_count += 1;
	if ( item_count >= item_start && returned_count < requested_count )
	{
		memset( rec_ptr,' ',22 );
		memccpyx(((REC *)rec_ptr)->volume,vol,'\0',6);
		upcase(((REC *)rec_ptr)->volume,6);
		memccpyx(((REC *)rec_ptr)->library,lib,'\0',8);
		upcase(((REC *)rec_ptr)->library,8);
		memccpyx(((REC *)rec_ptr)->file,fil,'\0',8);
		upcase(((REC *)rec_ptr)->file,8);
		++returned_count;			/*  	22 is number of bytes in each cobol struct... must */
		rec_ptr += 22;				/* <--- explicitly add this or C will pad to force 4 byte alignment */
							/*      getting us out of alignment with the cobol struct array */
	}
}

#endif /* unix || MSDOS || WIN32 */


static int haswc(char* p)
{
	if( strchr(p,'*') || strchr(p,'?') ) return(1);
	else return(0);
}

#ifdef unix
/*
	find a set of files in a directory.
*/

findlong( dir, wildcard, start, max, table, size, total )
char	*dir;										/* 80 byte path to search directory.	*/
char	*wildcard;									/* 32 byte name with unix wildcards.	*/
int4	*start;										/* Start table at this ordinal file.	*/
int4	*max;										/* Maximum and actual count of names.	*/
char	*table;										/* Table to hold found file names.	*/
int4	*size;										/* Size in bytes of each table entry.	*/
int4	*total;										/* Total number of wildcard matches.	*/
{
	int	istart, iend, imax, isize;						/* Local integer copies of variables.	*/
	char	*element;								/* Pointer into "table" for each name.	*/
	int	caps_found;								/* Flag set if any chars are uppercase.	*/
	char	wildlow[33];								/* Lower case version of wildcard.	*/
	char	cmd[256];								/* Unix command started with popen().	*/
	FILE	*pfb;									/* Pointer to FILE block from popen().	*/
	int	i;									/* Used to count found file names.	*/
	char	cmdline[256];								/* Holds single lines from cmd output.	*/
	int	linlen;									/* Length of cmdline as returned.	*/

	istart	= *start - 1;								/* Convert ordinal start to "0" index.	*/
	iend	= *max + istart;							/* Calculate last name wanted.		*/
	imax	= 0;									/* Set count of names returned to 0.	*/
	isize	= *size;								/* Size of each element in the table.	*/
	element	= table;								/* Start first element in the table.	*/

	caps_found = 0;									/* Set flag for no uppercase found.	*/
	for( i = 0 ;  i < 32 && wildcard[i] ; ++i )					/* For each character in wildcard,	*/
	{
		if( isupper( wildcard[i] ) )						/* If character is uppercase,		*/
		{
											/* _tolower() not available on SUN.	*/
											/* 0x20 | is the same as _tolower().	*/
			wildlow[i] = 0x20 | wildcard[i] ;				/* Move lowercase version to wildlow.	*/
			caps_found = 1;							/* Set flag for char is uppercase.	*/
		}
		else									/* If character is not uppercase,	*/
		{
			wildlow[i] = wildcard[i];					/* Then move it to wildlow.		*/
		}
	}

	if( caps_found )								/* If there were any uppercase chars,	*/
	{
		sprintf( cmd, "cd %80.80s ; ls %32.32s %32.32s 2> /dev/null", 		/* Build unix double search command.	*/
			dir, wildcard, wildlow );
	}
	else										/* If there were no uppercase chars,	*/
	{
		sprintf( cmd, "cd %80.80s ; ls %32.32s 2> /dev/null", 			/* Build unix single search command.	*/
			dir, wildcard );
	}

	pfb = popen( cmd, "r" );							/* Execute command and pipe output.	*/

	for( i = 0 ; fgets( cmdline, 256, pfb ) ; ++i )					/* Get each line of output from cmd.	*/
	{
		if( ( i >= istart ) && ( i < iend ) )					/* If from istart to iend, store it.	*/
		{
			strncpy( element, cmdline, isize );				/* Put cmdline into table.		*/
			linlen = strlen( cmdline ) - 1;					/* cmdline ends with newline+null.	*/
			if( linlen < isize )						/* Pad with spaces, no null.		*/
			{
				memset( ( element + linlen ), ' ', ( isize - linlen ) );
			}
			element += isize;						/* Advance to next element in table.	*/
			++imax;								/* Increment actual count of names.	*/
		}
	}
	*total = i;									/* Set total number of names found.	*/

	pclose ( pfb );									/* Close command pipe.			*/
	*max = imax;									/* Set actual count of names in table.	*/
}

#endif	/* unix */


/*
**	History:
**	$Log: find.c,v $
**	Revision 1.14  1997-04-15 23:11:13-04  gsl
**	Update to use wtrace()
**
**	Revision 1.13  1997-03-12 13:03:07-05  gsl
**	changed to use WIN32 define
**
**	Revision 1.12  1996-09-10 11:43:09-04  gsl
**	move the system include before wisp includes
**
**	Revision 1.11  1996-07-15 09:43:27-07  gsl
**	fix include filext
**
**	Revision 1.10  1996-06-28 08:45:25-07  gsl
**	fix prototypes, includes and reuse unix and msdos code for NT
**
**	Revision 1.9  1995-04-25 02:52:43-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.8  1995/04/17  11:46:07  gsl
 * drcs state V3_3_14
 *
 * Revision 1.7  1995/03/10  14:01:27  gsl
 * fix headers
 *
**
**
*/
