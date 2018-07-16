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

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef WIN32
#include <io.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

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
#include "wperson.h"
#include "paths.h"

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

#define		ROUTINE		19000


void FIND(char* the_file, char* the_lib, char* the_vol, int4 *starter, int4 *counter, char* receiver, ...)
{
	va_list	the_args;

	int	arg_count;
	char	*receiver_type;
	int4	*file_count;
	int4	l_starter,l_counter,l_file_count;

	l_starter = l_counter = l_file_count = 0;
	file_count = NULL;
	l_file_count = -1;
	receiver_type = NULL;

	va_start(the_args, receiver);							/* Point to the top of the stack.	*/

	arg_count = va_count(the_args);							/* How many args are there ?		*/
	if (arg_count < 6 || arg_count > 8)
	{
		char buff[80];
		
		sprintf(buff, "%%FIND-F-ARGS Invalid number of arguments [%d] expecting 6-8", arg_count);
		werrlog(104,buff,0,0,0,0,0,0,0);

		va_end(the_args);
		wexit(ROUTINE+4);
	}
	


	if (arg_count > 6)								/* Are there any more out there ?	*/
	{
		file_count = va_arg(the_args, int4*);					/* Get address of the file count.	*/
		l_file_count = 0;
	}
	if (arg_count == 8)
	{
		receiver_type = va_arg(the_args, char*);				/* Get receiver type if specified.	*/

		if ( *receiver_type=='F' )
		{
			va_end(the_args);
			werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);				/* Not yet supported.			*/
			wexit(ERRORCODE(2));
		}
	}
	va_end(the_args);

	l_starter = get_swap(starter);
	l_counter = get_swap(counter);

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
	else 
	{
		osd_find( the_file, the_lib, the_vol, l_starter, &l_counter, receiver, &l_file_count );
	}

	wtrace("FIND","RETURN", "Files returned=%ld  Files found=%ld", (long)l_counter, (long)l_file_count); 

	put_swap(counter,l_counter);
	
	if ( file_count )								/* If supplied this argument in list.	*/
	{
		put_swap(file_count,l_file_count);
	}
}



/*
**	The osd_find() routines are used for unix and WIN32
*/

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
static void build_list(char* vspec, char* lspec, char* fspec)				/* the VOLUMES.				*/
{
	logical_id *p;
	char	*vol, *vol_path;

	wtrace("FIND","BUILD_LIST","vspec=[%s] lspec=[%s] fspec=[%s]",
	       vspec, lspec, fspec);

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

	wtrace("FIND","BUILD_LIB_LIST","vol=[%s] vol_path=[%s] lspec=[%s] fspec=[%s]",
	       vol, vol_path, lspec, fspec);

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

		if ( WL_isadir(tmpbuf) )						/* See if it is a directory		*/
		{
			if (search_mode==LISTLIBS)					/* If were looking for libs		*/
			{
				++found_count;						/* Count number matches found.		*/
				pass_back(vol,lib_buf,BLANKNAME);			/* Load into receiver			*/
			}
			else								/* If not LISTLIB then  build filelist */
			{
				build_file_list(vol,vol_path,lib_buf,fspec);
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
			if ( WL_isadir(tmpbuf) )					/* Check if a directory			*/
			{
				if (search_mode==LISTLIBS)				/* If were looking for libs		*/
				{
					++found_count;					/* Count number matches found.		*/
					pass_back(vol,libname,BLANKNAME);		/* Load into receiver			*/
				}
				else							/* If not LISTLIB then  build filelist */
				{
					build_file_list(vol,vol_path,libname,fspec);
				}
				if (stop_count && found_count >= stop_count ) break;	/* If we found enough then stop.	*/
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

	wtrace("FIND","BUILD_FILE_LIST","vol=[%s] vol_path=[%s] lib=[%s] fspec=[%s]",
	       vol, vol_path, lib, fspec);

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
			/*
			 *	Remove the extension.
			 *	Later will remove duplicates.
			 */

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
			buildfilepath(tmpbuf,dirpath,filename);				/* Make path 				*/
			if ( WL_isafile(tmpbuf) )			    		/* Check if a file			*/
			{
				++found_count;

				pass_back(vol,lib,file_noext);

				if (stop_count && found_count >= stop_count ) break;
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

	wtrace("FIND","FOUND","vol=[%6.6s] lib=[%8.8s] fil=[%8.8s]", vol, lib, fil);

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


static int haswc(char* p)
{
	if( strchr(p,'*') || strchr(p,'?') ) return(1);
	else return(0);
}




/*
**	History:
**	$Log: find.c,v $
**	Revision 1.18.2.1  2002/10/09 21:03:00  gsl
**	Huge file support
**	
**	Revision 1.18  2001/11/07 21:30:31  gsl
**	Remove VMS * MSDOS code
**	ifdef'ed obsolete findlong()
**	
**	Revision 1.17  1999-08-20 12:37:33-04  gsl
**	Removed the logic that was ignoring .idx and .vix files. This was done
**	in a attempt to stop duplicate file names from being returned. However
**	it prevented an idx file from being found if the dat portion was missing.
**	Also, there is other logic that eliminates duplicates.
**
**	Revision 1.16  1998-11-02 14:56:39-05  gsl
**	change to use <stdarg.h> variable arguments plus extra error checking
**
**	Revision 1.15  1998-05-15 10:19:55-04  gsl
**	Add support for Vision4 file with .vix extensions
**
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
