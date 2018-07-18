/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/*
**	File:		wfname.c
**
**	Purpose:	To construct the native file spec based on the Wang style file spec.
**			This is used by ALL WISP routines to construct real file names from Wang templates.
**
**	Routines:	wfname()	Construct the real file name.
**			wtname()	Generate temp names (##).
**			vmsfilechars()	Translate VMS special file characters.
**			WL_wlgtrans()	LGMAP logical volume translation. 
**			WL_logworklib()	Log the work librarys generated.
**			matchnative()	Match native file spec to actual file.
**
*/


/*
	1	Get local and save copies
	2	Load Vol & Lib with default values if blank
	3	Load extension
	4	Handle workfiles
	5	Cleanup Vol & Lib & File 
	6	Perpare TEMP Vol & Lib & File for path generation
	7	Handle special cases
	8	Generate tempfile names if requied
	9	Contruct path
	10	Backfill if required

*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#if defined(WIN32) 
#include <io.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include "idsistd.h"
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"
#include "werrlog.h"
#include "cobrun.h"
#include "wfname.h"
#include "wdefines.h"
#include "wisplib.h"
#include "vssubs.h"
#include "idsisubs.h"
#include "paths.h"
#include "filext.h"
#include "wispcfg.h"

#ifdef WIN32
#include "isonames.h"
#endif

#ifdef unix
#define  PRINT_FILE_EXT 	""
#define  DEFAULT_FILE_EXT	""
#endif	/* unix */
#ifdef WIN32
#define  PRINT_FILE_EXT 	".lis"
#define  DEFAULT_FILE_EXT	""
#endif	/* WIN32 */

static int wtname(char *wang_vol, char *wang_lib, char *wang_file, 
		char *native_vol, char *native_lib, char *native_file, char *native_ext);

static char *x_wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path, 
		      int is_backfill, int is_lib);	

static void WL_logworklib( char *worklib );

char *WL_wanglib2path(char *p_vol, char *p_lib, char *native_path)
{
	int4 mode = 0; 
	char l_file[SIZEOF_FILE];

	memset(l_file, ' ', SIZEOF_FILE);

	return x_wfname(&mode, p_vol, p_lib, l_file, native_path, 0, 1); /* IS_LIB */

}

char *WFNAME2(	char *attrstr,		/* File attributes (10 chars)		*/
		char *vol,		/* the WANG volume name	(6 chars)	*/
		char *lib,		/* The WANG library name (8 chars)	*/
		char *file,		/* The file name	(8 chars)	*/
		char *native_path)	/* The resultant name			*/
{
	int4 mode;
	char* ptr;

	wisp_fileattr2mode(attrstr, &mode);

	ptr = WL_wfname_backfill(&mode, vol, lib, file, native_path);	/* IS_BACKFILL */

	wisp_mode2fileattr(mode, attrstr);

	return ptr;
}

#define IS_LIB		0x00000040 		/* Generate a LIB only.			*/
#define IS_BACKFILL	0x00001000 		/* Backfill file/lib/vol.		*/

char *WFNAME(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path)
{
	if (*mode & IS_LIB)
	{
		return WL_wanglib2path(p_vol, p_lib, native_path);
	}

	if (*mode & IS_BACKFILL)
	{
		return WL_wfname_backfill(mode, p_vol, p_lib, p_file, native_path);
	}

	return x_wfname(mode, p_vol, p_lib, p_file, native_path, 0, 0);
}

char *WL_wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path)
{
	return x_wfname(mode, p_vol, p_lib, p_file, native_path, 0, 0);
}

char *WL_wfname_backfill(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path)
{
	return x_wfname(mode, p_vol, p_lib, p_file, native_path, 1, 0);	/* IS_BACKFILL */
}

/* generate a local file name		*/
/* returns ptr to next avail char pos	*/
static char *x_wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path, 
		      int is_backfill, int is_lib)	
{
	char wang_file[SIZEOF_FILE], wang_lib[SIZEOF_LIB], wang_vol[SIZEOF_VOL];	/* Wang format copies of file/lib/vol.	*/
	char work_lib[SIZEOF_LIB], work_vol[SIZEOF_VOL];				/* WORKLIB and WORKVOL			*/
	char native_file[20], native_lib[20], native_vol[80], native_ext[40];		/* Native format of file/lib/vol/ext.	*/
	int i,len;
	int	nomodext;								/* No modifcations of ext allowed	*/
	char fileext[WISP_FILE_EXT_SIZE];
	char attrstr[WISP_FILE_ATTR_SIZE];


	/*
	**	INITIALIZE
	*/

	wisp_mode2fileattr(*mode, attrstr);
	wtrace("WFNAME","ENTRY", "vol=[%6.6s] lib=[%8.8s] file=[%8.8s] Attr=[%10.10s]", 
		p_vol, p_lib, p_file, attrstr);

      	WL_wpload();		 							/* load the usage constants		*/

	memset(native_path, ' ', COB_FILEPATH_LEN);					/* Clear out the 80 character filename.	*/


	/*
	**	GET LOCAL AND SAVE COPIES
	*/

	memcpy(wang_vol,p_vol,SIZEOF_VOL);						/* Grab local copies.			*/
	memcpy(wang_lib,p_lib,SIZEOF_LIB);
	memcpy(wang_file,p_file,SIZEOF_FILE);

	leftjust(wang_vol,SIZEOF_VOL);							/* Left justify file/lib/vol.		*/
	leftjust(wang_lib,SIZEOF_LIB);
	leftjust(wang_file,SIZEOF_FILE);

	if (is_lib)									/* If IS_LIB then blank filename	*/
	{
		memset(wang_file, ' ', SIZEOF_FILE );
	}

	/*
	**	LOAD VOL & LIB FROM DEFAULTS
	*/

	if ( wang_vol[0] == ' ' )							/* If volume is blank get default.	*/
	{
		if (*mode & IS_PRINTFILE)
		{
			WL_get_defs(DEFAULTS_SV,wang_vol);
		}
		else if (*mode & IS_OUTPUT)
		{
			WL_get_defs(DEFAULTS_OV,wang_vol);
		}
		else /* INPUT */
		{
			WL_get_defs(DEFAULTS_IV,wang_vol);
		}
		leftjust(wang_vol,SIZEOF_VOL);
	}

	if ( wang_lib[0] == ' ' )							/* If library is blank get default.	*/
	{
		if (*mode & IS_PRINTFILE)
		{
			WL_get_defs(DEFAULTS_SL,wang_lib);
		}
		else if (*mode & IS_OUTPUT)
		{
			WL_get_defs(DEFAULTS_OL,wang_lib);
		}                                               
		else /* INPUT */
		{
			WL_get_defs(DEFAULTS_IL,wang_lib);
		}
		leftjust(wang_lib,SIZEOF_LIB);
	}
      

	/*
	**	LOAD EXTENSION                                                          
	*/

	nomodext = 0;
	WGETFILEXT(fileext);
	if (fileext[0] != ' ' && fileext[0] != 0)					/* If file ext then load & null term	*/
	{
		native_ext[0] = '.';
		for( i=0; i<WISP_FILE_EXT_SIZE && fileext[i] != ' '; i++ )
		{
			native_ext[i+1] = fileext[i];
		}
		native_ext[i+1] = '\0';
		nomodext = 1;								/* No modification of ext allowed	*/
	}
	else if (*mode & IS_PRINTFILE)							/* Use PRINT file extension		*/
	{
		strcpy( native_ext, PRINT_FILE_EXT );
	}
	else										/* Use DEFAULT file extension		*/
	{
		strcpy( native_ext, DEFAULT_FILE_EXT );
	}

	WSETFILEXT(" ");								/* Always reset the file extension.	*/

	if (is_lib)									/* If IS_LIB then blank the extension	*/
	{
		native_ext[0] = '\0';
	}
	

	/*
	**	HANDLE WORKFILES
	*/

	WL_get_defs(DEFAULTS_WV,work_vol);
	WL_get_defs(DEFAULTS_WL,work_lib);

	if ( memcmp(wang_lib,work_lib,SIZEOF_LIB) == 0 &&
	     memcmp(wang_vol,work_vol,SIZEOF_VOL) == 0    )
	{
		*mode |= IS_WORK;
	}
	else
	{
		*mode &= ~IS_WORK;	/* Clear the IS_WORK flag */
	}

	if ( wang_file[0] == '#' || wang_file[0] == '%' )				/* If tempfile				*/
	{
		if ( !(*mode & IS_PRINTFILE) )						/* and not printfile			*/
		{
			*mode |= IS_WORK;						/* Set the IS_WORK bit			*/

			memcpy(wang_vol,work_vol,SIZEOF_VOL);				/* It is forced into work		*/
			memcpy(wang_lib,work_lib,SIZEOF_LIB);

			/*
			**	#xxxx	Work files are scratched when the link level ends.  (IS_SCRTACH)
			**	##xxxx	Temp files are scratched when the worklib is deleted when highest link level ends.
			*/

			if ( wang_file[1] == '#' || wang_file[1] == '%' )
			{
				*mode &= ~IS_SCRATCH;	/* Temp file - clear IS_SCRATCH */
		        }
		        else
			{
				*mode |= IS_SCRATCH;	/* Work file - set IS_SCRATCH */
		        }
		}
	}

	/*
	**	CLEANUP VOL & LIB & FILE
	*/


	if ( wang_vol[0] == ' ' )							/* If volume is blank ....		*/
	{
		wang_vol[0] = '.';							/* Replace with dot.			*/
	}
                                                                                           
	if ( wang_lib[0] == ' ' )				      			/* If volume is blank ....		*/
	{
		wang_lib[0] = '.';							/* Replace with dot.			*/
	}



	/*
	**	PERPARE TEMP VOL & LIB & FILE FOR PATH GENERATION
	*/



	WL_wlgtrans( wang_vol, native_vol );					       	/* Load vol with translation.		*/

	unloadpad(native_lib,  wang_lib,  SIZEOF_LIB);
	unloadpad(native_file, wang_file, SIZEOF_FILE);

	strcat(native_vol, DIR_SEPARATOR_STR);
	strcat(native_lib, DIR_SEPARATOR_STR);

	if ( native_lib[0]  == '#' ) native_lib[0]  = '%';			       	/* Convert leading # -> %		*/
	if ( native_file[0] == '#' ) native_file[0] = '%';				/* Convert leading # -> %		*/


#ifdef unix
	WL_lower_string(native_lib);							/* Convert lib & file to lowercase	*/
	WL_lower_string(native_file);
#endif	/* unix */
	

	/*
	**	HANDLE SPECIAL CASES
	*/


	if (*mode & IS_PRINTFILE)							/* if it is a print file		*/
	{
		*mode &= ~IS_SCRATCH;							/* Clear Scratch flag.			*/
	}


	/*
	**	GENERATE TEMPFILE NAMES
	*/

	*mode &= ~IS_TEMP;								/* Clear the IS_TEMP flag		*/

	if ( ((*mode & IS_OUTPUT) || (*mode & IS_PRINTFILE)) && 
	     (wang_file[0] == '#' || wang_file[0] == '%')       )
	{

		wtname( wang_vol,   wang_lib,   wang_file,
			native_vol, native_lib, native_file, native_ext );		/* Grenerate native_file name.		*/

		*mode |= IS_TEMP;

	}
	

	/*
	**	ASSEMBLE THE FILE NAME
	*/

	WL_matchnative(native_vol, native_lib, native_file, native_ext, nomodext, is_lib ); /* Match the file specs to the disk.	*/

	sprintf(native_path,"%s%s%s%s", native_vol, native_lib, native_file, native_ext );


	/*
	**	LOGGING OF WORK LIBRARIES
	*/

	if ( (*mode & IS_WORK) && (*mode & IS_OUTPUT) )
	{
		char native_worklib[80];
		static	char	last_work_vol[SIZEOF_VOL] = {0};
		static	char	last_work_lib[SIZEOF_LIB] = {0};

		if ( memcmp(wang_vol, last_work_vol, SIZEOF_VOL) != 0 ||
		     memcmp(wang_lib, last_work_lib, SIZEOF_LIB) != 0   )
		{
			memcpy(last_work_vol, wang_vol, SIZEOF_VOL);
			memcpy(last_work_lib, wang_lib, SIZEOF_LIB);
			sprintf(native_worklib, "%s%s", native_vol, native_lib );

			WL_logworklib( native_worklib );

		}
	}	


	/*
	**	BACKFILL THE FILE SPECIFICATIONS
	*/


	if ( is_backfill )
	{
		memcpy(p_vol,wang_vol,SIZEOF_VOL);					/* Backfill the file/lib/vol.		*/
		memcpy(p_lib,wang_lib,SIZEOF_LIB);
		memcpy(p_file,wang_file,SIZEOF_FILE);
	}

	wisp_mode2fileattr(*mode, attrstr);
	wtrace("WFNAME","RETURN", "Path=[%s] vol=[%6.6s] lib=[%8.8s] file=[%8.8s] Attr=[%10.10s]", 
	       native_path, p_vol, p_lib, p_file, attrstr);

	len = strlen(native_path);
	native_path[len] = ' ';								/* Remove null-termination		*/

	return(&native_path[len]);							/* return next available char pos	*/
}


/*
	wtname		Generate a Wang stlye temp file name.

			- extract a 4 char prefix
			- build a template for calling find
			- call find to get the filecount
			- call find to get the last matching file (this should be the largest sequence number)
			- extract the sequence number and increment
			- build the file path and test for existence
			- create a dummy file to hold the sequence number
*/

static int wtname(char *wang_vol, char *wang_lib, char *wang_file, 
		char *native_vol, char *native_lib, char *native_file, char *native_ext)
{
	char	prefix[5];								/* The up to 4 char prefix.		*/
	char	tempname[20];
	char	path[80];
	char	*ptr, *fptr;
	int	i;
	int4	starter, counter, filecount;
	char	recv[22], rtype, template[8];
	char	foundfile[8], foundseq[4];
	int     tseq;									/* Tempfile sequence number.		*/
	int	fdesc;

	tseq = 0;

	ptr = wang_file;								/* Skip over leading #'s		*/
	if (*ptr == '#' || *ptr == '%') { ptr++; }
	if (*ptr == '#' || *ptr == '%') { ptr++; }

	if (*ptr == '#')								/* THIS WOULD CAUSE SERIOUS PROBLEMS	*/
	{										/* AS THE GENERATED NAME WOULD NOW START*/
		*ptr = '$';								/* WITH '#'. CHANGE TO '$'		*/
	}

	strcpy(prefix,"    ");								/* Extract 4 char prefix.		*/
	for( i=0; i<4 && *ptr != '\0' && *ptr != ' '; i++, ptr++ )
	{
		prefix[i] = *ptr;
	}
	prefix[i] = '\0';
	                                                                             
	memset( template, ' ', 8 );							/* Build template for FIND.		*/
	memcpy( template, prefix, strlen(prefix) );
	memset( &template[strlen(prefix)], '*', 4 );


	starter=1;									/* Call FIND to get filecount		*/
	counter=1;
	filecount=0;
	WL_wswap(&starter);
	WL_wswap(&counter);
	WL_wswap(&filecount);
	rtype = 'A';

	WL_set_va_count(8);
	FIND( template, wang_lib, wang_vol, &starter, &counter, recv, &filecount, &rtype );

	WL_wswap(&filecount);
	if ( filecount == 0 ) 								/* If none then start at Zero.		*/
	{
		tseq = 0;
	}
	else
	{
		starter = filecount;							/* Get the last file****.		*/
		counter = 1;
		filecount = 0;
		WL_wswap(&starter);
		WL_wswap(&counter);
		WL_wswap(&filecount);
		WL_set_va_count( 8);
		FIND( template, wang_lib, wang_vol, &starter, &counter, recv, &filecount, &rtype );

		WL_wswap(&filecount);
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

			for( i=3; i>=0; i-- )						/* Fill foundseq. 			*/
			{
				if ( ! isdigit((int)*fptr) )					/* If invalid then start at zero.	*/
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

	sprintf(path,"%s%sXXX.XXX", native_vol, native_lib);				/* create a dummy path for makepath	*/
	makepath( path );								/* make the dir path up to file		*/


	for(;;)
	{
		if (tseq > 9999) tseq=0;						/* If too large then wrap around.	*/

		sprintf( tempname, "%s%04d", prefix, tseq );				/* Build the tempfile name		*/

		memset( wang_file, ' ', SIZEOF_FILE );
		memcpy( wang_file, tempname, strlen(tempname) );
		strcpy( native_file, tempname );

#ifdef unix
		WL_lower_string( native_file );						/* Convert lib & file to lowercase	*/
#endif

		sprintf(path,"%s%s%s%s", native_vol, native_lib, native_file, native_ext); /* create real path of temp file	*/

		if ( fexists(path) )							/* If file exists then ...		*/
		{
			tseq += 1;							/*   try next sequence number.		*/
		}
		else
		{
			break;
		}
	}

#ifdef unix
	fdesc = creat( path, 0666 );							/* Reserve the filespec			*/
#endif
#ifdef WIN32
	fdesc = _creat( path, _S_IREAD | _S_IWRITE );					/* Reserve the filespec			*/
#endif
	if ( fdesc != -1 )
	{
		close( fdesc );
	}

	return 0;
}



/*
	WL_wlgtrans:	LOGICAL TRANSLATOR
			- search the logical list for in_str and return translation in out_str.
			- if no match found then move in_str to out_str.
			- in_str is up to 6 chars and is not null terminated
			- return 1 if a translation was done; 0 if no translation
			- the logicals are case sensitive
			- out_str will be null terminated
*/
int WL_wlgtrans( char *in_str, char *out_str )
{
	char	in_temp[SIZEOF_VOL+1];
	logical_id	*logical_ptr;

	logical_ptr = WL_get_lgmap_list();

	unloadpad(in_temp, in_str, SIZEOF_VOL);

	upper_string(in_temp);									/* Make UPPERCASE.		*/

	for(;;)											/* Scan list of logicals	*/
	{
		if (!logical_ptr)
		{
			strcpy( out_str, in_temp );						/* No Match found.		*/
			return(0);
		}

		if ( strcmp(in_temp, logical_ptr->logical) == 0 )				/* Found Match.			*/
		{
			strcpy( out_str, logical_ptr->translate );				/* Use translation.		*/
			return(1);
		}

		logical_ptr = logical_ptr->next;				/* follow down the list.	*/
	}
}

static void WL_logworklib( char *worklib )
{
	int	fd;
	char	wf[128];

	worklib[strlen(worklib)-1] = '\0';						/* remove trailing '/'			*/
	buildfilepath(wf, wisptmpbasedir(NULL), "WLIBLIST");
	fd = open( wf, O_WRONLY | O_CREAT | O_APPEND, 0666 );
	if ( fd == -1 )
	{
		werrlog(WERRCODE(77504),wf, errno,0,0,0,0,0,0,0 );
		return;
	}
	write( fd, worklib, strlen(worklib) );
	write( fd, "\n", 1 );
	close( fd );
#ifdef unix
	chmod(wf,0666);
#endif
}


/*
**	Routine:	WL_wfexists()
**
**	Function:	To check if a file exists
**
**	Description:	This routine tests if a Wang style file exists.
**			On unix and MSDOS it can handle CISAM files.
**
**	Arguments:
**	file		The file name
**	lib		The library name
**	vol		The volume name
**
**	Globals:	None
**
**	Return:
**	0		File does not exist
**	1		File exists
**	2		File exists (CISAM file)
**
**	Warnings:	None
**
**	History:	
**	03/03/93	Written by GSL
**
*/
int WL_wfexists(char *file, char *lib, char *vol)
{
	int4	mode;
	char	*ptr;
	char	filename[COB_FILEPATH_LEN];
	int	outexists;

	mode = 0;
	ptr = WL_wfname(&mode, vol, lib, file, filename);			/* Construct the filename.			*/
	*ptr = (char)0;

	outexists = 0;
	if (fexists(filename))							/* Check if file exists				*/
	{
		outexists = 1;
	}
	if (!outexists && !hasext(filename))					/* Check if CISAM				*/
	{
		strcat(filename,".idx");
		if (fexists(filename))						/* Check if file exists				*/
		{
			outexists = 2;
		}
	}
	return outexists;
}

/*
**	Convert between mode and new file attr string
*/
static struct 
{
	int4	mode;
	char*	attr;
} mode_attr_list[] =
{
	{IS_PRINTFILE,	IS_PRINTFILE_ATTR},
	{IS_INDEXED,	IS_INDEXED_ATTR},
	{IS_SEQSEQ,	IS_SEQSEQ_ATTR},
	{IS_SEQDYN,	IS_SEQDYN_ATTR},
	{IS_RELATIVE,	IS_RELATIVE_ATTR},
	{IS_DBFILE,	IS_DBFILE_ATTR},
	{IS_OUTPUT,	IS_OUTPUT_ATTR},
	{IS_IO,		IS_IO_ATTR},
	{IS_SORT,	IS_SORT_ATTR},
	{IS_EXTEND,	IS_EXTEND_ATTR},
	{IS_GETPARM,	IS_GETPARM_ATTR},
	{IS_ERROR,	IS_ERROR_ATTR},
	{IS_WORK,	IS_WORK_ATTR},
	{IS_SCRATCH,	IS_SCRATCH_ATTR},
	{IS_TEMP,	IS_TEMP_ATTR},
	{IS_NORESPECIFY,IS_NORESPECIFY_ATTR},
	{IS_DECLARE,	IS_DECLARE_ATTR},
	{0, 0}
};

void wisp_fileattr2mode(const char *attrstr, int4 *mode)
{
	int i;
	*mode = 0;

	for(i=0; mode_attr_list[i].mode != 0; i++)
	{
		if (NULL != memchr(attrstr, *(mode_attr_list[i].attr), WISP_FILE_ATTR_SIZE))
		{
			*mode |= mode_attr_list[i].mode;
		}
	}
}


void wisp_mode2fileattr(int4 mode, char *attrstr)
{
	int i, j;
	memset(attrstr, ' ', WISP_FILE_ATTR_SIZE);

	j=0;
	for(i=0; mode_attr_list[i].mode != 0; i++)
	{
		if (mode & mode_attr_list[i].mode)
		{
			attrstr[j++] = *(mode_attr_list[i].attr);
		}
	}
}


/*
**	History:
**	$Log: wfname.c,v $
**	Revision 1.50  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.49  2009/10/18 20:57:07  gsl
**	Fix bug with Vista/2008 where creat() was failing because of unsupported mode.
**	
**	Revision 1.48  2003/04/03 20:27:53  gsl
**	WFNAME2()
**	
**	Revision 1.47  2003/03/20 18:28:45  gsl
**	Fix logical_id typedef
**	
**	Revision 1.46  2003/03/19 21:11:44  gsl
**	Remove USE_PVPL flag
**	
**	Revision 1.45  2003/03/06 21:37:52  gsl
**	Change trace to show ATTR instead of MODE
**	
**	Revision 1.44  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.43  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.42  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.41  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.40  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.39  2002/12/10 20:54:08  gsl
**	use WERRCODE()
**	
**	Revision 1.38  2002/07/29 15:46:49  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.37  2002/07/23 20:49:50  gsl
**	globals
**	
**	Revision 1.36  2002/07/18 21:04:29  gsl
**	Remove MSDOS code
**	
**	Revision 1.35  2002/07/12 19:10:19  gsl
**	Global unique WL_ changes
**	
**	Revision 1.34  2002/07/12 17:01:03  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.33  2002/07/11 20:29:17  gsl
**	Fix WL_ globals
**	
**	Revision 1.32  2002/07/10 21:05:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.31  2002/07/10 04:27:35  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.30  2002/06/28 04:02:58  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.29  2002/06/27 04:12:41  gsl
**	Clean up status/mode bits
**	
**	Revision 1.28  2002/06/26 06:26:21  gsl
**	Mode/status bit field changes
**	
**	Revision 1.27  2002/06/26 04:25:03  gsl
**	Cleanup mode/status bit fields
**	
**	Revision 1.26  2002/06/25 17:46:05  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.25  2002/06/25 03:47:58  gsl
**	remove IS_CASE_SEN - never actually implemented
**	
**	Revision 1.24  2002/06/21 20:49:30  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.23  2002/06/21 03:10:44  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.22  1999/01/30 00:02:06  gsl
**	Fix the handling of work files to make a distiction between # and ## work
**	files.  Only the single # files get the IS_SCRATCH flag and will be deleted
**	when the link-level terminates. ## files will be deleted when the worklib
**	is deleted when the highest link-level terminates.
**	
**	Revision 1.21  1998-05-15 09:23:18-04  gsl
**	Fixed bug 518.  When checking if a work file it incorrect was checking
**	if the lib or the vol was equal to the work lib and vol. It should have
**	been checking if the lib AND the vol were equal.
**
**	Revision 1.20  1998-05-14 16:44:26-04  gsl
**	fix the trace logic
**
**	Revision 1.19  1998-05-12 15:01:38-04  gsl
**	Fixed problem 524 where non-work directories were being added to WLIBLIST.
**	The IS_WORK flag was being set and not cleared.
**
**	Revision 1.18  1998-01-13 10:13:19-05  gsl
**	Change WIN32 default file extension to be "" (none), it was .DAT
**	but is documented in WISP manual to be none.
**
**	Revision 1.17  1997-08-29 10:35:14-04  gsl
**	Fix problem where re-opening a workfile was causing it to use
**	the current WORKVOL. The problem is that the WORKVOL value
**	may have changed since the file was first opened.
**
**	Revision 1.16  1997-04-15 23:12:04-04  gsl
**	Update to use wtrace
**
**	Revision 1.15  1997-04-03 17:01:37-05  gsl
**	Add info trace on native_path
**
**	Revision 1.14  1996-10-08 20:28:36-04  gsl
**	Add wispcfg.h
**	include
**
**	Revision 1.13  1996-09-16 14:43:19-07  gsl
**	Move matchnative() to findexts.c
**
**	Revision 1.12  1996-08-23 14:07:11-07  gsl
**	Combined the logworklib() logic for unix and MSFS and changed
**	to use wisptmpbasedir()
**
**	Revision 1.11  1996-07-15 10:14:00-07  gsl
**	fix use of filext.h
**
**	Revision 1.10  1996-07-10 17:10:41-07  gsl
**	Fixed prototypes and includes for NT
**	combined the msdos and unix logic together and reuse for NT
**
**	Revision 1.9  1996-01-03 06:44:22-08  gsl
**	CHange hard-code numbers into defines
**
**
**			OLD		Written by GSL
**			07/22/92	Added matchnative() to better create the actual file spec. GSL
**
*/
