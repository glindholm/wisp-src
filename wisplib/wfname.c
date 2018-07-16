			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wfname.c
**
**	Purpose:	To construct the native file spec based on the Wang style file spec.
**			This is used by ALL WISP routines to construct real file names from Wang templates.
**
**	Routines:	wfname()	Construct the real file name.
**			wtname()	Generate temp names (##).
**			vmsfilechars()	Translate VMS special file characters.
**			wlgtrans()	LGMAP logical volume translation. (UNIX)
**			wlgtrans()	LGMAP logical volume translation. (MSDOS)
**			logworklib()	Log the work librarys generated.
**			matchnative()	Match native file spec to actual file.
**
**	History:
**			OLD		Written by GSL
**			07/22/92	Added matchnative() to better create the actual file spec. GSL
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

#ifdef VMS
#include <ssdef.h>
#endif

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#ifndef VMS	/* unix or MSDOS */
#include <malloc.h>
#include <sys/types.h>
#include <fcntl.h>
#endif

#ifdef MSDOS
#include <sys/stat.h>
#include <io.h>
#endif

#include "idsistd.h"
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"
#include "werrlog.h"
#include "cobrun.h"

extern char WISPFILEXT[39];								/* GLOBAL file extension value.		*/

#ifdef VMS
#define  PRINT_FILE_EXT 	".LIS"
#define  SUBMIT_FILE_EXT 	".COM"
#define  DEFAULT_FILE_EXT	".DAT"								/* Used to be DATA_FILE_EXT	*/
#endif	/* VMS */
#ifdef unix
#define  PRINT_FILE_EXT 	""
#define  SUBMIT_FILE_EXT 	""
#define  DEFAULT_FILE_EXT	""								/* Used to be DATA_FILE_EXT	*/
#endif	/* unix */
#ifdef MSDOS
#define  PRINT_FILE_EXT 	".LIS"
#define  SUBMIT_FILE_EXT 	".BAT"
#define  DEFAULT_FILE_EXT	".DAT"								/* Used to be DATA_FILE_EXT	*/
#endif	/* MSDOS */

static int wtname();
static int matchnative();

#define ROUTINE 77500

char *wfname(mode,p_vol,p_lib,p_file,native_path)					/* generate a local file name		*/
											/* returns ptr to next avail char pos	*/
int4 *mode;										/* the mode flags			*/
char *p_vol;										/* the wang volume name			*/
char *p_lib;										/* the wang library name		*/
char *p_file;										/* the wang file name			*/
char *native_path;									/* the buffer for the local name	*/
{
	char wang_file[8], wang_lib[8], wang_vol[6];					/* Wang format copies of file/lib/vol.	*/
	char native_file[20], native_lib[20], native_vol[80], native_ext[40];		/* Native format of file/lib/vol/ext.	*/
	char work_string[80], buff[256];
	int i,len;
	int	nomodext;								/* No modifcations of ext allowed	*/

	/*
	**	INITIALIZE
	*/

	werrlog(ERRORCODE(1),p_vol,p_lib,p_file,0,0,0,0,0);

      	wpload();		 							/* load the usage constants		*/

	memset(native_path, ' ', NAME_LENGTH);						/* Clear out the 80 character filename.	*/


	/*
	**	GET LOCAL AND SAVE COPIES
	*/

	memcpy(wang_vol,p_vol,6);							/* Grab local copies.			*/
	memcpy(wang_lib,p_lib,8);
	memcpy(wang_file,p_file,8);

	leftjust(wang_vol,6);								/* Left justify file/lib/vol.		*/
	leftjust(wang_lib,8);
	leftjust(wang_file,8);

	if (*mode & IS_LIB)								/* If IS_LIB then blank filename	*/
	{
		memset(wang_file, ' ', 8 );
	}

	/*
	**	LOAD VOL & LIB FROM DEFAULTS
	*/

	if ( wang_vol[0] == ' ' )							/* If volume is blank get default.	*/
	{
		if (*mode & IS_PRINTFILE)
		{
			get_defs(DEFAULTS_SV,wang_vol);
		}
		else if (*mode & IS_SUBMIT)
		{
			get_defs(DEFAULTS_PV,wang_vol);
		}
		else if (*mode & IS_OUTPUT)
		{
			get_defs(DEFAULTS_OV,wang_vol);
		}
		else /* INPUT */
		{
			get_defs(DEFAULTS_IV,wang_vol);
		}
		leftjust(wang_vol,6);
	}

	if ( wang_lib[0] == ' ' )							/* If library is blank get default.	*/
	{
		if (*mode & IS_PRINTFILE)
		{
			get_defs(DEFAULTS_SL,wang_lib);
		}
		else if (*mode & IS_SUBMIT)
		{
			get_defs(DEFAULTS_PL,wang_lib);
		}
		else if (*mode & IS_OUTPUT)
		{
			get_defs(DEFAULTS_OL,wang_lib);
		}                                               
		else /* INPUT */
		{
			get_defs(DEFAULTS_IL,wang_lib);
		}
		leftjust(wang_lib,8);
	}
      

	/*
	**	LOAD EXTENSION                                                          
	*/

	nomodext = 0;
	leftjust(WISPFILEXT,39);
	if (WISPFILEXT[0] != ' ' && WISPFILEXT[0] != 0)					/* If WISPFILEXT then load & null term	*/
	{
		native_ext[0] = '.';
		for( i=0; i<39 && WISPFILEXT[i] != ' '; i++ )
		{
			native_ext[i+1] = WISPFILEXT[i];
		}
		native_ext[i+1] = '\0';
		nomodext = 1;								/* No modification of ext allowed	*/
	}
	else if (*mode & IS_PRINTFILE)							/* Use PRINT file extension		*/
	{
		strcpy( native_ext, PRINT_FILE_EXT );
	}
	else if (*mode & IS_SUBMIT)							/* Use SUBMIT file extension		*/
	{
		strcpy( native_ext, SUBMIT_FILE_EXT );
	}
	else if (*mode == IS_NOEXTENSION)						/* Use No file extension		*/
	{
		native_ext[0] = '\0';
		nomodext = 1;								/* No modification of ext allowed	*/
	}
	else										/* Use DEFAULT file extension		*/
	{
		strcpy( native_ext, DEFAULT_FILE_EXT );
	}

	memset(WISPFILEXT,' ',39);							/* Always reset the file extension.	*/

	if (*mode & IS_LIB)								/* If IS_LIB then blank the extension	*/
	{
		native_ext[0] = '\0';
	}
	

	/*
	**	HANDLE WORKFILES
	*/

	get_defs(DEFAULTS_WL,buff);
	if ( memcmp(wang_lib,buff,8) == 0 )
	{
		*mode |= IS_WORK;
	}

	if ( wang_file[0] == '#' || wang_file[0] == '%' )				/* If tempfile				*/
	{
		if ( !(*mode & IS_PRINTFILE) )						/* and not printfile			*/
		{
			*mode |= IS_WORK;						/* Set the IS_WORK bit			*/
		}
	}

	if (*mode & IS_WORK)
	{
		get_defs(DEFAULTS_WV,wang_vol);						/* It is forced into work		*/
		get_defs(DEFAULTS_WL,wang_lib);

		leftjust(wang_vol,6);                                            
		leftjust(wang_lib,8);

		*mode |= IS_SCRATCH;
	}
	

	/*
	**	CLEANUP VOL & LIB & FILE
	*/


#ifndef VMS	/* unix or MSDOS */
	if ( wang_vol[0] == ' ' )							/* If volume is blank ....		*/
	{
		wang_vol[0] = '.';							/* Replace with dot.			*/
	}
                                                                                           
	if ( wang_lib[0] == ' ' )				      			/* If volume is blank ....		*/
	{
		wang_lib[0] = '.';							/* Replace with dot.			*/
	}
#endif	/* unix or MSDOS */



	/*
	**	PERPARE TEMP VOL & LIB & FILE FOR PATH GENERATION
	*/


#ifdef VMS
                                                                                                      
	if ( vmsfilechars( wang_vol,  native_vol,  6, 0 ) )				/* Trans volume (No Wildcards)		*/
	{
		werrlog(ERRORCODE(2), wang_vol,0,0,0,0,0,0,0);				/* Wildcard found.			*/
		strcpy(native_vol, "");							/* Ignore volume.			*/
	}
	else if (native_vol[0])								/* If there is a volume, add ":"	*/
	{
		strcat(native_vol, ":");
	}

	vmsfilechars( wang_lib,  work_string,  8, 1 );					/* Trans lib	(Wildcards allowed)	*/

	if (work_string[0])								/* If there is a library named...	*/
		sprintf(native_lib,"[%s]",work_string);					/* Put it in brackets.			*/
	else										/* Otherwise...				*/
		native_lib[0] = '\0';							/* Make a null string.			*/

	vmsfilechars( wang_file, native_file, 8, 1 );					/* Trans file	(Wildcards allowed)	*/

#endif	/* VMS */

#ifdef unix
	wlgtrans( wang_vol, native_vol );					       	/* Load vol with translation.		*/
	strcat(native_vol,"/");

	for( i=0; i<8 && wang_lib[i] != ' '; i++ )					/* Load lib into native_lib		*/
	{
		native_lib[i] = wang_lib[i];
	}
	native_lib[i] = '\0';
	strcat(native_lib,"/");

	for( i=0; i<8 && wang_file[i] != ' '; i++ )					/* Load file into native_file		*/
	{
		native_file[i] = wang_file[i];
	}
	native_file[i] = '\0';


	if ( ! (*mode & IS_CASE_SEN) )							/* Convert lib & file to lowercase	*/
	{
		lower_string(native_lib);
		lower_string(native_file);
	}

	if ( native_lib[0] == '#' ) native_lib[0] = '%';			       	/* Convert leading # -> %		*/
	if ( native_file[0] == '#' ) native_file[0] = '%';				/* Convert leading # -> %		*/
#endif	/* unix */

#ifdef MSDOS
	wlgtrans( wang_vol, native_vol );					       	/* Load vol with translation.		*/
	if( native_vol[0] == '\0' )							/* If no volume,			*/
	{
		strcpy(native_vol,".\\");						/* Start at the current directory	*/
	}
	else										/* If there is a volume,		*/
	{
		strcat(native_vol,"\\");						/* Set it for a subdirectory or file.	*/
	}

	for( i=0; i<8 && wang_lib[i] != ' '; i++ )					/* Load lib into native_lib		*/
	{
		native_lib[i] = wang_lib[i];
	}
	native_lib[i] = '\0';
	if( native_lib[0] != '\0' )							/* If there is a library,		*/
		strcat(native_lib,"\\");						/* Set it for a subfile.		*/

	for( i=0; i<8 && wang_file[i] != ' '; i++ )					/* Load file into native_file		*/
	{
		native_file[i] = wang_file[i];
	}
	native_file[i] = '\0';

	if ( native_lib[0] == '#' ) native_lib[0] = '%';			       	/* Convert leading # -> %		*/
	if ( native_file[0] == '#' ) native_file[0] = '%';				/* Convert leading # -> %		*/

#endif	/* MSDOS */

	

	/*
	**	HANDLE SPECIAL CASES
	*/

#ifdef VMS
	if ( memcmp(wang_lib,"$SCRATCH",8)==0 )						/* is it the $SCRATCH library?		*/
	{	
		strcpy(native_vol,"SYS$SCRATCH:");					/* put SYS$SCRATCH into VOLUME !!!!	*/
		strcpy(native_lib,"");							/* clear library			*/
		*mode |= IS_SCRATCH;
	}

	get_defs(DEFAULTS_PM,buff);
	if ((*mode & IS_PRINTFILE) && buff[0] == 'O')					/* Printfiles may need LP: prefix	*/
	{
		int4	prtnum;
		get_defs(DEFAULTS_PR,&prtnum);
		sprintf(native_vol,"LP%d:",prtnum);					/* put LPxxx: into VOLUME		*/
		strcpy(native_lib,"");							/* clear library			*/
	}
#endif	                                                                                    

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
			native_vol, native_lib, native_file, native_ext, mode );	/* Grenerate native_file name.		*/

		*mode |= IS_TEMP;

	}
	

	/*
	**	ASSEMBLE THE FILE NAME
	*/

	matchnative(*mode, native_vol, native_lib, native_file, native_ext, nomodext );	/* Match the file specs to the disk.	*/

	sprintf(native_path,"%s%s%s%s", native_vol, native_lib, native_file, native_ext );

	len = strlen(native_path);
	native_path[len] = ' ';								/* Remove null-termination		*/
	

	/*
	**	LOGGING OF WORK LIBRARIES
	*/

	if ( (*mode & IS_WORK) && (*mode & IS_OUTPUT) )
	{
		char native_worklib[80];
		static	char	work_vol[6] = {0};

		if ( memcmp(wang_vol, work_vol, 6) != 0 )
		{
			memcpy(work_vol, wang_vol, 6);
			sprintf(native_worklib, "%s%s", native_vol, native_lib );

			logworklib( native_worklib );

		}
	}	


	/*
	**	BACKFILL THE FILE SPECIFICATIONS
	*/


	if ( *mode & IS_BACKFILL )
	{
		memcpy(p_vol,wang_vol,6);						/* Backfill the file/lib/vol.		*/
		memcpy(p_lib,wang_lib,8);
		memcpy(p_file,wang_file,8);
	}


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

static wtname(wang_vol, wang_lib, wang_file, native_vol, native_lib, native_file, native_ext, mode)
char *wang_vol, *wang_lib, *wang_file;
char *native_vol, *native_lib, *native_file, *native_ext;
int4 *mode;
{
	char	prefix[5];								/* The up to 4 char prefix.		*/
	char	tempname[20];
	char	path[80];
	char	*ptr, *fptr;
	int	i;
	int4	argcnt=8, starter, counter, filecount;
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
	wswap(&starter);
	wswap(&counter);
	wswap(&filecount);
	rtype = 'A';
	wvaset( &argcnt );
	FIND( template, wang_lib, wang_vol, &starter, &counter, recv, &filecount, &rtype );

	wswap(&filecount);
	if ( filecount == 0 ) 								/* If none then start at Zero.		*/
	{
		tseq = 0;
	}
	else
	{
		starter = filecount;							/* Get the last file****.		*/
		counter = 1;
		filecount = 0;
		wswap(&starter);
		wswap(&counter);
		wswap(&filecount);
		wvaset( &argcnt );
		FIND( template, wang_lib, wang_vol, &starter, &counter, recv, &filecount, &rtype );

		wswap(&filecount);
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

	sprintf(path,"%s%sXXX.XXX", native_vol, native_lib);				/* create a dummy path for makepath	*/
	makepath( path );								/* make the dir path up to file		*/


	for(;;)
	{
		if (tseq > 9999) tseq=0;						/* If too large then wrap around.	*/

		sprintf( tempname, "%s%04d", prefix, tseq );				/* Build the tempfile name		*/

		memset( wang_file, ' ', 8 );
		memcpy( wang_file, tempname, strlen(tempname) );
		strcpy( native_file, tempname );

#ifdef VMS
		vmsfilechars( tempname, native_file, 8, 1 );				/* Trans file	(Wildcards allowed)	*/
#endif
#ifdef unix
		if ( ! (*mode & IS_CASE_SEN) )						/* Convert lib & file to lowercase	*/
			lower_string( native_file );
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

	fdesc = creat( path, 0666 );							/* Reserve the filespec			*/
	if ( fdesc != -1 )
	{
		close( fdesc );
	}


}

#ifdef VMS
static int vmsfilechars( instr, outstr, inlen, wildcard )				/* VMS file character translation	*/
char	*instr;
char	*outstr;
int	inlen;
int	wildcard;
{
	int	i,j,rc;

	rc = 0;

	for( i=0,j=0; i<inlen && instr[i] != ' ' && instr[i] != '\0'; i++)
	{
		if ( instr[i] == '#' )							/* Translate   # -> $  (was N_)		*/
		{
			/*
			outstr[j++] = 'N';
			outstr[j++] = '_';
			*/
			outstr[j++] = '$';
		}
		else if ( instr[i] == '@' )						/* Translate   @ -> _ (was A_)		*/
		{
			/*
			outstr[j++] = 'A';
			outstr[j++] = '_';
			*/
			outstr[j++] = '_';
		}
		else if ( instr[i] == '*' )						/* Translate   * -> %			*/
		{
			outstr[j++] = '%';
			if ( ! wildcard )
			{
				rc = 1;
			}
		}
		else if ( instr[i] == '?' )						/* Translate   ? -> *			*/
		{
			outstr[j++] = '*';
			if ( ! wildcard )
			{
				rc = 1;
			}
		}
		else
		{
			outstr[j++] = instr[i];
		}
	}

	outstr[j] = '\0';								/* Null terminate it.			*/

	return( rc );
}
#endif	/* VMS */

#ifndef VMS
/*
	wlgtrans:	LOGICAL TRANSLATOR
			- search the logical list for in_str and return translation in out_str.
			- if no match found then move in_str to out_str.
			- in_str is up to 6 chars and is not null terminated
			- return 1 if a translation was done; 0 if no translation
			- the logicals are case sensitive
			- out_str will be null terminated
*/
int	wlgtrans( in_str, out_str )
char	*in_str;
char	*out_str;
{
	int	i;
	char	in_temp[6+1];
	logical_id	*logical_ptr;

	logical_ptr = get_logical_list();

	i=0;
	while( i<6 && in_str[i] != ' ' && in_str[i] != '\0' )					/* Put vol into string.		*/
	{
		in_temp[i] = in_str[i];
		i++;
	}
	in_temp[i] = '\0';

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
		else
		{
			logical_ptr = (logical_id *)logical_ptr->next;				/* follow down the list.	*/
		}
	}
}
#endif	/* !VMS */

logworklib( worklib )
char	*worklib;
{
#ifdef unix
	int	fd;
	char	*wf;

	worklib[strlen(worklib)-1] = '\0';						/* remove trailing '/'			*/
	wf = "/usr/tmp/WLIBLIST";
	fd = open( wf, O_WRONLY | O_CREAT | O_APPEND, 0666 );
	if ( fd == -1 )
	{
		werrlog( ERRORCODE(4),wf, errno,0,0,0,0,0,0,0 );
		return;
	}
	write( fd, worklib, strlen(worklib) );
	write( fd, "\n", 1 );
	close( fd );
	chmod(wf,0666);
#endif	/* unix */

#ifdef MSDOS
	int	fd;
	char	*wf;

	worklib[strlen(worklib)-1] = '\0';						/* remove trailing '\\'			*/
	wf = "C:\\TMP\\WLIBLIST";							/* This should not be hard coded.	*/
	makepath(wf);									/* Ensure parent dirs exist		*/
	fd = open( wf, (O_WRONLY | O_CREAT | O_APPEND), (S_IREAD | S_IWRITE) );
	if ( fd == -1 )
	{
		werrlog( ERRORCODE(4),wf, errno,0,0,0,0,0,0,0 );
		return(0);
	}
	write( fd, worklib, strlen(worklib) );
	write( fd, "\n", 1 );
	close( fd );
	chmod( wf, (S_IREAD | S_IWRITE) );
#endif	/* MSDOS */
}

/*
**	Routine:	matchnative()
**
**	Function:	To match the file spec components to the actual native file.
**
**	Description:	This routine attempts to find an existing file that matches the file specs
**			that are supplied.  It does this by adjusting the case and file extention.
**			This routine assumes that the file is supposed to already exist.
**			If the file is not matched then none of the args will be altered.
**
**	Arguments:
**		mode		(I)	The mode
**		native_vol	(I)	The Volume component 
**		native_lib	(I/O)	The Library component
**		native_file	(I/O)	The File component
**		native_ext	(I/O)	The Extension component
**		nomodext	(I)	Flag - no modifications of extension allowed
**
**					VOLUME		LIBRARY		FILE		EXT
**				VMS: 	"xxxxxx:"	"[xxxxxx]"	"xxxxxx"	".xxx"
**				UNIX:	"xxxxxx/"	"xxxxxxx/"	"xxxxxx"	".xxx"
**				DOS:	"xxxxxx\"	"xxxxxxx\"	"xxxxxx"	".xxx"
**
**	Return:
**		1	A file match was made.
**		0	File was not found.
**
**	Warnings:	VMS: 	The volume and library may be NULL.
**			UNIX:	The ext will usually be NULL.
**
**			CISAM: 	Files don't return the ext portion as that is added by COBOL.
**				Check first for ".idx" before ".dat"  and if found then DON'T return it.
**				This means that READFDR, RENAME, SCRATCH, FILECOPY etc still have to deal with ".idx" extension.
**
**			Library, file, and ext must be large enough to hold any result.
**
**	History:	07/16/92	Written by GSL
**			07/28/92	Added mode. GSL
**
*/
#ifdef unix
char	*g_exts[] = 
	{ "",".idx",".dat",".lis",".txt",".doc",".gnt",".int",".sh", ".com",".exe",".wps",".wpr",".seq",".cob",".wcb",NULL };
#endif
#ifdef VMS
char	*g_exts[] = 
	{           ".DAT",".LIS",".TXT",".DOC",                     ".COM",".EXE",              ".SEQ",".COB",".WCB",NULL };
#endif
#ifdef MSDOS
char	*g_exts[] = 
	{ "",".IDX",".DAT",".LIS",".TXT",".DOC",".GNT",".INT",".BAT",".COM",".EXE",".WPS",".WPR",".SEQ",".COB",".WCB",NULL };
#endif

static int matchnative(mode,native_vol, native_lib, native_file, native_ext, nomodext )
int4	mode;
char	*native_vol;
char	*native_lib;
char	*native_file;
char	*native_ext;
int	nomodext;
{
	char	buff[256], cmd[256], upper_lib[20], upper_file[20];
	int	i;

	if (!(mode & IS_LIB))
	{
		sprintf(buff,"%s%s%s%s", native_vol, native_lib, native_file, native_ext );
		if (fexists(buff)) return 1;
	}

#ifdef unix
	/*
	**	Match the CASE of the library.
	*/
	sprintf(buff,"%s%s",native_vol,native_lib);
	if (!fexists(buff))
	{
		strcpy(upper_lib,native_lib);
		upper_string(upper_lib);
		sprintf(buff,"%s%s",native_vol,upper_lib);				/* Try uppercase library		*/
		buff[strlen(buff)-1] = '\0';						/* Remove trailing '/'			*/
		if (fexists(buff))
		{
			strcpy(native_lib,upper_lib);					/* Replace library with uppercase	*/
		}
		else
		{
			return 0;							/* Library not found			*/
		}

		sprintf(buff,"%s%s%s%s", native_vol, native_lib, native_file, native_ext );
		if (fexists(buff)) return 1;
	}

	if (mode & IS_LIB) return 1;							/* If looking for a lib we've found it	*/

	/*
	**	Match the CASE of the file and extension
	**		- try uppercase file with no extension
	**		- if nomodext then only thing we can try is to uppercase the filename.
	**		- try lowercase with extensions-list
	**		- try uppercase with extensions-list
	*/
	strcpy(upper_file,native_file);
	upper_string(upper_file);

	sprintf(buff,"%s%s%s%s", native_vol, native_lib, upper_file, native_ext); 	/* Try uppercase			*/
	if (fexists(buff))
	{
		strcpy(native_file,upper_file);						/* Use uppercase file			*/
		return 1;
	}

	if (nomodext)									/* If no mods of ext allowed		*/
	{
		return 0;								/* not found				*/
	}

	for(i=0; g_exts[i] != NULL; i++)						/* Try each of the extensions		*/
	{
		sprintf(buff,"%s%s%s%s", native_vol, native_lib, native_file, g_exts[i] );
		if (fexists(buff))							/* Try with lowercase name		*/
		{
			if (0 == strcmp(g_exts[i],".idx"))
			{
				native_ext[0] = '\0';					/* If ".idx" then return NULL		*/
			}
			else
			{
				strcpy(native_ext,g_exts[i]);
			}
			return 1;
		}

		sprintf(buff,"%s%s%s%s", native_vol, native_lib, upper_file,  g_exts[i] );
		if (fexists(buff))							/* Try with uppercse name		*/
		{
			strcpy(native_file,upper_file);					/* Use the uppercase file name		*/
			if (0 == strcmp(g_exts[i],".idx"))
			{
				native_ext[0] = '\0';					/* If ".idx" then return NULL		*/
			}
			else
			{
				strcpy(native_ext,g_exts[i]);
			}
			return 1;
		}
	}
#else
	if (nomodext)									/* If no mods of ext allowed		*/
	{
		return 0;								/* not found				*/
	}

	for(i=0; g_exts[i] != NULL; i++)						/* Try each of the extensions		*/
	{
		sprintf(buff,"%s%s%s%s", native_vol, native_lib, native_file, g_exts[i] );
		if (fexists(buff))
		{
			if (0 == strcmp(g_exts[i],".IDX"))
			{
				native_ext[0] = '\0';					/* If ".idx" then return NULL		*/
			}
			else
			{
				strcpy(native_ext,g_exts[i]);
			}
			return 1;
		}
	}
#endif
	return 0;									/* not found				*/
}

/*
**	Routine:	wfexists()
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
int wfexists(file,lib,vol)
char	*file, *lib, *vol;
{
	int4	mode;
	char	*ptr;
	char	filename[80];
	int	outexists;

	mode = 0;
	ptr = wfname(&mode, vol, lib, file, filename);				/* Construct the filename.			*/
	*ptr = (char)0;

	outexists = 0;
	if (fexists(filename))							/* Check if file exists				*/
	{
		outexists = 1;
	}
#if defined(unix) || defined(MSDOS)
	if (!outexists && !hasext(filename))					/* Check if CISAM				*/
	{
		strcat(filename,".idx");
		if (fexists(filename))						/* Check if file exists				*/
		{
			outexists = 2;
		}
	}
#endif
	return outexists;
}