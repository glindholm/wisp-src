static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "wdefines.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wcommon.h"

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
static char* g_exts[] = 
#ifdef unix
	{ "",".idx",".dat",".lis",".txt",".doc",".gnt",".int",".sh", ".com",".exe",".wps",".wpr",".seq",".cbx",".acu",".cob",".wcb",NULL };
#endif
#ifdef VMS
	{           ".DAT",".LIS",".TXT",".DOC",                     ".COM",".EXE",              ".SEQ",              ".COB",".WCB",NULL };
#endif
#ifdef MSFS
	{ "",".IDX",".DAT",".LIS",".TXT",".DOC",".GNT",".INT",".BAT",".COM",".EXE",".WPS",".WPR",".SEQ",".cbx",".acu",".COB",".WCB",NULL };
#endif

int matchnative(int4 mode, char *native_vol, char *native_lib, char *native_file, char *native_ext, int nomodext )
{
	char	buff[256];
	int	i;
#ifdef unix
	char cmd[256], upper_lib[20], upper_file[20];
#endif

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
	findexts:	This routine takes a file basename and trys to find the complete filename by adding extensions.
			It returns 0 if the file was found and 1 if not found.  If the file was found it loads the found
			filename with possible extension into base_ext.
*/

#if defined(unix) || defined(MSFS)

int findexts(char* basename, char* base_ext)
{
	int 	i;
	char	base[100];
	char 	tmp[100];
	char	*ptr;
	int	not_found;

	not_found = 1;
	unloadpad(base,basename,80);
	strcpy(tmp,splitext(base));
	if (*tmp)									/* If there is an extension remove it	*/
	{
		ptr = strrchr(base,'.');
		*ptr = (char)0;
	}

	if ( fexists(base) )								/* Look for basename with no extension.	*/
	{
		strcpy(base_ext,base);
		not_found = 0;
	}
	else
	{
		for (i=0; g_exts[i] != NULL ; ++i)					/* For each extension.			*/
		{
			sprintf(tmp,"%s%s",base,g_exts[i]);				/* Put basename and extension into tmp.	*/
			if ( fexists(tmp) )						/* Does tmp file exist?			*/
			{								/* If it does,				*/
				strcpy(base_ext,tmp);					/* Move the path (tmp) to base_ext.	*/
				not_found = 0;
				break;							/* Break out of this "for" loop.	*/
			}
		}
	}

	return(not_found);								/* return 1 if not found, 0 if found.	*/
}

#endif /* unix || MSFS */
/*
**	History:
**	$Log: findexts.c,v $
**	Revision 1.11  1996-09-16 17:33:21-04  gsl
**	Move matchnative() from wfname.c to here so we can localize g_ext[] the
**	list of valid extensions
**	Add .ACU and .CBX as valid extensions for Acucobol object files
**
**	Revision 1.10  1996-08-19 15:32:21-07  gsl
**	drcs update
**
**
**
*/
