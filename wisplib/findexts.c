/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/


#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "wdefines.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wcommon.h"

/*
**	Routine:	WL_matchnative()
**
**	Function:	To match the file spec components to the actual native file.
**
**	Description:	This routine attempts to find an existing file that matches the file specs
**			that are supplied.  It does this by adjusting the case and file extention.
**			This routine assumes that the file is supposed to already exist.
**			If the file is not matched then none of the args will be altered.
**
**	Arguments:
**		native_vol	(I)	The Volume component 
**		native_lib	(I/O)	The Library component
**		native_file	(I/O)	The File component
**		native_ext	(I/O)	The Extension component
**		nomodext	(I)	Flag - no modifications of extension allowed
**		is_lib		(I)	Flag - is a lib (no file portion)
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
**	Warnings:	
**			UNIX:	The ext will usually be NULL.
**
**			CISAM: 	Files don't return the ext portion as that is added by COBOL.
**				Check first for ".idx" before ".dat"  and if found then DON'T return it.
**				This means that READFDR, RENAME, SCRATCH, FILECOPY etc still have 
**				to deal with ".idx" extension.
**
**			VISION4: Files have 2 or more "segments" the 1st index has a ".vix" extension.
**
**			Library, file, and ext must be large enough to hold any result.
**
**	History:	07/16/92	Written by GSL
**			07/28/92	Added mode. GSL
**
*/
static char* g_exts[] = 
	{ "",
	".idx",
	".dat",
	".vix",
	".acu",
	".cbx",
#ifdef unix
#ifdef HPUX
	".sl",	/* Look for .sl/.so before .gnt before .int -- same order as MF does*/
#else
	".so", 
#endif
#endif
	".gnt",
	".int",
#ifdef unix
	".sh", 
#endif
#ifdef WIN32
	".bat", 
	".com",
#endif
	".exe",
	".wps",
	".wpr",
	".seq",
	".lis",
	".txt",
	".doc",
	".cob",
	".wcb",
	NULL };

int WL_matchnative(char *native_vol, char *native_lib, char *native_file, char *native_ext, int nomodext, int is_lib )
{
	char	buff[256];
	int	i;
#ifdef unix
	char upper_lib[20], upper_file[20];
#endif

	if (!(is_lib))
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
		WL_upper_string(upper_lib);
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

	if (is_lib) return 1;							/* If looking for a lib we've found it	*/

	/*
	**	Match the CASE of the file and extension
	**		- try uppercase file with no extension
	**		- if nomodext then only thing we can try is to uppercase the filename.
	**		- try lowercase with extensions-list
	**		- try uppercase with extensions-list
	*/
	strcpy(upper_file,native_file);
	WL_upper_string(upper_file);

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
			else if (0 == strcmp(g_exts[i],".vix"))	/* Vision 4 */
			{
				native_ext[0] = '\0';					/* If ".vix" then return NULL		*/
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
			else if (0 == strcmp(g_exts[i],".vix"))	/* Vision 4 */
			{
				native_ext[0] = '\0';					/* If ".vix" then return NULL		*/
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
			if (0 == strcmp(g_exts[i],".idx"))
			{
				native_ext[0] = '\0';					/* If ".idx" then return NULL		*/
			}
			else if (0 == strcmp(g_exts[i],".vix"))	/* Vision 4 */
			{
				native_ext[0] = '\0';					/* If ".VIX" then return NULL		*/
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
**	ROUTINE:	WL_findexts()
**
**	FUNCTION:	Attempts to find a file name by adding known extensions before searching
**
**	DESCRIPTION:	This routine takes a file basename and trys to find the complete filename by adding extensions.
**			It returns 0 if the file was found and 1 if not found.  If the file was found it loads the found
**			filename with possible extension into base_ext.
**
**	ARGUMENTS:	(I)	basename	The base file name that findext( ) is suppose to encounter.
**			(O)	base_ext	If return was 0, then it should contain the basename without extension or
**						the basename concatenated with the first extention from g_ext that caused
**						a match to be found.
**	GLOBALS:	g_exts[i]	holds a list of possible file extentions used at the native platform.
**
**	RETURN:		0	if basename or basename concatenated with any extension mapped by g_ext was found.
**			1	if basename nor basename concatenated with any extension mapped by g_ext could not be found.
**
**	WARNINGS:	
**
*/

int WL_findexts(char* basename, char* base_ext)
{
	int 	i;
	char	base[WISP_FILEPATH_LEN];
	char 	tmp[WISP_FILEPATH_LEN];
	char	*ptr;
	int	not_found;

	not_found = 1;									/* prepare for the worse		*/
	cobx2cstr(base, basename, COB_FILEPATH_LEN);					/* convert basename to a cstr into base */
	strcpy(tmp, splitext(base));							/* strip EXT off if present		*/
	if (*tmp)									/* If there is an extension remove it	*/
	{
		ptr = strrchr(base,'.');
		*ptr = (char)0;
	}

	if ( fexists(base) )								/* Look for basename with no extension.	*/
	{
		strcpy(base_ext, base);
		not_found = 0;
	}
	else
	{
		for (i=0; g_exts[i] != NULL ; ++i)					/* For each extension.			*/
		{
			sprintf(tmp, "%s%s", base,g_exts[i]);				/* Put basename and extension into tmp.	*/
			if ( fexists(tmp) )						/* Does tmp file exist?			*/
			{								/* If it does,				*/
				strcpy(base_ext, tmp);					/* Move the path (tmp) to base_ext.	*/
				not_found = 0;
				break;							/* Break out of this "for" loop.	*/
			}
		}
	}

	return(not_found);								/* return 1 if not found, 0 if found.	*/
}

/*
**	History:
**	$Log: findexts.c,v $
**	Revision 1.25  2003/07/14 17:17:17  gsl
**	fix .so extension to be unix only (except HP-UX)
**	
**	Revision 1.24  2003/03/17 20:32:24  gsl
**	reorder extensions so .acu is before sequential files and .txt .lis .doc are near the end
**	
**	Revision 1.23  2003/01/31 21:53:42  gsl
**	Fix -Wall warnings
**	
**	Revision 1.22  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.21  2003/01/31 17:33:56  gsl
**	Fix  copyright header
**	
**	Revision 1.20  2002/08/21 15:42:56  gsl
**	move .so/.sl before .gnt and .int
**	
**	Revision 1.19  2002/08/20 16:09:51  gsl
**	Add support for Micro Focus Shared Object files .so/.sl
**	
**	Revision 1.18  2002/07/25 17:03:44  gsl
**	MSFS->WIN32
**	
**	Revision 1.17  2002/07/11 14:33:58  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.16  2002/07/10 04:27:38  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.15  2002/06/26 06:26:21  gsl
**	Mode/status bit field changes
**	
**	Revision 1.14  2002/06/21 03:10:36  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.13  1998/08/03 20:46:32  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**	
**	Revision 1.12  1998-05-14 17:02:50-04  gsl
**	Add support for Vision4 .vix files
**
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
