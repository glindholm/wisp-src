static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		      Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include required header files.							*/

#include "idsistd.h"
#include "wfiles.h"
#include "wcommon.h"
#include "werrlog.h"
#include "wfname.h"
#include "filext.h"

char trigpname[256];									/* The native trigger file name.	*/

#ifdef unix
int settrigprog(vol,lib,file)
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
{
	return(SETTRIGPROG(vol,lib,file));
}
#endif

int SETTRIGPROG(vol,lib,file)
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
{
#define		ROUTINE		59300
	int4 	mode;									/* The mode of opening			*/
	char	*ptr;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

#ifdef VMS
	if (WISPFILEXT[0] == ' ' || WISPFILEXT[0] == '\0') setwispfilext("EXE");
#endif
	mode = IS_SUBMIT;
	ptr=wfname(&mode,vol,lib,file,trigpname);					/* Get the native system name.		*/
	*ptr = '\0';									/* Be sure to null terminate.		*/

	return(1);									/* Return Success.			*/
}
/*
**	History:
**	$Log: settrigp.c,v $
**	Revision 1.9  1996-08-19 18:32:55-04  gsl
**	drcs update
**
**
**
*/
