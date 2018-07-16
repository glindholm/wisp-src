			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		      Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include required header files.							*/

#include "wfiles.h"
#include "wcommon.h"
#include "werrlog.h"

extern char *wfname();
extern char WISPFILEXT[39];								/* GLOBAL file extension value.		*/

char trigpname[256];									/* The native trigger file name.	*/

#ifdef unix
settrigprog(vol,lib,file)
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
{
	return(SETTRIGPROG(vol,lib,file));
}
#endif

SETTRIGPROG(vol,lib,file)
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
{
#define		ROUTINE		59300
	long 	mode;									/* The mode of opening			*/
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
