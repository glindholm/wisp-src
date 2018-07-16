/*
******************************************************************************
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
******************************************************************************
*/

/*
**	File:		win32wrun.c
**
**	Project:	wisp/utils
**
**	RCS:		$Source:$
**
**	Purpose:	Frontend to frontend COBOL runtime system.  Startup in this file
**                      is handled by a Winmain function to prevent auto creation of a console
**                      window;  the real wrun.c code is called from here
**
*/

#ifdef WIN32
#include <windows.h>
#include "wmalloc.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
int wrunmain(int argc, char **argv);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
static void win32getargs(LPSTR lpszCommandLine, int *argc, char ***argv);

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCommandLine, int cmdShow)
{
	int argc;
	char **argv;

	win32getargs(lpszCommandLine, &argc, &argv);

	return wrunmain(argc,argv);
}

/*
**	ROUTINE:	win32getargs()
**
**	FUNCTION:	convert a WIN32 command line arg string into an argv array
**
**	ARGUMENTS:	LPSTR   lpszCommandLine      the command string
**                      int*    argc                 receiver for arg count
**                      char*** argv                 receiver for arg array
**
**	GLOBALS:	none
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
static void win32getargs(LPSTR lpszCommandLine, int *argc, char ***argv)
{
	char *cmdcopy, *cmdptr;
	int argidx;
	
	/*
	** count the args in the command line (starting at 1, first arg
	** is program name)
	*/
	argidx=1;
	cmdptr = cmdcopy = wisp_strdup(lpszCommandLine);
	do
	{
		/*
		** skip spaces
		*/
		while (*cmdptr==' ')
		{
			++cmdptr;
		}
		/*
		** exit if there is a null after the spaces (end of command line)
		*/
		if (*cmdptr == '\0')
		{
			break;
		}
		/*
		** now look for next space or null
		*/
		while (*cmdptr!=' ' && *cmdptr!='\0')
		{
			++cmdptr;
		}
		/*
		** count the arg
		*/
		++argidx;
		/*
		** exit loop if at end
		*/
		if (*cmdptr == '\0')
		{
			break;
		}
		++cmdptr;
			
	} while(TRUE);

	/*
	** now grab char ** array for the args we counted
	*/
	*argv = wisp_calloc(argidx, sizeof(char*));

	/*
	** start the loop at 1 again (index zero is progname)
	*/
	argidx=1;

	/*
	** store the program name (wrun) as arg 0
	*/
	(*argv)[0] = wisp_strdup("wrun");

	/*
	** reinitialize the cmdptr
	*/
	cmdptr = cmdcopy;

	/*
	** this loop is the same as above, except that before counting
	** past the non-whitespace we save a pointer in our array
	*/
	do
	{
		while (*cmdptr==' ')
		{
			++cmdptr;
		}
		if (*cmdptr == '\0')
		{
			break;
		}
		(*argv)[argidx]=cmdptr;
		while (*cmdptr!=' ' && *cmdptr!='\0')
		{
			++cmdptr;
		}
		++argidx;
		if (*cmdptr == '\0')
		{
			break;
		}
		*cmdptr='\0';
		++cmdptr;
	} while(TRUE);
	*argc = argidx;
}
/*
**	ROUTINE:	wrun_message_box()
**
**	FUNCTION:	message box used to show user the args used by wrun.
**                      this is called if wrun is run with no args
**
**	ARGUMENTS:	char *line1    displayed as title
**                      char *line2    displayed as contents
**
**	GLOBALS:	
**
**	RETURN:		
**
**	WARNINGS:	
**
*/

void wrun_message_box(char *line1, char *line2)
{
		MessageBox(NULL, line2, line1, MB_OK | MB_ICONINFORMATION
			|MB_SETFOREGROUND | MB_TOPMOST);
}

#endif

/*
**	History:
**	$Log: win32wrn.c,v $
**	Revision 1.6  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.5  2002/07/02 21:15:40  gsl
**	Rename wstrdup
**	
**	Revision 1.4  1999/09/13 19:55:43  gsl
**	fix main() prototype
**	
**	Revision 1.3  1997-01-06 17:35:54-05  jockc
**	forgot to null terminate the argv strings (ahem)
**
**	Revision 1.2  1996-11-12 14:55:48-08  jockc
**	added messagebox function (for wrun w/ no args) and
**	fixed broken win32getargs
**
**	Revision 1.1  1996-11-07 09:00:45-08  jockc
**	Initial revision
**
*/
