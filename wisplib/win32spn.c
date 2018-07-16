static char copyright[]="Copyright (c) 1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		win32spn.c
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Spawn a new process;  old process minimizes and sleeps.  New
**                      process' console window pops up in the same place
**
**	Routines:	
**	win32spawnvp()
**	win32spawnlp()
*/
#ifdef WIN32

/*
**	Includes
*/
#include <windows.h>
#include <stdio.h>
#include <process.h>
#include "werrlog.h"
#include "win32spn.h"
#include "win32err.h"
#include "wmalloc.h"

/*
**	Structures and Defines
*/
struct newenvstr
{
	char *value;
	int len;
	struct newenvstr *next;
};

/*
**	Globals and Externals
*/
extern HWND vraw_get_console_hWnd(void);
extern WORD vrawgetattribute(int attr);

HANDLE opentempfile(char *path);


/*
**	Static data
*/
static struct newenvstr *lpNewEnv = NULL;
static int nNewEnvSize = 0;

/*
**	Static Function Prototypes
*/

static char *win32BuildNewEnv(void);
static void win32DeleteEnv(void);

/*
**	ROUTINE:	win32spawnvp()
**
**	FUNCTION:	spawn a program
**
**	DESCRIPTION:	convert char*[] to char[], and call win32spawnlp to
**                      do the rest
**
**	ARGUMENTS:	sh_parm       char* array of points to program plus args
**
**                      Mode          see win32spawnlp()
**
**	GLOBALS:	
**
**	RETURN:		return code from spawned process
**
**	WARNINGS:	?
**
*/
#define ROUTINE 28000
int win32spawnvp(char *sh_parm[], int Mode)
{
	char *cmdline;
	int cmd_len;
	int idx;
	int rc;

	/*
	**	Calculate the length of the shell command then malloc it.
	*/
	for (idx=0, cmd_len=1; sh_parm[idx]; ++idx)
	{					
		cmd_len += strlen(sh_parm[idx])+1;
	}
	cmdline = malloc(cmd_len);
	/*
	**	Assemble the shell command
	*/
	strcpy(cmdline, sh_parm[0]);
	for (idx=1; sh_parm[idx]; ++idx)
	{
		strcat(cmdline," ");
		strcat(cmdline,sh_parm[idx]);
	}

	rc =  win32spawnlp(NULL, cmdline, Mode);
	free(cmdline);
	return rc;
}
/*
**	ROUTINE:	win32spawnlp()
**
**	FUNCTION:	spawn a program
**
**	DESCRIPTION:	Minimize current process, then spawn a new one.  Optionally
**                      wait for new process to finish and return status to caller.
**                      args specified by pointer to  char array
**
**	ARGUMENTS:	cmd           char* to command or NULL (see CreateProcess)
**                      args          char* command line args or complete command
**                                          line (if cmd==NULL)
**                      Mode          int flags for spawn mode:
**				SPN_HIDE_PARENT	   attempt to hide behind new console
**				SPN_HIDE_CHILD     attempt to suppress new console (for
**                                                 background jobs)
**				SPN_WAIT_FOR_CHILD wait on child process to finish
**				SPN_DETACH_CHILD   detach new process from our console
**
**	GLOBALS:	
**
**	RETURN:		return code from spawned process
**
**	WARNINGS:	?
**
*/
int win32spawnlp(char *cmd, char *args, int Mode)
{
	PROCESS_INFORMATION pinfo;
	STARTUPINFO sinfo;
	BOOL bSuccess;
	DWORD dwRetcode, dwCreationFlags;
	char *lpEnv;
	BOOL bInhertHandles = TRUE;
	char tempout[MAX_PATH], temperr[MAX_PATH];

	wtrace("WIN32SPAWNLP","ENTRY","Cmd=[%s] Args=[%s]", cmd ? cmd:"(nil)", args ? args:"(nil)");

	/*
	**	Setup Startup info flags
	*/
	memset(&sinfo,'\0',sizeof(sinfo));
	sinfo.cb = sizeof(sinfo);

	sinfo.lpReserved=NULL;
	sinfo.lpDesktop=NULL;
	sinfo.lpTitle=NULL;
	sinfo.dwFlags = 0;

	/*
	**	Set up fill color for white background with black text
	*/
	sinfo.dwFlags |= STARTF_USEFILLATTRIBUTE;
	sinfo.dwFillAttribute = vrawgetattribute(0);

	/*
	**	Setup CreationFlags
	*/
	dwCreationFlags = NORMAL_PRIORITY_CLASS;
	if (Mode & SPN_DETACH_CHILD)
	{
		bInhertHandles = FALSE;
		dwCreationFlags |= CREATE_NEW_CONSOLE;

//		Spawning a CONSOLE process with DETACHED_PROCESS
//		gives an access violation.
//		dwCreationFlags |= DETACHED_PROCESS;

		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;

	}
	else if (Mode & SPN_HIDE_CHILD)
	{
		dwCreationFlags |= CREATE_NEW_CONSOLE;
	}

	/*
	** prepare the child's console window to cover our
	** window
	*/
	if (Mode & SPN_HIDE_PARENT)
	{
		sinfo.dwFlags |= STARTF_USECOUNTCHARS | STARTF_USEPOSITION;
		sinfo.dwX= 0;
		sinfo.dwY= 0;
		sinfo.dwXCountChars = 80;
		sinfo.dwYCountChars = 24;

		if (vraw_get_console_hWnd())
		{
			WINDOWPLACEMENT wndpl;

			wndpl.length = sizeof(wndpl);
			bSuccess = GetWindowPlacement(vraw_get_console_hWnd(), &wndpl);

			if (bSuccess)
			{
				char	envstring[80];

				sinfo.dwX= wndpl.rcNormalPosition.left;
				sinfo.dwY= wndpl.rcNormalPosition.top;

				/*
				**	Set the Window position in the environment
				**	so the new process get position its window correctly.
				*/
				sprintf(envstring,"CONWINPOS=%d:%d", sinfo.dwX, sinfo.dwY);
				win32SetNewEnv(envstring);
			}
			
		}

		/*
		**	The child window starts hidden so it can be moved and
		**	resized without screen flickering.
		*/
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;

	}
	if (Mode & SPN_HIDE_CHILD)
	{
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;
		
		if (Mode & SPN_WAIT_FOR_CHILD)
		{
			/*
			**	We are running a hidden child process and waiting for it to complete.
			**	This is used by SUBMIT to issues the QSUBMIT or REXEC command.
			**	We want to capture the stdout and stderr output to temp files
			**	which we will log after it is finished.
			*/
			bInhertHandles = TRUE;
			sinfo.dwFlags |= STARTF_USESTDHANDLES;
			sinfo.hStdInput  = INVALID_HANDLE_VALUE;
			sinfo.hStdOutput = opentempfile(tempout);
			sinfo.hStdError  = opentempfile(temperr);
		}
	}
	if (!vraw_get_console_hWnd())
	{
		/*
		**	If not console handle them assume COSTAR and hide the child window.
		*/
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;
		
	}

	if (Mode & SPN_NO_INHERIT)
	{
		/*
		**	Do not inherit handles:  
		**	Give the new process invalid handles to use.
		**	This was added so with costar the spawn of vutil doesn't overwrite the screen.
		*/
		bInhertHandles = TRUE;
		sinfo.dwFlags |= STARTF_USESTDHANDLES;
		sinfo.hStdInput  = INVALID_HANDLE_VALUE;
		sinfo.hStdOutput = INVALID_HANDLE_VALUE;
		sinfo.hStdError  = INVALID_HANDLE_VALUE;
	}


	lpEnv = win32BuildNewEnv();

	bSuccess=CreateProcess(cmd,args,NULL,NULL,bInhertHandles,dwCreationFlags,
			       lpEnv,NULL,&sinfo,&pinfo);

	if (lpEnv)
	{
		free(lpEnv);
	}
	win32DeleteEnv();
	
	if (0 == bSuccess)
	{
		char err1[1024];
		char *err2 = "CreateProcess() failed";
		
		sprintf(err1,"%%WIN32SPAWN-E-CREATEP %s Cmd=[%s] Args=[%s]",
			GetWin32Error(err2), cmd ? cmd:"(nil)", args ? args:"(nil)");
		werrlog(104,err1,0,0,0,0,0,0,0);

		if (sinfo.hStdOutput) 
			CloseHandle(sinfo.hStdOutput);
		if (sinfo.hStdError) 
			CloseHandle(sinfo.hStdError);
	        return 1;
	}
	else
	{
		if ((Mode & SPN_HIDE_PARENT) && vraw_get_console_hWnd())
		{
			Sleep(1500);
			ShowWindow(vraw_get_console_hWnd(), SW_HIDE);
		}
	}

	dwRetcode = 0;

	/*
	** wait for child to finish
	*/
	if (Mode & SPN_WAIT_FOR_CHILD)
	{
		DWORD	dwRc;
		BOOL	bSuccess;

		dwRc = WaitForSingleObject( pinfo.hProcess, INFINITE );
		bSuccess = GetExitCodeProcess( pinfo.hProcess, &dwRetcode );

		if (Mode & SPN_HIDE_CHILD)
		{
			/*
			**	Process the temp stdout and stderr files:
			**	Close them, copy them to the errlog and then delete them.
			*/

			CloseHandle(sinfo.hStdOutput);
			CloseHandle(sinfo.hStdError);

			if (1) /* (dwRetcode) Some processes (line QSUBMIT) fail without an exit code */
			{
				FILE 	*fh;
				char	the_line[256];

				sprintf(the_line,"%s: Spawned process finished with exit code [%d]\n", 
					"%WIN32SPAWNLP-T-FINISHED",dwRetcode);
				werr_write(the_line);

				if (fh = fopen(tempout,"r"))
				{
					while(fgets(the_line,sizeof(the_line)-1,fh))
					{
						werr_write("STDOUT:");
						werr_write(the_line);
					}
					
					fclose(fh);
				}

				if (fh = fopen(temperr,"r"))
				{
					while(fgets(the_line,sizeof(the_line)-1,fh))
					{
						werr_write("STDERR:");
						werr_write(the_line);
					}
					
					fclose(fh);
				}
			}
			unlink(temperr);
			unlink(tempout);
		}
	}
	
	/*
	** restore the parent window (that was hidden)
	*/
	if ((Mode & SPN_HIDE_PARENT) && vraw_get_console_hWnd())
	{
		ShowWindow(vraw_get_console_hWnd(),SW_SHOW);
	}

	CloseHandle(pinfo.hProcess);
	CloseHandle(pinfo.hThread);
	
	return dwRetcode;
}
/*
**	ROUTINE:	win32SetNewEnv()
**
**	FUNCTION:	add an environment var for the process about to be created
**
**	DESCRIPTION:	used instead of normal putenv because we aren't using fork().
**                      In Unix code the putenvs come after the fork, so they don't
**                      affect the existing environment.  Here we need to build a
**                      list of the new env variables to hand it to createprocess later
**
**	ARGUMENTS:	char *    envstr
**
**	GLOBALS:	struct newenvstr *    lpNewEnv  - single linked list of new
**                                                        env strings
**                      int                   nNewEnvSize - running size total
**
**	RETURN:		void
**
**	WARNINGS:	?
**
*/
void win32SetNewEnv(char *envstr)
{
	char *lpEnvCopy;
	struct newenvstr *strptr;
	int nCurSize;

	nCurSize = strlen(envstr);
	nNewEnvSize += nCurSize + 1;
	lpEnvCopy = wstrdup(envstr);

	if (lpNewEnv == NULL)
	{
		strptr = lpNewEnv = wmalloc(sizeof(struct newenvstr));
	}
	else
	{
		strptr = lpNewEnv;
		while (strptr->next != NULL)
		{
			strptr = strptr->next;
		}
		strptr->next = wmalloc(sizeof(struct newenvstr));
		strptr = strptr->next;
	}
	
	strptr->value = lpEnvCopy;
	strptr->len = nCurSize;
	strptr->next = NULL;
	return;
}
/*
**	ROUTINE:	win32BuildNewEnv()
**
**	FUNCTION:	take newenvstr list, and build an env block suitable
**                      for createprocess (format follows):
**                        <var=value>\0<var=value>\0<var=value>\0\0
**
**	ARGUMENTS:	void
**
**	GLOBALS:	lpNewEnv      structures freed; head pointer set to NULL
**                      nNewEnvSize   count reset to zero
**
**	RETURN:		pointer to malloced mem block
**
**	WARNINGS:	
**
*/
static char *win32BuildNewEnv(void)
{
	struct newenvstr *lpStrPtr;
	char *lpNewBlock, *lpNewPtr;
	int nOldSize;
	
	extern char **_environ;
	char **ep;

	/*
	**	Calculate the memory needed to hold the old environment
	*/
	for (nOldSize = 0, ep = _environ; *ep; ++ep)
	{
		nOldSize += strlen(*ep) + 1;
	}
	
	/*
	** extra byte for trailing null
	*/
	lpNewBlock = wcalloc(nOldSize + nNewEnvSize + 1, 1);
	
	lpNewPtr = lpNewBlock;

	/*
	**	Load the new environment vars first because there can be duplicates
	**	entries from the old environment.
	*/
	for (lpStrPtr=lpNewEnv; lpStrPtr != NULL; lpStrPtr = lpStrPtr->next)
	{
		memcpy(lpNewPtr,lpStrPtr->value,lpStrPtr->len);
		lpNewPtr += lpStrPtr->len + 1;

		wtrace("WIN32BUILDNEWENV","NEW","%s",lpStrPtr->value);
	}
	
	/*
	**	Add the old environment vars.
	*/
	for (ep = _environ; *ep; ++ep)
	{
		int dup = 0;

		/*
		**	Check for duplicate environment variables in lpNewEnv
		*/
		for (lpStrPtr=lpNewEnv; lpStrPtr != NULL; lpStrPtr = lpStrPtr->next)
		{
			char *pNew, *pOld;
			
			for (pNew = lpStrPtr->value, pOld = *ep; *pNew == *pOld; pNew++, pOld++)
			{
				if ('=' == *pNew)
				{
					/*
					**	Duplicate environment variable
					*/
					dup = 1;
					break;
				}
			}
			if (dup)
			{
				wtrace("WIN32BUILDNEWENV","DUP","%s duplicate removed",*ep);
				break;
			}
		}

		if (!dup)
		{
			int len;

			len = strlen(*ep);
			memcpy(lpNewPtr,*ep,len);
			lpNewPtr += len + 1;

			wtrace("WIN32BUILDNEWENV","OLD","%s",*ep);
		}
		
	}
	
	return lpNewBlock;
}
/*
**	ROUTINE:	win32DeleteEnv()
**
**	FUNCTION:	delete the global newenvstr list
**
**	ARGUMENTS:	void
**
**	GLOBALS:	lpNewEnv      structures freed; head pointer set to NULL
**                      nNewEnvSize   count reset to zero
**
**	RETURN:		
**
**	WARNINGS:	
**
*/
static void win32DeleteEnv(void)
{
	struct newenvstr *strptr, *strlast;

	/*
	** point to head
	*/
	if ((strptr = lpNewEnv) == NULL)
	{
		return;
	}
	
	do
	{
		/*
		** if has next node
		*/
		if (strptr->next)
		{
			/*
			** record current node in strlast
			*/
			strlast = strptr;

			/*
			** move pointer to next
			*/
			strptr = strptr->next;

			/*
			** free the node
			*/
			free(strlast->value);
			free(strlast);
		}
		else
		{
			/*
			** this is last node in list. just free it
			*/
			free(strptr->value);
			free(strptr);

			/*
			** reinit the head pointer
			*/
			lpNewEnv = NULL;
			nNewEnvSize = 0;
		}
		
		
	} while (lpNewEnv != NULL);

}

/*
**	ROUTINE:	opentempfile()
**
**	FUNCTION:	Create and open a temporary file.
**
**	DESCRIPTION:	Create a temp file and return an open handle to it
**
**	ARGUMENTS:	
**	path		The file path to the temp file.
**
**	GLOBALS:	None
**
**	RETURN:		The handle to the open temp file.
**
**	WARNINGS:	None
**
*/
HANDLE opentempfile(char *path)
{
	char	tempdir[MAX_PATH];
	SECURITY_ATTRIBUTES sa;

	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;

	GetTempPath(sizeof(tempdir)-1, tempdir);
	
	GetTempFileName(tempdir,"wis",0,path);
	
	return CreateFile(path, GENERIC_WRITE|GENERIC_READ, 0, &sa,
			  CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY, NULL);
}

#endif /* WIN32 */
/*
**	History:
**	$Log: win32spn.c,v $
**	Revision 1.16  1998-01-16 13:38:32-05  gsl
**	Fix SUBMIT of wproc getting an access violation.
**
**	Revision 1.15  1997-08-25 09:01:13-04  gsl
**	CLose open handles when an error occurs
**
**	Revision 1.14  1997-08-23 13:53:41-04  gsl
**	Add Security Attribute info to CreateFile() in opentempfile()
**
**	Revision 1.13  1997-08-22 16:28:57-04  gsl
**	Correct startup info struct so it is properly initialized.
**	After the CreateProcess() now close the pinfo process and thread handles.
**	This may have been causing problems ????
**	Add logic to capture stderr and stdout output from a hidden child\
**	process and log it to the error log.
**	Added opentempfile() routine
**
**	Revision 1.12  1997-07-16 21:57:10-04  gsl
**	Correct prototype for vrawgetattribute()
**
**	Revision 1.11  1997-07-16 21:24:14-04  gsl
**	Add SPN_NO_INHERIT which stops the child from inheriting the parents
**	standard handles.
**	Add FillAttribute for console window.
**
**	Revision 1.10  1997-07-16 15:11:26-04  gsl
**	Change to always build a new environment block.
**	Also fixed so that duplicates are not put into the environment block.
**
**	Revision 1.9  1997-07-14 08:34:01-04  gsl
**	Change to handle COSTAR on WIN32.
**	When COSTAR is present then vraw_get_console_hWnd() returns NULL
**	so modify to handle this condition.
**	Also, when COSTAR used then hide the child window.
**
**	Revision 1.8  1997-05-12 17:23:37-04  gsl
**	Improved the error message when CreateProcess() fails.
**	In win32BuildNewEnv() rearranged so new env vars get built into the
**	new environment first because there may be duplicates with the old
**	environment.
**
**	Revision 1.7  1997-05-09 15:13:57-04  gsl
**	If the Child process is hidden but not detached then added the CREATE_NEW_CONSOLE
**	flag to the creation flag.  This corrects a problem where a hidden child
**	process was causing the visible window/console to flicker and the title
**	to change.
**	Also added wtrace()
**
**	Revision 1.6  1997-02-24 21:52:35-05  gsl
**	Change win32spawnvp() to assemble a complete command line and
**	pass it to win32spawnlp() instead of splitting the program and the args
**	into two variables
**
**	Revision 1.5  1997-01-22 14:27:37-08  gsl
**	Add logic to set window location in environment.
**	If hide parent then also hide child so window can be resized and move
**	without screen flicker.
**
**	Revision 1.4  1997-01-14 17:09:09-08  gsl
**	Following the CreateProcess() is successful then sleep for 1.5 secs and
**	then hide the parent windows.
**	Change the SW_RESTORE into a SW_SHOW because we're hiding not minimizing.
**
**	Revision 1.3  1996-12-06 15:38:08-08  jockc
**	win32spawn broken up into win32spawnvp and
**	win32spawnlp  (like execvp and execlp).. code added to pass
**	environment to child process (for WBCKGRD flag)
**
**	Revision 1.2  1996-10-09 14:52:51-07  gsl
**	remove unused variables
**
**	Revision 1.1  1996-09-16 16:01:29-07  jockc
**	Initial revision
**
**
**
*/
