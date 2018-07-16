static char copyright[]="Copyright (c) 1996-1998 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		win32spn.c
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Spawn a new process...
**
**			LINK sytle	- parent waits hidden until child finishes
**			SUBMIT sytle	- parent continues, child runs hidden.
**			Hidden sytle	- parent waits, child runs hidden until done (e.g. VUTIL for SORT)
**			Standalone	- parent continues, child runs in separate window
**
**	Routines:	
**	win32spawnvp()
**	win32spawnlp()
**
**	TEST CASES:	This is a very difficult routine to test because of the large number
**			of combinations of cases.
**
**			OS:		Windows 95, 98
**					Windows NT
**
**			Screen IO:	WISP Screens
**					WISP Screens with COSTAR
**					NATIVE Screens
**					Background (No screen I/O)
**
**			Parent/Child:	Console  ==>  Console
**					Console  ==>  Window with a console (COBOL)
**					Console  ==>  Window with a GUI     (Notepad)
**					Window   ==>  Console
**					Window   ==>  Window with a console (COBOL)
**					Window   ==>  Window with a GUI     (Notepad)
**
**			Types:		LINK
**					SUBMIT
**					Hidden
**					Standalone
**
**			Options:	UTILSWINDOWS
**					DISPLAYUTIL
**
**			Run each of these cases on both NT and 95.
**
**			A) WISP screens
**			   	1) LINK wshell ==> wproc QASTART
**			   	2) LINK wproc  ==> SAMPLE
**				3) LINK SAMPLE ==> SAMPLE
**				4) LINK SAMPLE ==> wproc QASTART
**			   	5) SUBMIT wshell ==> QABCKGRD
**			   	6) SUBMIT wshell ==> wproc QABCKPRC
**				7) SUBMIT SAMPLE ==> QABCKGRD
**				8) SUBMIT SAMPLE ==> wproc QABCKPRC
**				9) HIDDEN WISPSORT
**				10) STANDALONE UTILSWINDOWS with VSEDIT 
**				11) STANDALONE UTILSWINDOWS with NOTEPAD
**				12) SUBMIT with AQM *** (HIDDEN with capture output)
**			B) WISP screens with COSTAR
**			   	1) LINK wshell ==> wproc QASTART
**			   	2) LINK wproc  ==> SAMPLE
**				3) LINK SAMPLE ==> SAMPLE
**				4) LINK SAMPLE ==> wproc QASTART
**			   	5) SUBMIT wshell ==> QABCKGRD
**			   	6) SUBMIT wshell ==> wproc QABCKPRC
**				7) SUBMIT SAMPLE ==> QABCKGRD
**				8) SUBMIT SAMPLE ==> wproc QABCKPRC
**				9) HIDDEN WISPSORT
**				10) STANDALONE UTILSWINDOWS with VSEDIT 
**				11) STANDALONE UTILSWINDOWS with NOTEPAD
**			C) NATIVE screens
**			   	2) LINK wshell ==> SAMPLE
**				3) LINK SAMPLE ==> wproc QASTART   (** dead window **)
**				4) LINK SAMPLE ==> SAMPLE          (USESOFTLINK)
**			   	6) SUBMIT wshell ==> QABCKGRD
**				7) SUBMIT SAMPLE ==> wproc QABCKPRC
**				8) SUBMIT SAMPLE ==> QABCKGRD
**				9) HIDDEN WISPSORT
**				10) STANDALONE UTILSWINDOWS with VSEDIT 
**				11) STANDALONE UTILSWINDOWS with NOTEPAD
**					
*/
#ifdef WIN32

/*
**	Includes
*/
#include <windows.h>
#include <stdio.h>
#include <process.h>
#include <crtdbg.h>
#include "werrlog.h"
#include "win32spn.h"
#include "win32err.h"
#include "wmalloc.h"
#include "wispnt.h"
#include "wperson.h"
#include "idsisubs.h"


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
extern int wbackground(void);
extern int use_costar(void);
extern long WL_filesize(const char* path);
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

		if ( strchr(sh_parm[idx], ' ') )					/* Any embedded spaces?			*/
			cmd_len += 2;							/* Yes, account for a double-quote wrap	*/
	}
	cmdline = malloc(cmd_len);
	/*
	**	Assemble the shell command
	*/
	dqw_strcpy(cmdline, sh_parm[0]);						/* Double Quote wrap if embedded spaces	*/

	for (idx=1; sh_parm[idx]; ++idx)
	{
		strcat(cmdline," ");
		dqw_strcat(cmdline, sh_parm[idx]);					/* Double Quote wrap if embedded spaces	*/
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
**				SPN_HIDE_CHILD     Child starts hidden.
**				SPN_WAIT_FOR_CHILD wait on child process to finish
**				SPN_SUBMIT_CHILD   detach new process from our console
**				SPN_NO_INHERIT	   child does not inherit std handles
**				SPN_CAPTURE_OUTPUT capture the stdout and stderr output 
**						   for a hidden child process.
**				SPN_STANDALONE_CHILD Child runs as a separate process.
**				SPN_HIDDEN_CMD     Hidden command with separate console
**
**	GLOBALS:	
**
**	RETURN:		return code from spawned process
**
**	WARNINGS:	?
**
*/
int win32spawnlp(const char *cmd, const char *args, int Mode)
{
	PROCESS_INFORMATION pinfo;
	STARTUPINFO sinfo;
	BOOL bSuccess;
	DWORD dwRetcode, dwCreationFlags;
	char *lpEnv;
	BOOL bInheritHandles = TRUE;
	char tempout[MAX_PATH], temperr[MAX_PATH];
	HWND hConsole;
	BOOL bForeground = TRUE;  /* Assume we have the foreground window */
	SECURITY_ATTRIBUTES processAttributes, threadAttributes;
	SECURITY_DESCRIPTOR  sd;
	HANDLE hStdin, hStdout, hDupStdin, hDupStdout;
	char err1[1024];
	BOOL bDebugNoHide = FALSE;

	wtrace("WIN32SPAWNLP","ENTRY","Cmd=[%s] Args=[%s] Mode=[%d]", cmd ? cmd:"(nil)", args ? args:"(nil)",Mode);
	wtrace("WIN32SPAWNLP","MODE","Mode=[ %s%s%s%s%s%s%s%s]",
	       ((Mode & SPN_HIDE_PARENT)      ? "HIDE_PARENT "      : ""),
	       ((Mode & SPN_HIDE_CHILD)       ? "HIDE_CHILD "       : ""),
	       ((Mode & SPN_WAIT_FOR_CHILD)   ? "WAIT_FOR_CHILD "   : ""),
	       ((Mode & SPN_SUBMIT_CHILD)     ? "SUBMIT_CHILD "     : ""),
	       ((Mode & SPN_NO_INHERIT)       ? "NO_INHERIT "       : ""),
	       ((Mode & SPN_CAPTURE_OUTPUT)   ? "CAPTURE_OUTPUT "   : ""),
	       ((Mode & SPN_STANDALONE_CHILD) ? "STANDALONE_CHILD " : ""),
	       ((Mode & SPN_HIDDEN_CMD)       ? "HIDDEN_CMD "       : ""));
	wtrace_timestamp("WIN32SPAWNLP");

	hStdin = INVALID_HANDLE_VALUE;
	hStdout = INVALID_HANDLE_VALUE;
	hDupStdin = INVALID_HANDLE_VALUE;
	hDupStdout = INVALID_HANDLE_VALUE;

	if (get_wisp_option("DEBUGNOHIDE"))
	{
		bDebugNoHide = TRUE;
	}

	/*
	**	Setup Startup info flags
	*/
	memset(&sinfo,'\0',sizeof(sinfo));
	sinfo.cb = sizeof(sinfo);

	sinfo.lpReserved=NULL;
	sinfo.lpDesktop=NULL;
	sinfo.lpTitle=NULL;
	sinfo.dwFlags = 0;


	sinfo.dwFlags |= STARTF_USECOUNTCHARS;
	sinfo.dwXCountChars = 80;
	sinfo.dwYCountChars = 24;

	/*
	**	Set up fill color for white background with black text
	*/
	sinfo.dwFlags |= STARTF_USEFILLATTRIBUTE;
	sinfo.dwFillAttribute = vrawgetattribute(0);

	/*
	**	If in background then always HIDE the window.
	**
	**	NOTE: Do not call vraw_get_console_hWnd() when in background as it displays a console.
	*/
	if (wbackground())
	{
		/*
		**	If not console handle them assume COSTAR and hide the child window.
		*/
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;
		hConsole = NULL;
	}
	else
	{

		hConsole = vraw_get_console_hWnd();

		if (hConsole)
		{
			/*
			**	We assume we have the forground window but if not set the flag to false.
			*/
			HWND hFore;

			hFore = GetForegroundWindow();
			
			if (hFore != hConsole)
			{
				bForeground = FALSE;
			}
		}
	}
	

	/*
	**	Setup CreationFlags
	*/
	dwCreationFlags = NORMAL_PRIORITY_CLASS;
	if (Mode & SPN_SUBMIT_CHILD)
	{
		/* No other options are allowed with SPN_SUBMIT_CHILD */
		_ASSERT(SPN_SUBMIT_CHILD == Mode);

//		Spawning a CONSOLE process with DETACHED_PROCESS
//		gives an access violation.
//		dwCreationFlags |= DETACHED_PROCESS;

		bInheritHandles = FALSE;

		dwCreationFlags |= CREATE_NEW_CONSOLE;
		dwCreationFlags |= CREATE_NEW_PROCESS_GROUP;
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;
	}
	else if (Mode & SPN_STANDALONE_CHILD)
	{
		/* No other options are allowed with SPN_STANDALONE_CHILD */
		_ASSERT(SPN_STANDALONE_CHILD == Mode);

		dwCreationFlags |= CREATE_NEW_CONSOLE;
		dwCreationFlags |= CREATE_NEW_PROCESS_GROUP;

		bInheritHandles = FALSE;

		/*
		**	Un-do the console position env var so earlier settings will not be acted on.
		*/
		win32SetNewEnv("CONWINPOS=");
		win32SetNewEnv("WISPHCONSOLE=");

	}
	else if (Mode & SPN_HIDDEN_CMD)
	{
		dwCreationFlags |= CREATE_NEW_CONSOLE;
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;
	}
	else if (use_costar() && (Mode & SPN_WAIT_FOR_CHILD))
	{
		HANDLE hProcess;
		
		/*
		**	If COSTAR 
		**	- hide the child window
		**	- duplicate the handles and setup sinfo
		**
		**	This was done through trial and error until we found a combination
		**	of flags and parameters which works correctly on both 95 and NT.
		*/
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;

		/*
		**	With COSTAR we use CREATE_SUSPENDED so we can close the 
		**	duplicate handles before the child runs.
		*/
		dwCreationFlags |= CREATE_SUSPENDED;

		/*
		**	Need to duplicate the stdin and stdout handles so they
		**	can be passed in the sinfo structure.
		**	This is required on NT (but not 95) when going from
		**	a windows process to a console process because the
		**	handles don't get inherited on NT.
		*/

		hStdout  = GetStdHandle(STD_OUTPUT_HANDLE);
		hStdin   = GetStdHandle(STD_INPUT_HANDLE);

		hProcess = GetCurrentProcess();

		if (INVALID_HANDLE_VALUE != hStdout)
		{
			bSuccess = DuplicateHandle(hProcess,
						   hStdout,
						   hProcess,
						   &hDupStdout,
						   0,
						   TRUE,
						   DUPLICATE_SAME_ACCESS);
			if (!bSuccess)
			{
				sprintf(err1,"%%WIN32SPAWN-E-DUPHANDLE %s hStdout=%ld",
					GetWin32Error("DuplicateHandle(hStdout) failed"), (long)hStdout);
				werrlog(104,err1,0,0,0,0,0,0,0);
			}
		}
		
		if (INVALID_HANDLE_VALUE != hStdin)
		{
			bSuccess = DuplicateHandle(hProcess,
						   hStdin,
						   hProcess,
						   &hDupStdin,
						   0,
						   TRUE,
						   DUPLICATE_SAME_ACCESS);
			if (!bSuccess)
			{
				sprintf(err1,"%%WIN32SPAWN-E-DUPHANDLE %s hStdin=%ld",
					GetWin32Error("DuplicateHandle(hStdin) failed"), (long)hStdin);
				werrlog(104,err1,0,0,0,0,0,0,0);
			}
		}
		
		sinfo.dwFlags |= STARTF_USESTDHANDLES;
		sinfo.hStdInput  = hDupStdin;
		sinfo.hStdOutput = hDupStdout;
		sinfo.hStdError  = hDupStdout;

	}

	if (Mode & SPN_HIDE_CHILD)
	{
		sinfo.dwFlags |= STARTF_USESHOWWINDOW;
		sinfo.wShowWindow = SW_HIDE;
	}
	

	/*
	** 	Prepare the child's console window to cover our window
	**
	**	NOTE: HIDE_PARENT does not get specified if linking to a
	**	non-cobol exe file because it may share the console.
	*/
	if (hConsole && !(Mode & SPN_STANDALONE_CHILD))
	{
		char	envstring[80];
		WINDOWPLACEMENT wndpl;

		wndpl.length = sizeof(wndpl);
		bSuccess = GetWindowPlacement(hConsole, &wndpl);
		if (bSuccess)
		{
			sinfo.dwFlags |= STARTF_USEPOSITION;
			sinfo.dwX= wndpl.rcNormalPosition.left;
			sinfo.dwY= wndpl.rcNormalPosition.top;

			/*
			 **	Set the Window position in the environment
			 **	so the new process get position its window correctly.
			 */
			sprintf(envstring,"CONWINPOS=%d:%d", sinfo.dwX, sinfo.dwY);
			win32SetNewEnv(envstring);
		}

		/*
		**	Pass the console handle to the child so that it can 
		**	move the window if needed. 
		*/
		sprintf(envstring,"WISPHCONSOLE=%d",(int)hConsole);
		win32SetNewEnv(envstring);
	}

	if ((Mode & SPN_HIDDEN_CMD) && (Mode & SPN_WAIT_FOR_CHILD))
	{
		/*
		**	We are running a hidden child process and waiting for it to complete.
		**	Decide what to do with the stdio handles?
		*/
		if (Mode & SPN_CAPTURE_OUTPUT)
		{
			/*
			**	This is used by SUBMIT to issues the QSUBMIT or REXEC command.
			**	We want to capture the stdout and stderr output to temp files
			**	which we will log after it is finished.
			*/
			bInheritHandles = TRUE;
			sinfo.dwFlags |= STARTF_USESTDHANDLES;
			sinfo.hStdInput  = INVALID_HANDLE_VALUE;
			sinfo.hStdOutput = opentempfile(tempout);
			sinfo.hStdError  = opentempfile(temperr);
		}
		else
		{
			/*
			**	Do not inherit handles:  
			**	This was added so with costar the spawn of vutil doesn't overwrite the screen.
			*/
			bInheritHandles = FALSE;
		}
	}

	if (Mode & SPN_NO_INHERIT)
	{
		/*
		**	Do not inherit handles:  
		**	This was added so with costar the spawn of vutil doesn't overwrite the screen.
		*/
		bInheritHandles = FALSE;
	}

	if (FALSE == bInheritHandles)
	{
		/*
		**	This is needed to prevent Costar from inheriting the handles.
		**	Give the new process invalid handles to use.
		**	The bInheritHandles=FALSE seems to work on NT but not on 95.
		*/
		if (use_costar() && !win32_nt())
		{
			sinfo.dwFlags |= STARTF_USESTDHANDLES;
			sinfo.hStdInput  = INVALID_HANDLE_VALUE;
			sinfo.hStdOutput = INVALID_HANDLE_VALUE;
			sinfo.hStdError  = INVALID_HANDLE_VALUE;
		}
	}
	

	lpEnv = win32BuildNewEnv();

	/*
	**	Setup default security attributes for NT, these are ignored on 95.
	*/
	memset(&processAttributes, '\0', sizeof(processAttributes));
	processAttributes.nLength = sizeof(processAttributes);
	processAttributes.lpSecurityDescriptor = NULL;
	processAttributes.bInheritHandle = TRUE;

	if (win32_nt())
	{
		InitializeSecurityDescriptor( &sd, SECURITY_DESCRIPTOR_REVISION );
		SetSecurityDescriptorDacl( &sd, TRUE, NULL, FALSE );
		processAttributes.lpSecurityDescriptor = &sd;
	}

	memset(&threadAttributes, '\0', sizeof(threadAttributes));
	threadAttributes.nLength = sizeof(threadAttributes);
	threadAttributes.lpSecurityDescriptor = NULL;
	threadAttributes.bInheritHandle = TRUE;

	if (bDebugNoHide && SW_HIDE == sinfo.wShowWindow)
	{
		/*
		**	DEBUGNOHIDE: Always show all windows.
		*/
		sinfo.wShowWindow = SW_SHOW;
	}

	wtrace("WIN32SPAWNLP","CreateProcess","bInheritHandles=%s, dwCreationFlags=0x%X, sinfo.dwFlags=0x%X",
	       ((bInheritHandles) ? "TRUE":"FALSE"),
	       dwCreationFlags, sinfo.dwFlags);

	bSuccess=CreateProcess((char*)cmd,
				(char*)args,
				&processAttributes,
				&threadAttributes,
				bInheritHandles,
				dwCreationFlags,
				lpEnv,
				NULL,
				&sinfo,
				&pinfo);

	/*
	**	If duplicate stdin and stdout were created then close them now. (Used with COSTAR)
	*/

	if (hDupStdin != INVALID_HANDLE_VALUE)
	{
		CloseHandle(hDupStdin);
	}
	if (hDupStdout != INVALID_HANDLE_VALUE)
	{
		CloseHandle(hDupStdout);
	}

	/*
	**	If we setup an environment then deleted it now.
	*/
	if (lpEnv)
	{
		free(lpEnv);
		lpEnv = NULL;
	}
	win32DeleteEnv();
	
	/*
	**	Check if the CreateProcess failed.
	*/
	if (!bSuccess)
	{
		sprintf(err1,"%%WIN32SPAWN-E-CREATEP %s Cmd=[%s] Args=[%s]",
			GetWin32Error("CreateProcess() failed"), cmd ? cmd:"(nil)", args ? args:"(nil)");
		werrlog(104,err1,0,0,0,0,0,0,0);

		if ( sinfo.dwFlags & STARTF_USESTDHANDLES )
		{
			if (INVALID_HANDLE_VALUE != sinfo.hStdOutput) 
				CloseHandle(sinfo.hStdOutput);
			if (INVALID_HANDLE_VALUE != sinfo.hStdError) 
				CloseHandle(sinfo.hStdError);
		}
		
	        return 1;
	}

	/*
	**	If the process was created suspended then let it resume now. (Used with COSTAR)
	*/
	if ( dwCreationFlags & CREATE_SUSPENDED)
	{
		ResumeThread(pinfo.hThread);
	}

	/*
	**	Wait a 1.5 seconds then hide the parent console window.
	*/
	if ((Mode & SPN_HIDE_PARENT) && hConsole && !bDebugNoHide)
	{
		Sleep(1500);
		ShowWindow(hConsole, SW_HIDE);
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

		if ((Mode & SPN_HIDDEN_CMD) && (Mode & SPN_CAPTURE_OUTPUT))
		{
			/*
			**	Process the temp stdout and stderr files:
			**	Close them, copy them to the errlog and then delete them.
			*/

			CloseHandle(sinfo.hStdOutput);
			CloseHandle(sinfo.hStdError);
			
			/* 
			**	Some processes (line QSUBMIT) fail without an exit code 
			**	so also check if the captured output files are not empty.
			*/
			if (dwRetcode || WL_filesize(tempout) > 0 || WL_filesize(temperr) > 0) 
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
	** restore the parent window (that may have been hidden)
	*/
	if (hConsole)
	{
		if (bForeground)
		{
			SetForegroundWindow(hConsole);
		}
		
		ShowWindow(hConsole,SW_SHOW);
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
**	Revision 1.29.2.1  2002/10/09 21:17:34  gsl
**	Huge file support
**	
**	Revision 1.29  1998/12/09 20:03:58  gsl
**	Fix for WIN98 to force window to foreground on return.
**	Change test for win32_95() to !win_nt()
**	
**	Revision 1.28  1998-11-02 11:20:21-05  gsl
**	Fixed the CAPTURE OUTPUT logic
**
**	Revision 1.27  1998-10-28 15:28:38-05  gsl
**	DEBUGNOHIDE option
**
**	Revision 1.26  1998-08-27 16:34:00-04  gsl
**	Pass the Console handle to the clild with envvar WISPHCONSOLE
**
**	Revision 1.25  1998-08-03 17:22:02-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.24  1998-06-23 15:46:11-04  gsl
**	Fix the COSTAR handling under NT. Add security stuff, added duplicating
**	stdin and stdout handles.
**
**	Revision 1.23  1998-05-22 10:43:06-04  gsl
**	Fix STANDALONE for working with Co*STAR
**
**	Revision 1.22  1998-05-12 11:24:42-04  gsl
**	Add wtrace for Createprocess flags and timestamp
**
**	Revision 1.21  1998-05-08 15:01:51-04  gsl
**	Fix HIDE_CHILD to HIDDEN_CMD, was messing up COSTAR
**
**	Revision 1.20  1998-05-05 17:36:56-04  gsl
**	Add HIDDEN_CMD flag to replace old HIDE_CHILD flag.
**	HIDE_PARENT now doesn't automatically HIDE_CHILD unless told to.
**	Clear CONWINPOS envvar when STANDALONE is set to prevent passing it along.
**
**	Revision 1.19  1998-05-05 13:54:23-04  gsl
**	Cleanup the mode flags for Native Screen support.
**	Rework handling of console handles
**
**	Revision 1.18  1998-03-16 14:15:14-05  gsl
**	make args const
**
**	Revision 1.17  1998-01-22 10:23:35-05  gsl
**	If in background then never call vraw_get_console_hWnd() also always
**	set the hide-window flag.
**
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
