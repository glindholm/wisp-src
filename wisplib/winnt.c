/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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

/*
**	File:		winnt.c
**
**	Project:	WISP/LIB
**
**	Purpose:	Hold Windows NT only files
**
**	Routines:	
**	WL_hsleep()
*/
#ifdef WIN32

/*
**	Includes
*/
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "wispnt.h"

/*
**	Structures and Defines
*/
#ifndef MAX_COMPUTERNAME_LENGTH
#define MAX_COMPUTERNAME_LENGTH         32
#endif
#ifndef MAX_USERNAME_LENGTH
#define MAX_USERNAME_LENGTH             32
#endif

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/


/*
**	ROUTINE:	WL_hsleep()
**
**	FUNCTION:	Sleep for hundreths of seconds
**
**	DESCRIPTION:	Use Sleep() to sleep for hundreths of seconds
**
**	ARGUMENTS:	
**	hundredths	Number of 1/100ths of seconds to sleep for.
**
**	GLOBALS:	none
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
void WL_hsleep(long hundredths)
{
	Sleep(hundredths * 10);
}

/*
**	ROUTINE:	WL_computername()
**
**	FUNCTION:	Return the computer name (from the registry)
**
**	DESCRIPTION:	Get the computer name, first try "ActiveComputerName" if not found
**			then use just "ComputerName". If can't get name return "Windows".
**
**	ARGUMENTS:	
**	cname		Location to store the name or NULL 
**
**	GLOBALS:	none
**
**	RETURN:		cname or if NULL a pointer to an internal static area which contains the name.
**
**	WARNINGS:	None
**
*/
char *WL_computername(char *cname)
{
	static char* the_computername = NULL;

	if (!the_computername)
	{
		char	tmp_name[MAX_COMPUTERNAME_LENGTH+2];
		DWORD	size = sizeof(tmp_name);

		if (TRUE == GetComputerName(tmp_name, &size))
		{
			tmp_name[size] = '\0';
		}
		else
		{
			strcpy(tmp_name,DEF_COMPUTERNAME);
		}

		the_computername = _strdup(tmp_name);
	}

	if (NULL == cname)
	{
		return the_computername;
	}
	else
	{
		strcpy(cname, the_computername);
		return cname;
	}

}

/*
**	ROUTINE:	WL_longuid()
**
**	FUNCTION:	Get the current user id (using GetuserName)
**
**	DESCRIPTION:	(The unix version is in wanguid.c)
**			If GetUsername fails the "USER" is returned.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	none
**
**	RETURN:		a pointer to an internal static area which contains the name.
**
**	WARNINGS:	None
**
*/
const char *WL_longuid(void)
{
	static char *the_userid = NULL;

	if (!the_userid)
	{
		DWORD dwScratch;
		BOOL bRet;
		char	tmp_name[MAX_USERNAME_LENGTH];

		dwScratch = MAX_USERNAME_LENGTH;

		if (!(bRet = GetUserName(tmp_name,&dwScratch))) 
		{
			strcpy(tmp_name,"USER");
		}
		the_userid = _strdup(tmp_name);
	}

	return the_userid;
}

/*
**	ROUTINE:	WL_handle_stdout()
**
**	FUNCTION:	Return the handle for stdout
**
**	DESCRIPTION:	On unix stdout is always handle #1, with WIN32 console I/O this is not true.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The file handle
**
**	WARNINGS:	none
**
*/
int WL_handle_stdout(void)
{
	return (int)GetStdHandle(STD_OUTPUT_HANDLE);
}

/*
**	ROUTINE:	WL_getuid()
**
**	FUNCTION:	Get the User Id number.
**
**	DESCRIPTION:	Emulate unix function.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The dummy uid
**
**	WARNINGS:	none
**
*/
int WL_getuid(void)
{
	return(1);
}

/*
**	ROUTINE:	WL_ttyname()
**
**	FUNCTION:	Find name of a terminal
**
**	DESCRIPTION:	Emulate unix routine.
**			Returns the machine name.
**
**	ARGUMENTS:	
**	fd		Open file descriptor  (Ignored)
**
**	GLOBALS:	none
**
**	RETURN:		The tty name or NULL in background
**
**	WARNINGS:	none
**
*/
const char *WL_ttyname(int fd)
{
	return WL_computername(NULL);
}

/*
**	ROUTINE:	WL_get_registry_value()
**
**	FUNCTION:	Get a value from the registry
**
**	DESCRIPTION:	Does and Open/Query/Close of the registry to get a value.
**
**	ARGUMENTS:	
**	key		The registry key (in HKEY_LOCAL_MACHINE)
**	value		The value to query
**	maxsize		The maximum size to put into result
**	result		The result
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Success
**	1		Value not set
**	2		Value too big
**	-1		Key not found
**
**	WARNINGS:	None
**
*/
int WL_get_registry_value(const char *key, const char *value, int maxsize, char *result)
{
	LONG lSuccess; 
	HKEY hKey; 		/* handle to registry key */
	DWORD dwType;
	DWORD dwSize = maxsize;

	lSuccess = RegOpenKeyEx(HKEY_LOCAL_MACHINE, key, 0, KEY_READ, &hKey);
	if (ERROR_SUCCESS != lSuccess)
	{
		return -1; /* FAILED - key not found */
	}


	lSuccess = RegQueryValueEx(hKey, value, NULL, &dwType, result, &dwSize);
	RegCloseKey(hKey);

	if (ERROR_MORE_DATA == lSuccess)
	{
		return 2; /* FAILED - value too big */
	}
	
	if (ERROR_SUCCESS != lSuccess)
	{
		return 1; /* FAILED - value not found */
	}

	return 0;
}

#ifdef NOT_YET
/* I was not able to get this to work */
/*
**	ROUTINE:	get_remote_registry_value()
**
**	FUNCTION:	Get a value from the registry on another machine
**
**	DESCRIPTION:	Does and Open/Query/Close of the registry to get a value.
**
**	ARGUMENTS:	
**	machine		The remote machine name.
**	key		The registry key (in HKEY_LOCAL_MACHINE)
**	value		The value to query
**	maxsize		The maximum size to put into result
**	result		The result
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Success
**	1		Value not set
**	2		Value too big
**	-1		Key not found
**	-2		Unable to connect to remote registry
**
**	WARNINGS:	None
**
*/
int get_remote_registry_value(const char* machine, const char *key, const char *value, int maxsize, char *result)
{
	LONG lSuccess; 
	HKEY hKeyRemote;
	HKEY hKey; 		/* handle to registry key */
	DWORD dwType;
	DWORD dwSize = maxsize;
	char	MachineName[256];
	
	strcpy(MachineName, "\\\\");
	strcat(MachineName, machine);
	
	lSuccess = RegConnectRegistry(MachineName, HKEY_LOCAL_MACHINE, &hKeyRemote);
	if (ERROR_SUCCESS != lSuccess)
	{
		return -2; /* FAILED - Unable to connect */
	}

	lSuccess = RegOpenKeyEx(hKeyRemote, key, 0, KEY_READ, &hKey);
	if (ERROR_SUCCESS != lSuccess)
	{
		return -1; /* FAILED - key not found */
	}


	lSuccess = RegQueryValueEx(hKey, value, NULL, &dwType, result, &dwSize);
	RegCloseKey(hKey);
	RegCloseKey(hKeyRemote);

	if (ERROR_MORE_DATA == lSuccess)
	{
		return 2; /* FAILED - value too big */
	}
	
	if (ERROR_SUCCESS != lSuccess)
	{
		return 1; /* FAILED - value not found */
	}

	return 0;
}
#endif /* NOT_YET */

/*
**	ROUTINE:	WL_wgetreg()
**
**	FUNCTION:	Get a registry value
**
**	DESCRIPTION:	Setup a static buff to receive the value then call WL_get_registry_value.
**
**	ARGUMENTS:	
**	key		The registry key (in HKEY_LOCAL_MACHINE)
**	value		The value to query
**
**	GLOBALS:	None
**
**	RETURN:		Pointer to the result or NULL
**
**	WARNINGS:	Works like getenv(), max size is 512.
**
*/
char *WL_wgetreg(const char *key, const char *value)
{
	static char buff[512];
	int	rc;

	rc = WL_get_registry_value(key, value, sizeof(buff), buff);
	if (0 == rc)
	{
		return buff;
	}
	else if ( 2 == rc ) /* TOO BIG */
	{
		sprintf(buff, "REGISTRY KEY=[%s\\%s] DATA IS LONGER THEN BUFFER [%d]",
			key, value, sizeof(buff));
		return buff;
	}
	else
	{
		return NULL;
	}
}

/*
**	ROUTINE:	WL_win32_nt()
**
**	FUNCTION:	Check if OS is NT
**
**	DESCRIPTION:	Get the OS version an test if NT
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		This is NT
**	0		Not NT (probably 95)
**
**	WARNINGS:	None
**
*/
int WL_win32_nt(void)
{
	static int rc = -1;
	
	if (-1 == rc)
	{
		OSVERSIONINFO osVer;
		osVer.dwOSVersionInfoSize = sizeof(osVer);
		GetVersionEx(&osVer);
		if (osVer.dwPlatformId == VER_PLATFORM_WIN32_NT)
		{
			rc = 1;
		}
		else
		{
			rc = 0;
		}
	}

	return rc;
}

/*
**	ROUTINE:	WL_win32_95()
**
**	FUNCTION:	Check if OS is Windows 95 (or 98)
**
**	DESCRIPTION:	Get the OS version an test if 95 (or 98)
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		This is Windows 95 (or 98)
**	0		Not 95 (probably NT)
**
**	WARNINGS:	This should be used to test if WIN32 and not NT.
**
*/
int WL_win32_95(void)
{
	static int rc = -1;
	
	if (-1 == rc)
	{
		OSVERSIONINFO osVer;
		osVer.dwOSVersionInfoSize = sizeof(osVer);
		GetVersionEx(&osVer);
		if (osVer.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
		{
			rc = 1;
		}
		else
		{
			rc = 0;
		}
	}

	return rc;
}

/*
**	ROUTINE:	WL_win32_98()
**
**	FUNCTION:	Check if OS is Windows 98 (only)
**
**	DESCRIPTION:	Get the OS version an test if 98.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		This is Windows 98
**	0		Not 98
**
**	WARNINGS:	none
**
*/
int WL_win32_98(void)
{
	static int rc = -1;
	
	if (-1 == rc)
	{
		OSVERSIONINFO osVer;
		osVer.dwOSVersionInfoSize = sizeof(osVer);
		GetVersionEx(&osVer);
		if (osVer.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS &&
		    4 == osVer.dwMajorVersion && 10 ==  osVer.dwMinorVersion)
		{
			rc = 1;
		}
		else
		{
			rc = 0;
		}
	}

	return rc;
}

/*
**	ROUTINE:	WL_win32_version()
**
**	FUNCTION:	Get the WIN32 version formatted as a string.
**
**	DESCRIPTION:	
**			Vista	"Vista 6.0.xxxx xx"
**			XP	"XP 5.1.xxxx xxx"
**			NT	"NT 4.0.xxxx xxx"
**			95	"(Not NT) 4.0.xxxx xxx"
**			98	"(Not NT) 4.10.xxxx xxx"
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The formatted string.
**
**	WARNINGS:	None
**
*/
const char* WL_win32_version(void)
{
	static char *the_version = NULL;
	
	
	if (NULL == the_version)
	{
		OSVERSIONINFO osVer;
		char	buff[256];
		char	*is_nt = "";
		char	*name = "";
		
		osVer.dwOSVersionInfoSize = sizeof(osVer);
		GetVersionEx(&osVer);

		if (osVer.dwPlatformId == VER_PLATFORM_WIN32_NT)
		{
			is_nt = "NT ";
		}
		else
		{
			is_nt = "(Not NT) ";
		}

		if (5 == osVer.dwMajorVersion)
		{
			name = "XP/2003 "; 
		}
		else if (6 == osVer.dwMajorVersion && 0 ==  osVer.dwMinorVersion)
		{
			name = "Vista/2008 "; 
		}
		else if (6 == osVer.dwMajorVersion && 1 ==  osVer.dwMinorVersion)
		{
			name = "Windows 7 "; 
		}

		sprintf(buff,"%s%s%d.%d %d %s", 
			name,
			is_nt,
			osVer.dwMajorVersion, 
			osVer.dwMinorVersion,
			osVer.dwBuildNumber, 
			osVer.szCSDVersion);

		the_version = _strdup(buff);
	}

	return the_version;
}

#endif /* WIN32 */

/*
**	History:
**	$Log: winnt.c,v $
**	Revision 1.23  2009/10/18 20:47:20  gsl
**	Fix how windows full version is displayed (wdiag)
**	
**	Revision 1.22  2007/08/02 20:09:21  gsl
**	Add XP and Vista based on major release
**	
**	Revision 1.21  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.20  2002/07/10 21:05:34  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.19  2002/07/10 04:27:34  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.18  2002/07/09 04:13:53  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.17  2001/11/27 21:46:07  gsl
**	Replace cuserid() with longuid()
**	
**	Revision 1.16  2001-11-27 16:29:16-05  gsl
**	Add wispusername()
**
**	Revision 1.15  2001-11-20 15:40:05-05  gsl
**	Increase buff size for registry keys
**
**	Revision 1.14  1998-12-04 13:08:31-05  gsl
**	Add win32_98() and win32_version()
**
**	Revision 1.13  1997-12-04 18:12:25-05  gsl
**	change to wispnt.h
**
**	Revision 1.12  1997-08-23 10:49:50-04  gsl
**	Add win32_nt() and win32_95() to test if on NT or 95
**
**	Revision 1.11  1997-03-20 16:07:33-05  gsl
**	Add get_remote_registry_value() in ifdefs as I was not able to get it working
**
**	Revision 1.10  1997-02-28 16:09:09-05  gsl
**	Use define for default computer name
**
**	Revision 1.9  1996-12-18 13:30:40-05  gsl
**	Change to use function GetComputerName()
**
**	Revision 1.8  1996-11-18 12:52:03-08  gsl
**	Add registry routines for use in wispcfg.c
**
**	Revision 1.7  1996-09-10 08:51:01-07  gsl
**	Fix _strdup()
**
**	Revision 1.6  1996-08-26 17:06:24-07  gsl
**	Add ttyname() for NT which returns the computername
**
**	Revision 1.5  1996-08-21 16:35:49-07  gsl
**	Fix compiler warning
**
**	Revision 1.4  1996-08-21 16:22:10-07  gsl
**	add routine handle_stdout() to get the handle for stdout
**
**	Revision 1.3  1996-08-19 14:51:31-07  gsl
**	Add a functioning cuserid() emulation and computername()
**
**	Revision 1.2  1996-07-24 13:27:23-07  gsl
**	Enclose in ifdef WIN32
**
**	Revision 1.1  1996-07-18 15:35:41-07  gsl
**	Initial revision
**
**
**
**
*/
