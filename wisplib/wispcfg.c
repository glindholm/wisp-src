static char copyright[]="Copyright (c) 1996-1997 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wispcfg.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	WISP configuration routines
**
**	Routines:	
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "wispcfg.h"
#include "wmalloc.h"
#include "idsisubs.h"
#include "setenvst.h"
#include "assert.h"
#include "wispnt.h"
#include "wisplib.h"
#include "wperson.h"
#include "werrlog.h"
#include "wanguid.h"

/*
**	Structures and Defines
*/
#define REG_WISP	"SOFTWARE\\NeoMedia\\WISP"
#define REG_VIDEOCAP	"SOFTWARE\\NeoMedia\\WISP\\VIDEOCAP"
#define REG_VSSUBS	"SOFTWARE\\NeoMedia\\WISP\\VSSUBS"
#define REG_VS_EXTRACT	"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\EXTRACT"
#define REG_VS_MESSAGE	"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\MESSAGE"
#define REG_VS_SCRATCH	"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\SCRATCH"
#define REG_LICENSE	"SOFTWARE\\NeoMedia\\WISP\\License"
#define REG_VERSIONS	"SOFTWARE\\NeoMedia\\WISP\\Versions"
#define REG_WISPBIN	"SOFTWARE\\NeoMedia\\WISP\\WISPBin"
#define REG_DISPLAY	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\DISPLAY"
#define REG_WPROC	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WPROC"
#define REG_WISPTRAN	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTran"
#define REG_WT_COBOL	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL"
#define REG_ACUCOBOL	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL\\ACUCOBOL"
#define REG_MFCOBOL	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL\\MFCOBOL"
#define REG_ACP		"SOFTWARE\\NeoMedia\\WISP\\ACP"

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
**	ROUTINE:	wisphomedir()
**
**	FUNCTION:	Get the users "HOME" directory path.
**
**	DESCRIPTION:	
**			On UNIX use envronment variable $HOME
**			On WIN32 <usersdir>\<userid>\WISP
**
**	ARGUMENTS:	
**	dir		The home directory path or NULL
**
**	GLOBALS:	None
**
**	RETURN:		The home directory path
**
**	WARNINGS:	None
**
*/
const char* wisphomedir(char *dir)
{
	static char *the_dir = NULL;
	
	if (!the_dir)
	{
#ifdef unix
		char	*ptr;
		ptr = getenv("HOME");
		if (ptr && *ptr)
		{
			the_dir = wstrdup(ptr);
		}
		else
		{
			/*
			**	HOME was not set so on unix use a dummy value. 
			*/
			the_dir = "$HOME";
		}
#endif /* unix */

#ifdef WIN32
		char 	*ptr;
		char	usersdir[128];
		char	homedir[128];
		
		ptr = wgetreg(REG_WISP, "USERSDIR");
		if (ptr && *ptr)
		{
			strcpy(usersdir,ptr);
		}
		else
		{
			strcpy(usersdir,"C:\\USERS");
		}

		buildfilepath( homedir, usersdir, longuid() );
		buildfilepath( homedir, homedir, "WISP" );
		
		the_dir = wstrdup(homedir);

		/* Add a dummy file to call makepath */
		buildfilepath( homedir, homedir, "dummy" );
		makepath( homedir);
#endif /* WIN32 */
	}
	ASSERT(the_dir);

	if (dir)
	{
		strcpy(dir, the_dir);
		return dir;
	}
	else
	{
		return the_dir;
	}
}

/*
**	ROUTINE:	wispconfigdir()
**
**	FUNCTION:	Get path to WISP configuration directory
**
**	DESCRIPTION:	
**			On others return environment variable $WISPCONFIG
**			If WISPCONFIG is not set it returns the string "$WISPCONFIG"
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		Path to WISP config directory
**
**	WARNINGS:	None
**
*/
const char* wispconfigdir(void)
{
	static char *wispconfig_dir = NULL;

	if (!wispconfig_dir)
	{
		char 	*ptr;

#ifdef unix
		ptr = getenv( "WISPCONFIG" );
#endif /* unix */
#ifdef WIN32
		ptr = wgetreg(REG_WISP, "WISPCONFIG");
#endif /* WIN32 */

		if (ptr && *ptr)
		{
			wispconfig_dir = wstrdup(ptr);
		}
		else
		{
			/*
			**	WISPCONFIG was not set so use a dummy value to generate the names.
			**	This will cause an error when a config file is trying to open, but if not
			**	used then no error message needed.
			*/
			wispconfig_dir = "$WISPCONFIG";
		}
	}
	ASSERT(wispconfig_dir);
	
	return wispconfig_dir;
}

/*
**	ROUTINE:	wispserver()
**
**	FUNCTION:	Get name of WISP server machine. (Only used for NT)
**
**	DESCRIPTION:	
**			On others return environment variable $WISPSERVER
**			If WISPSERVER is not set it returns the string "(LOCAL)"
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		Name of the WISP SERVER machine or "(LOCAL)"
**
**	WARNINGS:	None
**
*/
const char* wispserver(void)
{
	static char *server = NULL;

	if (!server)
	{
		char 	*ptr;

#ifdef WIN32
		ptr = wgetreg(REG_WISP, "SERVER");
#else
		ptr = getenv( "WISPSERVER" );
#endif

		if (ptr && *ptr)
		{
			server = wstrdup(ptr);
		}
		else
		{
			/*
			**	WISP SERVER was not set so use a dummy value to generate the names.
			*/
			server = DEF_WISPSERVER;
		}
	}
	ASSERT(server);
	
	return server;
}

/*
**	ROUTINE:	wispenvpath()
**
**	FUNCTION:	Get the enviroment PATH
**
**	DESCRIPTION:	Get the PATH from the environment, if not set use ".".
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The PATH
**
**	WARNINGS:	None
**
*/
const char* wispenvpath(void)
{
	static char *path = NULL;
	
	if (!path)
	{
		char *ptr;

		ptr = getenv("PATH");

		if (ptr && *ptr)
		{
			path = wstrdup(ptr);
		}
		else
		{
			path = ".";
		}
	}
	ASSERT(path);
	
	return path;
}

/*
**	ROUTINE:	wisplinkpath()
**
**	FUNCTION:	Get the PATH used for LINK type=S
**
**	DESCRIPTION:	Get the LINK PATH , if not set use ".".
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The PATH
**
**	WARNINGS:	None
**
*/
const char* wisplinkpath(void)
{
	static char *path = NULL;
	
	if (!path)
	{
		const char *ptr;
#ifdef WIN32
		ptr = wgetreg(REG_WISP, "PATH");
#else
		ptr = wispenvpath();
#endif
		if (ptr && *ptr)
		{
			path = wstrdup(ptr);
		}
		else
		{
			path = ".";
		}
	}
	ASSERT(path);
	
	return path;
}

/*
**	ROUTINE:	wisptmpbasedir()
**
**	FUNCTION:	Get the base directory for WISP temporary files and directories.
**
**	DESCRIPTION:	On unix this is "/usr/tmp"
**			On WIN32 this is "C:\TMP"
**
**	ARGUMENTS:	
**	dir		The returned base directory or NULL
**
**	GLOBALS:	None
**
**	RETURN:		The base directory
**
**	WARNINGS:	None
**
*/
const char* wisptmpbasedir(char *dir)
{
	static char *the_dir = NULL;

	if (!the_dir)
	{
#ifdef unix
		the_dir = "/usr/tmp";
#endif
#ifdef WIN32
		char *ptr;
		ptr = wgetreg(REG_WISP, "TMPDIR");
		if (ptr && *ptr)
		{
			the_dir = wstrdup(ptr);
		}
		else
		{
			the_dir = "C:\\TMP";
		}
#endif
	}
	ASSERT(the_dir);

	if (dir)
	{
		strcpy(dir, the_dir);
		return dir;
	}
	else
	{
		return the_dir;
	}
}

/*
**	ROUTINE:	wtmpdir()
**
**	FUNCTION:	Get the system temp directory
**
**	DESCRIPTION:	Uses envvar $TMPDIR if set.
**			On UNIX default to "/tmp"
**			On WIN32 default to "C:\TMP"
**
**	ARGUMENTS:	
**	dir		The returned tmp directory or NULL
**
**	GLOBALS:	None
**
**	RETURN:		The tmp directory
**
**	WARNINGS:	None
**
*/
const char* wtmpdir(char *dir)
{
	static char *the_dir = NULL;

	if (!the_dir)
	{
		char *ptr;
#ifdef WIN32
		ptr = wgetreg(REG_WISP, "TMPDIR");
#else
		ptr = getenv("TMPDIR");
#endif
		if (ptr && *ptr)
		{
			the_dir = wstrdup(ptr);
		}
		else
		{
#ifdef unix
			the_dir = "/tmp";
#endif
#if defined(WIN32)
			the_dir = "C:\\TMP";
#endif
		}
	}
	ASSERT(the_dir);

	if (dir)
	{
		strcpy(dir, the_dir);
		return dir;
	}
	else
	{
		return the_dir;
	}
}

/*
**	ROUTINE:	wispsortmemk()
**
**	FUNCTION:	Get the memory size in K to use for file sorts.
**
**	DESCRIPTION:	If envvar $WISPSORTMEM is set then read it's value
**			otherwise default to 512K.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The size to use in K.
**
**	WARNINGS:	This value gets passed to sortseqf() which may ignore 
**			it if it is outside it's valid range (currently 16-65535).
**
*/
int wispsortmemk(void)
{
	static long memsizek = 0;

	if (!memsizek)
	{
		char *ptr;

#ifdef WIN32
		ptr = wgetreg(REG_WISP, "WISPSORTMEM");
#else
		ptr = getenv("WISPSORTMEM");
#endif
		if (ptr && *ptr)
		{
			sscanf(ptr,"%ld",&memsizek);
		}

		if (memsizek <= 0)
		{
			memsizek = 512;
		}
		
	}
	ASSERT(memsizek);
	
	return memsizek;
}

/*
**	ROUTINE:	wispcpu()
**
**	FUNCTION:	Get the Wang style CPU id.
**
**	DESCRIPTION:	If envvar WISPCPU is set then use it otherwise return spaces.
**			If WISPCPU is set it should be 4 characters long, extra chars
**			will be truncated, short will be padded with spaces.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The 4 characters
**
**	WARNINGS:	
**
*/
const char* wispcpu(void)
{
	static char *ccmm = NULL;
	
	if (!ccmm)
	{
		char *ptr;

#ifdef WIN32
		ptr = wgetreg(REG_VS_EXTRACT, "WISPCPU");
#else
		ptr = getenv("WISPCPU");
#endif
		if (ptr && *ptr)
		{
			int	len;
			len = strlen(ptr);
			
			ccmm = wstrdup("    ");
			memcpy(ccmm, ptr, (len > 4) ? 4 : len);
		}
		else
		{
			ccmm = "    ";
		}
	}
	ASSERT(ccmm);
	
	return ccmm;
}

/*
**	ROUTINE:	wispnetid()
**
**	FUNCTION:	Get the Wangnet Id
**
**	DESCRIPTION:	If $WISPNETID is set use it.  
**			Pad with spaces or truncate to 8 characters.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The netid or spaces if not set.
**
**	WARNINGS:	None
**
*/
const char* wispnetid(void)
{
	static char *netid = NULL;
	
	if (!netid)
	{
		char *ptr;

#ifdef WIN32
		ptr = wgetreg(REG_VS_EXTRACT, "WISPNETID");
#else
		ptr = getenv("WISPNETID");
#endif
		if (ptr && *ptr)
		{
			int	len;
			len = strlen(ptr);
			
			netid = wstrdup("        ");
			memcpy(netid, ptr, (len > 8) ? 8 : len);
		}
		else
		{
			netid = "        ";
		}
	}
	ASSERT(netid);
	
	return netid;
}

/*
**	ROUTINE:	weditorexe()
**
**	FUNCTION:	Get the exe name of the editor.
**
**	DESCRIPTION:	If $WEDITOR is set use it, otherwise default to VSEDIT.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The exe name of the editor.
**
**	WARNINGS:	None
**
*/
char* weditorexe(void)
{
	static char *exe = NULL;

	if (!exe)
	{
		char *ptr;
#ifdef WIN32
		ptr = wgetreg(REG_WISPBIN, "WEDITOR");
#else
		ptr = getenv("WEDITOR");
#endif
		
		if (ptr && *ptr)
		{
			exe = wstrdup(ptr);
		}
		else
		{
#ifdef unix
			exe = "vsedit";
#else
			exe = "vsedit.exe";
#endif
		}
	}
	ASSERT(exe);

	return exe;
}

/*
**	ROUTINE:	wprocexe()
**
**	FUNCTION:	Get the exe name of the procedure interpreter.
**
**	DESCRIPTION:	If $WPROC is set use it, otherwise default to WPROC.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The exe name of the procedure interpreter.
**
**	WARNINGS:	None
**
*/
char* wprocexe(void)
{
	static char *wproc = NULL;

	if (!wproc)
	{
		char *ptr;

#ifdef WIN32
		ptr = wgetreg(REG_WPROC, "WPROC");
#else
		ptr = getenv("WPROC");
#endif
		if (ptr && *ptr)
		{
			wproc = wstrdup(ptr);
		}
		else
		{
#ifdef unix
			wproc = "wproc";
#else
			wproc = "wproc.exe";
#endif
		}
	}
	ASSERT(wproc);

	return wproc;
}

/*
**	ROUTINE:	wprocflags()
**
**	FUNCTION:	Get wproc debug flags
**
**	DESCRIPTION:	Get value for $WPROCDEBUG
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		Flags or NULL if not set.
**
**	WARNINGS:	None
**
*/
char* wprocflags(void)
{
#ifdef WIN32
	return wgetreg(REG_WPROC, "WPROCDEBUG");
#else
	return getenv("WPROCDEBUG");
#endif
}

/*
**	ROUTINE:	acpconfigdir()
**
**	FUNCTION:	Get the path to the ACP configuration directory (location of ACPMAP)
**
**	DESCRIPTION:	If set use $ACPCONFIG otherwise  use the current directory.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The path to the configuration directory or "." if not set.
**
**	WARNINGS:	None
**
*/
const char* acpconfigdir(void)
{
	static char *path = NULL;

	if (!path)
	{
		char *ptr;

#ifdef WIN32
		ptr = wgetreg(REG_ACP, "ACPCONFIG");
#else
		ptr = getenv("ACPCONFIG");
#endif
		if (ptr && *ptr)
		{
			path = wstrdup(ptr);
		}
		else
		{
			path = ".";
		}
	}
	ASSERT(path);
	
	return path;
}

/*
**	ROUTINE:	acpmapfile()
**
**	FUNCTION:	Get the name of the ACP map file.
**
**	DESCRIPTION:	If $ACPMAP id set use it, otherwise default to "ACPMAP"
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The name of the ACP map file.
**
**	WARNINGS:	none
**
*/
const char* acpmapfile(void)
{
	static char *file = NULL;

	if (!file)
	{
		char *ptr;

#ifdef WIN32
		ptr = wgetreg(REG_ACP, "ACPMAP");
#else
		ptr = getenv("ACPMAP");
#endif
		if (ptr && *ptr)
		{
			file = wstrdup(ptr);
		}
		else
		{
			file = "ACPMAP";
		}
	}
	ASSERT(file);

	return file;
}

/*
**	ROUTINE:	wispscratchmode()
**
**	FUNCTION:	Get the mode for the SCRATCH vssub.
**
**	DESCRIPTION:	Currently there is two modes "32" and other.
**			If $WISP_SCRATCH_MODE is set the use it,
**			otherwise return "33" (other).
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The mode string
**
**	WARNINGS:	none
**
*/
const char* wispscratchmode(void)
{
	static char* mode = NULL;
	
	if (!mode)
	{
		char *ptr;
#ifdef WIN32
		ptr = wgetreg(REG_VS_SCRATCH, "WISPSCRATCHMODE");
#else
		ptr = getenv("WISP_SCRATCH_MODE");
#endif

		if (ptr && *ptr)
		{
			mode = wstrdup(ptr);
		}
		else
		{
			mode = "33"; /* WISP Version 3.3 SCRATCH mode */
		}
	}
	ASSERT(mode);

	return mode;
}

#ifdef unix
/*
**	ROUTINE:	wispshellexe()
**
**	FUNCTION:	Get the exe name of the shell.
**
**	DESCRIPTION:	If $SHELL is set use it, otherwise default to "/bin/sh"
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The exe name of the shell
**
**	WARNINGS:	None
**
*/
char* wispshellexe(void)
{
	static char *exe = NULL;

	if (!exe)
	{
		char *ptr= getenv("SHELL");
		
		if (ptr && *ptr)
		{
			exe = wstrdup(ptr);
		}
		else
		{
			exe = "/bin/sh";
		}
	}
	ASSERT(exe);

	return exe;
}
#endif /* unix */

/*
**	ROUTINE:	wispdisplay8bit()
**
**	FUNCTION:	Get the 8-bit characters flag for DISPLAY
**
**	DESCRIPTION:	If $WISP_DISPLAY_8BIT == YES then return 1 otherwise return 0.
**			If not set then check DISPLAY8BIT in the OPTIONS file.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		1 or 0 for Yes or no
**
**	WARNINGS:	None
**
*/
int wispdisplay8bit(void)
{
	static int flag = -1;
	
	if (-1 == flag)
	{
		char *ptr;

		flag = 0;	/* Assume false */

#ifdef WIN32
		ptr = wgetreg(REG_DISPLAY, "WISPDISPLAY8BIT");
#else
		ptr = getenv("WISP_DISPLAY_8BIT");
#endif

		if (ptr)
		{
			if (0==strcmp(ptr,"YES"))
			{
				flag = 1;
			}
		}
		else if (get_wisp_option("DISPLAY8BIT"))
		{
			flag = 1;
		}

	}
	ASSERT(flag != -1);

	return flag;
}

/*
**	ROUTINE:	wispmenudir()
**
**	FUNCTION:	Get the directory for the menu files.
**
**	DESCRIPTION:	If $WISPMENU is set use it, otherwise use the current directory.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The menu directory or ".".
**
**	WARNINGS:	None
**
*/
const char* wispmenudir(void)
{
	static char *dir = NULL;

	if (!dir)
	{
		char	*ptr;

		if ((ptr = getenv("WISPMENU")) && *ptr)
		{
			dir = wstrdup(ptr);
		}
		else
		{
			dir = ".";
		}
	}
	ASSERT(dir);

	return dir;
}

/*
**	ROUTINE:	wispterm()
**
**	FUNCTION:	Get the WISP terminal type
**
**	DESCRIPTION:	
**			Use $WISPTERM, $TERM, default value in that order.
**
**	ARGUMENTS:	
**	the_term	The returned terminal type or an empty string if not set.
**
**	GLOBALS:	none
**
**	RETURN:		The terminal type or an empty string.
**
**	WARNINGS:	None
**
*/
const char *wispterm(char *the_term)
{
	static char *term = NULL;
	
	if (!term)
	{
		char *ptr;

#ifdef WIN32
		if (wisptelnet())
		{
			/*
			**	If no windows then don't use the registry for WISPTERM.
			**	Use the env variables WISPTERM then TERM for telnet.
			*/
			ptr = getenv("WISPTERM");
		}
		else
		{
			ptr = wgetreg(REG_VIDEOCAP, "WISPTERM");
		}
#else
		ptr = getenv("WISPTERM");
#endif
		if (ptr && *ptr)
		{
			term = wstrdup(ptr);
		}
		else if ((ptr = getenv("TERM")) && *ptr)
		{
			term = wstrdup(ptr);
		}
		else
		{
#if defined(WIN32)
			term = "WINCON";
#else
			term = "vt100";
#endif
		}
	}
	ASSERT(term);
	
	if (the_term)
	{
		strcpy(the_term, term);
	}
	return term;
}

/*
**	ROUTINE:	wisptermdir()
**
**	FUNCTION:	Get the directory for terminal definition (videocap) files.
**
**	DESCRIPTION:	
**			On other, if $VIDEOCAP is set use it, otherwise use $WISPCONFIG/videocap.
**
**	ARGUMENTS:	
**	the_dir		The returned videocap directory path.
**
**	GLOBALS:	None
**
**	RETURN:		The videocap directory path.
**
**	WARNINGS:	None
**
*/
const char *wisptermdir(char *the_dir)
{
	static char *dir = NULL;
	
	if (!dir)
	{
		char *ptr;

#ifdef WIN32
		ptr = wgetreg(REG_VIDEOCAP, "VIDEOCAP");
#else
		ptr = getenv("VIDEOCAP");
#endif
		if (ptr && *ptr)
		{
			dir = wstrdup(ptr);
		}
		else
		{
			char	buff[256];
			
			buildfilepath( buff, wispconfigdir(), "videocap" );
			dir = wstrdup(buff);
		}
	}
	ASSERT(dir);
	
	if (the_dir)
	{
		strcpy(the_dir, dir);
	}
	return dir;
}

/*
**	ROUTINE:	wisptermfilepath()
**
**	FUNCTION:	Get the full filepath to the terminal definition (videocap) file.
**
**	DESCRIPTION:	Asseble wispterm() and wisptermdir() into a path.
**			If wispterm() is empty ("") then wisptermfilepath will also be empty.
**
**	ARGUMENTS:	
**	the_path	The returned filepath or empty string.
**
**	GLOBALS:	none
**
**	RETURN:		The filepath or empty string
**
**	WARNINGS:	None
**
*/
const char* wisptermfilepath(char *the_path)
{
	static char *path = NULL;
	
	if (!path)
	{
		const char* ptr = wispterm(NULL);
		
		if (ptr && *ptr)
		{
			char	buff[256];

			buildfilepath(buff, wisptermdir(NULL), ptr);

			/*
			 * Check if the videocap file exists with then without ".vcap" ext.
			 */
			strcat(buff, ".vcap");
			if (!fexists(buff))
			{
				/* Check without ext */
				buff[strlen(buff) - 5] = '\0';
				if (!fexists(buff))
				{
					/* Didn't find it so put ext back on */
					strcat(buff, ".vcap");
				}				
			}
			
			path = wstrdup(buff);
		}
		else
		{
			path = "";
		}
	}
	ASSERT(path);

	if (the_path)
	{
		strcpy(the_path, path);
	}

	return path;
}


#ifdef WIN32
/*
**	ROUTINE:	wispmsgharedir()
**
**	FUNCTION:	Get the directory to use for msg queue files on WIN32
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	the_dir	        The returned dir or empty string.
**
**	GLOBALS:	none
**
**	RETURN:		The filepath or empty string
**
**	WARNINGS:	None
**
*/
const char* wispmsgsharedir(char *the_dir)
{
	static char *dir = NULL;
	
	if (!dir)
	{
		char *ptr;
		
		ptr = wgetreg(REG_VS_MESSAGE, "SHAREDIR");
		if (ptr && *ptr)
		{
			dir = wstrdup(ptr);
		}
		else
		{
			char	buff[128];
			
			buildfilepath(buff, wisptmpbasedir(NULL), "WISPMSG");
			dir = wstrdup(buff);
		}
	}
	ASSERT(dir);

	if (the_dir)
	{
		strcpy(the_dir, dir);
	}

	return dir;
}
#endif

/*
**	ROUTINE:	acu_vutil_exe()
**
**	FUNCTION:	Get the exe name of the Acucobol vutil utility
**
**	DESCRIPTION:	Check $VUTIL or default.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The exe name of vutil
**
**	WARNINGS:	None
**
*/
const char* acu_vutil_exe(void)
{
	static char *exe = NULL;

	if (!exe)
	{
		const char *ptr;

		/*
		 *	First check environment $VUTIL
		 */
		ptr = getenv("VUTIL");
		if (ptr && *ptr)
		{
			exe = wstrdup(ptr);
		}

		/*
		 *	Next check OPTIONS file
		 */
		if (NULL == exe)
		{
			ptr = get_wisp_option("VUTIL");
			
			if (ptr != NULL && *ptr != '\0')
			{
				exe = wstrdup(ptr);
			}
		}
		
		/*
		 *	Use default name
		 */
		if (NULL == exe)
		{
#ifdef unix
			exe = "vutil";
#endif
#ifdef WIN32
			exe = "vutil32.exe";
#endif
		}

		wtrace("WISPCFG","VUTIL","Using VUTIL = [%s]", exe);
		
	}
	ASSERT(exe);

	return exe;
}

/*
**	ROUTINE:	wispdir()
**
**	FUNCTION:	Get path to WISP installation directory
**
**	DESCRIPTION:	
**			On Unix return environment variable $WISPDIR
**			On WIN32 return registry entry
**
**			If WISPDIR is not set it returns a default path.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		Path to WISPDIR directory
**
**	WARNINGS:	None
**
*/
const char* wispdir(void)
{
	static char *wisp_dir = NULL;

	if (!wisp_dir)
	{
		char 	*ptr;

#ifdef unix
		ptr = getenv( "WISPDIR" );
#endif
#ifdef WIN32
		ptr = wgetreg(REG_WISP, "WISPDIR");
#endif

		if (ptr && *ptr)
		{
			wisp_dir = wstrdup(ptr);
		}
		else
		{
#ifdef unix
			wisp_dir = "/usr/local/wisp";
#endif
#ifdef WIN32
			wisp_dir = "C:\\WISP";
#endif
		}
	}
	ASSERT(wisp_dir);
	
	return wisp_dir;
}
const char *wispprbdir(char *dir)
{
	static char *the_dir = NULL;
	
	if (!the_dir)
	{
		char	buff[128];
		buildfilepath(buff, wisptmpbasedir(NULL), "wpparms");
		the_dir = wstrdup(buff);
	}

	if (dir)
	{
		strcpy(dir, the_dir);
		return dir;
	}
	else
	{
		return the_dir;
	}
}

const char *wisplinkdir(char *dir)
{
	static char *the_dir = NULL;
	
	if (!the_dir)
	{
		char	buff[128];
		buildfilepath(buff, wisptmpbasedir(NULL), "wisplink");
		the_dir = wstrdup(buff);
	}

	if (dir)
	{
		strcpy(dir, the_dir);
		return dir;
	}
	else
	{
		return the_dir;
	}
}

const char *wisptmpdir(char *dir)
{
	static char *the_dir = NULL;
	
	if (!the_dir)
	{
		char	buff[128];
		buildfilepath(buff, wisptmpbasedir(NULL), "wisptmp");
		the_dir = wstrdup(buff);
	}

	if (dir)
	{
		strcpy(dir, the_dir);
		return dir;
	}
	else
	{
		return the_dir;
	}
}

const char *wispdefdir(char *dir)
{
	return wisptmpdir(dir);
}

/*
**	ROUTINE:	no_windows()
**
**	FUNCTION:	Flag if windows are not available.
**
**	DESCRIPTION:	On UNIX this is always true.
**			On WIN32 this is normally false unless running in a telnet session.
**
**			This is used to determine if UTILSWINDOWS is valid and on WIN32 if
**			you can run a Windows application (instead of a console app).
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Windows
**	1		No windows
**
**	WARNINGS:	None
**
*/
int no_windows(void)
{
#ifdef unix
	return 1;		/* UNIX always returns "No Windows" */
#endif
#ifdef WIN32

	static int rc = -1;
	
	if (-1 == rc)
	{
		char	*ptr;

		rc = 0;		/* Default to having windows */

		/*
		 *	If running in a telnet session then assume no windows.
		 */
		if (wisptelnet())
		{
			rc = 1;
		}
		
		/*
		 *	Variable WISPNOWINDOWS can force this flag on or off.
		 */
		if (ptr = getenv("WISPNOWINDOWS"))
		{
			if ('1' == *ptr)
			{
				rc = 1;
			}
			else
			{
				rc = 0;
			}
		}
		
		if (1 == rc)
		{
			wtrace("WISPCFG","NOWINDOWS","Running without multiple windows.");
		}
	}
	return rc;

#endif /* WIN32 */
}

/*
**	ROUTINE:	wisptelnet()
**
**	FUNCTION:	Flag if running in a telnet session.
**
**	DESCRIPTION:	
**			On WIN32 this is normally false unless running in a telnet session.
**			On UNIX this is always considered true.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	0	       	Not in a telnet session.
**	1		Running in a telnet session
**
**	WARNINGS:	None
**
*/
int wisptelnet(void)
{
#ifdef unix
	return 1;		/* UNIX always returns true */
#endif
#ifdef WIN32

	static int rc = -1;
	
	if (-1 == rc)
	{
		char	*ptr;

		rc = 0;		/* Default to false */

		/*
		 *	The ATRLS telnet services sets the variable REMOTEADDRESS
		 *	when in a telnet session.  If set then assume telnet.
		 */
		if (getenv("REMOTEADDRESS"))
		{
			rc = 1;
		}

		/*
		 *	Variable WISPTELNET can force this flag on or off.
		 */
		if (ptr = getenv("WISPTELENT"))
		{
			if ('1' == *ptr)
			{
				rc = 1;
			}
			else
			{
				rc = 0;
			}
		}

		if (1 == rc)
		{
			wtrace("WISPCFG","TELNET","Running in a TELNET session.");
		}
		
	}
	return rc;

#endif /* WIN32 */
}

/*
**	ROUTINE:	wisprcfilepath()
**
**	FUNCTION:	Get path to ReturnCode file
**
**	DESCRIPTION:	The file is in the wisptmpdir and is named RC_<userid>_<gid>
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The filepath
**
**	WARNINGS:	None
**
*/
const char* wisprcfilepath(void)
{
	static char *rcfilename = NULL;

	if (!rcfilename)
	{
		char	file[40];
		char	path[128];
		
		sprintf(file, "RC_%s_%u", longuid(), (unsigned)wgetpgrp());
		buildfilepath(path, wisptmpdir(NULL), file);

		rcfilename = wstrdup(path);

		makepath(rcfilename);
	}
	
	return rcfilename;
}


#if defined(DEBUG) && defined(MAIN)
#define EXT_FILEXT
#include "filext.h"

/*
**	ROUTINE:	validate_wispcfg()
**
**	FUNCTION:	validation 
**
**	DESCRIPTION:	setup and call all the routines and assert the results
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
static void validate_wispcfg(void)
{
	char	*ptr;
	char	buff[256];
	
#if defined(unix)
	setenvstr("HOME=/home/user");
	ASSERT(0==strcmp(wisphomedir(buff),"/home/user"));
	ASSERT(0==strcmp(buff,"/home/user"));

	setenvstr("WISPCONFIG=/wisp/config");
	ASSERT(0==strcmp(wispconfigdir(),"/wisp/config"));

	setenvstr("PATH=path");
	ASSERT(0==strcmp(wispenvpath(),"path"));

	ASSERT(0==strcmp(wisptmpbasedir(buff),"/usr/tmp"));
	ASSERT(0==strcmp(buff,"/usr/tmp"));

	setenvstr("TMPDIR=tmpdir");
	ASSERT(0==strcmp(wtmpdir(buff),"tmpdir"));
	ASSERT(0==strcmp(buff,"tmpdir"));

	setenvstr("WISPSORTMEM=123");
	ASSERT(wispsortmemk() == 123);

	setenvstr("WISPTERM=wispterm");
	ASSERT(0==strcmp(wispterm(buff),"wispterm"));
	ASSERT(0==strcmp(buff,"wispterm"));
	
	setenvstr("VIDEOCAP=videocap");
	ASSERT(0==strcmp(wisptermdir(buff),"videocap"));
	ASSERT(0==strcmp(buff,"videocap"));

	ASSERT(0==strcmp(wisptermfilepath(buff),"videocap/wispterm"));
	ASSERT(0==strcmp(buff,"videocap/wispterm"));

#endif /* unix */

#ifndef WIN32
	setenvstr("WISPCPU=12");
	ASSERT(0==strcmp(wispcpu(),"12  "));

	setenvstr("WISPNETID=wispnet");
	ASSERT(0==strcmp(wispnetid(),"wispnet "));

	setenvstr("WEDITOR=weditor");
	ASSERT(0==strcmp(weditorexe(),"weditor"));

	setenvstr("WPROC=test");
	ASSERT(0==strcmp(wprocexe(),"test"));

	setenvstr("WPROCDEBUG=debug");
	ASSERT(0==strcmp(wprocflags(),"debug"));

	setenvstr("ACPCONFIG=acpconfig");
	ASSERT(0==strcmp(acpconfigdir(),"acpconfig"));

	setenvstr("ACPMAP=acpmap");
	ASSERT(0==strcmp(acpmapfile(),"acpmap"));

	setenvstr("WISP_SCRATCH_MODE=");
	ASSERT(0==strcmp(wispscratchmode(),"33"));

	setenvstr("SHELL=shell");
	ASSERT(0==strcmp(wispshellexe(),"shell"));

	setenvstr("WISP_DISPLAY_8BIT=No");
	ASSERT(wispdisplay8bit() == 0);

	setenvstr("WISPMENU=wispmenu");
	ASSERT(0==strcmp(wispmenudir(),"wispmenu"));
#endif
}

main()
{
	printf("Validate wispcfg routines\n");
	validate_wispcfg();
	return 0;
}

#endif /* DEBUG && MAIN */

/*
**	History:
**	$Log: wispcfg.c,v $
**	Revision 1.24  2001-11-27 17:05:03-05  gsl
**	Change to use longuid()
**
**	Revision 1.23  2001-11-08 11:47:58-05  gsl
**	add missing include
**
**	Revision 1.22  2001-10-31 15:34:55-05  gsl
**	moved wisprcfilepath() from retcode.c
**	Change wispdefdir() for unix to be the same as WIN32 and return wisptmpdir()
**	This is used by the temp PERSON files (now DEFS_)
**
**	Revision 1.21  2001-10-11 11:11:29-04  gsl
**	change wisptermfilepath() to first check for .vcap extension
**
**	Revision 1.20  2001-10-11 10:52:19-04  gsl
**	Remove VMS & MSDOS
**
**	Revision 1.19  1999-08-24 09:29:35-04  gsl
**	Fixed acu_vutil_exe() to get the VUTIL path out of the OPTIONS file.
**	On WIN32 it was looking in the registry, however, the registry never was
**	set. So now it check env $VUTIL first then VUTIL in OPTIONS then defaults.
**
**	Revision 1.18  1999-02-24 19:00:18-05  gsl
**	Added routine wisptelnet() and check for REMOTEADDRESS variable for ATRLS
**	In no_windows() add test of var WISPNOWINDOWS to force the value.
**
**	Revision 1.17  1999-02-23 16:58:21-05  gsl
**	Move the no_windows() routine from wfiledis.c
**
**	Revision 1.16  1999-02-23 16:26:18-05  gsl
**	For WIN32 if no_windows() (telnet) then use env vars WISPTERM and TERM
**	instead of the registry for the videocap file info.
**
**	Revision 1.15  1997-12-18 09:13:50-05  gsl
**	add missing include file
**
**	Revision 1.14  1997-12-15 15:38:09-05  gsl
**	Change wispdisplay8bit() to also check the DISPLAY8BIT option
**	in the OPTIONS file
**
**	Revision 1.13  1997-12-04 18:11:30-05  gsl
**	Add wisplinkpath()
**
**	Revision 1.12  1997-08-23 18:36:02-04  gsl
**	Move routines from wperson.c
**
**	Revision 1.11  1997-03-13 17:10:21-05  gsl
**	Add routine wispdir() which returns the WISP installation directory
**
**	Revision 1.10  1997-02-28 16:41:28-05  gsl
**	Added wispserver()
**
**	Revision 1.9  1996-12-11 19:48:48-05  gsl
**	Add acu_vutil_exe() routine
**
**	Revision 1.8  1996-12-11 13:25:47-08  gsl
**	Fix buf with TMPDIR
**
**	Revision 1.7  1996-11-18 12:52:58-08  gsl
**	Switched WIN32 to now get all the config info from the registry.
**
**	Revision 1.6  1996-11-11 09:05:47-08  jockc
**	added wispmsgsharedir for win32 msg routine's files
**
**	Revision 1.5  1996-10-14 11:27:14-07  gsl
**	Add validation routines
**
**	Revision 1.4  1996-10-11 17:22:56-07  gsl
**	add wispterm() wisptermdir() and wisptermfilepath()
**	These are used for identifing which videocap file to use for video.
**
**	Revision 1.3  1996-10-09 14:51:20-07  gsl
**	Document and standardize all the routines
**
**	Revision 1.2  1996-10-08 17:28:51-07  gsl
**	Write all the replacement routines for getenv() calls.
**	Centralize all the configuration variables to this file
**
**	Revision 1.1  1996-10-08 10:05:50-07  gsl
**	Initial revision
**
**
**
*/
