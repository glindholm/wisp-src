static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**      File:           wdiag.c
**
**      Purpose:        To analyze the system environment for files and variables needed to properly
**                      wisp, compile and run programs with the wisp runtime.
**
**                      Diagnose common wisp problems.
**
**                              uname
**                              $WISPCONFIG
**                              LGMAP
**                              wsysconfig
**                              wrunconfig
**                              wispmsg.dat
**                              videocap
**                              $VIDEOCAP
**                              $WISPTERM
**                              $TERM
**                              $HOME
**                              wrun
**                              /tmp
**                              /usr/tmp
**                              $ACU
**                              $A_CONFIG
**                              $A_TERMCAP
**                              a_termcap
**                              cblhelp
**                              $COBDIR
**                              $COBSW
**                              $COBPATH
**                              $COBOPT
**				$WISPGID
**
**
**      Routines:       check_access()          Checks the access permissions for files
**			define_os()		Gets system OS definitions
**			define_videocap()	Gets video control information
**			define_acu()		Gets ACUCOBOL information
**			define_mf()		Gets Micro Focus information
**			
**
**
**
*/


#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#ifdef unix
#include <sys/utsname.h>
#endif

#if defined(MSDOS) || defined(WIN32)
#include <process.h>
#include <conio.h>
#include <io.h>
#endif

#ifdef MSDOS
#include <dos.h>
#endif

#define EXT_FILEXT
#include "filext.h"

#include "paths.h"
#include "wrunconf.h"
#include "idsisubs.h"
#include "wispcfg.h"
#include "wlicense.h"
#include "wsysconf.h"
#include "wrunconf.h"
#include "machid.h"
#include "wisplib.h"
#include "wperson.h"
#include "wispvers.h"
#include "wispnt.h"

#ifndef false
#define false 0;
#define true !false;
#endif

/*
**	Define  called routines
*/
int check_access(const char *ptr, int mode);
int     define_os();
int	define_videocap();
int run_command(char *command);
int define_acu(const char* rts);
void checkexe(const char *tag, const char *exe);
void checklinkexe(const char *tag, const char *exe);
void print_options_file(const char* options_path);


/*
**	Global defines and data
*/
#define mfcobol 98
#define acucobol 99

int     comp,
	errsw,
	erracu,
	errmf;


/*
**      Routine:        main()
**
**      Function:       To run the basic checking functions.
**
**      Description:    To check for unix shell variables, wisp variables, files and
**                      information and compiler related variables and files.  The
**                      standard logic path is as follows: 1) request variable value,
**                      2) print variable value, 3) if a file path, check for file
**                      existance and access level, 4) print file status.
**
**      Input:          None
**
**      Output:         None
**
**      Return:         None
**
**      Warnings:       None
**
**
*/

main()
{
	char    buff[256];

	const char *cptr;
	const char *wc;
	const char *vcf;
	
	char     ut[256];

	struct wruncfg wrun_cfg;


	printf("**********************************************************************\n");
	printf("*                            WISP - WDIAG                            *\n");
	printf("*                    System Environment Diagnostic                   *\n");
	printf("*                  NeoMedia Technologies Incorporated                *\n");
	printf("*                        All rights reserved.                        *\n");
	printf("*                          Version %s                           *\n", wisp_version());
	printf("**********************************************************************\n");


	/*
	**	Set flags false
	*/
	erracu = false;
	errmf  = false;
	errsw  = false;
	comp   = false;

	/*
	**	Check operating system
	*/
	define_os(ut);

	/*
	**       Check for a wispconfig variable, videocap directory and/or
	**       videocap variable
	*/
	printf("\n\n");
	printf("WISP Environment\n");
	printf("================\n");

	if (wc = wispconfigdir())
	{
		printf("WISPCONFIG  = %s",wc);
		if (check_access(wc,05) != 0)
		{
			errsw = true;
		}
	}
	else
	{
		errsw = true;
		printf("WISPCONFIG  = *** NOT DEFINED ***\n\n");
	}

	cptr = wispdir();
	printf("WISPDIR     = %s",cptr);
	if (check_access(cptr,05) != 0)
	{
		errsw = true;
	}

	/*
	**	Videocap file
	*/
#ifdef unix
	if (cptr = getenv("WISPTERM"))
	{
		if (cptr[0] == '\0')
		{
			errsw = true;
			printf("WISPTERM    = (NULL) *** ERROR\n",cptr);
		}
		else
		{
			printf("WISPTERM    = %s\n",cptr);
		}
	}
	else
	{
		printf("WISPTERM    = *** NOT DEFINED\n");
	}

	printf("VIDEOCAP    = %s\n", (cptr = getenv("VIDEOCAP"))? cptr: "*** NOT DEFINED");

#endif

	/*
	**	Check for the videocap files
	*/
	vcf = wisptermfilepath(NULL);
	
	printf("videocapfile= %s",vcf);
	if (check_access(vcf,04) != 0)
	{
		errsw = true;
		printf("*** NO VIDEOCAP FILE FOUND ***\n\n");
	}

	cptr = wisptmpbasedir(NULL);
	printf("wisptmpbase = %s", cptr);
	if (check_access(cptr,07) != 0)
	{
		errsw = true;
	}

	cptr = wtmpdir(NULL);
	printf("wtmpdir     = %s", cptr);
	if (check_access(cptr,07) != 0)
	{
		errsw = true;
	}

	cptr = license_filepath(NULL);
	printf("licensefile = %s", cptr);
	if (check_access(cptr,04) != 0)
	{
		errsw = true;
	}
	
	printf("sortmem     = %d(K)\n", wispsortmemk());


#ifdef unix
	if (cptr = getenv("WISPGID"))
	{
		if (cptr[0] == '\0')
		{
			errsw = true;
			printf("WISPGID     = (NULL) *** ERROR\n");
		}
		else
		{
			printf("WISPGID     = %s\n",cptr);
		}
	}	
	else	
	{
		printf("WISPGID     = *** NOT DEFINED\n");
		errsw = true;
	}
#endif


	/*
	**       Check the wisp runtime control files and data files
	**       Use WISPCONFIG to find these file, wc points to WISPCONFIG
	*/
	printf("\n");

	buildfilepath(buff,wispconfigdir(),"LGMAP");
	printf("LGMAP       = %s",buff);
	if (check_access(buff,04) != 0)
	{
		errsw = true;
	}

	buildfilepath(buff,wispconfigdir(),CFGFNAME);
	printf("WSYSCONF    = %s",buff);
	if (check_access(buff,04) != 0)
	{
		errsw = true;
	}

	buildfilepath(buff,wispconfigdir(),"wispmsg.dat");
	printf("WISPMSG     = %s",buff);
	if (check_access(buff,04) != 0)
	{
		errsw = true;
	}

	buildfilepath(buff,wispconfigdir(),"OPTIONS");
	printf("OPTIONS     = %s",buff);
	if (check_access(buff,04) != 0)
	{
		errsw = true;
	}
	else
	{
		print_options_file(buff);
	}

	buildfilepath(buff,wispconfigdir(),"wproc.msg");
	printf("WPROCMSG    = %s",buff);
	if (check_access(buff,04) != 0)
	{
		errsw = true;
	}

	buildfilepath(buff,wispconfigdir(),WRUNCONFIG);
	printf("WRUNCONFIG  = %s",buff);
	if (check_access(buff,04) != 0)
	{
		errsw = true;
	}

	/*
	**	Check on wrun
	*/
	wrunconfig(&wrun_cfg);					/* Load wrunconfig options file		*/

	printf("WRUN COBOL   = %s\n", wrun_cfg.wrun_cobtype);
	printf("WRUN RUNCBL  = %s\n", wrun_cfg.wrun_runcbl);
	printf("WRUN OPTIONS = %s\n", wrun_cfg.wrun_options);

	if (0 == strcmp("ACU",wrun_cfg.wrun_cobtype))
	{
		comp = acucobol;
		printf("COBOL       = ACUCOBOL\n");
	} 
	else if (0 == strcmp("MF",wrun_cfg.wrun_cobtype))
	{
		comp = mfcobol;
		printf("COBOL       = MICRO FOCUS\n");
	}
	else
	{
		errsw = true;
		printf("COBOL       = *** NOT DEFINED\n");
	}

	checkexe("RTS", wrun_cfg.wrun_runcbl);

	printf("LINKPATH    = %s\n", wisplinkpath());
	

	/*
	**	Check for WISP utilitys
	*/
	printf("\n");

	checklinkexe("WSHELL",  "wshell");
	checklinkexe("PROC",     wprocexe());
	checklinkexe("COPY",    "wcopy");
	checklinkexe("SORT",    "wsort");
	checklinkexe("EDITOR",   weditorexe());
	checklinkexe("DISPLAY", "display");

	/*
	**	Check the ACUCOBOL environment
	*/
	if (comp == acucobol)
	{
		define_acu(wrun_cfg.wrun_runcbl);
	}

#ifdef unix
	/*
	**	Check the Micro Focus environment
	*/
	define_mf();
#endif


	printf("\n");
	if ((erracu != 0) && (comp == acucobol))
	{
		printf("\n");
		printf(">>>>>>> ACUCOBOL SETUP ERRORS FOUND <<<<<<<\n\n");
		errsw = true;
	}

#ifdef unix
	if ((errmf != 0) && (comp == mfcobol))
	{
		printf("\n");
		printf(">>>>>>> MICRO FOCUS SETUP ERRORS FOUND <<<<<<<\n\n");
		errsw = true;
	}
#endif

	if (errsw == 0)
	{
		if (comp == mfcobol)
		{
			printf(">>>>>>> MICRO FOCUS SETUP FOUND\n");
		}
		if (comp == acucobol)
		{
			printf(">>>>>>> ACUCOBOL SETUP FOUND\n");
		}
		printf(">>>>>>> WISP ENVIRONMENT OK <<<<<<<\n\n");
	}

	if (errsw != 0)
	{
		printf(">>>>>>> ENVIRONMENT SETUP ERRORS FOUND <<<<<<<\n\n");
	}	
	return 0;
}

/*
**      Routine:        define_os()
**
**      Function:       To check basic system characteristics.
**
**      Description:    To check for operating system variables
**
**      Input:          ut points to the TERM type work area
**
**      Output:         ut points to the returned TERM type
**
**      Return:         None
**
**      Warnings:       The flag errsw is a global variable set
**			in this routine
**
**      History:        06/15/93        Written by JAK
**
*/

int define_os(ut)
char	*ut;
{

#ifdef unix
	struct utsname unix_name;
	char	buff[256];
#endif

	char	temp[80];
	
	char	*ptr;
	const char *cptr;

	int	errsw=false;

#ifdef unix
	/*
	**       Check the basic unix shell environment
	*/
	printf("\n");
	printf("UNIX Environment\n");
	printf("================\n");
	if (uname(&unix_name) != -1)
	{
		sprintf(temp, "%s %s %s %s %s", unix_name.sysname,
						unix_name.nodename,
						unix_name.release,
						unix_name.version,
						unix_name.machine);
		printf("UNIX        = %s\n",temp);
	}
	else
	{
		printf("UNIX        = *** TYPE NOT DEFINED\n");
	}
#endif

#ifdef MSDOS
	/*
	**       Check the basic DOS shell environment
	*/
	printf("\n");
	printf("     DOS Environment\n");
	if (_osmajor > 0)
	{
		sprintf(temp, "%d.%d", _osmajor,
				       _osminor);
		printf("DOS         = %s\n",temp);
	}
	else
	{
		printf("DOS         = *** TYPE NOT DEFINED\n");
	}
#endif

#ifdef WIN32
	/*
	**       Check the basic DOS shell environment
	*/
	printf("\n");
	printf("WIN32 Environment\n");
	printf("=================\n");
	if (win32_nt())
	{
		printf("WINDOWS     = NT\n");
	}
	else if (win32_95())
	{
		printf("WINDOWS     = 95 (Not NT)\n");
	}
	else
	{
		printf("WINDOWS     = Unknown (Not NT)\n");
	}
#endif

	if (cptr = wisphomedir(NULL))
	{
		printf("HOME        = %s",cptr);
		if (check_access(cptr,07) != 0)
		{
			errsw = true;
		}
	}
	else
	{
		errsw = true;
		printf("HOME        = *** NOT DEFINED\n");
	}

	if (ptr = getenv("PATH"))
	{
		printf("PATH        = %s\n",ptr);
	}
	else
	{
		errsw = true;
		printf("PATH        = *** NOT DEFINED\n");
	}

#ifdef unix
	if (ptr = getenv("SHELL") )
	{
		printf("SHELL       = %s",ptr);
		if (ptr[0] == '/')
		{
			/*
			**	Only check access if a full path given
			*/
			if (check_access(ptr,01) != 0)
			{
				errsw = true;
			}
		}
	}
	else
	{
		printf("SHELL       = Assuming shell /bin/sh\n");
	}

	/*
	**       Check the terminal definition variables for unix and wisp
	*/
	printf("\n");
	if (ut = getenv("TERM"))
	{
		if (ut[0] == '\0')
		{
			errsw = true;
			printf("TERM        = (NULL) *** ERROR\n");
		}
		else
		{
			printf("TERM        = %s\n",ut);
		}
	}
	else
	{
		errsw = true;
		printf("TERM        = *** NOT DEFINED\n");
	}

	printf("\n");

	printf("TEMP        = /usr/tmp");
	if (check_access("/usr/tmp",07) != 0)
	{
		   errsw = true;
	}

	printf("TEMP        = /tmp");
	if (check_access("/tmp",07) != 0)
	{
		   errsw = true;
	}

	if (ptr = getenv("TMPDIR"))
	{
		printf("TMPDIR      = %s",ptr);
		if (check_access(ptr,07) != 0)
		{
			errsw = true;
		}
	}
	else
	{
		printf("TMPDIR      = (Not defined)\n");
	}
#endif

#ifdef MSDOS

	/*
	**       Check for the temporary work file directories
	*/

	if (ptr = getenv("TMP"))
	{
		printf("\n");
		printf("TMP         = %s",ptr);
		if (check_access(ptr,07) != 0)
		{
			errsw = true;
		}
	}
#endif

	printf("MACHID      = %s\n", (0==getmachineid(temp)) ? temp : "(unknown)");

	printf("COMPNAME    = %s\n", computername(NULL));

	printf("SERVER      = %s\n", wispserver());

	printf("ttyname     = %s\n", ttyname(0));

	ttyid5(temp);
	printf("ttyid5      = %s\n", temp);
	

	return (0);
}


/*
**      Routine:        define_acu()
**
**      Function:       To check ACUCOBOL system configuration.
**
**      Description:    To check for system variables
**
**      Input:          none
**
**      Output:         none
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/15/93        Written by JAK
**
*/

int define_acu(const char* rts)
{
	char    *acu;
	char	*ptr;
#ifdef unix
	char	command[256];
#endif

	/*
	**       Get the ACUCOBOL Cobol environment data
	*/

	printf("\n\n");
	printf("ACUCOBOL Environment\n");
	printf("====================\n");

	if (acu = getenv("A_CONFIG"))
	{
		printf("A_CONFIG    = %s",acu);
		if (check_access(acu,04) != 0)
		{
			erracu = true;
		}	
	}
	else
	{
		printf("A_CONFIG    = *** NOT DEFINED\n");
	}

#ifdef unix
	if (acu = getenv("A_TERMCAP"))
	{
		printf("A_TERMCAP   = %s",acu);
		if (check_access(acu,04) != 0)
		{
			erracu = true;
		}
	}
	else
	{
		ptr = "/etc/a_termcap";
		printf("a_termcap   = %s",ptr);
		if (check_access(ptr,04) != 0)
		{
			erracu = true;
		}
	}
#endif

#ifdef unix
	ptr = "/etc/cblhelp";
#endif
#ifdef MSFS
	ptr = "\\ETC\\CBLHELP";
#endif
	printf("cblhelp     = %s", ptr);
	check_access(ptr,04);


	checkexe("VUTIL", acu_vutil_exe());

#ifdef unix	
	sprintf(command,"%s -V", rts);
	run_command(command);
#endif

	return(0);
}



/*
**      Routine:        define_mf()
**
**      Function:       To check Micro Focus system characteristics.
**
**      Description:    To check for system variables
**
**      Input: 		none
**
**      Output:		none
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/15/93        Written by JAK
**
*/

int define_mf()
{
	char    *mf,
		*ptr;

	char    file_path[256];

	 /*
	 **       Get the Micro Focus Cobol environment data
	 */
	 printf("\n\n");
	 printf("Micro Focus Environment\n");
	 printf("=======================\n");

	 if (mf = getenv("COBDIR"))
	 {
		printf("COBDIR      = %s",mf);
		if (check_access(mf, 04) != 0)
		{
			errmf = true;
		}
	 }
	 else
	 {
		  errmf = true;
		  printf("COBDIR      = *** NOT DEFINED\n");
	 }

	 if (mf = getenv("COBSW"))
	 {
		  printf("COBSW       = %s\n",mf);
	 }
	 else
	 {
		  printf("COBSW       = *** NOT DEFINED\n");
	 }

	 if (mf = getenv("COBOPT"))
	 {
		  printf("COBOPT      = %s\n",mf);
	 }
	 else
	 {
		  printf("COBOPT      = *** NOT DEFINED\n");
	 }

	 if (mf = getenv("COBPATH"))
	 {
		  printf("COBPATH     = %s\n",mf);
	 }
	 else
	 {
		  printf("COBPATH     = *** NOT DEFINED\n");
	 }

	ptr = "fhconvert";

	if (0 == whichenvpath(ptr,file_path))
	{
		printf("fhconvert   = %s   [FOUND]\n",file_path);
	}
	else
	{
		printf("fhconvert   = %s   *** NOT FOUND ***\n",ptr);
		errmf = true;
	}


	ptr = "cob";

	if (0 == whichenvpath(ptr,file_path))
	{
		printf("cob         = %s   [FOUND]\n",file_path);

		if (0 != run_command("cob -V"))
		{
			errmf = true;
		}
	}
	else
	{
		printf("cob         = %s   *** NOT FOUND ***\n",ptr);
		errmf = true;
	}

	return(0);
}


/*
**      Routine:        check_access()
**
**      Function:       To check for specified files and thier access levels
**
**      Description:    To check for the existance of a specified file and that its'
**                      access permisions (level) are correct for the access mode
**                      defined in the checking request.
**
**      Input:          ptr is the pointer to the data string defining the file path.
**                      mode is the access bit pattern to be validated for file access.
**
**      Output:         None
**
**      Return:         access routine return code or zero.
**
**      Warnings:       None
**
**      History:        04/01/92        Written by GSL
**
*/

int check_access(const char *ptr, int mode)
{
		int     rc;

		if (rc = access(ptr, mode))
		{
			switch(errno)
			{
				case ENOENT:    printf("   *** NOT FOUND ***\n");
						break;
				case EACCES:    printf("   *** ACCESS DENIED ***\n");
						break;
#ifndef WATCOM
				case ENOTDIR:   printf("   *** INVALID DIRECTORY ***\n");
						break;
#endif
				default:        printf("   *** UNABLE TO ACCESS ***\n");
						break;
			}
		}
		else
		{
				rc = false;
				printf("   [FOUND]\n");
		}
		return(rc);
}

int run_command(char *command)
{
#ifdef unix
	FILE    *file;
	char	buff[1024];
	char	unixcommand[1024];
#endif

	printf("\n");
	printf("$ %s\n",command);
#ifdef unix
	sprintf(unixcommand, "%s 2>&1", command);
	
	if ((file = popen(unixcommand,"r")) != NULL)
	{
		while (fgets(buff, sizeof(buff), file) != NULL)
		{
			printf(" > %s", buff);
		}
		pclose(file);
		printf("\n");
	}
	else
	{
		return 1;
	}
#endif
#if defined(MSDOS) || defined(WIN32)
	system (command);
#endif

	return 0;
}

void checkexe(const char *tag, const char *exe)
{
	char	buff[256];
	char	file_path[256];
	
	strcpy(buff, exe);
	
#ifdef MSFS
	upper_string(buff);
	if (!osd_ext(buff))
	{
		strcat(buff,".EXE");
	}
#endif
	if (0 == whichenvpath(buff,file_path))
	{
		printf("%-10.10s  = %s  [FOUND]\n", tag, file_path);
	}
	else
	{
		printf("%-10.10s  = %s", tag, buff);
		if (check_access(buff,05) != 0)
		{
			errsw = true;
		}
	}
}

void checklinkexe(const char *tag, const char *exe)
{
	char	buff[256];
	char	file_path[256];
	
	strcpy(buff, exe);
	
#ifdef MSFS
	upper_string(buff);
	if (!osd_ext(buff))
	{
		strcat(buff,".EXE");
	}
#endif
	if (0 == whichlinkpath(buff,file_path))
	{
		printf("%-10.10s  = %s  [FOUND]\n", tag, file_path);
	}
	else
	{
		printf("%-10.10s  = %s", tag, buff);
		if (check_access(buff,05) != 0)
		{
			errsw = true;
		}
	}
}

void print_options_file(const char* options_path)
{
	FILE 	*the_file;
	char	inlin[512], keyword[80], value[80];
	int	cnt,len;

	the_file = fopen(options_path,"r");
	if (the_file)
	{
		while(fgets(inlin,sizeof(inlin)-1,the_file))
		{
			len=strlen(inlin);
			if (len>0 && inlin[len-1] == '\n') inlin[len-1] = '\0';	/* Null out the newline char		*/
			cnt = sscanf(inlin,"%s %s",keyword,value);
			if ( cnt < 1 ) continue;
			if (keyword[0] == '#') continue;

			printf(" > %s\n",inlin);
		}
		fclose(the_file);
	}
	else
	{
		printf(">>>>>> Unable to open OPTIONS file [%s]\n",options_path);
	}

}



#include "wutils.h"
/*
**	History:
**	$Log: wdiag.c,v $
**	Revision 1.14  1998-01-09 08:48:12-05  gsl
**	Remove non-error conditions from microfocus
**
**	Revision 1.13  1997-12-04 18:15:45-05  gsl
**	Added support of Windows NT/95
**
**	Revision 1.12  1996-12-12 13:13:56-05  gsl
**	DevTech -> NeoMedia
**
**	Revision 1.11  1996-09-10 09:14:07-07  gsl
**	fix compiler warnings
**
**	Revision 1.10  1996-09-04 17:29:41-07  gsl
**	Add include of filext.h
**
**	Revision 1.9  1996-08-23 14:23:28-07  gsl
**	change to use wisptmpbasedir()
**
**	Revision 1.8  1996-07-23 11:13:07-07  gsl
**	drcs update
**
**
**
*/
