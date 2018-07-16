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
#include <sys/types.h>
#include <time.h>

#ifdef unix
#include <sys/utsname.h>
#include <unistd.h>
#endif

#if defined(WIN32)
#include <process.h>
#include <conio.h>
#include <io.h>
#endif

#include "filext.h"

#include "paths.h"
#include "wrunconf.h"
#include "idsisubs.h"
#include "wispcfg.h"
#include "wisplib.h"
#include "wlicense.h"
#include "wsysconf.h"
#include "wrunconf.h"
#include "machid.h"
#include "wisplib.h"
#include "wperson.h"
#include "wispvers.h"
#include "wispnt.h"
#include "wglobals.h"
#include "wfiledis.h"
#include "platsubs.h"

#ifndef false
#define false 0;
#define true !false;
#endif

#define ACC_EXECUTE	01
#define ACC_READ	04
#define ACC_RX		05
#define ACC_FULL	07

/*
**	Define  called routines
*/
int check_access(int* pnError, const char *ptr, int mode);
int     define_os();
int	define_videocap();
int run_command(char *command);
int define_acu(const char* rts);
int checkexe(const char *tag, const char *exe, int* pError);
void checklinkexe(const char *tag, const char *exe);
void print_config_file(const char* options_path);
static void cat_file(const char* file_path);
static void check_path(const char* path);
static void test_int_sizes(void);

static int define_mf();

/*
**	Global defines and data
*/

#define UNKNOWNCOBOL 0
#define MFCOBOL 98
#define ACUCOBOL 99

static int	nCobol = UNKNOWNCOBOL;

static int	errsw = 0;	/* Error count (non-cobol) */
static int	erracu = 0;	/* Error count for Acucobol */
static int	errmf = 0;	/* Error count for MicroFocus */

static struct wruncfg wrun_cfg;

#ifdef unix
static int bBourneShell = 1; /* Assume using bourne shell */
#endif


static void print_nl(void)
{
	printf("\n");
}

static void print_mess_nl(const char* mess)
{
	printf(" %s\n",mess);
}

static void print_inset(const char* value)
{
	printf(" > %-52s", value);
}

static void print_pair(const char* lhs, const char *rhs)
{
	printf("%-12s = %-40s", lhs, rhs);
}

static void print_err_mess(const char* pszSeverity, const char *pszType)
{
	printf(" *** %s %s ***\n", pszSeverity, pszType);
}

static void print_pair_err_mess(const char* lhs, const char *rhs, const char* pszSeverity, const char *pszType)
{
	print_pair(lhs,rhs);
	print_err_mess(pszSeverity, pszType);
}

static void print_inset_nl(const char* value)
{
	print_inset(value);
	print_nl();
}

static void print_pair_nl(const char* lhs, const char *rhs)
{
	print_pair(lhs, rhs);
	print_nl();
}

static void print_pair_mess(const char* lhs, const char *rhs, const char* mess)
{
	print_pair(lhs, rhs);
	print_mess_nl(mess);
}

static int print_access(const char* pszLabel, const char* pszPath, int nAccess, int* pnError)
{
        if (NULL == pszPath)
	{
	        pszPath = "";
        }
       
	print_pair(pszLabel, pszPath);

	return check_access(pnError, pszPath,nAccess);
}

static int print_envvar(const char* pszVar, int* pnError)
{
	const char* cptr;
	int     rc = 0;
	int	nErrorLocal;
	char* 	pszSeverity;
  
	if (NULL == pnError)
	{
		pszSeverity = "WARNING";
		pnError = &nErrorLocal;	/* Fake it */
	}
	else
	{
		pszSeverity = "ERROR";
	}
	
	if ((cptr = getenv(pszVar)))
	{
		if (cptr[0] == '\0')
		{
			rc = 1;
			(*pnError)++;
			print_pair_err_mess(pszVar, "(NULL)",pszSeverity,"NULL VARIABLE");
		}
		else
		{
			print_pair_nl(pszVar, cptr);
		}
	}
	else
	{
		print_pair_nl(pszVar, "(not defined)");
	}

	return rc;
}




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

int main(int argc, char* argv[], char* envp[])
{
	time_t now;
	char    buff[256];

	const char *cptr;
	
	char     ut[256];

	test_int_sizes();

	WL_load_options();

	printf("**********************************************************************\n");
	printf("*                            WISP - WDIAG                            *\n");
	printf("*                    System Environment Diagnostic                   *\n");
	printf("*                  NeoMedia Technologies Incorporated                *\n");
	printf("*                        All rights reserved.                        *\n");
	printf("*                  Version %7s %-20.20s              *\n", wisp_version(), WL_platform_name());
	printf("**********************************************************************\n");

	now = time(NULL);
	printf("\n%s\n", ctime(&now));

#ifdef unix
	run_command("id");
#endif
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

	if (0 == strcmp(wispconfigdir(),WISPCONFIG_UNSET_VALUE))
	{
		print_pair_err_mess("WISPCONFIG","(not set)","ERROR","NOT SET");
	}
	else
	{
		print_access("WISPCONFIG", 	wispconfigdir(), 	ACC_RX, &errsw);
	}

	print_access("WISPDIR", 	wispdir(), 		ACC_RX, &errsw);

	/*
	**	Videocap file
	*/
#ifdef unix
	print_envvar("WISPTERM", &errsw);
	print_envvar("VIDEOCAP", NULL);
#endif

	/*
	**	Check for the videocap files
	*/
	print_access("videocapfile", wisptermfilepath(NULL), ACC_READ, &errsw);
	print_access("WISPTMPDIR",   wisptmpbasedir(NULL),   ACC_FULL, &errsw);
	print_access("SYSTMPDIR",    WL_systmpdir(NULL),     ACC_FULL, &errsw);
#ifdef WIN32
	print_access("WISPSHAREDIR", wispmsgsharedir(NULL),  ACC_FULL, NULL); /* warning */
#endif

	cptr = WLIC_license_filepath();
	if (print_access("licensefile", cptr, ACC_READ, &errsw) == 0)
	{
		cat_file(cptr);
	}
	
	sprintf(buff,"%d(K)",wispsortmemk());
	print_pair_nl("sortmem",buff);


#ifdef unix
	if (getenv("WISPGID"))
	{
		print_envvar("WISPGID", &errsw);
	}
	else if (bBourneShell)
	{
		print_pair_nl("WISPGID","(not defined)");
	}
	else
	{
		errsw++;
		print_pair_err_mess("WISPGID","(not defined)","ERROR","NOT DEFINED");
	}
#endif


	/*
	**       Check the wisp runtime control files and data files
	**       Use WISPCONFIG to find these file, wc points to WISPCONFIG
	*/
	print_nl();

	buildfilepath(buff,wispconfigdir(),"LGMAP");
	if (print_access("LGMAP",buff,ACC_READ,&errsw) == 0)
	{
		logical_id	*logical_ptr;
		logical_ptr = WL_get_lgmap_list();

		while(logical_ptr)
		{
			if (0 != strcmp(logical_ptr->logical,"."))
			{
				char lgbuff[256];
				
				sprintf(lgbuff, "%-6.6s %s", logical_ptr->logical, logical_ptr->translate);
				print_inset(lgbuff);
				check_access(NULL,logical_ptr->translate,ACC_RX);
			}

			logical_ptr = logical_ptr->next;
		}
		print_nl();
	}

	buildfilepath(buff,wispconfigdir(),"RVMAP");
	if (0==access(buff,ACC_READ))
	{
		print_access("RVMAP",buff,ACC_READ,NULL);
		print_config_file(buff);
	}
	else
	{
		print_pair_nl("RVMAP","(Not used)");
	}
	

	buildfilepath(buff,wispconfigdir(),CFGFNAME);
	print_access("WSYSCONF", buff, ACC_READ, &errsw);

	buildfilepath(buff,wispconfigdir(),"OPTIONS");
	if (print_access("OPTIONS",buff,ACC_READ,&errsw) == 0)
	{
		print_config_file(buff);
	}

	buildfilepath(buff,wispconfigdir(),"wproc.msg");
	print_access("WPROCMSG", buff, ACC_READ, &errsw);

	buildfilepath(buff,wispconfigdir(),WRUNCONFIG);
	print_access("WRUNCONFIG", buff, ACC_READ, &errsw);

	/*
	**	Check on wrun
	*/
	WL_wrunconfig(&wrun_cfg);				/* Load wrunconfig options file		*/

	print_pair_nl("WRUN COBOL",   wrun_cfg.wrun_cobtype);
	print_pair_nl("WRUN RUNCBL",  wrun_cfg.wrun_runcbl);
	print_pair_nl("WRUN OPTIONS", wrun_cfg.wrun_options);

	if (0 == strcmp(WRUNCOBTYPE_ACU,wrun_cfg.wrun_cobtype))
	{
		nCobol = ACUCOBOL;
		print_pair_nl("COBOL", "ACUCOBOL");
	} 
	else if (0 == strcmp(WRUNCOBTYPE_MF,wrun_cfg.wrun_cobtype))
	{
		nCobol = MFCOBOL;
		print_pair_nl("COBOL", "MICRO FOCUS");
	}
	else
	{
		errsw++;
		nCobol = UNKNOWNCOBOL;
		print_pair_err_mess("COBOL",wrun_cfg.wrun_cobtype,"ERROR", "UNKNOWN COBOL TYPE");
	}

	checkexe("RTS", wrun_cfg.wrun_runcbl, &errsw);

	print_pair_nl("LINKPATH", wisplinkpath());
#ifdef WIN32
	check_path(wisplinkpath());
#endif

	/*
	**	Check for WISP utilitys
	*/
	print_nl();

	checklinkexe("WSHELL",  "wshell");
	checklinkexe("PROC",     WL_wprocexe());
	checklinkexe("COPY",    "wcopy");
	checklinkexe("SORT",    "wsort");
	checklinkexe("EDITOR",   WL_weditorexe());
	if (WL_custom_display_utility())
	{
		checklinkexe("DISPLAY", WL_custom_display_utility());
	}
	else
	{
		checklinkexe("DISPLAY", "display");
	}

	
#ifdef unix
	cptr = NULL;
	
	switch(WL_opt_printqueue)
	{
	case PQ_UNIQUE:
	case PQ_ILP:
		if ((cptr = getenv("UNIQUE_PRINT")))
		{
		}
		else if (PQ_ILP == WL_opt_printqueue)
		{
			cptr = "ilp";
		}
		else
		{
			cptr = "ulp";
		}
		break;

	case PQ_LP:
		cptr = "lp";
		break;
		
	case PQ_NP:
		cptr = "np";
		break;

	case PQ_GENERIC:
		cptr = WL_get_wisp_option("PQCMD");
		break;
		
	case PQ_DEFAULT:
		break;
	}

	if (cptr)
	{
		checklinkexe("PRINT",  cptr);
	}
	else
	{
		errsw++;
		print_pair_err_mess("PRINT","","ERROR", "NOT DEFINED");
	}

	if (WL_opt_printqueue_manager)
	{
		checklinkexe("PQMANAGER", WL_opt_printqueue_manager);
	}
	else
	{
		print_pair_nl("PQMANAGER", "(none)");
	}
	
	if (*(WL_batchqueue_name()) != '\0')
	{
		checklinkexe("BATCH",  WL_batchqueue_name());
	}
	else
	{
		print_pair_nl("BATCH", "(none)");
	}
	
	if (WL_opt_batchman)
	{
		checklinkexe("BATCHMAN",  WL_batchman_name);
	}
	else
	{
		print_pair_nl("BATCHMAN", "(none)");
	}
#endif	

	/*
	**	Check the ACUCOBOL environment
	*/
	if (ACUCOBOL == nCobol || MFCOBOL != nCobol)
	{
		define_acu(wrun_cfg.wrun_runcbl);
	}

#ifdef unix
	/*
	**	Check the Micro Focus environment
	*/
	if (MFCOBOL == nCobol || ACUCOBOL != nCobol)
	{
		define_mf();
	}
#endif

	print_nl();
	printf("MISC Environment\n");
	printf("================\n");
	printf("env:\n");
	for(;*envp;envp++)
	{
		print_inset_nl(*envp);
	}

	print_nl();
	
	

	print_nl();
	printf("================\n\n");

	if (errsw != 0)
	{
		printf("*** %d ENVIRONMENT SETUP ERRORS FOUND ***\n", errsw);
	}	

	if (erracu != 0)
	{
		printf("*** %d ACUCOBOL SETUP ERRORS FOUND ***\n", erracu);
	}
	if (nCobol == ACUCOBOL && 0 == erracu)
	{
		printf("--- ACUCOBOL SETUP OK ---\n");
	}

#ifdef unix
	if (errmf != 0)
	{
		printf("*** %d MICRO FOCUS SETUP ERRORS FOUND ***\n", errmf);
	}
	if (nCobol == MFCOBOL && 0 == errmf)
	{
		printf("--- MICRO FOCUS SETUP OK ---\n");
	}
#endif

	if (0==errsw && 0==erracu && 0==errmf)
	{
		printf("--- WISP ENVIRONMENT OK ---\n");
	}
	else
	{
		print_nl();
		printf("*** %d TOTAL ERRORS DETECTED ***\n", errsw+erracu+errmf);
	}
	
	print_nl();
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

int define_os(char* ut)
{

#ifdef unix
	struct utsname unix_name;
#endif
#ifdef WIN32
	const char* pVersion;
	char	buff[256];
#endif

	char	temp[256];
	
	char	*ptr;

	/*	int	errsw=0; */
#ifdef unix
	/*
	**       Check the basic unix shell environment
	*/
	print_nl();
	printf("UNIX Environment\n");
	printf("================\n");
	if (uname(&unix_name) != -1)
	{
		sprintf(temp, "%s %s %s %s %s", unix_name.sysname,
						unix_name.nodename,
						unix_name.release,
						unix_name.version,
						unix_name.machine);

#ifdef SCO
		/*
		**	Add extra SCO info
		*/
		{
			struct scoutsname sco_buff;

			if (-1 != __scoinfo(&sco_buff, sizeof(sco_buff)))
			{
				char os_release[sizeof(sco_buff.release)+1];
				char os_kernelid[sizeof(sco_buff.kernelid)+1];

				memcpy(os_release, sco_buff.release, sizeof(sco_buff.release));
				os_release[sizeof(sco_buff.release)] = '\0';
				strcat(temp," ");
				strcat(temp,os_release);

				memcpy(os_kernelid, sco_buff.kernelid, sizeof(sco_buff.kernelid));
				os_kernelid[sizeof(sco_buff.kernelid)] = '\0';
				strcat(temp," ");
				strcat(temp,os_kernelid);
			}

		}
#endif /* SCO */

	}
	else
	{
		strcpy(temp, "(SYSTEM TYPE NOT DEFINED)");
	}
	print_pair_nl("UNIX", temp);
#endif


#ifdef WIN32
	/*
	**       Check the basic DOS shell environment
	*/
	print_nl();
	printf("WIN32 Environment\n");
	printf("=================\n");
	if (WL_win32_nt())
	{
		pVersion = "NT";
	}
	else if (WL_win32_98())
	{
		pVersion = "98";
	}
	else if (WL_win32_95())
	{
		pVersion = "95";
	}
	else
	{
		pVersion = "Unknown";
	}
	sprintf(buff, "%s %s", pVersion, WL_win32_version());
	print_pair_nl("WINDOWS", buff);
#endif

	print_access("HOME", wisphomedir(NULL), ACC_FULL, &errsw);

	if ((ptr = getenv("PATH")))
	{
		print_pair_nl("PATH",ptr);
		check_path(ptr);
	}
	else
	{
		errsw++;
		print_pair_err_mess("PATH","","ERROR", "NOT DEFINED");
	}

#ifdef unix
	if ((ptr = getenv("SHELL")) )
	{
		print_pair("SHELL",ptr);
		if (ptr[0] == '/')
		{
			/*
			**	Only check access if a full path given
			*/
			check_access(&errsw,ptr,ACC_EXECUTE);

			if (0!=strcmp(ptr,"/bin/sh"))
			{
				bBourneShell = 0;
			}
		}
		else
		{
			print_err_mess("WARNING","ACCESS UNKNOWN");
		}
	}
	else
	{
		print_pair_nl("SHELL", "Assuming shell /bin/sh");
	}

	/*
	**       Check the terminal definition variables for unix and wisp
	*/
	print_nl();
	print_envvar("TERM",&errsw);

	print_nl();

	print_access("TEMP","/usr/tmp",ACC_FULL,&errsw);
	print_access("TEMP","/tmp",    ACC_FULL,&errsw);

	if ((ptr = getenv("TMPDIR")))
	{
		print_access("TMPDIR", ptr, ACC_FULL, &errsw);
	}
	else
	{
		print_pair_nl("TMPDIR", "(Not defined)");
	}
#endif

	print_pair_nl("MACHID", (0==WL_getmachineid(temp)) ? temp : "(unknown)");

	print_pair_nl("COMPNAME", WL_computername(NULL));

	print_pair_nl("SERVER", wispserver());

	print_pair_nl("ttyname", ttyname(0));

	WL_ttyid5(temp);
	print_pair_nl("ttyid5", temp);
	

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
	char	a_config[256] = "";
#ifdef unix
	char	command[256];
#endif

	/*
	**       Get the ACUCOBOL Cobol environment data
	*/

	printf("\n\n");
	printf("ACUCOBOL Environment\n");
	printf("====================\n");

	if ((ptr = strstr(wrun_cfg.wrun_options, "-c")) ||
	    (ptr = strstr(wrun_cfg.wrun_options, "-C"))	)
	{
		ptr += 2;
		sscanf(ptr,"%s",a_config);

		if (print_access("-C A_CONFIG",a_config,ACC_READ,&erracu) != 0)
		{
			a_config[0] = '\0';
		}	
	}
	else if ((acu = getenv("A_CONFIG")))
	{
		if (print_access("A_CONFIG",acu,ACC_READ,&erracu) == 0)
		{
			strcpy(a_config,acu);
		}
	}
	else
	{
		erracu++;
		print_pair_err_mess("A_CONFIG","","ERROR", "NOT DEFINED");
	}

	if ('\0' != a_config[0])
	{
		FILE 	*the_file;
		char	inlin[512], keyword[80];
		int	cnt,len;
		char	*pszCodePrefix = NULL;

		print_config_file(a_config);


		/*
		 * Get CODE-PREFIX and search for ACULINK
		 */

		the_file = fopen(a_config,"r");
		if (the_file)
		{
			while(fgets(inlin,sizeof(inlin)-1,the_file))
			{
				len=strlen(inlin);
				if (len>0 && inlin[len-1] == '\n') inlin[len-1] = '\0';	/* Null out the newline char	*/
				cnt = sscanf(inlin,"%s",keyword);
				if ( cnt < 1 ) continue;


				if (0==strcmp(keyword,"CODE-PREFIX"))
				{
					pszCodePrefix = inlin;
					break;
				}
			}
			fclose(the_file);

			if (NULL != pszCodePrefix)
			{
				char*	token = NULL;
				int	aculink_found = 0;
				int	acuusing_found = 0;
				char 	aculink_path[256];
				char 	acuusing_path[256];
					
				print_pair_nl("CODE-PREFIX",&pszCodePrefix[11]);
				for(token = strtok(&pszCodePrefix[11]," \t"); 
				    token!=NULL;
				    token = strtok(NULL," \t"))
				{
					print_inset(token);
					check_access(NULL,token,ACC_RX);

					if (!aculink_found)
					{
						buildfilepath(aculink_path,token,"ACULINK");
						if (0==access(aculink_path,ACC_READ))
						{
							aculink_found = 1;
						}
					}
					if (!aculink_found)
					{
						buildfilepath(aculink_path,token,"ACULINK.acu");
						if (0==access(aculink_path,ACC_READ))
						{
							aculink_found = 1;
						}
					}
					if (!acuusing_found)
					{
						buildfilepath(acuusing_path,token,"ACUUSING");
						if (0==access(acuusing_path,ACC_READ))
						{
							acuusing_found = 1;
						}
					}
					if (!acuusing_found)
					{
						buildfilepath(acuusing_path,token,"ACUUSING.acu");
						if (0==access(acuusing_path,ACC_READ))
						{
							acuusing_found = 1;
						}
					}
				}

				if (aculink_found)
				{
					print_access("ACULINK",aculink_path,ACC_READ,&erracu);
				}
				else
				{
					erracu++;
					print_pair_err_mess("ACULINK","","ERROR","NOT FOUND");
				}

				if (acuusing_found)
				{
					print_access("ACUUSING",acuusing_path,ACC_READ,&erracu);
				}
				else
				{
					erracu++;
					print_pair_err_mess("ACUUSING","","ERROR","NOT FOUND");
				}
			}
			else
			{
				erracu++;
				print_pair_err_mess("CODE-PREFIX","","ERROR","NOT FOUND");
				print_pair_nl("ACULINK","(Unknown)");
				print_pair_nl("ACUUSING","(Unknown)");
			}
		}
	}
	else
	{
		print_pair_nl("CODE-PREFIX","(Unknown)");
		print_pair_nl("ACULINK","(Unknown)");
		print_pair_nl("ACUUSING","(Unknown)");
	}
	

	
#ifdef unix
	if ((acu = getenv("A_TERMCAP")))
	{
		print_access("A_TERMCAP",acu,ACC_READ,&erracu);
	}
	else
	{
		print_access("a_termcap","/etc/a_termcap",ACC_READ,&erracu);
	}
#endif

	if (0 == checkexe("VUTIL", WL_acu_vutil_exe(), &erracu))
	{
#ifdef unix
		sprintf(command, "%s -V", WL_acu_vutil_exe());
		if (0 != run_command(command))
		{
			errmf++;
		}
#endif
	}


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

static int define_mf()
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

	if (print_access("COBDIR", mf=getenv("COBDIR"), ACC_READ,&errmf) == 0)
	{
		sprintf(file_path,"%s/etc/cobver",mf);		/* Server Express style */
		if (0!=access(file_path,ACC_READ))
		{
			sprintf(file_path,"%s/cobver",mf); 	/* MF 4.1 style */
		}
		cat_file(file_path);
	}

	print_envvar("COBSW",NULL);
	print_envvar("COBOPT",NULL);

	if ((mf = getenv("COBPATH")))
	{
		print_pair_nl("COBPATH",mf);
		check_path(mf);
	}
	else
	{
		print_pair_err_mess("COBPATH","","WARNING", "NOT DEFINED");
	}
	
	{
		char* shared_lib_path_var = "LD_LIBRARY_PATH";
		
#ifdef AIX
		shared_lib_path_var = "LIBPATH";
#endif
#ifdef HPUX
		shared_lib_path_var = "SHLIB_PATH";
#endif

		if ((mf = getenv(shared_lib_path_var)))
		{
			print_pair_nl(shared_lib_path_var,mf);
			check_path(mf);
		}
		else
		{
			print_pair_err_mess(shared_lib_path_var,"","WARNING", "NOT DEFINED");
		}
	}
	



	/*
	*	Micro Focus 4.1 - Use fhconvert. (has a rebuild that doesn't work)
	*	Server Express  - Use rebuild.   (fhconvert doesn't exist)
	*/
	ptr = "fhconvert";
	if (0 == WL_whichenvpath(ptr,file_path))
	{
		print_pair_mess(ptr,file_path,"[FOUND]");
	}
	else
	{
		ptr = "rebuild";
		if (0 == WL_whichenvpath(ptr,file_path))
		{
			print_pair_mess(ptr,file_path,"[FOUND]");
		}
		else
		{
			/* Didn't find either so issue one error and one warning */
			print_pair_err_mess("fhconvert","fhconvert","ERROR", "NOT FOUND");
			print_pair_err_mess("rebuild",  "rebuild",  "WARNING", "NOT FOUND");
			errmf++;
		}
	}


	ptr = "cob";

	if (0 == WL_whichenvpath(ptr,file_path))
	{
		print_pair_mess("cob",file_path,"[FOUND]");

		if (0 != run_command("cob -V"))
		{
			errmf++;
		}
	}
	else
	{
		print_pair_err_mess("cob",ptr,"WARNING", "NOT FOUND");
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

int check_access(int* pnError, const char *ptr, int mode)
{
	int     rc;
	int	nErrorLocal;
	char* 	pszSeverity;
  
	if (NULL == pnError)
	{
		pszSeverity = "WARNING";
		pnError = &nErrorLocal;	/* Fake it */
	}
	else
	{
		pszSeverity = "ERROR";
	}

	if (NULL==ptr || '\0' == *ptr)
	{
		print_err_mess(pszSeverity, "NOT DEFINED");
		(*pnError)++;
		return 1;
	}
  

	if ((rc = access(ptr, mode)))
	{
		switch(errno)
		{
		case ENOENT:    
			print_err_mess(pszSeverity, "NOT FOUND");
			break;
		case EACCES:    
			print_err_mess(pszSeverity, "ACCESS DENIED");
			break;
		case ENOTDIR:   
			print_err_mess(pszSeverity, "INVALID DIRECTORY");
			break;
		default:        
			print_err_mess(pszSeverity, "UNABLE TO ACCESS");
			break;
		}
	}
	else
	{
		print_mess_nl("[FOUND]");
	}

	if (rc)
	{
		(*pnError)++;
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

	print_nl();
	printf("$ %s\n",command);
#ifdef unix
	sprintf(unixcommand, "%s 2>&1", command);
	
	if ((file = popen(unixcommand,"r")) != NULL)
	{
		while (fgets(buff, sizeof(buff), file) != NULL)
		{
			char	*ptr;

			if ((ptr=strchr(buff,'\n')))
			{
				*ptr = '\0';
			}
			
			print_inset_nl(buff);
		}
		pclose(file);
		print_nl();
	}
	else
	{
		return 1;
	}
#endif
#if defined(WIN32)
	system (command);
#endif

	return 0;
}

int checkexe(const char *tag, const char *exe, int* pnError)
{
	char	buff[256];
	char	file_path[256];
	
	strcpy(buff, exe);
	
#ifdef WIN32
	WL_upper_string(buff);
	if (!WL_osd_ext(buff))
	{
		strcat(buff,".EXE");
	}
#endif
	if (0 == WL_whichenvpath(buff,file_path))
	{
		print_pair_mess(tag,file_path,"[FOUND]");
		return 0;
	}
	else
	{
		return print_access(tag,buff,ACC_RX,pnError);
	}
}

void checklinkexe(const char *tag, const char *exe)
{
	char	buff[256];
	char	file_path[256];
	char	*ptr;
	
	strcpy(buff, exe);

	/*
	**	The exe sometimes includes arguments so terminate at the first space.
	*/
	if ((ptr = strchr(buff,' ')))
	{
		*ptr = '\0';
	}
	
	
#ifdef WIN32
	WL_upper_string(buff);
	if (!WL_osd_ext(buff))
	{
		strcat(buff,".EXE");
	}
#endif
	if (0 == WL_whichlinkpath(buff,file_path))
	{
		print_pair_mess(tag, file_path,"[FOUND]");
		return;
	}
#ifdef WIN32
	if (0 == WL_whichenvpath(buff,file_path))
	{
		print_pair_mess(tag, file_path,"[FOUND]");
		return;
	}
#endif

	print_access(tag,buff,ACC_RX,&errsw);
}

static void cat_file(const char* file_path)
{
	FILE 	*the_file;
	char	inlin[512];

	the_file = fopen(file_path,"r");
	if (the_file)
	{
		printf("%s:\n",file_path);
		while(fgets(inlin,sizeof(inlin)-1,the_file))
		{
			char *ptr;
			
			if ((ptr=strchr(inlin,'\n')))
			{
				*ptr = '\0';
			}
			
			print_inset_nl(inlin);
		}
		fclose(the_file);
	}
	else
	{
		printf(">>>>>> Unable to open file [%s]\n",file_path);
	}

}

void print_config_file(const char* options_path)
{
	FILE 	*the_file;
	char	inlin[2048];
	int	len;
	int	bContinued = 0;
	int	bComment = 0;

	the_file = fopen(options_path,"r");
	if (the_file)
	{
		while(fgets(inlin,sizeof(inlin)-1,the_file))
		{
			len=strlen(inlin);
			if (len>0 && inlin[len-1] == '\n') 
			{
				inlin[--len] = '\0';	/* Null out the newline char	*/
			}

			/*
			 *	If not continued (a new line) then set the comment flag
			 */
			if (!bContinued)
			{
				char	keyword[256];
				int	cnt;
				
				/*
				 *	Start of a new line so check if a comment or blank line.
				 */
				bComment = 0;
				cnt = sscanf(inlin,"%s",keyword);
				
				if (cnt < 1 || '#'==keyword[0]) 
				{
					bComment = 1;
				}
			}

			/*
			 *	If not a comment print it
			 */
			if (!bComment)
			{
				print_inset_nl(inlin);
			}
			
			/*
			 *	Check if this line is continued.
			 */
			bContinued = 0;
			if (len>0 && '\\'==inlin[len-1])
			{
				bContinued = 1;
			}
		}
		fclose(the_file);
	}
	else
	{
		printf(">>>>>> Unable to open file [%s]\n",options_path);
	}

}

static void check_path(const char* path)
{
	char	lpath[1024];
	char	*nextdir;
	char	ps[2];
	
	if (NULL==path || '\0'==*path)
	{
		return;
	}
	strcpy(lpath,path);

	ps[0] = PATH_SEPARATOR;
	ps[1] = '\0';
	
	nextdir = strtok(lpath,ps);
	while(nextdir)
	{
		print_inset(nextdir);
		check_access(NULL,nextdir,ACC_RX);
		nextdir = strtok(NULL,ps);
	}
}

static void test_int_sizes(void)
{
	int2	t_int2;
	uint2	t_uint2;
	int4	t_int4;
	uint4	t_uint4;
#ifdef INT8_DEFINED
	INT8	t_int8;
	UINT8	t_uint8;
#endif
	int	rc = 0;

	/*
	**	Check the sizes.
	*/
	if (sizeof(t_int2) != 2)
	{
		printf("********************************* Size error int2 = %lu\n", (unsigned long)sizeof(t_int2));
		rc = 1;
	}
	if (sizeof(t_uint2) != 2)
	{
		printf("********************************* Size error uint2 = %lu\n", (unsigned long)sizeof(t_uint2));
		rc = 1;
	}
	if (sizeof(t_int4) != 4)
	{
		printf("********************************* Size error int4 = %lu\n", (unsigned long)sizeof(t_int4));
		rc = 1;
	}
	if (sizeof(t_uint4) != 4)
	{
		printf("********************************* Size error uint4 = %lu\n", (unsigned long)sizeof(t_uint4));
		rc = 1;
	}
#ifdef INT8_DEFINED
	if (sizeof(t_int8) != 8)
	{
		printf("********************************* Size error INT8 = %lu\n", (unsigned long)sizeof(t_int8));
		rc = 1;
	}
	if (sizeof(t_uint8) != 8)
	{
		printf("********************************* Size error UINT8 = %lu\n", (unsigned long)sizeof(t_uint8));
		rc = 1;
	}
#endif

	/*
	**	Check the signs
	*/

	t_int2 = 0;
	t_uint2 = 0;
	t_int4 = 0;
	t_uint4 = 0;
	
	t_int2--;
	t_uint2--;
	t_int4--;
	t_uint4--;

	if ( !(t_int2 < 0 && -1 == t_int2) )
	{
		printf("********************************* Sign error on int2\n");
		rc = 1;
	}
	if ( !(t_int4 < 0 && -1 == t_int4) )
	{
		printf("********************************* Sign error on int4\n");
		rc = 1;
	}
	if ( !(t_uint2 > 0 && (unsigned)(65535u) == t_uint2) )
	{
		printf("********************************* Sign error on uint2\n");
		rc = 1;
	}
	if ( !(t_uint4 > 0 && (unsigned)(4294967295u) == t_uint4) )
	{
		printf("********************************* Sign error on uint4\n");
		rc = 1;
	}

#ifdef INT8_DEFINED
	t_int8 = 0;
	t_uint8 = 0;
	t_int8--;
	t_uint8--;
	if ( !(t_int8 < 0 && -1 == t_int8) )
	{
		printf("********************************* Sign error on int8\n");
		rc = 1;
	}
	if ( !(t_uint8 > 0) )
	{
		printf("********************************* Sign error on uint8\n");
		rc = 1;
	}
#endif

	if (rc)
	{
		printf("\n\n\n\n*****    Integer sign errors    ****\n\n\n");
	}
}


/*
**	History:
**	$Log: wdiag.c,v $
**	Revision 1.55  2003/07/29 13:41:25  gsl
**	extra SCO info
**	
**	Revision 1.54  2003/05/19 19:08:26  gsl
**	-Wall
**	
**	Revision 1.53  2003/05/06 18:27:25  gsl
**	-Wall
**	
**	Revision 1.52  2003/03/20 18:29:05  gsl
**	Fix logical_id typedef
**	
**	Revision 1.51  2003/02/14 16:28:43  gsl
**	Check $WISPCONFIG is set
**	
**	Revision 1.50  2003/02/13 20:51:37  gsl
**	fix warning
**	
**	Revision 1.49  2003/02/07 20:45:14  gsl
**	Add platform to version display
**	
**	Revision 1.48  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.47  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.46  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.45  2002/12/11 14:08:44  gsl
**	Removed wispmsg.dat/txt and makemsg
**	
**	Revision 1.44  2002/12/02 19:27:58  gsl
**	Fix bug, not recognizing MF as a valid Cobol type
**	
**	Revision 1.43  2002/11/21 22:21:52  gsl
**	Change to use ACULINK.acu and ACUUSING.acu
**	Added .acu to all Acucobol object files
**	
**	Revision 1.42  2002/11/08 18:19:49  gsl
**	Enlarge temp vars to prevent overflow
**	
**	Revision 1.41  2002/10/16 20:34:50  gsl
**	configure with environments variables vs registry on win32
**	
**	Revision 1.40  2002/10/11 20:39:50  gsl
**	Detect runtime Cobol type without needing INITWISP call.
**	For ACU set in sub85.c,
**	For utils set via WRUNCONFIG
**	Default to MF on UNIX
**	
**	Revision 1.39  2002/10/08 15:44:38  gsl
**	Change int8 to INT8 to avoid conficts
**	
**	Revision 1.38  2002/10/04 20:55:15  gsl
**	Add int8  tests
**	
**	Revision 1.37  2002/07/25 17:03:42  gsl
**	MSFS->WIN32
**	
**	Revision 1.36  2002/07/18 21:04:23  gsl
**	Remove MSDOS code
**	
**	Revision 1.35  2002/07/12 20:50:49  gsl
**	fix WL_ change
**	
**	Revision 1.34  2002/07/12 20:40:45  gsl
**	Global unique WL_ changes
**	
**	Revision 1.33  2002/07/11 14:33:56  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.32  2002/07/10 21:06:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.31  2002/07/09 04:13:50  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.30  2002/06/25 18:18:35  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.29  2002/03/27 16:20:24  gsl
**	If found run VUTIL -V to get Acucobol Version number
**	Missing cob is only a warning
**	
**	Revision 1.28  2002-03-27 10:10:47-05  gsl
**	FIxed MF shared library path
**
**	Revision 1.27  2001-11-12 14:55:26-05  gsl
**	Remove check for cblhelp
**
**	Revision 1.26  2001-11-07 14:56:32-05  gsl
**	fix unsigned warning
**
**	Revision 1.25  2001-10-26 14:14:42-04  gsl
**	Add user id.
**	Fix COBVER for Server Express
**	If fhconvert not found check rebuild
**
**	Revision 1.24  1999-09-15 09:17:17-04  gsl
**	Update to understand backslash continued long lines.
**	Fix A_CONFIG logic to understand both "-c" and "-C" on the command line.
**
**	Revision 1.23  1999-06-30 10:17:02-04  gsl
**	Enhance the ACULINK ligic
**
**	Revision 1.22  1999-06-29 19:23:45-04  gsl
**	Add RVMAP, CODE-PREFIX, and ACULINK tests
**
**	Revision 1.21  1999-06-18 09:00:10-04  gsl
**	Massive re-work.
**	Changed to clearly distingush between errors and warnings.
**	Add an error count.
**	Align the error messages so they are clearly visable.
**
**	Revision 1.20  1998-12-15 17:15:25-05  gsl
**	Add tests for integer sizes
**
**	Revision 1.19  1998-12-04 13:07:57-05  gsl
**	Add windows 98 test and win32 version
**
**	Revision 1.18  1998-10-26 09:17:16-05  gsl
**	Add a timestamp
**
**	Revision 1.17  1998-10-23 17:25:56-04  gsl
**	Enhance the PRINT and BATCH queue checking
**
**	Revision 1.16  1998-10-08 13:19:49-04  gsl
**	Add printing of the license file
**
**	Revision 1.15  1998-08-21 10:02:53-04  gsl
**	Enhanced wdiag to:
**	print and check LGMAP directories
**	on unix check for ulp and usubmit
**	print the environment
**	for acucobol check for A_CONFIG pass as a -c option in wrun
**	and print the A_CONFIG file
**	for MF print cobver file
**	Check and print PATH
**	Check anf print LIBPATH
**	check and print COBPATH
**
**	Revision 1.14  1998-01-09 08:48:12-05  gsl
**	Remove non-error conditions from microfocus
**
**	Revision 1.13  1997-12-04 18:15:45-05  gsl
**	Added support of Windows NT/95
**
**	Revision 1.12  1996-12-12 13:13:56-05  gsl
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
