


						/************************************************************************/
						/*                                                                      */
						/*              WISP - Wang Interchange Source Pre-processor            */
						/*               Copyright (c) 1988, 1989, 1990, 1991, 1992             */
						/*       An unpublished work of International Digital Scientific Inc.   */
						/*                          All rights reserved.                        */
						/*                              					*/
						/************************************************************************/

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
**			run_wrun()		Determines if wrun exists
**			define_acu()		Gets ACUCOBOL information
**			define_mf()		Gets Micro Focus information
**			
**
**
**      History:
**                      04/01/92        Written by GSL
**                      06/30/92        Updated by JAK
**                      07/21/92        Updated by JAK
**			06/14/93	Updated by JAK
**
*/


#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#ifdef unix
#include <sys/utsname.h>
#endif

#ifdef MSDOS
#include <process.h>
#include <conio.h>
#include <dos.h>
#endif

#ifndef false
#define false 0;
#define true !false;
#endif

/*
**	Define  called routines
*/
int     check_access();
int     define_os();
int	define_videocap();
int	run_wrun();
char	buildfilepath();


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
**      History:        04/01/92        Written by GSL
**                      06/30/92        Updated by JAK
**                      07/21/92        Updated by JAK
**                      07/31/92        Updated by JAK
**			06/14/93	Updated by JAK
**
*/

main()
{


	FILE    *file;

	char    *strchr(),
		*openb,
		*closeb;

	char    *tmp,
		temp[80];

	char    buff[256];

	char    *wc,
		 vcd[256],
		*ut,
		*wt;


	char    *cb,
		cobd[256];

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
	printf("     WISP Environment\n");
	vcd[0] = '\0';
	if (wc = getenv("WISPCONFIG"))
	{
		printf("WISPCONFIG  = %s",wc);
		if (check_access(wc,05) == 0)
		{
			buildfilepath(vcd,wc,"videocap");
		}
		else
		{
			errsw = true;
		}
	}
	else
	{
		errsw = true;
		printf("WISPCONFIG  = *** NOT DEFINED ***\n\n");
	}


	if (wt = getenv("WISPTERM"))
	{
		if (wt[0] == '\0')
		{
			errsw = true;
			printf("WISPTERM    = (NULL) *** ERROR\n",wt);
		}
		else
		{
			printf("WISPTERM    = %s\n",wt);
		}
	}
	else
	{
		errsw = true;
		wt = ut;
		printf("WISPTERM    = *** NOT DEFINED\n");
	}

	if (tmp = getenv("WISPGID"))
	{
		if (tmp[0] == '\0')
		{
			errsw = true;
			printf("WISPGID     = (NULL) *** ERROR\n");
		}
		else
		{
			printf("WISPGID     = %s\n",tmp);
		}
	}	
	else	
	{
		printf("WISPGID     = *** NOT DEFINED\n");
		errsw = true;
	}


	/*
	**	Check for the videocap files
	*/
	define_videocap(vcd,wt);

	/*
	**       Check the wisp runtime control files and data files
	**       Use WISPCONFIG to find these file, wc points to WISPCONFIG
	*/
	printf("\n");
	if (wc)
	{
		buildfilepath(buff,wc,"LGMAP");
		printf("VOLUMES     = %s",buff);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}

#ifdef unix
		buildfilepath(buff,wc,"wsysconfig");
#endif
#ifdef MSDOS
		buildfilepath(buff,wc,"wsysconf.cfg");
#endif
		printf("SYSTEM      = %s",buff);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}

		buildfilepath(buff,wc,"wispmsg.dat");
		printf("MESSAGES    = %s",buff);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}

		buildfilepath(buff,wc,"OPTIONS");
		printf("OPTIONS     = %s",buff);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}

#ifdef unix
		buildfilepath(buff,wc,"wrunconfig");
#endif
#ifdef MSDOS
		buildfilepath(buff,wc,"wrun.cfg");
#endif
		printf("RUNTIME     = %s",buff);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}
		else
		{
			errsw = run_wrun();
		}
	}
	else
	{
		errsw = true;
		printf("VOLUMES     = *** NOT DEFINED\n");
		printf("RUNTIME     = *** NOT DEFINED\n");
		printf("SYSTEM      = *** NOT DEFINED\n");
		printf("MESSAGES    = *** NOT DEFINED\n");
		printf("*** RUN TIME VARIABLES NOT DEFINED\n");
	 }



	/*
	**	Check the ACUCOBOL environment
	*/
	define_acu();



	/*
	**	Check the Micro Focus environment
	*/
	define_mf();


	printf("\n");
	if ((erracu != 0) && (comp == acucobol))
	{
		printf("\n");
		printf(">>>>>>> ACUCOBOL SETUP ERRORS FOUND <<<<<<<\n\n");
		errsw = true;
	}

	if ((errmf != 0) && (comp == mfcobol))
	{
		printf("\n");
		printf(">>>>>>> MICRO FOCUS SETUP ERRORS FOUND <<<<<<<\n\n");
		errsw = true;
	}

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
#endif

	char    temp[80],
		*ptr,
		buff[256];

	int	errsw=false;

	printf("*************************************************************************\n");
	printf("*                                  WISP                         	*\n");
	printf("*                       System Environment Diagnostic           	*\n");
	printf("*                  International Digital Scientific Inc.        	*\n");
	printf("*                           All rights reserved.                	*\n");
	printf("*                               Version 2.0                             *\n");
	printf("*************************************************************************\n");

#ifdef unix
	/*
	**       Check the basic unix shell environment
	*/
	printf("\n");
	printf("     UNIX Environment\n");
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

	if (ptr = getenv("HOME"))
	{
		printf("HOME        = %s",ptr);
		if (check_access(ptr,07) != 0)
		{
			errsw = true;
		}
	}
	else
	{
		printf("HOME        = *** NOT DEFINED\n");
	}

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
	printf("WORK        = /usr/tmp");
	if (check_access("/usr/tmp",07) != 0)
	{
		   errsw = true;
	}


	printf("WORK        = /tmp");
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

	if (ptr = getenv("HOME"))
	{
		printf("HOME        = %s",ptr);
		if (check_access(ptr,07) != 0)
		{
			errsw = true;
		}
	}
	else
	{
		printf("HOME        = *** NOT DEFINED\n");
	}

	/*
	**       Check for the temporary work file directories
	*/

	if (ptr = getenv("TMP"))
	{
		printf("\n");
		printf("WORK        = %s\n",ptr);
		if (check_access(ptr,07) != 0)
		{
			errsw = true;
		}
	}
#endif

	return (0);
}


/*
**      Routine:        define_videocap()
**
**      Function:       To check video characteristics.
**
**      Description:    Videocap run time variables check.
**
**      Input:
**	vcd		The videocap directory path.  This will be set
**			to $WISPCONFIG/videocap on entry but will be changed
**			to $VIDEOCAP if it is defined.
**	wt		The $WISPTERM (or $TERM if not defined)
**	
**
**      Output:		None
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        
**	06/15/93        Written by JAK
**	10/18/93	Fixed bug when $VIDEOCAP defined and found. GSL
**
*/

int define_videocap(vcd,wt)

char    vcd[256],
	*wt;

{
	char	*vc;
	char	 vcf[256];

	if (vc = getenv("VIDEOCAP"))
	{
		strcpy(vcd,vc);		/* $VIDEOCAP overrides $WISPCONFIG/videocap */

		printf("VIDEOCAP    = %s",vc);
		if (check_access(vc,05) == 0)
		{
		}
#ifdef unix
		else
		{
			errsw = true;
		}
#endif
	}
	if (vcd[0])
	{
		printf("videocap directory = %s",vcd);
		if (check_access(vcd,05) != 0)
		{
#ifdef unix
			errsw = true;
#endif
		}
	}
#ifdef unix
	else
	{
		errsw = true;
		printf("VIDEOCAP    = *** NO VIDEOCAP DIRECTORY DEFINED ***\n\n");
	}
#endif

#ifdef unix
	vcf[0] = '\0';
	if ((wt != NULL) && wt[0] && vcd[0])
	{
		buildfilepath(vcf,vcd,wt);
		printf("videocap file = %s",vcf);
		if (check_access(vcf,04) != 0)
		{
			errsw = true;
			printf("*** NO VIDEOCAP FILE FOUND ***\n\n");
		}
	}
#endif
	return(0);
}



/*
**      Routine:        run_wrun()
**
**      Function:       To check WISP characteristics.
**
**      Description:    To check for run time variables
**
**      Input:		None
**
**      Output:		None
**
**      Return:         errsw value of true or false
**
**      Warnings:       None
**
**      History:        06/15/93        Written by JAK
**
*/

int run_wrun()
{


	FILE    *file;

	char    *acu,
		cobd[256];

	char    *strchr(),
		*openb,
		*closeb;

	printf("\n");

#ifdef unix
	if ((file = popen("wrun\0","r")) != NULL)
	{
		while (fgets(cobd, sizeof(cobd), file) != NULL)
			printf("%s ", cobd);
		pclose(file);
	}
#endif

#ifdef MSDOS
	if ((spawnlp (P_WAIT,"wrun",NULL,NULL)) == -1)
	{
		printf("          = WISP bin directory not found\n");
		printf("COBOL     = *** NOT DEFINED\n");
		errsw = true;
		return (0);
	}
	else
	{
		system ("wrun > wdiag.tmp");
		if ((file = fopen("wdiag.tmp","r")) != NULL)
		{
			while (fgets(cobd, sizeof(cobd), file) != NULL)
			{
			};
			fclose(file);
			unlink("wdiag.tmp");
		}
		else
		{
			printf("COBOL     = *** NOT DEFINED\n");
			errsw = true;
			return (0);
		}

	}
#endif

	if (openb = strchr(cobd,'['))
	{
		if (memcmp(openb+1, "MF", 2) == 0)
		{
			comp = mfcobol;
		}
		else
		{
			if (memcmp(openb+1, "ACU", 3) == 0)
			{
				comp = acucobol;
			}
			else
			{
				errsw = true;
				printf("COBOL     = *** NOT DEFINED\n");
			}
		}
	}
	else
	{
		errsw = true;
		printf("COBOL     = *** NOT DEFINED\n");
	}
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

int define_acu()
{

	FILE    *file;

	char    *acu,
		cobd[256];

	char    ccbl_type[40],
		cob_type[40];

	int	acu_present;


	 /*
	 **       Get the ACUCOBOL Cobol environment data
	 */

	 acu_present = false;
	 printf("\n\n");
	 printf("     ACUCOBOL Environment\n");

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
		 erracu = true;
		 printf("A_CONFIG    = *** NOT DEFINED\n");
	 }

#ifdef OLD
This is neither an AcuCOBOL or WISP required variable.
	 if (acu = getenv("ACU"))
	 {
		printf("ACU         = %s",acu);
		acu_present = check_access(acu,04);
	 }
	 else
	 {
		erracu = true;
		printf("ACU         = *** NOT DEFINED\n");
	 }
#endif


#ifdef unix
	 printf("a_termcap   = /etc/a_termcap");
	 if (check_access("/etc/a_termcap",04) != 0)
	 {
		 if (acu = getenv("A_TERMCAP"))
		 {
			  printf("A_TERMCAP   = %s",acu);
			  check_access(acu,04);
		 }
		 else
		 {
			  printf("A_TERMCAP   = *** NOT DEFINED\n");
		 }
	 }
	 printf("cblhelp     = /etc/cblhelp");
	 check_access("/etc/cblhelp",04);

	 sprintf(ccbl_type,"/usr/bin/ccbl");
	 printf("ccbl        = %s",ccbl_type);
	 if (check_access(ccbl_type,04) == 0)
	 {
		  printf("\n");
		  if ((file = popen("ccbl -v\0","r")) != NULL)
		  {
			   while (fgets(cobd, sizeof(cobd), file) != NULL)
					 printf("%s ", cobd);
			   pclose(file);
		  }
		  else
		  {
			erracu = true;
		  }
	 }
	 else
	 {
		sprintf(ccbl_type,"/usr/local/bin/ccbl");
		printf("ccbl        = %s",ccbl_type);
		if (check_access(ccbl_type,04) == 0)
		{
			printf("\n");
			if ((file = popen("ccbl -v\0","r")) != NULL)
			{
				while (fgets(cobd, sizeof(cobd), file) != NULL)
					printf("%s ", cobd);
				pclose(file);
			}
			else
			{
				erracu = true;
			}
		}
	}
#endif

#ifdef MSDOS
	 printf("cblhelp     = C:\\etc\\cblhelp");
	 check_access("C:\\etc\\cblhelp",04);

	 if (acu_present == 0)
	 {
		buildfilepath(ccbl_type,acu,"ccbl386.exe");
		printf("ccbl        = %s",ccbl_type);
		if (check_access(ccbl_type,04) == 0)
		{
			printf("\n");
			sprintf(cob_type, "%s -v",ccbl_type);
			system (cob_type);
		}
		else
		{
			erracu = true;
		}
	 }
	 else
	 {
		sprintf(ccbl_type,"C:\\acucobol\\ccbl386.exe");
		printf("ccbl        = %s",ccbl_type);
		if (check_access(ccbl_type,04) == 0)
		{
			system ("c:\\acucobol\\ccbl386.exe -v");
		}
		else
		{
			erracu = true;
		}
	 }
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

	FILE    *file;

	char    *mf,
		cobd[256];

	char    ccbl_type[40],
		cob_type[40];

	 /*
	 **       Get the Micro Focus Cobol environment data
	 */
	 printf("\n\n");
	 printf("     Micro Focus Environment\n");
	 if (mf = getenv("COBSW"))
	 {
		  printf("COBSW       = %s\n",mf);
	 }
	 else
	 {
		  errmf = true;
		  printf("COBSW       = *** NOT DEFINED\n");
	 }
	 if (mf = getenv("COBOPT"))
	 {
		  printf("COBOPT      = %s\n",mf);
	 }
	 else
	 {
		  errmf = true;
		  printf("COBOPT      = *** NOT DEFINED\n");
	 }
	 if (mf = getenv("COBPATH"))
	 {
		  printf("COBPATH     = %s",mf);
		  if (check_access(mf,04) != 0)
		  {
			   errmf = true;
		  }
	 }
	 else
	 {
		  errmf = true;
		  printf("COBPATH     = *** NOT DEFINED\n");
	 }
#ifdef unix
	 sprintf(cob_type,"/usr/bin/cob");
	 printf("cob         = %s",cob_type);
	 if (check_access(cob_type,04) != 0)
	 {
		  errmf = true;
	 }

	 if (mf = getenv("COBDIR"))
	 {
		printf("COBDIR      = %s",mf);
		if (check_access(mf, 04) == 0)
		{
			printf("\n");
			if ((file = popen("more $COBDIR/cobver\0","r")) != NULL)
			{
				while (fgets(cobd, sizeof(cobd), file) != NULL)
					 printf("%s ", cobd);
				pclose(file);
			}
		}
		else
		{
			errmf = true;
		}
	 }
	 else
	 {
		  errmf = true;
		  printf("COBDIR      = *** NOT DEFINED\n");
	 }
#endif

#ifdef MSDOS
	 sprintf(cob_type,"C:\\microfocus\\cob");
	 printf("cob         = %s",cob_type);
	 if (check_access(cob_type,04) != 0)
	 {
		  errmf = true;
	 }

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
#endif

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

int check_access(ptr,mode)
char    *ptr;
int     mode;
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
				case ENOTDIR:   printf("   *** INVALID DIRECTORY ***\n");
						break;
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
