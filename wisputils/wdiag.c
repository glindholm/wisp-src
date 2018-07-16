

						/************************************************************************/
						/*                                                                      */
						/*              WISP - Wang Interchange Source Pre-processor            */
						/*               Copyright (c) 1988, 1989, 1990, 1991, 1992             */
						/*       An unpublished work of International Digital Scientific Inc.   */
						/*                          All rights reserved.                        */
						/*                                                                      */
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
**
**
**      Routines:       check_access()          Checks the access permissions for files
**
**
**      History:
**                      04/01/92        Written by GSL
**                      06/30/92        Updated by JAK
**                      07/21/92        Updated by JAK
**
*/

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/utsname.h>


char    *getenv();

#ifndef false
#define false 0;
#define true !false;
#endif

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
**
**      Output:         None
**
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        04/01/92        Written by GSL
**                      06/30/92        Updated by JAK
**                      07/21/92        Updated by JAK
**                      07/31/92        Updated by JAK
**
*/

main()
{
#define mfcobol 98
#define acucobol 99

	FILE    *file;

	struct utsname unix_name;

	char    *strchr(),
		*openb,
		*closeb;
	char    temp[80];
	char    *ptr,
		 buff[256];
	char    *wc,
		*vc,
		 vcd[256],
		*ut,
		*wt,
		 vcf[256];
	char    *acu,
		*mf,
		 cobd[256];

	int     comp,
		errsw,
		erracu,
		errmf;

	erracu = false;
	errmf  = false;
	errsw  = false;
	comp   = false;

	printf("*************************************************************************\n");
	printf("*                                  WISP                                 *\n");
	printf("*                       System Environment Diagnostic                   *\n");
	printf("*                  International Digital Scientific Inc.                *\n");
	printf("*                           All rights reserved.                        *\n");
	printf("*                               Version 1.0                             *\n");
	printf("*************************************************************************\n");


	/*
	**       Check the basic UNIX shell environment
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
		if (check_access(ptr,01) != 0)
		{
			errsw = true;
		}
	}
	else
	{
		printf("SHELL       = Using default /bin/sh\n");
	}



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
			strcpy(vcd,wc);
			strcat(vcd,"/videocap");
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

	if (vc = getenv("VIDEOCAP"))
	{
		printf("VIDEOCAP   = %s",vc);
		if (check_access(vc,05) == 0)
		{
			strcpy(vcd,vc);
		}
		else
		{
			errsw = true;
		}
	}
	if (vcd[0])
	{
		printf("videocap directory = %s",vcd);
		if (check_access(vcd,05) != 0)
		{
			errsw = true;
		}
	}
	else
	{
		errsw = true;
		printf("VIDEOCAP   = *** NO VIDEOCAP DIRECTORY DEFINED ***\n\n");
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
			ut = NULL;
			printf("TERM       = *** NOT DEFINED\n");
		}
		else
		{
		printf("TERM       = %s\n",ut);
		}
	}
	else
	{
		errsw = true;
		printf("TERM       = *** NOT DEFINED\n");
	}

	if (wt = getenv("WISPTERM"))
	{
		if (wt[0] == '\0')
		{
			errsw = true;
			wt = ut;
		}
		printf("WISPTERM   = %s\n",wt);
	}
	else
	{
		errsw = true;
		wt = ut;
		printf("WISPTERM   = *** NOT DEFINED\n");
	}


	vcf[0] = '\0';
	if ((wt != NULL) && wt[0] && vcd[0])
	{
		sprintf(vcf,"%s/%s",vcd,wt);
		printf("videocap file = %s",vcf);
		if (check_access(vcf,04) != 0)
		{
			errsw = true;
			printf("*** Using internal vt220 videocap definition ***\n\n");
		}
	}





	/*
	**       Check the wisp runtime control files and data files
	**       Use WISPCONFIG to find these file, wc points to WISPCONFIG
	*/
	printf("\n");
	if (wc)
	{
		printf("VOLUMES    = %s/LGMAP",wc);
		sprintf(buff,"%s/LGMAP",wc);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}

		printf("SYSTEM     = %s/wsysconfig",wc);
		sprintf(buff,"%s/wsysconfig",wc);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}

		printf("MESSAGES   = %s/wispmsg.dat",wc);
		sprintf(buff,"%s/wispmsg.dat",wc);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}
		printf("RUNTIME    = %s/wrunconfig",wc);
		sprintf(buff,"%s/wrunconfig",wc);
		if (check_access(buff,04) != 0)
		{
			errsw = true;
		}
		else
		{
			printf("\n");
			if ((file = popen("wrun\0","r")) != NULL)
			{
				while (fgets(cobd, sizeof(cobd), file) != NULL)
					printf("%s ", cobd);
				pclose(file);
			}
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
		}
	}
	else
	{
		errsw = true;
		printf("VOLUMES    = *** NOT DEFINED\n");
		printf("RUNTIME    = *** NOT DEFINED\n");
		printf("SYSTEM     = *** NOT DEFINED\n");
		printf("MESSAGES   = *** NOT DEFINED\n");
		printf("*** RUN TIME VARIABLES NOT DEFINED\n");
	}


	/*
	**       Check for the temporary work file directories
	*/
	printf("\nWORK       = /usr/tmp");
	if (check_access("/usr/tmp",07) != 0)
	{
		   errsw = true;
	} 

	printf("WORK       = /tmp");
	if (check_access("/tmp",07) != 0)
	{
		   errsw = true;
	} 



	 /*
	 **       Get the ACUCOBOL Cobol environment data
	 */
	 printf("\n\n");
	 printf("     ACUCOBOL Environment\n");
	 if (acu = getenv("ACU"))
	 {
		printf("ACU        = %s",acu);
		check_access(acu,04);
	 }
	 else
	 {
		erracu = true;
		printf("ACU        = *** NOT DEFINED\n");
	 }

	 if (acu = getenv("A_CONFIG"))
	 {
		printf("A_CONFIG   = %s",acu);
		if (check_access(acu,04) != 0)
		{
			erracu = true;
		} 
	 }
	 else
	 {
		 erracu = true;
		 printf("A_CONFIG   = *** NOT DEFINED\n");
	 }

	 printf("a_termcap  = /etc/a_termcap");
	 if (check_access("/etc/a_termcap",04) != 0)
	 {
		 if (acu = getenv("A_TERMCAP"))
		 {
			  printf("A_TERMCAP  = %s",acu);
			  check_access(acu,04);
		 }
		 else
		 {
			  printf("A_TERMCAP  = *** NOT DEFINED\n");
		 }
	 }
	 printf("cblhelp    = /etc/cblhelp");
	 check_access("/etc/cblhelp",04);
	 printf("ccbl       = /usr/bin/ccbl"); 
	 if (check_access("/usr/bin/ccbl",04) == 0)
	 {
		  printf("\n");   
		  if ((file = popen("ccbl -v\0","r")) != NULL)
		  {
			   while (fgets(cobd, sizeof(cobd), file) != NULL)
					 printf("%s ", cobd);
			   pclose(file);
		  }
	 }
	 else
	 {
		  erracu = true;
	 } 



	 /*
	 **       Get the Micro Focus Cobol environment data
	 */
	 printf("\n\n");
	 printf("     Micro Focus Environment\n");
	 if (mf = getenv("COBSW"))
	 {
		  printf("COBSW      = %s\n",mf);
	 }
	 else
	 {
		  errmf = true;
		  printf("COBSW      = *** NOT DEFINED\n");
	 }
	 if (mf = getenv("COBOPT"))
	 {
		  printf("COBOPT     = %s\n",mf);
	 }
	 else
	 {
		  errmf = true;
		  printf("COBOPT     = *** NOT DEFINED\n");
	 }
	 if (mf = getenv("COBPATH"))
	 {
		  printf("COBPATH    = %s",mf);
		  if (check_access(mf,04) != 0)
		  {
			   errmf = true;
		  } 
	 }
	 else
	 {
		  errmf = true;
		  printf("COBPATH    = *** NOT DEFINED\n");
	 }

	 printf("cob        = /usr/bin/cob"); 
	 if (check_access("/usr/bin/cob",04) != 0)
	 {
		  errmf = true;
	 } 
	 if (mf = getenv("COBDIR"))
	 {
		printf("COBDIR     = %s",mf);
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
		  printf("COBDIR     = *** NOT DEFINED\n");
	 }

	  
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

