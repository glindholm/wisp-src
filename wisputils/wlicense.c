			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	File:		wlicense.c
**
**	Purpose:	To contain the wlicense utility routines.
**			This is the program that is sent out to every customer to install there wisp license.
**
**	Routines:	main()			The wlicense utility main routine.
**			get_validation()	Get the validation code fron the user.
**			exit_wlicense()		Cleanup then exit.
**			assistance()		Print an assistance message.
**			putheader()		Print the copyright and running instructions.
**
**	History:
**			05/22/92	Written by GSL
**			05/26/92	Added get_validation() and exit_wlicense()  GSL
**
*/

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>

#include "wlicense.h"

#define IDSIPHONE	"(805) 295-1155"

extern char	*sys_errlist[];

static	int created_license_file = 0;

/*
**	Routine:	main()		wlicense
**
**	Function:	To install the WISP license.
**
**	Description:	This routine validate the LICENSE KEY displays the MACHINE ID and prompts for the VALIDATION CODE.
**			If all is correct it creates the WISP license file with this info.
**			You must be ROOT to run this program.
**			It will give detail instructions to the user.
**
**	Input:		stdin
**			
**
**	Output:		stdout
**			/lib/wisp.license
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/22/92	Written by GSL
**
*/

main(argc,argv)
int	argc;
char	*argv[];
{
	char	custname[80];
	long	custnum;
	char	platform[3];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[20];
	char	machineid[80];
	char	valcode[20];

	int	rc;
	int	done;
	char	*help;
	char	buff[256];

	putheader();

	if (0 != geteuid())
	{
		printf("SORRY - you must be root to continue.\n");
		exit(0);
	}

	help = "Enter Y to continue the WISP license installation.\nEnter N to exit.";
	rc = prompt_list("Do you wish to continue","y","y,n",help);
	if (rc == -1 || rc == 2) exit(0);

	/*
	**	Check if WISP license file already exists.
	*/

	if (0 == access(WISP_LICENSE_FILE,0000))
	{
		printf("\n");
		printf("**** WARNING ****\n");
		printf("A WISP license file %s already exists.\n",WISP_LICENSE_FILE);
		printf("Continuing will delete the previous WISP license file.\n\n");

		help = "Enter Y to continue.\nEnter N to exit the program.";
		rc = prompt_list("Do you wish to continue",NULL,"y,n",help);
		if (rc != 1) exit(0);
	}

	done = 0;
	while(!done)
	{
		printf("\n");
		help = "Enter your company name.\nEnter a period (.) to exit.";
		rc = prompt_text("Enter your company name",NULL,1,help,custname);
		printf("\n");
		switch(rc)
		{
		case -1:
			exit(0);
			break;
		case 1:
			done = 1;
			break;
		case 2:
			printf("Please enter your company name.");
			break;
		}
	}

	done = 0;
	while(!done)
	{
		help = "Enter the WISP software LICENSE KEY exactly as it is was given to you.\nEnter a period (.) to exit.";
		rc = prompt_text("Enter the LICENSE KEY",NULL,1,help,buff);
		printf("\n");
		switch(rc)
		{
		case -1:
			exit(0);
			break;
		case 1: /* text was entered */
			upper_string(buff);
			unformatkey(licensekey,buff);
			if (bklickey(&custnum,platform,&licensetype,licensedate,expdate,licensekey))
			{
				printf("You have entered an invalid LICENSE KEY.\n");
			}
			else
			{
				done = 1;
			}
			break;
		case 2: /* empty */
			printf("Please enter the LICENSE KEY.\n");
			break;
		}

		if (!done)
		{
			assistance();
			printf("To exit this program enter a period (.) at the prompt.\n");
			printf("\n");
		}
	}


	/*
	**	The user has entered a real LICENSE KEY.  Now check if it is valid for this machine.
	*/

	if (valplat(platform))
	{
		printf("SORRY - The LICENSE KEY given is not valid for this platform.\n");
		assistance();
		exit(0);
	}

	/*
	**	Create the WISP license file.
	*/

	if (rc = create_license_file())
	{
		printf("SORRY - Unable to create %s [errno = %d %s]\n",WISP_LICENSE_FILE,errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
	}

	created_license_file = 1;

	*machineid = '\0';
	*valcode = '\0';

	switch(licensetype)
	{
	case LICENSE_SINGLE:
		if (rc = getmachineid(machineid))
		{
			printf("SORRY - Unable to get the MACHINE ID\n");
			assistance();
			exit_wlicense();
		}
		get_validation(licensekey,machineid,valcode);
		break;
	case LICENSE_UNLIMITED:
		break;
	case LICENSE_TIMED:
		break;
	default:
		printf("SORRY - This LICENSE KEY contains an unknown LICENSE TYPE.\n");
		assistance();
		exit_wlicense(0);
		break;
	}

	rc = write_license(custname,custnum,platform,licensekey,licensetype,licensedate,expdate,machineid,valcode);
	switch(rc)
	{
	case 0:
		break;
	case 1:
		printf("SORRY - Unable to open file %s [errno = %d %s]\n",WISP_LICENSE_FILE,errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
		break;
	case 2:
		printf("SORRY - Unable to write file %s [errno = %d %s]\n",WISP_LICENSE_FILE,errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
		break;
	default:
		printf("SORRY - Error writing file %s [errno = %d %s]\n",WISP_LICENSE_FILE,errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
		break;
	}

	printf("\n");
	printf("The WISP license file %s has been created.\n\n",WISP_LICENSE_FILE);
	sprintf(buff,"cat %s",WISP_LICENSE_FILE);
	system(buff);
	printf("\n");

	exit(0);
}

/*
**	Routine:	get_validation()
**
**	Function:	To get the validation code from the user.
**
**	Description:	This routine will prompt the user to enter the validation code.  It will give a description of
**			how to get the validation code.  It will then ensure that a valid code was entered.
**
**	Input:		licensekey		the license key
**			machineid		the machine id
**			stdin
**
**	Output:		valcode			the validation code
**			stdout
**
**	Return:		None
**
**	Warnings:	If an invalid VALIDATION CODE was entered then this routine will call exit_wlicense() and not return 
**			to the caller.  Likewise it will call exit_wlicense() if the prompt routines recieve an exit request.
**
**	History:	05/26/92	Written by GSL
**
*/

static get_validation(licensekey,machineid,valcode)
char	licensekey[LICENSE_KEY_SIZE];
char	*machineid;
char	valcode[VALIDATION_CODE_SIZE];
{
	int	rc;
	char	*help;
	char	flickey[80];
	char	buff[80];
	char	helpbuff[1024];

	formatkey(licensekey,flickey);

	/*      12345678901234567890123456789012345678901234567890123456789012345678901234567890				*/
	printf("\n");
	printf("In order to install this WISP license you will need a VALIDATION CODE.\n");
	printf("A VALIDATION CODE can be received by calling I.D.S.I at %s and\n",IDSIPHONE);
	printf("asking for a VALIDATION CODE.  You will be asked for the following information:\n");
	printf("\n");
	printf("       LICENSE KEY:    %s\n",flickey);
	printf("       MACHINE ID:     %s\n",machineid);
	printf("\n");
	printf("Please supply the information EXACTLY as written above.\n");
	printf("\n");

	sprintf(helpbuff,"Call I.D.S.I. at %s to get a VALIDATION CODE.\n",IDSIPHONE);
	strcat (helpbuff,"If you are unable to call at this time you may record the information\n");
	strcat (helpbuff,"and call at your conveneince.\n\n");
	strcat (helpbuff,"       LICENSE KEY:    ");
	strcat (helpbuff,flickey);
	strcat (helpbuff,"\n");
	strcat (helpbuff,"       MACHINE ID:     ");
	strcat (helpbuff,machineid);
	strcat (helpbuff,"\n\n");
	strcat (helpbuff,"You may enter a period (.) to exit.");
	help = helpbuff;

	for(;;)
	{
		rc = prompt_text("Enter the VALIDATION CODE",NULL,1,help,buff);
		if (rc == -1) exit_wlicense();

		if (rc == 1)
		{
			upper_string(buff);

			if (VALIDATION_CODE_SIZE != strlen(buff) ||
			    0 != ckvalcode(licensekey,machineid,buff))
			{
				printf("\nSORRY - you have entered an invalid VALIDATION CODE.\n");
				assistance();
			}
			else
			{
				memcpy(valcode,buff,VALIDATION_CODE_SIZE);
				return;
			}
		}

		printf("\n%s\n\n",help);
	}
}

/*
**	Routine:	exit_wlicense()
**
**	Function:	To cleanup then exit.
**
**	Description:	This routine will do any cleanup required then call exit()
**			If the license file was created it will rename it to the temp inode name.
**
**	Input:		created_license_file	(global)	Flag if license file was created.
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	This routine will not return to the caller.
**
**	History:	05/26/92	Written by GSL
**			07/31/92	Added rename to temp inode file. GSL
*/

static exit_wlicense()
{
	if (created_license_file)
	{
		link(WISP_LICENSE_FILE,WISP_LICENSE_FILE_X);
		unlink(WISP_LICENSE_FILE);
	}
	exit(0);
}


/*
**	Routine:	assistance()
**
**	Function:	To put up a "If you need assistance..." message.
**
**	Description:	
**
**	Input:		None
**			
**
**	Output:		stdout
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/22/92	Written by GSL
**
*/

static assistance()
{
	printf("\n");
	printf("If you need assistance please call I.D.S.I. at %s.\n",IDSIPHONE);
}

/*
**	Routine:	putheader()
**
**	Function:	To print the copyright and header messages.
**
**	Description:	All it does is printf calls to output the info to stdout.
**
**	Input:		None
**
**	Output:		stdout
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/22/92	Written by GSL
**
*/

static putheader()
{
/*		123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890				*/
	printf("\n");
	printf("\n");
	printf("                   **** WISP LICENSE INSTALLATION TOOL ****\n");
	printf("         Copyright (c) 1992 by International Digital Scientific Inc.\n");
	printf("                             %s\n",IDSIPHONE);
	printf("\n");
	printf("\n");
	printf("This program will install the WISP runtime license onto this machine.\n");
	printf("In order to run this program you must be logged on as the root user.\n");
	printf("\n");
	printf("This program will request your WISP software LICENSE KEY.  The LICENSE KEY is\n");
	printf("a 16 digit code that is sent out when you license the WISP product, or when\n");
	printf("you request a demo license.\n");
	printf("\n");
	printf("You will need a LICENSE KEY to continue.  If you are licensed but do not know\n");
	printf("your LICENSE KEY or you would like a 30 day demo license please call the\n");
	printf("above number for assistance.\n");
	printf("\n");
	printf("While this program is running you may get help on any prompt by entering\n");
	printf("a question mark or you may exit the program by entering a period.\n");
	printf("         ?   HELP on the prompt.\n");
	printf("         .   EXIT the program.\n");
	printf("\n");
}

