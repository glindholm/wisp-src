			/************************************************************************/
			/*									*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wlicense.c
**
**	Purpose:	To contain the license utility routines.
**			This is the program that is sent out to every customer to install there license.
**
**	Routines:	main()			The wlicense utility main routine.
**			get_validation()	Get the validation code fron the user.
**			exit_wlicense()		Cleanup then exit.
**			assistance()		Print an assistance message.
**			putheader()		Print the copyright and running instructions.
**
**	History:
**	05/22/92	Written by GSL
**	05/26/92	Added get_validation() and exit_wlicense()  GSL
**	09/25/92	Added LICENSE_CLUSTER. GSL
**	07/07/93	Added MSDOS support. GSL
**
*/

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>

#include "wlicense.h"

extern char	*sys_errlist[];
static int get_validation();
static int exit_wlicense();
static int assistance();
static int putheader();

static	int created_license_file = 0;

/*
**	Routine:	main()		[xlicense]
**
**				wlicense	WISP
**				ulicense	UniQue
**
**	Function:	To install the license.
**
**	Description:	This routine validate the LICENSE KEY displays the MACHINE ID and prompts for the VALIDATION CODE.
**			If all is correct it creates the license file with this info.
**			You must be ROOT to run this program.
**			It will give detail instructions to the user.
**
**	Input:		stdin
**			
**
**	Output:		stdout
**			unix:	"/lib/{product}.license"
**			MSDOS:	"C:\{product}.LIC"
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/22/92	Written by GSL
**	07/07/93	Added MSDOS. GSL
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
	char	*help, helpbuff[256];
	char	buff[256];

	putheader();

#ifdef unix
	if (0 != geteuid())
	{
		printf("SORRY - you must be root to continue.\n");
		exit(0);
	}
#endif

	sprintf(helpbuff,"Enter Y to continue the %s license installation.\nEnter N to exit.",product_name());
	rc = prompt_list("Do you wish to continue","y","y,n",helpbuff);
	if (rc == -1 || rc == 2) exit(0);

	/*
	**	Check if license file already exists.
	*/

	if (0 == access(license_filepath(),0000))
	{
		printf("\n");
		printf("**** WARNING ****\n");
		printf("A %s license file %s already exists.\n",product_name(),license_filepath());
		printf("Continuing will delete the previous %s license file.\n\n",product_name());

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
		sprintf(helpbuff, "Enter the %s software LICENSE KEY exactly as it is was given to you.\n",product_name());
		strcat(helpbuff,"Enter a period (.) to exit.");
		rc = prompt_text("Enter the LICENSE KEY",NULL,1,helpbuff,buff);
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
	**	Create the license file.
	*/

	if (rc = create_license_file())
	{
		printf("SORRY - Unable to create %s [errno = %d %s]\n",license_filepath(),errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
	}

	created_license_file = 1;

	*machineid = '\0';
	*valcode = '\0';

	switch(licensetype)
	{
		/*
		**	CLUSTER license requires a validation code just like SINGLE does so use same logic.
		*/
	case LICENSE_CLUSTER:
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

#ifdef MSDOS
	/*
	**	On MSDOS there is no "root" user so we have to make file writable first in case
	**	we are overwriting an existing license file.
	**	(Otherwise an empty file was created earlier.)
	*/
	chmod(license_filepath(),0666);
#endif

	rc = write_license(custname,custnum,platform,licensekey,licensetype,licensedate,expdate,machineid,valcode);
	switch(rc)
	{
	case 0:
		break;
	case 1:
		printf("SORRY - Unable to open file %s [errno = %d %s]\n",license_filepath(),errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
		break;
	case 2:
		printf("SORRY - Unable to write file %s [errno = %d %s]\n",license_filepath(),errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
		break;
	default:
		printf("SORRY - Error writing file %s [errno = %d %s]\n",license_filepath(),errno,sys_errlist[errno]);
		assistance();
		exit_wlicense();
		break;
	}

	printf("\n");
	printf("The %s license file %s has been created.\n\n",product_name(),license_filepath());

#ifdef unix
	sprintf(buff,"cat %s",license_filepath());
	system(buff);
	printf("\n");
#endif

#ifdef MSDOS
	hide_file(license_filepath());
	sprintf(buff,"type %s",license_filepath());
	system(buff);
	printf("\n");
#endif

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
	printf("In order to install this %s license you will need a VALIDATION CODE.\n",product_name());
	printf("A VALIDATION CODE can be received by calling I.D.S.I at (805) 295-1155\n");
	printf("(M-F 8:30a-5:00p PST) and asking for a VALIDATION CODE.  You will be asked for\n");
	printf("the following information:\n");
	printf("\n\n");
	printf("       LICENSE KEY:    %s\n",flickey);
	printf("       MACHINE ID:     %s\n",machineid);
	printf("\n\n");
	printf("Please supply the information EXACTLY as written above.\n");
	printf("\n");

	/*                12345678901234567890123456789012345678901234567890123456789012345678901234567890			*/
	sprintf(helpbuff,"Call I.D.S.I. at (805) 295-1155 (M-F 8:30a-5:00p PST) to get a VALIDATION CODE.\n");
	strcat (helpbuff,"If you are unable to call at this time you may record the information and call\n");
	strcat (helpbuff,"at your convenience.\n\n\n");
	strcat (helpbuff,"       LICENSE KEY:    ");
	strcat (helpbuff,flickey);
	strcat (helpbuff,"\n");
	strcat (helpbuff,"       MACHINE ID:     ");
	strcat (helpbuff,machineid);
	strcat (helpbuff,"\n\n\n");
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
#ifdef unix
		link(license_filepath(),x_license_filepath());
#endif
		unlink(license_filepath());
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
	/*      12345678901234567890123456789012345678901234567890123456789012345678901234567890	*/
	printf("If you need assistance call I.D.S.I. at (805) 295-1155 (M-F 8:30a-5:00p PST).\n");
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
	printf("                   **** %s LICENSE INSTALLATION TOOL ****\n",product_name());
	printf("         Copyright (c) 1992,93 by International Digital Scientific Inc.\n");
	printf("                             (805) 295-1155\n");
	printf("\n");
	printf("\n");
	printf("This program will install the %s runtime license onto this machine.\n",product_name());
#ifdef unix
	printf("In order to run this program you must be logged on as the root user.\n");
#endif
	printf("\n");
	printf("This program will request your %s software LICENSE KEY.  The LICENSE KEY is\n",product_name());
	printf("a 16 digit code that is sent out when you license the %s product, or when\n",product_name());
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
