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
**
*/

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#ifdef unix
#include <unistd.h>
#include <sys/stat.h>
#endif

#ifdef WIN32
#include <io.h>
#include <direct.h>
#endif

#include "wcommon.h"
#include "wlicense.h"
#include "idsisubs.h"
#include "prompt.h"
#include "platsubs.h"
#include "machid.h"
#include "wispcfg.h"
#include "wispvers.h"

#if defined(unix) && !defined(LINUX)
extern char	*sys_errlist[];
#endif

static void get_validation(	char	licensekey[LICENSE_KEY_SIZE],
				char	*machineid,
				char	valcode[VALIDATION_CODE_SIZE]);
static void get_appcode_validation(	char	licensekey[LICENSE_KEY_SIZE],
					char	*appcode,
					char	valcode[VALIDATION_CODE_SIZE]);

static void exit_wlicense();
static void assistance();
static void putheader();
static void catfile(const char *filename);
static void finish_ok(void);

static int  WLIC_write_license(char	*custname,
		int4	custnum,
		char	platform[2],
		char	*licensekey,
		int	lictype,
		int4	version_number,
		char	licdate[8],
		char	expdate[8],
		char	*machineid,
		char	*valcode,
		char	*appcode);


/*
**	Routine:	main()		[wlicense]
**
**	Function:	To install the license.
**
**	Description:	This routine validate the LICENSE KEY displays the MACHINE ID 
**			and prompts for the VALIDATION CODE.
**			If all is correct it creates the license file with this info.
**			It will give detail instructions to the user.
**
**	Input:		stdin
**
**	Output:		stdout
**			wisp license file
**
**	Return:		None
**
**	Warnings:	None
**
*/

int main(argc,argv)
int	argc;
char	*argv[];
{
	char	custname[80];
	int4	custnum;
	char	platform[PLATFORM_CODE_LEN+1];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[80];
	char	machineid[MAX_MACHINEID_LENGTH+1];
	char	valcode[80] = { "" };
	char	appcode[80] = { "" };
	int4	version_number = 0;

	int	rc;
	int	done;
	char	*help, helpbuff[256];
	char	buff[256];

	int	already_exists = 0;

	putheader();

	/* Ensure we can get the machineid */
	if (0 != WL_getmachineid(machineid))
	{
		printf("\n");
		printf("------------------------------------------------------------------\n");
		printf("ERROR\n");
		printf("ERROR - Unable to determine the MACHINE ID\n");
		printf("ERROR\n");
		printf("------------------------------------------------------------------\n");
		printf("\n");

		assistance();
		finish_ok();
		exit(0);
	}

	/* Display License Info */
	{
		printf("PLATFORM:         [%s]\n", WL_platform_name());
		printf("MACHINE ID:       [%s]\n", machineid);
		printf("LICENSE FILE:     [%s]\n", WLIC_license_filepath());
	}

	if (0 == WLIC_get_license_info(licensekey, valcode, appcode))
	{
		printf("LICENSE KEY:      [%s]\n", licensekey);
		if (valcode[0] != '\0')
		{
			printf("VALIDATION CODE:  [%s]\n", valcode);
		}
		if (appcode[0] != '\0')
		{
			printf("APPLICATION CODE: [%s]\n", appcode);
		}

	}

#ifdef unix
	if (0 == strcmp(wispconfigdir(),WISPCONFIG_UNSET_VALUE))
	{
		/*      12345678901234567890123456789012345678901234567890123456789012345678901234567890			*/
		printf("\n");
		printf("------------------------------------------------------------------\n");
		printf("ERROR\n");
		printf("ERROR - The WISP Configuration directory ($WISPCONFIG) is not set.\n");
		printf("ERROR - It must be set in order to continue.\n");
		printf("ERROR\n");
		printf("------------------------------------------------------------------\n");
		printf("\n");

		assistance();
		finish_ok();
		exit(0);
	}
#endif /* unix */

	/*
	**	Check if license file already exists.
	*/

	if (0 == access(WLIC_license_filepath(),0000))
	{
		already_exists = 1;
		printf("\n");
		printf("-----------------------------------------------------------\n");
		printf("WARNING - A %s license file already exists.\n",WLIC_product_name());
		printf("WARNING - Continuing will delete the previous %s license file.\n",WLIC_product_name());
		printf("-----------------------------------------------------------\n");
		printf("\n");

		help = "Enter Y to continue.\nEnter N to exit the program.";
		rc = prompt_list("Do you wish to continue",NULL,"y,n",help);
		if (rc != 1) exit(0);
	}
	else
	{
		printf("\n");
		sprintf(helpbuff,"Enter Y to continue the %s license installation.\nEnter N to exit.",WLIC_product_name());
		rc = prompt_list("Do you wish to continue","y","y,n",helpbuff);
		if (rc == -1 || rc == 2) exit(0);
	}

	/*
	**	Enter COMPANY NAME
	*/
	done = 0;
	while(!done)
	{
		printf("\n");
		help = "Enter your company name.\nEnter a period (.) to exit.";
		rc = prompt_text("Enter your company name",NULL,1,help,custname);
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

	/*
	**	Enter LICENSE KEY
	*/
	done = 0;
	while(!done)
	{
		printf("\n");
		sprintf(helpbuff, "Enter the %s software LICENSE KEY exactly as it is was given to you.\n",WLIC_product_name());
		strcat(helpbuff,"Enter a period (.) to exit.");
		rc = prompt_text("Enter the LICENSE KEY",NULL,1,helpbuff,buff);
		switch(rc)
		{
		case -1:
			exit(0);
			break;
		case 1: /* text was entered */
			WL_upper_string(buff);
			WLIC_unformatkey(licensekey,buff);
			if (WLIC_bklickey(&custnum,platform,&licensetype,licensedate,expdate,
				&version_number,licensekey))
			{
				printf("------------------------------------------------\n");
				printf("SORRY - You have entered an invalid LICENSE KEY.\n");
				printf("------------------------------------------------\n");
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
	**	The user has entered a real LICENSE KEY.  
	**	Now check if it is valid for this machine.
	*/
	if (WL_valplat(platform))
	{
		printf("------------------------------------------------\n");
		printf("SORRY - The LICENSE KEY given is not valid for this [%s] platform.\n", WL_platform_name());
		printf("------------------------------------------------\n");
		assistance();
		finish_ok();
		exit_wlicense();
	}

	/*
	**	Get VALIDACTION CODE (if needed)
	*/

	*valcode = '\0';
	switch(licensetype)
	{
		/*
		**	CLUSTER license requires a validation code just like SINGLE does so use same logic.
		*/
	case LICENSE_CLUSTER:
	case LICENSE_SINGLE:
		get_validation(licensekey,machineid,valcode);
		break;
#ifdef WIN32
	case LICENSE_NETWORK:
		/*
		**	If running on a CLIENT then the machineid is 
		**	generated from the SERVER name.
		*/
		if (0 != strcmp(wispserver(), WL_computername(NULL)) &&
		    0 != strcmp(wispserver(), DEF_WISPSERVER)        )
		{
			*machineid = '\0';
			WL_encodemachid(wispserver(), machineid);
		}
		get_validation(licensekey,machineid,valcode);
		break;
#endif
	case LICENSE_UNLIMITED:
		printf("\n");
		/*      123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 */
		printf("---------------------------------------------------------------------------\n");
		printf("WARNING - You have entered an OLD sytle UNLIMITED license key. \n");
		printf("WARNING - This release of WISP does not support UNLIMITED license keys.\n");
		printf("WARNING - Contact your WISP vendor or NeoMedia for assistance.\n");
		printf("---------------------------------------------------------------------------\n");
		printf("\n");

		help = "Enter Y to continue.\nEnter N to exit the program.";
		rc = prompt_list("Do you wish to continue",NULL,"y,n",help);
		if (rc != 1) 
		{ 
			exit_wlicense(0);
		}

		break;
	case LICENSE_ENTERPRISE:
		*machineid = '\0';
		get_appcode_validation(licensekey,appcode,valcode);
		break;
	case LICENSE_TIMED:
		break;
	default:
		printf("----------------------------------------------------------\n");
		printf("SORRY - This LICENSE KEY contains an unknown LICENSE TYPE.\n");
		printf("----------------------------------------------------------\n");
		assistance();
		finish_ok();
		exit_wlicense(0);
		break;
	}

	/*
	**	Delete the old license file
	*/
	if (already_exists)
	{
#ifdef WIN32
		/*
		**	Make file writable first in case
		**	we are overwriting an existing license file.
		*/
		chmod(WLIC_license_filepath(),0666);
#endif

		if ( 0 != unlink(WLIC_license_filepath()))
		{
			printf("----------------------------------------------------------\n");
			printf("SORRY - Unable to delete license file %s [errno = %d %s]\n",
				WLIC_license_filepath(),errno,sys_errlist[errno]);
			printf("----------------------------------------------------------\n");
			assistance();
			finish_ok();
			exit_wlicense();
		}
	}

	/*
	**	Create the new license file
	*/
	rc = WLIC_write_license(custname,custnum,platform,licensekey,licensetype,
		version_number,licensedate,expdate,machineid,valcode,appcode);
	switch(rc)
	{
	case 0:
		break;
	case 1:
		printf("----------------------------------------------------------\n");
		printf("SORRY - Unable to open file %s [errno = %d %s]\n",WLIC_license_filepath(),errno,sys_errlist[errno]);
		printf("----------------------------------------------------------\n");
		assistance();
		finish_ok();
		exit_wlicense();
		break;
	case 2:
		printf("----------------------------------------------------------\n");
		printf("SORRY - Unable to write file %s [errno = %d %s]\n",WLIC_license_filepath(),errno,sys_errlist[errno]);
		printf("----------------------------------------------------------\n");
		assistance();
		finish_ok();
		exit_wlicense();
		break;
	default:
		printf("----------------------------------------------------------\n");
		printf("SORRY - Error writing file %s [errno = %d %s]\n",WLIC_license_filepath(),errno,sys_errlist[errno]);
		printf("----------------------------------------------------------\n");
		assistance();
		finish_ok();
		exit_wlicense();
		break;
	}

	printf("\n");
	printf("The %s license file %s has been created.\n\n",
		WLIC_product_name(),WLIC_license_filepath());

	catfile(WLIC_license_filepath());
	printf("\n");

	finish_ok();

	return 0;
}

static void finish_ok(void)
{
#ifdef WIN32
	char	buff[80];
	
	prompt_text("Press ENTER to finish",NULL,1,"Press ENTER to finish.", buff);
	printf("\n");
#endif
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

static void get_validation(	char	licensekey[LICENSE_KEY_SIZE],
				char	*machineid,
				char	valcode[VALIDATION_CODE_SIZE])
{
	int	rc;
	char	*help;
	char	flickey[80];
	char	buff[80];
	char	helpbuff[1024];

	WLIC_formatkey(licensekey,flickey);

	/*                12345678901234567890123456789012345678901234567890123456789012345678901234567890			*/
	sprintf(helpbuff,"In order to install this %s license you will need a VALIDATION CODE.\n",WLIC_product_name());
	strcat (helpbuff,"A VALIDATION CODE can be received by emailing NeoMedia at ");
	strcat (helpbuff,WISP_EMAIL);
	strcat (helpbuff,"\n");
	strcat (helpbuff,"and requesting a VALIDATION CODE for the LICENSE KEY and MACHINE ID displayed\n");
	strcat (helpbuff,"here:\n");
	strcat (helpbuff,"\n");
	strcat (helpbuff,"       LICENSE KEY:    ");
	strcat (helpbuff,flickey);
	strcat (helpbuff,"\n");
	strcat (helpbuff,"       MACHINE ID:     ");
	strcat (helpbuff,machineid);
	strcat (helpbuff,"\n\n");
	strcat (helpbuff,"Please supply the information EXACTLY as written above.\n");
	strcat (helpbuff,"\n");
	strcat (helpbuff,"You may enter a period (.) to exit.\n");
	help = helpbuff;

	for(;;)
	{
		printf("\n%s\n",help);

		rc = prompt_text("Enter the VALIDATION CODE",NULL,1,help,buff);
		if (rc == -1) exit_wlicense();

		if (rc == 1)
		{
			WL_upper_string(buff);

			if (VALIDATION_CODE_SIZE != strlen(buff) ||
			    0 != WLIC_ckvalcode(licensekey,machineid,buff))
			{
				printf("\n");
				printf("----------------------------------------------------\n");
				printf("SORRY - you have entered an invalid VALIDATION CODE.\n");
				printf("----------------------------------------------------\n");
			}
			else
			{
				memcpy(valcode,buff,VALIDATION_CODE_SIZE);
				return;
			}
		}

		assistance();
	}
}

/*
**	Routine:	get_appcode_validation()
**
**	Function:	To get the appcode and validation code from the user.
**
**	Description:	This routine will prompt the user to enter the appcode and 
**			validation code.  It will give a description of
**			how to get the validation code.  
**			It will then ensure that a valid code was entered.
**
**	Input:		licensekey		the license key
**			appcode			the applicarion code
**			stdin
**
**	Output:		valcode			the validation code
**			stdout
**
**	Return:		None
**
**	Warnings:	If an invalid VALIDATION CODE was entered then this routine 
**			will call exit_wlicense() and not return 
**			to the caller.  Likewise it will call exit_wlicense() if the 
**			prompt routines recieve an exit request.
**
*/

static void get_appcode_validation(	char	licensekey[LICENSE_KEY_SIZE],
					char	*appcode,
					char	valcode[VALIDATION_CODE_SIZE])
{
	int	rc;
	char	*help;
	char	flickey[80];
	char	buff[80];
	char	helpbuff[1024];
	int	done;

	WLIC_formatkey(licensekey,flickey);

	/*                12345678901234567890123456789012345678901234567890123456789012345678901234567890			*/
	sprintf(helpbuff,"In order to install this %s license you will need the APPLICATION CODE \n",WLIC_product_name());
	strcat (helpbuff,"and the VALIDATION CODE for this license.  If you don't have them contact \n");
	strcat (helpbuff,"your WISP vendor or NeoMedia at ");
	strcat (helpbuff,WISP_EMAIL);
	strcat (helpbuff,"\n");
	strcat (helpbuff,"\n");
	strcat (helpbuff,"You may enter a period (.) to exit.\n");
	help = helpbuff;

	printf("\n%s\n",help);
	rc = prompt_text("Enter the APPLICATION CODE",NULL,1,help,appcode);
	if (rc == PROMPT_RC_EXIT) exit_wlicense();
	WL_upper_string(appcode);

	done = 0;
	while(!done)
	{
		rc = prompt_text("Enter the VALIDATION CODE",NULL,1,help,buff);
		if (rc == PROMPT_RC_EXIT) exit_wlicense();

		if (rc == 1)
		{
			WL_upper_string(buff);

			if (VALIDATION_CODE_SIZE != strlen(buff) ||
			    0 != WLIC_ckvalcode(licensekey,appcode,buff))
			{
				printf("\n");
				printf("----------------------------------------------------\n");
				printf("SORRY - you have entered an invalid VALIDATION CODE.\n");
				printf("----------------------------------------------------\n");
			}
			else
			{
				memcpy(valcode,buff,VALIDATION_CODE_SIZE);
				return;
			}
		}

		assistance();
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
**	Input:		
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

static void exit_wlicense()
{
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

static void assistance()
{
	printf("\n");
	/*      12345678901234567890123456789012345678901234567890123456789012345678901234567890	*/
	printf("If you need assistance email NeoMedia at %s .\n", WISP_EMAIL);
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

static void putheader()
{
/*		123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890				*/
	printf("\n");
	printf("                **** %s %s LICENSE INSTALLATION TOOL ****\n",WLIC_product_name(), wisp_version());
	printf("         Copyright (c) 1994-" WISP_COPYRIGHT_YEAR_STR " by NeoMedia Technologies Incorporated\n");
	printf("                Email: %s   Phone: %s\n", WISP_EMAIL, WISP_PHONE_NUMBER);
	printf("\n");
	printf("This program will install a %s license onto this machine.\n",WLIC_product_name());
	printf("\n");
	printf("While this program is running you may get help on any prompt by entering\n");
	printf("a question mark or you may exit the program by entering a period.\n");
	printf("         ?   HELP on the prompt.\n");
	printf("         .   EXIT the program.\n");
	printf("\n");
}

static void catfile(const char *filename)
{
	char	inlin[256];
	FILE	*fh;
	
	fh = fopen(filename, "r");
	if (!fh)
	{
		printf("Error opening license file [%s]\n", filename);
		return;
	}

	while (fgets(inlin,sizeof(inlin),fh))
	{
		printf("%s",inlin);
	}
	
}



/*
**	Routine:	WLIC_write_license()
**
**	Function:	To write the license file.
**
**	Description:	This routine will write the license file.
**			The dates will be formatted.  A expiration date of 00000000 will display as "None".
**			After writing the file the mode is changed to read-only (444).
**
**			The file will contain.
**				LICENSEE	  ABC company
**				CUSTOMER-NUMBER	  10001
**				PLATFORM	  AX - AIX RISC
**				LICENSE-KEY	  ABCD-ABCD-ABCD-ABCD
**				LICENSE-TYPE	  SINGLE
**				LICENSE-DATE	  1992/05/01
**				EXPIRATION-DATE	  None
**				VERSION-NUMBER	  None
**				MACHINE-ID	  0000234500		| These are only on SINGLE machine licenses
**				VALIDATION-CODE   ASD			|
**
**	Input:		custname		the LICENSEE
**			custnum			the CUSTOMER-NUMBER
**			platform		the 2 digit platform code
**			licensekey		the 16 digit license key
**			lictype			the encode license type
**			licdate			the 8 digit license date
**			expdate			the 8 digit expiration date or 00000000
**			machineid		the MACHINE ID or empty string
**			valcode			the VALIDATION CODE or empty string
**
**	Output:		/lib/{product}.license	the license file
**			
**
**	Return:		0 = success
**			1 = open failed
**			2 = error in writing
**
**	Warnings:	Only the LICENSE-KEY and the VALIDATION-CODE are ever read for validation.
**
**	History:	05/26/92	Written by GSL
**
*/

static int WLIC_write_license(char	*custname,
		int4	custnum,
		char	platform[2],
		char	*licensekey,
		int	lictype,
		int4	version_number,
		char	licdate[8],
		char	expdate[8],
		char	*machineid,
		char	*valcode,
		char	*appcode)
{
	FILE	*fp;
	char	flickey[80];
	char	platname[80];
	char	licdatebuff[20];
	char	expdatebuff[20];

	if (WL_plat_code(platform,platname,NULL))				/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}

	WLIC_formatkey(licensekey,flickey);					/* format the key				*/

	sprintf(licdatebuff,"%4.4s/%2.2s/%2.2s",&licdate[0],&licdate[4],&licdate[6]);

	strcpy(expdatebuff,"None");
	if (lictype == LICENSE_TIMED)
	{
		sprintf(expdatebuff,"%4.4s/%2.2s/%2.2s",&expdate[0],&expdate[4],&expdate[6]);
	}

	fp = fopen(WLIC_license_filepath(),"w");
	if (!fp)
	{
		return(1);
	}

	fprintf(fp,		"LICENSEE           %s\n",custname);
	fprintf(fp,		"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,		"PLATFORM           %2.2s - %s\n",platform,platname);
	fprintf(fp,		"LICENSE-KEY        %s\n",flickey);
	fprintf(fp,		"LICENSE-TYPE       %s\n",WLIC_lictypename(lictype));
	fprintf(fp,		"LICENSE-DATE       %s\n",licdatebuff);
	fprintf(fp,		"EXPIRATION-DATE    %s\n",expdatebuff);
	if (version_number != 0)
	{
		fprintf(fp,	"VERSION-NUMBER     %d.%d\n", version_number/10, version_number%10);
	}
	else
	{
		fprintf(fp,	"VERSION-NUMBER     None\n");
	}
	if (machineid && *machineid)
	{
		fprintf(fp,	"MACHINE-ID         %s\n",machineid);
	}
	if (valcode && *valcode)
	{
		fprintf(fp,	"VALIDATION-CODE    %3.3s\n",valcode);
	}
	if (appcode && *appcode)
	{
		fprintf(fp,	"APPLICATION-CODE   %s\n",appcode);
	}

	if (ferror(fp))								/* Check for an error in writing		*/
	{
		fclose(fp);
		return(2);
	}

	fclose(fp);

	chmod(WLIC_license_filepath(),0444);					/* Make file readable by all			*/

	return(0);
}

/*
**	DUMMY routines to stop the whole WISPLIB from being linked in.
*/
void WL_wexit(int code)
{
	finish_ok();
	exit_wlicense();
}
void WL_werrlog()
{
}
void WL_werrlog_error()
{
}

void WL_get_wisp_option()
{
}

void WL_wtrace()
{
}


/*
**	History:
**	$Log: wlicense.c,v $
**	Revision 1.51  2003/06/13 17:36:12  gsl
**	ENTERPRISE License
**	
**	Revision 1.50  2003/06/12 20:54:30  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.49  2003/02/14 16:12:33  gsl
**	for unix ensure $WISPCONFIG is set
**	
**	Revision 1.48  2003/02/14 15:27:11  gsl
**	Remove unused routine, fix cosmetic
**	
**	Revision 1.47  2003/02/14 14:44:32  gsl
**	Rework for better error messages
**	
**	Revision 1.46  2003/02/13 22:28:01  gsl
**	fix permissions, since now not run as root.
**	Don't need to pre-create since inode stuff is gone.
**	Delete the old file just before creating the new one.
**	
**	Revision 1.45  2003/02/13 22:03:20  gsl
**	reorg startup
**	
**	Revision 1.44  2003/02/13 22:00:27  gsl
**	reorg startup
**	
**	Revision 1.43  2003/02/13 21:51:10  gsl
**	reorg startup
**	
**	Revision 1.42  2003/02/13 21:19:23  gsl
**	Change "call" Neomedia to "email" .
**	
**	Revision 1.41  2003/02/13 20:59:25  gsl
**	On unix don't need to be root.
**	Display the license file path and platform.
**	
**	Revision 1.40  2003/02/13 20:45:03  gsl
**	On unix change the license file from /lib/wisp.license to
**	$WISPCONFIG/wisp.{machineid}.license
**	
**	Revision 1.39  2003/02/07 20:45:14  gsl
**	Add platform to version display
**	
**	Revision 1.38  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.37  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.36  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.35  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.34  2003/01/03 16:48:52  gsl
**	Move routines to write license file to wlicense.c
**	
**	Revision 1.33  2002/12/10 17:09:11  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.32  2002/10/14 16:02:44  gsl
**	Wlicense Display the Machine Id and current license info at begining
**	
**	Revision 1.31  2002/10/14 15:55:14  gsl
**	Display the Machine Id and current license info at begining
**	
**	Revision 1.30  2002/10/14 15:41:40  gsl
**	Expose WLIC_get_license_info()
**	
**	Revision 1.29  2002/09/04 18:13:48  gsl
**	LINUX sys_errlist
**	
**	Revision 1.28  2002/07/18 21:04:24  gsl
**	Remove MSDOS code
**	
**	Revision 1.27  2002/07/11 14:33:56  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.26  2002/07/10 21:06:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.25  2002/07/09 04:13:49  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.24  2002/06/26 20:52:16  gsl
**	Fix phone number
**	
**	Revision 1.23  2002/06/25 18:18:36  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.22  2002/03/28 15:09:37  gsl
**	use define for copyright year
**	
**	Revision 1.21  2002-03-26 16:50:19-05  gsl
**	(C) 2002
**
**	Revision 1.20  2001-09-05 14:46:10-04  gsl
**	change copyright date
**
**	Revision 1.19  2000-03-16 10:28:52-05  gsl
**	2000
**
**	Revision 1.18  2000-03-13 14:19:35-05  gsl
**	Fix WIN32 warnings
**
**	Revision 1.17  1999-09-23 13:36:29-04  gsl
**	COPYRIGHT date
**
**	Revision 1.16  1999-02-24 09:37:26-05  gsl
**	Add dummy wtrace
**
**	Revision 1.15  1998-08-21 10:44:27-04  gsl
**	Enhance so that on WIN32 a NETWORK license server can be licensed
**	by wlicense from a client.
**
**	Revision 1.14  1997-12-18 20:44:03-05  gsl
**	Add DUMMY routines
**
**	Revision 1.13  1997-03-17 11:54:59-05  gsl
**	Add NETWORK license logic.
**	Add finish_ok() for WIN32 to prompt before finishing to stop the console
**	from closing
**
**	Revision 1.12  1997-03-14 15:59:11-05  gsl
**	update for WIN32
**
**	Revision 1.11  1997-02-17 16:54:42-05  gsl
**	Change address and phone number
**
**	Revision 1.10  1996-12-12 13:15:30-05  gsl
**
**	Revision 1.9  1996-07-24 16:37:13-07  gsl
**	Fix for NT
**
**	Revision 1.8  1996-07-23 12:51:21-07  gsl
**	Fix warning
**
**	Revision 1.7  1996-07-23 11:13:10-07  gsl
**	drcs update
**
**	05/22/92	Written by GSL
**	05/26/92	Added get_validation() and exit_wlicense()  GSL
**	09/25/92	Added LICENSE_CLUSTER. GSL
**	07/07/93	Added MSDOS support. GSL
**
**
*/
