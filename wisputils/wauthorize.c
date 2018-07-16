static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		wauthorize.c
**
**	Purpose:	The wauthorize utility
**
**	Routines:	main()		The wauthorize main routine.
**			gen_key()	Generate a LICENSE KEY
**			gen_val()	Generate a VALIDATION CODE
**			test_codes()	Test the LICENSE KEY and VALIDATION CODE
**			putheader()	Print the copyright and header info.
**			putkeyinfo()	Print the LICENSE KEY info
**			Logkeyinfo()	Log the LICENSE KEY info to the logfile.
**			logvalinfo()	Log the VALIDATION CODE info to the logfile.
**			printkeydoc()	Print the LICENSE KEY document
**
*/

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>
#include <grp.h>

#include "wlicense.h"

extern char	*sys_errlist[];

#define DOCTEMPLATE	"license.template"

static 	void printkeydoc(char* custname, int4 custnum, char platform[2], int lictype, char licdate[8], char expdate[8],
			 char lickey[LICENSE_KEY_SIZE], char* machid, char* valcode);


/*
**	Routine:	main()		[xauthorize]
**				wauthorize	WISP
**				uauthorize	UniQue
**
**	Function:	To generate a  LICENSE KEY and VALIDATION CODE.
**
**	Description:	This routine is for internal use only.
**
**			It will interactively prompt for information and use the info to generate a LICENSE KEY or a 
**			VALIDATION CODE.
**
**			It will record all this info to a log file.
**
**	Input:		stdin.
**			
**
**	Output:		stdout.
**			logfile.
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**
*/


main(argc,argv)
int	argc;
char	*argv[];
{
	char	*help;
	int	rc;
	struct group *grp;
	char	groupname[80];
	int	done;

	putheader();

	grp = getgrgid((int)getegid());
	if (grp)
	{
		strcpy(groupname,grp->gr_name);
		upper_string(groupname);
		if (0 != strcmp(groupname,"DEVTECH") &&
		    0 != strcmp(groupname,"NEOMEDIA"))
		{
			printf("\n\n**** Invalid Group ****\n");
			exit(1);
		}
	}
	else
	{
		printf("\n\n**** Unable to validate Group ****\n");
		exit(1);
	}

	for(;;)
	{
		printf("\n");
		/*      12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
		printf("===============================================================================\n");
		printf("%s AUTHORIZE UTILITY\n",product_name());
		printf("===============================================================================\n");
		printf("\n");
		
		help = "Enter K to generate a LICENSE KEY.\n"
		       "Enter V to generate a VALIDATION CODE.\n"
		       "Enter T to TEST either.\n"
		       "Enter Q to Quit.";
		
		rc = prompt_list("Generate a Key, Validation, Test, or Quit",NULL,"k,v,t,q",help);
		switch(rc)
		{
		case -1:
			exit(0);
			break;
		case 1:		/* LICENSE KEY */
			gen_key();
			printf("\n");
			help = "Enter Y to generate a VALIDATION CODE using the the LICENSE KEY information\n"
			       "you just entered.\n"
			       "Enter N to continue.\n"
			       "Enter . (period) to Exit";
			rc = prompt_list("Generate a VALIDATION CODE for this LICENSE KEY","n","y,n",help);
			if (rc == -1) exit(0);
			if (rc == 1)
			{
				gen_val(1);
			}
			break;
		case 2:		/* VALIDATION CODE */
			done=0;
			while(!done)
			{
				gen_val(0);					/* Generate the VALIDATION CODE			*/
				printf("\n");
				help = "Enter Y to generate another VALIDATION CODE.\nEnter N to continue.";
				rc = prompt_list("Generate another VALIDATION CODE","y","y,n",help);
				if (rc == -1) exit(0);
				if (rc == 2) done = 1;
			}
			break;
		case 3:		/* TEST */
			done=0;
			while(!done)
			{
				test_codes();					/* TEST keys or validation codes		*/
				printf("\n");
				help = "Enter Y to TEST another LICENSE KEY and VALIDATION CODE.\nEnter N to continue.";
				rc = prompt_list("Test another Key and Validation","y","y,n",help);
				if (rc == -1) exit(0);
				if (rc == 2) done = 1;
			}
			break;
			
		case 4:
			exit(0);
			
		}

	}
}


/*
**	Routine:	gen_key()
**
**	Function:	To generate a LICENSE KEY.
**
**	Description:	This routine prompts for the infomation neccessary to generate a LICENSE KEY.
**
**			For a LICENSE KEY it will ask for the following info:
**				CUSTOMER NAME: 		(comment)
**				CUSTOMER NUMBER:	6 digit code
**				PLATFORM:		2 digit code
**				LICENSE TYPE:		1 digit code
**				LICENSE DATE:		8 digit yyyymmdd  (default to current date)
**				EXPIRATION DATE:	8 digit yyyymmdd  (only for timed keys)
**
**	Input:		stdin
**			
**
**	Output:		stdout
**			logfile
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**			09/25/92	Added CLUSTER logic. GSL
**
*/

static	char	custname[256];
static	int4	custnum;
static	char	platform[3];
static	int	licensetype;
static	char	licensedate[20];
static	char	expdate[20];
static	char	licensekey[20];

static gen_key()
{

	char	buff[256];
	int	rc;
	char	*help;
	int	done;
	struct tm *l_tm;
	char	today[20];
	time_t	now;
	int	t_year, t_month, t_day;

	printf("\n\n>>> Generating a %s LICENSE KEY <<<\n\n",product_name());

	help = "Enter the customer's name for whom your generating this LICENSE KEY.";
	rc = prompt_text("CUSTOMER NAME",NULL,0,help,custname);
	if (rc == -1) exit(0);

	help = "Enter the customers account number. This is an up to 6 digit number.";
	rc = prompt_num("CUSTOMER NUMBER",NULL,help,&custnum);
	if (rc == -1) exit(0);

	help = "Enter the 2 digit PLATFORM code.\nPress <ENTER> for a list of valid values.";
	done = 0;
	while(!done)
	{
		rc = prompt_text("PLATFORM","Show values",1,help,buff);
		switch(rc)
		{
		case -1:
			exit(0);
			break;
		case 0:
			putplattab();						/* print a formated table of platform codes	*/
			break;
		case 1:
			if (plat_code(buff,NULL,NULL))				/* use plat_code to validate the code		*/
			{
				printf("\nInvalid PLATFORM code\n\n");
			}
			else
			{
				platform[0] = toupper(buff[0]);
				platform[1] = toupper(buff[1]);
				platform[2] = '\0';
				done = 1;
			}
		}
			
	}

	help = "Enter S for a SINGLE machine license.\n"
	       "Enter T for a TIMED license.\n"
	       "Enter N for a NETWORK license.\n"
	       "Enter C for a CLUSTER license.\n"
	       "Enter U for an UNLIMITED machine license.";
	
	rc = prompt_list("LICENSE TYPE","s","s,t,n,c,u",help);
	switch(rc)
	{
	case -1:
		exit(0);
		break;
	case 0:
	case 1:
		licensetype = LICENSE_SINGLE;
		break;
	case 2:
		licensetype = LICENSE_TIMED;
		break;
	case 3:
		licensetype = LICENSE_NETWORK;
		break;
	case 4:
		licensetype = LICENSE_CLUSTER;
		break;
	case 5:
		licensetype = LICENSE_UNLIMITED;
		break;
	}

	now = time(NULL);
	l_tm = localtime(&now);
	t_year  = l_tm->tm_year+1900;
	t_month = l_tm->tm_mon+1;
	t_day   = l_tm->tm_mday;
	sprintf(today,"%04d%02d%02d",t_year,t_month,t_day);

	help = "Enter the LICENSE DATE in YYYYMMDD format.";
	done = 0; 
	while(!done)
	{
		rc = prompt_text("LICENSE DATE",today,1,help,licensedate);
		if (rc == -1) exit(0);

		if (packdate(licensedate,buff))					/* Use packdate to validate the date		*/
		{
			printf("\nInvalid date.\n%s\n\n",help);
		}
		else
		{
			done = 1;
		}
	}

	if (licensetype == LICENSE_TIMED)					/* If a timed key then get expiration date	*/
	{
		t_month += 1;
		if (t_month == 13)
		{
			t_month = 1;
			t_year += 1;
		}
		sprintf(buff,"%04d%02d%02d",t_year,t_month,t_day);

		help = "Enter the EXPIRATION DATE in YYYYMMDD format.";
		done = 0; 
		while(!done)
		{
			rc = prompt_text("EXPIRATION DATE",buff,1,help,expdate);
			if (rc == -1) exit(0);

			if (packdate(expdate,buff))				/* Use packdate to validate the date		*/
			{
				printf("\nInvalid date.\n%s\n\n",help);
			}
			else
			{
				done = 1;
			}
		}
	}
	else
	{
		strcpy(expdate,"00000000");					/* No expiration				*/
	}

	printf("\n");
	putkeyinfo(custname,custnum,platform,licensetype,licensedate,expdate);
	printf("\n");
	printf("**** ENSURE THE ABOVE INFOMATION IS CORRECT BEFORE GENERATING THE KEY ****\n");
	printf("\n");

	help = "Enter Y to generate the LICENSE KEY with the above values.\nEnter N to abort.";
	rc = prompt_list("Generate the LICENSE KEY","y","y,n",help);
	switch(rc)
	{
	case -1:
		exit(0);
		break;
	case 2:
		return;
		break;
	}

	if (mklickey(custnum,platform,licensetype,licensedate,expdate,licensekey))
	{
		printf("\n**** ERROR ****\nUnable to generate key with given information\n\n");
		return;
	}

	formatkey(licensekey,buff);

	printf("\n");
	printf("                         =======================\n");
	printf("                        || %19s ||\n","");
	printf("LICENSE KEY             || %19s ||\n",buff);
	printf("                        || %19s ||\n","");
	printf("                         =======================\n");

	custnum = 0;								/* Reset all the values except the key		*/
	platform[0] = '\0';
	licensetype = 0;
	licensedate[0] = '\0';
	expdate[0] = '\0';

	if (bklickey(&custnum,platform,&licensetype,licensedate,expdate,licensekey))
	{
		printf("\n**** ERROR ****\nUnable to decode the above key DO NOT USE!!!\n\n");
		return;
	}

#ifdef OLD
	printf("\nThe above LICENSE KEY decodes to the following values:\n\n");
	putkeyinfo(NULL,custnum,platform,licensetype,licensedate,expdate);

	printf("\n");
	printf("**** ENSURE THIS INFOMATION IS CORRECT BEFORE ISSUEING THE KEY ****\n");
#endif
	logkeyinfo(custname,custnum,platform,licensetype,licensedate,expdate,licensekey);

	printkeydoc(custname,custnum,platform,licensetype,licensedate,expdate,licensekey,"___","___");
}


/*
**	Routine:	gen_val()
**
**	Function:	To generate a VALIDATION CODE.
**
**	Description:	This routine prompts for the infomation neccessary to generate a VALIDATION CODE.
**
**			For a VALIDATION CODE it will ask for the following info:
**				CUSTOMER NAME:		(comment)
**				LICENSE KEY:		19 digit key  (formated)
**				MACHINE ID:		up to 16 digit code
**
**	Input:		stdin
**			
**
**	Output:		stdout
**			logfile
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**
*/

static gen_val(int useglobals)
{
	char	machineid[80];
	char	valcode[20];

	char	buff[256];
	int	rc;
	char	*help;
	int	done;
	struct tm *l_tm;
	char	today[20];
	time_t	now;
	int	t_year, t_month, t_day;

	if (useglobals) goto get_machine_id;
	

	printf("\n\n>>> Generating a %s VALIDATION CODE <<<\n\n",product_name());

	help = "Enter the customer's name for whom your generating this VALIDATION CODE.";
	rc = prompt_text("CUSTOMER NAME",NULL,0,help,custname);
	if (rc == -1) exit(0);

	help = "Enter the LICENSE KEY.\n"
	       "This was sent to the customer with the kit.  If the customer doesn't \n"
	       "know his LICENSE KEY then you can look it up in the log book.";
	done = 0;
	while(!done)
	{
		rc = prompt_text("LICENSE KEY",NULL,0,help,buff);
		if (rc == -1) exit(0);

		upper_string(buff);
		unformatkey(licensekey,buff);

		if (bklickey(&custnum,platform,&licensetype,licensedate,expdate,licensekey))
		{
			printf("\nInvalid LICENSE KEY\n\n");
		}
		else
		{
			done = 1;
		}
	}
	
	printf("\nThis LICENSE KEY decodes to the following values:\n\n");

	putkeyinfo(NULL,custnum,platform,licensetype,licensedate,expdate);
	printf("\n");
	printf("**** ENSURE THIS INFOMATION IS CORRECT BEFORE CONTINUING ****\n");
	printf("\n");

	help = "Enter Y if the LICENSE KEY info is correct.\nEnter N if it is not correct.";
	rc = prompt_list("Is the information in this LICENSE KEY correct","y","y,n",help);
	switch(rc)
	{
	case -1:
		exit(0);
		break;
	case 2:
		printf("\n\nDo not use this LICENSE KEY to generate a VALIDATION CODE.\n");
		printf("Get the correct LICENSE KEY from the customer or issue a new LICENSE KEY.\n");
		printf("If this is the correct LICENSE KEY yet the information is incorrect then\n");
		printf("report this to the NeoMedia Customer Service Manager.\n\n");
		return;
		break;
	}

get_machine_id:
	printf("\n");
	help = "Enter the MACHINE ID. (Up to 16 digits)\n"
	       "The wlicense utility will display this for the customer.";
	rc = prompt_text("MACHINE ID",NULL,0,help,machineid);
	if (rc == -1) exit(0);

	formatkey(licensekey,buff);
	upper_string(machineid);						/* Shift to upper just in case there is alphas	*/

	printf("\n");
	printf("CUSTOMER NAME           %s\n",custname);
	printf("LICENSE KEY             %s\n",buff);
	printf("MACHINE ID              %s\n",machineid);
	printf("\n");
	printf("**** ENSURE THE ABOVE IS CORRECT BEFORE GENERATING THE VALIDATION CODE ****\n");
	printf("\n");

	help = "Enter Y to generate the VALIDATION CODE for the above values.\nEnter N to abort.";
	rc = prompt_list("Generate the VALIDATION CODE","y","y,n",help);
	switch(rc)
	{
	case -1:
		exit(0);
		break;
	case 2:
		return;
		break;
	}

	mkvalcode(licensekey,machineid,valcode);
	valcode[VALIDATION_CODE_SIZE] = '\0';

	printf("\n");
	printf("                         =======\n");
	printf("                        || %3s ||\n","");
	printf("VALIDATION CODE         || %3s ||\n",valcode);
	printf("                        || %3s ||\n","");
	printf("                         =======\n");

	logvalinfo(custname,custnum,platform,licensetype,licensedate,expdate,licensekey,machineid,valcode);

	printkeydoc(custname,custnum,platform,licensetype,licensedate,expdate,licensekey,machineid,valcode);
}

/*
**	Routine:	test_codes()
**
**	Function:	To test either a LICENSE KEY or a VALIDATION CODE.
**
**	Description:	This routine will decode a LICENSE KEY for the user and test if a VALIDATION CODE is correct.
**
**	Input:		stdin
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

static test_codes()
{
	int4	custnum;
	char	platform[3];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[20];
	char	machineid[80];
	char	valcode[20];

	char	buff[256];
	int	rc;
	char	*help;

	printf("\n\n>>> Testing %s LICENSE KEY and VALIDATION CODE <<<\n\n",product_name());

	help = "Enter the LICENSE KEY to decode.";
	rc = prompt_text("LICENSE KEY",NULL,0,help,buff);
	if (rc == -1) exit(0);

	upper_string(buff);
	unformatkey(licensekey,buff);

	if (bklickey(&custnum,platform,&licensetype,licensedate,expdate,licensekey))
	{
		printf("\nInvalid LICENSE KEY\n\n");
		return;
	}

	printf("\nThis LICENSE KEY decodes to the following values:\n\n");

	putkeyinfo(NULL,custnum,platform,licensetype,licensedate,expdate);

	printf("\n");
	help = "Enter Y to test the VALIDATION CODE for this LICENSE KEY.\nEnter N to continue.";
	rc = prompt_list("Test VALIDATION CODE for this LICENSE KEY","y","y,n",help);
	if (rc == -1) exit(0);
	if (rc == 2) return;

	printf("\n");
	help = "Enter the MACHINE ID. (Up to 16 digits)\nThe wlicense utility will display this for the customer.";
	rc = prompt_text("MACHINE ID",NULL,1,help,machineid);
	if (rc == -1) exit(0);
	if (rc == 0) return;
	upper_string(machineid);

	help = "Enter the VALIDATION CODE for the above LICENSE KEY.";
	rc = prompt_text("VALIDATION CODE",NULL,1,help,buff);
	if (rc == -1) exit(0);
	if (rc == 0) return;
	upper_string(buff);

	if (VALIDATION_CODE_SIZE != strlen(buff) ||
	    0 != ckvalcode(licensekey,machineid,buff))
	{
		printf("\nInvalid VALIDATION CODE\n");
	}
	else
	{
		printf("\nThis VALIDATION CODE will work with the given LICENSE KEY and MACHINE ID.\n");
	}
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
**	History:	05/21/92	Written by GSL
**
*/

static putheader()
{
/*		123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890				*/
	printf("\n");
	printf("                   **** %s LICENSE AUTHORIZATION TOOL ****\n",product_name());
	printf("         Copyright (c) 1992-1997 by NeoMedia Technologies Incorporated.\n");
	printf("\n");
	printf("This program will generate %s license keys and validation codes for use with\n",product_name());
	printf("the wlicense program.  This program is for use by NeoMedia personnel only!\n");
	printf("\n");
	printf("LICENSE KEY        The LICENSE KEY is the code that is sent out with every\n");
	printf("                   %s kit.  It defines the type of license and contains the\n",product_name());
	printf("                   encoded CUSTOMER NUMBER, PLATFORM, LICENSE TYPE, and the\n");
	printf("                   EXPIRATION DATE.\n");
	printf("\n");
	printf("VALIDATION CODE    The VALIDATION CODE is the code that \"locks\" the LICENSE\n");
	printf("                   KEY to the machine serial number (MACHINE ID).\n");
	printf("\n");
	printf("At any prompt you may type a question mark for help or a period to exit.\n");
	printf("                   ?   HELP on the prompt.\n");
	printf("                   .   EXIT the program.\n");
	printf("\n");
}


/*
**	Routine:	putkeyinfo()
**
**	Function:	To print the LICENSE KEY info.
**
**	Description:	All it does is printf calls to output the info to stdout.
**
**	Input:		custname	the customer name (or NULL)
**			custnum		the customer number
**			platform	the platform code
**			lictype		the license type
**			licdate		the license date
**			expdate		the expiration date
**
**	Output:		stdout
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**
*/

static putkeyinfo(custname,custnum,platform,lictype,licdate,expdate)
char	*custname;
int4	custnum;
char	platform[2];
int	lictype;
char	licdate[8];
char	expdate[8];
{
	char	platname[80];

	if (plat_code(platform,platname,NULL))					/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}

	if (custname)
	{
		printf(	"CUSTOMER NAME           %s\n",custname);
	}
	printf(		"CUSTOMER NUMBER         %06d\n",custnum);
	printf(		"PLATFORM                %2.2s - %s\n",platform,platname);
	printf(		"LICENSE TYPE            %s [%d]\n",lictypename(lictype),lictype);
	printf(		"LICENSE DATE            %8.8s\n",licdate);
	printf(		"EXPIRATION DATE         %8.8s\n",expdate);
}


/*
**	Routine:	logkeyinfo()
**
**	Function:	To write the LICENSE KEY info to the log file.
**
**	Description:	
**
**	Input:		custname		The customer name
**			custnum			The customer number
**			platform		The platform type (two character A-Z1-9)
**			lictype			The license type
**			licdate			The license date YYYYMMDD
**			expdate			The expiration date YYYYMMDD (No expiration == 00000000)
**			lickey			The license key
**			
**
**	Output:		log file.
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**
*/

static 	logkeyinfo(custname,custnum,platform,lictype,licdate,expdate,lickey)
char	*custname;
int4	custnum;
char	platform[2];
int	lictype;
char	licdate[8];
char	expdate[8];
char	lickey[LICENSE_KEY_SIZE];
{
	FILE	*fp;
	char	platname[80];
	time_t	now;
	char	buff[80];

	fp = fopen(authlogfile(),"a");
	if (!fp)
	{
		printf("\n*** ERROR ***\n");
		printf("Unable to open log file %s [errno=%d %s]\n\n",authlogfile(),errno,sys_errlist[errno]);
		return;
	}

	if (plat_code(platform,platname,NULL))					/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}
	formatkey(lickey,buff);							/* format the key				*/
	now = time(NULL);

	fprintf(fp,"*******************\n");
	fprintf(fp,"<LICENSE-KEY-ENTRY>\n");
	fprintf(fp,"OPERATOR           %s\n",cuserid(NULL));
	fprintf(fp,"TIME               %s",ctime(&now));			/* Note the newline is in the ctime() string	*/
	fprintf(fp,"CUSTOMER-NAME      %s\n",custname);
	fprintf(fp,"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,"PLATFORM           %2.2s - %s\n",platform,platname);
	fprintf(fp,"LICENSE-TYPE       %s [%d]\n",lictypename(lictype),lictype);
	fprintf(fp,"LICENSE-DATE       %8.8s\n",licdate);
	fprintf(fp,"EXPIRATION-DATE    %8.8s\n",expdate);
	fprintf(fp,"LICENSE-KEY        %s\n\n",buff);

	fclose(fp);
}


/*
**	Routine:	logvalinfo()
**
**	Function:	To write the VALIDATION CODE info to the log file.
**
**	Description:	
**
**	Input:		custname		The customer name
**			custnum			The customer number
**			lickey			The license key
**			machineid		The machine id
**			valcode			The validation code
**			
**
**	Output:		log file.
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/22/92	Written by GSL
**
*/

static logvalinfo(custname,custnum,platform,lictype,licdate,expdate,lickey,machineid,valcode)
char	*custname;
int4	custnum;
char	platform[2];
int	lictype;
char	licdate[8];
char	expdate[8];
char	lickey[LICENSE_KEY_SIZE];
char	*machineid;
char	*valcode;
{
	FILE	*fp;
	char	platname[80];
	time_t	now;
	char	buff[80];

	fp = fopen(authlogfile(),"a");
	if (!fp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open log file %s [errno=%d %s]\n\n",authlogfile(),errno,sys_errlist[errno]);
		return;
	}

	if (plat_code(platform,platname,NULL))					/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}
	formatkey(lickey,buff);							/* format the key				*/
	now = time(NULL);

	fprintf(fp,"*******************\n");
	fprintf(fp,"<VALIDATION-CODE-ENTRY>\n");
	fprintf(fp,"OPERATOR           %s\n",cuserid(NULL));
	fprintf(fp,"TIME               %s",ctime(&now));			/* Note the newline is in the ctime() string	*/
	fprintf(fp,"CUSTOMER-NAME      %s\n",custname);
	fprintf(fp,"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,"PLATFORM           %2.2s - %s\n",platform,platname);
	fprintf(fp,"LICENSE-TYPE       %s [%d]\n",lictypename(lictype),lictype);
	fprintf(fp,"LICENSE-DATE       %8.8s\n",licdate);
	fprintf(fp,"EXPIRATION-DATE    %8.8s\n",expdate);
	fprintf(fp,"LICENSE-KEY        %s\n",buff);
	fprintf(fp,"MACHINE-ID         %s\n",machineid);
	fprintf(fp,"VALIDATION-CODE    %s\n\n",valcode);

	fclose(fp);
}



/*
**	Routine:	printkeydoc()
**
**	Function:	To write and print the LICENSE KEY document that is sent to the user.
**
**	Description:	
**
**	Input:		custname		The customer name
**			custnum			The customer number
**			platform		The platform type (two character A-Z1-9)
**			lictype			The license type
**			licdate			The license date YYYYMMDD
**			expdate			The expiration date YYYYMMDD (No expiration == 00000000)
**			lickey			The license key
**
**			license.template (file)	The license key doc template file.			
**
**	Output:		docfile.
**			
**
**	Return:		None
**
**	Warnings:	If the file "license.template" is not found then an error will occur.
**			The sub-directory "doc" must exist.
**
**	History:	05/28/92	Written by GSL
**
*/

static 	void printkeydoc(char* custname, int4 custnum, char platform[2], int lictype, char licdate[8], char expdate[8],
			 char lickey[LICENSE_KEY_SIZE], char* machid, char* valcode)
{
	FILE	*tp, *fp;
	char	platname[80];
	char	docfile[80];
	char	custnum_s[10];
	char	platform_s[10];
	int	version;
	char	flickey[80];
	char	licdatebuff[20], expdatebuff[20];
	char	buff[256];

	sprintf(custnum_s,"%06d",custnum);					/* string out the customer number		*/

	formatkey(lickey,flickey);						/* format the key				*/

	sprintf(platform_s,"%2.2s",platform);					/* string out the platform code			*/

	if (plat_code(platform,platname,NULL))					/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}

	sprintf(licdatebuff,"%4.4s/%2.2s/%2.2s",&licdate[0],&licdate[4],&licdate[6]);	/* format the date			*/

	if (0 == memcmp(expdate,"00000000",8))					/* format the date				*/
	{
		strcpy(expdatebuff,"None");
	}
	else
	{
		sprintf(expdatebuff,"%4.4s/%2.2s/%2.2s",&expdate[0],&expdate[4],&expdate[6]);
	}

	tp = fopen(DOCTEMPLATE,"r");						/* open the template file			*/
	if (!tp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open template file %s [errno=%d %s]\n\n",DOCTEMPLATE,errno,sys_errlist[errno]);
		return;
	}

	for(version=1;;version++)						/* find an unused version number		*/
	{
		sprintf(docfile,"doc/%s.%s%d",custnum_s,platform_s,version);
		if ( 0 != access(docfile,0000) ) break;
	}

	fp = fopen(docfile,"w");						/* open the document file			*/
	if (!fp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open doc file %s [errno=%d %s]\n\n",docfile,errno,sys_errlist[errno]);
		fclose(tp);
		return;
	}

	while(fgets(buff,sizeof(buff),tp))					/* read the template & do substitution		*/
	{
		stredt(buff,"<LICENSEE>",custname);
		stredt(buff,"<CUSTNUM>",custnum_s);
		stredt(buff,"<LICENSEKEY>",flickey);
		stredt(buff,"<PLATFORM>",platname);
		stredt(buff,"<LICENSETYPE>",lictypename(lictype));
		stredt(buff,"<LICENSEDATE>",licdatebuff);
		stredt(buff,"<EXPIRATION>",expdatebuff);
		stredt(buff,"<MACHINEID>",machid);
		stredt(buff,"<VALCODE>",valcode);

		fprintf(fp,"%s",buff);						/* write out the edited line			*/
	}

	fclose(fp);								/* close both files				*/
	fclose(tp);

	printf("\n**** Printing license doc file %s ****\n\n",docfile);		/* print the file				*/
	sprintf(buff, "./PRINTLICENSE %s", docfile);
	system(buff);
}

/*
**	DUMMY routines to prevent the whole WISPLIB from being included
*/
wexit(int code)
{
	exit(code);
}
werrlog()
{
}

/*
**	History:
**	$Log: wauthorize.c,v $
**	Revision 1.9  1997-03-17 11:09:14-05  gsl
**	Add support for NETWORK license for WISP/NT
**	Change to NeoMedia Technologies
**	Streamline how the program works.
**	Add option to generate a validation code using the information just
**	entered for a license key so the user does not have to reenter the info.
**
**	Revision 1.8  1996-07-23 14:13:04-04  gsl
**	drcs update
**
**	05/22/92	Written by GSL
**	05/26/92	Moved putplattab() to platsubs.c to isolate platform_table GSL.
**	09/25/92	Added CLUSTER support. GSL
**	09/13/93	Generalize for UniQue. GSL
**
**
*/
