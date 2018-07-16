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
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>
#ifdef unix
#include <grp.h>
#endif
#ifdef WIN32
#include <io.h>
#endif

#include "wlicense.h"
#include "prompt.h"
#include "platsubs.h"
#include "idsisubs.h"
#include "wisplib.h"

extern char	*sys_errlist[];

#define DOCTEMPLATE	"license.template"

static int g_bCGI = 0;	/* Flag, are we running in a CGI ? */
static const char *g_cgi_query_string = NULL;

static void gen_key(void);
static void gen_val(int useglobals);
static void test_codes(void);
static void putheader(void);
static void putkeyinfo(const char *custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8]);
static void logkeyinfo(const char *custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       const char lickey[LICENSE_KEY_SIZE]);
static void logvalinfo(const char* custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       const char lickey[LICENSE_KEY_SIZE],
		       const char* machineid,
		       const char* valcode);
static 	void printkeydoc(const char* custname, 
			 int4 custnum, 
			 const char platform[2], 
			 int lictype, 
			 const char licdate[8], 
			 const char expdate[8],
			 const char lickey[LICENSE_KEY_SIZE], 
			 const char* machid, 
			 const char* valcode);
static void logtabfile(const char* r_type,		/* "VALCODE" or "LICKEY" */
		       const char* f_time,
		       const char* f_custname,
		       int4 custnum,
		       const char* f_platform,
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       const char* f_lickey,
		       const char* f_machid,
		       const char* f_valcode);
static void clean_text(char *text);

static int isCgi(void);
static int cgi_main(void);

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

int main(int argc, char* argv[])
{
	char	*help;
	int	rc;
	int	done;

	if (isCgi())
	{
		return cgi_main();
	}
	

	putheader();

#ifdef unix
	if (1)
	{
		struct group *grp;
		char	groupname[80];

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
	}
#endif /* unix */

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
	return 0;
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

static void gen_key(void)
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
	clean_text(custname);

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

static void gen_val(int useglobals)
{
	char	machineid[80];
	char	valcode[20];

	char	buff[256];
	int	rc;
	char	*help;
	int	done;

	if (useglobals) goto get_machine_id;
	

	printf("\n\n>>> Generating a %s VALIDATION CODE <<<\n\n",product_name());

	help = "Enter the customer's name for whom your generating this VALIDATION CODE.";
	rc = prompt_text("CUSTOMER NAME",NULL,0,help,custname);
	if (rc == -1) exit(0);
	clean_text(custname);

	help = "Enter the LICENSE KEY.\n"
	       "This was sent to the customer with the kit.  If the customer doesn't \n"
	       "know his LICENSE KEY then you can look it up in the log book.";
	done = 0;
	while(!done)
	{
		rc = prompt_text("LICENSE KEY",NULL,0,help,buff);
		if (rc == -1) exit(0);

		clean_text(buff);
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
	clean_text(machineid);

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

static void test_codes(void)
{
	int4	custnum;
	char	platform[3];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[20];
	char	machineid[80];

	char	buff[256];
	int	rc;
	char	*help;

	printf("\n\n>>> Testing %s LICENSE KEY and VALIDATION CODE <<<\n\n",product_name());

	help = "Enter the LICENSE KEY to decode.";
	rc = prompt_text("LICENSE KEY",NULL,0,help,buff);
	if (rc == -1) exit(0);

	clean_text(buff);
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
	clean_text(machineid);
	upper_string(machineid);

	help = "Enter the VALIDATION CODE for the above LICENSE KEY.";
	rc = prompt_text("VALIDATION CODE",NULL,1,help,buff);
	if (rc == -1) exit(0);
	if (rc == 0) return;
	clean_text(buff);
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

static void putheader(void)
{
/*		123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890				*/
	printf("\n");
	printf("                   **** %s LICENSE AUTHORIZATION TOOL ****\n",product_name());
	printf("         Copyright (c) 1992-1999 by NeoMedia Technologies Incorporated.\n");
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

static void putkeyinfo(const char *custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8])
{
	char	platname[80];

	if (plat_code((char*)platform,platname,NULL))				/* expand the platform code			*/
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
static void formatPlatform(const char pPlatform[2], char* pFPlatform)
{
	char	szPlatName[80];

	if (plat_code((char*)pPlatform,szPlatName,NULL))	/* expand the platform code			*/
	{
		strcpy(szPlatName,"??");
	}
	sprintf(pFPlatform,"%2.2s - %s", pPlatform, szPlatName);
}

static void logkeyinfo(const char *custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       const char lickey[LICENSE_KEY_SIZE])
{
	FILE	*fp;
	char	f_platform[80];
	time_t	now;
	char	f_lickey[80];
	char	f_time[40];

	fp = fopen(authlogfile(),"a");
	if (!fp)
	{
		printf("\n*** ERROR ***\n");
		printf("Unable to open log file %s [errno=%d %s]\n\n",authlogfile(),errno,sys_errlist[errno]);
		return;
	}

	formatPlatform(platform,f_platform);

	formatkey(lickey,f_lickey);						/* format the key				*/
	now = time(NULL);
	strcpy(f_time,ctime(&now));
	f_time[strlen(f_time)-1] = '\0'; /* Remove newline */

	fprintf(fp,"*******************\n");
	fprintf(fp,"<LICENSE-KEY-ENTRY>\n");
	fprintf(fp,"OPERATOR           %s\n",cuserid(NULL));
	fprintf(fp,"TIME               %s\n",f_time);
	fprintf(fp,"CUSTOMER-NAME      %s\n",custname);
	fprintf(fp,"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,"PLATFORM           %s\n",f_platform);
	fprintf(fp,"LICENSE-TYPE       %s [%d]\n",lictypename(lictype),lictype);
	fprintf(fp,"LICENSE-DATE       %8.8s\n",licdate);
	fprintf(fp,"EXPIRATION-DATE    %8.8s\n",expdate);
	fprintf(fp,"LICENSE-KEY        %s\n\n",f_lickey);

	fclose(fp);

	logtabfile("LICKEY", f_time, custname, custnum, f_platform, lictype, licdate, expdate, f_lickey,"","");

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

static void logvalinfo(const char* custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       const char lickey[LICENSE_KEY_SIZE],
		       const char* machineid,
		       const char* valcode)
{
	FILE	*fp;
	char	f_platform[80];
	time_t	now;
	char	f_lickey[80];
	char	f_time[40];

	fp = fopen(authlogfile(),"a");
	if (!fp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open log file %s [errno=%d %s]\n\n",authlogfile(),errno,sys_errlist[errno]);
		return;
	}

	formatPlatform(platform,f_platform);
	formatkey(lickey,f_lickey);						/* format the key				*/
	now = time(NULL);
	strcpy(f_time,ctime(&now));
	f_time[strlen(f_time)-1] = '\0'; /* Remove newline */

	fprintf(fp,"*******************\n");
	fprintf(fp,"<VALIDATION-CODE-ENTRY>\n");
	fprintf(fp,"OPERATOR           %s\n",cuserid(NULL));
	fprintf(fp,"TIME               %s\n",f_time);
	fprintf(fp,"CUSTOMER-NAME      %s\n",custname);
	fprintf(fp,"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,"PLATFORM           %s\n",f_platform);
	fprintf(fp,"LICENSE-TYPE       %s [%d]\n",lictypename(lictype),lictype);
	fprintf(fp,"LICENSE-DATE       %8.8s\n",licdate);
	fprintf(fp,"EXPIRATION-DATE    %8.8s\n",expdate);
	fprintf(fp,"LICENSE-KEY        %s\n",f_lickey);
	fprintf(fp,"MACHINE-ID         %s\n",machineid);
	fprintf(fp,"VALIDATION-CODE    %s\n\n",valcode);

	fclose(fp);

	logtabfile("VALCODE", f_time, custname, custnum, f_platform, lictype, licdate, expdate, f_lickey,
		   machineid, valcode);

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

static 	void printkeydoc(const char* custname, 
			 int4 custnum, 
			 const char platform[2], 
			 int lictype, 
			 const char licdate[8], 
			 const char expdate[8],
			 const char lickey[LICENSE_KEY_SIZE], 
			 const char* machid, 
			 const char* valcode)
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

	if (plat_code((char*)platform,platname,NULL))				/* expand the platform code			*/
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

static const char* tabfilepath()
{
	static int first=1;
	static char	tabfile[256];

	/*
	**	Create tab file name by replacing .log with .tab
	*/
	if (first)
	{
		first = 0;

		strcpy(tabfile,authlogfile());
		tabfile[strlen(tabfile)-4] = '\0';
		strcat(tabfile,".tab");
	}

	return tabfile;
}


static void logtabfile(const char* r_type,		/* "VALCODE" or "LICKEY" */
		       const char* f_time,
		       const char* f_custname,
		       int4 custnum,
		       const char *f_platform,
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       const char* f_lickey,
		       const char* f_machid,
		       const char* f_valcode)
{
	FILE	*fp;
	
	fp = fopen(tabfilepath(),"a");
	if (!fp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open tab file %s [errno=%d %s]\n\n",tabfilepath(),errno,sys_errlist[errno]);
		return;
	}

	fprintf(fp, "%s\t%s\t%s\t%s\t%06d\t%s\t%s [%d]\t%8.8s\t%8.8s\t%s\t%s\t%s\n",
			       r_type,
			       cuserid(NULL),
			       f_time,
			       f_custname,
			       custnum,
			       f_platform,
			       lictypename(lictype), lictype,
			       licdate,
			       expdate,
			       f_lickey,
			       f_machid,
			       f_valcode);
	fclose(fp);
}

static int readtabfile(FILE* fp, 
		       char *pType,
		       char *pUserId,
		       char *pTime,
		       char *pCustName,
		       char *pCustNum,
		       char *pPlatform,
		       char *pLicType,
		       char *pLicDate,
		       char *pExpDate,
		       char *pLicKey,
		       char *pMachId,
		       char *pValCode)
{
#define NUMARGS 12
	char buff[1024];
	char *args[NUMARGS];
	int i;
	char *bptr, *eptr;
	

	args[0] = pType;
	args[1] = pUserId;
	args[2] = pTime;
	args[3] = pCustName;
	args[4] = pCustNum;
	args[5] = pPlatform;
	args[6] = pLicType;
	args[7] = pLicDate;
	args[8] = pExpDate;
	args[9] = pLicKey;
	args[10] = pMachId;
	args[11] = pValCode;

	if (NULL == fgets(buff, sizeof(buff), fp))
	{
		return 1;
	}
	buff[strlen(buff)-1] = '\0';

	bptr = buff;
	for(i=0; i<NUMARGS; i++)
	{
		if (eptr = strchr(bptr,'\t'))
		{
			*eptr = '\0';
		}
		strcpy(args[i],bptr);

		if (NULL==eptr)
		{
			break;
		}
		
		bptr = eptr+1;
	}
	
	return 0;
}


static void clean_text(char *text)
{
	char *ptr;
	int len;
	
	/* Replace tabs by a space char */
	while(ptr = strchr(text,'\t'))
	{
		*ptr = ' ';
	}

	/* Remove trailing spaces */
	len = strlen(text);
	while(len > 0 && ' ' == text[len-1])
	{
		len--;
		text[len] = '\0';
	}
}

static const char* tmpupper(const char* instr)
{
	static char *tbuf = NULL;
	if (tbuf)
	{
		free(tbuf);
		tbuf = NULL;
	}
	tbuf = strdup(instr);
	upper_string(tbuf);
	return tbuf;
}

static char hex2char(const char *hex)
{
	char *mask = "0123456789ABCDEF";
	char *h1, *h2;
	
	h1 = strchr(mask, toupper(hex[0]));
	h2 = strchr(mask, toupper(hex[1]));
	if (NULL==h1 || NULL==h2)
	{
		return '?';
	}
	return (char)((h1-mask)*16 + (h2-mask));
}

static void cgi_process_arg(char *the_arg)
{
	char *pS, *pD;
	
	pS = the_arg;
	pD = the_arg;
	for(;;)
	{
		switch(*pS)
		{
		case '\0':
			*pD = '\0';
			return;
			
		case '+':
			*pD = ' ';
			break;
		case '%':
			pS++;
			*pD = hex2char(pS);
			pS++;
			break;
		default:
			*pD = *pS;
			break;
		}
		pD++;
		pS++;
	}
	
}

/* cname=maxwell&cnum=&lickey=&platform=SC&machid=&cmd=Search */

static char* get_cgi_arg(const char* arg)
{
	char *qstring = strdup(g_cgi_query_string);
	char *ptr;
	char *value = NULL;
	char *cmparg = NULL;

	cmparg = (char*)malloc(strlen(arg)+2);
	sprintf(cmparg,"%s=",arg);
	upper_string(cmparg);
    
	ptr = strtok(qstring,"&");
	while(NULL!=ptr)
	{
		if (0==memcmp(cmparg, tmpupper(ptr), strlen(cmparg)))
		{
			value = strdup(ptr + strlen(cmparg));
			cgi_process_arg(value);
			break;
		}
		ptr = strtok(NULL,"&");
	}

	ptr = NULL;
	free(qstring);
	free(cmparg);
	return value;
}

static void cgi_error(const char* pszTitle, const char* pszErrMess)
{
	printf("Content-type: text/html\n");
	printf("\n");
	printf("<html>\n");
	printf("<head>\n");
	printf("<title>%s</title>\n",pszTitle);
	printf("</head>\n");
	printf("<body>\n");

	printf("<hr width=80%% align=center size=10 color=grey>\n");
	printf("<h2 align=\"center\">WISP LICENSING - ERROR</h2>\n");
	printf("<hr width=80%% align=center size=10 color=grey>\n");

	printf("<p>&nbsp</p>\n");
	printf("<h3 align=center>%s</h3>\n",pszTitle);
	printf("<p>&nbsp</p>\n");

	printf("<hr width=80%% align=center size=20 color=red>\n");
	printf("<p align=center><big><big><em>%s</em></big></big></p>\n",pszErrMess);
	printf("<hr width=80%% align=center size=20 color=red>\n");

	printf("<p>&nbsp</p>\n");
	printf("<p align=center>Use the browser Back button to return.</p>\n");


	printf("</body>\n");
	printf("</html>\n");
}


static int cgi_search(void)
{
	FILE 	*fp;
	char 	szType[80],
		szUserId[80],
		szTime[80],
		szCustName[80],
		szCustNum[80],
		szPlatform[80],
		szLicType[80],
		szLicDate[80],
		szExpDate[80],
		szLicKey[80],
		szMachId[80],
		szValCode[80];
	const char *cptr;
	char	*pScriptName = NULL;
	char	*pTstCname, *pTstCnum, *pTstLicKey, *pTstPlatform, *pTstMachId;
	int	rownum;
	
	if (NULL==(pTstCname = get_cgi_arg("CNAME")))
	{
		pTstCname = "";
	}
	if (NULL==(pTstCnum = get_cgi_arg("CNUM")))
	{
		pTstCnum = "";
	}
	if (NULL==(pTstLicKey = get_cgi_arg("LICKEY")))
	{
		pTstLicKey = "";
	}
	if (NULL==(pTstPlatform = get_cgi_arg("PLATFORM")))
	{
		pTstPlatform = "";
	}
	else if ('*' == pTstPlatform[0])
	{
		pTstPlatform[0] = '\0';
	}
	if (NULL==(pTstMachId = get_cgi_arg("MACHID")))
	{
		pTstMachId = "";
	}
	upper_string(pTstCname);
	upper_string(pTstCnum);
	upper_string(pTstLicKey);
	upper_string(pTstPlatform);
	upper_string(pTstMachId);

	if (cptr = getenv("SCRIPT_NAME"))
	{
		pScriptName = strdup(cptr);
	}
	else
	{
		pScriptName = "wauthorize.exe";
	}
	
	
	printf("Content-type: text/html\n");
	printf("\n");
	printf("<html>\n");
	printf("<head>\n");
	printf("<title>WISP License Lookup - Results</title>\n");
	printf("</head>\n");

	printf("<body>\n");
	printf("<h2 align=\"center\">WISP LICENSE LOOKUP - RESULTS</h2>\n");

	printf("<h3 align=\"center\">\n");
	if (strlen(pTstCname))
	{
		printf("Customer Name = %s<br>\n",pTstCname);
	}
	if (strlen(pTstCnum))
	{
		printf("Customer Number = %s<br>\n",pTstCnum);
	}
	if (strlen(pTstLicKey))
	{
		printf("License Key = %s<br>\n",pTstLicKey);
	}
	if (strlen(pTstPlatform))
	{
		printf("Platform = %s<br>\n",pTstPlatform);
	}
	if (strlen(pTstMachId))
	{
		printf("Machine Id = %s<br>\n",pTstMachId);
	}
	printf("</h3>\n");
	

	fp = fopen(tabfilepath(),"r");
	if (!fp)
	{
		printf("<p>Unable to open tab file %s\n",tabfilepath());
		goto endbody;
	}

	printf("<table border cols=8>\n");

	printf("<tr>\n");
	printf("<th>ROW</th>\n");
	printf("<th width=\"20%%\">CUSTNAME</th>\n");
	printf("<th>CUSTNUM</th>\n");
	printf("<th>PLATFORM</th>\n");
	printf("<th>LICTYPE</th>\n");
	printf("<th nowrap>LICENSEKEY</th>\n");
	printf("<th>MACHID</th>\n");
	printf("<th>VALCODE</th>\n");
	printf("</tr>\n");

	rownum = 0;
	
	while( 0==readtabfile(fp, 
			      szType,
			      szUserId,
			      szTime,
			      szCustName,
			      szCustNum,
			      szPlatform,
			      szLicType,
			      szLicDate,
			      szExpDate,
			      szLicKey,
			      szMachId,
			      szValCode))
	{
		rownum++;
		
		if ((NULL!=strstr(tmpupper(szCustName),pTstCname)) &&
		    (NULL!=strstr(tmpupper(szCustNum),pTstCnum)) &&
		    (NULL!=strstr(tmpupper(szLicKey),pTstLicKey)) &&
		    (0==memcmp(tmpupper(szPlatform),pTstPlatform,strlen(pTstPlatform))) &&
		    (NULL!=strstr(tmpupper(szMachId),pTstMachId))
			)
		{
			printf("<tr>\n");

			printf("<td><a href=%s?CMD=SHOW&ROW=%d>%d</a></td>\n",pScriptName, rownum, rownum);
			printf("<td>%s</td>\n",szCustName);
			if (szCustNum[0])
			{
				printf("<td><a href=%s?CMD=SEARCH&CNUM=%s>%s</a></td>\n",pScriptName, szCustNum, szCustNum);
			}
			else
			{
				printf("<td>&nbsp</td>\n");
			}
			printf("<td>%s</td>\n",szPlatform[0] ? &szPlatform[5] : "&nbsp");
			if (szLicType[0])
			{
				char *ptr;
				
				if (ptr = strchr(szLicType,' '))
				{
					*ptr = '\0';
				}
				
				printf("<td>%s</td>\n",szLicType);
			}
			else
			{
				printf("<td>&nbsp</td>\n");
			}
			
			printf("<td nowrap><a href=%s?CMD=SEARCH&LICKEY=%s><kbd>%s</kbd></a></td>\n",pScriptName, szLicKey, szLicKey);
			printf("<td>%s</td>\n",szMachId[0] ? szMachId : "&nbsp");
			printf("<td>%s</td>\n",szValCode[0] ? szValCode : "&nbsp");
		
			printf("</tr>\n");
		}
	}
	printf("</table>\n");
	
	fclose(fp);

  endbody:
	printf("</body>\n");
	printf("</html>\n");
    
	return 0;
}

static void cgi_display_license_sheet(
	char* pszCustName,
	char* pszCustNum,
	char* pszPlatform,
	char* pszLicType,
	char* pszLicDate,
	char* pszExpDate,
	char* pszLicKey,
	char* pszMachId,
	char* pszValCode)
{
	printf("Content-type: text/html\n");
	printf("\n");
	printf("<html>\n");
	printf("<head>\n");
	printf("<title>WISP Runtime License Key</title>\n");
	printf("</head>\n");

	printf("<body>\n");

	/*
	 * Display the record.
	 */
	printf("<h3 align=center>NeoMedia Technologies Inc.</h3>\n");
	printf("<p>&nbsp</p>\n");

	printf("<hr width=40%% align=center>\n");
	printf("<h2 align=center>WISP<br>Runtime License Key</h2>\n");
	printf("<hr width=40%% align=center>\n");

	printf("<p>&nbsp</p>\n");
	printf("<p align=center>This is an important document,<br>please keep it in a safe location.</p>\n");
	printf("<p>&nbsp</p>\n");

	printf("<hr width=40%% align=center>\n");	
	printf("<p>&nbsp</p>\n");
	
	printf("<table cols=2 align=center cellpadding=5>\n");

	printf("<tr>\n");
	printf("<td align=right>Licensee</td>\n");
	printf("<td align=left>%s</td>\n",pszCustName);
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>Customer Number</td>\n");
	printf("<td align=left>%s</td>\n",pszCustNum);
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>License Key</td>\n");
	printf("<td align=left>%s</td>\n",pszLicKey);
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>Platform</td>\n");
	printf("<td align=left>%s</td>\n",pszPlatform);
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>License Type</td>\n");
	printf("<td align=left>%s</td>\n",pszLicType);
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>License Date</td>\n");
	printf("<td align=left>%4.4s/%2.2s/%2.2s</td>\n",pszLicDate, &pszLicDate[4], &pszLicDate[6]);
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>Expiration Date</td>\n");
	if (0==strcmp(pszExpDate,"00000000") || 0!=memcmp(pszLicType,"TIMED",5))
	{
		printf("<td align=left>None</td>\n");
	}
	else
	{
		printf("<td align=left>%4.4s/%2.2s/%2.2s</td>\n",pszExpDate, &pszExpDate[4], &pszExpDate[6]);
	}
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>Machine Id</td>\n");
	printf("<td align=left>%s</td>\n",pszMachId);
	printf("</tr>\n");

	printf("<tr>\n");
	printf("<td align=right>Validation Code</td>\n");
	printf("<td align=left>%s</td>\n",pszValCode);
	printf("</tr>\n");
	
	printf("</table>\n");

	printf("<p>&nbsp</p>\n");
	printf("<hr width=40%% align=center>\n");
	printf("<p>&nbsp</p>\n");

	printf("<table align=center width=80%%>\n");
	printf("To license a machine for the WISP runtime run the program \"wlicense\" "
	       "and enter the LICENSE KEY as written above. "
	       "<br><br>"
	       "If you have any questions, please contact NeoMedia Technical support "
	       "at (941)&nbsp337-3434.");
	printf("</table>");

	printf("</body>\n");
	printf("</html>\n");

}

static int cgi_show(void)
{
	char *pRow;
	int nRow, rownum;
	FILE *fp;
	char 	szType[80],
		szUserId[80],
		szTime[80],
		szCustName[80],
		szCustNum[80],
		szPlatform[80],
		szLicType[80],
		szLicDate[80],
		szExpDate[80],
		szLicKey[80],
		szMachId[80],
		szValCode[80];
	char 	errmess[256];
	

	if (NULL==(pRow = get_cgi_arg("ROW")))
	{
		pRow = "";
	}
	if (1 != sscanf(pRow,"%d",&nRow))
	{
		sprintf(errmess, "Unable to determine ROW to display.");
		goto displayerr;
	}
	
	fp = fopen(tabfilepath(),"r");
	if (!fp)
	{
		sprintf(errmess, "Unable to open tab file %s",tabfilepath());
		goto displayerr;
	}

	rownum = 0;
	
	while( 0==readtabfile(fp, 
			      szType,
			      szUserId,
			      szTime,
			      szCustName,
			      szCustNum,
			      szPlatform,
			      szLicType,
			      szLicDate,
			      szExpDate,
			      szLicKey,
			      szMachId,
			      szValCode))
	{
		rownum++;
		if (rownum >= nRow)
		{
			break;
		}
	}

	fclose(fp);
	
	if (rownum == nRow)
	{
		cgi_display_license_sheet(
			szCustName,
			szCustNum,
			szPlatform,
			szLicType,
			szLicDate,
			szExpDate,
			szLicKey,
			szMachId,
			szValCode);
	

		return 0;
	}
	else
	{
		sprintf(errmess, "ROW=%d not found.", nRow);
		goto displayerr;
	}

  displayerr:

	cgi_error("WISP Runtime License Key", errmess);

	return 1;
}

static int cgi_genkey(void)
{
	char 	*pszCustName,
		*pszCustNum,
		*pszLicType,
		*pszPlatform;
	int4 	nCustNum;
	int 	nLicType;
	char    szPlatform[80];
	char    szFormatPlatform[80];
	char    szLicKey[20];
	char    szFormatLicKey[20];
	char	szFormatCustNum[10];
	char 	errmess[256];
	struct tm *ptmNow;
	time_t	timeNow;
	int	t_year, t_month, t_day;
	char	szLicDate[20], szExpDate[20];

	/*
	 * Initialization
	 */
	timeNow = time(NULL);
	ptmNow = localtime(&timeNow);
	t_year  = ptmNow->tm_year+1900;
	t_month = ptmNow->tm_mon+1;
	t_day   = ptmNow->tm_mday;
	sprintf(szLicDate,"%04d%02d%02d",t_year,t_month,t_day);
	strcpy(szExpDate,"00000000");
	
	/*
	 * Get the arguments
	 */
	pszCustName = get_cgi_arg("CNAME");
	pszCustNum = get_cgi_arg("CNUM");
	pszLicType = get_cgi_arg("LICTYPE");
	pszPlatform = get_cgi_arg("PLATFORM");
	
	if (NULL==pszCustName || '\0' == *pszCustName)
	{
		strcpy(errmess,"Customer Name must be supplied.");
		goto displayerr;
	}
	if (NULL==pszCustNum || '\0' == *pszCustNum)
	{
		strcpy(errmess,"Customer Number must be supplied.");
		goto displayerr;
	}
	if (NULL==pszLicType || '\0' == *pszLicType)
	{
		strcpy(errmess,"License type must be supplied.");
		goto displayerr;
	}
	if (NULL==pszPlatform || '\0' == *pszPlatform)
	{
		strcpy(errmess,"Platform must be supplied.");
		goto displayerr;
	}

	if (1 != sscanf(pszCustNum, "%u", &nCustNum))
	{
		strcpy(errmess,"Customer number is invalid.");
		goto displayerr;
	}

	if (2 != strlen(pszPlatform))
	{
		strcpy(errmess,"Platform is invalid.");
		goto displayerr;
	}

	upper_string(pszLicType);
	switch(pszLicType[0])
	{
	case 'S':
		nLicType = LICENSE_SINGLE;
		break;
		
	case 'N':
		nLicType = LICENSE_NETWORK;
		break;

	case 'C':
		nLicType = LICENSE_CLUSTER;
		break;

	case 'D': /* D14 D30 D60 D90 D180 */
		nLicType = LICENSE_TIMED;
		if (0==strcmp(pszLicType,"D14"))
		{
			t_day += 14;
		}
		else if (0==strcmp(pszLicType,"D30"))
		{
			t_month += 1;
		}
		else if (0==strcmp(pszLicType,"D60"))
		{
			t_month += 2;
		}
		else if (0==strcmp(pszLicType,"D90"))
		{
			t_month += 3;
		}
		else if (0==strcmp(pszLicType,"D180"))
		{
			t_month += 6;
		}
		else
		{
			sprintf(errmess,"License type [%s] is invalid.", pszLicType);
			goto displayerr;
		}

		while(t_day > 28)
		{
			t_day -= 28;
			t_month += 1;
		}
		while(t_month > 12)
		{
			t_month -= 12;
			t_year += 1;
		}

		sprintf(szExpDate,"%04d%02d%02d",t_year,t_month,t_day);
		break;

	default:
		sprintf(errmess,"License type [%s] is invalid.", pszLicType);
		goto displayerr;
	}
	

	if (mklickey(nCustNum,pszPlatform,nLicType,szLicDate,szExpDate,szLicKey))
	{
		strcpy(errmess, "Unable to generate key with given information");
		goto displayerr;
	}

	/* Reset all the values except the key		*/
	nCustNum = 0;
	szPlatform[0] = '\0';
	nLicType = 0;
	szLicDate[0] = '\0';
	szExpDate[0] = '\0';

	if (bklickey(&nCustNum,szPlatform,&nLicType,szLicDate,szExpDate,szLicKey))
	{
		strcpy(errmess, "Unable to decode the generated key");
		goto displayerr;
	}

	logkeyinfo(pszCustName,nCustNum,szPlatform,nLicType,szLicDate,szExpDate,szLicKey);

	formatkey(szLicKey,szFormatLicKey);
	sprintf(szFormatCustNum, "%06d", nCustNum);
	formatPlatform(szPlatform,szFormatPlatform);
	
	cgi_display_license_sheet(
			pszCustName,
			szFormatCustNum,
			szFormatPlatform,
			lictypename(nLicType),
			szLicDate,
			szExpDate,
			szFormatLicKey,
			"",
			"");

	return 0;

  displayerr:

	cgi_error("WISP Runtime License Key", errmess);

	return 1;
}


static int isCgi(void)
{
	if (NULL != (g_cgi_query_string = getenv("QUERY_STRING")))
	{
		g_bCGI = 1;
	}
	return g_bCGI;
}

/*
  TODO
  - GENVAL
 */
static int cgi_main(void)
{
	char *pszArgCmd = NULL;

	if (NULL == (pszArgCmd = get_cgi_arg("CMD")))
	{
		pszArgCmd = "(nil)";
	}
	else
	{
		upper_string(pszArgCmd);
	}
	
	if (0==strcmp(pszArgCmd,"SEARCH"))	/* CMD=SEARCH&CNAME=x&CNUM=x&LICKEY=x&PLATFORM=xx&MACHID=x */
	{
		return cgi_search();
	}
	if (0==strcmp(pszArgCmd,"SHOW"))	/* CMD=SHOW&ROW=n */
	{
		return cgi_show();
	}
	if (0==strcmp(pszArgCmd,"GENKEY"))	/* CMD=GENKEY&CNAME=x&CNUM=x&LICTYPE=x&PLATFORM=xx */
	{
		return cgi_genkey(); 
	}
	if (0==strcmp(pszArgCmd,"GENVAL"))
	{
/*		return cgi_genval(); */
	}
	
    
	printf("Content-type: text/html\n");
	printf("\n");
	printf("<html>\n");
	printf("<head>\n");
	printf("<title>wauthorize results</title>\n");
	printf("</head>\n");

	printf("<body>\n");
	printf("<p>QUERY_STRING=%s\n",g_cgi_query_string);
	printf("<p>CMD=%s\n",pszArgCmd);
	printf("</body>\n");
	printf("</html>\n");
    
	return 0;
}


/*
**	DUMMY routines to prevent the whole WISPLIB from being included
*/
#ifdef OLD
wexit(int code)
{
	exit(code);
    return 0;
}
werrlog()
{
    return 0;
}
#endif
#include "wutils.h"

/*
**	History:
**	$Log: wauthorize.c,v $
**	Revision 1.11  1999-05-20 09:42:37-04  gsl
**	Add the first pass of CGI enabling.
**	This is a work in progress and is not complete.
**
**	Revision 1.10  1998-12-18 14:35:33-05  gsl
**	Added logging to the wauthorize.tab file
**
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
