/*
******************************************************************************
** Copyright (c) Shell Stream Software, LLC. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software, LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission is strictly prohibited.
******************************************************************************
*/

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
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
#include <ctype.h>

#ifdef unix
#include <grp.h>
#include <unistd.h>
#endif
#ifdef WIN32
#include <io.h>
#endif

#include "wcommon.h"
#include "wlicense.h"
#include "prompt.h"
#include "platsubs.h"
#include "wplatdef.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wanguid.h"
#include "machid.h"

#if defined(unix) && !defined(LINUX)
extern char	*sys_errlist[];
#endif

#define DOCTEMPLATE	"license.template"


struct license_s
{
	char	custname[256];
	int4	custnum;
	char	platform[20];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[80];		/* UN-formated license key */
	int4	version_number;
	char	machineid[80];
	char	valcode[80];
};

struct licensetable_s
{
	struct licensetable_s *next;
	int  row;
	char *pType;
	char *pUserId;
	char *pTime;
	char *pCustName;
	char *pCustNum;
	char *pPlatform;
	char *pLicType;
	char *pLicDate;
	char *pExpDate;
	char *pLicKey;
	char *pMachId;
	char *pValCode;
	char *pVersionNum;
};

static void freetabtable(struct licensetable_s *first);
static struct licensetable_s *loadtabtable(void);

static int g_bCGI = 0;	/* Flag, are we running in a CGI ? */
static const char *g_cgi_query_string = NULL;

static const char *operatorId();

static void init_license(struct license_s *l);
static void search_license(struct license_s *l);
static int gen_key(struct license_s *l);
static void gen_val(struct license_s *l);
static void gen_machine_id(struct license_s *l);

static void test_codes(void);
static void putheader(void);
static void putkeyinfo(const char *custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       int4 version_number);
static void logkeyinfo(const char *custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       int4 version_number,
		       const char lickey[LICENSE_KEY_SIZE]);
static void logvalinfo(const char* custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       int4 version_number,
		       const char lickey[LICENSE_KEY_SIZE],
		       const char* machineid,
		       const char* valcode);
static 	void printkeydoc(const char* custname, 
			 int4 custnum, 
			 const char platform[2], 
			 int lictype, 
			 const char licdate[8], 
			 const char expdate[8],
		         int4 version_number,
			 const char lickey[LICENSE_KEY_SIZE], 
			 const char* appcode, 
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
		       const char* f_valcode,
		       int4 version_number);
static void clean_text(char *text);

static int WLIC_mklickey(
		int4	custnum,
		char	platform[2],
		int	lictype,
		char	licdate[8],
		char	expdate[8],
		int4	version_number,
		char	lickey[LICENSE_KEY_SIZE]);
static void WLIC_mkvalcode(char lickey[LICENSE_KEY_SIZE],char* machineid,char valcode[3]);

static char *WLIC_authlogfile();

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
		       char *pValCode,
		       char *pVersionNum);


static int isCgi(void);
static int cgi_main(void);

static int stredt(char* src, const char* srch, const char* repl)
{
	int i;
	char tstring[512];
	char *sptr;

	if ( (i = strpos(src,srch)) != -1)						/* It's ok to go ahead			*/
	{
		src += i;								/* point to location of search string	*/
		i = strlen(srch);
		sptr = src + i;								/* Skip over the search string . . .	*/
		strcpy(tstring,sptr);							/* copy end of line into temp string	*/
		strcpy(src,repl);							/* put replacement string in place	*/
		strcat(src,tstring);							/* put end line back into source	*/
		i = 0;									/* edit was ok				*/
	}
	return(i);									/* return the value 0 or -1		*/
}

static void init_license(struct license_s *l)
{
	memset(l, 0, sizeof(struct license_s));
	l->licensetype = 0;
	l->version_number = 0;
}

/*
**	Routine:	main()		[xauthorize]
**				wauthorize	WISP
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
*/

int main(int argc, char* argv[])
{
	char	*help;
	int	rc;
	int	done;
	struct license_s the_license;

	init_license(&the_license);
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
			if (0 != strcmp(groupname,"GSL") &&
			    0 != strcmp(groupname,"SHELLSTREAM"))
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
		printf("%s AUTHORIZE UTILITY\n",WLIC_product_name());
		printf("===============================================================================\n");
		printf("\n");
		
		help =	"Enter K to generate a LICENSE KEY.\n"
			"Enter V to generate a VALIDATION CODE.\n"
			"Enter T to TEST either.\n"
			"Enter S to SEARCH database.\n"
			"Enter M to generate a Machine Id from a host name.\n"
			"Enter Q to Quit.";
		
		rc = prompt_list("Generate a Key, Validation, Test, Search, MachineId or Quit",NULL,"k,v,t,s,m,q",help);
		switch(rc)
		{
		case PROMPT_RC_EXIT:
			exit(0);
			break;
		case 1:		/* LICENSE KEY */
			init_license(&the_license);
			if (0 == gen_key(&the_license))
			{
				printf("\n");
				help = "Enter Y to generate a VALIDATION CODE using the the LICENSE KEY information\n"
					"you just entered.\n"
					"Enter N to continue.\n"
					"Enter . (period) to Exit";
				rc = prompt_list("Generate a VALIDATION CODE for this LICENSE KEY","n","y,n",help);
				if (rc == PROMPT_RC_EXIT) exit(0);
				if (rc == 1)
				{
					for(;;)
					{
						gen_val(&the_license);
						printf("\n");
						help = "Enter Y to generate another VALIDATION CODE for this LICENSE KEY.\nEnter N to continue.";
						rc = prompt_list("Generate another VALIDATION CODE for this LICENSE KEY","n","y,n",help);
						if (rc == PROMPT_RC_EXIT) 
						{
							exit(0);
						}
						if (rc != 1) 
						{
							break;
						}
					}
				}
			}
			break;
		case 2:		/* VALIDATION CODE */
			done=0;
			while(!done)
			{
				gen_val(&the_license);		/* Generate the VALIDATION CODE			*/
				printf("\n");
				help = "Enter Y to generate another VALIDATION CODE for this LICENSE KEY.\nEnter N to continue.";
				rc = prompt_list("Generate another VALIDATION CODE for this LICENSE KEY","n","y,n",help);
				if (rc == PROMPT_RC_EXIT) 
				{
					exit(0);
				}
				if (rc != 1) 
				{
					done = 1;
				}
			}
			break;
		case 3:		/* TEST */
			done=0;
			while(!done)
			{
				test_codes();					/* TEST keys or validation codes		*/
				printf("\n");
				help = "Enter Y to TEST another LICENSE KEY and VALIDATION CODE.\nEnter N to continue.";
				rc = prompt_list("Test another Key and Validation","n","y,n",help);
				if (rc == PROMPT_RC_EXIT) exit(0);
				if (rc != 1) done = 1;
			}
			break;
			
		case 4:		/* SEARCH */
			search_license(&the_license);
			break;
		case 5:		/* MACHINE ID */
			gen_machine_id(&the_license);
			break;
		default:
			return 0;
			
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
**	Return:		
**	0		Successfully Generated Key
**	1		Key not generated
**	-1		Error 
**
**	Warnings:	None
**
*/

static int gen_key(struct license_s *l)
{

	char	buff[256];
	char	flickey[80];	/* Formated license key */
	int	rc;
	char	*help;
	int	done;
	struct tm *l_tm;
	char	today[20];
	time_t	now;
	int	t_year, t_month, t_day;

	printf("\n\n>>> Generating a %s LICENSE KEY <<<\n\n",WLIC_product_name());

	help = "Enter the customer's name for whom your generating this LICENSE KEY.";
	rc = prompt_text("CUSTOMER NAME",NULL,PROMPT_EMPTY_NOTALLOWED,help,l->custname);
	if (rc == PROMPT_RC_EXIT) exit(0);
	clean_text(l->custname);

	help = "Enter the customers account number. This is an up to 6 digit number.";
	rc = prompt_num("CUSTOMER NUMBER",NULL,help,&l->custnum);
	if (rc == PROMPT_RC_EXIT) exit(0);

	help = "Enter the 2 digit PLATFORM code.\nPress <ENTER> for a list of valid values.";
	done = 0;
	while(!done)
	{
		rc = prompt_text("PLATFORM","Show values",PROMPT_EMPTY_ALLOWED,help,buff);
		switch(rc)
		{
		case PROMPT_RC_EXIT:
			exit(0);
			break;
		case PROMPT_RC_DEFAULT:
			WL_putplattab();					/* print a formated table of platform codes	*/
			break;
		case PROMPT_RC_USER_VALUE:
			if (WL_plat_code(buff,NULL,NULL))			/* use plat_code to validate the code		*/
			{
				printf("\nInvalid PLATFORM code\n\n");
			}
			else
			{
				l->platform[0] = toupper(buff[0]);
				l->platform[1] = toupper(buff[1]);
				l->platform[2] = '\0';
				done = 1;
			}
		}
			
	}

	help =  "Select the License Type:\n"
		"t - TIMED license      - Demo license\n"
		"s - SINGLE license     - Standard UNIX license\n"
		"n - NETWORK license    - Standard WINDOWS license\n"
		"e - ENTERPRISE license - UNLIMITED with Version restrictions\n"
		"c - CLUSTER license    - Rare special case\n"
		"u - UNLIMITED license  - Obsolete\n";
	
	printf("\n%s",help);
	rc = prompt_list("LICENSE TYPE","t","t,s,n,e,c,u",help);
	switch(rc)
	{
	case PROMPT_RC_EXIT:
		exit(0);
		break;
	case PROMPT_RC_DEFAULT:
	case 1:
		l->licensetype = LICENSE_TIMED;
		break;
	case 2:
		l->licensetype = LICENSE_SINGLE;
		break;
	case 3:
		l->licensetype = LICENSE_NETWORK;
		break;
	case 4:
		l->licensetype = LICENSE_ENTERPRISE;
		break;
	case 5:
		l->licensetype = LICENSE_CLUSTER;
		break;
	case 6:
		l->licensetype = LICENSE_UNLIMITED;
		break;
	}

	/*
	**	Valid dates are between 19900101 and 20241231
	*/

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
		rc = prompt_text("LICENSE DATE",today,PROMPT_EMPTY_ALLOWED,help,l->licensedate);
		if (rc == PROMPT_RC_EXIT) exit(0);

		if (WLIC_packdate(l->licensedate,buff))				/* Use packdate to validate the date		*/
		{
			printf("\nInvalid date.\n%s\n\n",help);
		}
		else
		{
			done = 1;
		}
	}

	if (l->licensetype == LICENSE_TIMED)					/* If a timed key then get expiration date	*/
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
			rc = prompt_text("EXPIRATION DATE",buff,PROMPT_EMPTY_ALLOWED,help,l->expdate);
			if (rc == PROMPT_RC_EXIT) exit(0);

			if (WLIC_packdate(l->expdate,buff))			/* Use packdate to validate the date		*/
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
		strcpy(l->expdate,"00000000");					/* No expiration				*/
	}

#define MIN_VERSION_NUMBER	40
	l->version_number = 0;
	if (l->licensetype == LICENSE_ENTERPRISE)
	{
		/*
		**	Use the expiration date to hold the version number
		*/
		
		help =  "Enter the 2 digit version number\n" \
			"Example: WISP version 4.4 is entered as '44'.";
		done = 0; 
		while(!done)
		{
			rc = prompt_num("VERSION NUMBER (2 digits)",
				NULL,  help, &l->version_number);
			if (rc == PROMPT_RC_EXIT) exit(0);

			if (l->version_number >= MIN_VERSION_NUMBER && l->version_number <= 99)
			{
				done = 1;
			}
			else
			{
				printf("\nInvalid VERSION NUMBER.\n%s\n\n",help);
			}
		}
	}

	printf("\n");
	putkeyinfo(l->custname,l->custnum,l->platform,l->licensetype,l->licensedate,l->expdate,l->version_number);
	printf("\n");
	printf("**** ENSURE THE ABOVE INFOMATION IS CORRECT BEFORE GENERATING THE KEY ****\n");
	printf("\n");

	help = "Enter Y to generate the LICENSE KEY with the above values.\nEnter N to abort.";
	rc = prompt_list("Generate the LICENSE KEY","y","y,n",help);
	switch(rc)
	{
	case PROMPT_RC_EXIT:
		exit(0);
		break;
	case 2:
		return 1;
		break;
	}

	if (WLIC_mklickey(l->custnum,l->platform,l->licensetype,l->licensedate,l->expdate,l->version_number,l->licensekey))
	{
		printf("\n**** ERROR ****\nUnable to generate key with given information\n\n");
		return -1;
	}
	l->licensekey[LICENSE_KEY_SIZE] = '\0';

	WLIC_formatkey(l->licensekey,flickey);

	printf("\n");
	printf("                         =======================\n");
	printf("                        || %19s ||\n","");
	printf("LICENSE KEY             || %19s ||\n",flickey);
	printf("                        || %19s ||\n","");
	printf("                         =======================\n");

	l->custnum = 0;								/* Reset all the values except the key		*/
	l->platform[0] = '\0';
	l->licensetype = 0;
	l->licensedate[0] = '\0';
	l->expdate[0] = '\0';

	if (WLIC_bklickey(&l->custnum,l->platform,&l->licensetype,l->licensedate,l->expdate,&l->version_number,l->licensekey))
	{
		printf("\n**** ERROR ****\nUnable to decode the above key DO NOT USE!!!\n\n");
		return -1;
	}
	l->licensekey[LICENSE_KEY_SIZE] = '\0';
	logkeyinfo(l->custname,l->custnum,l->platform,l->licensetype,l->licensedate,l->expdate,
		l->version_number,l->licensekey);


	{
		char	*appcode = "None";
		char	*machid  = "___";
		char	*valcode = "___";

		if (l->licensetype == LICENSE_ENTERPRISE)
		{
			appcode = "___";
		}
		if (l->licensetype == LICENSE_TIMED)
		{
			machid  = "None";
			valcode = "None";
		}

		printkeydoc(l->custname,l->custnum,l->platform,l->licensetype,l->licensedate,l->expdate,
			l->version_number, l->licensekey, appcode, machid, valcode);
	}
	return 0;
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
*/

static void gen_val(struct license_s *l)
{
	char	appcode[80] = { "" };

	char	buff[256];
	int	rc;
	char	*help;
	int	done;
	char	def[200];	
	char	flickey[80];

	printf("\n\n>>> Generating a %s VALIDATION CODE <<<\n\n",WLIC_product_name());

	strcpy(def, l->custname);
	help = "Enter the customer's name for whom your generating this VALIDATION CODE.";
	rc = prompt_text("CUSTOMER NAME",def,PROMPT_EMPTY_NOTALLOWED,help,l->custname);
	if (rc == PROMPT_RC_EXIT) exit(0);
	clean_text(l->custname);

	if (l->licensekey[0])
	{
		WLIC_formatkey(l->licensekey,def);
	}
	else
	{
		def[0] = '\0';
	}
	help = "Enter the LICENSE KEY.\n"
	       "This was sent to the customer with the kit.  If the customer doesn't \n"
	       "know his LICENSE KEY then you can look it up in the log book.";
	done = 0;
	while(!done)
	{
		char unflickey[80];
		rc = prompt_text("LICENSE KEY",def,PROMPT_EMPTY_NOTALLOWED,help,buff);
		if (rc == PROMPT_RC_EXIT) exit(0);

		clean_text(buff);
		upper_string(buff);
		WLIC_unformatkey(unflickey,buff);
		unflickey[LICENSE_KEY_SIZE] = '\0';

		if (WLIC_bklickey(&l->custnum,l->platform,&l->licensetype,l->licensedate,l->expdate,
			&l->version_number,unflickey))
		{
			printf("\nInvalid LICENSE KEY\n\n");
		}
		else
		{
			strcpy(l->licensekey,unflickey);
			done = 1;
		}
	}

	WLIC_formatkey(l->licensekey,flickey);
	
	printf("\nThis LICENSE KEY decodes to the following values:\n\n");

	putkeyinfo(l->custname,l->custnum,l->platform,l->licensetype,l->licensedate,l->expdate,l->version_number);
	printf("\n");
	printf("**** ENSURE THIS INFOMATION IS CORRECT BEFORE CONTINUING ****\n");
	printf("\n");

	help = "Enter Y if the LICENSE KEY info is correct.\nEnter N if it is not correct.";
	rc = prompt_list("Is the information in this LICENSE KEY correct","y","y,n",help);
	switch(rc)
	{
	case PROMPT_RC_EXIT:
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

	l->machineid[0] = '\0';
	l->valcode[0] = '\0';

	if (l->licensetype == LICENSE_ENTERPRISE)
	{
		help = 
		"An ENTERPRISE license uses an APPLICATION CODE instead of a MACHINE ID.\n"
		"The APPLICATION CODE is 'made-up' based on either the Customer's name or \n"
		"their application name. It must be 6-20 chars [A-Z,0-9,'-'].";

		done = 0;
		while(!done)
		{
			int len;
			printf("\n%s\n\n", help);
			rc = prompt_text("APPLICATION CODE",NULL,PROMPT_EMPTY_NOTALLOWED,
				help,l->machineid);
			if (rc == PROMPT_RC_EXIT) exit(0);
			printf("\n");

			len = strlen(l->machineid);
			if(len < 6 || len > 20)
			{
				printf("Invalid length of APPLICATION CODE\n");
			}
			else
			{
				int i;
				done = 1;
				upper_string(l->machineid);
				for(i=0; i<len; i++)
				{
					if (!isalnum((int)(l->machineid[i])) && '-' != l->machineid[i])
					{
						done = 0;
						printf("Invalid characters in APPLICATION CODE\n");
						break;
					}
				}
			}

		}
	}
	else
	{
		printf("\n");
		help = "Enter the MACHINE ID. \n"
		       "The wlicense utility will display this for the customer.";
		rc = prompt_text("MACHINE ID",NULL,PROMPT_EMPTY_NOTALLOWED,help,l->machineid);
	}
	if (rc == PROMPT_RC_EXIT) exit(0);
	clean_text(l->machineid);

	upper_string(l->machineid);						/* Shift to upper just in case there is alphas	*/

	printf("\n");
	printf("CUSTOMER NAME           %s\n",l->custname);
	printf("LICENSE KEY             %s\n",flickey);
	if (l->licensetype == LICENSE_ENTERPRISE)
	{
		printf("APPLICATION CODE        %s\n",l->machineid);
	}
	else
	{
		printf("MACHINE ID              %s\n",l->machineid);
	}
	printf("\n");
	printf("**** ENSURE THE ABOVE IS CORRECT BEFORE GENERATING THE VALIDATION CODE ****\n");
	printf("\n");

	help = "Enter Y to generate the VALIDATION CODE for the above values.\nEnter N to abort.";
	rc = prompt_list("Generate the VALIDATION CODE","y","y,n",help);
	switch(rc)
	{
	case PROMPT_RC_EXIT:
		exit(0);
		break;
	case 2:
		return;
		break;
	}

	WLIC_mkvalcode(l->licensekey,l->machineid,l->valcode);
	l->valcode[VALIDATION_CODE_SIZE] = '\0';

	printf("\n");
	printf("                         =======\n");
	printf("                        || %3s ||\n","");
	printf("VALIDATION CODE         || %3s ||\n",l->valcode);
	printf("                        || %3s ||\n","");
	printf("                         =======\n");

	logvalinfo(l->custname,l->custnum,l->platform,l->licensetype,l->licensedate,l->expdate,
		l->version_number,l->licensekey,l->machineid,l->valcode);


	if (l->licensetype == LICENSE_ENTERPRISE)
	{
		strcpy(appcode, l->machineid);
		strcpy(l->machineid, "None");
	}
	else
	{
		strcpy(appcode, "None");
	}

	printkeydoc(l->custname,l->custnum,l->platform,l->licensetype,l->licensedate,l->expdate,
		l->version_number,l->licensekey, appcode, l->machineid, l->valcode);
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
	int4	version_number = 0;

	char	buff[256];
	int	rc;
	char	*help;

	printf("\n\n>>> Testing %s LICENSE KEY and VALIDATION CODE <<<\n\n",WLIC_product_name());

	help = "Enter the LICENSE KEY to decode.";
	rc = prompt_text("LICENSE KEY",NULL,PROMPT_EMPTY_NOTALLOWED,help,buff);
	if (rc == PROMPT_RC_EXIT) exit(0);

	clean_text(buff);
	upper_string(buff);
	WLIC_unformatkey(licensekey,buff);

	if (WLIC_bklickey(&custnum,platform,&licensetype,licensedate,expdate,
		&version_number,licensekey))
	{
		printf("\nInvalid LICENSE KEY\n\n");
		return;
	}

	printf("\nThis LICENSE KEY decodes to the following values:\n\n");

	putkeyinfo(NULL,custnum,platform,licensetype,licensedate,expdate, version_number);

	printf("\n");
	help = "Enter Y to test the VALIDATION CODE for this LICENSE KEY.\nEnter N to continue.";
	rc = prompt_list("Test VALIDATION CODE for this LICENSE KEY","y","y,n",help);
	if (rc == PROMPT_RC_EXIT) exit(0);
	if (rc == 2) return;

	printf("\n");
	help = "Enter the MACHINE ID. (Up to 16 digits)\nThe wlicense utility will display this for the customer.";
	rc = prompt_text("MACHINE ID",NULL,PROMPT_EMPTY_NOTALLOWED,help,machineid);
	if (rc == PROMPT_RC_EXIT) exit(0);
	if (rc == PROMPT_RC_DEFAULT) return;
	clean_text(machineid);
	upper_string(machineid);

	help = "Enter the VALIDATION CODE for the above LICENSE KEY.";
	rc = prompt_text("VALIDATION CODE",NULL,PROMPT_EMPTY_NOTALLOWED,help,buff);
	if (rc == PROMPT_RC_EXIT) exit(0);
	if (rc == PROMPT_RC_DEFAULT) return;
	clean_text(buff);
	upper_string(buff);

	if (VALIDATION_CODE_SIZE != strlen(buff) ||
	    0 != WLIC_ckvalcode(licensekey,machineid,buff))
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
	printf("                   **** %s LICENSE AUTHORIZATION TOOL ****\n",WLIC_product_name());
	printf("         Copyright (c) 1994-" WISP_COPYRIGHT_YEAR_STR " by Shell Stream Software, LLC.\n");
	printf("\n");
	printf("This program will generate %s license keys and validation codes for use with\n",WLIC_product_name());
	printf("the wlicense program.  This program is for use by Shell Stream personnel only!\n");
	printf("\n");
	printf("LICENSE KEY        The LICENSE KEY is the code that is sent out with every\n");
	printf("                   %s kit.  It defines the type of license and contains the\n",WLIC_product_name());
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
		       const char expdate[8],
		       int4 version_number)
{
	char	platname[80];

	if (WL_plat_code((char*)platform,platname,NULL))			/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}

	if (custname)
	{
		printf(	"CUSTOMER NAME           %s\n",custname);
	}
	printf(		"CUSTOMER NUMBER         %06d\n",custnum);
	printf(		"PLATFORM                %2.2s - %s\n",platform,platname);
	printf(		"LICENSE TYPE            %s [%d]\n",WLIC_lictypename(lictype),lictype);
	printf(		"LICENSE DATE            %8.8s\n",licdate);
	printf(		"EXPIRATION DATE         %8.8s\n",expdate);
	if (version_number != 0)
	{
		printf(	"VERSION NUMBER          %d.%d\n",version_number/10, version_number%10);
	}
}


static void formatPlatform(const char pPlatform[2], char* pFPlatform)
{
	char	szPlatName[80];

	if (WL_plat_code((char*)pPlatform,szPlatName,NULL))	/* expand the platform code			*/
	{
		strcpy(szPlatName,"??");
	}
	sprintf(pFPlatform,"%2.2s - %s", pPlatform, szPlatName);
}

/*
**	Format a timestamp: 
**	YYYY-MM-DD HH:MM:SS
*/
static void formatTimeStamp(char* f_time)
{
	time_t	now;
	struct tm* now_time;

	now = time(NULL);
	now_time = localtime(&now);

	sprintf(f_time,"%04d-%02d-%02d %02d:%02d:%02d",
		now_time->tm_year + 1900,
		now_time->tm_mon + 1,
		now_time->tm_mday,
		now_time->tm_hour,
		now_time->tm_min,
		now_time->tm_sec);
}
/*
**	Format a now Date: 
**	YYYY-MM-DD HH:MM:SS
*/
static void formatNowDate(char* f_now)
{
	time_t	now;
	struct tm* now_time;

	now = time(NULL);
	now_time = localtime(&now);

	sprintf(f_now,"%04d-%02d-%02d",
		now_time->tm_year + 1900,
		now_time->tm_mon + 1,
		now_time->tm_mday);
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
**			version_number		2 digit version
**			lickey			The license key
**			
**
**	Output:		log file.
**			
**
**	Return:		None
**
*/
static void logkeyinfo(const char *custname,
		       int4 custnum,
		       const char platform[2],
		       int lictype,
		       const char licdate[8],
		       const char expdate[8],
		       int4 version_number,
		       const char lickey[LICENSE_KEY_SIZE])
{
	FILE	*fp;
	char	f_platform[80];
	char	f_lickey[80];
	char	f_time[40];

	fp = fopen(WLIC_authlogfile(),"a");
	if (!fp)
	{
		printf("\n*** ERROR ***\n");
		printf("Unable to open log file %s [errno=%d %s]\n\n",WLIC_authlogfile(),errno,sys_errlist[errno]);
		return;
	}

	formatPlatform(platform,f_platform);

	WLIC_formatkey(lickey,f_lickey);						/* format the key				*/
	formatTimeStamp(f_time);

	fprintf(fp,"*******************\n");
	fprintf(fp,"<LICENSE-KEY-ENTRY>\n");
	fprintf(fp,"OPERATOR           %s\n",operatorId());
	fprintf(fp,"TIME               %s\n",f_time);
	fprintf(fp,"CUSTOMER-NAME      %s\n",custname);
	fprintf(fp,"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,"PLATFORM           %s\n",f_platform);
	fprintf(fp,"LICENSE-TYPE       %s [%d]\n",WLIC_lictypename(lictype),lictype);
	fprintf(fp,"LICENSE-DATE       %8.8s\n",licdate);
	fprintf(fp,"EXPIRATION-DATE    %8.8s\n",expdate);
	fprintf(fp,"VERSION-NUMBER     %d\n",version_number);
	fprintf(fp,"LICENSE-KEY        %s\n\n",f_lickey);

	fclose(fp);

	logtabfile("LICKEY", f_time, custname, custnum, f_platform, lictype, licdate, 
		expdate, f_lickey, "", "", version_number);

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
		       int4 version_number,
		       const char lickey[LICENSE_KEY_SIZE],
		       const char* machineid,
		       const char* valcode)
{
	FILE	*fp;
	char	f_platform[80];
	char	f_lickey[80];
	char	f_time[40];

	fp = fopen(WLIC_authlogfile(),"a");
	if (!fp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open log file %s [errno=%d %s]\n\n",WLIC_authlogfile(),errno,sys_errlist[errno]);
		return;
	}

	formatPlatform(platform,f_platform);
	WLIC_formatkey(lickey,f_lickey);						/* format the key				*/
	formatTimeStamp(f_time);

	fprintf(fp,"*******************\n");
	fprintf(fp,"<VALIDATION-CODE-ENTRY>\n");
	fprintf(fp,"OPERATOR           %s\n",operatorId());
	fprintf(fp,"TIME               %s\n",f_time);
	fprintf(fp,"CUSTOMER-NAME      %s\n",custname);
	fprintf(fp,"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,"PLATFORM           %s\n",f_platform);
	fprintf(fp,"LICENSE-TYPE       %s [%d]\n",WLIC_lictypename(lictype),lictype);
	fprintf(fp,"LICENSE-DATE       %8.8s\n",licdate);
	fprintf(fp,"EXPIRATION-DATE    %8.8s\n",expdate);
	fprintf(fp,"VERSION-NUMBER     %d\n",version_number);
	fprintf(fp,"LICENSE-KEY        %s\n",f_lickey);
	fprintf(fp,"MACHINE-ID         %s\n",machineid);
	fprintf(fp,"VALIDATION-CODE    %s\n\n",valcode);

	fclose(fp);

	logtabfile("VALCODE", f_time, custname, custnum, f_platform, lictype, licdate, expdate, 
		   f_lickey, machineid, valcode, version_number);

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
**			version_number		2 digit version
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
*/

static 	void printkeydoc(const char* custname, 
			 int4 custnum, 
			 const char platform[2], 
			 int lictype, 
			 const char licdate[8], 
			 const char expdate[8],
			 int4 version_number,
			 const char lickey[LICENSE_KEY_SIZE], 
			 const char* appcode, 
			 const char* machid, 
			 const char* valcode)
{
	FILE	*tp, *fp;
	char	platname[80];
	char	platform_id[80];
	char	docfile[80];
	char	custnum_s[10];
	char	platform_s[10];
	char	flickey[80];
	char	licdatebuff[20], expdatebuff[20];
	char	versionbuff[10];
	char	buff[256];
	char	issuedDate[40];

	formatNowDate(issuedDate);

	sprintf(custnum_s,"%06d",custnum);					/* string out the customer number		*/

	WLIC_formatkey(lickey,flickey);						/* format the key				*/

	sprintf(platform_s,"%2.2s",platform);					/* string out the platform code			*/

	if (WL_plat_code((char*)platform,platname,NULL))			/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}
	sprintf(platform_id,"%s - %s", platform_s, platname);

	sprintf(licdatebuff,"%4.4s-%2.2s-%2.2s",&licdate[0],&licdate[4],&licdate[6]);	/* format the date			*/

	strcpy(expdatebuff,"None");
	if (lictype == LICENSE_TIMED)						/* format the date				*/
	{
		sprintf(expdatebuff,"%4.4s-%2.2s-%2.2s",&expdate[0],&expdate[4],&expdate[6]);
	}

	strcpy(versionbuff,"None");
	if (version_number != 0)
	{
		sprintf(versionbuff,"%d.%d",version_number/10, version_number%10);
	}

	tp = fopen(DOCTEMPLATE,"r");						/* open the template file			*/
	if (!tp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open template file %s [errno=%d %s]\n\n",DOCTEMPLATE,errno,sys_errlist[errno]);
		return;
	}

	{
		int	seq;
		for(seq=1;;seq++)						/* find an unused sequence number		*/
		{
			sprintf(docfile,"doc/%s.%s%d",custnum_s,platform_s,seq);
			if ( 0 != access(docfile,0000) ) break;
		}
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
		stredt(buff,"[LICENSEE]",custname);
		stredt(buff,"[CUSTNUM]",custnum_s);
		stredt(buff,"[LICENSEKEY]",flickey);
		stredt(buff,"[PLATFORM]",platform_id);
		stredt(buff,"[LICENSETYPE]",WLIC_lictypename(lictype));
		stredt(buff,"[LICENSEDATE]",licdatebuff);
		stredt(buff,"[EXPIRATION]",expdatebuff);
		stredt(buff,"[ISSUEDDATE]",issuedDate);
		stredt(buff,"[VERSION]",versionbuff);
		stredt(buff,"[MACHINEID]",machid);
		stredt(buff,"[APPCODE]",appcode);
		stredt(buff,"[VALCODE]",valcode);

		fprintf(fp,"%s",buff);						/* write out the edited line			*/
	}

	fclose(fp);								/* close both files				*/
	fclose(tp);

	chmod(docfile, 0644);

	printf("\n**** License doc file %s ****\n\n",docfile);
#ifdef unix
	sprintf(buff, "./PRINTLICENSE %s", docfile);
#endif
#ifdef WIN32
	sprintf(buff, ".\\PRINTLICENSE.bat %s", docfile);
#endif
	system(buff);
}

static const char *operatorId()
{
#ifdef unix
	return cuserid(NULL);
#endif
#ifdef WIN32
	return WL_longuid();
#endif
}


/*
**	Routine:	WLIC_mklickey()		Make License Key
**
**	Function:	To create the license key from it's parts.
**
**	Description:	The parts of the key are first encoded one byte at a time 
**			with WLIC_entran() to make a 15 byte key
**			then a checksum is added to make the 16th byte.
**			
**			lickey		6	custnum	
**					2	platform
**					1	license type
**					3	license date
**					3	expriration date
**					1	checksum
**
**	Input:		custnum			The customer number
**			platform		The platform type (two character A-Z1-9)
**			lictype			The license type
**			licdate			The license date YYYYMMDD
**			expdate			The expiration date YYYYMMDD (No expiration == 00000000)
**			version_number		2 digit number
**
**	Output:		lickey			The license key
**
**	Return:		0 = Success
**			1 = Invalid key component
**
*/

static int WLIC_mklickey(
		int4	custnum,
		char	platform[2],
		int	lictype,
		char	licdate[8],
		char	expdate[8],
		int4	version_number,
		char	lickey[LICENSE_KEY_SIZE])
{
	char	buff[20];
	int	i, idx;
	int	rc;
	int	checksum;

	/*
	**	First load lickey[] with numbers 0-34 
	*/

	/*
	**	6 byte customer number (offset 0-5)
	*/
	sprintf(buff,"%06d",custnum);
	for(i=0,idx=0;i<6;i++,idx++)
	{
		lickey[idx] = buff[i] - '0';
	}

	/*
	**	2 byte platform	(offset 6,7)
	**
	**	the platform is already in the format of A-Z1-9
	*/
	lickey[idx++] = WLIC_detran((char)toupper(platform[0]));      	       	/* Detran the platfrom code, it will be		*/
	lickey[idx++] = WLIC_detran((char)toupper(platform[1]));	       	/* validated later.				*/

	/*
	**	1 byte license type (offset 8)
	*/
	lickey[idx++] = lictype;

	/*
	**	3 byte license date (offset 9-11)
	**
	**	Valid dates are between 19900101 and 20241231
	*/
	if ((rc = WLIC_packdate(licdate,&lickey[idx]))) return(rc);
	idx += 3;

	/*
	**	3 byte expire date (offset 12-14)
	*/
	if (lictype == LICENSE_TIMED)
	{
		if ((rc = WLIC_packdate(expdate,&lickey[idx]))) return(rc);
	}
	else if (lictype == LICENSE_ENTERPRISE)
	{
		lickey[idx+0] = version_number / 10;
		lickey[idx+1] = version_number % 10;
		lickey[idx+2] = 0;
	}
	else
	{
		memset(&lickey[idx],'\0',3);
	}

	/*
	**	1 byte checksum (offset 15)
	*/
	checksum = WLIC_checksummem(lickey,LICENSE_KEY_SIZE-1,WLIC_MAXTRAN);
	lickey[LICENSE_KEY_SIZE-1] = checksum;

	/*
	**	Do a rolling-up with offset encryption.  (Don't do the checksum)
	*/
	for(i=0;i<LICENSE_KEY_SIZE-1;i++)
	{
		if (lickey[i] < 0 || lickey[i] > WLIC_MAXTRAN) return(1);
		lickey[i] = (lickey[i] + i + checksum) % WLIC_MAXTRAN;
	}

	/*
	**	Use WLIC_entran to convert each of the bytes in the key
	*/
	for(i=0;i<LICENSE_KEY_SIZE;i++)
	{
		lickey[i] = WLIC_entran((int)lickey[i]);
	}

		
	return(0);
}


/*
**	Routine:	WLIC_mkvalcode()
**
**	Function:	To generate a VALIDATION CODE
**
**	Description:	This routine accepts a LICENSE KEY and MACHINE ID and generates a VALIDATION CODE
**			by checksumming.
**			The VALIDATION CODE is a 3 digit code
**				1	Checksum of LICENSE KEY 	(mod 34 encoded)
**				2	Checksum of MACHINE ID 		(mod 34 encoded)
**				3	Checksum of digits 1 & 2	(mod 34 encoded)
**
**	Input:		lickey		the LICENSE KEY
**			machineid	the MACHINE ID
**			
**
**	Output:		valcode		the VALIDATION CODE   
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/22/92	Written by GSL
**
*/

static void WLIC_mkvalcode(char lickey[LICENSE_KEY_SIZE],char* machineid,char valcode[3])
{
	valcode[0] = WLIC_entran(WLIC_checksummem(lickey,LICENSE_KEY_SIZE,WLIC_MAXTRAN));
	valcode[1] = WLIC_entran(WLIC_checksummem(machineid,strlen(machineid),WLIC_MAXTRAN));
	valcode[2] = WLIC_entran(WLIC_checksummem(valcode,2,WLIC_MAXTRAN));
}


/*
**	Routine:	WLIC_authlogfile()
**
**	Function:	Return the authorize log filename.
**
**	Description:	Returns a pointer to a static data area that contains the file name.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to name.
**
**	Warnings:	None
**
**
*/
static char *WLIC_authlogfile(void)
{
	static	char	name[] = {"wauthorize.log"};

	return name;
}


static const char* tabfilepath()
{
	static	char	name[] = {"wauthorize.tab"};

	return name;
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
		       const char* f_valcode,
		       int4 version_number)
{
	FILE	*fp;
	
	fp = fopen(tabfilepath(),"a");
	if (!fp)
	{
		printf("\n**** ERROR ****\n");
		printf("Unable to open tab file %s [errno=%d %s]\n\n",tabfilepath(),errno,sys_errlist[errno]);
		return;
	}

	fprintf(fp, "%s\t%s\t%s\t%s\t%06d\t%s\t%s [%d]\t%8.8s\t%8.8s\t%s\t%s\t%s\t%d\n",
			       r_type,
			       operatorId(),
			       f_time,
			       f_custname,
			       custnum,
			       f_platform,
			       WLIC_lictypename(lictype), lictype,
			       licdate,
			       expdate,
			       f_lickey,
			       f_machid,
			       f_valcode,
			       version_number);
	fclose(fp);
}

static void freetabtable(struct licensetable_s *first)
{

	while(first != NULL)
	{
		struct licensetable_s *next = first->next;

		free(first->pType);
		free(first->pUserId);
		free(first->pTime);
		free(first->pCustName);
		free(first->pCustNum);
		free(first->pPlatform);
		free(first->pLicType);
		free(first->pLicDate);
		free(first->pExpDate);
		free(first->pLicKey);
		free(first->pMachId);
		free(first->pValCode);
		free(first->pVersionNum);

		free(first);

		first = next;
	}
}

static struct licensetable_s *loadtabtable(void)
{
	int rownum;
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
		szValCode[80],
		szVersionNum[80];
	struct licensetable_s *first = NULL;
	struct licensetable_s *next = NULL;
	
	fp = fopen(tabfilepath(),"r");
	if (!fp)
	{
		printf("Unable to open tab file %s\n",tabfilepath());
		return NULL;
	}

	rownum = 0;
	
	/*
	**	Load the tab file in a linked list in reverse order
	*/
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
			      szValCode,
			      szVersionNum))
	{
		rownum++;
		if (rownum == 1)
		{
			/* Skip first row containing headers */
			continue;
		}

		next = (struct licensetable_s *)malloc(sizeof(struct licensetable_s));
		next->next = first;
		next->row = rownum;
		next->pType = strdup(szType);
		next->pUserId = strdup(szUserId);
		next->pTime = strdup(szTime);
		next->pCustName = strdup(szCustName);
		next->pCustNum = strdup(szCustNum);
		next->pPlatform = strdup(szPlatform);
		next->pLicType = strdup(szLicType);
		next->pLicDate = strdup(szLicDate);
		next->pExpDate = strdup(szExpDate);
		next->pLicKey = strdup(szLicKey);
		next->pMachId = strdup(szMachId);
		next->pValCode = strdup(szValCode);
		next->pVersionNum = strdup(szVersionNum);

		first = next;
	}

	fclose(fp);

	return first;
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
		       char *pValCode,
		       char *pVersionNum)
{
#define NUMARGS 13
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
	args[12] = pVersionNum;

	/*
	**	Set all the args to empty strings
	*/
	for(i=0; i<NUMARGS; i++)
	{
		args[i][0] = '\0';  
	}


	if (NULL == fgets(buff, sizeof(buff), fp))
	{
		return 1;
	}
	buff[strlen(buff)-1] = '\0';

	bptr = buff;
	for(i=0; i<NUMARGS; i++)
	{
		if ((eptr = strchr(bptr,'\t')))
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
	while((ptr = strchr(text,'\t')))
	{
		*ptr = ' ';
	}

	/* Replace double quote (") with single quote (') */
	while((ptr = strchr(text,'\"')))
	{
		*ptr = '\'';
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

#define MAXRESULTS 10
static struct licensetable_s *select_search_result(struct licensetable_s *next, char* ss)
{
	struct licensetable_s *results[MAXRESULTS];
	int cnt = 0;
	int done = 0;
	int rc = 0;

	for(;;)
	{
		/*
		**	If maximum results shown or no more items to search then
		**	ask the user to select a result.
		*/
		if (next == NULL || cnt == MAXRESULTS)
		{
			if (cnt == 0)
			{
				printf("No results found.\n");
				return NULL;
			}

			printf("\n");
			done = 0;
			while(!done)
			{
				char promptbuf[80];
				int result_idx = 0;

				sprintf(promptbuf, "Select a result 1-%d ('0'=Show more, '.'=Return)", cnt);
				rc = prompt_num(promptbuf,"0",promptbuf,&result_idx);
				if (rc == PROMPT_RC_EXIT)
				{
					return NULL;
				}
				if (rc == PROMPT_RC_DEFAULT)
				{
					done = 1;
				}
				else if (result_idx >=1 && result_idx <= cnt)
				{
					return results[result_idx-1];
				}
				else
				{
					printf("Invalid selection\n");
				}
			}

			if (next == NULL)
			{
				printf("No results found.\n");
				return NULL;
			}

			cnt = 0;
		}

		if ((NULL!=strstr(tmpupper(next->pCustName),ss)) ||
		    (NULL!=strstr(tmpupper(next->pCustNum),ss))  ||
		    (NULL!=strstr(tmpupper(next->pLicKey),ss)) ||
		    (NULL!=strstr(tmpupper(next->pMachId),ss)) ||
		    (NULL!=strstr(tmpupper(next->pValCode),ss)) )
		{
			char szLicType[80];
			char *ptr;

			/*
			**	Found a match 
			*/
			cnt++;
			if (cnt == 1)
			{
		/*       12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
		/*	 XXXXX  XXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX YYYY-MM-DD HH:MM_SS*/
		/*	        XX     XXXX-XXXX-XXXX-XXXX XXXXXXXX XXXXXXXXXXXXXXX  XXX  */
				printf(
			"RESULT NUMBER CUSTOMER-NAME                                 TIMESTAMP\n"
			"       PLAT   LICENCE-KEY         TYPE     MACHINE-ID       VALCODE\n"
			"-------------------------------------------------------------------------------\n");
			}
			strcpy(szLicType, next->pLicType);
			if ((ptr=strchr(szLicType,' ')) != NULL)
			{
				*ptr = '\0';
			}

			printf("%5d  %6.6s %-45.45s %s\n       %2.2s     %s %-8.8s %-15s  %3.3s\n",
				cnt,
				next->pCustNum,
				next->pCustName,
				next->pTime,
				next->pPlatform,
				next->pLicKey,
				szLicType,
				next->pMachId,
				next->pValCode);

			results[cnt-1] = next;

		}

		next = next->next;
	}
}

static void search_license(struct license_s *l)
{
	char *help;
	char ss[80];
	int rc;
	struct licensetable_s *licensetable =  NULL;
	struct licensetable_s *selected_result =  NULL;

	init_license(l);

	licensetable = loadtabtable();

	if (licensetable == NULL)
	{
		return;
	}

	for(;;)
	{
		help =	"Enter the search string\n"
			"Enter '.' to return";

		printf("\n");
		rc = prompt_text("Enter search string ('.' to return)",NULL,PROMPT_EMPTY_NOTALLOWED,
			help,ss);
		if (rc == PROMPT_RC_EXIT)
		{
			break;
		}

		upper_string(ss);

		printf("\n");
		selected_result = select_search_result(licensetable, ss);
		if (selected_result != NULL)
		{
			printf("You selected %s %s\n", 
				selected_result->pCustName, selected_result->pLicKey);
			strcpy(l->custname, selected_result->pCustName);
			WLIC_unformatkey(l->licensekey,selected_result->pLicKey);
			l->licensekey[LICENSE_KEY_SIZE] = '\0';

		}
	}

	freetabtable(licensetable);
}


static void gen_machine_id(struct license_s *l)
{
	char *help;
	char* os_type;
	int os_type_cd;
	char machineName[80];
	char machineid[80];
	int rc;

	for(;;)
	{
		help =	"Enter W for a Windows Machine Id.\n"
			"Enter L for a Linux Machine Id.\n"
			"Enter A for a Alpha Machine Id.\n"
			"Enter Q to Return.";
		
		rc = prompt_list("Windows, Linux, Alpha or Quit",NULL,"w,l,a,q",help);
		switch(rc)
		{
		case PROMPT_RC_EXIT:
			return;

		case 1:		/* Windows */
			os_type = "Windows";
			os_type_cd = PLATFORM_WINDOWS_NT;
			break;
		case 2:		/* Linux */
			os_type = "Linux";
			os_type_cd = PLATFORM_LINUX;
			break;
		case 3:		/* Alpha */
			os_type = "Alpha";
			os_type_cd = PLATFORM_OSF1_ALPHA;
			break;

		default:
			return;
		}

		printf("\n");
		help = "Enter the machine name";
		rc = prompt_text("Enter machine name ('.' to return)",NULL,PROMPT_EMPTY_NOTALLOWED,
			help,machineName);
		if (rc == PROMPT_RC_EXIT)
		{
			break;
		}

		printf("\n");
		printf("You entered [%s]\n", machineName);

		if (os_type_cd == PLATFORM_WINDOWS_NT)
		{
			WL_encodemachid(machineName, machineid);
		}
		else if (os_type_cd == PLATFORM_LINUX)
		{
			WL_GetMachineIdFromName(machineid, machineName, "LX");
		}
		else if (os_type_cd == PLATFORM_OSF1_ALPHA)
		{
			WL_GetMachineIdFromName(machineid, machineName, "A1");
		}

		printf("The %s Machine Id for machine name [%s] is [%s]\n", 
			os_type, machineName, machineid);

		strcpy(l->machineid, machineid);
	}
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
		szValCode[80],
		szVersionNum[80];
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

	if ((cptr = getenv("SCRIPT_NAME")))
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
			      szValCode,
			      szVersionNum))
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
				
				if ((ptr = strchr(szLicType,' ')))
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
	       "at " WISP_PHONE_NUMBER ".");
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
		szValCode[80],
		szVersionNum[80];
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
			      szValCode,
			      szVersionNum))
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
	int4	version_number = 0;

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
	

	if (WLIC_mklickey(nCustNum,pszPlatform,nLicType,szLicDate,szExpDate,version_number,szLicKey))
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

	if (WLIC_bklickey(&nCustNum,szPlatform,&nLicType,szLicDate,szExpDate,&version_number,szLicKey))
	{
		strcpy(errmess, "Unable to decode the generated key");
		goto displayerr;
	}

	logkeyinfo(pszCustName,nCustNum,szPlatform,nLicType,szLicDate,szExpDate,version_number,szLicKey);

	WLIC_formatkey(szLicKey,szFormatLicKey);
	sprintf(szFormatCustNum, "%06d", nCustNum);
	formatPlatform(szPlatform,szFormatPlatform);
	
	cgi_display_license_sheet(
			pszCustName,
			szFormatCustNum,
			szFormatPlatform,
			WLIC_lictypename(nLicType),
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
#include "wutils.h"

/*
**	History:
**	$Log: wauthorize.c,v $
**	Revision 1.42  2009/07/19 20:54:22  gsl
**	Shell Stream
**	
**	Revision 1.41  2009/07/19 00:39:23  gsl
**	Update for Shell Stream
**	
**	Revision 1.40  2004/06/14 15:42:57  gsl
**	make external the routines to generate MachineId from the Unix Machine Name
**	Used by Linux and Alpha. Add M function to wauthorize to generate machine id for Window or Linux or Alpha from the machine name.
**	
**	Revision 1.39  2004/05/03 17:41:27  gsl
**	Add IssueDate and change license.template macros to be [XXX] so as tobe
**	compatiable with HTML
**	
**	Revision 1.38  2003/06/17 16:50:31  gsl
**	fix key printout for missing values
**	
**	Revision 1.37  2003/06/17 16:31:18  gsl
**	warning
**	
**	Revision 1.36  2003/06/17 16:24:38  gsl
**	Add Search function
**	
**	Revision 1.35  2003/06/16 21:13:59  gsl
**	add search
**	
**	Revision 1.34  2003/06/13 20:49:15  gsl
**	ENTERPRISE License - log the version number to the tab file
**	
**	Revision 1.33  2003/06/13 17:36:12  gsl
**	ENTERPRISE License
**	
**	Revision 1.32  2003/06/12 20:54:29  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.31  2003/05/27 20:53:07  gsl
**	Update PROMPT prototypes and add defines for return codes
**	
**	Revision 1.30  2003/05/23 20:06:27  gsl
**	In clean_text() change " to '
**	Add formatTimeStamp() to given new format YYYY-MM-DD HH:MM:SS
**	add return code to gen_key() to test for gen_val()
**	
**	Revision 1.29  2003/04/29 13:03:51  gsl
**	Print the platform code as well as the name on the license doc
**	
**	Revision 1.28  2003/02/10 16:27:07  gsl
**	Add revision to header
**	
**	Revision 1.27  2003/02/10 16:23:30  gsl
**	FIx -Wall warnings
**	
**	Revision 1.26  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.25  2003/01/23 19:36:58  gsl
**	Ensure the doc file has read-other permission
**	
**	Revision 1.24  2003/01/09 16:21:21  gsl
**	Fix default action [enter] on "Generate another valcode?"
**	
**	Revision 1.23  2003/01/09 16:12:33  gsl
**	Fix default action [enter] on "Generate another valcode?"
**	
**	Revision 1.22  2003/01/08 17:42:20  gsl
**	Update year 2003
**	
**	Revision 1.21  2003/01/08 17:29:58  gsl
**	first pass at makefile
**	
**	Revision 1.20  2002/12/31 16:31:16  gsl
**	change default to "n" for Q "Generate another VALIDATION CODE"?
**	
**	Revision 1.19  2002/12/31 16:25:43  gsl
**	Move the license key generation stuff to wauthoize.c
**	
**	Revision 1.18  2002/09/04 18:13:40  gsl
**	LINUX sys_errlist
**	
**	Revision 1.17  2002/07/10 21:06:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.16  2002/07/09 04:13:50  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.15  2002/07/02 21:15:40  gsl
**	Rename wstrdup
**	
**	Revision 1.14  2002/06/26 20:52:16  gsl
**	Fix phone number
**	
**	Revision 1.13  2002/05/14 20:11:03  gsl
**	On Win32 use WL_longuid() instead of cuserid()
**	
**	Revision 1.12  2000-03-13 14:16:29-05  gsl
**	Fix WIN32 warnings
**
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
**	05/26/92	Moved WL_putplattab() to platsubs.c to isolate platform_table GSL.
**	09/25/92	Added CLUSTER support. GSL
**	09/13/93	Generalize for UniQue. GSL
**
**
*/
