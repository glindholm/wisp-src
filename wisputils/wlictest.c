/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		wlictest.c
**
**	Routines:	
**	wlictest()
*/

/*
**	Includes
*/
#include "filext.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/


#include <sys/types.h>
#include <sys/stat.h>
#ifdef unix
#include <sys/utsname.h>
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

#include "idsistd.h"
#include "wlicense.h"
#include "platsubs.h"
#include "idsisubs.h"
#include "machid.h"
#include "wispcfg.h"

#ifdef WIN32
#include "isonames.h"
#endif

static int get_license_info(char* flickey, char* valcode);

static void print_ckvalcode(char lickey[LICENSE_KEY_SIZE], char* machineid, char valcode[3])
{
	char	lvalcode[4];
	int	rc;
	
	lvalcode[0] = WLIC_entran(rc = WLIC_checksummem(lickey,LICENSE_KEY_SIZE,WLIC_MAXTRAN));
	printf("lickey    => [%2d] => [%c] == [%c]\n", rc, lvalcode[0], valcode[0]);
	
	lvalcode[1] = WLIC_entran(rc = WLIC_checksummem(machineid,strlen(machineid),WLIC_MAXTRAN));
	printf("machineid => [%2d] => [%c] == [%c]\n", rc, lvalcode[1], valcode[1]);
	
	lvalcode[2] = WLIC_entran(rc = WLIC_checksummem(valcode,2,WLIC_MAXTRAN));
	printf("valcode   => [%2d] => [%c] == [%c]\n", rc, lvalcode[2], valcode[2]);
}

/*
**	Routine:	debug_validate_license()
**
**	Function:	To validate the license.
**
**	Description:	This routine will read the license file and validate the LICENSE-KEY and VALIDATION-CODE.
**
**	Input:		/lib/{product}.license
**			
**
**	Output:		None
**			
**
**	Return:		LICENSE_CHECK_OK		All is OK
**			LICENSE_CHECK_MISSING		No license file
**			LICENSE_CHECK_TIMEDOUT		License has timed out
**			LICENSE_CHECK_INVALID		License or validation is not valid on this platform
**			LICENSE_CHECK_UNKNOWN		Unable to validate 
**
**	Warnings:	None
**
**	History:	05/26/92	Written by GSL
**			09/24/92	Added LICENSE_CLUSTER. GSL
**
*/

static int debug_validate_license(void)
{
	int4	custnum;
	char	platform[3];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[80];
	char	machineid[256];
	char	valcode[80];
	int4	version_number = 0;

	char	flickey[80];
	int	rc;


	/*
	**	Get the LICENSE KEY and VALIDATION CODE
	*/
	rc = get_license_info(flickey, valcode);
	printf("get_license_info() = [%d]\n", rc);
	if (rc)
	{
		return rc;
	}
	printf("flickey = [%s]\n",flickey);
	printf("valcode = [%s]\n",valcode);
	
	/*
	**	Check if the LICENSE KEY is a valid key
	*/

	WL_upper_string(flickey);
	printf("upper_string(flickey) = [%s]\n",flickey);

	WLIC_unformatkey(licensekey,flickey);
	printf("unformatkey() = [%16.16s]\n", licensekey);

	if ((rc = WLIC_bklickey(&custnum,platform,&licensetype,licensedate,expdate,
		&version_number, licensekey)))
	{
		printf("bklickey() failed rc = %d\n", rc);
		return(LICENSE_CHECK_INVALID);
	}

	printf("custnum     = [%d]\n",custnum);
	printf("platform    = [%2.2s]\n",platform);
	printf("lictype     = [%d]\n",licensetype);
	printf("licensedate = [%8.8s]\n",licensedate);
	printf("expdate     = [%8.8s]\n",expdate);
	if (version_number != 0)
	{
		printf("version     = [%d.%d]\n",version_number/10, version_number%10);
	}

	/*
	**	Check if the LICENSE KEY is valid for this platform
	*/

	
	printf("PLATFORM = [%d] [%s] [%2.2s]\n", WL_platform_number(), WL_platform_name(), WL_platform_code());
	
	if ((rc = WL_valplat(platform)))
	{
		printf("WL_valplat(\"%2.2s\") = [%d]\n", platform, rc);
		
		return(LICENSE_CHECK_INVALID);
	}

	/*
	**	Check if the LICENSE TYPE is valid
	*/

	switch(licensetype)
	{
	case LICENSE_SINGLE:
		printf("LICENSE_SINGLE\n");
		
		/*
		**	Check if a VALIDATION CODE was found
		*/
		if (! valcode[0])
		{ 
			printf("VALIDATION CODE was not found\n");
			
			return(LICENSE_CHECK_UNKNOWN);
		}
		

		/*
		**	Get the MACHINE ID
		*/
		if ((rc = WL_getmachineid(machineid)))
		{
			printf("WL_getmachineid() failed rc = [%d]\n",rc);
			
			return(LICENSE_CHECK_UNKNOWN);
		}
		printf("WL_getmachineid() = [%s]\n", machineid);

		/*
		**	Check if the VALIDATION CODE is valid
		*/
		WL_upper_string(valcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			printf("INVALID strlen(valcode) = [%lu]\n", (unsigned long)strlen(valcode));
			return(LICENSE_CHECK_INVALID);
		}

		print_ckvalcode(licensekey,machineid,valcode);

		rc = WLIC_ckvalcode(licensekey,machineid,valcode);
		printf("chvalcode() rc = [%d]\n", rc);
		if ( 0 != rc)
		{
			printf("WLIC_ckvalcode() FAILED\n");
			return(LICENSE_CHECK_INVALID);
		}
		break;

	case LICENSE_UNLIMITED:
		printf("LICENSE_UNLIMITED\n");
		break;

	case LICENSE_ENTERPRISE:
		printf("LICENSE_ENTERPRISE\n");
		break;

	case LICENSE_TIMED:
		printf("LICENSE_TIMED\n");
		if ((rc = WLIC_check_timeout(licensedate,expdate)))
		{
			printf("check_timeout() FAILED rc = [%d]\n", rc);
			return(LICENSE_CHECK_TIMEDOUT);
		}
		break;

	case LICENSE_CLUSTER:
		printf("LICENSE_CLUSTER\n");
		/*
		**	For a cluster don't check the validation code
		**	as it will only be valid on the machine that
		**	was used to install the license. 
		**	This is used when multiple machines share the
		**	same /lib/{product}.license file as with HP/UX cluster.
		*/

		/*
		**	Check if a VALIDATION CODE was found
		*/
		if (! valcode[0]) 
		{
			printf("VALIDATION CODE was not found\n");

			return(LICENSE_CHECK_UNKNOWN);
		}
		

		/*
		**	Check if the VALIDATION CODE is right size
		*/
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			printf("INVALID strlen(valcode) = [%lu]\n", (unsigned long)strlen(valcode));
			return(LICENSE_CHECK_INVALID);
		}
		break;

#ifdef WIN32
	case LICENSE_NETWORK:
		printf("LICENSE_NETWORK\n");
		/*
		**	If on the server then validate normally.
		**	If on a client then validate using the server name as the machine id.
		*/

		/*
		**	Check if a VALIDATION CODE was found
		*/
		if (! valcode[0]) 
		{
			printf("VALIDATION CODE was not found\n");

			return(LICENSE_CHECK_UNKNOWN);
		}

		/*
		**	Get the MACHINE ID
		*/
		if ((rc = WL_getmachineid(machineid)))
		{
			printf("WL_getmachineid() failed rc = [%d]\n",rc);

			return(LICENSE_CHECK_UNKNOWN);
		}
		printf("WL_getmachineid() = [%s]\n", machineid);

		/*
		**	If running on a CLIENT then the machineid is 
		**	generated from the SERVER name.
		*/
		printf("wispserver()   = [%s]\n", wispserver());
		printf("computername() = [%s]\n", WL_computername(NULL));
		printf("DEF_WISPSERVER = [%s]\n", DEF_WISPSERVER);
		
		if (0 != strcmp(wispserver(), WL_computername(NULL)) &&
		    0 != strcmp(wispserver(), DEF_WISPSERVER)        )
		{
			printf("ON CLIENT MACHINE\n");
			WL_encodemachid(wispserver(), machineid);
			printf("SERVER machineid = [%s]\n", machineid);
		}
		
		/*
		**	Check if the VALIDATION CODE is valid
		*/
		WL_upper_string(valcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			printf("INVALID strlen(valcode) = [%d]\n", strlen(valcode));
			return(LICENSE_CHECK_INVALID);
		}

		print_ckvalcode(licensekey,machineid,valcode);

		rc = WLIC_ckvalcode(licensekey,machineid,valcode);
		printf("chvalcode() rc = [%d]\n", rc);
		if ( 0 != rc)
		{
			printf("WLIC_ckvalcode() FAILED\n");
			return(LICENSE_CHECK_INVALID);
		}
		break;
#endif /* WIN32 */
		
	default:
		printf("LICENSE_CHECK_UNKNOWN = [%d]\n",licensetype);
		return(LICENSE_CHECK_INVALID);
	}

	printf("\nLICENSE IS OK\n");
	
	return(LICENSE_CHECK_OK);
}

/*
**	ROUTINE:	get_license_info()
**
**	FUNCTION:	Get the stored license information.
**
**	DESCRIPTION:	Read the license file to get the LICENSE KEY and VALIDATION CODE
**
**	ARGUMENTS:	
**	flickey		The returned LICENSE KEY
**	valcode		The returned VALIDATION CODE (may be empty string if not found)
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Successfully returned license info
**	LICENSE_CHECK_MISSING	License info was missing
**	LICENSE_CHECK_UNKNOWN	Unable to get license info
**
**	WARNINGS:	None
**
*/
static int get_license_info(char* flickey, char* valcode)
{
	FILE	*fp;
	char	buff[256];
	int	rc;
	int 	lineno;

	/*
	**	Check if license file exists
	*/

	printf("WLIC_license_filepath() = [%s]\n", WLIC_license_filepath());	
	rc = access(WLIC_license_filepath(),0000);
	printf("access(WLIC_license_filepath(),0000) = [%d]\n", rc);

	if (0 != rc)
	{
		printf("LICENSE_CHECK_MISSING\n");
		return(LICENSE_CHECK_MISSING);
	}

	/*
	**	Open license file for reading
	*/
	fp = fopen(WLIC_license_filepath(),"r");
	printf("fopen(\"%s\",\"r\") = [%lx]\n", WLIC_license_filepath(), (long)fp);
	if (!fp)
	{
		printf("LICENSE_CHECK_UNKNOWN fopen() failed\n");
		return(LICENSE_CHECK_UNKNOWN);
	}

	/*
	**	Read thru the file extracting the LICENSE KEY and the VALIDATION CODE
	*/

	flickey[0] = '\0';
	valcode[0] = '\0';
	lineno = 0;

	printf("\n== START ==\n");
	while(fgets(buff,sizeof(buff),fp))
	{
		lineno++;
		printf("%3d: %s",lineno, buff);
		
		if (0 == memcmp(buff,"LICENSE-KEY ",12))
		{
			rc = sscanf(buff,"LICENSE-KEY %s",flickey);
			if (rc != 1) 
			{
				printf("sscanf(\"LICENSE-KEY\") failed rc = [%d]\n", rc);
				return(LICENSE_CHECK_UNKNOWN);
			}
			printf("LICENSE-KEY = [%s]\n", flickey);
		}
		else if (0 == memcmp(buff,"VALIDATION-CODE ",16))
		{
			rc = sscanf(buff,"VALIDATION-CODE %s",valcode);
			if (rc != 1)
			{
				printf("sscanf(\"VALIDATION-CODE\") failed rc = [%d]\n", rc);
				return(LICENSE_CHECK_UNKNOWN);
			}
			printf("VALIDATION-CODE = [%s]\n", valcode);
		}
	}
	printf("\n== END ==\n");
	fclose(fp);

	/*
	**	Check if a LICENSE KEY was found
	*/

	if (!flickey[0])
	{
		printf("LICENSE-KEY was not found\n");
		return(LICENSE_CHECK_UNKNOWN);
	}

	return 0;
}

/*
**	ROUTINE:	wlictest()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	?
**
**	GLOBALS:	?
**
**	RETURN:		?
**
**	WARNINGS:	?
**
*/


int main(int argc, char *argv[])
{
	int rc;
	
	rc = debug_validate_license();
	
	return 0;
}

/*
**	History:
**	$Log: wlictest.c,v $
**	Revision 1.15  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.14  2010/01/22 23:03:12  gsl
**	Fix printf %lu size_t warnings
**	
**	Revision 1.13  2010/01/22 16:44:05  gsl
**	Fix printf %lu size_t warnings
**	
**	Revision 1.12  2010/01/20 22:25:37  gsl
**	fix unsigned format warning
**	
**	Revision 1.11  2003/06/12 20:54:30  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.10  2003/02/07 17:55:21  gsl
**	Rework the platform routines and add AIX HPUX SOLARIS 64-bit
**	
**	Revision 1.9  2003/02/04 21:23:44  gsl
**	fix -Wall warnings
**	
**	Revision 1.8  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.7  2002/12/31 16:25:43  gsl
**	Move the license key generation stuff to wauthoize.c
**	
**	Revision 1.6  2002/07/18 21:04:24  gsl
**	Remove MSDOS code
**	
**	Revision 1.5  2002/07/11 14:33:56  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.4  2002/07/10 21:06:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.3  2002/07/09 04:13:49  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.2  2002/06/25 18:18:36  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.1  1997/10/14 14:34:55  gsl
**	Initial revision
**	
**	Revision 1.9  1997-01-09 14:46:28-05  gsl
**	Changed copyright date 1996 -> 1997
**
**
**
**
*/
