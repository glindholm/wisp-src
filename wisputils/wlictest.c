static char copyright[]="Copyright (c) 1997 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wlictest.c
**
**	Project:	????
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
**	Routines:	
**	wlictest()		???
*/

/*
**	Includes
*/
#define EXT_FILEXT
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
#endif

#ifdef _MSC_VER
#include <io.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <string.h>

#include "idsistd.h"
#include "wlicense.h"
#include "platsubs.h"
#include "idsisubs.h"
#include "machid.h"
#include "wispcfg.h"

#define MAXTRAN		34

static int get_license_info(char* flickey, char* valcode);

static void print_ckvalcode(char lickey[LICENSE_KEY_SIZE], char* machineid, char valcode[3])
{
	char	lvalcode[4];
	int	rc;
	
	lvalcode[0] = entran(rc = checksummem(lickey,LICENSE_KEY_SIZE,MAXTRAN));
	printf("lickey    => [%2d] => [%c] == [%c]\n", rc, lvalcode[0], valcode[0]);
	
	lvalcode[1] = entran(rc = checksummem(machineid,strlen(machineid),MAXTRAN));
	printf("machineid => [%2d] => [%c] == [%c]\n", rc, lvalcode[1], valcode[1]);
	
	lvalcode[2] = entran(rc = checksummem(valcode,2,MAXTRAN));
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
**	Return:		LICENSE_OK			All is OK
**			LICENSE_MISSING			No license file
**			LICENSE_TIMEDOUT		License has timed out
**			LICENSE_INVALID			License or validation is not valid on this platform
**			LICENSE_UNKNOWN			Unable to validate 
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

	char	flickey[80];
	int	rc;
	char the_plat_name[80], the_plat_code[3];


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

	upper_string(flickey);
	printf("upper_string(flickey) = [%s]\n",flickey);

	unformatkey(licensekey,flickey);
	printf("unformatkey() = [%16.16s]\n", licensekey);

	if (rc = bklickey(&custnum,platform,&licensetype,licensedate,expdate,licensekey))
	{
		printf("bklickey() failed rc = %d\n", rc);
		return(LICENSE_INVALID);
	}

	printf("custnum     = [%d]\n",custnum);
	printf("platform    = [%2.2s]\n",platform);
	printf("lictype     = [%d]\n",licensetype);
	printf("licensedate = [%8.8s]\n",licensedate);
	printf("expdate     = [%8.8s]\n",expdate);

	/*
	**	Check if the LICENSE KEY is valid for this platform
	*/

	
	rc = whatplat(the_plat_name, the_plat_code);
	
	printf("whatplat() = [%d] [%s] [%2.2s]\n", rc, the_plat_name, the_plat_code);
	
	if (rc = valplat(platform))
	{
		printf("valplat(\"%2.2s\") = [%d]\n", platform, rc);
		
		return(LICENSE_INVALID);
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
			
			return(LICENSE_UNKNOWN);
		}
		

		/*
		**	Get the MACHINE ID
		*/
		if (rc = getmachineid(machineid))
		{
			printf("getmachineid() failed rc = [%d]\n",rc);
			
			return(LICENSE_UNKNOWN);
		}
		printf("getmachineid() = [%s]\n", machineid);

		/*
		**	Check if the VALIDATION CODE is valid
		*/
		upper_string(valcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			printf("INVALID strlen(valcode) = [%d]\n", strlen(valcode));
			return(LICENSE_INVALID);
		}

		print_ckvalcode(licensekey,machineid,valcode);

		rc = ckvalcode(licensekey,machineid,valcode);
		printf("chvalcode() rc = [%d]\n", rc);
		if ( 0 != rc)
		{
			printf("ckvalcode() FAILED\n");
			return(LICENSE_INVALID);
		}
		break;

	case LICENSE_UNLIMITED:
		printf("LICENSE_UNLIMITED\n");
		break;

	case LICENSE_TIMED:
		printf("LICENSE_TIMED\n");
		if (rc = check_timeout(licensedate,expdate))
		{
			printf("check_timeout() FAILED rc = [%d]\n", rc);
			return(LICENSE_TIMEDOUT);
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

			return(LICENSE_UNKNOWN);
		}
		

		/*
		**	Check if the VALIDATION CODE is right size
		*/
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			printf("INVALID strlen(valcode) = [%d]\n", strlen(valcode));
			return(LICENSE_INVALID);
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

			return(LICENSE_UNKNOWN);
		}

		/*
		**	Get the MACHINE ID
		*/
		if (rc = getmachineid(machineid))
		{
			printf("getmachineid() failed rc = [%d]\n",rc);

			return(LICENSE_UNKNOWN);
		}
		printf("getmachineid() = [%s]\n", machineid);

		/*
		**	If running on a CLIENT then the machineid is 
		**	generated from the SERVER name.
		*/
		printf("wispserver()   = [%s]\n", wispserver());
		printf("computername() = [%s]\n", computername(NULL));
		printf("DEF_WISPSERVER = [%s]\n", DEF_WISPSERVER);
		
		if (0 != strcmp(wispserver(), computername(NULL)) &&
		    0 != strcmp(wispserver(), DEF_WISPSERVER)        )
		{
			printf("ON CLIENT MACHINE\n");
			encodemachid(wispserver(), machineid);
			printf("SERVER machineid = [%s]\n", machineid);
		}
		
		/*
		**	Check if the VALIDATION CODE is valid
		*/
		upper_string(valcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			printf("INVALID strlen(valcode) = [%d]\n", strlen(valcode));
			return(LICENSE_INVALID);
		}

		print_ckvalcode(licensekey,machineid,valcode);

		rc = ckvalcode(licensekey,machineid,valcode);
		printf("chvalcode() rc = [%d]\n", rc);
		if ( 0 != rc)
		{
			printf("ckvalcode() FAILED\n");
			return(LICENSE_INVALID);
		}
		break;
#endif /* WIN32 */
		
	default:
		printf("LICENSE_UNKNOWN = [%d]\n",licensetype);
		return(LICENSE_INVALID);
	}

	printf("\nLICENSE IS OK\n");
	
	return(LICENSE_OK);
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
**	LICENSE_MISSING	License info was missing
**	LICENSE_UNKNOWN	Unable to get license info
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

	printf("license_filepath() = [%s]\n", license_filepath());	
	rc = access(license_filepath(),0000);
	printf("access(license_filepath(),0000) = [%d]\n", rc);

	if (0 != rc)
	{
		printf("LICENSE_MISSING\n");
		return(LICENSE_MISSING);
	}

	/*
	**	Open license file for reading
	*/
	fp = fopen(license_filepath(),"r");
	printf("fopen(\"%s\",\"r\") = [%lx]\n", license_filepath(), (long)fp);
	if (!fp)
	{
		printf("LICENSE_UNKNOWN fopen() failed\n");
		return(LICENSE_UNKNOWN);
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
				return(LICENSE_UNKNOWN);
			}
			printf("LICENSE-KEY = [%s]\n", flickey);
		}
		else if (0 == memcmp(buff,"VALIDATION-CODE ",16))
		{
			rc = sscanf(buff,"VALIDATION-CODE %s",valcode);
			if (rc != 1)
			{
				printf("sscanf(\"VALIDATION-CODE\") failed rc = [%d]\n", rc);
				return(LICENSE_UNKNOWN);
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
		return(LICENSE_UNKNOWN);
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
