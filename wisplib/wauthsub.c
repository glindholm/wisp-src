/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/



/*
**	File:		wauthsub.c
**
**	Function:	subroutines used by wauthorize and WLIC_validate_license()
**
**	Routines:	WLIC_entran()		Translate a number 0-35 to a printable char
**			WLIC_detran()		Reverse on WLIC_entran()
**			WLIC_checksummem()	Calculate a checksum
**			WLIC_packdate()		Pack an 8 char date into 3 bytes
**			WLIC_unpkdate()		Unpack a date created by WLIC_packdate()
**			WLIC_lictypename()	Return the text string name of the license type code.
**			WL_getmachineid()	Get the MACHINE ID.
**			WLIC_validate_license()	Read the license file and validate the license.
**			WLIC_check_timeout()	Check if the license has timed-out
**
**
*/

#include <sys/types.h>
#include <sys/stat.h>
#ifdef unix
#include <sys/utsname.h>
#endif

#ifdef WIN32
#include <io.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <string.h>

#include "idsistd.h"
#include "wcommon.h"
#include "wlicense.h"
#include "platsubs.h"
#include "idsisubs.h"
#include "machid.h"
#include "wispcfg.h"

#ifdef WIN32
#include "isonames.h"
#endif



/*
**	Routine:	WLIC_entran()
**
**	Function:	To translate a number 0-34 into A-Z1-9
**			
**	Description:	This routine will map a number 0-34 into the corresponding translation table character.
**			The translation table characters are all printable characters - they have been scrambled.
**
**	Input:		innum		the number to translate.
**			tt_tran		translation table;
**
**	Output:		None
**
**	Return:		The translated character.
**
**	Warnings:	The innum must be between 0-34, if not a "#" will be returned.
**			We do not use char '0' (ZERO) because of confusion with 'O' (OH).
**
**	History:	
**	05/19/92	Written by GSL
**	05/20/92	Changed to use a scrambled translation table GSL
**	09/13/93	Generalized to use WLIC_lic_trantable(). GSL
**
*/

#ifdef OLD
static	char	*tt_tran = "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789";
#endif

char	WLIC_entran(int innum)
{
	const char *tt_tran = WLIC_lic_trantable();
	if (innum >= 0 && innum <= 34)
	{
		return( tt_tran[innum] );
	}

	return('#');
}

/*
**	Routine:	WLIC_detran()
**
**	Function:	Detranslate a character translated by WLIC_entran() back into a number 0-34.
**			
**	Description:	This routine reverses the WLIC_entran() routine by using the same translation table to map a character
**			back into a number.
**
**	Input:		inchar		the translated character
**			tt_tran		the translation table
**			
**
**	Output:		None
**			
**	Return:		the detranslated number
**
**	Warnings:	An invalid character outside A-Z1-9 will return a -1.
**
**	History:	
**	05/19/92	Written by GSL
**	09/13/93	Generalized to use WLIC_lic_trantable(). GSL
*/

int	WLIC_detran(char inchar)
{
	char	*ptr;
	const char *tt_tran = WLIC_lic_trantable();

	ptr = strchr(tt_tran, toupper(inchar));					/* Search for the char in the table (uppercase)	*/
	if (ptr)
	{
		return( ptr - tt_tran );					/* calculate the number				*/
	}

	return -1;								/* invalid character				*/
}

/*
**	Routine:	WLIC_checksummem()
**
**	Function:	Does a checksum
**			
**	Description:	This routine adds together the unsigned integer value of each character then mods it by the 
**			given mod value.  The result is a checksum in the range of 0 - (mod-1).
**
**	Input:		instr		The string to checksum
**			size		the size of string
**			mod		the mod
**
**	Output:		None
**
**	Return:		the sum mod by mod.
**
**	Warnings:	Size must be valid, dosen't stop on a null.
**
**	History:	05/19/92	Written by GSL
**			06/09/92	Add (int) cast to % line. GSL
**
*/

int	WLIC_checksummem(char* instr, int size, int mod)
{
	int	i;
	int	cs;
	unsigned char* ptr = (unsigned char*)instr;

	cs = 0;
	for (i=0; i<size; i++)
	{
		cs = (int)(cs + ptr[i]) % mod;
	}

	return(cs);
}

/*
**	Routine:	WLIC_packdate()
**
**	Function:	This routine takes a date YYYYMMDD and packs it into 3 bytes. 
**
**	Description:	This routine compresses a char date i.e. 19920527 (27 May 1992) into 3 bytes.
**			Each byte is treated as a one byte int.
**			The first bytes is the year since 1990.  Eg 1992 = 2.  (1990 - 2117)
**			The second byte is the month 1-12.
**			The third byte is the day 1-31.
**			The maximum packed year is 127 so that it will fit in a signed char. (127 == 2117ad)
**			
**
**	Input:		yyyymmdd	date in YYYYMMDD format
**
**	Output:		ymd		the packed date
**
**	Return:		0 = success
**			1 = invalid date
**
**	Warnings:	This routine does not test for non-existent dates like Feb 31st etc.
**			This routine can handle years up to 127 (2117) but if used with WLIC_entran() it has a limit of 34 (2024).
**			This routine can only handle dates since 1990.
**
**	History:	05/20/92	Written by GSL
**
*/

#define BASEYEAR	1990
#define YEARMONTHS	12
#define MONTHDAYS	31
#define MAXPACKYEARS	127

int WLIC_packdate(char* yyyymmdd, char* ymd)
{
	int	i;
	int	year, month, day;
	int thdig, hudig, tedig, odig;
	
	for(i=0;i<8;i++)
	{
		if (!isdigit((int)yyyymmdd[i])) return(1);
	}

	/* 
	 * the following kludge was added courtesy of a broken C compiler on 
	 * SCO unix, which was unable to handle the previous form of this simple
	 * formula.
	 * 
	 */
	thdig = (int)(yyyymmdd[0]-'0');
	hudig = (int)(yyyymmdd[1]-'0');
	tedig = (int)(yyyymmdd[2]-'0');
	odig  = (int)(yyyymmdd[3]-'0');

	year = thdig*1000 + hudig*100 + tedig*10 + odig;

	if (year < BASEYEAR) return(1);
	year -= BASEYEAR;
	if (year > MAXPACKYEARS) return(1);

	month = (yyyymmdd[4]-'0')*10 + (yyyymmdd[5]-'0');
	if (month < 1 || month > YEARMONTHS) return(1);

	day = (yyyymmdd[6]-'0')*10 + (yyyymmdd[7]-'0');
	if (day < 1 || day > MONTHDAYS) return(1);

	ymd[0] = (char) year;
	ymd[1] = (char) month;
	ymd[2] = (char) day;

	return(0);
}

/*
**	Routine:	WLIC_unpkdate()
**
**	Function:	Unpack a date that was packed by WLIC_packdate().
**
**	Description:	This routine converts a 3 byte packed date into a 8 character date by exactly reversing the
**			process used in WLIC_packdate().
**
**	Input:		ymd		the packed date
**
**	Output:		yyyymmdd	date in YYYYMMDD format
**
**	Return:		0 = success
**			1 = invalid date
**
**	Warnings:	This routine does not test for non-existent dates like Feb 31st etc.
**
**	History:	05/20/92	Written by GSL
**
*/

int WLIC_unpkdate(char* yyyymmdd,char* ymd)
{
	int	year, month, day;
	char	buff[20];

	year = ymd[0];
	if (year < 0 || year > MAXPACKYEARS) return(1);
	year += BASEYEAR;

	month = ymd[1];
	if (month < 1 || month > YEARMONTHS) return(1);

	day = ymd[2];
	if (day < 1 || day > MONTHDAYS)	return(1);

	sprintf(buff,"%04d%02d%02d",year,month,day);

	memcpy(yyyymmdd,buff,8);
	return(0);
}


/*
**	Routine:	WLIC_lictypename()
**
**	Function:	To return the text string name of the license type code.
**
**	Description:	
**
**	Input:		lictype		the license type code
**			
**
**	Output:		None
**			
**
**	Return:		pointer to a string that is the name of the license type
**
**	Warnings:	The caller must not modify the string pointed to.
**
**	History:	05/21/92	Written by GSL
**			09/25/92	Added LICENSE_CLUSTER. GSL
**
*/

char *WLIC_lictypename(int lictype)
{
static	char	*l_single	= "SINGLE";
static	char	*l_unlimited	= "UNLIMITED";
static	char	*l_timed	= "TIMED";
static	char	*l_cluster	= "CLUSTER";
static	char	*l_network	= "NETWORK";
static	char	*l_enterprise	= "ENTERPRISE";
static	char	*l_invalid	= "INVALID";

	switch(lictype)
	{
	case LICENSE_SINGLE:	return(l_single);
	case LICENSE_UNLIMITED:	return(l_unlimited);
	case LICENSE_TIMED:	return(l_timed);
	case LICENSE_CLUSTER:	return(l_cluster);
	case LICENSE_NETWORK:	return(l_network);
	case LICENSE_ENTERPRISE:return(l_enterprise);
	default:		return(l_invalid);
	}
}

/*
**	Routine:	WLIC_validate_license()
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

int WLIC_validate_license(void)
{
	int4	custnum;
	char	platform[3];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[80];
	char	machineid[256];
	char	valcode[80];
	char	appcode[80];
	int4	version_number = 0;

	char	flickey[80];
	int	rc;


	/*
	**	Get the LICENSE KEY and VALIDATION CODE
	*/
	rc = WLIC_get_license_info(flickey, valcode, appcode);
	if (rc)
	{
		return rc;
	}
	
	/*
	**	Check if the LICENSE KEY is a valid key
	*/

	upper_string(flickey);
	WLIC_unformatkey(licensekey,flickey);
	if (WLIC_bklickey(&custnum,platform,&licensetype,licensedate,expdate,
		&version_number,licensekey))
	{
		return(LICENSE_CHECK_INVALID);
	}

	/*
	**	Check if the LICENSE KEY is valid for this platform
	*/

	if (WL_valplat(platform))
	{
		return(LICENSE_CHECK_INVALID);
	}

	/*
	**	Check if the LICENSE TYPE is valid
	*/

	switch(licensetype)
	{
	case LICENSE_SINGLE:
		/*
		**	Check if a VALIDATION CODE was found
		*/
		if (! valcode[0]) return(LICENSE_CHECK_UNKNOWN);

		/*
		**	Get the MACHINE ID
		*/
		if ((rc = WL_getmachineid(machineid)) != 0)
		{
			return(LICENSE_CHECK_UNKNOWN);
		}

		/*
		**	Check if the VALIDATION CODE is valid
		*/
		upper_string(valcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode) ||
		    0 != WLIC_ckvalcode(licensekey,machineid,valcode))
		{
			return(LICENSE_CHECK_INVALID);
		}
		break;

	case LICENSE_UNLIMITED:
		/*
		**	UNLIMITED licenses are no longer supported.
		**	Replaced by ENTERPRISE licenses.
		*/
		return LICENSE_CHECK_UNLIMITED;	
		break;

	case LICENSE_ENTERPRISE:
		/*
		**	Check version_number
		*/
		if (version_number < WISP_LICENSE_VERSION)
		{
			return LICENSE_CHECK_VERSION;
		}

		/*
		**	Check if a VALIDATION CODE and APPLICATION CODE was found
		*/
		if (valcode[0] == '\0' || appcode[0] == '\0') 
		{
			return(LICENSE_CHECK_UNKNOWN);
		}

		/*
		**	Check if the VALIDATION CODE is valid (against the APPCODE)
		*/
		upper_string(valcode);
		upper_string(appcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode) ||
		    0 != WLIC_ckvalcode(licensekey,appcode,valcode))
		{
			return(LICENSE_CHECK_INVALID);
		}

		break;

	case LICENSE_TIMED:
		if (WLIC_check_timeout(licensedate,expdate))
		{
			return(LICENSE_CHECK_TIMEDOUT);
		}
		break;

	case LICENSE_CLUSTER:
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
		if (! valcode[0]) return(LICENSE_CHECK_UNKNOWN);

		/*
		**	Check if the VALIDATION CODE is right size
		*/
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			return(LICENSE_CHECK_INVALID);
		}
		break;

#ifdef WIN32
	case LICENSE_NETWORK:
		/*
		**	If on the server then validate normally.
		**	If on a client then validate using the server name as the machine id.
		*/

		/*
		**	Check if a VALIDATION CODE was found
		*/
		if (! valcode[0]) return(LICENSE_CHECK_UNKNOWN);

		/*
		**	Get the MACHINE ID
		*/
		if ((rc = WL_getmachineid(machineid)) != 0)
		{
			return(LICENSE_CHECK_UNKNOWN);
		}

		/*
		**	If running on a CLIENT then the machineid is 
		**	generated from the SERVER name.
		*/
		if (0 != strcmp(wispserver(), WL_computername(NULL)) &&
		    0 != strcmp(wispserver(), DEF_WISPSERVER)        )
		{
			WL_encodemachid(wispserver(), machineid);
		}
		
		/*
		**	Check if the VALIDATION CODE is valid
		*/
		upper_string(valcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode) ||
		    0 != WLIC_ckvalcode(licensekey,machineid,valcode))
		{
			return(LICENSE_CHECK_INVALID);
		}
		break;
#endif /* WIN32 */
		
	default:
		return(LICENSE_CHECK_INVALID);
	}

	return(LICENSE_CHECK_OK);
}

/*
**	ROUTINE:	WLIC_get_license_info()
**
**	FUNCTION:	Get the stored license information.
**
**	DESCRIPTION:	Read the license file to get the LICENSE KEY and VALIDATION CODE
**
**	ARGUMENTS:	
**	flickey		The returned LICENSE KEY
**	valcode		The returned VALIDATION CODE (may be empty string if not found)
**	appcode		The returned APPLICATION CODE (may be empty string if not found)
**
**	GLOBALS:	None
**
**	RETURN:		
**	0			Successfully returned license info
**	LICENSE_CHECK_MISSING	License info was missing
**	LICENSE_CHECK_UNKNOWN	Unable to get license info
**
**	WARNINGS:	None
**
*/
int WLIC_get_license_info(char* flickey, char* valcode, char* appcode)
{
	FILE	*fp;
	char	buff[256];
	int	rc;

	/*
	**	Check if license file exists
	*/

	if (0 != access(WLIC_license_filepath(),0000))
	{
		return(LICENSE_CHECK_MISSING);
	}

	/*
	**	Open license file for reading
	*/
	fp = fopen(WLIC_license_filepath(),"r");
	if (!fp)
	{
		return(LICENSE_CHECK_UNKNOWN);
	}

	/*
	**	Read thru the file extracting the LICENSE KEY and the VALIDATION CODE
	*/

	flickey[0] = '\0';
	valcode[0] = '\0';
	appcode[0] = '\0';
	while(fgets(buff,sizeof(buff),fp))
	{
		if (0 == memcmp(buff,"LICENSE-KEY ",12))
		{
			rc = sscanf(buff,"LICENSE-KEY %s",flickey);
			if (rc != 1) return(LICENSE_CHECK_UNKNOWN);
		}
		else if (0 == memcmp(buff,"VALIDATION-CODE ",16))
		{
			rc = sscanf(buff,"VALIDATION-CODE %s",valcode);
			if (rc != 1) return(LICENSE_CHECK_UNKNOWN);
		}
		else if (0 == memcmp(buff,"APPLICATION-CODE ",17))
		{
			rc = sscanf(buff,"APPLICATION-CODE %s",appcode);
			if (rc != 1) return(LICENSE_CHECK_UNKNOWN);
		}
	}
	fclose(fp);

	/*
	**	Check if a LICENSE KEY was found
	*/

	if (!flickey[0])
	{
		return(LICENSE_CHECK_UNKNOWN);
	}

	return 0;
}

/*
**	Routine:	WLIC_check_timeout()
**
**	Function:	To check if the the key has timed out.
**
**	Description:	This routine is passed a low date and a high date and must test if todays date is between the two.
**
**	Input:		lowdate		the low date  YYYYMMDD
**			highdate	the high date YYYYMMDD
**
**	Output:		None
**			
**
**	Return:		0 = OK
**			1 = after high date
**			2 = before low date
**			3 = invalid date
**
**	Warnings:	None
**
**	History:	05/26/92	Written by GSL
**
*/

int WLIC_check_timeout(char lowdate[8], char highdate[8])
{
	char	low_ymd[3], high_ymd[3];
	struct tm *l_tm;
	time_t	now;
	int	t_year, t_month, t_day;
	int	l_year, l_month, l_day;
	int	h_year, h_month, h_day;

	if ( WLIC_packdate(lowdate,low_ymd) ) return(3);
	if ( WLIC_packdate(highdate,high_ymd) ) return(3);

	now = time(NULL);
	l_tm = localtime(&now);
	t_year  = l_tm->tm_year+1900;
	t_month = l_tm->tm_mon+1;
	t_day   = l_tm->tm_mday;

	l_year  = (int)low_ymd[0] + BASEYEAR;
	l_month = (int)low_ymd[1];
	l_day   = (int)low_ymd[2];

	h_year  = (int)high_ymd[0] + BASEYEAR;
	h_month = (int)high_ymd[1];
	h_day   = (int)high_ymd[2];

	if (t_year  > h_year) return(1);
	if (t_year  < l_year) return(2);
	
	if (t_year == h_year && t_month > h_month) return(1);
	if (t_year == l_year && t_month < l_month) return(2);

	if (t_year == h_year && t_month == h_month && t_day > h_day) return(1);
	if (t_year == l_year && t_month == l_month && t_day < l_day) return(2);

	return(0);
}
/*
**	History:
**	$Log: wauthsub.c,v $
**	Revision 1.27  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.26  2003/06/13 17:36:12  gsl
**	ENTERPRISE License
**	
**	Revision 1.25  2003/06/12 20:54:29  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.24  2003/02/13 20:45:02  gsl
**	On unix change the license file from /lib/wisp.license to
**	$WISPCONFIG/wisp.{machineid}.license
**	
**	Revision 1.23  2003/02/11 16:09:32  gsl
**	fix warnings
**	
**	Revision 1.22  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.21  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.20  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.19  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.18  2003/01/03 16:48:52  gsl
**	Move routines to write license file to wlicense.c
**	
**	Revision 1.17  2002/10/14 15:41:48  gsl
**	Expose WLIC_get_license_info()
**	
**	Revision 1.16  2002/07/18 21:04:29  gsl
**	Remove MSDOS code
**	
**	Revision 1.15  2002/07/10 21:05:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.14  2002/07/09 04:13:55  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.13  2002/06/21 03:10:43  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.12  1997/03/21 15:29:55  gsl
**	Split the retrieval of the LICENSE_KEY and VALICATION_CODE into a separate
**	routine in preparation of storing the info in the registry for WIN32
**	
**	Revision 1.11  1997-03-17 11:58:29-05  gsl
**	Add logic for NETWORK license
**
**	Revision 1.10  1996-08-19 18:33:07-04  gsl
**	drcs update
**
**	05/26/92	Written GSL
**	09/25/92	Added LICENSE_CLUSTER logic. GSL
**	09/13/93	Generalized for UniQue. GSL
**
**
*/
