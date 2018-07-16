			/************************************************************************/
			/*									*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#if defined(unix) || defined(MSDOS)

/*
**	File:		wauthsub.c
**
**	Function:	subroutines used by wauthorize and validate_license()
**
**	Routines:	entran()		Translate a number 0-35 to a printable char
**			detran()		Reverse on entran()
**			checksummem()		Calculate a checksum
**			packdate()		Pack an 8 char date into 3 bytes
**			unpkdate()		Unpack a date created by packdate()
**			lictypename()		Return the text string name of the license type code.
**			create_license_file()	Create the license file.
**			getmachineid()		Get the MACHINE ID.
**			write_license()		Write the info to the license file.
**			validate_license()	Read the license file and validate the license.
**			check_timeout()		Check if the license has timed-out
**
**	History:
**	05/26/92	Written GSL
**	09/25/92	Added LICENSE_CLUSTER logic. GSL
**	09/13/93	Generalized for UniQue. GSL
**      11/09/93        copied from wisplib wauthsub.c into unique to
**                      simplify porting of Unique.  JEC
**
*/

#include <sys/types.h>
#include <sys/stat.h>
#ifdef unix
#include <sys/utsname.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <string.h>

#include "idsistd.h"
#include "wlicense.h"

/*
**	Routine:	entran()
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
**	09/13/93	Generalized to use lic_trantable(). GSL
**
*/

#ifdef OLD
static	char	*tt_tran = "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789";
static	char	*tt_tran = "QAZ9CX1EU2GR3IO4YL5MJN6PH7SFT8VDWKB";	/* WISP */
static	char	*tt_tran = "YL5MJN6PH7SFT8VDWKBQAZ9CX1EU2GR3IO4";	/* UniQue */
#endif

char	entran(innum)
int	innum;
{
	char	*tt_tran = lic_trantable();
	if (innum >= 0 && innum <= 34)
	{
		return( tt_tran[innum] );
	}

	return('#');
}

/*
**	Routine:	detran()
**
**	Function:	Detranslate a character translated by entran() back into a number 0-34.
**			
**	Description:	This routine reverses the entran() routine by using the same translation table to map a character
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
**	09/13/93	Generalized to use lic_trantable(). GSL
*/

int	detran(inchar)
char	inchar;
{
	char	*ptr;
	char	*tt_tran = lic_trantable();

	ptr = strchr(tt_tran, toupper(inchar));					/* Search for the char in the table (uppercase)	*/
	if (ptr)
	{
		return( ptr - tt_tran );					/* calculate the number				*/
	}

	return -1;								/* invalid character				*/
}

/*
**	Routine:	checksummem()
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

int	checksummem(instr,size,mod)
unsigned char	*instr;
int	size;
int	mod;
{
	int	i;
	int	cs;

	cs = 0;
	for (i=0; i<size; i++)
	{
		cs = (int)(cs + instr[i]) % mod;
	}

	return(cs);
}

/*
**	Routine:	packdate()
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
**			This routine can handle years up to 127 (2117) but if used with entran() it has a limit of 34 (2024).
**			This routine can only handle dates since 1990.
**
**	History:	05/20/92	Written by GSL
**
*/

#define BASEYEAR	1990
#define YEARMONTHS	12
#define MONTHDAYS	31
#define MAXPACKYEARS	127

int packdate(yyyymmdd,ymd)
char	*yyyymmdd;
char	*ymd;
{
	int	i;
	int	year, month, day;

	for(i=0;i<8;i++)
	{
		if (!isdigit(yyyymmdd[i])) return(1);
	}

	year =  (yyyymmdd[0]-'0')*1000 + (yyyymmdd[1]-'0')*100 + (yyyymmdd[2]-'0')*10 + (yyyymmdd[3]-'0');
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
**	Routine:	unpkdate()
**
**	Function:	Unpack a date that was packed by packdate().
**
**	Description:	This routine converts a 3 byte packed date into a 8 character date by exactly reversing the
**			process used in packdate().
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

int unpkdate(yyyymmdd,ymd)
char	*yyyymmdd;
char	*ymd;
{
	int	i;
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
**	Routine:	lictypename()
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

char *lictypename(lictype)
int	lictype;
{
static	char	*l_single	= "SINGLE";
static	char	*l_unlimited	= "UNLIMITED";
static	char	*l_timed	= "TIMED";
static	char	*l_cluster	= "CLUSTER";
static	char	*l_invalid	= "INVALID";

	switch(lictype)
	{
	case LICENSE_SINGLE:	return(l_single);	break;
	case LICENSE_UNLIMITED:	return(l_unlimited);	break;
	case LICENSE_TIMED:	return(l_timed);	break;
	case LICENSE_CLUSTER:	return(l_cluster);	break;
	default:		return(l_invalid);	break;
	}
}

/*
**	Routine:	create_license_file()
**
**	Function:	To create the license file
**
**	Description:	This routine will create the license file if it doesn't already exist.
**			If the temp inode file exists it will rename it to be the license file.
**
**	Input:		None
**			
**
**	Output:		/lib/{product}.license
**			
**
**	Return:		0 = success
**			1 = unable to create
**
**	Warnings:	None
**
**	History:	05/26/92	Written by GSL
**			07/31/92	Added rename of temp inode file. GSL
**
*/

int create_license_file()
{
	FILE	*fh;

	if ( 0 != access(license_filepath(),0000) )					/* check if file exists			*/
	{
		if ( 0 != access(x_license_filepath(),0000) )				/* Check for temp inode file		*/
		{
			fh = fopen(license_filepath(),"w");
			if (!fh)
			{
				return(1);
			}
			fclose(fh);
		}
		else									/* Rename the temp file			*/
		{
#ifdef unix
			link(x_license_filepath(),license_filepath());			/* link the temp file to license file	*/
			unlink(x_license_filepath());					/* Remove the temp file link		*/
#endif
#ifdef MSDOS
			rename(x_license_filepath(),license_filepath());
#endif
			if ( 0 != access(license_filepath(),0000) )			/* Make sure it worked			*/
			{
				return(1);
			}
		}
	}

	return(0);
}


/*
**	Routine:	getmachineid()
**
**	Function:	To get the MACHINE ID.
**
**	Description:	This routine will return the MACHINE ID if one is available.  If there is no MACHINE ID it will
**			"fake it" by returning the inode number of the license file.
**
**	Input:		None
**			
**
**	Output:		machineid	The MACHINE ID (or inode number)
**			
**
**	Return:		0 = success
**			1 = no machine id and the stat failed on the license file.
**
**	Warnings:	Ensure that the license file in created first before calling this routine.
**			Machineid must be large enough to hold the result.
**
**	History:	05/26/92	Written by GSL
**
*/

#ifdef unix
int getmachineid(machineid)
char	*machineid;
{
	struct stat 	stat_buf;
	struct utsname 	uname_s;
	int4	inode;

	*machineid = '\0';							/* Init machineid to NULL string		*/

#ifdef AIX
	if (uname(&uname_s))
	{
		return(1);
	}

	strcpy(machineid,uname_s.machine);
#endif
#ifdef HPUX
	if (uname(&uname_s))
	{
		return(1);
	}

	strcpy(machineid,uname_s.__idnumber);
#endif

	/*
	**	If no machine id use the inode.
	*/
	if (! *machineid)
	{
		if (stat(license_filepath(),&stat_buf))
		{
			return(1);
		}

		inode = (int4) stat_buf.st_ino;
		sprintf(machineid,"I%d",inode);
	}

	upper_string(machineid);						/* Shift to upper case just in case alphas	*/
	return(0);

}
#endif /* unix */

/*
**	Routine:	write_license()
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

int write_license(custname,custnum,platform,licensekey,lictype,licdate,expdate,machineid,valcode)
char	*custname;
int4	custnum;
char	platform[2];
char	licensekey[LICENSE_KEY_SIZE];
int	lictype;
char	licdate[8];
char	expdate[8];
char	*machineid;
char	valcode[VALIDATION_CODE_SIZE];
{
	FILE	*fp;
	char	flickey[80];
	char	platname[80];
	char	licdatebuff[20];
	char	expdatebuff[20];

	if (plat_code(platform,platname,NULL))					/* expand the platform code			*/
	{
		strcpy(platname,"??");
	}

	formatkey(licensekey,flickey);						/* format the key				*/

	sprintf(licdatebuff,"%4.4s/%2.2s/%2.2s",&licdate[0],&licdate[4],&licdate[6]);

	if (0 == memcmp(expdate,"00000000",8))
	{
		strcpy(expdatebuff,"None");
	}
	else
	{
		sprintf(expdatebuff,"%4.4s/%2.2s/%2.2s",&expdate[0],&expdate[4],&expdate[6]);
	}

	fp = fopen(license_filepath(),"w");
	if (!fp)
	{
		return(1);
	}

	fprintf(fp,		"LICENSEE           %s\n",custname);
	fprintf(fp,		"CUSTOMER-NUMBER    %06d\n",custnum);
	fprintf(fp,		"PLATFORM           %2.2s - %s\n",platform,platname);
	fprintf(fp,		"LICENSE-KEY        %s\n",flickey);
	fprintf(fp,		"LICENSE-TYPE       %s\n",lictypename(lictype));
	fprintf(fp,		"LICENSE-DATE       %s\n",licdatebuff);
	fprintf(fp,		"EXPIRATION-DATE    %s\n",expdatebuff);
	if (machineid && *machineid)
	{
		fprintf(fp,	"MACHINE-ID         %s\n",machineid);
	}
	if (valcode && *valcode)
	{
		fprintf(fp,	"VALIDATION-CODE    %3.3s\n",valcode);
	}

	if (ferror(fp))								/* Check for an error in writing		*/
	{
		fclose(fp);
		return(2);
	}

	fclose(fp);

	chmod(license_filepath(),0444);						/* Make file read-only				*/

	return(0);
}


/*
**	Routine:	validate_license()
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

int validate_license()
{
	int4	custnum;
	char	platform[3];
	int	licensetype;
	char	licensedate[20];
	char	expdate[20];
	char	licensekey[80];
	char	machineid[80];
	char	valcode[80];

	FILE	*fp;
	char	buff[256];
	char	flickey[80];
	int	rc;

	/*
	**	Check if license file exists
	*/

	if (0 != access(license_filepath(),0000))
	{
		return(LICENSE_MISSING);
	}

	/*
	**	Open license file for reading
	*/
	fp = fopen(license_filepath(),"r");
	if (!fp)
	{
		return(LICENSE_UNKNOWN);
	}

	/*
	**	Read thru the file extracting the LICENSE KEY and the VALIDATION CODE
	*/

	flickey[0] = '\0';
	valcode[0] = '\0';
	while(fgets(buff,sizeof(buff),fp))
	{
		if (0 == memcmp(buff,"LICENSE-KEY ",12))
		{
			rc = sscanf(buff,"LICENSE-KEY %s",flickey);
			if (rc != 1) return(LICENSE_UNKNOWN);
		}
		else if (0 == memcmp(buff,"VALIDATION-CODE ",16))
		{
			rc = sscanf(buff,"VALIDATION-CODE %s",valcode);
			if (rc != 1) return(LICENSE_UNKNOWN);
		}
	}
	fclose(fp);

	/*
	**	Check if a LICENSE KEY was found
	*/

	if (!flickey[0])
	{
		return(LICENSE_UNKNOWN);
	}
 
	/*
	**	Check if the LICENSE KEY is a valid key
	*/

	upper_string(flickey);
	unformatkey(licensekey,flickey);
	if (bklickey(&custnum,platform,&licensetype,licensedate,expdate,licensekey))
	{
		return(LICENSE_INVALID);
	}

	/*
	**	Check if the LICENSE KEY is valid for this platform
	*/

	if (valplat(platform))
	{
		return(LICENSE_INVALID);
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
		if (! valcode[0]) return(LICENSE_UNKNOWN);

		/*
		**	Get the MACHINE ID
		*/
		if (rc = getmachineid(machineid))
		{
			return(LICENSE_UNKNOWN);
		}

		/*
		**	Check if the VALIDATION CODE is valid
		*/
		upper_string(valcode);
		if (VALIDATION_CODE_SIZE != strlen(valcode) ||
		    0 != ckvalcode(licensekey,machineid,valcode))
		{
			return(LICENSE_INVALID);
		}
		break;

	case LICENSE_UNLIMITED:
		break;

	case LICENSE_TIMED:
		if (check_timeout(licensedate,expdate))
		{
			return(LICENSE_TIMEDOUT);
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
		if (! valcode[0]) return(LICENSE_UNKNOWN);

		/*
		**	Check if the VALIDATION CODE is right size
		*/
		if (VALIDATION_CODE_SIZE != strlen(valcode))
		{
			return(LICENSE_INVALID);
		}
		break;

	default:
		return(LICENSE_INVALID);
		break;
	}

	return(LICENSE_OK);
}

/*
**	Routine:	check_timeout()
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

int check_timeout(lowdate,highdate)
char	lowdate[8];
char	highdate[8];
{
	int	rc;
	char	low_ymd[3], high_ymd[3];
	struct tm *l_tm;
	time_t	now;
	int	t_year, t_month, t_day;
	int	l_year, l_month, l_day;
	int	h_year, h_month, h_day;

	if ( packdate(lowdate,low_ymd) ) return(3);
	if ( packdate(highdate,high_ymd) ) return(3);

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
#endif /* unix || MSDOS */
