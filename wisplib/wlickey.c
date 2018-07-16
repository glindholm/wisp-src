static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	File:		wlickey.c
**
**	Function:	Licence Key routines
**
**	Routines:	mklickey()		Make License Key
**			bklickey()		Break License Key
**			formatkey()		Format the key XXXX-XXXX-XXXX-XXXX
**			unformatkey()		Unformat the key
**			mkvalcode()		Make a validation code
**			ckvalcode()		Check a validation code
**
**	History:
**			05/19/92	Written GSL
**			05/20/92	Added bklickey() GSL
**
*/

#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "wlicense.h"

#define MAXTRAN		34

/*
**	Routine:	mklickey()		Make License Key
**
**	Function:	To create the license key from it's parts.
**
**	Description:	The parts of the key are first encoded one byte at a time with entran() to make a 15 byte key
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
**
**	Output:		lickey			The license key
**
**	Return:		0 = Success
**			1 = Invalid key component
**
**	Warnings:	None
**
**	History:	5/19/92		Written by GSL
**			5/20/92 	changed platform to be 2 char encoded value.
**
*/

int	mklickey(
		int4	custnum,
		char	platform[2],
		int	lictype,
		char	licdate[8],
		char	expdate[8],
		char	lickey[LICENSE_KEY_SIZE])
{
	char	buff[20];
	int	i, idx;
	int	rc;
	int	checksum;

	sprintf(buff,"%06d",custnum);
	for(i=0,idx=0;i<6;i++,idx++)
	{
		lickey[idx] = buff[i] - '0';
	}

	/*
	**	the platform is already in the format of A-Z1-9
	*/
	lickey[idx++] = detran((char)toupper(platform[0]));				/* Detran the platfrom code, it will be		*/
	lickey[idx++] = detran((char)toupper(platform[1]));				/* validated later.				*/

	lickey[idx++] = lictype;

	if (rc = packdate(licdate,&lickey[idx])) return(rc);
	idx += 3;

	if (0 == memcmp(expdate,"00000000",8))					/* No expiration date				*/
	{
		memset(&lickey[idx],'\0',3);
	}
	else
	{
		if (rc = packdate(expdate,&lickey[idx])) return(rc);
	}

	checksum = checksummem(lickey,LICENSE_KEY_SIZE-1,MAXTRAN);
	lickey[LICENSE_KEY_SIZE-1] = checksum;

	/*
	**	Do a rolling-up with offset encryption.  (Don't do the checksum)
	*/
	for(i=0;i<LICENSE_KEY_SIZE-1;i++)
	{
		if (lickey[i] < 0 || lickey[i] > MAXTRAN) return(1);
		lickey[i] = (lickey[i] + i + checksum) % MAXTRAN;
	}

	/*
	**	Use entran to convert each of the bytes in the key
	*/
	for(i=0;i<LICENSE_KEY_SIZE;i++)
	{
		lickey[i] = entran((int)lickey[i]);
	}

		
	return(0);
}

/*
**	Routine:	bklickey()		Break License Key
**
**	Function:	To break the license key into it's parts.
**
**	Description:	This routine in the opposite of mklickey, it exactly reverses the process in mklickey().
**
**	Input:		licensekey		The license key
**
**	Output:		custnum			The customer number
**			platform		The platform type
**			lictype			The license type
**			licdate			The license date YYYYMMDD
**			expdate			The expiration date YYYYMMDD (No expiration == 00000000)
**
**	Return:		0 = Success
**			1 = Invalid key
**
**	Warnings:	None
**
**	History:	5/20/92		Written by GSL
**
*/

int	bklickey(
		int4	*custnum,
		char	*platform,
		int	*lictype,
		char	licdate[8],
		char	expdate[8],
		char	licensekey[LICENSE_KEY_SIZE])
{
	char	lickey[LICENSE_KEY_SIZE];
	int	i, idx;
	int	rc;
	int	checksum;

	/*
	**	Use detran to convert each of the bytes in the key
	*/
	for(i=0;i<LICENSE_KEY_SIZE;i++)
	{
		lickey[i] = detran(licensekey[i]);
		if (lickey[i] < 0) return(1);
	}

	/*
	**	Undo a rolling-up with offset encryption. (Don't do the checksum)
	*/
	checksum = lickey[LICENSE_KEY_SIZE-1];
	for(i=0;i<LICENSE_KEY_SIZE-1;i++)
	{
		if (lickey[i] < 0 || lickey[i] > MAXTRAN) return(1);

		lickey[i] = (lickey[i] - i - checksum + MAXTRAN*2) % MAXTRAN;
	}


	if ( checksum != checksummem(lickey,LICENSE_KEY_SIZE-1,MAXTRAN) ) return(1);

	*custnum = 0;
	for(i=0,idx=0;i<6;i++,idx++)
	{
		*custnum = (*custnum * 10) +  lickey[idx];
	}

	platform[0] = entran((int)lickey[idx++]);
	platform[1] = entran((int)lickey[idx++]);

	*lictype  = lickey[idx++];

	if (rc = unpkdate(licdate,&lickey[idx])) return(rc);
	idx += 3;

	if (0 == memcmp(&lickey[idx],"\000\000\000",3))				/* No expiration date				*/
	{
		memset(expdate,'0',8);
	}
	else
	{
		if (rc = unpkdate(expdate,&lickey[idx])) return(rc);
	}

	
	return(0);
}

/*
**	Routine:	formatkey()
**
**	Function:	To format the key for printing.
**
**	Description:	Change to XXXX-XXXX-XXXX-XXXX format and null terminate.
**
**	Input:		lickey		the unformated license key
**			
**
**	Output:		formkey		the formated key
**			
**
**	Return:		None
**
**	Warnings:	No error checking is done.
**
**	History:	05/21/92	Written by GSL
**
*/

void formatkey(char* lickey, char* formkey)
{
	sprintf(formkey,"%4.4s-%4.4s-%4.4s-%4.4s",&lickey[0],&lickey[4],&lickey[8],&lickey[12]);
}

/*
**	Routine:	unformatkey()
**
**	Function:	To change a formated key into an unformated key.
**
**	Description:	Change from XXXX-XXXX-XXXX-XXXX format to unformated.
**			This will also work if the input key is not formated.
**			It simply moves the first 16 not '-' characters in formkey into lickey.
**
**	Input:		formkey		the formated key
**			
**
**	Output:		lickey		the unformated license key
**			
**
**	Return:		None
**
**	Warnings:	No error checking is done.
**
**	History:	05/21/92	Written by GSL
**
*/

void unformatkey(char* lickey, char* formkey)
{
	int	i1,i2;

	for(i1=0,i2=0;i1<LICENSE_KEY_SIZE;i2++)
	{
		if (formkey[i2] != '-')
		{
			lickey[i1++] = formkey[i2];
		}
	}
}


/*
**	Routine:	mkvalcode()
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

void mkvalcode(char lickey[LICENSE_KEY_SIZE],char* machineid,char valcode[3])
{
	valcode[0] = entran(checksummem(lickey,LICENSE_KEY_SIZE,MAXTRAN));
	valcode[1] = entran(checksummem(machineid,strlen(machineid),MAXTRAN));
	valcode[2] = entran(checksummem(valcode,2,MAXTRAN));
}


/*
**	Routine:	ckvalcode()
**
**	Function:	To check a VALIDATION CODE
**
**	Description:	This routine accepts a LICENSE KEY, MACHINE ID and a VALIDATION CODE and checks if the code is valid.
**
**	Input:		lickey		the LICENSE KEY
**			machineid	the MACHINE ID
**			valcode		the VALIDATION CODE
**			
**
**	Output:		None
**			
**
**	Return:		0 = a valid code
**			1 = invalid code
**
**	Warnings:	None
**
**	History:	05/22/92	Written by GSL
**
*/

int ckvalcode(char lickey[LICENSE_KEY_SIZE],char* machineid,char valcode[3])
{
	if ( valcode[0] == entran(checksummem(lickey,LICENSE_KEY_SIZE,MAXTRAN)) &&
	     valcode[1] == entran(checksummem(machineid,strlen(machineid),MAXTRAN)) &&
	     valcode[2] == entran(checksummem(valcode,2,MAXTRAN))                      )
	{
		return(0);
	}
	else
	{
		return(1);
	}
}

/*
**	History:
**	$Log: wlickey.c,v $
**	Revision 1.7  1996-08-19 18:33:18-04  gsl
**	drcs update
**
**
**
*/
