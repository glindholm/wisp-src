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
**	Routines:	
**			bklickey()		Break License Key
**			formatkey()		Format the key XXXX-XXXX-XXXX-XXXX
**			unformatkey()		Unformat the key
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
		if (lickey[i] < 0 || lickey[i] > WLIC_MAXTRAN) return(1);

		lickey[i] = (lickey[i] - i - checksum + WLIC_MAXTRAN*2) % WLIC_MAXTRAN;
	}


	if ( checksum != checksummem(lickey,LICENSE_KEY_SIZE-1,WLIC_MAXTRAN) ) return(1);

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

void formatkey(const char* lickey, char* formkey)
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

void unformatkey(char* lickey, const char* formkey)
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
	if ( valcode[0] == entran(checksummem(lickey,LICENSE_KEY_SIZE,WLIC_MAXTRAN)) &&
	     valcode[1] == entran(checksummem(machineid,strlen(machineid),WLIC_MAXTRAN)) &&
	     valcode[2] == entran(checksummem(valcode,2,WLIC_MAXTRAN))                      )
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
**	Revision 1.8.2.1  2003/01/03 15:09:01  gsl
**	Move the license gen stuff out of runtime into wauthorize.c
**	
**	Revision 1.8  1998/12/18 18:28:59  gsl
**	fix templates
**	
**	Revision 1.7  1996-08-19 18:33:18-04  gsl
**	drcs update
**
**
**
*/
