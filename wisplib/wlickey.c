/*
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
*/

/*
**	File:		wlickey.c
**
**	Function:	Licence Key routines
**
**	Routines:	
**			WLIC_bklickey()		Break License Key
**			WLIC_formatkey()	Format the key XXXX-XXXX-XXXX-XXXX
**			WLIC_unformatkey()	Unformat the key
**			WLIC_ckvalcode()	Check a validation code
**
*/

#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "wlicense.h"



/*
**	Routine:	WLIC_bklickey()		Break License Key
**
**	Function:	To break the license key into it's parts.
**
**	Description:	This routine in the opposite of WLIC_mklickey, it exactly reverses the process in WLIC_mklickey().
**
**	Input:		licensekey		The license key
**
**	Output:		custnum			The customer number
**			platform		The platform type
**			lictype			The license type
**			licdate			The license date YYYYMMDD
**			expdate			The expiration date YYYYMMDD (No expiration == 00000000)
**			version_number		2 digit number
**
**	Return:		0 = Success
**			1 = Invalid key
**
**
*/

int	WLIC_bklickey(
		int4	*custnum,
		char	*platform,
		int	*lictype,
		char	licdate[8],
		char	expdate[8],
		int4	*version_number,
		char	licensekey[LICENSE_KEY_SIZE])
{
	char	lickey[LICENSE_KEY_SIZE];
	int	i, idx;
	int	rc;
	int	checksum;

	/*
	**	Use WLIC_detran to convert each of the bytes in the key
	*/
	for(i=0;i<LICENSE_KEY_SIZE;i++)
	{
		lickey[i] = WLIC_detran(licensekey[i]);
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


	if ( checksum != WLIC_checksummem(lickey,LICENSE_KEY_SIZE-1,WLIC_MAXTRAN) ) return(1);

	/*
	**	6 byte customer number (offset 0-5)
	*/
	*custnum = 0;
	for(i=0,idx=0;i<6;i++,idx++)
	{
		*custnum = (*custnum * 10) +  lickey[idx];
	}

	/*
	**	2 byte platform	(offset 6,7)
	*/
	platform[0] = WLIC_entran((int)lickey[idx++]);
	platform[1] = WLIC_entran((int)lickey[idx++]);

	/*
	**	1 byte license type (offset 8)
	*/
	*lictype  = lickey[idx++];

	/*
	**	3 byte license date (offset 9-11)
	**
	**	Valid dates are between 19900101 and 20241231
	*/
	if ((rc = WLIC_unpkdate(licdate,&lickey[idx]))) return(rc);
	idx += 3;

	/*
	**	3 byte expire date (offset 12-14)
	*/
	*version_number = 0;
	memset(expdate,'0',8);
	if (*lictype == LICENSE_TIMED)
	{
		if ((rc = WLIC_unpkdate(expdate,&lickey[idx]))) return(rc);
	}
	else if (*lictype == LICENSE_ENTERPRISE)
	{
		*version_number = (lickey[idx+0] * 10) + lickey[idx+1];
	}
	
	return(0);
}

/*
**	Routine:	WLIC_formatkey()
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

void WLIC_formatkey(const char* lickey, char* formkey)
{
	sprintf(formkey,"%4.4s-%4.4s-%4.4s-%4.4s",&lickey[0],&lickey[4],&lickey[8],&lickey[12]);
}

/*
**	Routine:	WLIC_unformatkey()
**
**	Function:	To change a formated key into an unformated key.
**
**	Description:	Change from XXXX-XXXX-XXXX-XXXX format to unformated.
**			This will also work if the input key is not formated.
**			It simply moves the first 16 non '-' or ' ' characters 
**			in formkey into lickey.
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
*/

void WLIC_unformatkey(char* lickey, const char* formkey)
{
	int	i1,i2;

	for(i1=0,i2=0;i1<LICENSE_KEY_SIZE;i2++)
	{
		if (formkey[i2] != '-' &&
		    formkey[i2] != ' '	)
		{
			lickey[i1++] = formkey[i2];
		}
	}
}


/*
**	Routine:	WLIC_ckvalcode()
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

int WLIC_ckvalcode(char lickey[LICENSE_KEY_SIZE],char* machineid,char valcode[3])
{
	if ( valcode[0] == WLIC_entran(WLIC_checksummem(lickey,LICENSE_KEY_SIZE,WLIC_MAXTRAN)) &&
	     valcode[1] == WLIC_entran(WLIC_checksummem(machineid,strlen(machineid),WLIC_MAXTRAN)) &&
	     valcode[2] == WLIC_entran(WLIC_checksummem(valcode,2,WLIC_MAXTRAN))                      )
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
**	Revision 1.14  2003/06/12 20:54:29  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.13  2003/02/04 17:22:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.12  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.11  2002/12/31 16:25:44  gsl
**	Move the license key generation stuff to wauthoize.c
**	
**	Revision 1.10  2002/12/04 16:59:39  gsl
**	Allow spaces or dashes in a license key
**	
**	Revision 1.9  2002/07/10 21:05:35  gsl
**	Fix globals WL_ to make unique
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
