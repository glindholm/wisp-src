/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
**	File:		machid.c
**
**	Purpose:	To get machine id.
**
**	Routines:	
**	WL_getmachineid()	Get machine id.
**
**
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "idsisubs.h"
#include "intdef.h"
#include "wlicense.h"
#include "wispnt.h"
#include "assert.h"
#include "machid.h"
#include "wmalloc.h"
#include "wisplib.h"
#include "platsubs.h"

/*
**	Routine:	WL_getmachineid()
**
**	Function:	To get the MACHINE ID. (UNIX)
**
**	Description:	This routine will return the MACHINE ID if one is available.  
**
**	Input:		None
**			
**
**	Output:		machineid	The MACHINE ID 
**			
**
**	Return:		0 = success
**			1 = no machine id and the stat failed on the license file.
**
**	Warnings:	Ensure that the license file in created first before calling this routine.
**			Machineid must be large enough to hold the result.
**
*/

#ifdef unix
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <unistd.h>

#ifdef AIX
int WL_getmachineid(char* machineid)
{
	struct utsname 	uname_s;
	*machineid = '\0';

	if (uname(&uname_s))
	{
		return(1);
	}

	strcpy(machineid,uname_s.machine);

	WL_upper_string(machineid);
	return(0);
}
#endif /* AIX */

#ifdef HPUX
int WL_getmachineid(char* machineid)
{
	struct utsname 	uname_s;
	*machineid = '\0';

	if (uname(&uname_s))
	{
		return(1);
	}

	strcpy(machineid,uname_s.__idnumber);

	WL_upper_string(machineid);
	return(0);
}
#endif /* HPUX */

#ifdef SCO
int WL_getmachineid(char* machineid)
{
	struct scoutsname sco_buff;
	*machineid = '\0';

	if (-1 == __scoinfo(&sco_buff, sizeof(sco_buff)))
	{
		return(1);
	}
	memcpy(machineid, sco_buff.sysserial, sizeof(sco_buff.sysserial));
	machineid[sizeof(sco_buff.sysserial)] = '\0';

	WL_upper_string(machineid);
	return(0);
}
#endif /* SCO */

#if defined(LINUX) || defined(OSF1_ALPHA)
int WL_getmachineid(char* machineid)
{
	char name[80];
	WL_computername(name);
	WL_GetMachineIdFromName(machineid, name, WL_platform_code());
	return(0);
}
#endif /* LINUX || OSF1_ALPHA */

#if defined(SOLARIS) || defined(UNIXWARE)
#include <sys/systeminfo.h>

int WL_getmachineid(char* machineid)
{
	*machineid = '\0';
	if ( -1 == sysinfo(SI_HW_SERIAL, machineid, MAX_MACHINEID_LENGTH))
	{
		return(1);
	}

	WL_upper_string(machineid);
	return(0);
}
#endif

#ifdef OLD /* DO NOT USE */
/*
** This was the old style used up thru 4.4.06 for machines that
** did not provide a hardware id.
** 
** It used the inode of the license file with a leading 'I' as
** the machine id.
** 
** It's biggest problem is if the license file was deleted and
** recreated the machine id would change.
** Also the machine id could not be determined unless the
** license file exists.
** 
** int WL_getmachineid(char* machineid)  OLD INODE STYLE
** {
** 	long	inode = WL_inode(WLIC_license_filepath());
** 	*machineid = '\0';
** 
** 	if (0==inode)
** 	{
** 		return(1);
** 	}
** 
** 	sprintf(machineid,"I%ld", inode);
** 
** 	WL_upper_string(machineid);
** 	return(0);
** }
*/
#endif

/*
**	ROUTINE:	WL_computername()
**
**	FUNCTION:	Return the computer name.
**
**	DESCRIPTION:	Get the computer name from uname().
**			The name will be trucated at the first period.
**
**	ARGUMENTS:	
**	cname		Location to store the name or NULL 
**
**	GLOBALS:	none
**
**	RETURN:		cname or if NULL a pointer to an internal static area which contains the name.
**
**	WARNINGS:	None
**
*/
char *WL_computername(char *cname)
{
	static char* the_computername = NULL;

	if (NULL == the_computername)
	{
		struct utsname unix_name;
		char	*tmp_name = NULL;

		if ( -1 != uname(&unix_name) )
		{
			char	*ptr;

			tmp_name = unix_name.nodename;

			/*
			**	Some systems return a fully qualified name 
			**	(e.g. ozone.fortmyers.neom.com) instead of just the 
			**	computer name. So truncate the name at the first period.
			*/
			ptr = strchr(tmp_name,'.');
			if (NULL != ptr)
			{
				*ptr = '\0';
			}
		}

		if (!tmp_name || !*tmp_name)
		{
			tmp_name = "(UNKNOWN)";
		}

		the_computername = wisp_strdup(tmp_name);
	}

	if (NULL == cname)
	{
		return the_computername;
	}
	else
	{
		strcpy(cname, the_computername);
		return cname;
	}

}


#endif /* unix */



#ifdef WIN32

/*
**	Routine:	WL_getmachineid()
**
**	Function:	To get the MACHINE ID. (WIN32)
**
**	Description:	This routine will return the MACHINE ID if one is available. 
**			The machine id is an encoded version of the computername.
**			The machineid will be at least 6 chars long.
**
**
**	Input:		None
**			
**
**	Output:		machineid	The MACHINE ID 
**			
**
**	Return:		0 = success
**			1 = no machine id and the stat failed on the license file.
**
**	Warnings:	Machineid must be large enough to hold the result.
**
*/
int WL_getmachineid(char* machineid)
{
	char	compname[256];
	int	len;
	
	WL_computername(compname);
	len = strlen(compname);
	
	ASSERT(len < sizeof(compname));

	if (0==strcmp(compname, DEF_COMPUTERNAME))
	{
		return 1;
	}
	if (len < 1)
	{
		return 1;
	}

	WL_encodemachid(compname, machineid);
	
	return 0;
}
#endif /* WIN32 */

/*
**	ROUTINE:	WL_encodemachid()
**
**	FUNCTION:	Convert a machine name into an encoded character string
**
**	DESCRIPTION:	Each byte in source is converted to an encode character.
**			This is a one way encoding.
**			The low nibble of each byte plus it's position is used to
**			create a hex character.
**
**	ARGUMENTS:	
**	source		The source string. (A machine name)
**	target		The resulting encoded string.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	Target must be as long as source.
**
*/
void WL_encodemachid(const char *source, char *target)
{
	const char* src = source;
	char	*trg, *mask;
	int	len, i;
	char	lsource[10];
	
	len = strlen(source);
	/*
	**	Ensure computername is at least 6 chars long by duplicating it.
	*/
	if (len < 6)
	{
		strcpy(lsource,source);
		for (i=0; len+i < 6; i++ )
		{
			lsource[len+i] = lsource[i];
		}
		lsource[6] = '\0';

		src = lsource;
		len = 6;
	}
	
	trg = target;
	mask = "0123456789ABCDEF";

	while (len-- > 0)
	{
		*trg++ = mask[ ((*src++ & 0x0F) + len) % 16 ];
	}
	*trg = '\0';
}

void WL_GetMachineIdFromName(char* machineid, const char* computername, const char* platform_code)
{
	char name[80];
	char* ptr;
	*machineid = '\0';

	strcpy(name, computername);

	if (strlen(name) < 10)
	{
		strcat(name,"0102010301");
		name[10] = '\0';
	}

	sprintf(machineid,"%s0%s0X0", platform_code, name);

	/*
	** Remove unwanted file name characters from machine id
	*/
	for(ptr = machineid; *ptr != '\0'; ptr++)
	{
		if (!isalnum((int)(*ptr)))
		{
			*ptr = '-';
		}
	}

	WL_upper_string(machineid);
}

#ifdef DEBUG
#ifdef MAIN
main()
{
	char	id[80], idx[80];

	if ( WL_getmachineid(id) )
	{
		printf("No machine_id\n");
	}
	else
	{
		printf("machine_id = %s\n", id);
	}

	WL_encodemachid(id, idx);
	printf("Encoded machine id = %s\n", idx);
	
	exit(0);
}
#endif /* MAIN */
#endif /* DEBUG */

/*
**	History:
**	$Log: machid.c,v $
**	Revision 1.31  2004/06/14 15:56:43  gsl
**	Fix isalnum() warning
**	
**	Revision 1.30  2004/06/14 15:42:57  gsl
**	make external the routines to generate MachineId from the Unix Machine Name
**	Used by Linux and Alpha. Add M function to wauthorize to generate machine id for Window or Linux or Alpha from the machine name.
**	
**	Revision 1.29  2004/06/11 15:06:03  gsl
**	Move WL_encodemachid() out of WIN32 only code
**	
**	Revision 1.28  2003/02/13 21:29:25  gsl
**	fix isalnum() test
**	
**	Revision 1.27  2003/02/13 20:43:47  gsl
**	Remove old INODE based machine id logic.
**	Change OSF1_ALPHA machine id logic to be the same as LINUX which
**	is based on machine name instead of inode.
**	
**	Revision 1.26  2003/02/11 16:10:54  gsl
**	Remove OLD code
**	Rearrange WL_getmachineid() so each #define has its own routine
**	
**	Revision 1.25  2003/01/31 21:40:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.24  2003/01/31 18:25:18  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.23  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.22  2002/11/14 19:04:30  gsl
**	LINUX base machineid on the computer name
**	
**	Revision 1.21  2002/11/14 18:11:01  gsl
**	LINUX base machineid on the computer name
**	
**	Revision 1.20  2002/10/04 21:00:55  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.19  2002/07/11 14:33:57  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.18  2002/07/10 21:05:19  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.17  2002/07/09 04:13:59  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.16  2002/07/02 21:15:26  gsl
**	Rename wstrdup
**	
**	Revision 1.15  2001/09/27 14:13:06  gsl
**	Add hardware machine id support for SOLARIS, SCO, and UNIXWARE
**	
**	Revision 1.14  1998-07-08 09:26:37-04  gsl
**	Fix computername() to truncate the name at the first period.
**
**	Revision 1.13  1997-12-04 18:13:20-05  gsl
**	changed to wispnt.h
**
**	Revision 1.12  1997-10-17 13:43:30-04  gsl
**	Fix bug on NT when server name was less then 6 characters.
**	The name now gets expanded in WL_encodemachid() instead of WL_getmachineid()
**	so that its the same everywhere.
**
**	Revision 1.11  1997-03-06 16:36:09-05  gsl
**	Add computername() for unix
**
**	Revision 1.10  1997-03-06 14:59:13-05  gsl
**	For NT ensure the machine id is at least 6 chars long
**
**	Revision 1.9  1997-02-28 17:48:32-05  gsl
**	Add real getmachineid() for WIN32
**
**	Revision 1.8  1996-08-19 18:32:28-04  gsl
**	drcs update
**
**
**
*/
