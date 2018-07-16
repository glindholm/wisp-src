static char copyright[]="Copyright (c) 1995-1997 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		machid.c
**
**	Purpose:	To get machine id.
**
**	Routines:	
**	getmachineid()	Get machine id.
**	low_disk_id()	Get machine id based on disk serial number
**	hide_file()	Make file "hidden"
**
**
*/

#include <string.h>

#include "intdef.h"
#include "wlicense.h"
#include "wispnt.h"
#include "assert.h"
#include "machid.h"
#include "wmalloc.h"

/*
**	Routine:	getmachineid()
**
**	Function:	To get the MACHINE ID. (UNIX)
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
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>

#if defined(SOLARIS) || defined(UNIXWARE)
#include <sys/systeminfo.h>
#endif

int getmachineid(char* machineid)
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

#ifdef SCO
	{
		struct scoutsname sco_buff;
		if (-1 == __scoinfo(&sco_buff, sizeof(sco_buff)))
		{
			return(1);
		}
		memcpy(machineid, sco_buff.sysserial, sizeof(sco_buff.sysserial));
		machineid[sizeof(sco_buff.sysserial)] = '\0';
	}
	
#endif

#if defined(SI_HW_SERIAL)
	/*
	 *	This is used on SOLARIS and looks like it is also available on UNIXWARE
	 */
	if (! *machineid)
	{
		if ( -1 == sysinfo(SI_HW_SERIAL, machineid, MAX_MACHINEID_LENGTH))
		{
			return(1);
		}
	}	
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

/*
**	ROUTINE:	computername()
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
char *computername(char *cname)
{
	static char* the_computername = NULL;

	if (!the_computername)
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
			tmp_name = "UNIX";
		}

		the_computername = wstrdup(tmp_name);
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


#if defined (_INTELC32_) || defined (WATCOM)

#include <stdio.h>
#if defined (WATCOM)
#include <i86.h>
#endif
#include <dos.h>
#include <ctype.h>

/*
**	Routine:	getmachineid()
**
**	Function:	To return a machine id. (Code Builder)
**
**	Description:	This routine creates a "unique" machine id string.
**			It first tries to get the volume serial number from
**			the hard drive.  It starts looking at the lowest
**			drive "C:" then "D:" and up until it finds one.  
**			If no volume serial number if found it creates a
**			timestamp ID in "C:\WISP.LID".  If all else fails
**			it returns a dummy ID.
**
**			If a disk id is found then the drive letter will prefix the 8 hex characters.
**			If a timestamp is found the a "T" will prefix 8 hex chars.
**			The Dummy id is "I28460100" (our address).
**
**			Examples:
**				C18C466D1	Drive C serial number id
**				F082714ED	Drive F serial number id
**				T2C3A1127	Timestamp id
**				I28460100	Dummy id
**
**	Arguments:
**	id		The machine id to return.  It is 9 bytes long but pass a 20 byte buffer for future.
**
**	Globals:	None
**
**	Return:
**	0		Success   (Always returns success)
**	1		Failed
**
**	Warnings:	None
**
**	History:	
**	07/06/93	Written by GSL
**
*/
int getmachineid(char* id)
{
#define	LICENSE_ID_FILE	"C:\\WISP.LID"
#define ID_SIZE		9

	FILE	*fh;
	char	*ptr;

	/*
	**	First try to get a disk id.
	*/
	if (0==low_disk_id(id)) return(0);					/* Found a disk id.				*/


	/*
	**	See if timestamp id already exists.
	*/
	if (fh = fopen(LICENSE_ID_FILE,"r"))
	{
		ptr = fgets(id, ID_SIZE+1, fh);
		fclose(fh);
		if (ptr) return(0);						/* Found a timestamp id.			*/
	}

	/*
	**	Create a timestamp id.
	*/
	if (fh = fopen(LICENSE_ID_FILE,"w"))
	{
		fprintf(fh,"T%08X\n",time(NULL));
		fclose(fh);
		hide_file(LICENSE_ID_FILE);
	}

	/*
	**	Try to read the newly created timestamp id.
	*/
	if (fh = fopen(LICENSE_ID_FILE,"r"))
	{
		ptr = fgets(id, ID_SIZE+1, fh);
		fclose(fh);
		if (ptr) return(0);						/* Got new timestamp id.			*/
	}

	/*
	**	Use the dummy id.
	*/
	strcpy(id,"I28460100");

	return(0);
}

/*
**	Routine:	low_disk_id()
**
**	Function:	To return machine id based on the lowest disk with a serial number.
**
**	Description:	This looks at the drives starting a "C:" for a serial number and
**			when it find one it creates a machine id prefixed with the drive letter.
**			If no drive is found with a serial number then it returns failure and id
**			is not set.
**
**	Arguments:
**	id		The machine id to return. Currently 9 bytes but pass in 20 for furture growth.
**
**	Globals:	None
**
**	Return:
**	0		Success 
**	1		Failed
**
**	Warnings:	None
**
**	History:	
**	07/06/93	Written by GSL
**
*/
static int low_disk_id(id)
char	*id;
{
	union	REGS	r;
	unsigned	drive;
	int		not_found;

	struct media_struct
	{
		short	info_level;
		short	serial1;
		short	serial2;
		char	vol_id[11];
		char	file_sys[8];
	};
	struct media_struct *media_ptr;

	not_found = 1;

	/*
	**	Alloc some low memory
	*/
	if ( _dos_allocmem(sizeof(struct media_struct), (unsigned *)&media_ptr) )
	{
		return(not_found);
	}

	/*
	**	Get the volume serial number
	**	C: = 3, D: = 4, E: = 5,...
	*/
	for(drive=3; drive<8; drive++)
	{
		media_ptr->info_level = 0;

		r.h.ah = 0x44;
		r.h.al = 0x0d;

		r.h.bh = 0;
		r.h.bl = drive;

		r.h.ch = 0x08;
		r.h.cl = 0x66;

		#ifdef _INTELC32_
		r.w.edx = (unsigned) media_ptr;
		int86(0x21, &r, &r);
		#endif
		#ifdef WATCOM
		r.x.edx = (unsigned) media_ptr;
		int386(0x21, &r, &r);
		#endif

		if (! r.x.cflag)
		{
			/*
			**	Found a drive with a serial number.
			*/
			not_found = 0;
			sprintf(id, "%c%04X%04X", (int)(drive + '@'), (int)media_ptr->serial2, (int)media_ptr->serial1);
			break;
		}
	}

	/*
	**	Free the low memory
	*/
	_dos_freemem((unsigned)media_ptr);

	return(not_found);
}

/*
**	Routine:	hide_file()
**
**	Function:	To make the file into a "hidden" file.
**
**	Description:	This routine sets the "hidden" attribute on in a file.
**
**	Arguments:
**	filename	The path to the file to hide.
**
**	Globals:	None
**
**	Return:
**	0		Success
**	non-zero	Error code
**
**	Warnings:	None
**
**	History:	
**	07/06/93	Written by GSL
**
*/
int hide_file(filename)
char	*filename;
{
	union	REGS	r;

	/*
	**	Get the currect file attributes
	*/
	r.h.ah = 0x43;
	r.h.al = 0x00;

	#ifdef _INTELC32_
	r.w.edx = (unsigned) filename;
 		int86(0x21, &r, &r);
	#endif
	#ifdef WATCOM
	r.x.edx = (unsigned) filename;
	int386(0x21, &r, &r);
	#endif

	if (r.x.cflag)
	{
		return((int)r.x.cflag);
	}

	/*
	**	Turn the hidden bit (0x02) on and set the file attribute
	*/
#ifdef _INTELC32_
	r.x.cx |= 0x02;
#else
	r.w.cx |= 0x02;
#endif

	r.h.ah = 0x43;
	r.h.al = 0x01;

	#ifdef _INTELC32_
	r.w.edx = (unsigned) filename;
	int86(0x21, &r, &r);
	#endif
	#ifdef WATCOM
	r.x.edx = (unsigned) filename;
	int386(0x21, &r, &r);
	#endif

	if (r.x.cflag)
	{
		return((int)r.x.cflag);
	}

	return(0);
}

#endif /* _INTELC32_ */

#ifdef WIN32

/*
**	Routine:	getmachineid()
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
int getmachineid(char* machineid)
{
	char	compname[256];
	int	len;
	
	computername(compname);
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

	encodemachid(compname, machineid);
	
	return 0;
}

/*
**	ROUTINE:	encodemachid()
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
void encodemachid(const char *source, char *target)
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
#endif /* WIN32 */

#ifdef DEBUG
#ifdef MAIN
main()
{
	char	id[80], idx[80];

	if ( getmachineid(id) )
	{
		printf("No machine_id\n");
	}
	else
	{
		printf("machine_id = %s\n", id);
	}

	encodemachid(id, idx);
	printf("Encoded machine id = %s\n", idx);
	
	exit(0);
}
#endif /* MAIN */
#endif /* DEBUG */

/*
**	History:
**	$Log: machid.c,v $
**	Revision 1.15  2001-09-27 10:13:06-04  gsl
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
**	The name now gets expanded in encodemachid() instead of getmachineid()
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
