			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef _INTELC32_

/*
**	File:		machid.c
**
**	Purpose:	To get MS-DOS machine id for Intel CodeBuilder C.
**
**	Routines:	
**	getmachineid()	Get machine id.
**	low_disk_id()	Get machine id based on disk serial number
**	hide_file()	Make file "hidden"
**
**
*/

#include <stdio.h>
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
int getmachineid(id)
char	*id;
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

		r.w.edx = (unsigned) media_ptr;

		int86(0x21, &r, &r);

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
	r.w.edx = (unsigned) filename;

	int86(0x21, &r, &r);

	if (r.x.cflag)
	{
		return((int)r.x.cflag);
	}

	/*
	**	Turn the hidden bit (0x02) on and set the file attribute
	*/
	r.x.cx |= 0x02;

	r.h.ah = 0x43;
	r.h.al = 0x01;
	r.w.edx = (unsigned) filename;

	int86(0x21, &r, &r);

	if (r.x.cflag)
	{
		return((int)r.x.cflag);
	}

	return(0);
}

#ifdef MAIN
main()
{
	char	id[20];

	if ( getmachineid(id) )
	{
		printf("No machine_id\n");
	}
	else
	{
		printf("machine_id = %s\n", id);
	}
	exit(0);
}
#endif /* MAIN */

#else /* _INTELC32_ */
static int dummy_machid;
#endif
