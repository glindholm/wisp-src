static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
kcsio.c
Imitates the KCSIO program as a shell into c-isam (d-isam) calls.
the ufb is ignored as not needed by c-isam though it is used in the Wang
COBOL version.
------*/
#include <stdio.h>
#include <ctype.h>
#include "cobstat.h"
#include "iocode.h"
#include "kcsio.h"
#include "cobioblk.h"
#include "kwisp.h"
#include "intdef.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)kcsio.c	1.15 11/15/93";

/*
Temporary patch.
*/
/*
#define IS_OUTPUT 0x00000001
#define IS_IO     0x00000010
#define IS_PRNAME 0x00000800
#define IS_INDEXED 0x00000100
*/

/*
#define	DEBUG
*/
/*----
The passed io_block is converted to the global struct to allow all routines
to access the data and to get it into a format that 'C' can live with.
The conversion is based on the type of IO being done.
For OPENs it is done for all fields.
For reads and starts the appropriate key is converted.
The record_area is copied into and out of rec for the length in the
passed io block.
------*/
/*----
Some shorthand and specialized versions of semi-standard routines
and to improve readability.
------*/
/*Copy for target length -1 and add nul. x must not be a pointer */
#define Make_c_str(x,y)   memcpy(x,y,sizeof(x)-1); x[sizeof(x)-1] = 0;
#define Cobol_status(x)   memcpy(&kio[STATUS_POS],x,2) /* Set COBOL status  */
#define Streq(x,y)        (!strcmp(x,y))               /* True when equal   */
#define IOis(y)           Streq(kfb->_io,y)            /* Test for IO type  */
#define LastIOis(y)       Streq(kfb->_last_io,y)        /* Test the last IO  */

/*---
Non-integer returning functions.
------*/
static long atollen(char *s,int len);
/*----
This should handle an alignment problem by copying the struct
from the sys-io-block into an aligned structure.
------*/
KCSIO_BLOCK akfb;

/*----
0. The extract info routine is passed the kio character array so
   it cannot be handled by the standard dispatcher. Test this first and
   execute if needed.
1. Handle two initialization problems.
2. Extract the IO request.
3. Handle special init for open or input logic (read hold and start).

4. Copy the passed record to the record area
5. Execute the dispatcher routine.
------*/

static void init_io_key(char *kio,KCSIO_BLOCK *kfb);
static int io_is_on_primary(KCSIO_BLOCK *kfb);
static void init_for_io(char *kio,KCSIO_BLOCK *kfb);
static void init_for_open(char *kio,KCSIO_BLOCK *kfb);
static void init_for_oo(char *kio,KCSIO_BLOCK *kfb);
static void init_rel_key(char *kio,KCSIO_BLOCK *fb);
static void init_keys(char *kio,KCSIO_BLOCK *kfb);
static void init_primary(char *kio,KCSIO_BLOCK *kfb);
static void init_altkeys(char *kio,KCSIO_BLOCK *kfb);
static void init_for_input(char *kio,KCSIO_BLOCK *kfb);
static void init_for_return(char *kio,KCSIO_BLOCK *kfb);
static void cvt_file_info(char *kio,KCSIO_BLOCK *kfb);
static void cvt_file_space(char *kio,KCSIO_BLOCK *kfb);
static void cvt_record_len(char *kio,KCSIO_BLOCK *kfb);
static void cvt_file_org(char *kio,KCSIO_BLOCK *kfb);
static void cvt_primary(char *kio,KCSIO_BLOCK *kfb);
static void cvt_altkeys(char *kio,KCSIO_BLOCK *kfb);
static int atointlen(char *s,int len);
static void itoalen(char *dest,int value,int len);
static void ltoalen(char *dest,long value,int len);

/*
main()
{
}
*/

/*----
Returns file info type.
Type '0' files can be opened without a full file description.
Type '1' files require a full record and key layout.
------*/
void KFILTYP(char* typ)
{
	*typ = '0';
#ifdef	KCSI_MFX
	*typ = '1';
#endif
}

/*----
This is a simple function to compare two 100 byte chunks of memory. 
It could have
been done in COBOL, but I wanted C style control of it, so I could turn
it on and off because of the Microfocus bug in the file info request
routine.
It is used in dtectl.wcb, rptwmn.wcb and inqmain.wcb after a call to
KCSIO File-info, to compare general-1 and general-2 that contain the
file description information.
------*/
void KMATCH(char *rc,char *g1,char *g2)
{
	memcpy(rc,"00",2);
#ifndef	KCSI_MFX
	if(memcmp(g1,g2,100))
		memcpy(rc,"95",2);
#endif
}

/*----
COBOL entry point uses a local global for the KCSIO_BLOCK array.
------*/
void KCSIO(char *kio,char *ufb,char *recarea)
{
	KCSIO_BLOCK *kfb;

	kfb = &akfb;
/*
 * Handle any failure to init variable and compressed flags. These
 * are used in tests at higher levels. Compressed and variable
 * are not used in c-isam.
 */
	if(kio[COMP_FLAG_POS] != 'Y')
		kio[COMP_FLAG_POS] = 'N';
	if(kio[VAR_FLAG_POS] != 'Y')
		kio[VAR_FLAG_POS] = 'N';

/*
 * retrieve the io block
 */
	memcpy(kfb,&kio[SYS_IO_BLOCK_POS],sizeof(akfb));


/*
 * Extract the IO type and initialize based on root IO type.
 */
	Make_c_str(kfb->_io,&kio[IO_POS]);
	switch(kfb->_io[0])
		{
		case 'O':
			init_for_open(kio,kfb);
			break;
		case 'R':
		case 'H':
		case 'S':
		case 'K':
			init_for_input(kio,kfb);
			break;
		default:
			init_for_io(kio,kfb);
			break;
		}

	ccsio(kfb,recarea);

	if(	(IOis(FILE_INFO))	||
		(IOis(TABLE_INFO))	)
		cvt_file_info(kio,kfb);
	init_for_return(kio,kfb);

	memcpy(&kio[SYS_IO_BLOCK_POS],kfb,sizeof(akfb));

}

/*----
         <<<<<<<<   INITIALIZE FOR COBOL IO        >>>>>>>>
------*/
/*----
The io key will be 0 thru 16 where 0 is the primary key.
------*/
static void init_io_key(char *kio,KCSIO_BLOCK *kfb)
{
	if(io_is_on_primary(kfb))
		memcpy(&kio[IO_KEY_POS],"00",IO_KEY_LEN);
	kfb->_io_key = atointlen(&kio[IO_KEY_POS],IO_KEY_LEN);
}
/*----
Return true for reads holds and starts that request
primary key only.
------*/
static int io_is_on_primary(KCSIO_BLOCK *kfb)
{
	if((IOis(READ_RECORD)) 		||
           (IOis(HOLD_RECORD))		||
	   (IOis(START_EQ))		||
	   (IOis(START_NLT))		||
	   (IOis(START_GT))		)
		return(1);
	else
		return(0);

}

/*----
This function used to work by accident. It was an empty function, so rel
key was not being initialized on writes. However DATENTRY which uses this
was doing a read before writing theby acidentally setting the rel_key
correctly. Todd picked this up in VAX testing.
------*/
static void init_for_io(char *kio,KCSIO_BLOCK *kfb)
{
	init_rel_key(kio,kfb);
}

/*----
            <<<<<<<   INITIALIZE FOR OPENS LOGIC   >>>>>>>>
Consists basically of loading the 'C' structure from the the data passed
in by COBOL.
------*/

static void init_for_open(char *kio,KCSIO_BLOCK *kfb)
{
	extern int x4dbfile(char *select_name, int4 l1, int4 *select_status, int4 l2);
	int4 mode;

	memset(kfb,0,sizeof(KCSIO_BLOCK));
	Make_c_str(kfb->_io,&kio[IO_POS]);

	Make_c_str(kfb->_name,&kio[NAME_POS]);
	Make_c_str(kfb->_library,&kio[LIBRARY_POS]);
	Make_c_str(kfb->_volume,&kio[VOLUME_POS]);
	Make_c_str(kfb->_prname,&kio[PRNAME_POS]);
	Make_c_str(kfb->_org,&kio[ORG_POS]);
	kfb->_record_len = atointlen(&kio[RECORD_LEN_POS],RECORD_LEN_LEN);
/*
 * Mode flags used by wfopen
 */

	mode = WISP_PRNAME;
	if(IOis(OPEN_OUTPUT))
		{
		mode += WISP_OUTPUT;
		}
	else
	/*if(IOis(OPEN_IO)) */
		{
		mode += WISP_IO;
		}
	if(kfb->_org[0] == 'I')
		mode += WISP_INDEXED;

/*
 	 Determine if is a Database file
	 Set the mode bit if it is.
*/
	x4dbfile(kfb->_name,8,&mode,sizeof(mode));
	
	kcsio_wfopen(mode,kfb);


/*
 * All Opens reset the key of reference to the primary key. Force
 * this condition into the COBOL IO block.
 */
	memcpy(&kio[IO_KEY_POS],"00",IO_KEY_LEN);
/*
 * Micro focus has no way of extracting file information, so
 * Every open must initialize the full kfb.
 */
#ifndef	KCSI_MFX
	if(IOis(OPEN_OUTPUT))
#endif
		init_for_oo(kio,kfb);
}

static void init_for_oo(char *kio,KCSIO_BLOCK *kfb)
{
	kfb->_altkey_count = atointlen(&kio[ALTKEY_COUNT_POS],ALTKEY_COUNT_LEN);
	kfb->_space = atollen(&kio[SPACE_POS],SPACE_LEN);

	/*
	**	This is the default format for MF and ACU
	*/
	kfb->_format = '\0';

	init_rel_key(kio,kfb);
	init_keys(kio,kfb);
}

/*----
The rel key is only used for non keyed files.
------*/
static void init_rel_key(char *kio,KCSIO_BLOCK *kfb)
{
	kfb->_rel_key = atollen(&kio[REL_KEY_POS],REL_KEY_LEN);
}
/*----
Initialize all key structures
Only init the alternates if a primary exists.
------*/
static void init_keys(char *kio,KCSIO_BLOCK *kfb)
{
	clear_keys(kfb);
	init_primary(kio,kfb);
	if(file_is_indexed(kfb))
		init_altkeys(kio,kfb);
}

/*----
Initialize the first key structure from the passed data.
------*/
static void init_primary(char *kio,KCSIO_BLOCK *kfb)
{
	init_a_key(&kfb->_key[0],
		atointlen(&kio[KEY_POS_POS],KEY_POS_LEN),
		atointlen(&kio[KEY_LEN_POS],KEY_LEN_LEN),
		ISNODUPS
		 );
}
/*----
Initialize the alternate keys.
1.  Set kio to the base of the altkey data structure.
2.  Process each key arrangement into elements 1 thru 16
    (primary key is in element 0)
------*/
static void init_altkeys(char *kio,KCSIO_BLOCK *kfb)

{
	int i;
	kio += ALTKEY_BLOCKS_POS;

	for(i=1 ; i <= kfb->_altkey_count ; ++i)
		{
	init_a_key(&kfb->_key[i],
			atointlen(&kio[ALTKEY_POS_OFF],ALTKEY_POS_LEN),
			atointlen(&kio[ALTKEY_LEN_OFF],ALTKEY_LEN_LEN),
			(kio[ALTKEY_DUP_OFF]) == '1'?ISDUPS:ISNODUPS
		);
		kio += ALTKEY_BLOCK_LEN ;
		}
}
/*----
         <<<<<<<<   INITIALIZE FOR INPUT     >>>>>>>>
Input requires that the IO key (or rel key) be initialized.
------*/
static void init_for_input(char *kio,KCSIO_BLOCK *kfb)
{
	if(file_is_indexed(kfb))
		init_io_key(kio,kfb);
	else
		init_rel_key(kio,kfb);
}

/*----
Once an IO is completed, several values may have been modified
in the 'C' file sturcture.

These are extracted into the COBOL fields.

The areas that will be affected by an IO are.
1.  The file status must be translated from the C error code to
    a COBOL equivalent.
2.  The record count (file-space)
3.  If the IO was an Open, then the IO channel must be converted.
4.  For opens or closes the open-status must me changed if the IO
    was successful.
------*/
static void init_for_return(char *kio,KCSIO_BLOCK *kfb)
{

/*
 * These are approximate equivalents for file errors.
 */
	switch(kfb->_status)
		{
		case 0:
			Cobol_status(I_O_OK);
			break;
		case EDUPL:
			Cobol_status(DUPLICATE_KEY);
			break;
		case EENDFILE:
			Cobol_status(AT_END);
			break;
		case ENOREC:
			Cobol_status(RECORD_NOT_FOUND);
			break;
		case ELOCKED:
			Cobol_status(RECORD_LOCKED);
			break;
		case EBADMEM:
			Cobol_status(HARDWARE_ERROR);
			break;
		default:
			Cobol_status(INVALID_KEY);
			break;
		}

	itoalen(&kio[STATUS_EXT_POS],kfb->_status,STATUS_EXT_LEN);
	if(kfb->_open_status == 1)
		{
		cvt_file_space(kio,kfb);
		}
	if(io_is_open(kfb))
		{
		itoalen(&kio[IO_CHANNEL_POS],kfb->_io_channel,IO_CHANNEL_LEN);
		}
	if((io_is_open(kfb))||(IOis(CLOSE_FILE)))
		{
		if(kfb->_status == 0)
			kio[OPEN_STATUS_POS] = kfb->_open_status + '0';
		}
	itoalen(&kio[REL_KEY_POS],kfb->_rel_key,REL_KEY_LEN);

}
/*----
		COBOL Conversions

Convert data from the C struct to the COBOL passed IO-BLOCK
-------*/
/*----
Devolve the new values back into the passed COBOL area.
------*/
static void cvt_file_info(char *kio,KCSIO_BLOCK *kfb)
{
	cvt_file_space(kio,kfb);
	cvt_record_len(kio,kfb);
	cvt_file_org(kio,kfb);
	cvt_primary(kio,kfb);
	cvt_altkeys(kio,kfb);
}

static void cvt_file_space(char *kio,KCSIO_BLOCK *kfb)

{
	ltoalen(&kio[SPACE_POS],kfb->_space,SPACE_LEN);
}

static void cvt_record_len(char *kio,KCSIO_BLOCK *kfb)
{

	itoalen(&kio[RECORD_LEN_POS],kfb->_record_len,RECORD_LEN_LEN);
}

static void cvt_file_org(char *kio,KCSIO_BLOCK *kfb)
{
	kio[ORG_POS] = kfb->_org[0];
}

/*----
Initialize the first key structure from the passed data.
------*/
static void cvt_primary(char *kio,KCSIO_BLOCK *kfb)
{

	itoalen(&kio[KEY_POS_POS],kfb->_key[0].k_part[0].kp_start + 1,
		KEY_POS_LEN);
	itoalen(&kio[KEY_LEN_POS],kfb->_key[0].k_part[0].kp_leng,
		KEY_LEN_LEN);
/*
 * If the length of the key is zero, then force the starting position
 * to zero. (Relative file)
 */
	if(kfb->_key[0].k_part[0].kp_leng == 0)
		memcpy(&kio[KEY_POS_POS],"0000",KEY_POS_LEN);

}
/*----
Initialize the alternate keys.
0.  Convert the altkey count.
1.  Set kio to the base of the altkey data structure.
2.  Process each key 1 thru 16
    (primary key is in element 0)
------*/
static void cvt_altkeys(char *kio,KCSIO_BLOCK *kfb)
{
	int i,kflag;
	int kstart,klen,keynum;

	itoalen(&kio[ALTKEY_COUNT_POS],kfb->_altkey_count,ALTKEY_COUNT_LEN);

	kio += ALTKEY_BLOCKS_POS;

	for(i=1 ; i < 17 ; ++i)
		{
		if( i > kfb->_altkey_count)
			{
			kio[ALTKEY_DUP_OFF] = '0';
			kflag = kstart = klen = keynum = 0;
			}
		else
			{
			kflag = kfb->_key[i].k_flags;
			kflag &= ISDUPS;		/* ISDUPS is a mask to test if the flag is set. */
			kstart = kfb->_key[i].k_part[0].kp_start + 1;
			klen = kfb->_key[i].k_part[0].kp_leng;
			keynum = i;
			}
		kio[ALTKEY_DUP_OFF] = kflag?'1':'0';
		itoalen(&kio[ALTKEY_POS_OFF],kstart,ALTKEY_POS_LEN);
		itoalen(&kio[ALTKEY_LEN_OFF],klen,ALTKEY_LEN_LEN);
		itoalen(&kio[ALTKEY_NUMBER_OFF],keynum,ALTKEY_NUMBER_LEN);
		kio += ALTKEY_BLOCK_LEN ;
		}
	itoalen(&kio[ALTKEY_COUNT_POS],kfb->_altkey_count,ALTKEY_COUNT_LEN);
}

/*----
Utility routines.
------*/

/*----
Convert a character string delimited by a length to an integer.
Returns the integer.
------*/
static int atointlen(char *s,int len)
{
	char buf[20];

	memcpy(buf,s,len);
	buf[len]=0;
	return(atoi(buf));
}
/*----
Convert a character string delimited by a length to a long.
Returns the long.
------*/
static long atollen(char *s,int len)
{
	char buf[20];

	memcpy(buf,s,len);
	buf[len]=0;
	return(atol(buf));
}

/*----
Convert an integer to a string of length.
------*/
static void itoalen(char *dest,int value,int len)
{
	char buf[20],format[20];
/*
 * Build a format sring of the correct length
 */
	sprintf(format,"%%0%dd",len);
	sprintf(buf,format,value);
	memcpy(dest,buf,len);
}
/*----
Convert a long to a string of length.
------*/
static void ltoalen(char *dest,long value,int len)
{
	char buf[20],format[20];
	
	sprintf(format,"%%0%dld",len);
	sprintf(buf,format,value);
	memcpy(dest,buf,len);
}

/*
**	History:
**	$Log: kcsio.c,v $
**	Revision 1.13  1998-08-03 15:48:27-04  gsl
**	The logic which tests is the data file has duplicate alternate keys
**	was broken in CRID 2.93.  It was incorrectly reporting that all alternate
**	keys allow duplicates.  This would cause the CONTROL file to not match
**	the data file if the CONTROL file specifed that duplicates are not
**	allowed on an alternate key.
**
**	Revision 1.12  1998-07-29 18:15:36-04  scass
**	Corrected bit manipulation.
**	Was & when should be |  for duplicate flags
**
**	Revision 1.11  1998-05-18 17:37:34-04  gsl
**	Explicetly initialize _format on an open output
**
**	Revision 1.10  1997-07-18 15:14:09-04  gsl
**	Add proto for x4dbfile()
**
**	Revision 1.9  1997-07-17 12:48:39-04  scass
**	Change subroutine name from atoilen to atointlen()
**	Was causing problems on solaris with re-declared message
**
**	Revision 1.8  1997-07-09 17:02:18-04  scass
**	Added a call to x4dbfile() so will set the appropriate
**	bit if is a database file.  Thus wfopen will function
**	properly with Acu4GL environment.
**
**	Revision 1.7  1996-09-17 19:45:40-04  gsl
**	drcs update
**
**
**
*/
