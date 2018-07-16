/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/*
**	File:		vssort.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Hold common sort routines.
**
**	Routines:	
**	WL_vssort()	Front-end to os specific sort routines.
**
**
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "idsistd.h"
#include "idsisubs.h"
#include "vssort.h"
#include "sortseqf.h"
#include "wfname.h"
#include "wisplib.h"

/*
**	Routine:	WL_vssort()
**
**	Function:	Full Wang SORT utility functionality
**
**	Description:	This is a front-end to the 
**				WL_wangsort() routine for Unix and WIN32 (It does NOT have full functionality.)
**
**	Arguments:
**	infiles		List of input FILE names
**	inlibs		List of input LIBRARY names
**	invols		List of input VOLUME names
**	infilename	List of native input filename.
**	intypes		List of input file types  FILETYPE (I, F, N, A, C)
**	inlens		List of input file lenghts RECSIZE
**	incount		Count of input files ( 1 - 20 )
**	outfile		Output FILE name
**	outlib		Output LIBRARY name
**	outvol		Output VOLUME name
**	outfilename	The native output filename.
**	outfileorg	Output file organization FILEORG (C, R, I)
**	outmaxrec	Output file maximum record size RECSIZE
**	outrectype	Output file record type RECTYPE (F, V)
**	replace_flag	Was REPLACE=YES specified
**	selinfo		Select criteria list
**	selcnt		Select count
**	keyinfo		Keys info list
**	keycnt		Keys count
**	stable_flag	Stable sort flag
**	retcode		Return Code
**				 0	Success
**				 4	Input file was empty
**				 8	Insufficient buffer space (or other internal error)
**				12	Record size greater then 9999 bytes  (Was 2024 on Wang)
**				16	Invalid sort key
**				20	Program check (see sortcode for reason).
**				24	Input records are out of order for merge
**				(The following are extensions)
**				40	Invalid filetype
**				41	No input file or access denied
**				42	Not an ACUCOBOL/Vision file.
**				43	Unable to get recordsize from Vision file
**				44	Unable to unload Vision file
**				45	Not a CISAM file
**				46	Unable to get recordsize from CISAM/FH-ISAM file
**				47	Unable to unload CISAM file
**				48	Unable to unload FH ISAM file (check for fhconvert)
**
**	errcode		An error code used to indicate what type of error occured.
**				 0	no error
**				 1	a recoverable error (reissue OUTPUT getparm)
**				 2	non recoverable error.
**	errmess		An 80 char error message.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	10/10/94	Written by GSL
**
*/
void WL_vssort(	char infiles[][9], char inlibs[][9], char invols[][7], char infilenames[][80], 
		char intypes[], int4 inlens[], int4 incount,
		char *outfile, char *outlib, char *outvol, char *outfilename, 
		char outfileorg, int4 outmaxrec, char outrectype, int replace_flag,
		struct select_criteria selinfo[], int selcnt, struct key_info keyinfo[], int keycnt, int stable_flag, 
		int4 *retcode, int4 *errcode, char *errmess)
{
	struct sortdata_s sortdata;
	int	i;
	int4	sortcode;

	memset(&sortdata,' ',sizeof(sortdata));
	memcpy(sortdata.ifile, infiles[0], 8);
	memcpy(sortdata.ilib,  inlibs[0],  8);
	memcpy(sortdata.ivol,  invols[0],  6);
	memcpy(sortdata.ofile, outfile, 8);
	memcpy(sortdata.olib,  outlib,  8);
	memcpy(sortdata.ovol,  outvol,  6);
	for (i=0; i<keycnt; i++)
	{
		char	buff[40];

		sprintf(buff,"%04ld",(long)keyinfo[i].spos);
		memcpy(sortdata.keys[i].pos,buff,4);
		sprintf(buff,"%03ld",(long)keyinfo[i].len);
		memcpy(sortdata.keys[i].len,buff,3);
		sortdata.keys[i].type = keyinfo[i].type;
		sortdata.keys[i].order = keyinfo[i].order;
	}

	*errcode = 0;
	*retcode = 0;
	sortcode = 0;
	WL_wswap(&inlens[0]);
	WL_wangsort((char*)&sortdata, &intypes[0], &inlens[0], stable_flag, &sortcode, retcode);

	WL_wswap(retcode);
	WL_wswap(&sortcode);

	if (*retcode <= 20)
	{
		sprintf(errmess, "RC=%ld Sortcode=%ld (%s)", (long)*retcode, (long)sortcode, WL_vssort_sortcode(sortcode));
	}
	else
	{
		sprintf(errmess, "RC=%ld Sortcode=%ld (%s)", (long)*retcode, (long)sortcode, WL_vssort_retcode(*retcode));
	}

	switch(sortcode)
	{
	case 0:		/* SUCCESS */
	case 42:	/* Empty input file */
		*errcode = 0;
		break;

	case 22:	/* No output file */
	case 34:	/* Error opening output */
	case 37:	/* Error writing output */
		*errcode = 1;
		break;

	default:
		*errcode = 2;
		break;
	}
	return;
}


char *WL_vssort_retcode(int4 retcode)
{
	char	*message;

	switch(retcode)
	{
	case  0:	message = "";						break;
	case  4:	message	= "Input file empty";				break;
	case  8:	message = "Insufficient buffer sapce";			break;
	case 12:	message = "Record size greater then 9999 bytes";	break;  /* Was 2024 on Wang */
	case 16:	message = "Invalid sort key";				break;
	case 20:	message = "Sort failed see Sortcode";			break;
	case 40:	message = "Invalid file type";				break;
	case 41:	message	= "No INPUT file or access denied";		break;
	case 42:	message = "Not an ACUCOBOL/Vision file";		break;
	case 43:	message = "Unable to get recsize from Vision";		break;
	case 44:	message = "Unable to unload Vision file";		break;
	case 45:	message = "Not a CISAM file";				break;
	case 46:	message = "Unable to get recsize from CISAM";		break;
	case 47:	message = "Unable to unload CISAM file";		break;
	default: 	message = "UNKNOWN ERROR";				break;
	}

	return message;
}

char *WL_vssort_sortcode(int4 sortcode)
{
	char	*message;

	switch(sortcode)
	{
	case ERR_NOINFILE:	message = "No INFILE";					break;
	case ERR_NOOUTFILE:	message = "No OUTFILE";					break;
	case ERR_BADRECSIZE:	message = "Invalid record size";			break;
	case ERR_BADFILETYPE:	message = "Invalid record type";			break;
	case ERR_BADNUMKEYS:	message = "Invalid number of sort keys";		break;
	case ERR_NOSORTKEYS:	message = "No sort keys";				break;
	case ERR_BADOFFSET:	message = "Invalid sort key offset";			break;
	case ERR_BADLENGTH:	message = "Invalid sort key length";			break;
	case ERR_BADDIRECTION:	message = "Invalid sort key direction";			break;
	case ERR_BADTYPE:	message = "Invalid sort key type";			break;
	case ERR_BINLEN:	message = "Invalid sort key length (BINARY 2 or 4)";	break;
	case ERR_FLOATLEN:	message = "Invalid sort key length (FLOAT 4 or 8)";	break;
	case ERR_OPENINPUT:	message = "Open for input failed";			break;
	case ERR_OPENOUTPUT:	message = "Open for output failed";			break;
	case ERR_MALLOC:	message = "Unable to malloc enough memory";		break;
 	case ERR_READ:		message = "Read failed";				break;
	case ERR_WRITE:		message = "Write failed.";				break;
	case ERR_SIZEINT:	message = "Invalid size of integer";			break;
	case ERR_SIZEFLOAT:	message = "Invalid size of float";			break;
	case ERR_NORECEND:	message = "No recend supplied.";			break;
	case ERR_BADSIZEEND:	message = "Invalid size of recend";			break;
	case ERR_NORECORDS:	message = "Infile contains no records";			break;
	default: 		message = "UNKNOWN ERROR";				break;
	}

	return message;
}

/*
**	History:
**	$Log: vssort.c,v $
**	Revision 1.13  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.12  2002/07/18 21:04:29  gsl
**	Remove MSDOS code
**	
**	Revision 1.11  2002/07/12 19:10:18  gsl
**	Global unique WL_ changes
**	
**	Revision 1.10  2002/07/12 17:01:02  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.9  2002/07/09 04:13:56  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.8  2002/06/21 03:10:43  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.7  1998/04/17 18:53:41  gsl
**	Change max recsize to 9999 from 2024
**	
**	Revision 1.6  1996-07-10 19:52:36-04  gsl
**	fix missing includes and fix prototype warning
**
**	Revision 1.5  1995-06-23 08:41:02-07  gsl
**	Correct the errcode handling so that empty input file was not
**	considered a serious error.
**
**
**
*/
