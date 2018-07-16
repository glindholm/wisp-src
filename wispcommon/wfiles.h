/*
******************************************************************************
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
******************************************************************************
*/


#ifndef WFILES_INCLUDED
#define WFILES_INCLUDED

#include <string.h>
#include <stdlib.h>
#include "wdefines.h"

struct wisp_fstruct	{									/* The structure for scratch files.	*/
			char vol[SIZEOF_VOL];						/* the local volume name of the file	*/
			char lib[SIZEOF_LIB];						/* the library name of the file		*/
			char file[SIZEOF_FILE];						/* the file name			*/
			char name[COB_FILEPATH_LEN + 1];				/* the generated name of the file	*/
			struct wisp_fstruct *nextfile;					/* pointer to the next file		*/
		};
typedef struct wisp_fstruct wisp_fstruct;

struct wisp_pstruct	{									/* The structure for PRINT files.	*/
			char name[COB_FILEPATH_LEN + 1];				/* the name of the file to print	*/
			int  form;							/* The form to use.			*/
			char class;							/* The printer class.			*/
			int numcopies;							/* The number of copies to spool.	*/
			struct wisp_pstruct *nextfile;					/* pointer to the next one		*/
		};
typedef struct wisp_pstruct wisp_pstruct;


#endif	/* WFILES_INCLUDED */

/*
**	History:
**	$Log: wfiles.h,v $
**	Revision 1.14  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/01 04:02:45  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.12  2002/06/25 17:46:06  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.11  1998/10/22 18:05:48  gsl
**	Fix the fstruct and pstruct typedefs
**	
**	Revision 1.10  1998-08-03 16:40:26-04  jlima
**	Support Logical Volume Translations to long file names containing eventual embedded blanks.
**
**	Revision 1.9  1996-07-23 14:17:57-04  gsl
**	drcs update
**
**
**
*/
