/* 
	Copyright (c) 19951998 NeoMedia Technologies, All rights reserved.
	$Id:$
*/


/* Header file used to define useful file information.										*/

#ifndef WFILES_INCLUDED
#define WFILES_INCLUDED

#include <string.h>
#include <stdlib.h>
#include "wdefines.h"

struct fstruct	{									/* The structure for scratch files.	*/
			char vol[SIZEOF_VOL];						/* the local volume name of the file	*/
			char lib[SIZEOF_LIB];						/* the library name of the file		*/
			char file[SIZEOF_FILE];						/* the file name			*/
			char name[COB_FILEPATH_LEN + 1];				/* the generated name of the file	*/
			struct fstruct *nextfile;					/* pointer to the next file		*/
		};
typedef struct fstruct fstruct;

struct pstruct	{									/* The structure for PRINT files.	*/
			char name[COB_FILEPATH_LEN + 1];				/* the name of the file to print	*/
			int  form;							/* The form to use.			*/
			char class;							/* The printer class.			*/
			int numcopies;							/* The number of copies to spool.	*/
			struct pstruct *nextfile;					/* pointer to the next one		*/
		};
typedef struct pstruct pstruct;

#endif	/* WFILES_INCLUDED */

/*
**	History:
**	$Log: wfiles.h,v $
**	Revision 1.11.2.1  2002/11/14 21:12:29  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
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
