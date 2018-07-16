			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* Header file used to define useful file information.										*/

#ifndef WFILES_INCLUDED
#define WFILES_INCLUDED

#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif

#define NAME_LENGTH	80

typedef struct	{									/* The structure for scratch files.	*/
			char vol[6];							/* the local volume name of the file	*/
			char lib[8];							/* the library name of the file		*/
			char file[8];							/* the file name			*/
			char name[NAME_LENGTH];						/* the generated name of the file	*/
			struct fstruct *nextfile;					/* pointer to the next file		*/
		} fstruct;

typedef struct	{									/* The structure for PRINT files.	*/
			char name[NAME_LENGTH];						/* the name of the file to print	*/
			int  form;							/* The form to use.			*/
			char class;							/* The printer class.			*/
			int numcopies;							/* The number of copies to spool.	*/
			struct pstruct *nextfile;					/* pointer to the next one		*/
		} pstruct;

static char X_twfxt[39];

#define	SAVE_WISPFILEXT		memcpy(X_twfxt,WISPFILEXT,sizeof(X_twfxt))
#define RESTORE_WISPFILEXT	memcpy(WISPFILEXT,X_twfxt,sizeof(X_twfxt))

#endif	/* WFILES_INCLUDED */

