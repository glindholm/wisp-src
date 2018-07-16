			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	filext.h	This header defines variables that may or may not be defined by the COBOL.
**
**			NOTE: This file has been "hard" included into sub85.c
*/

#ifndef FILEXT_DEF
#define FILEXT_DEF

#ifdef EXT_FILEXT
#define EXTERN_DEF 
#else
#define EXTERN_DEF extern
#endif
											/* If COBOL defines storage for the	*/
											/* extern variables then don't define	*/
											/* storage here.			*/

EXTERN_DEF char	WISPFILEXT[39];								/* Define the file extension variable.	*/
EXTERN_DEF char	WISPRETURNCODE[3];							/* Define the return code field.	*/

#undef EXTERN_DEF

#endif

