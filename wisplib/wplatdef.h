			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	File:		wplatdef.h
**
**	Function:	Provide defines for platform types.
**
**	History:
**			05/19/92	Written GSL
**
*/

#ifndef WPLATDEF_H
#define WPLATDEF_H

#define		PLATFORM_UNKNOWN	0

/*
**	UNIX systems
*/
#define		PLATFORM_AIX		1001
#define		PLATFORM_ULTRIX		1002
#define		PLATFORM_HPUX		1003
#define		PLATFORM_SUNOS		1004
#define		PLATFORM_DGUX		1005
#define		PLATFORM_SCO		1006
#define		PLATFORM_NCR486		1007
#define		PLATFORM_NCR32		1008
#define		PLATFORM_MIPS		1009
#define		PLATFORM_ATT3B2		1010
#define		PLATFORM_BULL		1011
#define		PLATFORM_MOTOROLA	1012
#define		PLATFORM_UNISYS		1013
#define		PLATFORM_SEQUENT	1014
#define		PLATFORM_AIX_PS2	1015
#define		PLATFORM_ULTRIX_VAX	1016
#define		PLATFORM_ULTRIX_ALPHA	1017
#define		PLATFORM_AIX_3090	1018
#define		PLATFORM_NEXT		1019
#define		PLATFORM_MPEIX		1020
#define		PLATFORM_STRATUS	1021
#define		PLATFORM_ICL		1022
#define		PLATFORM_ALTOS		1023
#define		PLATFORM_CONCUR		1024
#define		PLATFORM_CTRLDATA	1025
#define		PLATFORM_CONVRG		1026
#define		PLATFORM_AMIGA		1027
#define		PLATFORM_NEC		1028
#define		PLATFORM_NIXDORF	1029
#define		PLATFORM_PRIME		1030
#define		PLATFORM_PYRAMID	1031
#define		PLATFORM_SONY		1032
#define		PLATFORM_WYSE		1033

/*
**	Non-UNIX
*/
#define		PLATFORM_VMS		2001
#define		PLATFORM_MSDOS		2002
#define		PLATFORM_HPMPE		2003
#define		PLATFORM_VMS_ALPHA	2004

#endif

