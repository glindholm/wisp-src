/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/

/*
**	File:		wplatdef.h
**
**	Function:	Provide defines for platform types.
**
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
#define		PLATFORM_SOLARIS	1034
#define		PLATFORM_SOLARIS_PC	1035
#define		PLATFORM_OSF1_ALPHA	1036
#define		PLATFORM_OSF1_DEC	1037
#define		PLATFORM_SUN_3		1038
#define		PLATFORM_UNIXWARE	1039
#define		PLATFORM_DGUX_INTEL	1040
#define		PLATFORM_LINUX		1041
#define		PLATFORM_AIX_64		1042
#define		PLATFORM_HPUX_64	1043
#define		PLATFORM_SOLARIS_64	1044
#define		PLATFORM_HPUX_IA	1045
#define		PLATFORM_HPUX_IA_64	1046
#define		PLATFORM_LINUX_64	1047

/*
**	Non-UNIX
*/
#define		PLATFORM_VMS		2001
#define		PLATFORM_MSDOS		2002
#define		PLATFORM_HPMPE		2003
#define		PLATFORM_VMS_ALPHA	2004
#define		PLATFORM_WINDOWS_NT	2005
#define		PLATFORM_WINDOWS_64	2006

#endif
