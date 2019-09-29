/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
*/




/*
**	File:		platsubs.c
**
**	Function:	subroutines used by to validate platform number.
**
**	Routines:	
**	WL_valplat()			Validate platform.
**	WL_platform_number(void);	Get platform number
**	WL_platform_name(void);		Get platform name
**	WL_platform_define(void);	Get platform #define name
**	WL_platform_code(void);		Get platform code
**	WISPPLAT()			Cobol version 
**	WL_plat_code()			Get platform info by code.
**	WL_putplattab()			Print a formatted table of platform codes.
**
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>


#include "idsistd.h"
#include "wplatdef.h"
#include "platsubs.h"

struct platform_struct
{
	int	number;		/* The platform number 1000's are unix  2000's are non-unix 	*/
	char	*code;		/* Two char unique code (must not include char '0' zero)	*/
	char	*name;		/* The platform name (max 20 char)				*/
	char	*define_name;	/* Define name no-spaces UPPERCASE underscores			*/
};

static struct platform_struct	platform_table[] = 
{
	/*			"12"	"12345678901234567890"	"1234567890"	*/
	{PLATFORM_UNKNOWN,      "UK",	"Unknown",		"UNKNOWN"},

	{PLATFORM_OSF1_ALPHA,	"A1",	"Tru64 Alpha",		"OSF1_ALPHA"},	/* Tru64 Digital->Compaq->HP */
	{PLATFORM_AIX,          "AX",	"AIX RS/6000 (32-bit)",	"AIX"},
	{PLATFORM_AIX_64,       "A6",	"AIX RS/6000 (64-bit)",	"AIX_64"},
	{PLATFORM_DGUX,         "DG",	"DG/UX Aviion",		"DGUX"},
	{PLATFORM_DGUX_INTEL,   "DI",	"DG/UX Intel",		"DGUX_INTEL"},	
	{PLATFORM_HPUX,         "HP",	"HP-UX PA-RISC 32-bit",	"HPUX"},
	{PLATFORM_HPUX_64,      "H6",	"HP-UX PA-RISC 64-bit",	"HPUX_64"},
	{PLATFORM_HPUX_IA,      "IA",	"HP-UX IA 32-bit",	"HPUX_IA"},
	{PLATFORM_HPUX_IA_64,   "I6",	"HP-UX IA 64-bit",	"HPUX_IA_64"},
	{PLATFORM_LINUX,        "LX",	"LINUX Intel",		"LINUX"},
	{PLATFORM_LINUX_64,     "L6",	"LINUX Intel 64-bit",		"LINUX_64"},
	{PLATFORM_MSDOS,        "DS",	"MSDOS",		"MSDOS"},
	{PLATFORM_SCO,          "SC",	"SCO UNIX Intel",	"SCO"},
	{PLATFORM_SOLARIS,      "SO",   "Solaris Sparc 32-bit",	"SOLARIS"},
	{PLATFORM_SOLARIS_64,   "S6",   "Solaris Sparc 64-bit",	"SOLARIS_64"},
	{PLATFORM_SEQUENT,      "SQ",	"Sequent",		"SEQUENT"},
	{PLATFORM_ULTRIX,       "UL",	"Ultrix (DEC RISC)",	"ULTRIX"},
	{PLATFORM_UNIXWARE,     "UW",	"UNIXWARE Intel",	"UNIXWARE"},
	{PLATFORM_WINDOWS_NT,	"WN",	"Windows (WIN32)",	"WIN32"},
	{PLATFORM_WINDOWS_64,	"W6",	"Windows (WIN64)",	"WIN64"},
	{-1,			(char *)0, (char *)0,		(char *)0}

#ifdef OLD
	PLATFORM_MSDOS,		"DS",	"MS-DOS",
	PLATFORM_HPMPE,		"HM",	"HP MPE",
	PLATFORM_VMS_ALPHA,	"VA",	"VMS (ALPHA)",
	PLATFORM_VMS,		"VM",	"VAX/VMS",
	PLATFORM_ICL,		"IC",	"ICL DRS 6000",
	PLATFORM_ATT3B2,	"AT",	"AT&T 3b2",
	PLATFORM_BULL,		"BU",	"BULL DPX/2",
	PLATFORM_MOTOROLA,	"MO",	"Motorola 88000",
	PLATFORM_UNISYS,	"US",	"Unisys 6000 SVR4",
	PLATFORM_NCR486,	"NC",	"NCR 386/486",
	PLATFORM_NCR32,		"NT",	"NCR Tower/32 68020",
	PLATFORM_MIPS,		"MI",	"Mips R2000",
	PLATFORM_SOLARIS_PC,	"SI",	"Solaris 2 (Intel)",
	PLATFORM_SUNOS,		"SU",	"SunOS (Sparc)",
	PLATFORM_SUN_3,		"S3",	"Sun 3 (68020)",
	PLATFORM_STRATUS,	"SX",	"Stratus FTX",
	PLATFORM_AIX_PS2,	"AP",	"AIX PS/2",		
	PLATFORM_ULTRIX_VAX,	"UV",	"Ultrix (VAX)",	
	PLATFORM_ULTRIX_ALPHA,	"UA",	"Ultrix (ALPHA)", 	
	PLATFORM_OSF1_DEC,	"D1",	"OSF/1 (DEC RISC)",	
	PLATFORM_AIX_3090,	"A3",	"AIX 3090",		
	PLATFORM_NEXT,		"NX",	"NeXT 68040",		
	PLATFORM_MPEIX,		"HI",	"HP MPE/iX",		
	PLATFORM_ALTOS,		"AL",	"ALTOS 68020",
	PLATFORM_CONCUR,	"CC",	"CONCURRENT",
	PLATFORM_CTRLDATA,	"CD",	"CONTROL DATA",
	PLATFORM_CONVRG,	"CV",	"CONVERGENT",
	PLATFORM_AMIGA,		"AM",	"AMIGA 3000UX",
	PLATFORM_NEC,		"NE",	"NEC",
	PLATFORM_NIXDORF,	"ND",	"NIXDORF TARGON",
	PLATFORM_PRIME,		"PR",	"PRIME",
	PLATFORM_PYRAMID,	"PY",	"PYRAMID",
	PLATFORM_SONY,		"SY",	"SONY NEWS",
	PLATFORM_WYSE,		"WY",	"WYSE",
#endif /* OLD */

};

/*
**	Routine:	WL_valplat()
**
**	Function:	Validates the platform we are on.
**			
**
**	Input:		code	The platform code
**			
**
**	Output:		None
**
**	Return:		0 = Platform matches
**			1 = Incorrect platform
**
**	Warnings:	None
**
**	History:	05/20/92	Written by GSL
**
*/

int WL_valplat(char code[PLATFORM_CODE_LEN])
{
	const char* rcode = WL_platform_code();

	if (toupper(code[0]) == rcode[0] &&
	    toupper(code[1]) == rcode[1]   ) return(0);

	return(1);
}


/*
**	Routine:	WL_platform_number()
**
**	Function:	Figures out what platform this is.
**			
**	Args:		None
**			
**	Return:		The platform number from wplatdef.h
**
**	Warnings:	None
**
*/
int WL_platform_number(void)
{

#ifdef WIN32
	return PLATFORM_WINDOWS_NT;
#endif

#ifdef AIX_64
	return PLATFORM_AIX_64;
#else
#ifdef AIX
	return PLATFORM_AIX;
#endif
#endif

/*
* There are 4 flavors of HPUX (they will all define HPUX)
* The 2 flavours of 64-bit will both define HPUX_64
* The Itanium flavors will define also define HPUX_IA  
*/
#ifdef HPUX
#ifdef HPUX_IA
#ifdef HPUX_64
	return PLATFORM_HPUX_IA_64;
#else
	return PLATFORM_HPUX_IA;
#endif
#else /* NOT HPUX_IA */
#ifdef HPUX_64
	return PLATFORM_HPUX_64;
#else
	return PLATFORM_HPUX;
#endif
#endif /* HPUX_IA */
#endif /* HPUX */


#ifdef SOLARIS_64
	return PLATFORM_SOLARIS_64;
#else
#ifdef SOLARIS
	return PLATFORM_SOLARIS;
#endif
#endif

#ifdef LINUX_64
	return PLATFORM_LINUX_64;
#endif
#ifdef LINUX
	return PLATFORM_LINUX;
#endif

#ifdef SCO
	return PLATFORM_SCO;
#endif

#ifdef OSF1_ALPHA
	return PLATFORM_OSF1_ALPHA;
#endif

#ifdef UNIXWARE
	return PLATFORM_UNIXWARE;
#endif

#ifdef ULTRIX
	return PLATFORM_ULTRIX;
#endif
#ifdef DGUX
	return PLATFORM_DGUX;
#endif
#ifdef DGUX_INTEL
	return PLATFORM_DGUX_INTEL;
#endif
#ifdef UNISYS
	return PLATFORM_UNISYS;
#endif
#ifdef SEQUENT
	return PLATFORM_SEQUENT;
#endif

}

/*
**	Routine:	WL_platform_name()
**
**	Function:	Figures out what platform this is.
**			
**	Args:		None
**			
**	Return:		The platform name
**
**	Warnings:	None
**
*/
const char* WL_platform_name(void)
{
	static const char* name = NULL;

	if (name == NULL) /* First time */
	{
		int	i;
		int	num = WL_platform_number();

		for (i=0; platform_table[i].name; i++ )
		{
			if ( num == platform_table[i].number )
			{
				name = platform_table[i].name;
				break;
			}
		}
		if (NULL == name)
		{
			name = "UNKNOWN";
		}
	}

	return name;
}

/*
**	Routine:	WL_platform_define()
**
**	Function:	Figures out what platform this is.
**			
**	Args:		None
**			
**	Return:		The platform name
**
**	Warnings:	None
**
*/
const char* WL_platform_define(void)
{
	static const char* define_name = NULL;

	if (define_name == NULL) /* First time */
	{
		int	i;
		int	num = WL_platform_number();

		for (i=0; platform_table[i].name; i++ )
		{
			if ( num == platform_table[i].number )
			{
				define_name = platform_table[i].define_name;
				break;
			}
		}
		if (NULL == define_name)
		{
			define_name = "UNKNOWN";
		}
	}

	return define_name;
}


/*
**	Routine:	WL_platform_code()
**
**	Function:	Figures out what platform this is.
**			
**	Args:		None
**			
**	Return:		The platform code (as a null terminate string)
**
**	Warnings:	None
**
*/
const char* WL_platform_code(void)
{
	static  char code[PLATFORM_CODE_LEN+1] = "";

	if ('\0' == code[0]) /* First time */
	{
		int	i;
		int	num = WL_platform_number();

		for (i=0; platform_table[i].name; i++ )
		{
			if ( num == platform_table[i].number )
			{
				memcpy(code,platform_table[i].code,PLATFORM_CODE_LEN);
				code[PLATFORM_CODE_LEN] = '\0';
				break;
			}
		}
		if ('\0' == code[0])
		{
			strcpy(code,"??");
		}
	}

	return code;
}


/*
**	ROUTINE:	WISPPLAT()
**
**	FUNCTION:	Get the platform name and code
**
**	DESCRIPTION:	COBOL style args
**
**	ARGUMENTS:	
**	name		The returned platform name
**	code		The returned platform code
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void WISPPLAT(char name[PLATFORM_NAME_MAX], char code[PLATFORM_CODE_LEN])
{
	static char the_name[PLATFORM_NAME_MAX] = "";  /* COBOL STYLE - blank padded */

	if (!*the_name) /* First time */
	{
		const char* pname = WL_platform_name();
		memset(the_name,' ',PLATFORM_NAME_MAX);
		memcpy(the_name,pname,strlen(pname));
		
	}
	memcpy(name,the_name,PLATFORM_NAME_MAX);
	memcpy(code,WL_platform_code(),PLATFORM_CODE_LEN);
}


/*
**	Routine:	WL_plat_code()
**
**	Function:	Get platform info by code.
**			
**
**	Input:		code		The platform number.
**			
**
**	Output:		name		The platform name. (or NULL)
**			num		The platform number. (or NULL)
**
**	Return:		0 = found
**			1 = not found
**
**	Warnings:	None
**
**	History:	05/20/92	Written by GSL
**
*/

int	WL_plat_code(char code[PLATFORM_CODE_LEN], char* name, int* num)
{
	int	i;
	int	*num_p, numbuff;
	char	*name_p, buff[80], codebuff[PLATFORM_CODE_LEN];

	name_p = (name) ? name : buff;
	num_p  = (num)  ? num  : &numbuff;

	codebuff[0] = toupper(code[0]);
	codebuff[1] = toupper(code[1]);

	for (i=0; platform_table[i].name; i++ )
	{
		if ( 0 == memcmp(platform_table[i].code,codebuff,PLATFORM_CODE_LEN) )
		{
			strcpy(name_p,platform_table[i].name);
			*num_p = platform_table[i].number;
			return(0);
		}
	}
	return(1);
}


/*
**	Routine:	WL_putplattab()
**
**	Function:	To print a formatted table of platform codes.
**
**	Description:	All it does is printf calls to output the info to stdout.
**
**	Input:		None
**
**	Output:		stdout
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**
*/

void WL_putplattab(void)
{
	int	i,col;

	printf("\n");
	for(i=0,col=0;platform_table[i].name;i++,col++)
	{
		if (col == 3)
		{
			printf("\n");
			col = 0;
		}

		printf(" %2.2s - %-20s",platform_table[i].code, platform_table[i].name);
	}
	printf("\n\n");
}
/*
**	History:
**	$Log: platsubs.c,v $
**	Revision 1.25  2010/02/10 14:50:05  gsl
**	fix HPUX defines
**	
**	Revision 1.24  2010/02/10 03:56:29  gsl
**	add HPUX IA Itanium platform
**	
**	Revision 1.23  2007/08/06 17:48:31  gsl
**	Change "Windows NT (WIN32)" to "Windows (WIN32)"
**	Drop the "NT"
**	
**	Revision 1.22  2004/05/03 17:40:41  gsl
**	Add MS-DOS back into list of platforms (still in use)
**	
**	Revision 1.21  2004/05/03 17:24:49  gsl
**	Add MS-DOS back into list of platforms (still in use)
**	
**	Revision 1.20  2003/05/19 18:53:52  gsl
**	Add 32/64 bit to platform names
**	
**	Revision 1.19  2003/02/07 17:55:21  gsl
**	Rework the platform routines and add AIX HPUX SOLARIS 64-bit
**	
**	Revision 1.18  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.17  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.16  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.15  2002/09/04 03:38:32  gsl
**	PLATFORM_CODE LINUX "LX"
**	
**	Revision 1.14  2002/08/28 01:32:01  gsl
**	LINUX
**	
**	Revision 1.13  2002/08/08 20:13:31  gsl
**	remove unused platforms
**	
**	Revision 1.12  2002/07/09 04:13:59  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.11  1997/09/09 18:27:38  scass
**	Added DG/UX Intel as a platform
**	
**	Revision 1.10  1997-03-12 12:58:11-05  gsl
**	Changed to use define WIN32
**
**	Revision 1.9  1996-11-05 18:07:41-05  gsl
**	Add WISPPLAT() a cobol callable routine to retrieve the current platform
**
**	Revision 1.8  1996-08-19 15:32:39-07  gsl
**	drcs update
**
**	History:
**			05/20/92	Written GSL
**			05/26/92	Move WL_putplattab() from wauthorize.c to isolate platform_table GSL
**			09/09/97	Add DG/UX Intel platform  SMC
**
**
*/
