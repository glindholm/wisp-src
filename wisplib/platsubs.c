			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/



/*
**	File:		platsubs.c
**
**	Function:	subroutines used by to validate platform number.
**
**	Routines:	valplat()	Validate platform.
**			whatplat()	What platform is this?
**			plat_num()	Get platform info by number.
**			plat_code()	Get platform info by code.
**			putplattab()	Print a formatted table of platform codes.
**
**	History:
**			05/20/92	Written GSL
**			05/26/92	Move putplattab() from wauthorize.c to isolate platform_table GSL
*/

#include <stdio.h>


#include "wplatdef.h"

struct platform_struct
{
	int	number;			/* The platform number 1000's are unix  2000's are non-unix 	*/
	char	*code;			/* Two char unique code (must not include char '0' zero)	*/
	char	*name;			/* The platform name (max 20 char)				*/
};

static struct platform_struct	platform_table[] = 
{
	PLATFORM_UNKNOWN,	"UK",	"Unknown",

	PLATFORM_AIX,		"AX",	"AIX RISC",
	PLATFORM_ULTRIX,	"UL",	"Ultrix (DEC RISC)",
	PLATFORM_HPUX,		"HP",	"HP/UX 9000 RISC",
	PLATFORM_SUNOS,		"SU",	"SunOS",
	PLATFORM_DGUX,		"DG",	"DG/UX Aviion",
	PLATFORM_SCO,		"SC",	"SCO UNIX 386/486",
	PLATFORM_NCR486,	"NC",	"NCR 386/486",
	PLATFORM_NCR32,		"NT",	"NCR Tower/32 68020",
	PLATFORM_MIPS,		"MI",	"Mips R2000",
	PLATFORM_ATT3B2,	"AT",	"AT&T 3b2",
	PLATFORM_BULL,		"BU",	"BULL DPX/2",
	PLATFORM_MOTOROLA,	"MO",	"Motorola 88000",
	PLATFORM_UNISYS,	"US",	"Unisys 6000 SVR4",
	PLATFORM_SEQUENT,	"SQ",	"Sequent",
	PLATFORM_AIX_PS2,	"AP",	"AIX PS/2",
	PLATFORM_ULTRIX_VAX,	"UV",	"Ultrix (VAX)",
	PLATFORM_ULTRIX_ALPHA,	"UA",	"Ultrix (ALPHA)",
	PLATFORM_AIX_3090,	"A3",	"AIX 3090",
	PLATFORM_NEXT,		"NX",	"NeXT 68040",
	PLATFORM_MPEIX,		"HI",	"HP MPE/iX",
	PLATFORM_STRATUS,	"SX",	"Stratus FTX",
	PLATFORM_ICL,		"IC",	"ICL DRS 6000",
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

	PLATFORM_VMS,		"VM",	"VAX/VMS",
	PLATFORM_MSDOS,		"DS",	"MS-DOS 386/484",
	PLATFORM_HPMPE,		"HM",	"HP MPE",
	PLATFORM_VMS_ALPHA,	"VA",	"VMS (ALPHA)",

	-1,			(char *)0, (char *)0,
};


/*
**	Routine:	valplat()
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

int valplat(code)
char	code[2];
{
	char	rcode[2];
	int	num;

	num = whatplat(NULL,rcode);
	if (!num) return(0);

	if (toupper(code[0]) == rcode[0] &&
	    toupper(code[1]) == rcode[1]   ) return(0);

	return(1);
}


/*
**	Routine:	whatplat()
**
**	Function:	Figures out what platform this is.
**			
**	Input:		None
**			
**
**	Output:		name		The name of the system. (or NULL)
**			code		The 2 character code.   (or NULL)
**
**	Return:		The platform number from wplatdef.h
**			0 = failure
**
**	Warnings:	None
**
**	History:	05/20/92	Written by GSL
**
*/

int	whatplat(name,code)
char	*name;
char	code[2];
{
	int	p;
	char	*name_p, *code_p, buff[80];

	name_p = (name) ? name : buff;
	code_p = (code) ? code : buff;

	p = 0;

#ifdef AIX
	p = PLATFORM_AIX;
#endif
#ifdef ULTRIX
	p = PLATFORM_ULTRIX;
#endif
#ifdef HPUX
	p = PLATFORM_HPUX;
#endif
#ifdef SUNOS
	p = PLATFORM_SUNOS;
#endif
#ifdef DGUX
	p = PLATFORM_DGUX;
#endif
#ifdef SCO
	p = PLATFORM_SCO;
#endif
#ifdef NCR486
	p = PLATFORM_NCR486;
#endif
#ifdef NCR32
	p = PLATFORM_NCR32;
#endif
#ifdef MIPS
	p = PLATFORM_MIPS;
#endif
#ifdef ATT3B2
	p = PLATFORM_ATT3B2;
#endif
#ifdef BULL
	p = PLATFORM_BULL;
#endif
#ifdef MOTOROLA
	p = PLATFORM_MOTOROLA;
#endif
#ifdef UNISYS
	p = PLATFORM_UNISYS;
#endif
#ifdef SEQUENT
	p = PLATFORM_SEQUENT;
#endif
#ifdef AIX_PS2
	p = PLATFORM_AIX_PS2;
#endif
#ifdef ULTRIX_VAX
	p = PLATFORM_ULTRIX_VAX;
#endif
#ifdef ULTRIX_ALPHA
	p = PLATFORM_ULTRIX_ALPHA;
#endif
#ifdef AIX_3090
	p = PLATFORM_AIX_3090;
#endif
#ifdef NEXT
	p = PLATFORM_NEXT;
#endif
#ifdef MPEIX
	p = PLATFORM_MPEIX;
#endif
#ifdef STRATUS
	p = PLATFORM_STRATUS;
#endif
#ifdef ICL
	p = PLATFORM_ICL;
#endif
#ifdef ALTOS
	p = PLATFORM_ALTOS;
#endif
#ifdef CONCUR
	p = PLATFORM_CONCUR;
#endif
#ifdef CTRLDATA
	p = PLATFORM_CTRLDATA;
#endif
#ifdef CONVRG
	p = PLATFORM_CONVRG;
#endif
#ifdef AMIGA
	p = PLATFORM_AMIGA;
#endif
#ifdef NEC
	p = PLATFORM_NEC;
#endif
#ifdef NIXDORF
	p = PLATFORM_NIXDORF;
#endif
#ifdef PRIME
	p = PLATFORM_PRIME;
#endif
#ifdef PYRAMID
	p = PLATFORM_PYRAMID;
#endif
#ifdef SONY
	p = PLATFORM_SONY;
#endif
#ifdef WYSE
	p = PLATFORM_WYSE;
#endif

/*
**	NON-UNIX PLATFORMS
*/
#ifdef VMS
	p = PLATFORM_VMS;
#endif
#ifdef MSDOS
	p = PLATFORM_MSDOS;
#endif
#ifdef HPMPE
	p = PLATFORM_HPMPE;
#endif
#ifdef VMS_ALPHA
	p = PLATFORM_VMS_ALPHA;
#endif

	if (plat_num(p, name_p, code_p))
	{
		strcpy(name_p,"??");
		memcpy(code_p,"??",2);
		p = 0;
	}

	return(p);
}


/*
**	Routine:	plat_num()
**
**	Function:	Get platform info by number.
**			
**
**	Input:		num		The platform number.
**			
**
**	Output:		name		The platform name. (or NULL)
**			code		The platform code. (or NULL)
**
**	Return:		0 = found
**			1 = not found
**
**	Warnings:	None
**
**	History:	05/20/92	Written by GSL
**
*/

int	plat_num(num,name,code)
int	num;
char	*name;
char	code[2];
{
	int	i;
	char	*name_p, *code_p, buff[80];

	name_p = (name) ? name : buff;
	code_p = (code) ? code : buff;

	for (i=0; platform_table[i].name; i++ )
	{
		if ( num == platform_table[i].number )
		{
			strcpy(name_p,platform_table[i].name);
			memcpy(code_p,platform_table[i].code,2);
			return(0);
		}
	}
	return(1);
}


/*
**	Routine:	plat_code()
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

int	plat_code(code,name,num)
char	code[2];
char	*name;
int	*num;
{
	int	i;
	int	*num_p, numbuff;
	char	*name_p, buff[80], codebuff[2];

	name_p = (name) ? name : buff;
	num_p  = (num)  ? num  : &numbuff;

	codebuff[0] = toupper(code[0]);
	codebuff[1] = toupper(code[1]);

	for (i=0; platform_table[i].name; i++ )
	{
		if ( 0 == memcmp(platform_table[i].code,codebuff,2) )
		{
			strcpy(name_p,platform_table[i].name);
			*num_p = platform_table[i].number;
			return(0);
		}
	}
	return(1);
}


/*
**	Routine:	putplattab()
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

putplattab()
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
