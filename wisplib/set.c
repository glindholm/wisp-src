			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	SET
		FC	Char(1)		File protection class 	(Ignored)
		FH	Char(1)		Force Help		(Ignored)
		FN	Int(4)		Form Number
		IL	Char(8)		Input Lib
		IV	Char(6)		Input Vol
		JC	Char(1)		Job Class		(Not yet implemented)
		JL	Int(4)		Job Limit		(Not yet implemented)
		JS	Char(1)		Job Status		(Not yet implemented)
		LI	Int(4)		Lines Per Page
		OL	Char(8)		Output Lib
		OV	Char(6)		Output Vol
		P#/PR	Int(4)		Printer Number
		PC	Char(1)		Printer Class
		PM	Char(1)		Print Mode
		PF	Char(1)		Print file protection	(Ignored)
		PL	Char(8)		Program Lib		(Not yet implemented)
		PV	Char(6)		Program Vol		(Not yet implemented)
		RL	Char(8)		Run Lib
		RV	Char(6)		Run Vol
		RR	Int(4)		Return Code		(Not Supported)
		RS	Char(8)		Remote Spooling		(Ignored)
		SL	Char(8)		Spool Lib
		SV	Char(6)		Spool Vol
		WV	Char(6)		Work Vol
*/

#include <stdio.h>									/* Include standard I/O module.		*/
#include <varargs.h>									/* Function uses variable params.	*/

#include "idsistd.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"

#define NOT_SUPP 	{werrlog(ERRORCODE(2),keyword,0,0,0,0,0,0,0);}
#define NOT_YET		{werrlog(ERRORCODE(4),keyword,0,0,0,0,0,0,0);}

#define		ROUTINE		58000

SET(va_alist)										/* emulate the WANG VS SET usersub	*/

va_dcl

{
	va_list the_args;

	char	*keyword;
	char	*value;                            
	int4    long_value;
	int	to_do;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	wpload();

	va_start(the_args);
	to_do = va_count(the_args);
	va_start(the_args);


	while (to_do>1) 								/* Do this till all done.		*/
	{
		keyword = va_arg(the_args, char*);
		to_do--;
		value = va_arg(the_args, char*);
		to_do--;

		switch(keyword[0])							/* set what?				*/
		{
			case 'F':
				switch(keyword[1])
				{
					case 'C':					/* FC File protection class (Ignored)	*/
					case 'H':					/* FH Force Help            (Ignored)	*/
						break;
					case 'N':					/* FN Set form number.			*/
					 	memcpy(&long_value,value,4);
						wswap(&long_value);
						set_defs(DEFAULTS_FN,(char*)&long_value);
						break;
					default:
						NOT_SUPP
						break;
				}
				break;

			case 'I':
				switch(keyword[1])
				{
#ifdef MSDOS
					case 'D':					/* MSDOS Set the users ID (3 chars)	*/
						set_cuserid(value,3);
						break;
					case '8':					/* MSDOS Set the users ID (8 chars)	*/
						set_cuserid(value,8);
						break;
#endif /* MSDOS */
					case 'L':					/* IL INLIB.				*/
						set_defs(DEFAULTS_IL,value);
						break;
					case 'V':					/* IV INVOL.				*/
						set_defs(DEFAULTS_IV,value);
						break;
					default:
						NOT_SUPP
						break;
				}
				break;

			case 'J':

				NOT_YET
				break;

			case 'L':
				switch(keyword[1])
				{
					case 'I':					/* LI Set default lines per page printer.*/
					 	memcpy(&long_value,value,4);
						wswap(&long_value);
						set_defs(DEFAULTS_LI,(char*)&long_value);
						break;
					default:
						NOT_SUPP
						break;
				}
				break;

			case 'O':
				switch(keyword[1])
				{
					case 'L':					/* OL OUTLIB.				*/
						set_defs(DEFAULTS_OL,value);
						break;
					case 'V':					/* OV OUTVOL.				*/
						set_defs(DEFAULTS_OV,value);
						break;
					default:
						NOT_SUPP
						break;
				}
				break;

			case 'P':							/* 'P' commands				*/
				switch(keyword[1])
				{
					case '#':
					case 'R':
					 	memcpy(&long_value,value,4);
						wswap(&long_value);
						set_defs(DEFAULTS_PR,(char*)&long_value);
						break;
					case 'C':					/* PC set the printer class		*/
						set_defs(DEFAULTS_PC,value);
						break;
					case 'M':					/* PM set the printer mode		*/
						set_defs(DEFAULTS_PM,value);
						break;
					case 'F':					/* PF Print file protection (Ignored)	*/
						break;
					case 'L':					/* PL Program Lib			*/
						set_defs(DEFAULTS_PL,value);
						break;
					case 'V':					/* PV Program Vol			*/
						set_defs(DEFAULTS_PV,value);
						break;
					default:
						NOT_SUPP
						break;
	                        }
				break;

			case 'R':
				switch(keyword[1])
				{
					case 'L':					/* RL RUNLIB.				*/
						set_defs(DEFAULTS_RL,value);
						break;
					case 'V':					/* RV RUNVOL.				*/
						set_defs(DEFAULTS_RV,value);
						break;
					case 'S':					/* RS Remote Spooling (Ignored)		*/
						break;
					case 'R':
					default:
						NOT_SUPP
						break;
				}
				break;

			case 'S':
				switch(keyword[1])
				{
					case 'L':					/* Spool lib.				*/
						set_defs(DEFAULTS_SL,value);
						break;
					case 'V':
						set_defs(DEFAULTS_SV,value);
						break;
					default:
						NOT_SUPP
						break;
				}        
				break;

			case 'W':
				switch(keyword[1])
				{
					case 'V':					/* Work volume.				*/
						set_defs(DEFAULTS_WV,value);
						break;
					default:
						NOT_SUPP
						break;
				}
				break;

			default:
				NOT_SUPP
				break;
		}									/* End of switch.			*/

		save_defaults();							/* Update defaults			*/

	}										/* End of WHILE				*/

	if (to_do != 0)
	{
		werrlog(ERRORCODE(6),0,0,0,0,0,0,0,0);
	}
}       
