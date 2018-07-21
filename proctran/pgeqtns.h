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

/* PGEQTNS.H															*/
/*		 Initialization of the equation specific arrays									*/

#ifdef INIT_COMMON

EXT char *search_equations[] = {	"0",
					"1",
					"2",
					"3",
					"4",
					"5",
					"6",
					"7",
					"8",
					"9",
					"'",
					"(",
					")",
					"!",
					",",
					"+",
					"-",
					"*",
					"/",
					"&",
					"=",
					">",
					"<",
					""
				};

EXT char *search_run_equations[] = {	"=",
					"(",
					")",
					"!",
					",",
					"+",
					"-",
					"*",
					"/",
					">",
					"<",
					""
				 };

EXT char *search_numbers[] = {	"0",
				"1",
				"2",
				"3",
				"4",
				"5",
				"6",
				"7",
				"8",
				"9",
				""
			};
#else

EXT char *search_equations[];
EXT char *search_run_equations[];
EXT char *search_numbers[];

#endif
/*
**	History:
**	$Log: pgeqtns.h,v $
**	Revision 1.9  2003/02/04 21:51:17  gsl
**	fix -Wall warnings
**	
**	Revision 1.8  1997/04/21 14:53:09  scass
**	Corr4ected copyright.
**	
**	Revision 1.7  1996-09-12 19:22:12-04  gsl
**	Add drcs headers
**
**
**
*/
