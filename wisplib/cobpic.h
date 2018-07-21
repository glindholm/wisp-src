/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/

/*
**	File:		cobpic.h
**
**	Project:	WISPLIB
**
**	Purpose:	Cobol picture clauses
**
*/

#ifndef cobpic_H
#define cobpic_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

#define PIC_NOEDIT	0
#define PIC_ALPHAEDIT	1
#define PIC_NUMERIC	2

#define PIC_UNSIGNED	0
#define PIC_SIGNED	1
#define PIC_CR_SIGNED	2
#define PIC_DB_SIGNED	3

/*
**	Function Prototypes
*/
void WL_cobpic_edit(char* object, const char* source, const char* picture,int* errcode );
void WL_cobxpic_edit(char	*object,		/* Input and Output areas */
		  const char	*source, 
		  const char	*xpicture,		/* The expanded picture (also adjusted picture for Numeric only) */
		  int	size,			/* The number of character positions in pic */
		  int	pic_type,		/* The type of picture 0=noedit 1=Alphaedit 2=Numeric */
		  int	pic_dp,			/* Number of characters to the left of the decimal point 	(Numeric only)*/
		  int	blankdecimal,		/* Does the decimal portion contains no '9's	 		(Numeric only)*/
		  char	suppchar,		/* Zero suppression character 					(Numeric only)*/
		  int	suppidx,		/* Zero suppression index; offset of first 'Z' or '*' in pic	(Numeric only)*/
		  int	floatidx,		/* Floating character index; offset of start of float 		(Numeric only)*/
		  int	psigned,		/* Is the picture signed			 		(Numeric only)*/
		  int 	*errcode);		/* Error code 0=success */

void WL_parse_pic(const char *picture,	/* The COBOL picture clause. */
	       char	*xpic,		/* The expanded picture (also adjusted picture for Numeric only) */
	       int	*xflag,		/* Flag if xpic was created. */
	       int	*psize,		/* The number of character positions in pic */
	       int	*pic_type,	/* The type of picture 0=noedit 1=Alphaedit 2=Numeric */
	       int	*pic_dp,	/* Number of characters to the left of the decimal point 	(Numeric only)*/
	       int	*blankdecimal,	/* Does the decimal portion contains no '9's	 		(Numeric only)*/
	       char	*suppchar,	/* Zero suppression character 					(Numeric only)*/
	       int	*suppidx,	/* Zero suppression index; offset of first 'Z' or '*' in pic	(Numeric only)*/
	       int	*floatidx,	/* Floating character index; offset of start of float 		(Numeric only)*/
	       int	*psigned);	/* Is the picture signed			 		(Numeric only)*/

void WL_set_char_decimal_point(char dp);
char WL_get_char_decimal_point();

void WL_set_char_comma(char comma);
char WL_get_char_comma();

#endif /* cobpic_H */

/*
**	History:
**	$Log: cobpic.h,v $
**	Revision 1.6  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.5  2002/07/11 20:29:06  gsl
**	Fix WL_ globals
**	
**	Revision 1.4  2001/09/25 20:38:24  gsl
**	Add defines
**	
**	Revision 1.3  1997-12-17 15:32:01-05  gsl
**	fix prototypes for const
**
**	Revision 1.2  1996-06-27 20:04:08-04  gsl
**	Add prototypes for NT
**
**	Revision 1.1  1996-06-27 09:43:28-07  gsl
**	Initial revision
**
**
**
*/
