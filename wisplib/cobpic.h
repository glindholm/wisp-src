/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		cobpic.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
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

/*
**	Function Prototypes
*/
void cobpic_edit(char* object, const char* source, const char* picture,int* errcode );
void cobxpic_edit(char	*object,		/* Input and Output areas */
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

void parse_pic(const char *picture,	/* The COBOL picture clause. */
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

#endif /* cobpic_H */

/*
**	History:
**	$Log: cobpic.h,v $
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
