/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		vutil.h
**
**	Project:	VIDEO/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef vutil_H
#define vutil_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int vre(char *text, ...);
int vre_window(char *text, ...);
int vml(int y);										/* Calculate the virtual map line no.	*/
int vmlx(int top, int y);								/* Calculate the virtual map line no.	*/
int vha(void);										/* Move to vcur_lin and vcur_col.	*/
int visible(char c, int a);								/* Check char c with attributes a.	*/
int vmaskc(int cset);									/* Mask all but character set bits.	*/
int vmaskm(int mode);									/* Mask all but rendition bits.		*/
unsigned char *vsss(int row, int col, int rows, int cols);				/* Save a screen segment.		*/
int vrss(unsigned char *loc);								/* Restore a screen segment.		*/
void varb(int row, int col, int rows, int cols);					/* Invalidate map section.		*/


#endif /* vutil_H */

/*
**	History:
**	$Log: vutil.h,v $
**	Revision 1.3  1997-07-09 12:38:37-04  gsl
**	Removed obsolete routines
**
**	Revision 1.2  1996-07-18 13:14:29-04  jockc
**	correct decl of vre and vre_window for new ... arg syntax
**
**	Revision 1.1  1996-03-28 13:07:14-08  gsl
**	Initial revision
**
**
*/
