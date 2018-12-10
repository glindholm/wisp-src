/*
******************************************************************************
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
******************************************************************************
*/


/*
**	File:		vutil.h
**
**	Project:	VIDEO/LIB
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
#define vre	VL_vre
/*
**	Function Prototypes
*/
void VL_vre_set_logfile(const char* filepath);
void VL_vre_write_logfile(const char* buff);

int VL_vre(char *text, ...);
int VL_vre_window(char *text, ...);
int VL_vml(int y);									/* Calculate the virtual map line no.	*/
int VL_vmlx(int top, int y);								/* Calculate the virtual map line no.	*/
int VL_vha(void);									/* Move to vcur_lin and vcur_col.	*/
int VL_vmaskc(int cset);								/* Mask all but character set bits.	*/
int VL_vmaskm(int mode);								/* Mask all but rendition bits.		*/
void VL_varb(int row, int col, int rows, int cols);					/* Invalidate map section.		*/
void VL_vtitle(const char *titlestr);
void VL_trace_enable();
void VL_trace(char* text, ...);

#endif /* vutil_H */

/*
**	History:
**	$Log: vutil.h,v $
**	Revision 1.7  2011/08/22 03:10:00  gsl
**	Support for WinSSHd on Windows
**	
**	Revision 1.6  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.5  2002/07/15 20:16:16  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.4  2001/10/15 13:12:55  gsl
**	Add proto's
**	
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
