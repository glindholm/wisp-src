/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/*
**	File:		vraw.h
**
**	Project:	videolib
**
**	RCS:		$Source:$
**
**	Purpose:	Raw layer routines
**
*/

#ifndef VRAW_H
#define VRAW_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/



extern void vraw_stty_sync(void);
extern void vraw_stty_restore(void);
extern int  vrawtimeout_check(void);
extern void vrawtimeout(int seconds);
extern void vrawtimeout_clear(void);
extern int  vrawcursor(int state);
extern int  vrawscroll(int dir);
extern void vrawsetscroll(int top,int bottom);
extern void vrawerase(int fr,int fc,int tr,int tc);
extern void vrawerasetype(int type);
extern void vrawmove(int row, int col);
extern void vrawattribute(int atr);
extern void vrawsetattributes(int attrs[]);
extern int  vrawntcn_get_mouse_position( int *row, int *col );
extern void VL_vshut(void);
extern int  vrawexit(void);
extern int  vrawputc(char ch);
extern int  vrawprint(char *buf);
extern char vrawinput(void);
extern char vrawcheck(void);
extern void vrawtitle(const char *title);
extern int  vrawflush(void);		/* Flush output */
extern void vrawerror(const char* message);
extern int vrawdirectio(void);

#ifdef WIN32
extern void vrawDebugBreak(void);
#endif

#ifdef unix
extern int  vrawerrno(void);			/* Get vraw layer error. */
extern int  vrawttyset(int fildes, void* tt);
extern int  vrawttyget(int fildes, void* tt);
extern void* vrawttyalloc(void);
#endif /* unix */

#endif /* VRAW_H */

/*
**	History:
**	$Log: vraw.h,v $
**	Revision 1.4  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.3  2002/07/15 20:16:12  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.2  1997/07/16 02:00:53  gsl
**	Added vrawDebugBreak() for WIN32
**	
**	Revision 1.1  1997-06-24 10:56:47-04  gsl
**	Initial revision
**
**
**
**
*/
