/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		costar.h
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Wisp for Windows COSTAR support
**
*/

#ifndef costar_H
#define costar_H
/*
**	Includes
*/
#include "video.h"
#include "vwang.h"

/*
**	Structures and Defines
*/

/*
**	COSTAR	- this define is used strictly with the Co*STAR product.
**	W4W	- this define is more generic mouse support.
*/
#if defined(unix) || defined(VMS) || defined(WIN32)
#define COSTAR
#define W4W
#endif

#define COSTAR_EDIT_VMODE		(VMODE_UNDERSCORE)
#define COSTAR_HOTSPOT_VMODE		(VMODE_REVERSE)
#define COSTAR_TAB_STOP_VMODE		(VMODE_REVERSE | VMODE_BLINK)

#define COSTAR_FAC_SET_EDIT(fac)	FAC_SET_UNDERSCORED(fac)
#define COSTAR_FAC_CLEAR_EDIT(fac)	FAC_CLEAR_UNDERSCORED(fac)
#define COSTAR_FAC_EDIT(fac)		FAC_UNDERSCORED(fac)

/*
**	Function Prototypes
*/

int use_costar(void);
unsigned char costar_fac(int the_fac);
void costar_enable_mouse(int flag);
int costar_get_mouse_position(int *m_row, int *m_col);
void costar_messtext(char *message);
void costar_errtext(char *message);
void costar_ctrl_api(const char *control);

void W4WAPI(char *buff);
int w4w_mask_row(int rownum, char *the_row, char *the_mask);
int w4w_click_row(int rownum, int click_offset, char *the_row);

#endif /* costar_H */

/*
**	History:
**	$Log: costar.h,v $
**	Revision 1.15  1997-08-25 16:15:24-04  gsl
**	Add prototype for costar_ctrl_api()
**
**	Revision 1.14  1997-07-12 19:00:18-04  gsl
**	Add support for WIN32
**
**	Revision 1.13  1996-08-23 11:58:39-04  gsl
**	Fix TAB_STOP_VMODE for NT
**
**	Revision 1.12  1996-08-16 11:12:39-07  gsl
**	Define W4W
**
**	Revision 1.11  1996-07-18 16:30:35-07  gsl
**	Define COSTAR only for unix and VMS
**
**	Revision 1.10  1996-07-15 09:43:02-07  gsl
**	Fix for NT
**
**	Revision 1.9  1995-07-06 10:01:19-07  gsl
**	add w4w_click_row()
**
 * Revision 1.8  1995/07/05  16:53:31  gsl
 * add w4w_mask_row() routine
 *
 * Revision 1.7  1995/06/26  10:59:28  gsl
 * add the routine costar_load_hotspots()
 *
 * Revision 1.6  1995/06/21  12:55:49  gsl
 * add W4WAPI()
 *
 * Revision 1.5  1995/05/01  09:46:30  gsl
 * change costar_message() to costar_errtext() and add costar_messtext()
 *
 * Revision 1.4  1995/04/25  09:52:33  gsl
 * drcs state V3_3_15
 *
 * Revision 1.3  1995/04/17  11:45:59  gsl
 * drcs state V3_3_14
 *
 * Revision 1.2  1995/04/06  08:03:19  gsl
 * add VMODE defines
 *
 * Revision 1.1  1995/04/05  09:35:24  gsl
 * Initial revision
 *
**
*/
