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
**	File:		costar.h
**
**	Project:	wisp/lib
**
**	Purpose:	Wisp for Windows COSTAR support
**
*/

#ifndef costar_H
#define costar_H
/*
**	Includes
*/
#include "scnfacs.h"

/*
**	Structures and Defines
*/

/*
**	COSTAR	- this define is used strictly with the Co*STAR product.
**	W4W	- this define is more generic mouse support.
**
**	These are now supported for all versions of WISP.
*/
#define COSTAR
#define W4W

/*
**	Function Prototypes
*/

int use_costar(void);
fac_t costar_fac(fac_t the_fac);
void costar_enable_mouse(int flag);
int costar_get_mouse_position(int *m_row, int *m_col);
void costar_messtext(char *message);
void costar_errtext(char *message);
void costar_ctrl_api(const char *control);
void costar_title(const char *title);

int costar_edit_vmode(void);
int costar_hotspot_vmode(void);
int costar_tabstop_vmode(void);
int costar_message_vmode(void);
int costar_high_message_vmode(void);

void W4WAPI(char *buff);
int w4w_mask_row(const char *the_row, char *the_mask);
int w4w_click_row(int click_offset, const char *the_row);
int use_w4w(void);

int w4w_hotspot_vmode(void);

#endif /* costar_H */

/*
**	History:
**	$Log: costar.h,v $
**	Revision 1.22  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.21  2001/09/25 15:00:02  gsl
**	Remove unneeded ifdefs
**	
**	Revision 1.20  1999-05-11 12:48:49-04  gsl
**	Add costar_title()
**
**	Revision 1.19  1998-10-02 15:18:46-04  gsl
**	Add prototypes
**
**	Revision 1.18  1998-10-01 09:33:40-04  gsl
**	Move from wisp/lib to wisp/common
**
**	Revision 1.17  1998-09-30 17:10:39-04  gsl
**	fix proto's
**
**	Revision 1.16  1998-05-21 13:27:33-04  gsl
**	Replace VMODE defines with routines which dynamically return vmode
**	based on the costar attribute mode
**
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
