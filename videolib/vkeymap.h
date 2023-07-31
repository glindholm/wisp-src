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
**	File:		vkeymap.h
**
**	Purpose:	To ...
*/

#ifndef vkeymap_H
#define vkeymap_H

#ifdef unix
#define VKEYMAP_FILE		"vkeymap"
#define VKEYMAP_HOME_FILE	".vkeymap"
#else
#define VKEYMAP_FILE		"VKEYMAP.DAT"
#define VKEYMAP_HOME_FILE	"VKEYMAP.DAT"
#endif

/*
**	Currently there are two types of meta keys, FUNCTION keys
**	and SPECIAL keys.  They are identified by a mask bit.
**	The function keys are generated by there value plus the mask bit.
**	The special keys are explicitly listed.
*/
#define	KM_FUNCTION_MASK	0x00010000
#define KM_SPECIAL_MASK		0x00020000

#define KM_NONE			0x00020000
#define KM_UP			0x00020001
#define KM_DOWN			0x00020002
#define KM_LEFT			0x00020003
#define KM_RIGHT		0x00020004
#define KM_ENTER		0x00020005
#define KM_PAGEUP		0x00020006
#define KM_PAGEDOWN		0x00020007
#define KM_HOME			0x00020008
#define KM_END			0x00020009
#define KM_INSERT		0x0002000a
#define KM_DELETE		0x0002000b
#define KM_BACKSPACE		0x0002000c
#define KM_UNKNOWN		0x0002ffff

char *VL_vkeymap_path();

#endif /* vkeymap_H */
/*
**	History:
**	$Log: vkeymap.h,v $
**	Revision 1.9  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.8  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.7  2002/07/15 20:16:09  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.6  1996/10/11 22:16:07  gsl
**	drcs update
**	
**
**
*/
