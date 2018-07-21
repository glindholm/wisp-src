/*
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
*/

#if !defined EXTERNS_H
#define EXTERNS_H

extern cApplication	cApp;
extern cWindows		cWnd;
extern struct _GlbFlags {
	BOOL isTranslating,
		isCompiling;
}GlbFlags;

#endif

/*
**	History:
**	$Log: #Externs.h,v $
**	Revision 1.3  2003/06/18 16:43:06  gsl
**	Add CVS header and history
**	
**
*/
