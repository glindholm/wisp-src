/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
**	File:		fcopy.h
**
**	Purpose:	Header for function fcopy()
**
**
**	History:	
**	02/15/93	Written by GSL
**
*/

#ifndef FCOPY_H
#define FCOPY_H

int wisp_fcopy(const char* srcfile, const char* dstfile);

#endif /* FCOPY_H */



/*
**	History:
**	$Log: fcopy.h,v $
**	Revision 1.8  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.7  2002/10/07 20:54:48  gsl
**	Huge file support
**	
**	Revision 1.6  2002/06/26 01:41:12  gsl
**	Change fcopy() to wisp_fcopy()
**	
**	Revision 1.5  1996/08/19 22:32:19  gsl
**	drcs update
**	
**
**
*/
