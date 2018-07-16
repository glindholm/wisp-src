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
**	File:		rvmap.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Remote Volume map
**
*/

#ifndef rvmap_H
#define rvmap_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int WL_remote_volume(const char *local_path, char *remote_path);

#endif /* rvmap_H */

/*
**	History:
**	$Log: rvmap.h,v $
**	Revision 1.4  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.3  2002/07/10 21:05:23  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.2  1996/01/03 11:35:00  gsl
**	Finished
**	
 * Revision 1.1  1996/01/03  10:52:52  gsl
 * Initial revision
 *
**
*/
