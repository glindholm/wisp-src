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
**	File:		machid.h
**
**	Project:	WISP COMMON
**
**	RCS:		$Source:$
**
**	Purpose:	Header for machid.c
**
*/

#ifndef machid_H
#define machid_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#ifndef MAX_COMPUTERNAME_LENGTH
#define MAX_COMPUTERNAME_LENGTH         32
#endif

#define MAX_MACHINEID_LENGTH		256

/*
**	Function Prototypes
*/
int WL_getmachineid(char* machineid);
char *WL_computername(char *cname);
void WL_encodemachid(const char *source, char *target);
void WL_GetMachineIdFromName(char* machineid, const char* computername, const char* platform_code);

#endif /* machid_H */

/*
**	History:
**	$Log: machid.h,v $
**	Revision 1.5  2004/06/14 15:42:57  gsl
**	make external the routines to generate MachineId from the Unix Machine Name
**	Used by Linux and Alpha. Add M function to wauthorize to generate machine id for Window or Linux or Alpha from the machine name.
**	
**	Revision 1.4  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.3  2002/07/09 04:14:04  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.2  2001/09/27 13:45:28  gsl
**	Add define for machineid length
**	
**	Revision 1.1  1997-03-06 16:22:04-05  gsl
**	Initial revision
**
**
**
**
*/
