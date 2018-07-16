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
**	File:		submit.h
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	Header for SUBMIT 
**
*/

#ifndef submit_H
#define submit_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#define SUBMIT_USER_LEN		40
#define SUBMIT_PASS_LEN		40

/*
**	Function Prototypes
*/
void SUBMIT(char* l_file, ...);
void SETSUBMIT(short *arg_count_ptr, ...);

const char* WL_submit_err(int error);

void WL_setbatchcmd(const char* command);
const char* WL_batchcmd(void);

void WL_setbatchcmd95(const char* command);
const char* WL_batchcmd95(void);

void WL_setbatchlogvol(const char* value);
const char* WL_batchlogvol(void);

void WL_setbatchloglib(const char* value);
const char* WL_batchloglib(void);

void WL_setbatchpass(const char* value);
const char* WL_batchpass(void);

void WL_setbatchuser(const char* value);
const char* WL_batchuser(void);

void WL_setbatchserver(const char* value);
const char* WL_batchserver(void);

void WL_setbatchhold(const char* value);
const char* WL_batchhold(void);

void WL_setbatchrun(const char* value);
const char* WL_batchrun(void);

#endif /* submit_H */

/*
**	History:
**	$Log: submit.h,v $
**	Revision 1.6  2003/03/19 20:03:17  gsl
**	Standardize SUBMIT return code defines
**	
**	Revision 1.5  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.4  2003/01/20 17:01:56  gsl
**	Change to use stdarg.h
**	plus document args
**	
**	Revision 1.3  2002/07/12 20:40:38  gsl
**	Global unique WL_ changes
**	
**	Revision 1.2  1997/08/23 21:14:33  gsl
**	Add all the new prototypes plus defines for all
**	the SUBMIT return codes
**	
**	Revision 1.1  1997-08-18 11:10:27-04  gsl
**	Initial revision
**
**
**
*/
