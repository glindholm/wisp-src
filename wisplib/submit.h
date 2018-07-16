/* 
	Copyright (c) 1997 NeoMedia Migrations, All rights reserved.
	$Id:$
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

#define SUBMIT_ERR_SUCCESS	0
#define SUBMIT_ERR_NOVOL	4
#define SUBMIT_ERR_VOLLOCK	8
#define SUBMIT_ERR_FILELOCK	12
#define SUBMIT_ERR_NOLIB	16
#define SUBMIT_ERR_NOFILE	20
#define SUBMIT_ERR_NOACCESS	28
#define SUBMIT_ERR_INVALID	40
#define SUBMIT_ERR_SERVICE	44
#define SUBMIT_ERR_ACCESS	52
#define SUBMIT_ERR_OPTIONS	56
#define SUBMIT_ERR_STATUS	900
#define SUBMIT_ERR_DISP		901
#define SUBMIT_ERR_CLASS	902
#define SUBMIT_ERR_ABORT	903
#define SUBMIT_ERR_TIME		904
#define SUBMIT_ERR_LIMIT	905
#define SUBMIT_ERR_ABORTED	1000

/*
**	Function Prototypes
*/
void SUBMIT();
void SETSUBMIT();
const char* submit_err(int error);

void setbatchcmd(const char* command);
const char* batchcmd(void);

void setbatchcmd95(const char* command);
const char* batchcmd95(void);

void setbatchlogvol(const char* value);
const char* batchlogvol(void);

void setbatchloglib(const char* value);
const char* batchloglib(void);

void setbatchpass(const char* value);
const char* batchpass(void);

void setbatchuser(const char* value);
const char* batchuser(void);

void setbatchserver(const char* value);
const char* batchserver(void);

void setbatchhold(const char* value);
const char* batchhold(void);

void setbatchrun(const char* value);
const char* batchrun(void);

#endif /* submit_H */

/*
**	History:
**	$Log: submit.h,v $
**	Revision 1.2  1997-08-23 17:14:33-04  gsl
**	Add all the new prototypes plus defines for all
**	the SUBMIT return codes
**
**	Revision 1.1  1997-08-18 11:10:27-04  gsl
**	Initial revision
**
**
**
*/
