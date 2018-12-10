/*
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
*/


/*
	ERR_GETPARM ... 	This routine will put up the operator intervention error screen.
*/
#include <string.h>

#include "idsistd.h"
#include "wangkeys.h"
#include "wperson.h"
#include "werrlog.h"

#include "wdefines.h"
#include "wisplib.h"
#include "vssubs.h"

static	int4	N[255];
static	int	Ni=0;

void WL_err_getparm(prname,messid,issuer,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8)
char 	prname[8], messid[4], issuer[6];
char *msg1,*msg2,*msg3,*msg4,*msg5,*msg6,*msg7,*msg8;
{                                                  
#define		ROUTINE		18100

	int4	pfkey;
	char	pfkey_rcvr;
	int	i;
	char	*cancel_msg;

	char* gp_args[GETPARM_MAX_ARGS];
	int4	cnt;
#define GP	gp_args[cnt++] = (char *)

	WL_wtrace("ERR_GETPARM","ENTRY","Entry into ERR_GETPARM");

	if (!Ni) 
	{
		for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 
		{
			N[i]=i; 
			WL_wswap(&N[i]);
		}
		++Ni;
	}

	WL_wpload();									/* Get user personality and defaults.	*/

	if (WL_pfkeys12())
	{
		pfkey = PFKEY_12_ENABLED | PFKEY_16_ENABLED;
		cancel_msg = "Press (12) to Cancel Processing    ";
	}
	else
	{
		pfkey = PFKEY_16_ENABLED;
		cancel_msg = "Press (16) to Cancel Processing    ";
	}
	

	WL_wswap( &pfkey );								/* Do system dependent swap		*/



	cnt = 0;
	GP "R ";	GP  "R";	GP  prname;	GP  &pfkey_rcvr;	GP  messid;	GP  issuer;	GP  &N[0];
	GP "T";	GP  "***  ERROR Processor  ***";	GP  &N[25];		GP  "A"; GP  &N[9];  GP  "A"; GP  &N[28];
	GP msg1?"T":"t";	GP  msg1;	GP  &N[msg1?strlen(msg1):0];	GP  "A"; GP  &N[11]; GP  "A"; GP  &N[2];
	GP msg2?"T":"t";	GP  msg2;	GP  &N[msg2?strlen(msg2):0];	GP  "A"; GP  &N[12]; GP  "A"; GP  &N[2];
	GP msg3?"T":"t";	GP  msg3;	GP  &N[msg3?strlen(msg3):0];	GP  "A"; GP  &N[13]; GP  "A"; GP  &N[2];
	GP msg4?"T":"t";	GP  msg4;	GP  &N[msg4?strlen(msg4):0];	GP  "A"; GP  &N[14]; GP  "A"; GP  &N[2];
	GP msg5?"T":"t";	GP  msg5;	GP  &N[msg5?strlen(msg5):0];	GP  "A"; GP  &N[15]; GP  "A"; GP  &N[2];
	GP msg6?"T":"t";	GP  msg6;	GP  &N[msg6?strlen(msg6):0];	GP  "A"; GP  &N[16]; GP  "A"; GP  &N[2];
	GP msg7?"T":"t";	GP  msg7;	GP  &N[msg7?strlen(msg7):0];	GP  "A"; GP  &N[17]; GP  "A"; GP  &N[2];
	GP msg8?"T":"t";	GP  msg8;	GP  &N[msg8?strlen(msg8):0];	GP  "A"; GP  &N[18]; GP  "A"; GP  &N[2];
	GP "T";	GP  cancel_msg;	GP  &N[35];	GP  "A"; GP  &N[24]; GP  "A"; GP  &N[22];
	GP "N";
	GP "P";	GP  &pfkey;

	GETPARM2(gp_args,cnt);						/* Use the new method			*/

}

/*
**	History:
**	$Log: errgparm.c,v $
**	Revision 1.17  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.16  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.15  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.14  2002/12/09 21:09:27  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.13  2002/07/12 17:00:55  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.12  2002/07/10 21:05:15  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.11  1997/09/24 20:11:55  gsl
**	Add support for pfkeys12()
**	
**	Revision 1.10  1996-08-19 18:32:17-04  gsl
**	drcs update
**
**
**
*/
