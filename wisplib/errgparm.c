static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	ERR_GETPARM ... 	This routine will put up the operator intervention error screen.
*/

#include "idsistd.h"
#include "wangkeys.h"
#include "wperson.h"
#include "movebin.h"
#include "werrlog.h"

#include "wdefines.h"
#include "wisplib.h"

static	int4	N[255];
static	int	Ni=0;

void err_getparm(prname,messid,issuer,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8)
char 	prname[8], messid[4], issuer[6];
char *msg1,*msg2,*msg3,*msg4,*msg5,*msg6,*msg7,*msg8;
{                                                  
#define		ROUTINE		18100

	int4	pfkey;
	char	pfkey_rcvr;
	int	i;
	int4 	two=2;
	char	*cancel_msg;

	struct argst { char *ptrs[100]; } args;
	int4	cnt;
#define GP	args.ptrs[cnt++] = (char *)

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	if (!Ni) 
	{
		for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 
		{
			N[i]=i; 
			wswap(&N[i]);
		}
		++Ni;
	}

	wpload();									/* Get user personality and defaults.	*/

	if (pfkeys12())
	{
		pfkey = PFKEY_12_ENABLED | PFKEY_16_ENABLED;
		cancel_msg = "Press (12) to Cancel Processing    ";
	}
	else
	{
		pfkey = PFKEY_16_ENABLED;
		cancel_msg = "Press (16) to Cancel Processing    ";
	}
	

	wswap( &pfkey );								/* Do system dependent swap		*/



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

	wvaset(&two);
	GETPARM(&args,&cnt);						/* Use the new method			*/

}

/*
**	History:
**	$Log: errgparm.c,v $
**	Revision 1.11  1997/09/24 20:11:55  gsl
**	Add support for pfkeys12()
**	
**	Revision 1.10  1996-08-19 18:32:17-04  gsl
**	drcs update
**
**
**
*/
