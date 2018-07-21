/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
	WACCEPT		This routine builds an ACCEPT getparm.
*/

#include <string.h>
#include "idsistd.h"
#include "wisplib.h"
#include "vssubs.h"

static void nullterm(char* str, int len);

void WACCEPT(argcnt,item1,item2,item3,item4,item5,item6,item7,item8,item9,item10,item11,item12,item13,item14,item15,item16)
int4	*argcnt;
char	*item1,*item2,*item3,*item4,*item5,*item6,*item7,*item8,*item9,*item10,*item11,*item12,*item13,*item14,*item15,*item16;
{
	char* gp_args[GETPARM_MAX_ARGS];
	int4 	cnt;
	char	tags[16][8];
	char	*items[16];
	int	i;
	char	pfkeyrecvr[1];
	int4 	N[256];
	int4	pfkey;

	for (i=0; i<256; ++i) 
	{
		N[i]=i; 
		WL_wswap(&N[i]);
	}

	items[0] = item1;
	if (*argcnt >=  2) items[ 1] = item2;
	if (*argcnt >=  3) items[ 2] = item3;
	if (*argcnt >=  4) items[ 3] = item4;
	if (*argcnt >=  5) items[ 4] = item5;
	if (*argcnt >=  6) items[ 5] = item6;
	if (*argcnt >=  7) items[ 6] = item7;
	if (*argcnt >=  8) items[ 7] = item8;
	if (*argcnt >=  9) items[ 8] = item9;
	if (*argcnt >= 10) items[ 9] = item10;
	if (*argcnt >= 11) items[10] = item11;
	if (*argcnt >= 12) items[11] = item12;
	if (*argcnt >= 13) items[12] = item13;
	if (*argcnt >= 14) items[13] = item14;
	if (*argcnt >= 15) items[14] = item15;
	if (*argcnt >= 16) items[15] = item16;

	for(i=0; i<*argcnt; i++)
	{
		memcpy(tags[i],items[i],8);
		memset(items[i],' ',80);
	}

	cnt = 0;
	gp_args[cnt++] = (char *) "I ";
	gp_args[cnt++] = (char *) "R";
	gp_args[cnt++] = (char *) "ACCEPT  ";
	gp_args[cnt++] = (char *) pfkeyrecvr;
	gp_args[cnt++] = (char *) "0001";
	gp_args[cnt++] = (char *) "WISP  ";
	gp_args[cnt++] = (char *) &N[1];
				   /*123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 */
	gp_args[cnt++] = (char *) "                               Enter ACCEPT data                                ";
	gp_args[cnt++] = (char *) &N[79];

	for(i=0; i<*argcnt; i++)
	{
		gp_args[cnt++] = (char *) "K";
		gp_args[cnt++] = (char *) tags[i];
		gp_args[cnt++] = (char *) items[i];
		gp_args[cnt++] = (char *) &N[68];
		gp_args[cnt++] = (char *) "A";
		gp_args[cnt++] = (char *) &N[9+i];
		gp_args[cnt++] = (char *) "A";
		gp_args[cnt++] = (char *) &N[2];
		gp_args[cnt++] = (char *) "C";
	}

	gp_args[cnt++] = (char *) "E";
	gp_args[cnt++] = (char *) "P";
	gp_args[cnt++] = (char *) &pfkey;

	pfkey = 0;
	GETPARM2(gp_args,cnt);								/* Use the new method			*/

	for(i=0; i<*argcnt; i++)
	{
		nullterm(items[i],68);
	}
}

/*
	nullterm	Scan from the end of a string and insert a NULL after the last non-space.
*/
static void nullterm(char* str, int len)
{
	for(len--;len>=0;len--)
	{
		if (str[len] != ' ')
		{
			break;
		}
	}
	str[len+1] = '\0';
}

/*
**	History:
**	$Log: waccept.c,v $
**	Revision 1.12  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.11  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.10  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.9  2002/07/12 17:01:02  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.8  1996/08/19 22:33:06  gsl
**	drcs update
**	
**
**
*/
