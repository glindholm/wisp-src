			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	WACCEPT		This routine builds an ACCEPT getparm.
*/

#include "idsistd.h"

WACCEPT(argcnt,item1,item2,item3,item4,item5,item6,item7,item8,item9,item10,item11,item12,item13,item14,item15,item16)
int4	*argcnt;
char	*item1,*item2,*item3,*item4,*item5,*item6,*item7,*item8,*item9,*item10,*item11,*item12,*item13,*item14,*item15,*item16;
{
	struct argst { char *ptrs[500]; } args;
	int4 	cnt;
	char	tags[16][8];
	char	*items[16];
	int	i;
	char	pfkeyrecvr[1];
	int4 	N[256];
	int4	pfkey, two;

	for (i=0; i<256; ++i) 
	{
		N[i]=i; 
		wswap(&N[i]);
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
	args.ptrs[cnt++] = (char *) "I ";
	args.ptrs[cnt++] = (char *) "R";
	args.ptrs[cnt++] = (char *) "ACCEPT  ";
	args.ptrs[cnt++] = (char *) pfkeyrecvr;
	args.ptrs[cnt++] = (char *) "0001";
	args.ptrs[cnt++] = (char *) "WISP  ";
	args.ptrs[cnt++] = (char *) &N[1];
				   /*123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 */
	args.ptrs[cnt++] = (char *) "                               Enter ACCEPT data                                ";
	args.ptrs[cnt++] = (char *) &N[79];

	for(i=0; i<*argcnt; i++)
	{
		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) tags[i];
		args.ptrs[cnt++] = (char *) items[i];
		args.ptrs[cnt++] = (char *) &N[68];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[9+i];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[2];
		args.ptrs[cnt++] = (char *) "C";
	}

	args.ptrs[cnt++] = (char *) "E";
	args.ptrs[cnt++] = (char *) "P";
	args.ptrs[cnt++] = (char *) &pfkey;

	two = 2;
	pfkey = 0;
	wvaset(&two);
	GETPARM(&args,&cnt);								/* Use the new method			*/

	for(i=0; i<*argcnt; i++)
	{
		nullterm(items[i],68);
	}
}

/*
	nullterm	Scan from the end of a string and insert a NULL after the last non-space.
*/
int nullterm(str,len)
char	*str;
int	len;
{
	for(len--;len>=0;len--)
	{
		if (str[len] != ' ')
		{
			break;
		}
	}
	str[len+1] = '\0';
	return(0);
}

