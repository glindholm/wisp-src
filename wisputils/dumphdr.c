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

#include <stdio.h>

main(c,v)
char *v[];
int c;
{
	FILE *x;
	char encf[5];
	int pos,ch;
	struct 
	{
		char d1[28];
		char lib[8];
		short seq;
		char d2[8];
		char org;
		char flags;
		short d3;
		char name[8];

	}foo;
	
	

	x=fopen(v[1],"r");

	pos=0;
	strcpy(encf,"ENCF");
	
	while ((ch=fgetc(x))!=EOF)
	{
		if (ch==encf[pos])
		{
			++pos;
			if (pos == 4)
			{
				pos=0;
				fread(x,&foo,sizeof(foo));
				printf("%3d  %8s %8s  %02x %02x\n",foo.seq,foo.lib,foo.name,foo.org,foo.flags);
			}
		}
		else pos=0;
			
		
	}
}
/*
**	History:
**	$Log: dumphdr.c,v $
**	Revision 1.6  2003/02/04 18:50:26  gsl
**	fix copyright header
**	
**	Revision 1.5  1996/07/23 18:12:51  gsl
**	drcs update
**	
**
**
*/
