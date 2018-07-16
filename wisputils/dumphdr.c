static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
**	Revision 1.5  1996/07/23 18:12:51  gsl
**	drcs update
**	
**
**
*/
