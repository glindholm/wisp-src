static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
/*----
This file unpacks ASCII files that have been appended together.
The files must contain a line
===== filename.ext
5 or more ===== followed by a file name
To mark the begining of the file
------*/

static char sccsid[]="@(#)kunpak.c	1.1 2/7/93";

static char pakname[512];
static char unpakname[512];
static char junk[512];
static char buff[512];
FILE *inf, *outf;

main(argc,argv)
{
	printf("Please name the input .pak file (full name) ?\n");
	gets(pakname);
	inf = fopen(pakname,"r");
	if(! inf)
		{
		printf("File %s not found\n",pakname);
		exit(0);
		}
	unpak(inf);
	fclose(inf);
	if(outf)
		fclose(outf);
}

unpak(infile)
FILE *infile;
{
	while(fgets(buff,511,infile))
		{
		if(!memcmp(buff,"=====",5))
			newfile();
		else
			output(buff);
		}
}

output(str)
char *str;
{
	int len;

	if(outf)
		{
		len = strlen(str);
		if(len)
			--len;
		if(str[len] == '\n')
			fprintf(outf,"%s",str);
		else
			fprintf(outf,"%s\n",str);
		}
}
newfile()
{
	if(outf)
		{
		fclose(outf);
		outf = NULL;
		}
	strcpy(unpakname,"");
	sscanf(buff,"%s %s",junk,unpakname);
	outf = fopen(unpakname,"w");
	if(!outf)
		printf("Error opening %s\n",unpakname);
}




/*
**	History:
**	$Log: kunpak.c,v $
**	Revision 1.2  1996/09/17 23:45:41  gsl
**	drcs update
**	
**
**
*/
