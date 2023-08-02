/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
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
**
******************************************************************************
*/

#include <stdio.h>
#include <fcntl.h>

unsigned int column = 80;
char *buf;

main(c,v)
char *v[];
{
	int ifd,ofd;
	char newname[200];
	char *calloc();
	char icol[10];

	if (c!=3)
	{
		fprintf(stderr,"usage: addlf <filename> <recsize>\n");
		exit(1);
	}
	ifd=open(v[1],0);
	if (ifd<0)
	{
		perror(v[1]);
		exit(1);	
	}

	sprintf(icol,"%s",v[2]);
	column = atoi(icol);
	if (!column)
	{
		printf("Invalid column (%d)",column);
		perror(v[2]);
		exit(1);	
	}
	if (!(buf = (char *)calloc(column,sizeof(char))))
	{
		printf("No memory! - need (%d)",column);
		perror(v[2]);
		exit(1);	
	}
	sprintf(newname,"%s.dat",v[1]);
	printf("%s ==> %s\n",v[1],newname);
	ofd=open(newname,O_WRONLY|O_CREAT);
	if (ofd<0)
	{
		perror(newname);
		exit(1);	
	}
	addlf(ifd,ofd);
	close(ifd);
	close(ofd);
	chmod (newname,0666);
	free(buf);
	return 0;
}
addlf(ifd,ofd)
int ifd,ofd;
{
	int cnt;
	char nl[2];

	nl[0]='\n';
	nl[1]=(char)0;
	memset(buf,0,sizeof(buf));
	while(cnt=read(ifd,buf,column))
	{
                if (column == 80 && cnt > 72) cnt = 72;
		while (buf[--cnt] == ' ');
		write(ofd,buf,cnt+1);
		write(ofd,nl,1);
		memset(buf,0,sizeof(buf));
	}
}
/*
**	History:
**	$Log: addlf.c,v $
**	Revision 1.5  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.4  1996/07/23 18:12:46  gsl
**	drcs update
**	
**
**
*/
