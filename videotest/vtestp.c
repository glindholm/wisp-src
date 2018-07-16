/************************************************************************/
/*									*/
/*	     VIDEO - Video Interactive Development Environment		*/
/*									*/
/*			    Copyright (c) 1987				*/
/*									*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <v/video.h>

testp()
{
	testp1();
	return(SUCCESS);
}
testp1()
{
	register int i;
	int j;
	char c;
	char s[10];

	verase(FULL_SCREEN);
	vmove(0,0);

	vprint("\nTest P1 - verify parameter passing (multiple args).\n");

	i = 12345;
	c = 'Z';
	strcpy(s,"ABCDEF");
	j = 4321;

	vprint("The following two lines should appear identical\n");
	vprint("This is a test i=%d, c=%c, s=%s, j=%d\n",i,c,s,j);
	b1sub("This is a test i=%d, c=%c, s=%s, j=%d\n",i,c,s,j);
	vprint("Were the above two lines the same? ");
	c = vgetc(); vprint("%c\n",c);
	if ((c == 'y') || (c == 'Y'))
	{
		vprint("You have indicated success.\n");
		return(SUCCESS);
	}
	else
	{
		vprint("You have indicated failure.\n");
		return(FAILURE);
	}
}

b1sub(format,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
	char format[];
	int a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p;
{
	char work[256];
	register int ii;
	sprintf(work,format,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p);
	vprint("%s",work);
	return(SUCCESS);
}
