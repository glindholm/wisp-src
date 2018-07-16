			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wutils.h	Including this header at the end of a utility probram to stop the whole wisp and video libraries
**			from being included.
*/


#ifndef WUTILS_DEF
#define WUTILS_DEF

werrvre(buff)
char	*buff;
{
	vre("%s",buff,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
	return(0);
}

int vre(text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) 
char *text; 
char *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8;
{
	char	string[256];
	sprintf(string,text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);		/* Display the text.			*/
	fprintf(stderr,"%s\n",string);
	return(0);
}

vexit()
{
	return;
}

vonexit()
{
	return;
}

#ifdef VMS
wexit(num)
int num;
{
	int	rc;

	if ( num == 0 ) rc = 1;
	else		rc = 0;
	exit(rc);
}
#else /* !VMS */
wexit(num)
int num;
{
	exit(num);
}
#endif /* !VMS */

#endif /* WUTILS_DEF */


