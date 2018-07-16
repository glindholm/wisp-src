/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
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


int vre(text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) 
char *text; 
char *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8;
{
	char	string[256];
	sprintf(string,text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);		/* Display the text.			*/
	fprintf(stderr,"%s\n",string);
	return(0);
}

werrvre(buff)
char	*buff;
{
	vre("%s",buff,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
	return(0);
}

werr_message_box(char *buff)
{
	werrvre(buff);
	return(0);
}

vexit()
{
	return 0;
}

void vwang_init_video()
{
	return;
}


#ifdef VMS
void wexit(int num)
{
	int	rc;

	if ( num == 0 ) rc = 1;
	else		rc = 0;
	exit(rc);
}
#else /* !VMS */
void wexit(int num)
{
	exit(num);
}
#endif /* !VMS */

void set_vsharedscreen_true(void)
{
}


#endif /* WUTILS_DEF */


/*
**	History:
**	$Log: wutils.h,v $
**	Revision 1.12  2001/10/15 14:21:16  gsl
**	Change vwang_set_videocap() to vwang_init_video()
**	
**	Revision 1.11  1997-09-30 14:04:30-04  gsl
**	Add set_vsharedscreen_true()
**
**	Revision 1.10  1996-11-04 18:57:56-05  gsl
**	Add a dummy vwang_set_videocap()
**
**	Revision 1.9  1996-07-23 11:13:14-07  gsl
**	drcs update
**
**
**
*/
