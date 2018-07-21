/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
******************************************************************************
*/


/*
**	wutils.h	Including this header at the end of a utility probram to stop the whole wisp and video libraries
**			from being included.
*/


#ifndef WUTILS_DEF
#define WUTILS_DEF


int VL_vre(text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) 
char *text; 
char *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8;
{
	char	string[256];
	sprintf(string,text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);		/* Display the text.			*/
	fprintf(stderr,"%s\n",string);
	return(0);
}

int werrvre(buff)
char	*buff;
{
	VL_vre("%s",buff,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
	return(0);
}

int WL_werr_message_box(const char *buff)
{
	werrvre(buff);
	return(0);
}

int VL_vexit()
{
	return 0;
}

void vwang_init_video()
{
	return;
}


void WL_wexit(int num)
{
	exit(num);
}

void VL_set_vsharedscreen_true(void)
{
}


#endif /* WUTILS_DEF */


/*
**	History:
**	$Log: wutils.h,v $
**	Revision 1.21  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.20  2003/02/04 18:43:32  gsl
**	fix -Wall warnings
**	
**	Revision 1.19  2002/12/06 22:52:46  gsl
**	WL_werr_message_box(const char *buff)
**	
**	Revision 1.18  2002/07/16 14:11:44  gsl
**	VL_ globals
**	
**	Revision 1.17  2002/07/15 20:16:17  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.16  2002/07/15 17:10:07  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2002/07/10 21:06:33  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.14  2002/07/09 04:13:47  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.13  2002/06/26 01:42:48  gsl
**	Remove VMS code
**	
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
