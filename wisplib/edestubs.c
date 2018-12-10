/*
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
*/


/*					
** Stubs to make EDE modules defined in regular WISPLIB					
*/


int EDE_using(void)	/* Is EDE being used */
{
	return 0;	/* NOT USING EDE */
}

/* NetronCap support */
int gen_ncpfkey(int type, char** wsb, int num_chars, int* st_win, int* end_win)		/* Stub so doesn't try to generate a   	*/
{											/*  pop-up PFkey menu window.		*/
	return(0);									/* Just do regular vwang display.	*/
}

/* Called from vwang */
int nc_pop_menu(int *filling, const char *terminate_list, unsigned char *no_mod, char *pfkey )
{
	return(0);									/* Return FAILURE.			*/
}

/* Called from vwang */
int ws_bar_menu( int curset, int vr, int vc, int ak, int ar, unsigned char* nm, int dp ) /* Stub so doesn't try to put up a bar	*/
{											/*  menu.				*/
	return(1);									/* Return success.			*/
}

/*
**	History:
**	$Log: edestubs.c,v $
**	Revision 1.20  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.19  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.18  2002/08/01 15:07:36  gsl
**	type warnings
**	
**	Revision 1.17  2002/08/01 02:42:17  gsl
**	fix type warning
**	
**	Revision 1.16  2002/07/11 20:29:07  gsl
**	Fix WL_ globals
**	
**	Revision 1.15  2002/07/10 21:05:15  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.14  2002/07/09 04:14:02  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.13  2002/06/26 01:42:45  gsl
**	Remove VMS code
**	
**	Revision 1.12  1996/08/19 22:32:17  gsl
**	drcs update
**	
**
**
*/
