static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#ifdef unix
void wtransl()
{
	werrlog(102,"wtransl: Not Supported",0,0,0,0,0,0,0);
}
#endif
/*
**	History:
**	$Log: wtransl.c,v $
**	Revision 1.11.2.1  2002/11/14 21:12:29  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
**	
**	Revision 1.11  1996/08/19 22:33:26  gsl
**	drcs update
**	
**
**
*/
