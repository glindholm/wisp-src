static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*		WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1991				*/
			/*	An unpublished work of International Digital Scientific, Inc.	*/
			/*			   All rights reserved.				*/
			/*									*/
			/************************************************************************/

/*  wpfkey.c															*/

/*  This utility will return the string value of the PFkey pressed.								*/

#include <video.h>
#include <vlocal.h>
#include <vcap.h>
#include <vdata.h>

main()
{
	int x;

	vcapload();
	x= vgetm();
	if (x == enter_key) printf("");
	if (x == return_key) printf("");
	if (x == fn1_key) printf("1");
	if (x == fn2_key) printf("2");
	if (x == fn3_key) printf("3");
	if (x == fn4_key) printf("4");
	if (x == fn5_key) printf("5");
	if (x == fn6_key) printf("6");
	if (x == fn7_key) printf("7");
	if (x == fn8_key) printf("8");
	if (x == fn9_key) printf("9");
	if (x == fn10_key) printf("10");
	if (x == fn11_key) printf("11");
	if (x == fn12_key) printf("12");
	if (x == fn13_key) printf("13");
	if (x == fn14_key) printf("14");
	if (x == fn15_key) printf("15");
	if (x == fn16_key) printf("16");
	vexit();
}
/*
**	History:
**	$Log: wpfkey.c,v $
**	Revision 1.8  1996-07-26 13:20:09-04  gsl
**	fix video includes
**
**	Revision 1.7  1996-07-23 11:13:10-07  gsl
**	drcs update
**
**
**
*/
