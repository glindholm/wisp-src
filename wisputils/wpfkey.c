/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/*  wpfkey.c															*/

/*  This utility will return the string value of the PFkey pressed.								*/

#include <video.h>
#include <vlocal.h>
#include <vcap.h>
#include <vdata.h>

main()
{
	int x;

	VL_vcapload();
	x= VL_vgetm();
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
	VL_vexit();
}
/*
**	History:
**	$Log: wpfkey.c,v $
**	Revision 1.11  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.10  2002/07/16 14:11:44  gsl
**	VL_ globals
**	
**	Revision 1.9  2002/07/15 20:16:17  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.8  1996/07/26 17:20:09  gsl
**	fix video includes
**	
**	Revision 1.7  1996-07-23 11:13:10-07  gsl
**	drcs update
**
**
**
*/
