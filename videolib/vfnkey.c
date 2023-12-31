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


#include "video.h"
#include "vlocal.h"
#include "vdata.h"

int VL_vfnkey(key) int key;							/* Return the numeric value of a function key.	*/
{
	if (key == fn1_key) return(1);						/* Return the value of the function key.	*/
	if (key == fn2_key) return(2);
	if (key == fn3_key) return(3);
	if (key == fn4_key) return(4);
	if (key == fn5_key) return(5);
	if (key == fn6_key) return(6);
	if (key == fn7_key) return(7);
	if (key == fn8_key) return(8);
	if (key == fn9_key) return(9);
	if (key == fn10_key) return(10);
	if (key == fn11_key) return(11);
	if (key == fn12_key) return(12);
	if (key == fn13_key) return(13);
	if (key == fn14_key) return(14);
	if (key == fn15_key) return(15);
	if (key == fn16_key) return(16);
	if (key == fn17_key) return(17);
	if (key == fn18_key) return(18);
	if (key == fn19_key) return(19);
	if (key == fn20_key) return(20);
	if (key == fn21_key) return(21);
	if (key == fn22_key) return(22);
	if (key == fn23_key) return(23);
	if (key == fn24_key) return(24);
	if (key == fn25_key) return(25);
	if (key == fn26_key) return(26);
	if (key == fn27_key) return(27);
	if (key == fn28_key) return(28);
	if (key == fn29_key) return(29);
	if (key == fn30_key) return(30);
	if (key == fn31_key) return(31);
	if (key == fn32_key) return(32);
	return(0);								/* Was not a function key.			*/
}
/*
**	History:
**	$Log: vfnkey.c,v $
**	Revision 1.11  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/15 20:16:08  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.9  1996/10/11 22:16:04  gsl
**	drcs update
**	
**
**
*/
