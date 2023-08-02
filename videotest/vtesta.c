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

#include <stdio.h>
#include <video.h>
#include <vlocal.h>
#include <vdata.h>
#include <vintdef.h>
#include <vmenu.h>
#include <vmodules.h>

#define CANCEL_CODE -1
#define CLOCK_CODE 13
#define NOTE_CODE 11
#define CALC_CODE 12
#define CALEND_CODE 14
#define PUZZLE_CODE 15

void testa()
{
	int4 i;
	struct video_menu goodies;

	verase(FULL_SCREEN);
	VL_vmenumode(STATIC_MENU);

	VL_vmenuinit(&goodies, POP_UP_MENU, VMODE_REVERSE, 10, 10, 0);
	VL_vmenuitem(&goodies, "Clock", CLOCK_CODE, NULL);
	VL_vmenuitem(&goodies, "Calculator", CALC_CODE, NULL);
	VL_vmenuitem(&goodies, "Calendar", CALEND_CODE, NULL);
	VL_vmenuitem(&goodies, "Notepad", NOTE_CODE, NULL);
	VL_vmenuitem(&goodies, "Puzzle", PUZZLE_CODE, NULL);
	VL_vmenuitem(&goodies, "", 0, NULL);        
	VL_vmenuitem(&goodies, "Cancel", CANCEL_CODE, NULL);

	while ((i = VL_vmenugo(&goodies)) != CANCEL_CODE)
	{
		switch(i)
		{
			case CALC_CODE:   {gcalc();    break;}
			case CLOCK_CODE:  {gclock();   break;}
			case CALEND_CODE: {gcalend();  break;}
			case NOTE_CODE:   {gnotepad(); break;}
			case PUZZLE_CODE: {gpuzzle();  break;}

		}
	}
}
