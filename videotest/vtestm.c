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

static void vmtest();
static int vmt4();

#define CANCEL_CODE -1
#define CLOCK_CODE 13
#define NOTE_CODE 11
#define CALC_CODE 12
#define CALEND_CODE 14
#define PUZZLE_CODE 15

int testm()
{
	register int i;

	verase(FULL_SCREEN);
	VL_vmenumode(STATIC_MENU);
	for (i = 0; i < 24; i++) vtext(0,i,0,"  MENU TEST \t MENU TEST \t MENU TEST \t MENU TEST");
	vmtest();
	verase(FULL_SCREEN);
	VL_vmenumode(STATIC_MENU);
	for (i = 0; i < 24; i++) vtext(0,i,0,"                         MACRO RESTORE IS ON");
	vmtest();

	verase(FULL_SCREEN);
	vscreen(LIGHT);
	VL_vmenu_pfkeys(ON);
	vtext(CLEAR,10,34," Function key processing is on.");
	vmtest();
	VL_vmenu_pfkeys(OFF);
	vscreen(DARK);

	verase(FULL_SCREEN);
	VL_vmenumode(DYNAMIC_MENU);
	vmt4();

	return(SUCCESS);
}

static void vmtest()
{
	int4 i;
	struct video_menu bmtest, manhattan, new_york, greenwich;
	struct video_menu goodies, preferences, run;
	struct video_menu progsel, stock, yesno;

	VL_vmenuinit(&bmtest, BAR_MENU, REVERSE, 0, 2, 4);
		VL_vmenuitem(&bmtest, "Goodies", 0, &goodies);
			VL_vmenuinit(&goodies, 0, REVERSE, 0, 0, 0);
			VL_vmenuitem(&goodies, "Clock", CLOCK_CODE, NULL);
			VL_vmenuitem(&goodies, "Calculator", CALC_CODE, NULL);
			VL_vmenuitem(&goodies, "Calendar", CALEND_CODE, NULL);
			VL_vmenuitem(&goodies, "Notepad", NOTE_CODE, NULL);
			VL_vmenuitem(&goodies, "Puzzle", PUZZLE_CODE, NULL);
		VL_vmenuitem(&bmtest, "Help", 0, NULL);
		VL_vmenuitem(&bmtest, "Run", 0, &run);
			VL_vmenuinit(&run, 0, REVERSE, 0, 0, 0);
			VL_vmenuitem(&run, "Submit Procedure", 21, NULL);
			VL_vmenuitem(&run, "Detach Program", 22, NULL);
			VL_vmenuitem(&run, "Run Program", 23, NULL);
			VL_vmenuitem(&run, "Run Application", 24, &progsel);
				VL_vmenuinit(&progsel, 0, REVERSE, 0, 0, 0);
				VL_vmenuitem(&progsel, "Accounts Payable", 101, NULL);
				VL_vmenuitem(&progsel, "Accounts Receivable", 102, NULL);
				VL_vmenuitem(&progsel, "General Ledger", 103, NULL);
				VL_vmenuitem(&progsel, "Invoice Tracking", 104, NULL);
				VL_vmenuitem(&progsel, "Inventory Control", 105, &stock);
					VL_vmenuinit(&stock, 0, REVERSE, 0, 0, 0);
					VL_vmenuitem(&stock, "Atlanta", 401, NULL);
					VL_vmenuitem(&stock, "Chicago", 402, NULL);
					VL_vmenuitem(&stock, "Dallas", 403, NULL);
					VL_vmenuitem(&stock, "Los Angeles", 404, NULL);
					VL_vmenuitem(&stock, "New York", 405, &new_york);
						VL_vmenuinit(&new_york, 0, REVERSE, 0, 0, 0);
						VL_vmenuitem(&new_york, "Brooklyn", 502, NULL);
						VL_vmenuitem(&new_york, "Bronx", 505, NULL);
						VL_vmenuitem(&new_york, "Manhattan", 501, &manhattan);
							VL_vmenuinit(&manhattan, 0, REVERSE, 0, 0, 0);
							VL_vmenuitem(&manhattan, "Upper West Side", 1501, NULL);
							VL_vmenuitem(&manhattan, "Upper East Side", 1502, NULL);
							VL_vmenuitem(&manhattan, "Lower East Side", 1503, NULL);
							VL_vmenuitem(&manhattan, "Greenwich Village", 1504, &greenwich);
								VL_vmenuinit(&greenwich, 0, REVERSE, 0, 0, 0);
								VL_vmenuitem(&greenwich, "Bars & Pubs", 1601, NULL);
								VL_vmenuitem(&greenwich, "Restaurants", 1602, NULL);
								VL_vmenuitem(&greenwich, "Theaters", 1603, NULL);
								VL_vmenuitem(&greenwich, "Hotels", 1604, NULL);
								VL_vmenuitem(&greenwich, "Libraries", 1605, NULL);
								VL_vmenuitem(&greenwich, "Museums", 1609, NULL);
							VL_vmenuitem(&manhattan, "East Village", 1505, NULL);
							VL_vmenuitem(&manhattan, "Harlem", 1506, NULL);
							VL_vmenuitem(&manhattan, "Financial District", 1507, NULL);
							VL_vmenuitem(&manhattan, "Central Park", 1508, NULL);
						VL_vmenuitem(&new_york, "Queens", 504, NULL);
						VL_vmenuitem(&new_york, "Staten Island", 503, NULL);
					VL_vmenuitem(&stock, "", 0, NULL);
					VL_vmenuitem(&stock, "London", 406, NULL);
					VL_vmenuitem(&stock, "Toronto", 407, NULL);
				VL_vmenuitem(&progsel, "Payroll", 106, NULL);
		VL_vmenuitem(&bmtest, "Preferences", 0, &preferences);
			VL_vmenuinit(&preferences, 0, REVERSE, 0, 0, 0);
			VL_vmenuitem(&preferences, "Screen Background", 30, &yesno);
			VL_vmenuitem(&preferences, "Pseudo Blanks", 31, &yesno);
			VL_vmenuitem(&preferences, "Number of lines", 32, &yesno);
			VL_vmenuitem(&preferences, "Number of columns", 33, &yesno);
				VL_vmenuinit(&yesno, 0, REVERSE, 0, 0, 0);
				VL_vmenuitem(&yesno, "Yes", 1001, NULL);
				VL_vmenuitem(&yesno, "No", 1000, NULL);
		VL_vmenuitem(&bmtest, "Cancel", CANCEL_CODE, NULL);

	while ((i = VL_vmenugo(&bmtest)) != CANCEL_CODE)
	{
		vmove(23,0);
		vprint("The menu returned a value of %d",i); verase(TO_EOL);
	}
}



static int vmt4()			/* Test dynamic linking */
{
	struct video_menu autobar, dynamic;
	int4 choice;

	VL_vmenuinit(&autobar, 0, REVERSE, 0, 0, 0);
	VL_vmenuitem(&autobar,"Dynamic 1", 1, DYNAMIC_LINK);
	VL_vmenuitem(&autobar,"Direct 2", 2, NULL);
	VL_vmenuitem(&autobar,"Dynamic 3", 3, DYNAMIC_LINK);
	VL_vmenuitem(&autobar,"Direct 4", 4, NULL);

	VL_vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);

go:	choice = VL_vmenugo(&autobar);
go1:	switch (choice)
	{
		case 1:
		{
			VL_vdynaunlink(&dynamic);
			VL_vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);
			VL_vmenuitem(&dynamic, "Direct choice 1111", 1111, NULL);
			VL_vmenuitem(&dynamic, "Abort -1", -1, NULL);
			VL_vdynalink(&autobar, &dynamic);
			choice = VL_vmenucont(&autobar);
			goto go1;
		}
		case 3:
		{
			VL_vdynaunlink(&dynamic);
			VL_vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);
			VL_vmenuitem(&dynamic, "Direct choice 3333", 3333, NULL);
			VL_vmenuitem(&dynamic, "Direct choice 3332", 3332, NULL);
			VL_vmenuitem(&dynamic, "Direct choice 3331", 3331, NULL);
			VL_vdynalink(&autobar, &dynamic);
			choice = VL_vmenucont(&autobar);
			goto go1;
		}
		default:
		{
			vmove(21,0); verase(TO_EOL); vprint("The result returned was %d.",choice);
			vmove(22,0); verase(TO_EOL);
			if (choice == -1) vprint("This is the abort code.");
			else vprint("This was an acceptable direct choice...");
			vmove(23,0); verase(TO_EOL); vprint("Depress any key to continue...");
			choice = vgetm();
			verase_menu(ALL_MENUS, &autobar);
			vmove(21,0); verase(TO_EOL);
			vmove(22,0); verase(TO_EOL);
			vmove(23,0); verase(TO_EOL);
			if (choice != fn16_key) goto go;
			return(SUCCESS);
		}
	}
}
