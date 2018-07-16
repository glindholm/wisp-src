/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1987				*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>
#include <v/vintdef.h>
#include <v/vmenu.h>

#define CANCEL_CODE -1
#define CLOCK_CODE 13
#define NOTE_CODE 11
#define CALC_CODE 12
#define CALEND_CODE 14
#define PUZZLE_CODE 15

testm()
{
	register int i;

	verase(FULL_SCREEN);
	vmenumode(STATIC_MENU);
	for (i = 0; i < 24; i++) vtext(0,i,0,"  MENU TEST \t MENU TEST \t MENU TEST \t MENU TEST");
	vmtest();
	verase(FULL_SCREEN);
	vmenumode(STATIC_MENU);
	for (i = 0; i < 24; i++) vtext(0,i,0,"                         MACRO RESTORE IS ON");
	vmtest();

	verase(FULL_SCREEN);
	vscreen(LIGHT);
	vmenu_pfkeys(ON);
	vtext(CLEAR,10,34," Function key processing is on.");
	vmtest();
	vmenu_pfkeys(OFF);
	vscreen(DARK);

	verase(FULL_SCREEN);
	vmenumode(DYNAMIC_MENU);
	vmt4();

	return(SUCCESS);
}

vmtest()
{
	register int i,j;
	struct video_menu bmtest, manhattan, new_york, greenwich;
	struct video_menu goodies, preferences, run;
	struct video_menu progsel, stock, yesno;

	vmenuinit(&bmtest, BAR_MENU, REVERSE, 0, 2, 4);
		vmenuitem(&bmtest, "Goodies", 0, &goodies);
			vmenuinit(&goodies, 0, REVERSE, 0, 0, 0);
			vmenuitem(&goodies, "Clock", CLOCK_CODE, NULL);
			vmenuitem(&goodies, "Calculator", CALC_CODE, NULL);
			vmenuitem(&goodies, "Calendar", CALEND_CODE, NULL);
			vmenuitem(&goodies, "Notepad", NOTE_CODE, NULL);
			vmenuitem(&goodies, "Puzzle", PUZZLE_CODE, NULL);
		vmenuitem(&bmtest, "Help", 0, NULL);
		vmenuitem(&bmtest, "Run", 0, &run);
			vmenuinit(&run, 0, REVERSE, 0, 0, 0);
			vmenuitem(&run, "Submit Procedure", 21, NULL);
			vmenuitem(&run, "Detach Program", 22, NULL);
			vmenuitem(&run, "Run Program", 23, NULL);
			vmenuitem(&run, "Run Application", 24, &progsel);
				vmenuinit(&progsel, 0, REVERSE, 0, 0, 0);
				vmenuitem(&progsel, "Accounts Payable", 101, NULL);
				vmenuitem(&progsel, "Accounts Receivable", 102, NULL);
				vmenuitem(&progsel, "General Ledger", 103, NULL);
				vmenuitem(&progsel, "Invoice Tracking", 104, NULL);
				vmenuitem(&progsel, "Inventory Control", 105, &stock);
					vmenuinit(&stock, 0, REVERSE, 0, 0, 0);
					vmenuitem(&stock, "Atlanta", 401, NULL);
					vmenuitem(&stock, "Chicago", 402, NULL);
					vmenuitem(&stock, "Dallas", 403, NULL);
					vmenuitem(&stock, "Los Angeles", 404, NULL);
					vmenuitem(&stock, "New York", 405, &new_york);
						vmenuinit(&new_york, 0, REVERSE, 0, 0, 0);
						vmenuitem(&new_york, "Brooklyn", 502, NULL);
						vmenuitem(&new_york, "Bronx", 505, NULL);
						vmenuitem(&new_york, "Manhattan", 501, &manhattan);
							vmenuinit(&manhattan, 0, REVERSE, 0, 0, 0);
							vmenuitem(&manhattan, "Upper West Side", 1501, NULL);
							vmenuitem(&manhattan, "Upper East Side", 1502, NULL);
							vmenuitem(&manhattan, "Lower East Side", 1503, NULL);
							vmenuitem(&manhattan, "Greenwich Village", 1504, &greenwich);
								vmenuinit(&greenwich, 0, REVERSE, 0, 0, 0);
								vmenuitem(&greenwich, "Bars & Pubs", 1601, NULL);
								vmenuitem(&greenwich, "Restaurants", 1602, NULL);
								vmenuitem(&greenwich, "Theaters", 1603, NULL);
								vmenuitem(&greenwich, "Hotels", 1604, NULL);
								vmenuitem(&greenwich, "Libraries", 1605, NULL);
								vmenuitem(&greenwich, "Museums", 1609, NULL);
							vmenuitem(&manhattan, "East Village", 1505, NULL);
							vmenuitem(&manhattan, "Harlem", 1506, NULL);
							vmenuitem(&manhattan, "Financial District", 1507, NULL);
							vmenuitem(&manhattan, "Central Park", 1508, NULL);
						vmenuitem(&new_york, "Queens", 504, NULL);
						vmenuitem(&new_york, "Staten Island", 503, NULL);
					vmenuitem(&stock, "", 0, NULL);
					vmenuitem(&stock, "London", 406, NULL);
					vmenuitem(&stock, "Toronto", 407, NULL);
				vmenuitem(&progsel, "Payroll", 106, NULL);
		vmenuitem(&bmtest, "Preferences", 0, &preferences);
			vmenuinit(&preferences, 0, REVERSE, 0, 0, 0);
			vmenuitem(&preferences, "Screen Background", 30, &yesno);
			vmenuitem(&preferences, "Pseudo Blanks", 31, &yesno);
			vmenuitem(&preferences, "Number of lines", 32, &yesno);
			vmenuitem(&preferences, "Number of columns", 33, &yesno);
				vmenuinit(&yesno, 0, REVERSE, 0, 0, 0);
				vmenuitem(&yesno, "Yes", 1001, NULL);
				vmenuitem(&yesno, "No", 1000, NULL);
		vmenuitem(&bmtest, "Cancel", CANCEL_CODE, NULL);

	while ((i = vmenugo(&bmtest)) != CANCEL_CODE)
	{
		vmove(23,0);
		vprint("The menu returned a value of %d",i); verase(TO_EOL);
	}
}

vmt2()
{
	struct video_menu auto1, auto2, auto3, auto4, auto5, auto6, auto7, auto8, autobar;
	int choice;

	vmenuinit(&auto1, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	auto1.item = 4;
	vmenuitem(&auto1,"This is the first selection.", 1, 0);
	vmenuitem(&auto1,"This is the second selection.", 2, 0);
	vmenuitem(&auto1,"This is the third selection.", 3, 0);
	vmenuitem(&auto1,"This is the fourth selection.", 4, 0);
	vmenuitem(&auto1,"This item will link.",5, &auto2);
	vmenuitem(&auto1,"This is the last selection.", 6, 0);

	vmenuinit(&auto2, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	vmenuitem(&auto2,"This is the first selection.", 11, 0);
	vmenuitem(&auto2,"This is the second selection.",12, 0);
	vmenuitem(&auto2,"This is the third selection.", 13, 0);
	vmenuitem(&auto2,"This is the fourth selection.", 14, 0);
	vmenuitem(&auto2,"This item will link.", 15, &auto3);
	vmenuitem(&auto2,"This is the last selection.", 16, 0);

	vmenuinit(&auto3, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	vmenuitem(&auto3,"This is the first selection.", 21, 0);
	vmenuitem(&auto3,"This is the second selection.", 22, 0);
	vmenuitem(&auto3,"This is the third selection.", 23, 0);
	vmenuitem(&auto3,"This is the fourth selection.", 24, 0);
	vmenuitem(&auto3,"This item will link.", 25, &auto4);
	vmenuitem(&auto3,"This is the last selection.", 26, 0);

	vmenuinit(&auto4, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	vmenuitem(&auto4,"This is the first selection.", 31, 0);
	vmenuitem(&auto4,"This is the second selection.", 32, 0);
	vmenuitem(&auto4,"This is the third selection.", 33, 0);
	vmenuitem(&auto4,"This is the fourth selection.", 34, 0);
	vmenuitem(&auto4,"This item will link.", 35, &auto5);
	vmenuitem(&auto4,"This is the last selection.", 36, 0);

	vmenuinit(&auto5, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	vmenuitem(&auto5,"This is the first selection.", 41, 0);
	vmenuitem(&auto5,"This is the second selection.", 42, 0);
	vmenuitem(&auto5,"This is the third selection.", 43, 0);
	vmenuitem(&auto5,"This is the fourth selection.", 44, 0);
	vmenuitem(&auto5,"This item will link.", 45, &auto6);
	vmenuitem(&auto5,"This is the last selection.", 46, 0);

	vmenuinit(&auto6, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	vmenuitem(&auto6,"This is the first selection.", 51, 0);
	vmenuitem(&auto6,"This is the second selection.", 52, 0);
	vmenuitem(&auto6,"This is the third selection.", 53, 0);
	vmenuitem(&auto6,"This is the fourth selection.", 54, 0);
	vmenuitem(&auto6,"This item will link.", 55, &auto7);
	vmenuitem(&auto6,"This is the last selection.", 56, 0);

	vmenuinit(&auto7, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	vmenuitem(&auto7,"This is the first selection.", 61, 0);
	vmenuitem(&auto7,"This is the second selection.", 62, 0);
	vmenuitem(&auto7,"This is the third selection.", 63, 0);
	vmenuitem(&auto7,"This is the fourth selection.", 64, 0);
	vmenuitem(&auto7,"This item will link.", 65, &auto8);
	vmenuitem(&auto7,"This is the last selection.", 66, 0);

	vmenuinit(&auto8, 0, 0, 0, 0, 0);						/* Init totally automatic.		*/
	vmenuitem(&auto8,"This is the first selection.", 71, 0);
	vmenuitem(&auto8,"This is the second selection.", 72, 0);
	vmenuitem(&auto8,"This is the third selection.", 73, 0);
	vmenuitem(&auto8,"This is the fourth selection.", 74, 0);
	vmenuitem(&auto8,"This item will link.", 75, 0);
	vmenuitem(&auto8,"This is the last selection.", 76, 0);

	choice = vmenugo(&auto1);
	vmove(21,0); verase(TO_EOL); vprint("The result returned was %d.",choice);
	vmove(22,0); verase(TO_EOL); vprint("Now about to call the same menu stream in continuation mode.");
	vmove(23,0); verase(TO_EOL); vprint("Depress any key to continue...");
	vgetm();
	vmove(21,0); verase(TO_EOL);
	vmove(22,0); verase(TO_EOL);
	vmove(23,0); verase(TO_EOL);
	vmenusave(&auto1);
	choice = vmenucont(&auto1);
	vmove(21,0); verase(TO_EOL); vprint("The result returned was %d.",choice);
	vmove(22,0); verase(TO_EOL); vprint("Now about to erase all the menus and do a bar menu.");
	vmove(23,0); verase(TO_EOL); vprint("Depress any key to continue...");
	vgetm();
	vmove(21,0); verase(TO_EOL);
	vmove(22,0); verase(TO_EOL);
	vmove(23,0); verase(TO_EOL);
	verase_menu(ALL_MENUS,&auto1);							/* Now clean up after ourselves.	*/
	verase(FULL_SCREEN);								/* Erase the screen.			*/
	vmenurestore();
	choice = vmenugo(&auto1);
	vmove(21,0); verase(TO_EOL); vprint("The result returned was %d.",choice);
	vmove(22,0); verase(TO_EOL); vprint("This menu was put up using vmenurestore()");
	vmove(23,0); verase(TO_EOL); vprint("Depress any key to continue...");
	vgetm();
	verase_menu(ALL_MENUS,&auto1);
}

vmt3()			/* Test dynamic linking */
{
	struct video_menu autobar, static2, static12, dynamic;
	int choice;

	vmenuinit(&autobar, BAR_MENU, REVERSE, 0, 3, 3);
	vmenuitem(&autobar,"Direct 1", 1, NULL);
	vmenuitem(&autobar,"Static 2", 2, &static2);
		vmenuinit(&static2, 0, REVERSE, 0, 0, 0);
		vmenuitem(&static2,"Direct 11", 11, NULL);
		vmenuitem(&static2,"Static 12", 12, &static12);
			vmenuinit(&static12, 0, REVERSE, 0, 0, 0);
			vmenuitem(&static12,"Direct 111", 111, NULL);
			vmenuitem(&static12,"Abort -1", -1, NULL);
		vmenuitem(&static2,"Dynamic 13", 13, DYNAMIC_LINK);
		vmenuitem(&static2,"Abort -1", -1, NULL);
	vmenuitem(&autobar,"Dynamic 3", 3, DYNAMIC_LINK);
	vmenuitem(&autobar,"Abort -1", -1, NULL);

	vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);
	vmenuitem(&dynamic, "Direct choice 1111", 1111, NULL);
	vmenuitem(&dynamic, "Abort -1", -1, NULL);

go:	choice = vmenugo(&autobar);
go1:	switch (choice)
	{
		case 3:
		case 13:
		{
			vdynaunlink(&dynamic);
			vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);
			vmenuitem(&dynamic, "Direct choice 1111", 1111, NULL);
			vmenuitem(&dynamic, "Abort -1", -1, NULL);
			vdynalink(&autobar, &dynamic);
			choice = vmenucont(&autobar);
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

vmt4()			/* Test dynamic linking */
{
	struct video_menu autobar, static2, static12, dynamic;
	int choice;

	vmenuinit(&autobar, 0, REVERSE, 0, 0, 0);
	vmenuitem(&autobar,"Dynamic 1", 1, DYNAMIC_LINK);
	vmenuitem(&autobar,"Direct 2", 2, NULL);
	vmenuitem(&autobar,"Dynamic 3", 3, DYNAMIC_LINK);
	vmenuitem(&autobar,"Direct 4", 4, NULL);

	vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);

go:	choice = vmenugo(&autobar);
go1:	switch (choice)
	{
		case 1:
		{
			vdynaunlink(&dynamic);
			vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);
			vmenuitem(&dynamic, "Direct choice 1111", 1111, NULL);
			vmenuitem(&dynamic, "Abort -1", -1, NULL);
			vdynalink(&autobar, &dynamic);
			choice = vmenucont(&autobar);
			goto go1;
		}
		case 3:
		{
			vdynaunlink(&dynamic);
			vmenuinit(&dynamic, 0, REVERSE, 0, 0, 0);
			vmenuitem(&dynamic, "Direct choice 3333", 3333, NULL);
			vmenuitem(&dynamic, "Direct choice 3332", 3332, NULL);
			vmenuitem(&dynamic, "Direct choice 3331", 3331, NULL);
			vdynalink(&autobar, &dynamic);
			choice = vmenucont(&autobar);
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
