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
#include <string.h>
#include <video.h>
#include <vmodules.h>

static char l1[80],l2[80],l3[80],l4[80],l5[80],l6[80];
static void draw_form();
static void set_input();
static void draw_input();
static void reset_input();

int testr()
{
	enum e_vop sav_op;

	sav_op = voptimize(VOP_DEFER_MODE);

	vstate(0);
	verase(FULL_SCREEN);
	
	draw_form();
	set_input();
	draw_input();
	vmove(21,0);
	vprint("Next action is to rewrite form with new data but no erase.\n");
	vprint("You should only see the data fields change.\n");
	vprint("Depress any key to continue...");
	vgetc();

	reset_input();
	draw_form();
	draw_input();
	vmove(21,0);
	vprint("Next action is to erase the screen and rewrite form new data\n");
	vprint("You should only see the data fields change.\n");
	vprint("Depress any key to continue...");
	vgetc();

	verase(FULL_SCREEN);
	set_input();
	draw_form();
	draw_input();

        vmove(23,0);
	vprint("Depress return to continue...");
	vgetc();

	voptimize(sav_op);
	return(SUCCESS);
}

static void draw_form()
{
	vmove(0,30);
	vmode(REVERSE);
	vprint("Sample form layout");

	vmode(CLEAR);
	vmove(5,5);
	vprint("Input field #1");	/* next col is 19 */

	vmove(10,5);
	vprint("A different input field");	/* next col is 28 */

	vmove(15,5);
	vprint("Line number 3");	/* next is 18 */

	vmove(5,40);
	vprint("Next collumn");		/* next is 52 */

	vmove(10,40);
	vprint("Yet another field");	/* next is 57 */

	vmove(15,40);
	vprint("last one");		/* next is 48 */

}


static void draw_input()
{

	vmode(UNDERSCORE);

	vmove(5,20);
	vprint("%s",l1);

	vmove(10,29);
	vprint("%s",l2);

	vmove(15,19);
	vprint("%s",l3);

	vmove(5,53);
	vprint("%s",l4);

	vmove(10,58);
	vprint("%s",l5);

	vmove(15,49);
	vprint("%s",l6);

	vmode(CLEAR);

}


static void set_input()
{
	strcpy(l1,"123456.00");
	strcpy(l2,"freddy.dat");
	strcpy(l3,"-12.879");
	strcpy(l4,"Bogus Company");
	strcpy(l5,"84678754455");
	strcpy(l6,"The End");
}

static void reset_input()
{
	strcpy(l1,"         ");
	strcpy(l2,"noname.dat");
	strcpy(l3,"       ");
	strcpy(l4,"Bogus Formula");
	strcpy(l5,"           ");
	strcpy(l6,"Takeoff");
}
 
