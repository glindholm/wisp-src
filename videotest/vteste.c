/************************************************************************/
/*									*/
/*	     VIDEO - Video Interactive Development Environment		*/
/*									*/
/*			    Copyright (c) 1987				*/
/*									*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <video.h>
#include <vmodules.h>
#define LOOPS 5000

static void testex(enum e_vop vop, int scroll);


int teste()
{
	char c;
	char *ctime();
	long start_time;
	long end_time;
	long time();

	verase(FULL_SCREEN);
	vmove(0,0);
	vprint("Test group E - Output timing speeds - loops %d times.\n",LOOPS);
start:	VL_vbuffering_start();
	vprint("\nChoose: 0 - To exit\n");
vprint("        1 -    scrolling, no tracking, no optimization, no defer\n");
vprint("        2 -    scrolling,    tracking, no optimization, no defer\n");
vprint("        3 -    scrolling,    tracking,    optimization, no defer\n");
vprint("        4 -    scrolling,    tracking,    optimization,    defer\n");
vprint("        5 - no scrolling, no tracking, no optimization, no defer\n");
vprint("        6 - no scrolling,    tracking, no optimization, no defer\n");
vprint("        7 - no scrolling,    tracking,    optimization, no defer\n");
vprint("        8 - no scrolling,    tracking,    optimization,    defer\n");
	while (vcheck() != 0);
	vprint("\nSelect? ");
	c = vgetc(); vprint("%c\n",c);
	while (vcheck() != 0);
	verase(FULL_SCREEN);
	VL_vbuffering_end();
	start_time = time(NULL);
	switch(c)
	{
	case '0': 
		return(SUCCESS);

	case '1': 
		testex(VOP_OFF, 1);
		break;

	case '2': 
		testex(VOP_TRACKING_ONLY, 1);
		break;

	case '3': 
		testex(VOP_DATA_AND_CONTROLS, 1);
		break;

	case '4': 
		testex(VOP_DEFER_MODE, 1);
		break;

	case '5': 
		testex(VOP_OFF, 0);
		break;

	case '6': 
		testex(VOP_TRACKING_ONLY, 0);
		break;

	case '7': 
		testex(VOP_DATA_AND_CONTROLS, 0);
		break;

	case '8': 
		testex(VOP_DEFER_MODE, 0);
		break;

	default:
		vbell();
		vprint("Invalid selection, please try again.\n");
		break;
		
	}
	vmove(23,0);
	end_time = time(NULL);
	vprint("\nOption %c : Timer = %ld seconds.\n", c, (long)(end_time - start_time));

	goto start;
}

static void testex(enum e_vop vop, int scroll)
{
	int i;

	vop = voptimize(vop);

	for (i = 0; i < LOOPS; i++)
	{
		if (!scroll && i%20 == 0)
		{
			vmove(0,0);
		}
		
		vprint("This is a test. This is a test. This is a test. This is a test. %d\n",i);

	}
	voptimize(vop);
}

