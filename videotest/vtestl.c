/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			 Copyright (c) 1987-1991			*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <video.h>
#include <vmodules.h>

testl()
{
	extern int vlin_op;
	int i;

	vstate(0);
	vscreen(0);

	vmove(0,0);
	vprint("Next test - A vertical line from (10,10) of length down 10      \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(10,10);
	vline(VERTICAL,10);

	vmove(0,0);
	vprint("Next test - A vertical line from (0,70) of length down 24      \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(0,70);
	vline(VERTICAL,24);

	vmove(0,0);
	vprint("Next test - A vertical line from (20,40) of length up 10     \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(20,40);
	vline(VERTICAL,-10);

	vmove(0,0);
	vprint("Next test - A vertical line from (23,75) of length up 24     \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(23,75);
	vline(VERTICAL,-24);

	vlin_op = OFF;
	vmove(0,0);
	vprint("Next test - No optimization from (20,45) of length up 10     \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(20,45);
	vline(VERTICAL,-10);

	vmove(0,0);
	vprint("Next test - No optimization from (23,79) of length up 24     \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(23,79);
	vline(VERTICAL,-24);

	vmove(0,0);
	vprint("Next - A set of some horizontal lines.                       \n");
	vprint("Depress any key to do it."); vgetc();
	vlin_op = ON;

	verase(FULL_SCREEN);
	vmove(0,0);
	vprint("Next test - A horizontal line from (23,0) of length right 79.   \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(23,0);
	vline(HORIZONTAL,79);

	vlin_op = OFF;
	vmove(0,0);
	vprint("Next test - No optimization from (15,40) of length left 30.      \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(15,40);
	vline(HORIZONTAL,-30);

	vmove(0,0);
	vprint("Next test - No optimization from (20,79) of length left 79       \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(20,79);
	vline(HORIZONTAL,-79);

	vmove(0,0);
	vprint("Next - A set of some wide lines.                             \n");
	vprint("Depress any key to do it."); vgetc();
	vlin_op = ON;

	verase(FULL_SCREEN);

	vmove(0,0);
	vprint("Next test - A fat line from (10,10) of length down 10            \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(10,10);
	vline(FAT_VERTICAL,10);

	vmove(0,0);
	vprint("Next test - A fat line from (23,0) of length right 79.           \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(23,0);
	vline(FAT_HORIZONTAL,79);

	vlin_op = OFF;
	vmove(0,0);
	vprint("Next test - No optimization fat from (15,60) left 50.            \n");
	vprint("Depress any key to do it."); vgetc();
	vmove(15,60);
	vline(FAT_HORIZONTAL,-50);

	vmove(0,0);
	vprint("Next test - Some fun.                                            \n");
	vprint("Depress any key to do it."); vgetc();
	vbuffering_start();
	for (i = 0; i < MAX_LINES_PER_SCREEN/2; i++)
	{
		vmove(i,(i*2));
		vline(FAT_HORIZONTAL,(80-(i*4)));
		vmove((i+1),(78-(i*2)));
		vline(FAT_VERTICAL,(23-(i*2)));
		vmove(23-i,(77-(i*2)));
		vline(FAT_HORIZONTAL,((i*4)-78));
		vmove((22-i),(i*2));
		vline(FAT_VERTICAL,((i*2)-22));
	}
	vbuffering_end();
	vlin_op = ON;

	vset_cursor_off();
	vmode(BOLD);
	vmove(10,22); vprint("         All done.          ");
	vmove(11,22); vprint(" Depress any key to exit... "); vgetc();
	vmove(23,0);
	vmode(0);
	vset_cursor_on();
	return(SUCCESS);
}
