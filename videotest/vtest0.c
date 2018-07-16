/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			 Copyright (c) 1987-1991			*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <v/video.h>

main()
{
	extern char vgetc();
	char t;
	register int i;

	vstate(0);
	vscreen(NARROW|DARK);

start:	vbuffering(LOGICAL);
	vmode(0);
	vmove(0,0);
	vline(FAT_HORIZONTAL,78);
	vmove(0,78);
	vline(FAT_VERTICAL,23);
	vmove(23,79);
	vline(FAT_HORIZONTAL,-79);
	vmove(23,0);
	vline(FAT_VERTICAL,-23);
	vmove(4,0);
	vline(FAT_HORIZONTAL,78);
	vtext(BOLD,2,30,"VIDEO TEST MAIN MENU");

	i = 6;
	vtext(0,i++,6,"A - Accessory test (goodies).        B - ");
	vtext(0,i++,6,"C - Clock simulation.                D - Drawing tests.");
	vtext(0,i++,6,"E - Evaluate output performance.     F - ");
	vtext(0,i++,6,"G - Graphics and renditions.         H - ");
	vtext(0,i++,6,"I - Input tests (basic level).       J - Input echo tests.");
	vtext(0,i++,6,"K -                                  L - Line and grid test.");
	vtext(0,i++,6,"M - Multi-layer overlay test.        N - ");
	vtext(0,i++,6,"O - Output test (basic vprint).      P - Parameter passing.");
	vtext(0,i++,6,"Q -                                  R - Refresh test.");
	vtext(0,i++,6,"S - Scrolling tests.                 T - Tabs in output stream.");
        vtext(0,i++,6,"U -                                  V - ");
	vtext(0,i++,6,"W - Windowing tests.                 X - Terminal ID response.");
	vtext(0,i++,6,"Y -                                  ^B, ^E & ^G macro control.");
	vtext(BOLD,21,20,"Please select? ");
	vmode(0);
	vbuffering(AUTOMATIC);
inp:	t = vgetc();
	if ((t >= 'a') && (t <= 'z')) t = t - 'a' + 'A';

	if (t == '\002')
	{
		vmacro(START_SAVE);
		goto inp;
	}
	if (t == '\005')
	{
		vmacro(END_SAVE);
		goto inp;
	}
	if (t == '\007')
	{
		vmacro(START_RESTORE);
		goto inp;
	}
	if (!((t == '0') || ((t >= 'A') && (t <= 'Z'))))
	{
		vbell();
		goto inp;
	}
	vprint("%c",t); vslew(0,-1);
	vrelease();
	switch(t)
	{
		case '0': {vrelease(); verase(FULL_SCREEN); vmove(0,0); vexit(); exit(0); break;}
		case 'A': {testa(); break;}
		case 'B': {testb(); break;}
		case 'C': {testc(); break;}
		case 'D': {testd(); break;}
		case 'E': {teste(); break;}
		case 'F': {testf(); break;}
		case 'G': {testg(); break;}
		case 'H': {testh(); break;}
		case 'I': {testi(); break;}
		case 'J': {testj(); break;}
		case 'K': {testk(); break;}
		case 'L': {testl(); break;}
		case 'M': {testm(); break;}
		case 'N': {testn(); break;}
		case 'O': {testo(); break;}
		case 'P': {testp(); break;}
		case 'Q': {testq(); break;}
		case 'R': {testr(); break;}
		case 'S': {tests(); break;}
		case 'T': {testt(); break;}
		case 'U': {testu(); break;}
		case 'V': {testv(); break;}
		case 'W': {testw(); break;}
		case 'X': {testx(); break;}
		case 'Y': {testy(); break;}
		case 'Z': {testz(); break;}
		default:
		{
			testa();
			line();
			vrelease();
			testb();
			line();
			vrelease();
			testc();
			line();
			vrelease();
			testd();
			line();
			vrelease();
			teste();
			line();
			vrelease();
			testf();
			line();
			vrelease();
			testg();
			line();
			vrelease();
			testh();
			line();
			vrelease();
			testi();
			line();
			vrelease();
			testj();
			line();
			vrelease();
			testk();
			line();
			vrelease();
			testl();
			line();
			vrelease();
			testm();
			line();
			vrelease();
			testn();
			line();
			vrelease();
			testo();
			line();
			vrelease();
			testp();
			line();
			vrelease();
			testq();
			line();
			vrelease();
			testr();
			line();
			vrelease();
			tests();
			line();
			vrelease();
			testt();
			line();
			vrelease();
			testu();
			line();
			vrelease();
			testv();
			line();
			vrelease();
			testw();
			line();
			vrelease();
			testx();
			line();
			vrelease();
			testy();
			line();
			vrelease();
			testz();
		}
	}
	vrelease();
	verase(FULL_SCREEN);
	goto start;
}
line()
{
vprint("\n-------------------------------------------------------------\n");
}
