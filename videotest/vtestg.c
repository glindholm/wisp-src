
/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			  Copyright (c) 1987-1991			*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <v/video.h>

testg()
{
	register int i,j,k;
	extern int vcur_lin;

										/* On 2nd pass, show all renditions only.	*/
	for (j = 0; j < 3; j++)
	{
		verase(FULL_SCREEN);
		i = 0;
		if(2==j)vtext(0,i++,0,"Examples of all possible rendition combinations:");
		if(2==j)i++;
		vtext(0,i++,0,"0: This is plain text.");
		vtext(BOLD,i++,0,"1: This is bold text.");
		vtext(UNDERSCORE,i++,0,"2: This text is underlined.");
		vtext(UNDERSCORE|BOLD,i++,0,"3: This text is underlined and bold.");
		if(2==j)i++;
		vtext(BLINK,i++,0,"4: This text is blinking.");
		if(2==j)vtext(BLINK|BOLD,i++,0,"5: This text is blinking and bold.");
		if(2==j)vtext(BLINK|UNDERSCORE,i++,0,"6: This text is blinking and underlined.");
		if(2==j)vtext(BLINK|UNDERSCORE|BOLD,i++,0,"7: This text is blinking, underlined and bold.");
		if(2==j)i++;
		vtext(REVERSE,i++,0,"8: This text is in reverse video.");
		vtext(REVERSE|BOLD,i++,0,"9: This text is in reverse video and bold.");
		vtext(REVERSE|UNDERSCORE,i++,0,"A: This text is in reverse video and underlined.");
		vtext(REVERSE|UNDERSCORE|BOLD,i++,0,"B: This text is in reverse video, underlined and bold.");
		if(2==j)i++;
		if(2==j)vtext(REVERSE|BLINK,i++,0,"C: This text is in reverse video and blinking.");
		if(2==j)vtext(REVERSE|BLINK|BOLD,i++,0,"D: This text is in reverse video, blinking and bold.");
		if(2==j)vtext(REVERSE|UNDERSCORE|BLINK,i++,0,"E: This text is in reverse video, underlined and blinking.");
		vmove(i++,0); vmode(BOLD|UNDERSCORE|BLINK|REVERSE);
		vprint("F: This text is all renditions together (bold, blink, underline and reverse).");
		if(2!=j)
		{
			i++;
			vtext(0,i,0,"DEFAULT  ");
			vmove(i++,9); vcharset(DEFAULT);
			for (k = 0; k < 040; k++) vputc(' ');
			for (k = 040; k <= 077; k++) vputc(k);
			vmove(i++,9);
			for (k = 0100; k < 0177; k++) vputc(k);

			vtext(GRAPHICS,i,0,"GRAPHICS ");
			vmove(i++,9); vcharset(GRAPHICS);
			for (k = 0; k < 040; k++) vputc(' ');
			for (k = 040; k <= 077; k++) vputc(k);
			vmove(i++,9);
			for (k = 0100; k < 0177; k++) vputc(k);

			vmove(i++,0);
			vcharset(DEFAULT);
			vprint("EIGHTBIT ");
			for (k = 0200; k <= 0277; k++)
			{
#ifdef VMS
				if (k <= 0240) vputc(' ');
				else vputc(k);
#else
				vputc(k);
#endif
			}
			vmove(i++,9);
			for (k = 0300; k <= 0377; k++) vputc(k);

			vmove(i++,0);
			vcharset(GRAPHICS);
			vprint("GRAPH-8  ");
			for (k = 0200; k <= 0277; k++)
			{
#ifdef VMS
				if (k <= 0240) vputc(' ');
				else vputc(k);
#else
				vputc(k);
#endif
			}
			vmove(i++,9);
			for (k = 0300; k <= 0377; k++) vputc(k);

			vcontrol(DUMP_OUTPUT);
			i++;
			vtext(DOUBLE_WIDTH,i++,0,"This line is double width");
			i++;
			vtext(DOUBLE_HEIGHT,i++,0,"This line is double height");
		}
		vtext(0,23,0,"Page %d done. ",j+1);
		vprint("Depress any key to continue..."); vgetc();
	}
	vprint("\n");
	vrelease();
	return(SUCCESS);
}
