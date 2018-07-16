
/************************************************************************/
/*	     VIDEO - Video Interactive Development Environment		*/
/*			  Copyright (c) 1987-1991			*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <video.h>
#include <vmodules.h>

int testg()
{
	register int i,j, k,col;
										/* On 2nd pass, show all renditions only.	*/

	enum e_vop old_op;
	

	old_op = voptimize(VOP_TRACKING_ONLY);
	
	for (j = 0; j < 3; j++)
	{
		verase(FULL_SCREEN);
		i = 0;
		if(2==j)vtext(0,i++,0,"Examples of all possible rendition combinations:");
		if(2==j)i++;
		vtext(0,i++,0,"0: This is plain text.");
		vtext(VMODE_BOLD,i++,0,"1: This is bold text.");
		vtext(VMODE_UNDERSCORE,i++,0,"2: This text is underlined.");
		vtext(VMODE_UNDERSCORE|VMODE_BOLD,i++,0,"3: This text is underlined and bold.");
		if(2==j)i++;
		vtext(VMODE_BLINK,i++,0,"4: This text is blinking.");
		if(2==j)vtext(VMODE_BLINK|VMODE_BOLD,i++,0,"5: This text is blinking and bold.");
		if(2==j)vtext(VMODE_BLINK|VMODE_UNDERSCORE,i++,0,"6: This text is blinking and underlined.");
		if(2==j)vtext(VMODE_BLINK|VMODE_UNDERSCORE|VMODE_BOLD,i++,0,"7: This text is blinking, underlined and bold.");
		if(2==j)i++;
		vtext(VMODE_REVERSE,i++,0,"8: This text is in reverse video.");
		vtext(VMODE_REVERSE|VMODE_BOLD,i++,0,"9: This text is in reverse video and bold.");
		vtext(VMODE_REVERSE|VMODE_UNDERSCORE,i++,0,"A: This text is in reverse video and underlined.");
		vtext(VMODE_REVERSE|VMODE_UNDERSCORE|VMODE_BOLD,i++,0,"B: This text is in reverse video, underlined and bold.");
		if(2==j)i++;
		if(2==j)vtext(VMODE_REVERSE|VMODE_BLINK,i++,0,"C: This text is in reverse video and blinking.");
		if(2==j)vtext(VMODE_REVERSE|VMODE_BLINK|VMODE_BOLD,i++,0,"D: This text is in reverse video, blinking and bold.");
		if(2==j)vtext(VMODE_REVERSE|VMODE_UNDERSCORE|VMODE_BLINK,i++,0,"E: This text is in reverse video, underlined and blinking.");
		vmove(i++,0); vmode(VMODE_BOLD|VMODE_UNDERSCORE|VMODE_BLINK|VMODE_REVERSE);
		vprint("F: This text is all renditions together (bold, blink, underline and reverse).");
		if(2!=j)
		{
			i++;
			vtext(0,i,0,"(Hex     0,4,8,C         1,5,9,D         2,6,A,E         3,7,B,F         ");
			i++;
			vtext(0,i,0," scale)  0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF");
			i++;
			vtext(0,i,0,"         |       |       |       |       |       |       |       |       ");
	
			i++;
			vmove(i,0);
			vcharset(DEFAULT);
			vprint("DEFAULT  ");
			for (k = 0; k < 040; k++) vputc(' ');
			for (k = 040; k <= 077; k++) vputc((char)k);
			i++;
			col=9;
			vtext(0,i,0," x40");
			for (k = 0100; k < 0177; k++,col++) 
			{
				vmove(i,col);
				vputc((char)k);
			}
			
			i++;
			col=9;
			vtext(0,i,0," x80");
			for (k = 0200; k <= 0277; k++,col++)
			{
				vmove(i,col);
				
				if (0==memcmp(VL_vcapterm(),"vt",2))
				{
					if (k <= 0240) vputc(' ');
					else vputc((char)k);
				}
				else
				{
					vputc((char)k);
				}
			}
			i++;
			col=9;
			vtext(0,i,0," xC0");
			for (k = 0300; k <= 0377; k++,col++) 
			{
				vmove(i,col);
				vputc((char)k);
			}
			
			i++;
			vtext(GRAPHICS,i,0,"GRAPHICS ");
			vmove(i,9); 
			vcharset(GRAPHICS);
			for (k = 0; k < 040; k++) vputc(' ');
			for (k = 040; k <= 077; k++) 
			{
				vputc((char)k);
			}
			i++;
			col=9;
			vtext(0,i,0," x40");
			vcharset(GRAPHICS);
			for (k = 0100; k < 0177; k++,col++) 
			{
				vmove(i,col);
				vputc((char)k);
			}

			
			i++;
			col=9;
			vtext(0,i,0," x80");
			vcharset(GRAPHICS);
			for (k = 0200; k <= 0277; k++,col++)
			{
				vmove(i,col);
				if (0==memcmp(VL_vcapterm(),"vt",2))
				{
					if (k <= 0240) vputc(' ');
					else vputc((char)k);
				}
				else
				{
					vputc((char)k);
				}
			}
			i++;
			col=9;
			vtext(0,i,0," xC0");
			vcharset(GRAPHICS);
			for (k = 0300; k <= 0377; k++,col++) 
			{
				vmove(i,col);
				vputc((char)k);
			}
			
			VL_vcontrol_flush();
		}
		vtext(0,23,0,"Page %d done. ",j+1);
		vprint("Depress any key to continue..."); vgetc();
	}
	vrelease();
	voptimize(old_op);
	return(SUCCESS);
}
