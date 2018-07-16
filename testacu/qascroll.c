#include <v/video.h>

QASCROLL()
{
	register int i;
	int c;
	char	buff[80];
	int ln;

	init_screen();
	verase(FULL_SCREEN);
	vset(SCROLL,SMOOTH);
	vmove(0,25);
	vprint("SCROLL REGION TEST EXAMPLE");
	vroll(3,20);
	vmove(3,0);
	ln=1;
	for (i = 1; i < 30; i++)
	{
		sprintf(buff,"This should be nicely scrolling forward.(%d)\n",ln++);
		vprint(buff);
	}
	vmove(23,0);
	vprint("Hit a key to scroll in reverse.");
	c = vgetc();
	vmove(3,0);
	for (i = 1; i < 30; i++)
	{
		vlinefeed(REVERSE);
		sprintf(buff,"This should be nicely scrolling backward.(%d)\n",ln++);
		vprint(buff);
		vmove(3,0);
	}
	vmove(23,0);
	vprint("Hit a key to scroll forward again.");
	c = vgetc();
	vmove(20,0);
	for (i = 1; i < 30; i++)
	{
		sprintf(buff,"This should be nicely scrolling forward.(%d)\n",ln++);
		vprint(buff);
	}
	vset(SCROLL,JUMP);
	vmove(23,0);
	verase(CURRENT_LINE);
	vprint("Hit a key to return to menu.");
	c = vgetc();
	vexit();
}
