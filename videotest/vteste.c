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
#include <v/video.h>
#define LOOPS 200

teste()
{
	extern int vera_op;
	char c;
	char *ctime();
	long start_time;
	long end_time;
	long time();

	verase(FULL_SCREEN);
	vmove(0,0);
	vprint("Test group E - Output timing speeds - loops %d times.\n",LOOPS);
start:	vbuffering(LOGICAL);
	vprint("\nChoose: 0 - To exit\n");
vprint("        1 -    scrolling, no tracking, no optimization (using printf)\n");
vprint("        2 -    scrolling, no tracking, no optimization\n");
vprint("        3 -    scrolling,    tracking, no optimization (worse case)\n");
vprint("        4 - no scrolling, no tracking, no optimization\n");
vprint("        5 - no scrolling,    tracking, no optimization (worst case)\n");
vprint("        6 - no scrolling,    tracking, data level optimization\n");
vprint("        7 - no scrolling,    tracking, deferred action optimization\n");
vprint("        8 - no scrolling,    tracking, predicitive with erase\n");
	while (vcheck() != 0);
	vprint("\nSelect? ");
	c = vgetc(); vprint("%c\n",c);
	while (vcheck() != 0);
	vera_op = OFF;
	verase(FULL_SCREEN);
	vbuffering(AUTOMATIC);
	start_time = time(NULL);
	switch(c)
	{
		case '0': return(SUCCESS);
		case '1': {teste1(); break;}
		case '2': {teste2(); break;}
		case '3': {teste3(); break;}
		case '4': {teste4(); break;}
		case '5': {teste5(); break;}
		case '6': {teste6(); break;}
		case '7': {teste7(0); break;}
		case '8': {teste7(1); break;}
		default:
		{
			vbell();
			vprint("Invalid selection, please try again.\n");
		}
	}
	vmove(23,0);
	end_time = time(NULL);
	vprint("\nStart = %s\n",ctime(&start_time));
	vprint("End   = %s\n",ctime(&end_time));
	goto start;
}

teste1()
{
	register int i,j;
	int save;

	vmove(23,0);
	save = voptimize(OFF);
	for (i = 0; i < LOOPS; i++)
	{
printf("This is a test. This is a test. This is a test. This is a test.\n\r");
	}
	voptimize(save);
	return(SUCCESS);
}

teste2()
{
	register int i,j;
	int save;

	vmove(23,0);
	save = voptimize(OFF);
	for (i = 0; i < LOOPS; i++)
	{
vprint("This is a test. This is a test. This is a test. This is a test.\n");
	}
	voptimize(save);
	return(SUCCESS);
}

teste3()
{
	register int i,j;
	int save;

	vmove(23,0);
	save = voptimize(TRACKING_ONLY);
	for (i = 0; i < LOOPS; i++)
	{
vprint("This is a test. This is a test. This is a test. This is a test.\n");
	}
	voptimize(save);
	return(SUCCESS);
}

teste4()
{
	register int i,j,k;
	extern int vcur_lin,vcur_col;
	int save;

	save = voptimize(OFF);
	vmove(0,0);
	j = 0;
	for (i = 0; i < LOOPS; i++)
	{
vprint("This is a test. This is a test. This is a test. This is a test.\n");
		j = j + 1;
		if (j >= 20)
		{
			j = 0;
			vmove(0,0);
		}
	}
	voptimize(save);
	return(SUCCESS);
}

teste5()
{
	register int i,j,k;
	int save;

	save = voptimize(TRACKING_ONLY);
	vmove(0,0);
	j = 0;
	for (i = 0; i < LOOPS; i++)
	{
vprint("This is a test. This is a test. This is a test. This is a test.\n");
		j = j + 1;
		if (j >= 20)
		{
			j = 0;
			vmove(0,0);
		}
	}
	voptimize(save);
	return(SUCCESS);
}

teste6()
{
	register int i,j,k;
	int save;

	save = voptimize(DATA_ONLY);
	vmove(0,0);
	j = 0;
	for (i = 0; i < LOOPS; i++)
	{
vprint("This is a test. This is a test. This is a test. This is a test.\n");
		j = j + 1;
		if (j >= 20)
		{
			j = 0;
			vmove(0,0);
		}
	}
	voptimize(save);
	return(SUCCESS);
}

teste7(erase) int erase;
{
	register int i,j;
	int save;
	save = voptimize(BLOCK_MODE);

	vmove(0,0);
	j = 0;
	for (i = 0; i < LOOPS; i++)
	{
vprint("This is a test. This is a test. This is a test. This is a test.\n");
		j = j + 1;
		if (j >= 20)
		{
			j = 0;
			vmove(0,0);
			if (erase) verase(FULL_SCREEN);
		}
	}
	voptimize(save);
	return(SUCCESS);
}
