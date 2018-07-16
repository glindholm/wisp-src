static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*					Include standard header files.								*/

#include <ctype.h>										/* Get character type macros.	*/
#include <stdio.h>										/* Reference standard I/O.	*/
#include <stdlib.h>
#include <string.h>

#include <video.h>

/*						Entry point.									*/

main(argc,argv) int argc; char *argv[];								/* Determine which goodie.	*/
{
	if (argc != 2) 										/* Did we get a selection?	*/
	{
		printf("Usage: good <selection>\n");
		printf("Valid selections are: calc, calend, clock, notepad or puzzle.\n");
		exit(0);
	}

	vstate(DEFAULT);
	verase(FULL_SCREEN);
	vscreen(NARROW|DARK);
	vmove(0,0);

	if (strcmp(argv[1],"CLOCK") == 0) gclock();						/* Determine what to run.	*/
	else if (strcmp(argv[1],"clock") == 0) gclock();

	else if (strcmp(argv[1],"CALC") == 0) gcalc();
	else if (strcmp(argv[1],"calc") == 0) gcalc();
	else if (strcmp(argv[1],"CALCULATOR") == 0) gcalc();
	else if (strcmp(argv[1],"calculator") == 0) gcalc();

	else if (strcmp(argv[1],"CALEND") == 0) gcalend();
	else if (strcmp(argv[1],"calend") == 0) gcalend();
	else if (strcmp(argv[1],"CALENDAR") == 0) gcalend();
	else if (strcmp(argv[1],"calendar") == 0) gcalend();

	else if (strcmp(argv[1],"NOTEPAD") == 0) gnotepad();
	else if (strcmp(argv[1],"notepad") == 0) gnotepad();
	else if (strcmp(argv[1],"NOTE") == 0) gnotepad();
	else if (strcmp(argv[1],"note") == 0) gnotepad();
	else if (strcmp(argv[1],"NOTES") == 0) gnotepad();
	else if (strcmp(argv[1],"notes") == 0) gnotepad();

	else if (strcmp(argv[1],"puzzle") == 0) gpuzzle();
	else if (strcmp(argv[1],"PUZZLE") == 0) gpuzzle();

	else if (strcmp(argv[1],"zone") == 0) gzones();
	else if (strcmp(argv[1],"ZONE") == 0) gzones();
	else if (strcmp(argv[1],"zones") == 0) gzones();
	else if (strcmp(argv[1],"ZONES") == 0) gzones();
	else if (strcmp(argv[1],"timezones") == 0) gzones();
	else if (strcmp(argv[1],"TIMEZONES") == 0) gzones();
	else
	{
		vbell();
		vmove(23,0); vprint("%s is an invalid selection.\n",argv[1]);
		vprint("Valid selections are: calc, calend, clock, notepad or puzzle.\n");
		vprint("Depress any key to continue...");
		vgetm();
		vprint("\n");
	}
	vexit();
	return 0;
}
/*
**	History:
**	$Log: good.c,v $
**	Revision 1.8  1996/09/13 18:02:43  gsl
**	Fix headers and warnings for NT
**	
**
**
*/
