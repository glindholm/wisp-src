/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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



/*					Include standard header files.								*/

#include <ctype.h>										/* Get character type macros.	*/
#include <stdio.h>										/* Reference standard I/O.	*/
#include <stdlib.h>
#include <string.h>

#include <video.h>

/*						Entry point.									*/

int main(argc,argv) int argc; char *argv[];								/* Determine which goodie.	*/
{
	if (argc != 2) 										/* Did we get a selection?	*/
	{
		printf("Usage: good <selection>\n");
		printf("Valid selections are: calc, calend, clock, notepad or puzzle.\n");
		exit(0);
	}

	VL_vstate(DEFAULT);
	VL_verase(FULL_SCREEN);
	VL_vscreen(NARROW|DARK);
	VL_vmove(0,0);

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
		VL_vbell();
		VL_vmove(23,0); vprint("%s is an invalid selection.\n",argv[1]);
		vprint("Valid selections are: calc, calend, clock, notepad or puzzle.\n");
		vprint("Depress any key to continue...");
		VL_vgetm();
		vprint("\n");
	}
	VL_vexit();
	return 0;
}
/*
**	History:
**	$Log: good.c,v $
**	Revision 1.12  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.10  2002/07/16 14:11:51  gsl
**	VL_ globals
**	
**	Revision 1.9  2002/07/15 20:16:05  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.8  1996/09/13 18:02:43  gsl
**	Fix headers and warnings for NT
**	
**
**
*/
