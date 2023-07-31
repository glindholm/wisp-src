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

#include <video.h>
#include <vmodules.h>

int testt()
{
	register int i;

	vmove(0,0);
	verase(FULL_SCREEN);
	vmode(BOLD);
	vprint("This is a test of tabs in the output stream.\n");
	vprint("All text should align on eight column boundaries.\n");
	vprint("Use ^W to verify that what is on the screen matches what is in the maps.");
	vmode(CLEAR);

	i = 5;
	vmove(i++,0); vprint("This should start in column 0");
	vmove(i++,0); vprint("\tThis should start in column 8");
	vmove(i++,0); vprint("\t\tThis should start in column 16");
	vmove(i++,0); vprint("\t\t\tThis should start in column 24");
	vmove(i++,0); vprint("\t\t\t\tThis should start in column 32");
	i++;
	vmove(i++,0); vprint("0\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("01\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("012\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("0123\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("01234\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("012345\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("0123456\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("01234567\tThis is preceded with a single tab.");
	vmove(i++,0); vprint("012345678\tThis is preceded with a single tab.");

	vmove(23,0);
	vprint("^W to check the maps, anything else to continue...");
	vgetc();

	return(SUCCESS);
}
