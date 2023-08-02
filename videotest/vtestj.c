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

#include <ctype.h>
#include <video.h>
#include <vmodules.h>

int testj()
{
	char c;
	int retcount;
	unsigned char uc;

	verase(FULL_SCREEN);
	vmove(0,0);
	retcount = 0;
	vprint("Enter any character and its sequence will be echoed back.\n");
	vprint("   (Press ENTER twice to exit.)\n\n");

again:	c = vgetc();
	if (c == '\15')
	{
		retcount = retcount + 1;
		if (retcount >= 2) return(SUCCESS);
	}
	else retcount = 0;

	uc = (unsigned char)c;
	if (isprint((int)c) && isascii((int)c))
	{
		vprint("Character is \'%c\' = 0x%02x hex, %3u dec, %04o oct\n",c,(int)uc,(int)uc,(int)uc);
	}
	else if (uc <= 27) /* Print as control char */
	{
		vprint("Character is ^%c  = 0x%02x hex, %3u dec, %04o oct\n",c+'@',(int)uc,(int)uc,(int)uc);
	}
	else 
	{
		vprint("Character is     = 0x%02x hex, %3u dec, %04o oct\n",(int)uc,(int)uc,(int)uc);
	}
	goto again;
}
