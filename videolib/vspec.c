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

/*			Include standard header files.										*/

#include <stdio.h>										/* Reference standard I/O.	*/
#include "video.h"										/* Reference video defs.	*/

/*			Prototype all internal routines.									*/

void vusageabort();
extern void vview();
extern int vfcon();

/*			Entry point.												*/

main(argc,argv) int argc; char *argv[];								/* Determine which forms file.	*/
{
	int i,j;										/* Working integers.		*/
	char c;
	int verbose, conversion, debugging, read_only, edit_after_conversion;			/* Switch options.		*/

	verbose = FALSE;									/* Assume not verbose.		*/
	conversion = FALSE;									/* Assume no conversion.	*/
	debugging = FALSE;									/* Assume not debugging.	*/
	read_only = FALSE;									/* Assume edit is read write.	*/
	edit_after_conversion = FALSE;								/* Assume no edit after conv.	*/

	if ((argc < 2) || (argc > 3))								/* Valid number of args?	*/
	{
		if (argc > 3) printf("Error: Too many files specified.\n");			/* Error is too many.		*/
		vusageabort();									/* Else report the usage.	*/
	}

	if (argc == 2)										/* Only 1 arg given?		*/
	{
		if (argv[1][0] == '-')								/* Is it a switch?		*/
		{
			printf("Error: File name must be specified.\n");			/* Yes then it is an error.	*/
			vusageabort();								/* Report the usage.		*/
		}
		else vview(argv[1],read_only,debugging);					/* No, then just edit it.	*/
	}

	else if (argc == 3)									/* Switches?			*/
	{
		if (argv[1][0] != '-')								/* Is it?			*/
		{
			printf("Error: Only one file can be specified.\n");			/* No so report an error.	*/
			vusageabort();								/* Show the usage.		*/
		}

		for (i = 1; argv[1][i] != CHAR_NULL; i++)					/* Process the switches.	*/
		{
			c = argv[1][i];								/* Copy the character.		*/
			if      ((c == 'c') || (c == 'C')) conversion = TRUE;			/* Convert forms listing?	*/
			else if ((c == 'd') || (c == 'D')) debugging = TRUE;			/* Debugging mode?		*/
			else if ((c == 'r') || (c == 'R')) read_only = TRUE;			/* Read only mode.		*/
			else if ((c == 'e') || (c == 'E')) 					/* Edit after conversion.	*/
			{
				if (conversion) edit_after_conversion = TRUE;			/* Want after conversion edit.	*/
				else
				{
					printf("Error: Edit mode (e) is only valid for conversions (c).\n");
					vusageabort();
				}
				
			}
			else if ((c == 'v') || (c == 'V')) 					/* Verbose?			*/
			{
				if (conversion) verbose = TRUE;					/* Valid if convert mode.	*/
				else
				{
					printf("Error: Verbose mode (v) is only valid for conversions (c).\n");
					vusageabort();
				}
			}
			else
			{
				printf("Error: %c is an invalid switch.\n",c);
				vusageabort();
			}
		}

		if (conversion)									/* Do a conversion?		*/
		{
			if (vfcon(argv[2],verbose,debugging) && edit_after_conversion)
			{
				vview(argv[2],read_only,debugging);
			}
		}
		else vview(argv[2],read_only,debugging);
	}
}

void vusageabort()
{
	printf("Usage: vspec [-r] <forms_file>              Graphically edit a VVF forms file.\n");
	printf("Or...  vspec -c[evr] <hp_forms_listing>     Convert an HPF listing to a VVF.\n");
	printf("       -r = read_only (just view the form).\n");
	printf("       -e = edit after converson.\n");
	printf("       -v = verbose reporting during conversion.\n");
	printf("Note:  <hp_forms_listing> files must end in .hpf\n");

	exit();
}
