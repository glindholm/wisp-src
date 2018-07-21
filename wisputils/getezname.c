/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
** $Author: gsl $
**
**
******************************************************************************
*/

#include <stdio.h>										/* Reference standard I/O.	*/
#include "filext.h"

char *WL_wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);

/*						Entry point.									*/

main(argc,argv) int argc; char *argv[];
{
	int4 mode;										/* Mode for wfname.		*/
	int i;
	char *end_name, *end_name1;							/* Pointers for wfname.		*/
 	char in_name[256];									/* Vax style input name.	*/
 	char out_name[256];									/* Vax style output name.	*/
 	char out_file[8];									/* Wang Style Name.		*/
 	char out_lib[8];									/* Wang Style Name.		*/
 	char out_vol[8];									/* Wang Style Name.		*/
 	char in_file[8];									/* Wang Style Name.		*/
 	char in_lib[8];										/* Wang Style Name.		*/
 	char in_vol[8];										/* Wang Style Name.		*/

        memset(in_name,' ',256);
        memset(out_name,' ',256);
        memset(in_file,' ',8);
        memset(in_lib,' ',8);
        memset(in_vol,' ',6);
        memset(out_file,' ',8);
        memset(out_lib,' ',8);
        memset(out_vol,' ',6);
        WSETFILEXT(" ");
	mode		= 0;

	if (argc == 3)										/* Did user give a filename?	*/
	{
		strcpy(in_name,argv[1]);
		strcpy(out_name,argv[2]);
                EZMAKE(in_name,out_name);
		return;
	}
	if (argc == 2)										/* Did user give a filename?	*/
	{
		strcpy(in_name,argv[1]);
                EZFORMAT(in_name);
		return;
	}
	if (argc == 4 || argc == 5 || argc == 6 || argc == 7)
	{
		strcpy(in_file,argv[1]);							/* Load the wang style name.	*/
		strcpy(in_lib,argv[2]);								/* Load Wang library.		*/
		strcpy(in_vol,argv[3]);								/* Load wang Volume.		*/
	}
	if (argc == 4)
	{
		WSETFILEXT("SCR");

		end_name = WL_wfname(&mode,in_vol,in_lib,in_file,in_name);				/* Call wfname to make name. 	*/
		in_name[*end_name] = '\0';							/* Null terminate the name.	*/
                EZFORMAT(in_name);	
		return;
	}
	if (argc == 5)										/* Did user give a filename?	*/
	{
		strcpy(out_file,argv[4]);							/* Load wang Volume.		*/
		WSETFILEXT("SCR");
		end_name = WL_wfname(&mode,in_vol,in_lib,in_file,in_name);				/* Call wfname to make name. 	*/
		in_name[*end_name] = '\0';							/* Null terminate the name.	*/
		WSETFILEXT("COB");								/* Just copy it in.		*/
		end_name1 = WL_wfname(&mode,in_vol,in_lib,out_file,out_name);			/* Call wfname to make name. 	*/
		out_name[*end_name1] = '\0';							/* Null terminate the name.	*/
                EZMAKE(in_name,out_name);	
		return;
	}
	if (argc == 6)										/* Did user give a filename?	*/
	{
		strcpy(out_file,argv[4]);							/* Load wang Volume.		*/
		strcpy(out_lib,argv[5]);							/* Load wang Volume.		*/
		WSETFILEXT("SCR");
		end_name = WL_wfname(&mode,in_vol,in_lib,in_file,in_name);				/* Call wfname to make name. 	*/
		in_name[*end_name] = '\0';							/* Null terminate the name.	*/
		WSETFILEXT("COB");								/* Just copy it in.		*/
		end_name1 = WL_wfname(&mode,in_vol,out_lib,out_file,out_name);			/* Call wfname to make name. 	*/
		out_name[*end_name1] = '\0';							/* Null terminate the name.	*/
                EZMAKE(in_name,out_name);	
		return;
	}
	if (argc == 7)										/* Did user give a filename?	*/
	{
		strcpy(out_file,argv[4]);							/* Load wang Volume.		*/
		strcpy(out_lib,argv[5]);							/* Load wang Volume.		*/
		strcpy(out_vol,argv[6]);							/* Load wang Volume.		*/
		WSETFILEXT("SCR");
		end_name = WL_wfname(&mode,in_vol,in_lib,in_file,in_name);				/* Call wfname to make name. 	*/
		in_name[*end_name] = '\0';							/* Null terminate the name.	*/
		WSETFILEXT("COB");								/* Just copy it in.		*/
		end_name1 = WL_wfname(&mode,out_vol,out_lib,out_file,out_name);			/* Call wfname to make name. 	*/
		out_name[*end_name1] = '\0';							/* Null terminate the name.	*/
                EZMAKE(in_name,out_name);	
		return;
	}
	printf("%F-EZFORMAT-F-Unable to open or find file \n");
}
COBNAME(new_name,fdl_name)								
char new_name[255], fdl_name[255];							/*Passed vaules name, fdl name.		*/
{
	int c,d;									/*Local pointers and integers for count.*/
	char *hld_adr;
	hld_adr = new_name;								/*Hold the original pointer.		*/
	c = 0;										/*Initialize the Loop.			*/
	while (c < 255)									/*perfrom for length of string.		*/
	{
		if (new_name[c] == ' ' || new_name[c] == '\0')				/*If blank or null I'm done.		*/
		{
			fdl_name[c] = '\0';						/*Null terminate the Name, and FDL name.*/
			new_name[c] = '\0';
			d = c;								/*Hold ending point to count in reverse.*/
			c = 256;							/*Terminate the Loop.			*/
		}
		else								
		{
			fdl_name[c] = new_name[c];					/*Load the values to FDL name		*/
			c++;								/*Up the count by one.			*/
		}
	}
	while (d > 1)									/*Start revese count.			*/
	{
		if (fdl_name[d] == '.')							/*Look for period.			*/
		{
			d++;								/*If found load fdl as extention.	*/
			fdl_name[d] = 'C';
			d++;
			fdl_name[d] = 'O';
			d++;
			fdl_name[d] = 'B';
			d++;
			fdl_name[d] = '\0';						/*Null terminate the FDL name.		*/
			d = 0;
		}
		else
			d--;								/*Decriment the index down by one.	*/
	}
}
/*
**	History:
**	$Log: getezname.c,v $
**	Revision 1.13  2003/02/04 18:50:26  gsl
**	fix copyright header
**	
**	Revision 1.12  2002/07/29 15:46:51  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.11  2002/07/12 19:10:22  gsl
**	Global unique WL_ changes
**	
**	Revision 1.10  2002/07/12 17:17:03  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  2002/06/25 17:46:06  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.8  1997/06/10 19:44:18  scass
**	Changed long to int4 for portability.
**	
**	Revision 1.7  1996-07-23 14:12:54-04  gsl
**	drcs update
**
**
**
*/
