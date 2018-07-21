/*
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
*/


/*
**	wfclose.c
*/

#include "idsistd.h"
#include "wperson.h"
#include "wfiles.h"
#include "wisplib.h"
#include "idsisubs.h"

#include "wdefines.h"
#include "wglobals.h"
#include "werrlog.h"


void WFCLOSE(const char* fname)								/* This routine is called after COBOL*/
{											/* closes the file.			*/
	int retcd;
	wisp_pstruct *lptr;								/* A local pointer into the structure.	*/
	char	def_prt_mode;

	if (WL_g_print_file_list)								/* if any files opened print/hold	*/
	{
		WL_wpload();
		WL_get_defs(DEFAULTS_PM,&def_prt_mode);
		if (fname[0] == '*')							/* '*' means dump them all.		*/
		{
			WL_wtrace("WFCLOSE","ENTRY","Entry into WFCLOSE(*)");

			/*
			**	Print everything in the list and free the list.
			**	This is called from wexith.
			*/
			while(WL_g_print_file_list)
			{
				lptr = WL_g_print_file_list;
				if (lptr->name[0])
				{
					WL_wprint(lptr->name,def_prt_mode,NULL,lptr->numcopies,
							lptr->class,lptr->form,&retcd);

					lptr->name[0] = '\0';				/* Say we have already processed it.	*/
				}
				WL_g_print_file_list = lptr->nextfile;
				free(lptr);
			}
		}
		else
		{
			char trim_fname[COB_FILEPATH_LEN + 1];

			cobx2cstr(trim_fname, fname, COB_FILEPATH_LEN);
			WL_wtrace("WFCLOSE","ENTRY","Entry into WFCLOSE(%s)", trim_fname);
			
			lptr = WL_g_print_file_list;
			do
			{
				if (!strcmp(lptr->name,trim_fname))			/* Is the name the same?		*/
				{
					break;						/* Found a match.			*/
				}
				lptr = lptr->nextfile;
			} while (lptr);

			if (lptr)							/* If there was a match, spool it.	*/
			{
				WL_wprint(lptr->name,def_prt_mode,NULL,lptr->numcopies,
						lptr->class,lptr->form,&retcd);
				lptr->name[0] = '\0';					/* Say we have already processed it.	*/
			}

		}
	}
	else
	{
		WL_wtrace("WFCLOSE","NOFILES","No open print files.");
	}
}
/*
**	History:
**	$Log: wfclose.c,v $
**	Revision 1.19  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.18  2002/12/10 20:54:08  gsl
**	use WERRCODE()
**	
**	Revision 1.17  2002/12/10 17:09:14  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.16  2002/07/23 02:57:51  gsl
**	wfclose -> WFCLOSE
**	
**	Revision 1.15  2002/07/11 20:29:16  gsl
**	Fix WL_ globals
**	
**	Revision 1.14  2002/07/10 21:05:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  2002/07/01 04:02:42  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.12  1998/10/22 18:12:42  gsl
**	change to use WL_g_print_file_list
**	Fix "*" processing to free and null the list
**	
**	Revision 1.11  1998-08-03 17:16:49-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.10  1996-08-19 18:33:14-04  gsl
**	drcs update
**
**
**
*/
