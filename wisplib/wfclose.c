static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


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

void wfclose(const char* fname)
{
	WFCLOSE(fname);
}
void WFCLOSE(const char* fname)								/* This routine is called after COBOL*/
{											/* closes the file.			*/

	int retcd;
	
	pstruct *lptr;									/* A local pointer into the structure.	*/
	char	def_prt_mode;

	if (g_print_file_list)								/* if any files opened print/hold	*/
	{
		wpload();
		get_defs(DEFAULTS_PM,&def_prt_mode);
		if (fname[0] == '*')							/* '*' means dump them all.		*/
		{
			/*
			**	Print everything in the list and free the list.
			**	This is called from wexith.
			*/
			while(g_print_file_list)
			{
				lptr = g_print_file_list;
				if (lptr->name[0])
				{
					wprint(lptr->name,def_prt_mode,NULL,lptr->numcopies,
							lptr->class,lptr->form,&retcd);

					lptr->name[0] = '\0';				/* Say we have already processed it.	*/
				}
				g_print_file_list = lptr->nextfile;
				free(lptr);
			}
		}
		else
		{
			char trim_fname[COB_FILEPATH_LEN + 1];

			cobx2cstr(trim_fname, fname, COB_FILEPATH_LEN);
			
			lptr = g_print_file_list;
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
				wprint(lptr->name,def_prt_mode,NULL,lptr->numcopies,
						lptr->class,lptr->form,&retcd);
				lptr->name[0] = '\0';					/* Say we have already processed it.	*/
			}

		}
	}
}
/*
**	History:
**	$Log: wfclose.c,v $
**	Revision 1.12.2.1  2002/11/12 16:00:23  gsl
**	Applied global unique changes to be compatible with combined KCSI
**	
**	Revision 1.12  1998/10/22 18:12:42  gsl
**	change to use g_print_file_list
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
