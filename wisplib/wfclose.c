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

extern pstruct *plist;									/* pointer to printer files		*/

void wfclose(char* fname)								/* This routine is called after COBOL*/
{											/* closes the file.			*/
	int spos, retcd;
	pstruct *lptr;									/* A local pointer into the structure.	*/
	char	def_prt_mode;

	if (plist)									/* if any files opened print/hold	*/
	{
		wpload();
		get_defs(DEFAULTS_PM,&def_prt_mode);
		if (fname[0] == '*')							/* '*' means dump them all.		*/
		{
			lptr = plist;
			do
			{
				if (lptr->name[0])
				{
					wprint(lptr->name,def_prt_mode,NULL,lptr->numcopies,
							lptr->class,lptr->form,&retcd);

					lptr->name[0] = '\0';				/* Say we have already processed it.	*/
				}
				lptr = (pstruct *)lptr->nextfile;
			} while (lptr);

		}
		else
		{
			spos = strpos(fname," ");
			fname[spos] = '\0';						/* Put a null at the first space.	*/
			lptr = plist;
			do
			{
				if (!strcmp(lptr->name,fname))				/* Is the name the same?		*/
				{
					break;						/* Found a match.			*/
				}
				lptr = (pstruct *)lptr->nextfile;
			} while (lptr);

			if (lptr)							/* If there was a match, spool it.	*/
			{
				wprint(lptr->name,def_prt_mode,NULL,lptr->numcopies,
						lptr->class,lptr->form,&retcd);
				lptr->name[0] = '\0';					/* Say we have already processed it.	*/
			}
			fname[spos] = ' ';						/* put the space back in the name.	*/
		}
	}
}
/*
**	History:
**	$Log: wfclose.c,v $
**	Revision 1.10  1996-08-19 18:33:14-04  gsl
**	drcs update
**
**
**
*/
