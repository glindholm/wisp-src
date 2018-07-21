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

/* these go in video.h I guess */
#define UNIMODE_WANG 1
#define UNIMODE_NORM 2

#ifdef unix
int vrun_unique(mode) int mode;								/* Visual mode (Wang, normal etc.	*/
{
	int	pid, rc;
	int	st;

	VL_vpushscr();									/* Save the current screen.		*/
	VL_vexit();									/* Shut video down.			*/

	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/

	switch (pid=fork())
	{
		case 0:
		{	
			st = execlp("unique","unique",mode==UNIMODE_WANG?"-w":(char*)0,(char *)0);
			exit(9);
			break;
		}
		default:
		{
			vwaitpid(pid,&rc);					/* this is in WISP.. it gets the return code. */
			break;
		}
	}

	signal(SIGCLD,  SIG_IGN);							/* Ignore DEATH-OF-CHILD signal		*/

	vstate(0);									/* Start up video again.		*/
	VL_vpopscr();									/* Restore the screen that was there.	*/

	if (rc)
	{
		switch(rc)
		{
			case 1:
			{
				VL_vre_window("Unique Print Queue Daemon not running.");
				break;
			}
			case 9:
			{
				VL_vre_window("Cannot find the UniQue queue management program \"unique\".");
				break;
			}
			default:
			{
				VL_vre_window("Error code %d when trying to run \"unique\".                    Contact system administrator",rc);
				break;
			}
		}
	}

	return(SUCCESS);								/* All done.				*/
}

int vuniprint(file, copies, lpclass, printernum, printer, form, hold, del, respool)
	char	*file;									/* File to be printed.			*/
	int	copies;									/* 0 or 1 = 1 copy.			*/
	char	lpclass;								/* '\0' or ' ' for no class.		*/
	int4	printernum;								/* 0 for no printer number.		*/
	char    *printer;								/* Printer name or NULL			*/
	char    *form;									/* form name or NULL			*/
	int     hold, del, respool;  /* TRUE or FALSE */
{
	int	rc;
	char	*ptr;
	char	cmd[256];
	char    modestr[64];
	int del=0, re=0, hold=0;
	char l_form[35], l_copies[10], l_printer[35], l_class[5], modestr[20];
	char *argv[10];
	int argc;
	
	argc=0;
	argv[argc++]="unique";
	
	if (form!=NULL)
	{
		sprintf(l_form,"-f%s",form);
		argv[argc++] = l_form;
	}
	if (copies > 1)
	{
		sprintf(l_copies,"-n%d",copies);
		argv[argc++] = l_copies;
	}		
	if ((lpclass!='\0') && (lpclass != ' '))
	{
		sprintf(l_class,"-C%c",lpclass);
		argv[argc++] = l_class;
	}
	if (printer)
	{
		sprintf(l_printer,"-P%s",printer);
		argv[argc++] = l_printer;
	}
	else if (printernum)
	{
		strcpy(l_printer,"-P%d",printernum);
		argv[argc++] = l_printer;
	}
	if (respool || del || hold)
	{
		strcpy(modestr,"-M");
		if (hold)
		{
			strcat(modestr,"hold");
			if (re || del)
			{
				strcat(modestr,",");
			}
		}
		if (respool)
		{
			strcat(modestr,"re");
		}
		else if (del)
		{
			strcat(modestr,"del");
		}
		argv[argc++] = modestr;
	}
	argv[argc]=NULL;
	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/

	switch(pid=fork())
	{
		case 0:
		{	
			st = execlp("unique",argv);
			exit(9);
			break;
		}
		default:
		{
			vwaitpid(pid,&rc);
			break;
		}
	}
	signal(SIGCLD,  SIG_IGN);							/* Ignore DEATH-OF-CHILD signal		*/

	return(rc);
}

#endif
#ifdef unix
#include <stdio.h>
#include <sys/types.h>

#if !defined(u3b2) && !defined(NCR32)
#include <sys/wait.h>
#endif
#include "idsistd.h"

#ifndef WEXITSTATUS
#define WIFEXITED(x)    ( !((x) & 0xff) )
/* evaluates to the low-order 8 bits of the child exit status   */
#define WEXITSTATUS(x)  (int)(WIFEXITED(x) ? (((x) >> 8) & 0xff) : -1)
/* evaluates to a non-zero value if status returned for abnormal termination */

#endif

vwaitpid(pid,rc)								/* Wait for process pid to complete		*/
int *rc,pid;

{
	int	stat_loc;

	do
	{
		wait(&stat_loc);
	} while (kill(pid,0) == 0);

        *rc=WEXITSTATUS(stat_loc);
}
#endif
/*
**	History:
**	$Log: vuip.c,v $
**	Revision 1.9  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.8  2002/07/16 14:11:48  gsl
**	VL_ globals
**	
**	Revision 1.7  2002/07/15 20:16:15  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.6  1996/10/11 22:16:23  gsl
**	drcs update
**	
**
**
*/
