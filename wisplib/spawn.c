			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#ifdef VMS
#include <stdio.h>
#include <descrip.h>
#include <stsdef.h>
#include <v/video.h>

#include "idsistd.h"
#include "spawn.h"

int4 spawn2(action,progname,message,status)
int action;
char *progname;
char *message;
uint4 *status;
{
	uint4 retstat;
	static char command[200];
	static $DESCRIPTOR(prompt,"DCL> ");
	struct dsc$descriptor_s icom;
	struct dsc$descriptor_s *comptr;
	char tmode;

	osd_mode(&tmode);							/* Check our current mode.			*/

	icom.dsc$b_dtype = DSC$K_DTYPE_T;					/* Type "T".					*/
	icom.dsc$b_class = DSC$K_CLASS_S;					/* Class "S".					*/

	switch(action)
	{
	case SPAWN_DCL:								/* Spawn a DCL task.				*/
		comptr = 0;							/* No command,just run dcl.			*/
		break;

	case SPAWN_RUN:								/* Run a program				*/
		strcpy(command,"run ");
		strcat(command,progname);
		comptr = &icom;							/* Point to command descriptor.			*/
		icom.dsc$w_length = strlen(command);				/* Set the descriptor length.			*/
		icom.dsc$a_pointer = command;					/* Point to the command string.			*/
		break;
	

	case SPAWN_CMD_AND_PROMPT:						/* execute a command with a prompt at return 	*/
	case SPAWN_CMD:								/* execute a command with no return prompt	*/
	case SPAWN_CMD_QUIET:							/* execute a command with no screen IO		*/
		comptr = &icom;							/* Point to command descriptor.			*/
		icom.dsc$w_length = strlen(progname);				/* Set the descriptor length.			*/
		icom.dsc$a_pointer = progname;					/* Point to the command string.			*/
		break;

	default:
		return(-1);							/* Some kind of error.				*/
	}

	if (action == SPAWN_CMD_QUIET)
	{
		static $DESCRIPTOR(devnull,"NL:");
		retstat = lib$spawn(comptr,&devnull,&devnull,0,0,0,status,0,0,0,&prompt);
		*status &= 0x00ffffff;    					/* Mask so is just condition identification.	*/
	}
	else
	{
		if (tmode == 'F')						/* If it's not a batch job...			*/
		{
			wpushscr();						/* Save the screen.				*/
			vshut();						/* Shut down any QIOs.				*/

			vmove(22,0);						/* Move to line 22.				*/
			verase(TO_EOS);						/* Erase some lines.				*/
			vprint("%s\n",message);					/* Print the message.				*/
			vset(CURSOR,VISIBLE);					/* make the cursor visible.			*/
			vdefer(RESTORE);					/* Restore deferred mode.			*/
		}

		retstat = lib$spawn(comptr,0,0,0,0,0,status,0,0,0,&prompt);	/* Now Spawn a sub process.			*/
		*status &= 0x00ffffff;    					/* Mask so is just condition identification.	*/

		if (tmode == 'F')						/* If it's not a batch job...			*/
		{
			if (action == SPAWN_CMD_AND_PROMPT)
			{
				vmove(23,0);
				vprint("Please press return to continue.");

				while ( vgetc() != 13 ) { }			/* Wait for a CR				*/
			}
			wpopscr();						/* Restore the screen.				*/
		}
	}
	return(retstat);
}
#endif	/* VMS */

#ifndef VMS	/* unix or MSDOS */
#include "idsistd.h"
spawn2(action,progname,message,status)
int action;
char *progname;
char *message;
uint4 *status;
{
	return(-1);
}
#endif	/* unix or MSDOS */
