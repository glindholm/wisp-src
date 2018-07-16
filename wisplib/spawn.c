			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#ifdef VMS
#include <stdio.h>
#include <descrip.h>
#include <stsdef.h>
#include <v/video.h>

long spawn(action,progname,message)
int action;
char *progname;
char *message;
{
	unsigned long status;
	long retstat;
	static char command[200];
	static $DESCRIPTOR(prompt,"DCL> ");
	struct dsc$descriptor_s icom;
	struct dsc$descriptor_s *comptr;
	char tmode;

	osd_mode(&tmode);							/* Check our current mode.			*/

	icom.dsc$b_dtype = DSC$K_DTYPE_T;					/* Type "T".					*/
	icom.dsc$b_class = DSC$K_CLASS_S;					/* Class "S".					*/

	if (action == 0)							/* Spawn a DCL task.				*/
	{
		comptr = 0;							/* No command,just run dcl.			*/
	}
	else if (action == 1)							/* Run a program				*/
	{
		strcpy(command,"run ");
		strcat(command,progname);
		comptr = &icom;							/* Point to command descriptor.			*/
		icom.dsc$w_length = strlen(command);				/* Set the descriptor length.			*/
		icom.dsc$a_pointer = command;					/* Point to the command string.			*/
	}
	else if (action == 2)							/* execute a command with a prompt at return 	*/
	{
		comptr = &icom;							/* Point to command descriptor.			*/
		icom.dsc$w_length = strlen(progname);				/* Set the descriptor length.			*/
		icom.dsc$a_pointer = progname;					/* Point to the command string.			*/
	}
	else if (action == 3)							/* execute a command with no return prompt	*/
	{
		comptr = &icom;							/* Point to command descriptor.			*/
		icom.dsc$w_length = strlen(progname);				/* Set the descriptor length.			*/
		icom.dsc$a_pointer = progname;					/* Point to the command string.			*/
	}
	else return(-1);							/* Some kind of error.				*/

	if (tmode == 'F')							/* If it's not a batch job...			*/
	{
		wpushscr();							/* Save the screen.				*/
		vshut();							/* Shut down any QIOs.				*/

		vmove(22,0);							/* Move to line 22.				*/
		verase(TO_EOS);							/* Erase some lines.				*/
		vprint("%s\n",message);						/* Print the message.				*/
		vset(CURSOR,VISIBLE);						/* make the cursor visible.			*/
		vdefer(RESTORE);						/* Restore deferred mode.			*/
	}

	retstat = lib$spawn(comptr,0,0,0,0,0,&status,0,0,0,&prompt);		/* Now Spawn a sub process.			*/
	status &= 0x00ffffff;    						/* Mask so is just condition identification.	*/
	if (tmode == 'F')							/* If it's not a batch job...			*/
	{
		if (action == 2)
		{
			vmove(23,0);
			vprint("Please press return to continue.");

			while ( vgetc() != 13 ) { }				/* Wait for a CR				*/
		}
		wpopscr();							/* Restore the screen.				*/
	}
	return(status);
}
#endif	/* VMS */

#ifndef VMS	/* unix or MSDOS */
spawn(action,progname,message)
int action;
char *progname;
char *message;
{
	return(-1);
}
#endif	/* unix or MSDOS */
