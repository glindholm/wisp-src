			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/
#ifdef unix
static int vcommand_dummy;
#else
#ifdef MSDOS
#else
			/************************************************************************/
			/*									*/
			/*	Get a user's command line (DEC style).				*/
			/*									*/
			/*	If running from a command file,   input will come from		*/
			/*	the command file, else from the terminal. Only the com-		*/
			/*	mand parameters are passed  (the command itself is not		*/
			/*	included. For example:						*/
			/*									*/
			/*	.FOO A B C will return only A B C, not the FOO.			*/
			/*									*/
			/*	The prompt string will be given when only the command		*/
			/*	is entered by the user. For example:				*/
			/*									*/
			/*			.FOO<ret>					*/
			/*									*/
			/*	will result in the prompt.					*/
			/*									*/
			/*			Prompt? A B C					*/
			/*									*/
			/*	Under RT-11 the USR will scramble the input to form an		*/
			/*	assumed CSI string.    This is unscrambled to simulate		*/
			/*	the type of input on most other operating systems. The		*/
			/*	characters "=" and "@" are not allowed under RT-11.		*/
			/*									*/
			/*	Call using:							*/
			/*									*/
			/*		vcommand(command_string,prompt_string);			*/
			/*									*/
			/*	For compatibility with VMS, the command_string will be		*/
			/*	set to upper case.						*/
			/*									*/
			/*	If the command EXIT is given,   the program will exit.		*/
			/*									*/
			/*	If the command was entered inline (non-prompting form)		*/
			/*	subsequent calls to this routine will exit.			*/
			/*									*/
			/************************************************************************/

#ifdef	vax11c
	/* All ok */
#else
#ifdef	decus
	/* All ok */
#else
	*** ERROR: Neither vax11c or decus is defined ***
#endif
#endif

/************************************************************************/
/*	Definitions and macros.						*/
/************************************************************************/

#ifdef vax11c
#include <ssdef.h>			/* Get the system definitions.	*/
#include <rmsdef.h>			/* Get the RMS definitions.	*/
#include <descrip.h>			/* Get the $DESCRIPTOR macro.	*/
#define	MAXSTR	132			/* Maximum command length + 1	*/
#define	FALSE	0			/* Standard false definition.	*/
#define	TRUE	1			/* Standard true definition.	*/
#endif

/************************************************************************/
/*	Global data definitions.					*/
/************************************************************************/

#ifdef vax11c
	int first = TRUE;		/* First time flag.		*/
	int inline = TRUE;		/* First command was inline.	*/
#endif

#ifdef decus
	int $$narg = 1;			/* Run-time not to prompt.	*/
#endif


/************************************************************************/
/*	Entry point for vcommand function.				*/
/************************************************************************/

vcommand(command,prompt)

	char *prompt;			/* User supplied prompt.	*/
	char *command;			/* Buffer for command string.	*/
{

/************************************************************************/
/*	Data declarations.						*/
/************************************************************************/

#ifdef decus
	extern int gtlin();		/* RT-11 get line routine.	*/
	register int i,j,k;		/* Some working registers.	*/
	char temp[81];			/* Temporary storage.		*/
#endif

#ifdef	vax11c
	extern int lib$get_foreign();	   /* Use get foreign command.	*/
	int status;			   /* Status from get_foreign.	*/
	long int length;		   /* Maximum command length.	*/
	long int force_prompt; 		   /* Flag to force a prompt.	*/
#include "inc:vcommand.d"
	cmd.dsc$w_length = MAXSTR;		/* Actual str length.	*/
	prm.dsc$w_length = strlen(prompt);	/* Actual prompt len.	*/
#endif

#ifdef	vax11c
	if (first)
	{
		status = lib$get_foreign(&cmd);	/* Try for actual cmd.	*/
		strtrm(command,MAXSTR);		/* Command trim down.	*/
		if (command[0] == '\000')
		{
			first = FALSE;		/* Force prompting.	*/
			inline = FALSE;		/* Command not inline.	*/
		}
	}
	if (first == FALSE)
	{
		if (inline) exit(SS$_NORMAL);	/* 1st time was inline.	*/

		force_prompt = 1;		/* Force a prompt.	*/
		status = lib$get_foreign(&cmd,&prm,&length,&force_prompt);
		if (status != SS$_NORMAL)	/* Process errors.	*/
		{
		    if (status == RMS$_EOF) exit(SS$_NORMAL);
		    else printf("?GETCOM-W-Input error, code = %d\n",status);
		}
		strtrm(command,MAXSTR);		/* Trim blanks.		*/
	}
	first = FALSE;				/* Not first any more.	*/
	if (strcmp("EXIT",command) == 0) exit(SS$_NORMAL);
	return(status);				/* Return good status.	*/
#endif

#ifdef decus
/* Determine if a prompt is required. */

	if (prompt[0] == '\000') call(gtlin,1,command);
	else
	{
		i = 0;
		while (prompt[i] != '\000') i++;
		prompt[i] = '\200';
		call(gtlin,2,command,prompt);
		prompt[i] = '\000';
	}

/* Determine how many equal signs are in the command. */

	i = 0;
	j = 0;
	while (command[i] != 0)
	{
		if (command[i] == '=') { j++; k = i; }
		i++;
	}

/* Unscramble if there is 1 equal sign. */

	if ((j == 1) && (command[k+1] != 0))
	{
		strcpy(temp,&command[k+1]);
		command[k] = 0;

		i = 0;
		while (temp[i] != 0) i++;
		temp[i] = ' ';
		temp[i+1] = 0;

		i = 0;
		while (temp[i] != 0)
		{
			for (j = k; j >= i; j--) command[j+1] = command[j];
			k++;
			command[i] = temp[i];
			i++;
		}
	}

	for (i = 0; command[i] != 0; i++) command[i] = toupper(command[i]);
	if (strcmp("EXIT",command) == 0) exit(1);
	return(1);
#endif
}


static strtrm(string,size)
	int size;			/* Size of string.		*/
	char string[];			/* String to be trimmed.	*/
{
	register int i;			/* Working variable.		*/

	i = size - 1;
	while ((i >= 0) && (string[i] == ' '))
	{
		string[i] = 0;
		i = i - 1;
	}
	return;
}

#endif
#endif
