/*
******************************************************************************
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
******************************************************************************
*/


/*
**	File:		prompt.c
**
**	Purpose:	A set of prompt routines for getting responses from the user running on a serial terminal.
**
**	Routines:	prompt_list()		Accept from a list of values.
**			prompt_text()		Accept a text string.
**			prompt_num()		Accept a number.
**			printhelp()		Print the help string for the prompt
**			upstr()			Shift a string to uppercase.
**
**
**	History:
**			05/21/92	Written by GSL
**
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>

#include "idsistd.h"
#include "prompt.h"

static void printhelp(const char* help);
static char *upstr(char* str);

/*
**	Routine:	prompt_list()		Accept from a list of values.
**
**	Function:	Prompts the user to choose from a list of values.
**
**	Description:	
**
**	Input:		message		The message to display.
**			defstr		The default value to be displayed. NULL == no default.
**			list		A comma separated list of acceptable values.  This will be display as part of the prompt.
**					The values will be case insensitive.
**			help		The help message to print when ? is entered.
**
**	Output:		None
**
**	Return:		-1	Exit request
**			0	Default
**			1	The first value in the list
**			2	The second value in the list etc ...
**
**	Warnings:	It is up to the caller to ensure the message and list lengths do not exceed the screen width.
**
**	History:	05/21/92	Written by GSL
**
*/

int prompt_list(const char *message, const char *defstr, const char *list, const char *help)
{
	char	prompt[256], instr[256], uplist[256];

	if (defstr && *defstr)
	{
		sprintf(prompt,"%s (%s) [%s]",message,list,defstr);
	}
	else
	{
		sprintf(prompt,"%s (%s)",message,list);
	}

	for(;;)
	{
		printf("%s ? ",prompt);
		if (NULL == fgets(instr, sizeof(instr), stdin)) 
		{
			return(PROMPT_RC_EXIT);
		}
		else
		{
			char *ptr;
			if ((ptr = strchr(instr,'\n'))) *ptr = (char)0;
			if ((ptr = strchr(instr,'\r'))) *ptr = (char)0;
		}

		if (0 == strlen(instr))
		{
			if (defstr && *defstr)
			{
				return(PROMPT_RC_DEFAULT);
			}
			else
			{
				printf("\nPlease choose a value from the list.\n\n");
			}
		}
		else if (0==strcmp(instr,"?"))
		{
			printhelp(help);
		}
		else if (0==strcmp(instr,"."))
		{
			return( PROMPT_RC_EXIT );
		}
		else
		{
			char	*tok;
			int	pos;

			upstr(instr);						/* shift the instr to uppercase			*/

			strcpy(uplist,list);					/* Shift the list to uppercase			*/
			upstr(uplist);

			tok = strtok(uplist,",");
			pos = 1;

			while(tok)
			{
				if (0==strcmp(instr,tok)) 
				{
					return(pos);
				}
				tok = strtok(NULL,",");
				pos++;
			}

			printf("\nInvalid value.\nPlease choose a value from the list.\n\n");
		}
	}
}


/*
**	Routine:	prompt_text()		Accept a text string from the user.
**
**	Function:	Prompts the user to enter a text string.
**
**	Description:	
**
**	Input:		message		The message to display.
**			defstr		The default value to be displayed. NULL == no default. The default value will be
**					copied to "text".  This implies a empty=1.
**			empty		Flag, is an empty string allowed. 0=empty not allowed.
**			help		The help message to print when ? is entered.
**
**	Output:		text		The text string entered
**
**	Return:		-1	Exit request
**			0	Default string
**			1	Text string
**			2	Empty string
**
**	Warnings:	It is up to the caller to ensure the message and list lengths do not exceed the screen width.
**			It is up to the caller to ensure that "text" is large enough to hold the result.
**
**	History:	05/21/92	Written by GSL
**
*/

int prompt_text(const char* message, const char* defstr, int empty, const char* help, char* text)
{
	char	prompt[256];
	char	instr[256];

	if (defstr && *defstr)
	{
		sprintf(prompt,"%s [%s]",message,defstr);
	}
	else
	{
		strcpy(prompt,message);
	}

	for(;;)
	{
		printf("%s ? ",prompt);
		if (NULL == fgets(instr, sizeof(instr), stdin)) 
		{
			return(PROMPT_RC_EXIT);
		}
		else
		{
			char *ptr;
			if ((ptr = strchr(instr,'\n'))) *ptr = (char)0;
			if ((ptr = strchr(instr,'\r'))) *ptr = (char)0;
		}

		strcpy(text, instr);

		if (0 == strlen(text))
		{
			if (defstr && *defstr)
			{
				strcpy(text,defstr);
				return(PROMPT_RC_DEFAULT);
			}
			else if (empty)
			{
				return(PROMPT_RC_EMPTY);
			}
			else
			{
				printf("\nPlease enter a text string.\n\n");
			}
		}
		else if (0==strcmp(text,"?"))
		{
			printhelp(help);
		}
		else if (0==strcmp(text,"."))
		{
			return(PROMPT_RC_EXIT);
		}
		else
		{
			return(PROMPT_RC_USER_VALUE);
		}
	}
}


/*
**	Routine:	prompt_num()		Accept a number from the user.
**
**	Function:	Prompts the user to enter a number.
**
**	Description:	
**
**	Input:		message		The message to display.
**			defstr		The default value to be displayed. NULL == no default.
**			help		The help message to print when ? is entered.
**
**	Output:		outnum		The number entered by the user.
**
**	Return:		-1	Exit request
**			0	Default
**			1	User entered a number
**
**	Warnings:	It is up to the caller to ensure the message and list lengths do not exceed the screen width.
**			This routine does not check for overflow of the input number.
**
**	History:	05/21/92	Written by GSL
**
*/

int prompt_num (const char *message, const char *defstr, const char *help, int4 *outnum)
{
	char	prompt[256], instr[256];

	if (defstr && *defstr)
	{
		sprintf(prompt,"%s [%s]",message,defstr);
	}
	else
	{
		strcpy(prompt,message);
	}

	for(;;)
	{
		printf("%s ? ",prompt);
		if (NULL == fgets(instr, sizeof(instr), stdin)) 
		{
			return(PROMPT_RC_EXIT);
		}
		else
		{
			char *ptr;
			if ((ptr = strchr(instr,'\n'))) *ptr = (char)0;
			if ((ptr = strchr(instr,'\r'))) *ptr = (char)0;
		}

		if (0 == strlen(instr))
		{
			if (defstr && *defstr)
			{
				return(PROMPT_RC_DEFAULT);
			}
			else
			{
				printf("\nPlease enter a number.\n\n");
			}
		}
		else if (0==strcmp(instr,"?"))
		{
			printhelp(help);
		}
		else if (0==strcmp(instr,"."))
		{
			return( PROMPT_RC_EXIT );
		}
		else
		{
			char	*ptr;

			*outnum = (int4) strtol(instr,&ptr,10);
			if (ptr == instr || (!isspace((int)*ptr) && *ptr !='\0'))
			{
				printf("\nInvalid number.\nPlease enter a number.\n\n");
			}
			else
			{
				return(PROMPT_RC_USER_VALUE);
			}
		}
	}
}


/*
**	Routine:	printhelp()	
**
**	Function:	To print the help string for the prompt
**
**	Description:	If a help string was given it is printed if NULL then a "no help" message is printed.
**
**	Input:		help		the help string
**
**	Output:		stdout
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**
*/

static void printhelp(const char* help)
{
	if (help)
	{
		printf("\n%s\n\n",help);
	}
	else
	{
		printf("\nSorry, no help available\n\n");
	}
}

/*
**	Routine:	upstr()
**
**	Function:	To shift a string to uppercase (inplace).
**
**	Description:	
**
**	Input:		str		the string to uppercase
**			
**
**	Output:		str		the string shifted to uppercase
**			
**
**	Return:		pointer to the terminating null of str.
**
**	Warnings:	None
**
**	History:	05/21/92	Written by GSL
**
*/

static char *upstr(char* str)
{
	for (; *str; str++)
	{
		*str = toupper(*str);
	}
	return( str );
}
/*
**	History:
**	$Log: prompt.c,v $
**	Revision 1.14  2003/06/17 16:12:54  gsl
**	No default value can be specified by either a NULL or empty string
**	
**	Revision 1.13  2003/05/27 20:53:08  gsl
**	Update PROMPT prototypes and add defines for return codes
**	
**	Revision 1.12  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 18:50:26  gsl
**	fix copyright header
**	
**	Revision 1.10  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.9  2002/09/06 15:06:43  gsl
**	When changing gets() to fgets() you have to now strip off the trailing NL (and CR)
**	
**	Revision 1.8  2002/09/05 14:20:08  gsl
**	gets()->fgets()
**	
**	Revision 1.7  1996/07/24 23:36:46  gsl
**	Fix warnings for NT
**	
**	Revision 1.6  1996-07-23 11:12:57-07  gsl
**	drcs update
**
**
**
*/
