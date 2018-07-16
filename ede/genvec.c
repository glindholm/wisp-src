			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	GENVEC		General Program Vectoring Routine.
			This routine is passed the name of a program to run plus two parameters.

			NOTE: If called from COBOL the parameters are not NULL terminated.
*/

GENVEC( program, inparms, outparms)
char	*program;								/* The PROGRAM to vector to			*/
char	*inparms;								/* Input parameters				*/
char	*outparms;								/* Output parameters				*/
{
	werrvre("GENVEC: unable to vector program. Aborting!");
	wexit(0);
}

