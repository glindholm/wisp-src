/*
	wispmf.c	WISP Micro Focus COBOL/2
			This file contains globals and routines which are for
			the MF version of WISP only.
*/

char	WISPFILEXT[39];		/* Define the file extension variable.	*/
char	WISPRETURNCODE[3];	/* Define the return code field.	*/

wispmf()
{
	return 0;
}

shutexitcobol(exit_code)
int exit_code;
{
	cobtidy();
	exit(exit_code);
}

