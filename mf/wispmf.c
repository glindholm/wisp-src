/* 
	Copyright (c) 1996 NeoMedia Technologies, All rights reserved.
	$Id:$
*/
/*
	wispmf.c	WISP Micro Focus COBOL
			This file contains globals and routines which are for
			the MF version of WISP only.
*/

extern int cobfunc();

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


/*
**	Routine:	call_mfcobol()
**
**	Function:	To "call" an MF COBOL routine from 'C'.
**
**	Description:	This routine builds the correct calling sequence to call
**			an MF COBOL routine from 'C'.
**
**	Arguments:
**	filespec	The full file spec of the COBOL routine to call.
**	parmcnt		The number of parameters to pass.
**	parms		Array of pointers to the parms.
**	lens		Array of lengths of the parms
**	rc		The return code from the "cobol" routine. 
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void call_mfcobol( filespec, parmcnt, parms, lens, rc )
char *filespec;
int parmcnt;
char *parms[];
int lens[];
int *rc;
{
	/*
	**	cobfunc is equivalent of cobcall() followed by cobcancel().
	**
	**	Note: the lens arrary is not used by MF.
	*/

	*rc = cobfunc( filespec, parmcnt, parms );
}

