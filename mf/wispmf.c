/* 
	Copyright (c) Shell Stream Software LLC, All rights reserved.
	$Id:$
*/
/*
	wispmf.c	WISP Micro Focus COBOL
			This file contains globals and routines which are for
			the MF version of WISP only.
*/

extern int cobfunc();
extern int cobtidy();
extern void exit();

int wispmf()
{
	return 0;
}

void WL_shutexitcobol(exit_code)
int exit_code;
{
	cobtidy();
	exit(exit_code);
}


/*
**	Routine:	WL_call_mfcobol()
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
void WL_call_mfcobol( filespec, parmcnt, parms, lens, rc )
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

/*
**	Routine:	wisp_mf_cobol()
**
**	Function:	To identify if we are in an Mico Focus RTS.
**
**	Description:	Two versions exist.
**			The one in wispmf.c returns TRUE.
**			The one in mfstubs.c returns FALSE.
**
**	Arguments:	None
**
**	Return:		
**	1		TRUE	- in an Micro Focus RTS
**	0		FALSE	- not in an Micro Focus RTS
**
*/
int wisp_mf_cobol()
{
	return 1; /* TRUE */
}

