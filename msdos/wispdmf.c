/*
	Copyright (c) 1991, International Digital Scientific, Incorporated.
*/
/*
	wispdmf.c	WISP DMF MS-DOS Micro Focus COBOL
			This file contains globals and routines which are for
			the DMF version of WISP only.

	These modules are from SCS WISP MSDOS:

		DOSSTUBS.C - Stubbed routines not found on MSDOS.
		MSDOSFNS.C - Functions written for MSDOS (many are from unix).
		WISPDMF.C  - Like WISPAIX.C, includes shutexitcobol().

	When a stubbed routine gets written, it should be moved from
	the DOSSTUBS.C module into the MSDOSFNS.C module.
*/

#ifdef MSDOS

char	WISPFILEXT[39];		/* Define the file extension variable.	*/
char	WISPRETURNCODE[3];	/* Define the return code field.	*/

wispdmf()
{
	return 0;
}

shutexitcobol(exit_code)
int exit_code;
{
	cobtidy();
	exit(exit_code);
}

#endif	/* #ifdef MSDOS */

