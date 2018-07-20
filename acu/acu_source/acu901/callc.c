/* callc - directly call a 'C' subroutine with many parameters.  */
/*  $Id: callc.c 57907 2010-04-19 18:56:40Z gary $  */

/* Copyright (c) 1992-2007 by Micro Focus */

/* Micro Focus grants the right to modify this file for the purpose of	*/
/* extending the ACUCOBOL runtime system to licensed users of ACUCOBOL.	*/


/* Acall_direct - this routine is called by the runtime system to when	*/
/* calling a 'C' subroutine that has more than eight parameters.  This	*/
/* routine is normally setup to handle a maximum of 20 parameters.  You	*/
/* may extend this routine to allow more.  To do so, simply modify the	*/
/* number of "params" passed to the routine "func".  */

/* The "func" parameter is the function being called, the "num_params"	*/
/* field is the number of parameters being passed.  This is provided	*/
/* solely for your own benefit - the normal implementation does not	*/
/* use it.  The "params" array are the actual parameters being passed	*/
/* to the subroutine.  Because 'C' has no method of passing a variable	*/
/* number of parameters, you must explicitly code each element in the	*/
/* "func" call.  Note that on 16-bit machines, you must code two	*/
/* parameters for each "long" or "pointer" data type being passed.	*/

/* Once you modify this routine, you must link it into the runtime.	*/
/* To do this, you must explicitly add "callc" to the list of routines	*/
/* being linked to the runtime.  If you don't do this, you will get the	*/
/* version of this routine in located in the runtime library - which	*/
/* only allows for 20 parameters.  */


#ifdef	ACU_SOURCE_FILENAME
#undef	ACU_SOURCE_FILENAME
#endif	/* ACU_SOURCE_FILENAME */
#define	ACU_SOURCE_FILENAME	"lib/callc.c"
const char what_lib_callc_c_str[] = "@(#) " ACU_SOURCE_FILENAME " $Date: 2010-04-19 19:56:40 +0100 (Mon, 19 Apr 2010) $$Rev: 57907 $";

#include <stdio.h>
#if	defined(_WINDOWS) || defined(_WIN64)
#include <windows.h>
#endif	/* _WINDOWS */
#include "sub.h"
extern void		run_abort(char *, ...);

long
Acall_direct(long (*func)(), int num_params, void *params[])
{
    long retval;
    char *errmsg = "Too many parameters passed to a direct C routine.\n"
      "The runtime has a limit to the number of parameters it can\n"
      "pass to a C routine, though that limit can be changed by end\n"
      "users.  To change the limit, modify the file 'lib/callc.c'\n"
      "and relink the runtime with the modified file.\n";

    /* Try to be a little bit efficient about pushing parameters. */
    if (num_params <= 5)
	retval = (*func)(params[0], params[1], params[2], params[3], params[4]);
    else if (num_params <= 10)
	retval = (*func)(params[0], params[1], params[2], params[3], params[4],
			 params[5], params[6], params[7], params[8], params[9]);
    else if (num_params <= 15)
	retval = (*func)(params[0], params[1], params[2], params[3], params[4],
			 params[5], params[6], params[7], params[8], params[9],
			 params[10], params[11], params[12], params[13], params[14]);
    else if (num_params <= 20)
	retval = (*func)(params[0], params[1], params[2], params[3], params[4],
			 params[5], params[6], params[7], params[8], params[9],
			 params[10], params[11], params[12], params[13], params[14],
			 params[15], params[16], params[17], params[18], params[19]);
    else if (num_params <= 25)
	retval = (*func)(params[0], params[1], params[2], params[3], params[4],
			 params[5], params[6], params[7], params[8], params[9],
			 params[10], params[11], params[12], params[13], params[14],
			 params[15], params[16], params[17], params[18], params[19],
			 params[20], params[21], params[22], params[23], params[24]);
    else if (num_params <= 30)
	retval = (*func)(params[0], params[1], params[2], params[3], params[4],
			 params[5], params[6], params[7], params[8], params[9],
			 params[10], params[11], params[12], params[13], params[14],
			 params[15], params[16], params[17], params[18], params[19],
			 params[20], params[21], params[22], params[23], params[24],
			 params[25], params[26], params[27], params[28], params[29]);
    else
	run_abort(errmsg);
    return retval;

}   /* Acall_direct */

/* */
