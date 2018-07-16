#ifdef VMS
/********************************************************************************************************************************
*																*
*	checkacp.c	This program always returns 0 for success.  Checking the status of operations pending on a		*
*			given terminal line is only necessary on the WANG.  For the VAX, all I/O operations in the		*
*			ACP emulation are done synchronously, completing upon return.						*
*			input:	rel_line	relative line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*				length		length of input buffer								*
*				inarea		input buffer									*
*				acp_wait	(optional) amount of time in .01 second units to wait before timeout		*
*				ws_interrupt	(optional) Y or N to allow or disallow detection of workstation interrupts	*
*																*
*			output:	ret_code	WANG 4-byte binary format return code (wswap an int) -- always 0 here.		*
*																*
********************************************************************************************************************************/
#include <varargs.h>							/* Allow variable number of arguments.			*/
#include "werrlog.h"

checkacp(va_alist)

va_dcl

{

#define		ROUTINE		8000

	va_list	arg_stack;						/* Pointer to traverse stack of input arguments.	*/
	int	arg_count;						/* Argument counter.					*/

	int	*rel_line;						/* Index from 1 to 6 for later access to acp_term[].	*/
	int	*length;						/* Length of inarea.					*/
	char	*inarea;						/* Input buffer for read request.			*/
	int	*acp_wait;						/* Amount of time to wait before returning to user.	*/
	char	*ws_interrupt;						/* Y or N for detection of workstation interrupts	*/
	int	*ret_code;						/* WANG ACP return code.				*/


	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	/********************************************************
	*	Receive variable number of arguments		*
	********************************************************/
	va_start(arg_stack);						/* Set pointer to top of stack.				*/
	arg_count = va_count(arg_stack);				/* How many args are there ?				*/

	va_start(arg_stack);						/* Go back to the top of the stack.			*/

	rel_line = va_arg(arg_stack, int*);				/* Get relative line from arg stack.			*/
	arg_count--;							/* One less argument.					*/

	length = va_arg(arg_stack, int*);				/* Get buffer length from arg stack.			*/
	arg_count--;							/* One less argument.					*/

	inarea = va_arg(arg_stack, char*);				/* Get pointer to buffer from arg stack.		*/
	arg_count--;							/* One less argument.					*/

	if (arg_count > 1)						/* If more than one argument left, then get acp_wait.	*/
	{
		acp_wait = va_arg(arg_stack, int*);			/* Get wait time from arg stack.			*/
		arg_count--;						/* One less argument.					*/
	}

	if (arg_count > 1)						/* If more than one argument left, get ws_interrupt.	*/
	{
		ws_interrupt = va_arg(arg_stack, char*);		/* Get ws_interrupt from arg stack.			*/
		arg_count--;						/* One less argument.					*/
	}

	ret_code = va_arg(arg_stack, int*);				/* Get pointer to return code from arg stack.		*/
	arg_count--;							/* One less argument.					*/

	*ret_code = 0;							/* Initialize the return value.				*/
}
#endif
#ifndef VMS	/* unix and MSDOS */
CHECKACP()
{
	return(0);
}
#endif

