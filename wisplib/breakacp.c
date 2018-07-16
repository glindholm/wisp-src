#ifdef VMS
/********************************************************************************************************************************
*																*
*	breakacp.c	This program always returns 0 for success.  It is not necessary to send break sequences on		*
*			the VAX.												*
*			input:	rel_line	relative line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*			output:	ret_code	WANG 4-byte binary format return code (wswap an int) -- always 0 here.		*
*																*
********************************************************************************************************************************/

#include "werrlog.h"

breakacp(rel_line,ret_code)

long	*rel_line;							/* Index from 1 to 6 for later access to acp_term[].	*/
long	*ret_code;							/* WANG ACP return code.				*/

{
#define		ROUTINE		5000

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	*ret_code = 0;							/* Initialize the return value.				*/
}
#endif
#ifdef unix
BREAKACP()
{
	return;
}
#endif
#ifdef MSDOS
BREAKACP()
{
	return(0);
}
#endif
