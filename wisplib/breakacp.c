static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/********************************************************************************************************************************
*																*
*	breakacp.c	This program always returns 0 for success.  It is not necessary to send break sequences on		*
*			the VAX.												*
*			input:	rel_line	relative line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*			output:	ret_code	WANG 4-byte binary format return code (wswap an int) -- always 0 here.		*
*																*
********************************************************************************************************************************/

#include "idsistd.h"
#include "werrlog.h"

#ifdef VMS
void BREAKACP(rel_line,ret_code)

int4	*rel_line;							/* Index from 1 to 6 for later access to acp_term[].	*/
int4	*ret_code;							/* WANG ACP return code.				*/

{
#define		ROUTINE		5000

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	*ret_code = 0;							/* Initialize the return value.				*/
}
#endif

#if defined(unix)
void BREAKACP(int4* rel_line,int4 *ret_code)
{
	return;
}
#endif
/*
**	History:
**	$Log: breakacp.c,v $
**	Revision 1.10  1996/08/19 22:32:10  gsl
**	drcs update
**	
**
**
*/
