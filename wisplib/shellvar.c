			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include "idsistd.h"
#ifdef MSDOS
#include <stdlib.h>
#include <string.h>
#endif

/*
**	shellvar.c	This routine returns a pointer to a string that contains the current shell to use.
**			This value comes from the variable SHELL or defaults to /bin/sh.
*/


char *shell_var()
{
	static	int	first = 1;
	static	char	*ptr, sh_path[80];

	if (first)
	{
		first = 0;
		if (!(ptr = (char *)getenv("SHELL")))
		{
			ptr = "/bin/sh";
		}
		else
		{
			if (!*ptr)
				ptr = "/bin/sh";
		}
		strcpy(sh_path,ptr);
	}
	return( &sh_path[0] );
}

