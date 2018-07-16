			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/
#ifdef MSDOS
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "idsistd.h"

/*
	setenvstr		Set an environment string
*/

int setenvstr(envstring)
char	*envstring;
{
	int	msize;
	char	*env_ptr;

	msize = strlen(envstring)+1;
	env_ptr = (char *)malloc( msize );
	if( ! env_ptr )
	{
		return(1);
	}

	strcpy(env_ptr, envstring);
	if ( putenv(env_ptr) )
	{
		return(2);
	}

	return(0);
}

