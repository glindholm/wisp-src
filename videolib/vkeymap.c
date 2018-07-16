/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		vkeymap.c
**
**	Purpose:	Routines to provide keymapping.
**
**	Routines:	
**	VL_vkeymap()	Retrieve the metakey value based on component and function.
**	VL_vkeymap_path()	Generate the native path to the vkeymap file.
**	strtometakey()	Convert vkeymap key string into a metakey.
**
**
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#ifdef unix
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
#endif

#include "vkeymap.h"
#include "vintdef.h"
#include "vmodules.h"

static int4 strtometakey();
static char *upstring(char *string);


/*
**	Routine:	VL_vkeymap()
**
**	Function:	Retrieve the metakey value based on component and function.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	component	The system component (uppercase). E.g. "UNIQUE"
**	function	The function within the component (uppercase).  E.g. "MODIFY"
**	rc_metakey	The metakey value returned.
**
**	Globals:	None
**
**	Return:
**	0		Found function requested.
**	1		Component+function not found.
**	-1		Keymap file not found.
**	-2		keymap file not readable.
**
**	Warnings:	None
**
**	History:	
**	08/11/93	Written by GSL
**
*/
int VL_vkeymap(component, function, rc_metakey)
char	*component;
char	*function;
int4	*rc_metakey;
{
#define MAXMAPSTRING	25
	struct vkeymap_struct
	{
		struct vkeymap_struct *next;
		char	component[MAXMAPSTRING];
		char	function[MAXMAPSTRING];
		int4	metakey;
	};
	static	struct vkeymap_struct	*vkeymap_head = NULL;
	struct vkeymap_struct 	*vkeymap_curr;
	char	l_component[80], l_function[80], l_key[80];
	char	buff[256];
	int	rc;

	*rc_metakey = KM_UNKNOWN;

	/*
	**	First time in load the linked list.
	*/
	if (!vkeymap_head)
	{
		char	path[256];
		FILE	*fh;

		if ( 0 != access(VL_vkeymap_path(path),0))
		{
			/*
			**	VKEYMAP not found.
			*/
			return(-1);
		}

		fh = fopen(path,"r");
		if (!fh)
		{
			/*
			**	Open of VKEYMAP failed.
			*/
			return(-2);
		}

		/*
		**	Preallocate one item on list.
		*/
		vkeymap_head = (struct vkeymap_struct *)malloc(sizeof(struct vkeymap_struct));
		vkeymap_curr = vkeymap_head;

		/*
		**	Read through the VKEYMAP file adding items to end of list.
		*/
		while(fgets(buff,sizeof(buff),fh))
		{
			/*
			**	All invalid items are ignored without warning.
			**		- leading '#' is a comment
			**		- must have 3 items on a line
			**		- items too long
			*/
			if ('#' == buff[0]) continue;

			rc = sscanf(buff, "%s %s %s", l_component, l_function, l_key);
			if (3 != rc) continue;

			if (strlen(l_component) >= MAXMAPSTRING) continue;
			if (strlen(l_function) >= MAXMAPSTRING) continue;

			/*
			**	Found a valid item, shift tp uppercase and load up the list item.
			*/
			upstring(l_key);
			upstring(l_component);
			upstring(l_function);

			vkeymap_curr->metakey = strtometakey(l_key);
			strcpy(vkeymap_curr->component, l_component);
			strcpy(vkeymap_curr->function, l_function);

			/*
			**	Preallocate the next list item and link down to it.
			*/
			vkeymap_curr->next = (struct vkeymap_struct *)malloc(sizeof(struct vkeymap_struct));
			vkeymap_curr = vkeymap_curr->next;
		}

		/*
		**	Tidy up the unused list item and terminate the linked list.
		*/
		vkeymap_curr->next = NULL;
		vkeymap_curr->component[0] = (char)0;
		vkeymap_curr->function[0] = (char)0;
		vkeymap_curr->metakey = KM_UNKNOWN;

		fclose(fh);
	}

	/*
	**	Scan through the linked list for an item with a matching component
	**	and function.  If found return the metakey value.
	*/
	for(vkeymap_curr = vkeymap_head; vkeymap_curr; vkeymap_curr = vkeymap_curr->next)
	{
		if (0==strcmp(vkeymap_curr->component, component) &&
		    0==strcmp(vkeymap_curr->function, function)      )
		{
			*rc_metakey = vkeymap_curr->metakey;
			return(0);
		}
	}

	/*
	**	Return item not found.
	*/
	*rc_metakey = KM_UNKNOWN;
	return(1);
}

/*
**	Routine:	VL_vkeymap_path()
**
**	Function:	Generate the native path to the vkeymap file.
**
**	Description:	For VMS this calls vinfoname().
**			For unix and MSDOS this 
**				Use $VKEYMAP if set
**				Else use $HOME/.vkeymap if present
**				Else use vinfoname()
**
**	Arguments:
**	path		The generated path.
**
**	Globals:	None
**
**	Return:		path.
**
**	Warnings:	None
**
**	History:	
**	08/11/93	Written by GSL
**
*/
char *VL_vkeymap_path(path)
char	*path;
{
	char	*ptr;

	path[0] = (char)0;

	if ((ptr = getenv("VKEYMAP")))
	{
		/*
		**	If $VKEYMAP is set then it overrides all others.
		*/
		strcpy(path,ptr);
	}
	else if ((ptr = getenv("HOME")))
	{
		/*
		**	Check for a "personal" $HOME vkeymap file.
		*/

		VL_vbldfilepath(path,ptr,VKEYMAP_HOME_FILE);
		if ( 0 != access(path,0))
		{
			/* 
			**	The "personal" vkeymap file was not found.
			*/
			path[0] = (char)0;
		}
	}

	if (!path[0])
	{
		char *VL_vinfoname();

		strcpy(path, VL_vinfoname(VKEYMAP_FILE));
	}
	return(path);
}

/*
**	Routine:	strtometakey()
**
**	Function:	Convert vkeymap key string into a metakey.
**
**	Description:	First check for function keys F0 - F63 then special keys.
**			An unrecognized key string will return KM_UNKNOWN.
**			The function key metakey values are generated by oring the numerical
**			value with the KM_FUNCTION bit mask.  The special keys are all
**			listed in the tanslation table.
**
**	Arguments:
**	key		The vkeymap key string (uppercase).
**
**	Globals:	None
**
**	Return:		The metakey.
**
**	Warnings:	None
**
**	History:	
**	08/11/93	Written by GSL
**
*/
static int4 strtometakey(key)
char	*key;
{
	int	i;
	static struct
	{
		char	*keystring;
		int4	metakey;
	} trantable[] = 
		{
			{"X",		KM_NONE},
			{"UP",		KM_UP},
			{"DOWN",	KM_DOWN},
			{"LEFT",	KM_LEFT},
			{"RIGHT",	KM_RIGHT},
			{"ENTER",	KM_ENTER},
			{"PAGEUP",	KM_PAGEUP},
			{"PAGEDOWN",	KM_PAGEDOWN},
			{"HOME",	KM_HOME},
			{"END",		KM_END},
			{"INSERT",	KM_INSERT},
			{"DELETE",	KM_DELETE},
			{"BACKSPACE",	KM_BACKSPACE},
			{NULL,		0}
		};

	/*
	**	First look for function keys.
	*/
	if ('F' == key[0] && isdigit((int)key[1]))
	{
		i = atoi(&key[1]);
		if (i < 0 || i > 63)
		{
			return(KM_UNKNOWN);
		}

		/*
		**	Generate the metakey by oring the numerical value with the function bit mask.
		*/
		return( i | KM_FUNCTION_MASK );
	}

	/*
	**	Look for a match in the translation table.
	*/
	for(i=0; trantable[i].keystring; i++)
	{
		if (0==strcmp(key, trantable[i].keystring))
		{
			return(trantable[i].metakey);
		}
	}
	return(KM_UNKNOWN);
}

static char *upstring(char *string)
{
	char	*ptr;
	for (ptr=string; *ptr; ptr++)
	{
		*ptr = toupper(*ptr);
	}
	return( string );
}

#ifdef MAIN
main(argc, argv)
int	argc;
char	*argv[];
{
	int4	metakey;
	int	rc;
	char	path[256];

	if (argc != 3)
	{
		printf("Usage: vkeymap {component} {function}\n");
		exit(0);
	}

	printf("VL_vkeymap_path=[%s]\n",VL_vkeymap_path(path));

	rc = VL_vkeymap(argv[1], argv[2], &metakey);

	printf("%d = VL_vkeymap(\"%s\", \"%s\", 0x%08x)\n", rc, argv[1], argv[2], metakey);
	exit(0);
}
#endif /* MAIN */
/*
**	History:
**	$Log: vkeymap.c,v $
**	Revision 1.14  2003/06/20 15:04:28  gsl
**	VL_ globals
**	
**	Revision 1.13  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.12  2003/01/31 20:35:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.11  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/18 21:04:22  gsl
**	Remove MSDOS code
**	
**	Revision 1.9  2002/07/15 20:56:38  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.8  2002/07/15 20:16:09  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.7  1996/10/11 22:16:07  gsl
**	drcs update
**	
**
**
*/
