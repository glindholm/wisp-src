static char copyright[]="Copyright (c) 1988-1997 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		pttype.c
**
**	Purpose:	To process the type for the linkage section and RUN keywords.
**
**	Routines:	p_link_kw()	Process link keywords: USING, STRING, INTEGER
**			save_link_var()	Save the variable in link_item.
**
**
**	History:
**			mm/dd/yy	Written by ...
**
*/

#define EXT extern

#include <stdio.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"

void p_link_kw(int tndx, int* num_link_var)						/* Process link keywords.		*/
{
	char *cstr, curlen[FLDLEN];

	*curlen = '\0';									/* Init the current length to not found.*/
	switch (tndx)
	{
		case USING:
		{
			write_log(util,'I','R',"PROCESS","Processing USING parameter keyword.");
			*num_link_var = 0;						/* Set count for linkage variables.	*/
			got_link = 1;							/* Turn link flag on.			*/
			break;								/* Get out of here.			*/
		}
		case STRING:
		{
			write_log(util,'I','R',"PROCESS","Processing STRING parameter keyword.");
			hld_link = cur_link;						/* Save ptr to end of link list.	*/
			while (cur_link && *cur_link->type == ' ')			/* While type not set init data.	*/
			{
				strcpy(cur_link->type,loc_str_typ);			/* Set value to String.			*/
				while (tststrt(aptr) && *aptr != '(') aptr++;		/* Move to next length token.		*/
				if (*aptr == ' ') nexttok();
				if (*aptr == '(')					/* See if a length was specified.	*/
				{
					cstr = cur_link->length;			/* Load picture clause.			*/
					while (tststrt(aptr))  *cstr++ = *aptr++;
					*cstr = '\0';					/* Null terminate the length.		*/
					strcpy(curlen,cur_link->length);		/* Save the current length.		*/
				}
				else if (curlen[0]) /* GSL 6/8/92 */
				{
					strcpy(cur_link->length,curlen);		/* Assign the current length.		*/
				}
				else
				{
					write_log(util,'E','R',"NOLEN","No length specified for %s.",cur_link->field1);
					strcpy(cur_link->length,len_one);		/* Else set length to 1.		*/
				}
				cur_link= cur_link->prev_item;
			}
			cur_link = hld_link;						/* Put ptr to end of link list.		*/
			break;
		}
		case INTEGER:
		{
			write_log(util,'I','R',"PROCESS","Processing INTEGER parameter keyword.");
			hld_link = cur_link;						/* Save ptr to end of link list.	*/
			while (cur_link && *cur_link->type == ' ')			/* While still have an item.		*/
			{
				strcpy(cur_link->type,loc_int_typ);			/* Set value to Integer.		*/
				cur_link = cur_link->prev_item; 			/* Set to the previous structure.	*/
			}
			cur_link = hld_link;						/* Set ptr to end of link list.		*/
			break;
		}
		default:
		{
			write_log(util,'W','R',"INVALID","Invalid procedure parameter keyword.");
			break;
		}
	}
}

void save_link_var(int* num_link_var)							/* Save the variable in link_item.	*/
{
	char *cstr;

	write_log(util,'I','R',"PROCESS","Processing a linkage section variable name.");
	if (*aptr == '&')								/* Check if variable name.		*/
	{
		init_link(num_link_var);						/* Get pointer to link item.		*/
		cstr = cur_link->field1;
		while (tststrt(aptr))
		{
			if (*aptr == '_') *aptr = '-';					/* Replcae underscore with dash.	*/
			else if (*aptr == '#') *aptr = '-';				/* Replcae # with dash.			*/
			*cstr++ = toupper(*aptr++);					/* Put in the variable name.		*/
		}
		*cstr = '\0';								/* Null terminate the string.		*/
	}
	else if (*aptr == '(')								/* Check if variable name.		*/
	{
		hld_link = cur_link;							/* Save ptr to end of link list.	*/
		while (cur_link && *cur_link->length != '(' && *cur_link->type != 'I')	/* While type not set init data.	*/
		{
			cstr = cur_link->length;
			while (*aptr != ')') *cstr++ = *aptr++;				/* Load COBOL picture clause.		*/
			*cstr++ = *aptr++;						/* Copy the closing parenthesis.	*/
			*cstr = '\0';							/* Null Terminate the length.		*/
			cur_link = cur_link->prev_item; 				/* Set to the previous node.		*/
		}
		cur_link = hld_link;							/* Put ptr to end of link list.		*/
	}
}
/*
**	History:
**	$Log: pttype.c,v $
**	Revision 1.5  1997/04/21 15:22:45  scass
**	Corrected copyright.
**	
**	Revision 1.4  1996-09-12 19:18:08-04  gsl
**	Fix prototypes
**
**
**
*/
