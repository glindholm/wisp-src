#define EXT extern
			/************************************************************************/
			/*	   PROCTRAN - Wang Procedure Language to VS COBOL Translator	*/
			/*			Copyright (c) 1990				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/
/* PG_DECLARE.C	*/

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"

p_declare(tndx,num_val,num_var)
int tndx, *num_val, *num_var;
{
	register int i;
	char *cstr, curlen[FLDLEN];

	*curlen = '\0';									/* Init the current length to not found.*/
	switch (tndx)
	{
		case STRING:
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing STRING declare keyword.");
			hld_table = cur_decl;						/* Save ptr to end of declare list.	*/
			while (cur_decl && *cur_decl->type == ' ')			/* While still have a declare item.	*/
			{
				*cur_decl->type = 'S';					/* Set value to String.			*/
				if (in_global) cur_decl->type[1] = 'G';			/* Set the global type indicator.	*/
				while (tststrt(aptr) && *aptr != '(') aptr++;		/* Move to next length token.		*/
				if (*aptr == ' ') nexttok();
				if (*aptr == '(')					/* See if a length was specified.	*/
				{
					cstr = cur_decl->length;			/* Load picture clause.			*/
					while (tststrt(aptr))  *cstr++ = *aptr++;
					*cstr = '\0';					/* Null terminate the length.		*/
					strcpy(curlen,cur_decl->length);		/* Save the current length.		*/
				}
				else if (curlen[0]) /* GSL 6/8/92 */
				{
					strcpy(cur_decl->length,curlen);		/* Assign the current length.		*/
				}
				else
				{
					write_log("PROCTRAN",'W',"NOLENGTH","No string length supplied.  Verify string length.");
					strcpy(cur_decl->length,"(1)");			/* Else set length to 1.		*/
				}
				cur_decl = cur_decl->prev_item;
			}
			cur_decl = hld_table;						/* Set ptr to end of declare list.	*/
			break;
		}
		case INTEGER:
		{
			int i;

			write_log("PROCTRAN",'I',"PROCESS","Processing INTEGER declare keyword.");
			hld_table = cur_decl;						/* Save ptr to end of declare list.	*/
			if ((i=strpos(cur_decl->field1,"KEY") >= 0))			/* If defined PFKEY as an integer,	*/
			{								/*  need to convert to string.		*/
				write_log("PROCTRAN",'W',"PROCESS",
                                            "Processing %s as INTEGER.  Convert to STRING.",cur_decl->field1);
				*cur_decl->type = 'S';					/* Set value to String.			*/
				write_log("PROCTRAN",'I',"VERIFY","Verify if line processed correctly.");
				strcpy(cur_decl->length,"(2)");				/* Set length to 2.			*/
				cur_decl = cur_decl->prev_item;				/* Step to prev to set any more types.	*/
			}
			while (cur_decl && *cur_decl->type == ' ')
			{
				*cur_decl->type = 'I';					/* Set value to Integer.		*/
				if (in_global) cur_decl->type[1] = 'G';			/* Set the global type indicator.	*/
				cur_decl = cur_decl->prev_item;				/* Set to the previous structure.	*/
			}
			cur_decl = hld_table;						/* Put the pointer bake if multiple.	*/
			break;
		}
		case INIT:
		case INITIAL:
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing INITIAL declare keyword.");
			nexttok();							/* Step to first value			*/
			if (!tststrt(aptr))						/* If no more tokens on line.		*/
			{
				get_next_line();
				if (*aptr == ' ') nexttok();				/* Move to first token on line.		*/
			}
			if (*cur_decl->type == 'S' && *aptr == '\'') aptr++;		/* Step past quote for a string type.	*/
			if (*aptr != '\0' && *aptr != LNFD)				/* If value string and not eol.		*/
			{
				char tval[100];

				i = 0;							/* Get the value string.		*/
				while ((*cur_decl->type == 'S' && *aptr != '\'') || (*cur_decl->type != 'S' && tststrt(aptr)) ) 
				{
					tval[i++] = *aptr++;
					if (*cur_decl->type == 'S' && (*aptr == '\0' || *aptr == LNFD || (aptr - inline >= WPLMAX)))
					{						/* At end of line but still have more.	*/
						get_next_line();
					}
				}
				tval[i] = '\0';						/* Null terminate the string.		*/
				if (*aptr == '\'') aptr++;				/* Step past closing quote.		*/
				next_ptr = aptr;					/* Set so processsing is correct.	*/

				hld_table = cur_decl;					/* Save the current ptr to the table.	*/
				for (i = 0; i < *num_val; i++)				/* Init the value for each var on line.	*/
				{
					strcpy(cur_decl->value,tval);			/* Init the value of the variable.	*/
					cur_decl = cur_decl->prev_item;			/* Set to the previous structure.	*/
				}
				*num_val = 0;						/* reset to no values.			*/
				cur_decl = hld_table;					/* Set ptr to end of declare list.	*/
			}
			else 	write_log("PROCTRAN",'E',"NOVAL","No value assignment for INITIAL.");
			break;
		}
		case GLOBAL:
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing GLOBAL declare keyword.");
			in_global = TRUE;
			break;
		}
		default:
		{
			write_log("PROCTRAN",'W',"INVALID","Invalid Declare keyword.");
			break;
		}
	}									/* End declare switch.				*/
}

save_declare_var(num_val,num_var)							/* Save the variable in declare_item.	*/
int *num_val, *num_var;
{
	char *cstr;

	write_log("PROCTRAN",'I',"PROCESS","Processing a declare variable name.");
	init_current(num_var);								/* Alloc next node for declare.		*/
	(*num_val)++;									/* Keep track of init stmnt role back.	*/
	cstr = cur_decl->field1;
	while (tststrt(aptr))
	{
		if (*aptr == '_') *aptr = '-';						/* Change all underscores to dashes.	*/
		*cstr++ = toupper(*aptr++);						/* Put in the variable name.		*/
	}
	*cstr = '\0';									/* Null terminate.			*/
}
