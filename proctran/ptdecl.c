static char copyright[]="Copyright (c) 1995-97 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		ptdecl.c
**
**	Project:	wisp/proctran
**
**	RCS:		$Source:$
**
**	Purpose:	To process the DECLARE statement.
**
**	Routines:	
**	p_declare()		Initial processing of the DECLARE statement:
**				STRING, INTEGER, INITIAL, GLOBAL keywords.
**	save_declare_var()	Save the variable in declare_item.
*/

/*
**	Includes
*/
#define EXT extern

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

/*
**	ROUTINE:	p_declare()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void p_declare(int tndx,int *num_val,int *num_var,int *num_asn)
{
	register int i;
	char *cstr, curlen[FLDLEN];
	int pos;

	*curlen = '\0';									/* Init the current length to not found.*/
	switch (tndx)
	{
		case STRING:
		{
			write_log(util,'I','R',"PROCESS","Processing STRING declare keyword.");
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
					while (tststrt(aptr))
					{
						if (*aptr == '*')
						{					/* If defined &func as an initial.	*/
							if ((pos=strpos(aptr,"&DATE")) >= 0) *aptr = '6';
							else if ((pos=strpos(aptr,"&TIME")) >= 0) *aptr = '8';
							else
							{
								write_log(util,'E','R',"NOINITIAL",
									"Invalid INITIAL of %s.",cur_decl->field1);
								strcpy(cur_decl->length,len_one);
								break;
							}
						}

						*cstr++ = *aptr++;
					}
					*cstr = '\0';					/* Null terminate the length.		*/
					strcpy(curlen,cur_decl->length);		/* Save the current length.		*/
				}
				else if (curlen[0]) /* GSL 6/8/92 */
				{
					strcpy(cur_decl->length,curlen);		/* Assign the current length.		*/
				}
				else
				{
					write_log(util,'W','R',"NOLENGTH",
						"No string length supplied.  Verify string length.");
					strcpy(cur_decl->length,len_one);		/* Else set length to 1.		*/
				}
				cur_decl = cur_decl->prev_item;
			}
			cur_decl = hld_table;						/* Set ptr to end of declare list.	*/
			break;
		}
		case INTEGER:
		{
			write_log(util,'I','R',"PROCESS","Processing INTEGER declare keyword.");
			hld_table = cur_decl;						/* Save ptr to end of declare list.	*/
			if ((pos=strpos(cur_decl->field1,"KEY") >= 0))			/* If defined PFKEY as an integer,	*/
			{								/*  need to convert to string.		*/
				write_log(util,'W','R',"PROCESS",
                                            "Processing %s as INTEGER.  Convert to STRING.",cur_decl->field1);
				*cur_decl->type = 'S';					/* Set value to String.			*/
				strcpy(cur_decl->length,len_two);			/* Set length to 2.			*/
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
			write_log(util,'I','R',"PROCESS","Processing INITIAL declare keyword.");
			nexttok();							/* Step to first value			*/
			if (!tststrt(aptr))						/* If no more tokens on line.		*/
			{
				get_next_line();
				if (*aptr == ' ') nexttok();				/* Move to first token on line.		*/
			}
			if (*cur_decl->type == 'S' && *aptr == '\'') aptr++;		/* Step past quote for a string type.	*/

			if (*aptr == '&')						/* If have and INTIAL with a function.	*/
			{
				write_log(util,'I','R',"PROCESS","Processing AND sign for DECLARE with INITIAL.");
				strcpy(cur_decl->value,"\0");				/* Init the value of the variable.	*/

				init_assign(num_asn);					/* Create an ASSIGN for declare var.	*/
				strcpy(cur_assign->field1,cur_decl->field1);		/* Set the assign variable.		*/
				strcpy(cur_assign->type,cur_decl->type);
				get_type_len(cur_assign->field1,cur_assign->length,num_var,num_val,'L');

				init_assign(num_asn);
				cstr = cur_assign->field1;				/* Get the assign variable.		*/
				while(tststrt(aptr) && *aptr != '=' && *aptr != '(' && !operand(*aptr))
				{
					if (*aptr == '_') *aptr = '-';			/* Replace the underscore with dash.	*/
					else if (*aptr == '#') *aptr = '-';		/* Replace the # with dash.		*/
					*cstr++ = toupper(*aptr++);			/* Copy Variable.			*/
				}
				*cstr = '\0';						/* Null the string.			*/
				get_type_len(cur_assign->field1,cur_assign->type,num_var,num_val,'T');	 /* Get type of str.	*/
				get_type_len(cur_assign->field1,cur_assign->length,num_var,num_val,'L'); /* Get length of str.	*/
				next_ptr = aptr;
			}
			else if (*aptr != '\0' && *aptr != LNFD)			/* If value string and not eol.		*/
			{
				char tval[100];

				i = 0;							/* Get the value string.		*/
				while ( (*cur_decl->type == 'S' && *aptr != '\'') ||
					(*cur_decl->type != 'S' && tststrt(aptr)) ) 
				{
					if (*aptr == '"') *aptr = '\'';			/* Set quotes to correct form.		*/
					tval[i++] = *aptr++;
					if (*cur_decl->type == 'S' &&
					    (*aptr == '\0' || *aptr == LNFD || (aptr - linein >= WPLMAX)) )
					{						/* At end of line but might have more.	*/
						if ((pos=strpos(cur_decl->field1,"KEY") >= 0)) break; /* If the PFKEY value.	*/
						else	get_next_line();
					}
					if ((pos=strpos(cur_decl->field1,"KEY") >= 0) && *aptr == ' ') break;
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
			else 	write_log(util,'E','R',"NOVAL","No value assignment for INITIAL.");
			break;
		}
		case GLOBAL:
		{
			write_log(util,'I','R',"PROCESS","Processing GLOBAL declare keyword.");
			in_global = TRUE;
			break;
		}
		default:
		{
			write_log(util,'W','R',"INVALID","Invalid Declare keyword.");
			break;
		}
	}										/* End declare switch.			*/
}

/*
**	ROUTINE:	save_declare_var()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void save_declare_var(int *num_asn,int *num_var,int *num_val, int *num_cmds)		/* Save the variable in declare_item.	*/
{
	char *cstr;
	char fielda[FLDLEN], fieldb[FLDLEN], type[3];
	int found, cont, pos;

	write_log(util,'I','R',"PROCESS","Processing a declare variable name.");

	found = FALSE;
	cstr = fieldb;
	while (tststrt(aptr))
	{
		if (*aptr == '_') *aptr = '-';						/* Change all underscores to dashes.	*/
		else if (*aptr == '#') *aptr = '-';					/* Change all # to dashes.		*/
		*cstr++ = toupper(*aptr++);						/* Put in the variable name.		*/
	}
	*cstr = '\0';									/* Null terminate.			*/

	hld_table = cur_decl;								/* Keep track of starting declare posn.	*/
	while (cur_decl)								/* Till beginning of the list.		*/
	{
		strcpy(fielda,cur_decl->field1);					/* Copy declare field to local variable.*/
		go_upper(fielda);							/* Upcase the name.			*/

		if (!strcmp(fielda,fieldb))						/* If declare fld match.		*/
		{
			found = TRUE;							/* Found a match, already declared.	*/
			strcpy(type,cur_decl->type);					/* Save the type for later.		*/
			break;
		}
		cur_decl = cur_decl->prev_item;						/* Point to the previous one.		*/
	}
	cur_decl = hld_table;								/* Set ptr back.			*/

	if (!found)									/* No match so add to list.		*/
	{
		init_current(num_var);							/* Alloc next node for declare.		*/
		(*num_val)++;								/* Keep track of init stmnt role back.	*/
		strcpy(cur_decl->field1,fieldb);
	}
	else										/* Check if has INITIAL keyword.	*/
	{
		if ( (pos=strpos(aptr,"INITIAL")) >= 0 || (pos=strpos(aptr,"initial")) >= 0  )
		{
			write_log(util,'I','R',"PROCESS","Processing multiple DECLARE with INITIAL keyword.");
			setflag();							/* Turn flags off.			*/
			*num_asn = 0;							/* Count number of variables in field.	*/
			in_assign = 1;							/* In the assign clause.		*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			init_assign(num_asn);
			strcpy(cur_assign->field1,fieldb);				/* Get the assign variable.		*/
			aptr += (pos+7);						/* Set ptr to INITIAL keyword.		*/
			while (*aptr == ' ') aptr++;					/* Step to value.			*/
			init_assign(num_asn);
			cstr = cur_assign->field1;
			strcpy(cur_assign->type,type);					/* Set the type for the ASSIGN field.	*/
			if (*aptr == '\'')
			{
				aptr++;							/* Step past the open quote.		*/
				if (*aptr == '\0' || *aptr == LNFD)			/* If nothing between quotes then	*/
				{							/* save as a NULL.			*/
					cont = FALSE;
				}
				else 	cont = TRUE;

				while (cont)
				{
					while(*aptr != '\0' && *aptr != LNFD && *aptr != '\'' && (aptr - linein < WPLMAX))
					{
						if (*aptr == '"') *aptr = '\'';		/* Set quote withing string to single.	*/
						*cstr++ = *aptr++;
					}
					if (*aptr != '\'')				/* If not the end of string,		*/
					{						/* then read next line.			*/
						get_next_line();
					}
					else cont = FALSE;
				}
				*cstr = '\0';						/* Null the string.			*/
				aptr++;							/* Step past the closing quote.		*/
			}
			else	while(tststrt(aptr)) *cstr++ = toupper(*aptr++); 	/* Copy value.				*/

			next_ptr = aptr;						/* Set so finds next token properly.	*/
			if (*aptr == ' ') nexttok();					/* Set up for next item.		*/
		}
		else
		{
			setflag();
			write_log(util,'I','R',"DECLARE"," Multiple DECLARE of (%s).  Rest of line not processed.",fieldb);
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
		}
	}
}

/*
**	History:
**	$Log: ptdecl.c,v $
**	Revision 1.8  1999/09/13 19:50:20  gsl
**	fix "==" vs. "="
**	
**	Revision 1.7  1997-04-21 11:05:19-04  scass
**	Corrected copyright.
**
**	Revision 1.6  1996-09-12 19:15:07-04  gsl
**	fix return codes
**
**	Revision 1.5  1995-09-22 07:22:42-07  scass
**	Added DTMI standard header stuff.
**
**
**
*/
