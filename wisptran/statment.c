static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		statment.c
**
**	Purpose:	To ...
**
**	Routines:	
**	get_statement()		Load a statement
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#include <string.h>
#include "token.h"
#include "node.h"
#include "lines.h"
#include "statment.h"
#include "tokenize.h"
#include "proto.h"
#include "ring.h"

/*
**	Routine:	get_statement()
**
**	Function:	To load the next statement.
**
**	Description:	This routine builds a node tree representation of the next statement.
**			The node tree begins with a START node and ends with an END node.
**			Significant tokens follow the next node list. Fluff tokens are tied
**			on the down list. The nodes are meant to be traversed in self-down-next
**			order.
**			It uses get_token() to get the next token until a PERIOD is found then
**			it checks the token_cache and if the rest of the line is fluff loads it.
**
**			Special case for IDCOMMENT: An IDCOMMENT makes and terminates a statement.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		The START node of the loaded statement or NULL if no tokens loaded.
**
**	Warnings:	None
**
**	History:	
**	05/19/93	Written by GSL
**	06/01/93	Added IDCOMMENT special case. GSL
**
*/
NODE get_statement(void)
{
	NODE	start_node, curr_node, temp_node;
	TOKEN	*tokptr;
	int	token_count;

	start_node = makenode(NODE_START,NULL,NULL,NULL);
	curr_node  = start_node;
	token_count = 0;

	for(;;)
	{
		tokptr = get_token();

		if (!tokptr) break;
		token_count++;

		if (fluff_token(tokptr))
		{
			tie_bottom(curr_node,maketoknode(tokptr));
		}
		else
		{
			curr_node->next = maketoknode(tokptr);
			curr_node = curr_node->next;

			if (PERIOD == tokptr->type)
			{
				int	tokcnt, fluff, cache_index;

				tokcnt = token_cache_count();
				fluff = 1;

				for(cache_index = 0; cache_index<tokcnt; cache_index++)
				{
					TOKEN	tmptok;

					token_cache_get(cache_index, &tmptok);
	
					if (!fluff_token(&tmptok))
					{
						fluff = 0;
						break;
					}
				}

				while(tokcnt && fluff)
				{
					tokptr = get_token();
					temp_node = maketoknode(tokptr);
					temp_node->down = curr_node->down;
					curr_node->down = temp_node;
					tokcnt--;
				}

				break;
			}
			else if (IDCOMMENT == tokptr->type)
			{
				break;
			}
		}
	}

	if (0==token_count)
	{
		wfree(start_node);
		return(NULL);
	}

	curr_node->next = makenode(NODE_END,NULL,NULL,NULL);

	debug4_print_statement(start_node);

	return start_node;
}

/*
**	Routine:	get_verb_statement()
**
**	Function:	To load the next verb statement.
**
**	Description:	This routine builds a node tree representation of the next verb statement.
**			The node tree begins with a START node and ends with an END node.
**			Significant tokens follow the next node list. Fluff tokens are tied
**			on the down list. The nodes are meant to be traversed in self-down-next
**			order.
**			It uses get_token() to get the next token until a PERIOD or a VERB is found then
**			it checks the token_cache and if the rest of the line is fluff loads it.
**
**			Translate	FAC OF identifier		-> F-identifier
**					ORDER-AREA OF identifier	-> O-A-identifier
**
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		The START node of the loaded statement or NULL if no tokens loaded.
**
**	Warnings:	None
**
**	History:	
**	06/11/93	Written by GSL
**	06/12/93	Added "FAC OF" and "ORDER-AREA OF" translation. GSL
**
*/
NODE get_verb_statement(void)
{
	NODE	start_node, curr_node, temp_node;
	TOKEN	*tokptr;
	int	token_count;
	int	first_sig;

	start_node = makenode(NODE_START,NULL,NULL,NULL);
	curr_node  = start_node;
	token_count = 0;
	first_sig = 1;

	for(;;)
	{
		tokptr = get_token();

		if (!tokptr) break;
		token_count++;

		if (fluff_token(tokptr))
		{
			tie_bottom(curr_node,maketoknode(tokptr));
		}
		else
		{
			if (!first_sig)
			{
				if (VERB==tokptr->type ||
				    eq_token(tokptr,KEYWORD,"ELSE") || 
				    eq_token(tokptr,KEYWORD,"WHEN") || 
				    (KEYWORD==tokptr->type && 0==memcmp(tokptr->data,"END-",4)))
				{
					/*
					**	Statement ends when we see another VERB or an END-IF, END-PERFORM etc...
					*/
					unget_token(tokptr);
					break;
				}
			}

			first_sig = 0;

			curr_node->next = maketoknode(tokptr);
			curr_node = curr_node->next;

			if (PERIOD == tokptr->type)
			{
				int	tokcnt, fluff, cache_index;

				tokcnt = token_cache_count();
				fluff = 1;

				for(cache_index = 0; cache_index<tokcnt; cache_index++)
				{
					TOKEN	tmptok;

					token_cache_get(cache_index, &tmptok);
	
					if (!fluff_token(&tmptok))
					{
						fluff = 0;
						break;
					}
				}

				while(tokcnt && fluff)
				{
					tokptr = get_token();
					temp_node = maketoknode(tokptr);
					temp_node->down = curr_node->down;
					curr_node->down = temp_node;
					tokcnt--;
				}

				break;
			}
		}
	}

	if (0==token_count)
	{
		wfree(start_node);
		return(NULL);
	}

	curr_node->next = makenode(NODE_END,NULL,NULL,NULL);

	/*
	**	Loop thru the statement and perform any post-statement modifications.
	*/
	for(curr_node=start_node->next; NODE_END != curr_node->type; curr_node = curr_node->next)
	{
		char	buff[80];
		int	is_fac, is_oa;

		is_fac = is_oa = 0;

		if ((is_fac = eq_token(curr_node->token,KEYWORD,"FAC"))       ||
		    (is_oa  = eq_token(curr_node->token,KEYWORD,"ORDER-AREA"))  )
		{
			/*
			**	-[FAC]-[OF]-[identifier]-
			**	-[ORDER-AREA]-[OF]-[identifier]-
			**
			**	1) Unlink and delete the "OF" node.
			**	2) Replace the "FAC" node token with a "F-identifier" token
			**	3) Move the identifer fluff onto the old "FAC" node.
			**	4) Unlink and delete the old identifier node
			*/

			temp_node = curr_node->next;				/* Point to the "OF" node			*/
			if (!eq_token(temp_node->token,KEYWORD,"OF"))
			{
				if (is_fac)
				{
					write_tlog(temp_node->token, "WISP",'E',"FAC-OF",
						"Error parsing FAC OF, expecting OF found [%s]",
						token_data(temp_node->token));
				}
				else
				{
					write_tlog(temp_node->token, "WISP",'E',"ORDER-AREA-OF",
						"Error parsing ORDER-AREA OF, expecting OF found [%s]",
						token_data(temp_node->token));
				}
			}
			else
			{
				curr_node->next = temp_node->next;		/* Unlink the "OF" node				*/
				temp_node->next = NULL;
				free_statement(temp_node);			/* Delete the "OF" node				*/
			}

			temp_node = curr_node->next;				/* Point to the identifier node			*/
			if (is_fac)
			{
				make_fac(buff,token_data(temp_node->token));	/* Create a F-identifier			*/
			}
			else
			{
				make_oa(buff,token_data(temp_node->token));	/* Create a A-O-identifier			*/
			}

			edit_token(curr_node->token,buff);			/* Replace the "FAC" node with "F-identifier"	*/
			curr_node->token->type = IDENTIFIER;

			if (!curr_node->down)					/* Move the fluff to the new node		*/
			{
				curr_node->down = temp_node->down;
			}
			else
			{
				curr_node->down->next = temp_node->down;
			}
			temp_node->down = NULL;

			curr_node->next = temp_node->next;			/* Unlink the old identifier			*/
			temp_node->next = NULL;
			free_statement(temp_node);				/* Delete the old identifier node		*/
		}
	}

	debug4_print_statement(start_node);

	return start_node;
}

/*
**	Routine:	make_statement()
**
**	Function:	To create a token/node statement from a line.
**
**	Description:	Take a character string line and tokenize it then
**			build a statement from the tokens.
**
**	Arguments:
**	line		The input line, it must be a complete line starting a column 1.
**	the_context	The token context to use.
**
**	Globals:	None
**
**	Return:		The node statement.
**
**	Warnings:	Don't use this in the identification division.
**			If line contains a "PICTURE" token than it must also contain the picture.
**
**	History:	
**	05/26/93	Written by GSL
**
*/
NODE make_statement(char *line, void *the_context)
{
	static	int	first = 1;
	static	char	*private_cache;
	static	int	do_reswords = 0;
	int	iddiv, iddiv_mode;
	int	tokcnt;

	NODE	the_statement, curr_node;
	int	rc;

	if (first)
	{
		first = 0;
		if (rc = ring_open(&private_cache, sizeof(TOKEN), 50, 5, NULL, 0))
		{
			write_log("WISP",'F',"RINGOPEN","Unable to open ring [private_cache] rc=%d [%s]",rc,ring_error(rc));
			exit_with_err();
		}
	}

	iddiv = iddiv_mode = 0;
	rc = tokenize_cobol_line(private_cache, line, NORMAL_LINE, &iddiv, &iddiv_mode, 0, 0, the_context, 
				is_dpcomma(), do_reswords);

	the_statement = makenode(NODE_START,NULL,NULL,NULL);

	curr_node = the_statement;
	tokcnt = a_token_cache_count(private_cache);

	while(tokcnt--)
	{
		TOKEN	*tokptr;

		tokptr = (TOKEN *)make_token(UNKNOWN,NULL);
		a_token_cache_unque(private_cache, tokptr);

		if (fluff_token(tokptr))
		{
			tie_bottom(curr_node,maketoknode(tokptr));
		}
		else
		{
			curr_node->next = maketoknode(tokptr);
			curr_node = curr_node->next;
		}
	}

	curr_node->next = makenode(NODE_END,NULL,NULL,NULL);

	return( the_statement );	
}

NODE make_clause(int column, char *clause, void *the_context)
{
	char	the_line[128];

	memset(the_line,' ',column);
	strcpy(&the_line[column-1],clause);

	return(make_statement(the_line,the_context));
}

NODE first_node_with_token(NODE the_tree)
{
	NODE the_node;

	if (!the_tree) return(NULL);						/* Dead end, return not found			*/

	if (the_tree->token) return(the_tree);					/* Found a node with a token, return it		*/

	the_node = first_node_with_token(the_tree->down);			/* Search down and if found return it		*/
	if (the_node) return(the_node);

	return(first_node_with_token(the_tree->next));				/* Search next					*/
}

NODE free_statement(NODE start)
{
	delete_tree(start);
	return NULL;
}

void hold_statement(NODE start)
{
	/*
	**	THIS IS NOT FINISHED !!!
	*/
	hold_line();
	free_statement(start);
}

NODE delint_statement(NODE start)
{
	if (!start) return start;

	delint_statement(start->down);
	delint_statement(start->next);

	if (!start->token) return start;

	if (lint_token(start->token))
	{
		free_token_from_node(start);
	}
	return start;
}

NODE strip_statement(NODE start)
{
	if (!start) return start;

	strip_statement(start->down);
	strip_statement(start->next);

	if (!start->token) return start;

	if (lint_token(start->token) || COMMENT == start->token->type)
	{
		free_token_from_node(start);
	}
	else
	{
		start->token->absline = 0;
		start->token->line = 0;
		if (!start->token->column_fixed)
		{
			start->token->column = 0;
		}
		start->token->context = NULL;
	}
	return start;
}

NODE decontext_statement(NODE start)
{
	if (!start) return start;

	decontext_statement(start->down);
	decontext_statement(start->next);

	if (!start->token) return start;

	start->token->absline = 0;
	start->token->line = 0;
	if (!start->token->column_fixed)
	{
		start->token->column = 0;
	}
	start->token->context = NULL;

	return start;
}
/*
**	History:
**	$Log: statment.c,v $
**	Revision 1.7  1996-08-30 21:56:09-04  gsl
**	drcs update
**
**
**
*/
