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
#include <assert.h>

#include "token.h"
#include "node.h"
#include "lines.h"
#include "statment.h"
#include "tokenize.h"
#include "proto.h"
#include "ring.h"

static NODE backup_next_chain(NODE start_node, NODE curr_node);


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
**	Routine:	get_statement_fragment()
**
**	Function:	To load the next procedure statement fragment.
**
**	Description:	This routine builds a node tree representation of the next statement fragment.
**			The node tree begins with a START node and ends with an END node.
**			Significant tokens follow the next node list. Fluff tokens are tied
**			on the down list. The nodes are meant to be traversed in self-down-next
**			order.
**			It uses get_token() to get the next token until we have a fragment.
**			If a PERIOD is found then it checks the token_cache and if the rest of the line is fluff loads it.
**
**			Fragments are:
**				VERB tokens...
**
**				.
**				END-VERB
**				ELSE
**				NO-MOD
**
**				WHEN tokens...
**				TIMEOUT tokens...
**
**				[NOT] INVALID [KEY]
**				[NOT] [AT] END
**				[NOT] [AT] END-OF-PAGE
**				[NOT] [AT] EOP
**				[NOT] [ON] OVERFLOW
**				[NOT] [ON] EXCEPTION
**				[NOT] [ON] [SIZE] ERROR
**				
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
NODE get_statement_fragment(void)
{
	NODE	start_node, curr_node, temp_node, prev_node;
	TOKEN	*tokptr;
	int	token_count;
	int	first_non_fluff = 1;
	int	break_on_error_clause = 1;

	start_node = makenode(NODE_START,NULL,NULL,NULL);
	curr_node  = start_node;
	token_count = 0;
	prev_node = NULL;

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
			if (first_non_fluff) /* This is our first non-fluff token */
			{
				/*
				**	Default is to break on an error clause unless this fragment
				**	is the error clause.
				*/
				break_on_error_clause = 1;

				if (KEYWORD == tokptr->type)
				{

					if (eq_token(tokptr, KEYWORD, "NOT") ||
					    eq_token(tokptr, KEYWORD, "AT") ||
					    eq_token(tokptr, KEYWORD, "SIZE") ||
					    eq_token(tokptr, KEYWORD, "ON"))
					{
						/*
						**	This is an error clause so don't break until after
						**	the keyword is found.
						*/
						break_on_error_clause = 0;
					}
					else if (eq_token(tokptr, KEYWORD, "INVALID") ||
						 eq_token(tokptr, KEYWORD, "END") ||
						 eq_token(tokptr, KEYWORD, "END-OF-PAGE") ||
						 eq_token(tokptr, KEYWORD, "EOP") ||
						 eq_token(tokptr, KEYWORD, "OVERFLOW") ||
						 eq_token(tokptr, KEYWORD, "EXCEPTION") ||
						 eq_token(tokptr, KEYWORD, "ERROR"))
					{
						/*
						**	This is an error clause and we found the keyword
						**	so if we see another error keyword we will break
						**	on it.
						*/
						break_on_error_clause = 1;
					}
				}
			}
			else
			{
				if (VERB == tokptr->type ||
				    PERIOD == tokptr->type ||
				    eq_token(tokptr,KEYWORD,"ELSE") || 
				    eq_token(tokptr,KEYWORD,"NO-MOD") || 
				    eq_token(tokptr,KEYWORD,"WHEN") || 
				    eq_token(tokptr,KEYWORD,"TIMEOUT") || 
				    (KEYWORD==tokptr->type && 0==memcmp(tokptr->data,"END-",4) 
				                           && !eq_token(tokptr,KEYWORD,"END-OF-PAGE")))
				{
					/*
					**	Statement ends when we see another VERB or an END-IF, END-PERFORM etc...
					*/
					unget_token(tokptr);
					break;
				}

				/*
				**	Invalid condition clauses end a statement.
				**	If the keyword is found them we have to backup and unget tokens.
				**
				**	[NOT] INVALID [key]
				**	[NOT] [AT] END
				**	[NOT] [AT] END-OF-PAGE
				**	[NOT] [AT] EOP
				**	[NOT] [ON] OVERFLOW
				**	[NOT] [ON] EXCEPTION
				**	[NOT] [ON] [SIZE] ERROR
				*/
				if (!break_on_error_clause && KEYWORD == tokptr->type)
				{
					if (eq_token(tokptr, KEYWORD, "INVALID") ||
					    eq_token(tokptr, KEYWORD, "END") ||
					    eq_token(tokptr, KEYWORD, "END-OF-PAGE") ||
					    eq_token(tokptr, KEYWORD, "EOP") ||
					    eq_token(tokptr, KEYWORD, "OVERFLOW") ||
					    eq_token(tokptr, KEYWORD, "EXCEPTION") ||
					    eq_token(tokptr, KEYWORD, "ERROR"))
					{
						/*
						**	We found the first error clause so turn on the flag
						**	in case there is a second error clause.
						**
						**	Handling case:
						**		VERB ....
						**		   ERROR {empty}
						**		   NOT ERROR statement.
						*/
						break_on_error_clause = 1;
					}
				}
				else if (break_on_error_clause && KEYWORD == tokptr->type)
				{
					/* 
					**	prev_node must point to the last good node of this fragment
					*/
					if (eq_token(tokptr, KEYWORD, "INVALID"))
					{
						prev_node = curr_node;
						unget_token(tokptr);
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "NOT"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						break;
						
					}
					else if (eq_token(tokptr, KEYWORD, "END") ||
						 eq_token(tokptr, KEYWORD, "END-OF-PAGE") ||
						 eq_token(tokptr, KEYWORD, "EOP"))
					{
						prev_node = curr_node;
						unget_token(tokptr);
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "AT"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "NOT"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						break;
					}
					else if (eq_token(tokptr, KEYWORD, "OVERFLOW") ||
						 eq_token(tokptr, KEYWORD, "EXCEPTION"))
					{
						prev_node = curr_node;
						unget_token(tokptr);
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "ON"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "NOT"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						break;
					}
					else if (eq_token(tokptr, KEYWORD, "ERROR"))
					{
						prev_node = curr_node;
						unget_token(tokptr);
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "SIZE"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "ON"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						if (prev_node && prev_node->token && eq_token(prev_node->token, KEYWORD, "NOT"))
						{
							unget_token(prev_node->token);
							prev_node->token = NULL;
							prev_node = backup_next_chain(start_node,prev_node);
						}
						break;
					}
				}
			}


			curr_node->next = maketoknode(tokptr);
			curr_node = curr_node->next;

			if (first_non_fluff)
			{
				first_non_fluff = 0;
			}
			

			/*
			**     	Catch the single token fragments: PERIOD, ELSE, NO-MOD, END-VERB
			*/
			if (eq_token(tokptr,KEYWORD,"ELSE") || 
			    eq_token(tokptr,KEYWORD,"NO-MOD") || 
			    (KEYWORD==tokptr->type && 0==memcmp(tokptr->data,"END-",4)
				                   && !eq_token(tokptr,KEYWORD,"END-OF-PAGE")))
			{
				break;
			}
			
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

	if (prev_node)
	{
		curr_node = prev_node;
		curr_node->next = free_statement(curr_node->next);
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
**	line		The input line, it must be a complete line starting at column 1.
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

/* Find the first node in the tree which is not fluff or NULL */
NODE first_non_fluff_node(NODE the_tree)
{
	NODE the_node;
	
	if (!the_tree)
	{
		return NULL;
	}
	
	if (the_tree->token && !fluff_token(the_tree->token))
	{
		return the_tree;
	}

	if (the_node = first_non_fluff_node(the_tree->down))
	{
		return the_node;
	}

	return first_non_fluff_node(the_tree->next);
}

NODE free_statement(NODE start)
{
	delete_tree(start);
	return NULL;
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

static NODE backup_next_chain(NODE start_node, NODE curr_node)
{
	for(; start_node; start_node=start_node->next)
	{
		if (start_node->next == curr_node)
		{
			return start_node;
		}
	}
	return NULL;
}

/*
**	ROUTINE:	get_sentence_tree()
**
**	FUNCTION:	Get the next sentence.
**
**	DESCRIPTION:	Build a sentence tree where each down-node is a statement fragment
**			from get_statement_fragment().
**			The tree has a NODE_START and an NODE_END and the last fragment 
**			has a period.
**
**			[START] -> [NODE] -> [NODE] -> [END]
**				     |	       |
**			        [STATEMENT]  [STATEMENT]
**
**			Defn: A "sentence" is a list of statement fragments ending in a period.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The start node of the sentence tree or NULL if EOF.
**
**	WARNINGS:	none
**
*/
NODE get_sentence_tree(void)
{
	NODE	start_node, curr_node, temp_node;
	NODE	the_statement;

	start_node = makenode(NODE_START,NULL,NULL,NULL);
	curr_node  = start_node;

	for(;;)
	{
		the_statement = get_statement_fragment();
		if (!the_statement)
		{
			if (start_node == curr_node)
			{
				/* Empty tree */
				wfree(start_node);
				return(NULL);
			}

			if (first_non_fluff_node(start_node))
			{
				/*  ERROR: missing period */

				write_log("WISP",'E',"PERIOD","Missing trailing period. Period added.");

				curr_node->next = makenode(NODE_TOKEN, NULL, NULL, make_token(PERIOD,"."));
				curr_node = curr_node->next;
			}
			
			curr_node->next = makenode(NODE_END,NULL,NULL,NULL);
			return start_node;
		}
		
		curr_node->next = makenode(NODE_STATEMENT, the_statement, NULL, NULL);
			
		curr_node = curr_node->next;

		/*
		**	Check if this statement has a trailing PERIOD.
		*/

		for (temp_node=the_statement->next; temp_node; temp_node = temp_node->next)
		{
			if (temp_node->token && PERIOD == temp_node->token->type)
			{
				/* Found period so done. */
				curr_node->next = makenode(NODE_END,NULL,NULL,NULL);
				return start_node;
			}
		}
	}
}

/*
**	ROUTINE:	get_statement_from_sentence()
**
**	FUNCTION:	Get the next statement fragment from the sentence.
**
**	DESCRIPTION:	The next statement fragment is removed from the sentence.
**			When no fragments are left then NULL is returned and the sentence
**			can be safely deleted.
**
**	ARGUMENTS:	
**	the_sentence	The sentence to remove the statement fragement from (or NULL).
**
**	GLOBALS:	None
**
**	RETURN:		The next statement fragment or NULL
**
**	WARNINGS:	none
**
*/
NODE get_statement_from_sentence(NODE the_sentence)
{
	NODE	curr_node, next_statement;
	
	if (!the_sentence)
	{
		return NULL;
	}

	assert(NODE_START == the_sentence->type);
	
	curr_node = the_sentence->next;
	
	if (NODE_END == curr_node->type)
	{
		return NULL;
	}
	
	next_statement = curr_node->down;

	/*
	**	Unlink next_statement from the sentence.
	*/
	curr_node->down = NULL;
	the_sentence->next = curr_node->next;
	wfree(curr_node);

	return next_statement;
}

/*
**	ROUTINE:	unget_statement_from_sentence()
**
**	FUNCTION:	Unget a statement fragment from the sentence.
**
**	DESCRIPTION:	Put a statement fragment onto the front of a sentence.
**			This should only be called following a get_statement_from_sentence().
**
**	ARGUMENTS:	
**	the_statement	The statement fragment to add.
**	the_sentence	The sentence to add the statement fragement to.
**
**	GLOBALS:	None
**
**	RETURN:		NULL if successful of the_statement if no sentence.
**
**	WARNINGS:	none
**
*/
NODE unget_statement_from_sentence(NODE the_statement, NODE the_sentence)
{
	if (!the_sentence || !the_statement)
	{
		return the_statement;
	}

	assert(NODE_START == the_sentence->type);
	assert(NODE_START == the_statement->type);

	the_sentence->next = makenode(NODE_STATEMENT, the_statement, the_sentence->next, NULL);

	return NULL;
}

/*
**	ROUTINE:	unhook_next_fragment()
**
**	FUNCTION:	Unhook a trailing statement fragment.
**
**	DESCRIPTION:	Curr_node should point past the last significant token in a
**			statement.  If it points to anything other then a PERIOD then
**			unhook it and return it as the next fragment.
**
**	ARGUMENTS:	
**	the_statement	The statement that curr_node is a part of.
**	curr_node	The node past the last significant token in the_statement.
**
**	GLOBALS:	None
**
**	RETURN:		The next fragment (curr_node) or NULL.
**
**	WARNINGS:	None
**
*/
NODE unhook_next_fragment(NODE the_statement, NODE curr_node)
{
	if (curr_node &&
	    NODE_END != curr_node->type &&
	    !eq_token(curr_node->token, PERIOD, "."))
	{
		return unhook_sub_tree(the_statement, curr_node);
	}
	else
	{
		return NULL;
	}
}

/*
**	ROUTINE:	find_period_node()
**
**	FUNCTION:	Search a tree for a PERIOD node.
**
**	DESCRIPTION:	Seach recursively for the PERIOD and return it.
**
**	ARGUMENTS:	
**	the_statement	The statement to search
**
**	GLOBALS:	None
**
**	RETURN:		The period node or NULL.
**
**	WARNINGS:	None
**
*/
NODE find_period_node(NODE the_statement)
{
	NODE period_node;
	
	if (!the_statement)
	{
		return NULL;
	}
	
	if (the_statement->token && PERIOD == the_statement->token->type)
	{
		return the_statement;
	}
	
	if (period_node = find_period_node(the_statement->down))
	{
		return period_node;
	}
	
	return find_period_node(the_statement->next);
}


/*
**	ROUTINE:	next_non_fluff_node_in_sentence()
**
**	FUNCTION:	Find the next non-fluff node in a sentence.
**
**	DESCRIPTION:	Traverse the tree in self-down-next order, 
**			when the curr_node is found set the flag to return
**			the next node, if the flag is set and the node is 
**			non-fluff return it.
**
**	ARGUMENTS:	
**	the_tree	The sentence tree.
**	curr_node	The current node to find the next one against.
**
**	GLOBALS:	none
**
**	RETURN:		The next node or NULL.
**
**	WARNINGS:	None
**
*/
NODE next_non_fluff_node_in_sentence(NODE the_tree, NODE curr_node)
{
	static int level = 0;
	static int return_next;
	NODE next_node;

	if (0 == level)
	{
		return_next = 0;
	}

	level++;
	
	next_node = NULL;

	if (the_tree)
	{
		if (return_next && the_tree->token && !fluff_token(the_tree->token))
		{
			/*
			**	SUCCESS: We found the next node so clear the flag and return it.
			*/
			next_node = the_tree;
			return_next = 0;
		}
		else if (the_tree == curr_node)
		{
			/*
			**	Found the curr_node, so set the flag to return the next node
			*/
			return_next = 1;
		}

		if (!next_node)
		{
			next_node = next_non_fluff_node_in_sentence(the_tree->down, curr_node);
		}
		
		if (!next_node)
		{
			next_node = next_non_fluff_node_in_sentence(the_tree->next, curr_node);
		}
	}
	
	level--;
	return next_node;
}

NODE last_non_fluff_node(NODE the_tree)
{
	NODE	last_node;
	
	if (!the_tree)
	{
		return NULL;
	}

	if (last_node = last_non_fluff_node(the_tree->next))
	{
		return last_node;
	}

	if (last_node = last_non_fluff_node(the_tree->down))
	{
		return last_node;
	}
	
	if (the_tree->token && !fluff_token(the_tree->token))
	{
		return the_tree;
	}
	return NULL;
}

NODE unhook_trailing_fluff(NODE the_tree)
{
	NODE 	last_node;
	NODE 	fluff_node;
	
	if (last_node = last_non_fluff_node(the_tree))
	{
		fluff_node = last_node->down;
		last_node->down = NULL;
		return fluff_node;
	}
	else
	{
		return NULL;
	}
}

/*
**	History:
**	$Log: statment.c,v $
**	Revision 1.11  1998/12/15 19:24:50  gsl
**	enhance get_statement_fragment() to recognize END-OF-PAGE clauses
**	
**	Revision 1.10  1998-03-04 13:44:11-05  gsl
**	Fix SIZE ERROR clause handling
**	Add EXCEPTION clause handling
**
**	Revision 1.9  1998-03-03 16:07:46-05  gsl
**	Add routines to fully support cobol-85 logic.
**
**	Revision 1.8  1998-02-10 15:06:58-05  gsl
**	Added "sentence" handling routines.
**	Added INVALID KEY handling to get_verb_statement()
**	Renamed get_verb_statement() to get_statement_fragment()
**	Added backup_next_chain() routine
**
**	Revision 1.7  1996-08-30 21:56:09-04  gsl
**	drcs update
**
**
**
*/
