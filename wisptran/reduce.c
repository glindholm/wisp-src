			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		reduce.c
**
**			Reducing means to take a sequence of token nodes out of the list and replace
**			them with a single node that represents them.  The nodes that get remove are then
**			attached under the node that replaced them.
**
**				[MOVE]-[A]-[OF]-[B]-[TO]-[C]-[(]-[D]-[)]-[.]-[]
**
**				[MOVE]-[data item]-[TO]-[data item]-[.]-[]
**				            |                |
**				           [A]-[OF]-[B]-[]  [C]-[parens]-[]
**				                                   |
**				                                  [(]-[D]-[)]-[]
**
**	Purpose:	Parsing routines that reduce token streams.
**
**	Routines:	
**	reduce_data_item()	Reduce a common data item.
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#include "token.h"
#include "node.h"

NODE reduce_parens();

/*
**	Routine:	reduce_data_item()
**
**	Function:	To reduce a common data item.
**
**	Description:	This will reduce the following:
**
**				Data name			identifier
**				Qualified data name		identifier OF/IN identifier
**				Subscripted data name		identifier ( xxx )
**				Number				number
**				Literal				"xxxx"
**				Continued literal		"xxxx| - "xxx"
**				Reference modification		identifier ( xxx:xxx )
**				Figurative constants		SPACES/ZEROS/QUOTES/...
**
**	Arguments:
**	start		The starting node in a list.
**
**	Globals:	None
**
**	Return:		First node in the reduced data item (this will be start->down) or NULL if can't reduce it.
**
**	Warnings:	None
**
**	History:	
**	05/22/93	Written by GSL
**	06/02/93	Added Figurative Constants. GSL
**
*/
NODE reduce_data_item(start)
NODE start;
{
	NODE	new_node, temp_node;

	if (NODE_TOKEN != start->type)
	{
		/* Only knowns how to reduce token nodes */
		return(NULL);
	}

	if (NUMBER==start->token->type)
	{
		tie_down(start,makenode(NODE_TOKEN,NULL,makenode(NODE_END,NULL,NULL,NULL),start->token));
		start->token = NULL;
		start->type = NODE_DATAITEM;
		return(start->down);
	}

	if (LITERAL==start->token->type)
	{
		new_node = makenode(NODE_TOKEN,NULL,start->next,start->token);

		temp_node = new_node;
		while(temp_node->next && NODE_TOKEN==temp_node->next->type && 
			(CONTINUATION==temp_node->next->token->type || LITERAL==temp_node->next->token->type))
		{
			temp_node = temp_node->next;
		}

		tie_down(start,new_node);
		start->token = NULL;
		start->type = NODE_DATAITEM;
		start->next = temp_node->next;
		temp_node->next = makenode(NODE_END,NULL,NULL,NULL);
		return(start->down);
	}

	if (IDENTIFIER==start->token->type)
	{
		/*
		**		identifier of/in identifier
		**		identifier ( xxx (xxx) )
		*/

		new_node = makenode(NODE_TOKEN,NULL,start->next,start->token);

		temp_node = new_node;
		while(temp_node->next && NODE_TOKEN==temp_node->next->type)
		{
			if (eq_token(temp_node->next->token,0,"("))
			{
				reduce_parens(temp_node->next);
			}
			else if (eq_token(temp_node->next->token,0,"OF") ||
			         eq_token(temp_node->next->token,0,"IN")   )
			{
				/* Skip over token */
				temp_node = temp_node->next;
			}
			else
			{
				/* Unrecognized */
				break;
			}
			temp_node = temp_node->next;
		}

		tie_down(start,new_node);
		start->token = NULL;
		start->type = NODE_DATAITEM;
		start->next = temp_node->next;
		temp_node->next = makenode(NODE_END,NULL,NULL,NULL);
		return(start->down);
	}

	if (KEYWORD==start->token->type)
	{
		/*
		**	Figurative Constants
		**
		**	ZERO, SPACE, HIGH-VALUE, LOW-VALUE, QUOTE
		*/

		if (	eq_token(start->token,KEYWORD,"ZERO") 		||
			eq_token(start->token,KEYWORD,"ZEROS") 		||
			eq_token(start->token,KEYWORD,"ZEROES") 	||
			eq_token(start->token,KEYWORD,"SPACE") 		||
			eq_token(start->token,KEYWORD,"SPACES") 	||
			eq_token(start->token,KEYWORD,"HIGH-VALUE") 	||
			eq_token(start->token,KEYWORD,"HIGH-VALUES") 	||
			eq_token(start->token,KEYWORD,"LOW-VALUE") 	||
			eq_token(start->token,KEYWORD,"LOW-VALUES") 	||
			eq_token(start->token,KEYWORD,"QUOTE") 		||
			eq_token(start->token,KEYWORD,"QUOTES")		  )
		{
			tie_down(start,makenode(NODE_TOKEN,NULL,makenode(NODE_END,NULL,NULL,NULL),start->token));
			start->token = NULL;
			start->type = NODE_DATAITEM;
			return(start->down);
		}		
	}

	return(NULL);								/* Could not reduce				*/
}


/*
**	Routine:	reduce_parens()
**
**	Function:	To reduce parenthesis.
**
**	Description:	This will reduce everything between parenthesis.
**			If nested parenthesis are found this routine will recurse.
**
**	Arguments:
**	start		The starting node in a list.
**
**	Globals:	None
**
**	Return:		First node in the reduced data item (this will be start->down) or NULL if can't reduce it.
**
**	Warnings:	None
**
**	History:	
**	05/24/93	Written by GSL
**
*/

NODE reduce_parens(start)
NODE start;
{
	NODE	new_node, temp_node;

	if (NODE_TOKEN != start->type)
	{
		/* Only knowns how to reduce token nodes */
		return(NULL);
	}

	if (!eq_token(start->token,0,"("))
	{
		/* Must start with a open parenthesis */
		return(NULL);
	}

	new_node = makenode(NODE_TOKEN,NULL,start->next,start->token);

	/*
	**	Loop until temp_node points to the last node in the list to be reduced.
	*/

	temp_node = new_node;
	while(temp_node->next)
	{
		if (eq_token(temp_node->next->token,0,"("))
		{
			reduce_parens(temp_node->next);
		}
		else if (eq_token(temp_node->next->token,0,")"))
		{
			temp_node = temp_node->next;
			break;
		}
		temp_node = temp_node->next;
	}

	tie_down(start,new_node);
	start->token = NULL;
	start->type = NODE_PARENS;
	start->next = temp_node->next;
	temp_node->next = makenode(NODE_END,NULL,NULL,NULL);
	return(start->down);
}

/*
**	Routine:	reduce_one()
**
**	Function:	To reduce one token.
**
**	Description:	This will reduce one token of any type.
**			This is simple case that would normally be used when other reduce methods failed.
**
**	Arguments:
**	start		The starting node in a list.
**
**	Globals:	None
**
**	Return:		First node in the reduced data item (this will be start->down.)
**
**	Warnings:	None
**
**	History:	
**	05/24/93	Written by GSL
**
*/

NODE reduce_one(start)
NODE start;
{
	if (start)
	{

		tie_down(start,makenode(start->type,NULL,NULL,start->token));
		start->token = NULL;
		start->type = NODE_UNKNOWN;
		start->down->next = makenode(NODE_END,NULL,NULL,NULL);
		return(start->down);
	}
	else
	{
		return(makenode(NODE_UNKNOWN,NULL,NULL,NULL));
	}
}
