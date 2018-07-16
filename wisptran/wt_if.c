/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/


#define EXT extern

#include "wisp.h"
#include "token.h"
#include "node.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"

/*
	IF condition [THEN] { statmemt-1    }
			    { NEXT SENTENCE }
                     [ELSE  { statment-2    } ]
		     [      { NEXT SENTENCE } ]


	condition 	( condition )

			FAC-OF-data-name ALTERED		* "FAC OF" has been changed to "FAC-OF-"

			figcon  {IN} {identifier}     [IS] [NOT] {ON}
				{OF} {F-display-item}		 {OFF}




	(BEFORE)
           IF FIGCON-1  IN ITEM-1 IS ON OR
              FIGCON-2  IN FAC OF ITEM-2 OFF THEN
              PERFORM XXX-PARA.

	(AFTER)
           MOVE FIGCON-1 TO WISP-TEST-BYTE,
           CALL "BIT_TEST" USING
                    WISP-TEST-BYTE,
                    ITEM-1,
                    WISP-SCRATCH-BYTE-1,

           MOVE FIGCON-2 TO WISP-TEST-BYTE,
           CALL "BIT_TEST" USING
                    WISP-TEST-BYTE,
                    ITEM-2,
                    WISP-SCRATCH-BYTE-2,

           IF WISP-SCRATCH-BYTE-1-TRUE
              OR
              NOT WISP-SCRATCH-BYTE-2-TRUE
           THEN
              PERFORM XXX-PARA.

*/

NODE parse_if(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, temp_node, id_node, save_node, verb_node;
	int	col;
	int 	bytenum, negate, flag_on;
	char	buff[256];
	int	next_sentence_found;

	verb_node = first_token_node(the_statement);
	curr_node = verb_node;

	if (!eq_token(curr_node->token,VERB,"IF"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected IF found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	bytenum = 1;
	next_sentence_found = 0;

	col = verb_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	tput_leading_fluff(the_statement);

	for(curr_node=verb_node->next; NODE_END != curr_node->type; curr_node=curr_node->next)
	{
		if (eq_token(curr_node->token,KEYWORD,"NEXT"))
		{
			curr_node = curr_node->next;
			if ( curr_node && eq_token(curr_node->token,KEYWORD,"SENTENCE"))
			{
				next_sentence_found = 1;
				curr_node = curr_node->next;
				break;
			}
		}

		if (curr_node->token && IDENTIFIER==curr_node->token->type)
		{
			temp_node = curr_node->next;

			if (isafigcon1(token_data(curr_node->token)) &&
				( eq_token(temp_node->token,KEYWORD,"IN") || 
				  eq_token(temp_node->token,KEYWORD,"OF")   ))
			{
				/*
				**	IF figcon {IN|OF} identifier IS [NOT] {ON|OFF}
				**
				**	MOVE figcon TO WISP-TEST-BYTE
				**	CALL "BIT_TEST" USING WISP-TEST-BYTE, identifier, WISP-SCRATCH-BYTE-x
				**
				**	IF [NOT] WISP-SCRATCH-BYTE-x-TRUE ...
				*/

				write_log("WISP",'I',"IFFIGCON","Processing IF figcon condition.");

				curr_node->next = temp_node->next;		/* Unhooking the {IN|OF} node			*/
				temp_node->next = NULL;
				free_statement(temp_node);			/* Delete the  {IN|OF} node			*/

				id_node = curr_node->next;
				if (!reduce_data_item(id_node))			/* Reduce identifier				*/
				{
					write_tlog(id_node->token, "WISP",'W',"PROCIF",
						"Error parsing IF figcon {IN|OF} identifier, unrecognized data item.[%s]",
						token_data(id_node->token));
					reduce_one(id_node);
				}
				decontext_statement(id_node->down);

				if (opt_native_screens)	/* IF FAC 0F CONDITION - WARNING */
				{
					if (0 == memcmp(token_data(id_node->down->token),
						FAC_OF_PREFIX, strlen(FAC_OF_PREFIX)))
					{
						write_log("WISP",'W',"FACCOND",
							  "FAC OF condition may not be valid with Native Screens");
					}
				}

				temp_node = id_node->next;
				if (eq_token(temp_node->token,KEYWORD,"IS"))
				{
					temp_node = temp_node->next;
				}

				negate = 0;
				if (eq_token(temp_node->token,KEYWORD,"NOT"))
				{
					negate = 1;
					temp_node = temp_node->next;
				}

				if (eq_token(temp_node->token,KEYWORD,"ON"))
				{
					flag_on = 1;
				}
				else if (eq_token(temp_node->token,KEYWORD,"OFF"))
				{
					flag_on = 0;
				}
				else
				{
					write_tlog(temp_node->token, "WISP",'E',"PROCIF",
						"Error parsing IF, expecting ON or OFF found [%s]",
						token_data(temp_node->token));
					tput_statement(col,the_statement);
				}

				if (bytenum > 16)
				{
					write_log("WISP",'F',"SCRATCHBYTE","Maximum 16 BIT_TEST operations per statement");
				}

				tput_line_at  (col,   "MOVE %s",token_data(curr_node->token));
				tput_clause   (col+4, "TO WISP-TEST-BYTE,");
				tput_line_at  (col,   "CALL \"BIT_TEST\" USING WISP-TEST-BYTE,");
				tput_statement(col+4, id_node->down);
				tput_clause   (col+4, ", WISP-SCRATCH-BYTE-%d,",bytenum);
				tput_flush();

				save_node = curr_node->next;			/* Save curr->next in temp node			*/
				curr_node->next = temp_node->next;		/* Unhook "figcon IN id IS NOT ON" clause	*/
				temp_node->next = NULL;

				sprintf(buff,"WISP-SCRATCH-BYTE-%d-TRUE",bytenum);

				if ((flag_on && negate) || (!flag_on && !negate))
				{
					edit_token(curr_node->token,"NOT");
					curr_node->token->type = KEYWORD;
					tie_next(curr_node,maketoknode(make_token(IDENTIFIER,buff)));
					curr_node = curr_node->next;
				}
				else
				{
					edit_token(curr_node->token,buff);
				}

				curr_node->down = temp_node->down;		/* Save the fluff				*/
				temp_node->down = NULL;
				free_statement(save_node);			/* Delete the figcon clause			*/

				bytenum++;
			}
			else
			{
				/*
				**	Current node is an IDENTIFIER
				*/

				id_node = curr_node;
				if (!reduce_data_item(id_node))			/* Reduce identifier				*/
				{
					write_tlog(id_node->token, "WISP",'W',"PROCIF",
						"Error parsing IF condition, unrecognized data item.[%s]",
						token_data(id_node->token));
					reduce_one(id_node);
				}

				temp_node = curr_node->next;

				/*
				**	Skip over optional [IS] [NOT] looking for ALTERED
				*/
				if (eq_token(temp_node->token,KEYWORD,"IS"))
				{
					temp_node = temp_node->next;
				}
				if (eq_token(temp_node->token,KEYWORD,"NOT"))
				{
					temp_node = temp_node->next;
				}

				if (eq_token(temp_node->token,KEYWORD,"ALTERED"))
				{
					/*
					**	IF  FAC OF identifier [IS] [NOT] ALTERED ...
					**	IF  F-identifier      [IS] [NOT] ALTERED ...
					**
					**	CALL "BIT_TEST" USING WISP-ALTERED-BIT, 
					**           FAC-OF-identifier, WISP-SCRATCH-BYTE-x
					**
					**	IF  [NOT] WISP-SCRATCH-BYTE-x-TRUE  ...
					*/	

					write_log("WISP",'I',"IFALTERED","Processing IF FAC OF id ALTERED condition");

					if (opt_native_screens)		/* IF FAC OF ALTERED - WARNING */
					{
						write_log("WISP",'W',"FACALTERED",
							  "IF FAC OF field ALTERED not supported with Native Screens");
					}

					negate = 0;
					/*
					**	Reset temp now that we know were processing an ALTERED clause.
					*/
					temp_node = curr_node->next;

					/*
					**	Remove optional [IS] [NOT] before ALTERED
					*/
					if (eq_token(temp_node->token,KEYWORD,"IS"))
					{
						/*
						**	Remove the [invalid] "IS" token. (Not allowed per manual)
						*/
						curr_node->next = temp_node->next;	/* Unhook the IS node.			*/
						temp_node->next = NULL;
						free_statement(temp_node);		/* Free the IS node.			*/
						temp_node = curr_node->next;		/* Point to the [NOT] ALTERED  node	*/
					}
					if (eq_token(temp_node->token,KEYWORD,"NOT"))
					{
						/*
						**	Remove the [invalid] "NOT" token. (Not allowed per manual)
						*/
						curr_node->next = temp_node->next;	/* Unhook the IS node.			*/
						temp_node->next = NULL;
						free_statement(temp_node);		/* Free the IS node.			*/
						temp_node = curr_node->next;		/* Point to the ALTERED  node		*/
						negate = 1;
					}

					if (0 != memcmp(token_data(id_node->down->token),
						FAC_OF_PREFIX, strlen(FAC_OF_PREFIX)))
					{
						/*
						**	Missing the "FAC OF" clause, so add it.
						*/
						write_tlog(id_node->down->token,
							"WISP",'W',"IFALTERED","Warning IF ALTERED missing \"FAC OF\" for [%s]",
							token_data(id_node->down->token));

						make_fac(buff,token_data(id_node->down->token)); /* Create a F-identifier	*/
						edit_token(id_node->down->token,buff);
					}

					if (bytenum > 16)
					{
						write_log("WISP",'F',"SCRATCHBYTE","Maximum 16 BIT_TEST operations per statement");
					}

					tput_line_at  (col,   "CALL \"BIT_TEST\" USING WISP-ALTERED-BIT,");
					decontext_statement(id_node->down);
					tput_statement(col+4, id_node->down);
					tput_clause   (col+4, ", WISP-SCRATCH-BYTE-%d,",bytenum);
					tput_flush();

					free_statement(id_node->down);		/* Free the identifier				*/
					id_node->down = NULL;

					if (negate)
					{
						id_node->type = NODE_TOKEN;
						id_node->token = make_token(IDENTIFIER,"NOT");

						tie_next(id_node,maketoknode(NULL));
						id_node = id_node->next;
					}
					id_node->down = temp_node->down;	/* Move the fluff off the ALTERED node		*/
					temp_node->down = NULL;
					id_node->next = temp_node->next;	/* Unhook the ALTERED node.			*/
					temp_node->next = NULL;
					free_statement(temp_node);		/* Free the ALTERED node.			*/

					sprintf(buff,"WISP-SCRATCH-BYTE-%d-TRUE",bytenum);
					id_node->type = NODE_TOKEN;
					id_node->token = make_token(IDENTIFIER,buff);

					bytenum++;
				} /* End of if (ALTERED) */
			}
		} /* End of if (IDENTIFIER) */
	} /* End of for loop */

	tput_statement(col,the_statement);
	the_statement = free_statement(the_statement);

	if (!next_sentence_found)
	{
		the_statement = parse_imperative_statements(the_statement, the_sentence);

	}

	if (!the_statement)
	{
		the_statement = get_statement_from_sentence(the_sentence);
	}

	/*
	**	ELSE [statement]
	**	ELSE NEXT SENTENCE
	*/
	curr_node = first_non_fluff_node(the_statement);
	if (curr_node && eq_token(curr_node->token, KEYWORD, "ELSE"))
	{
		/* ELSE is a single token fragment */
		curr_node->token->column_fixed = 1;
		tput_statement(col,the_statement);
		the_statement = free_statement(the_statement);

		the_statement = get_statement_from_sentence(the_sentence);
		curr_node = first_non_fluff_node(the_statement);
		if (curr_node && eq_token(curr_node->token, KEYWORD, "NEXT"))
		{
			/*
			**	ELSE NEXT SENTENCE
			*/

			curr_node->token->column_fixed = 1;
			tput_statement(col+4,the_statement);
			the_statement = free_statement(the_statement);
		}
		else
		{
			/*
			**	ELSE imperative-statement
			*/

			the_statement = parse_imperative_statements(the_statement, the_sentence);
		}
		
	}
	
	if (!the_statement)
	{
		the_statement = get_statement_from_sentence(the_sentence);
	}

	curr_node = first_non_fluff_node(the_statement);
	if (eq_token(curr_node->token, KEYWORD, "END-IF"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);
	}

	return the_statement;
}

/*
**	History:
**	$Log: wt_if.c,v $
**	Revision 1.22  2003/08/11 17:18:18  gsl
**	MF Native screens
**	
**	Revision 1.21  2003/08/08 19:52:46  gsl
**	Add native screens comments
**	
**	Revision 1.20  2003/08/06 18:12:10  gsl
**	
**	Revision 1.19  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.18  2002/07/31 20:24:26  gsl
**	globals
**	
**	Revision 1.17  2002/07/30 22:00:49  gsl
**	globals
**	
**	Revision 1.16  2002/07/26 19:20:43  gsl
**	
**	Revision 1.15  2002/07/25 17:53:35  gsl
**	Fix FAC-OF- prefix
**	
**	Revision 1.14  1998/03/03 18:03:18  gsl
**	change flush into fixed
**	
**	Revision 1.13  1998-02-25 09:57:01-05  gsl
**	Update for cobol-85 and to fully handle the statement
**
**	Revision 1.12  1997-09-12 16:50:59-04  gsl
**	Add error for SCRATCH-BYTE overflow
**
**	Revision 1.11  1997-09-12 13:13:22-04  gsl
**	Add Native Screens warnings for FAC OF conditions
**
**	Revision 1.10  1996-08-30 21:56:20-04  gsl
**	drcs update
**
**
**
*/
