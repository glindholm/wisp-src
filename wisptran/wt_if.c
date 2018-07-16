			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern

#include "wisp.h"
#include "token.h"
#include "node.h"

/*
	IF condition [THEN] { statmemt-1    }
			    { NEXT SENTENCE }
                     [ELSE  { statment-2    } ]
		     [      { NEXT SENTENCE } ]


	condition 	( condition )

			F-data-name ALTERED						* "FAC OF" has been changed to "F-"

			figcon  {IN} {identifier}     [IS] [NOT] {ON}
				{OF} {F-display-item}		 {OFF}




	(BEFORE)
           IF FIGCON-1  IN ITEM-1 IS ON OR
              FIGCON-2  IN FAC OF ITEM-2 OFF THEN
              PERFORM XXX-PARA.

	(AFTER)
           MOVE FIGCON-1 TO WISP-TEST-BYTE,
           CALL "bit_test" USING
                    WISP-TEST-BYTE,
                    ITEM-1,
                    WISP-SCRATCH-BYTE-1,

           MOVE FIGCON-2 TO WISP-TEST-BYTE,
           CALL "bit_test" USING
                    WISP-TEST-BYTE,
                    ITEM-2,
                    WISP-SCRATCH-BYTE-2,

           IF WISP-SCRATCH-BYTE-1-TRUE
              OR
              NOT WISP-SCRATCH-BYTE-2-TRUE
           THEN
              PERFORM XXX-PARA.

*/

parse_if(the_statement)
NODE the_statement;
{
	NODE	curr_node, if_node, temp_node, id_node, save_node;
	int	col;
	int 	bytenum, negate, flag_on;
	char	buff[256];

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"IF"))
	{
		tput_statement(12,the_statement);
		return(0);
	}

	bytenum = 1;

	write_log("WISP",'I',"PROCIF","Processing IF statement.");

	if_node = curr_node;

	col = if_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	for(curr_node=if_node->next; NODE_END != curr_node->type; curr_node=curr_node->next)
	{
		if (IDENTIFIER==curr_node->token->type)
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
				**	CALL "bit_test" USING WISP-TEST-BYTE, identifier, WISP-SCRATCH-BYTE-x
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

				tput_line_at  (col,   "MOVE %s",token_data(curr_node->token));
				tput_clause   (col+4, "TO WISP-TEST-BYTE,");
				tput_line_at  (col,   "CALL \"bit_test\" USING WISP-TEST-BYTE,");
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
				id_node = curr_node;
				if (!reduce_data_item(id_node))			/* Reduce identifier				*/
				{
					write_tlog(id_node->token, "WISP",'W',"PROCIF",
						"Error parsing IF condition, unrecognized data item.[%s]",
						token_data(id_node->token));
					reduce_one(id_node);
				}

				temp_node = curr_node->next;

				if (eq_token(temp_node->token,KEYWORD,"IS") && 
				    eq_token(temp_node->next->token,KEYWORD,"ALTERED"))
				{
					/*
					**	Remove the [invalid] "IS" token. (Not allowed per manual)
					*/
					curr_node->next = temp_node->next;	/* Unhook the IS node.				*/
					temp_node->next = NULL;
					free_statement(temp_node);		/* Free the IS node.				*/
					temp_node = curr_node->next;		/* Point to the ALTERED  node			*/
				}

				if (eq_token(temp_node->token,KEYWORD,"ALTERED"))
				{
					/*
					**	IF  FAC OF identifier [IS] ALTERED ...
					**
					**	CALL "bit_test" USING WISP-ALTERED-BIT, F-identifier, WISP-SCRATCH-BYTE-x
					**
					**	IF  WISP-SCRATCH-BYTE-x-TRUE  ...
					*/	

					write_log("WISP",'I',"IFALTERED","Processing IF FAC OF id ALTERED condition");

					if (0 != memcmp(token_data(id_node->down->token),"F-",2))
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

					tput_line_at  (col,   "CALL \"bit_test\" USING WISP-ALTERED-BIT,");
					decontext_statement(id_node->down);
					tput_statement(col+4, id_node->down);
					tput_clause   (col+4, ", WISP-SCRATCH-BYTE-%d,",bytenum);
					tput_flush();

					free_statement(id_node->down);		/* Free the identifier				*/
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

	tput_statement(12,the_statement);
	return(0);
}

#ifdef OLD
p_if()											/* Process IF statements.		*/
{
	int i,bytenum,negate;
	char tstr[132],*tptr,*oldstr,*lastr,*curstr,str1[80],str2[80],str3[80],pstr[80];
	int *tnl,*oldnl,*lastnl,*curnl,nl1,nl2,nl3;
	char ifbuff[4096];

	bytenum  = 1;
	oldstr = str1;									/* assign pointers and initialize.	*/
	oldstr[0] = '\0';
	oldnl = &nl1;
	nl1 = 0;

	lastr = str2;
	lastr[0] = '\0';
	lastnl = &nl2;
	nl2 = 0;

	curstr = str3;
	curnl = &nl3;
	nl3 = 0;

	strcpy(ifbuff,inline);								/* Copy the current input line.		*/
	i = strpos(ifbuff,"IF") + 2;
	ifbuff[i] = '\0';								/* put a null after the IF.		*/
	stredt(inline," IF","");							/* remove the if.			*/
	ptype = get_param(curstr);							/* Fetch it.				*/

	for(;;)
	{
		ptype = get_param(curstr);						/* Get the next word.			*/
		*curnl = 0;

		if (curstr[0] == '"')							/* It is a literal			*/
		{
			if (strpos(&curstr[1],"\"") == -1)				/* not closed.				*/
			{
				ptype = 0;
				i = strpos(inline,curstr) + 1;				/* look for param in inline		*/
				memset(curstr,' ',i);					/* set up spaces			*/
				curstr[0] = '\n';
				curstr[i] = '\0';
				i--;
				strcat(curstr,&inline[i]);				/* get the rest of the line.		*/
				get_line();
				ptype = 1;
			}
		}

		if (ptype == 1)
		{									/* New line needed, flag it.		*/
			*curnl = 1;
		}
		else
		{
			*curnl = 0;
		}

		if (!strcmp(curstr,"IS"))
		{
			stredt(inline," IS","");
			i = ptype;
			ptype = get_param(curstr);					/* Skip over IS.			*/
			if (i) ptype = i;						/* Remember if it was first.		*/
		}

		if (ptype == 1)
		{									/* New line needed, finish old line.	*/
			*curnl = 1;
		}
		else
		{
			*curnl = 0;
		}
											/* All done.				*/
		if (!strcmp(curstr,"THEN") || proc_keyword(curstr) || strcmp(area_a,"    "))
		{
			if (!strcmp(curstr,"THEN"))
			{
				i = 1;
				stredt(inline," THEN","");
			}
			else
			{
				i = 0;
			}
			if (*lastnl) strcat(ifbuff,"\n           ");
			else	     strcat(ifbuff," ");
			strcat(ifbuff,lastr);						/* output the final few words.		*/
			if (*oldnl) strcat(ifbuff,"\n           ");
			else	    strcat(ifbuff," ");
			strcat(ifbuff,oldstr);
			if (i) strcat(ifbuff,"\n            THEN\n");			/* put the THEN in.			*/
			else   strcat(ifbuff,"\n");
			dump_ifbuff = 1;						/* Flag we are dumping the if buffer.	*/
			tput_block(ifbuff);
			dump_ifbuff = 0;
			if (!strcmp(curstr,"ELSE"))					/* Special case, IF test ELSE		*/
			{
				write_log("WISP",'E',"IFNOIMPER","IF statement with no imperative, CONTINUE inserted.");
				tput_line("               CONTINUE\n");
			}
			hold_line();							/* allow the remainder to parse		*/
			return(0);
		}
		else if (!strcmp(curstr,"ALTERED") || !strcmp(curstr,"ALTERED)") || !strcmp(curstr,"ALTERED))"))
		{
			strcpy(tstr," ");						/* precede it with a space.		*/
			strcat(tstr,curstr);
			stredt(inline,curstr,"");					/* remove it.				*/

			if ((i = strpos(curstr,")")) != -1)				/* If there is a closing paren.		*/
			{
				strcpy(pstr,&curstr[i]);				/* Copy the parens and stuff.		*/
				curstr[i] = '\0';
			}
			else
			{
				pstr[0] = '\0';						/* Blank it out.			*/
			}

			if (!strcmp(oldstr,"NOT"))					/* preceded by a NOT?			*/
			{
				tptr = lastr;						/* Use the LAST string.			*/
											/* add the the TRUE value.		*/
				if (tptr[0] == '(')					/* Has an open paren.			*/
				{
					stredt(tptr,"(","");				/* Remove it				*/
					sprintf(tstr,"\n            (NOT WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
				else
				{
					sprintf(tstr,"\n            NOT WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
			}
			else
			{
				tptr = oldstr;						/* use the OLD string.			*/
				if (*lastnl) strcat(ifbuff,"\n           ");		/* and the newline info.		*/
				else	     strcat(ifbuff," ");
				strcat(ifbuff,lastr);					/* remember to output the LAST string.	*/
				if (tptr[0] == '(')					/* Has it got an open paren?		*/
				{
					stredt(tptr,"(","");				/* Remove it				*/
					sprintf(tstr,"\n            (WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
				else
				{
					sprintf(tstr,"\n            WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
			}

			strcat(ifbuff,tstr);

			tput_line_at(16, "CALL \"bit_test\" USING WISP-ALTERED-BIT,");	/* see if the bit is set		*/
											/* Did they leave off FAC OF?		*/
			if (tptr[0] == 'F' && tptr[1] == '-')				/* Does it have "F-"?			*/
			{
				tput_clause(20, "%s,",tptr);				/* Yes, it's ok.			*/
			}
			else
			{
				make_fac(tstr,tptr);					/* Make it a FAC field.			*/
				tput_clause(20, "%s,",tstr);				/* Add it on.				*/
			}

			tput_clause(20,"WISP-SCRATCH-BYTE-%d,",bytenum);
			tput_blank();

			*curstr = '\0';							/* Clear all the strings.		*/
			*oldstr = '\0';
			*lastr = '\0';
			bytenum++;
		}
		else if (isafigcon1(oldstr) && (!strcmp(curstr,"IN") || !strcmp(curstr,"OF")))
		{
			if (!strcmp(curstr,"IN"))
			{
				stredt(inline," IN","");				/* remove it.				*/
			}
			else
			{
				stredt(inline," OF","");				/* remove it.				*/
			}

			if (*lastnl) strcat(ifbuff,"\n           ");
			else	     strcat(ifbuff," ");
			strcat(ifbuff,lastr);						/* output the LAST string.		*/
			ptype = get_param(curstr);					/* Get the tested string.		*/
			stredt(inline,curstr,"");

			if (ptype == 1)
			{								/* New line needed, finish old line.	*/
				*curnl = 1;
			}
			else
			{
				*curnl = 0;
			}

			ptype = get_param(o_parms[1]);					/* Get the tested string.		*/
			stredt(inline,o_parms[1],"");
											/* next parm has open,it is subscripted	*/
			if (((strpos(curstr,"(") != -1) && (strpos(curstr,")") == -1)) || (o_parms[1][0] == '('))
			{								/* it has a subscript			*/
				strcat(curstr," ");					/* add a space				*/
				strcat(curstr,o_parms[1]);				/* add it on to the end of the name	*/
				while (strpos(o_parms[1],")") == -1)			/* no closing parentheses		*/
				{
					get_param(o_parms[1]);				/* get the next part of the sub script	*/
					if (o_parms[1][0] != ')')			/* a field is next, add a space		*/
					{
						strcat(curstr," ");
					}
					strcat(curstr,o_parms[1]);			/* add it on to the end of the name	*/
					stredt(inline,o_parms[1],"");			/* Remove from input line.		*/
				}
				write_log("WISP",'I',"SUBSCRMOVE",
							"Handle subscripted source field %s in MOVE WITH CONVERSION.",o_parms[0]);
				ptype = get_param(o_parms[1]);				/* get the tested string		*/
			}

			tput_line_at(16, "MOVE %s TO WISP-TEST-BYTE,\n",oldstr);	/* Output the test value.	*/
			tput_line_at(16, "CALL \"bit_test\" USING WISP-TEST-BYTE,");
			tput_clause (20, "%s,",curstr);
			tput_clause (20, "WISP-SCRATCH-BYTE-%d,",bytenum);
			tput_blank();

			strcpy(curstr,o_parms[1]);

			if (ptype == 1)
			{								/* New line needed, finish old line.	*/
				*curnl = 1;
			}
			else
			{
				*curnl = 0;
			}

			if (!strcmp(curstr,"IS"))
			{
				i = ptype;
				ptype = get_param(curstr);				/* Skip over IS.			*/
				if (i) ptype = i;

				stredt(inline,curstr,"");

				if (ptype == 1)
				{							/* New line needed, finish old line.	*/
					*curnl = 1;
				}
				else
				{
					*curnl = 0;
				}
			}

			negate = 0;

			if (!strcmp(curstr,"NOT"))					/* preceded by a NOT?			*/
			{
				ptype = get_param(curstr);				/* Skip over it.			*/
				stredt(inline,curstr,"");

				if (ptype == 1)
				{							/* New line needed, finish old line.	*/
					*curnl = 1;
				}
				else
				{
					*curnl = 0;
				}
				negate = 1;						/* add the the TRUE value.		*/
			}

			if ((i = strpos(curstr,")")) != -1)				/* If there is a closing paren.		*/
			{
				strcpy(pstr,&curstr[i]);				/* Copy the parens and stuff.		*/
				curstr[i] = '\0';
			}
			else
			{
				pstr[0] = '\0';						/* Blank it out.			*/
			}
											/* Check OFF or NOT ON or NOT OFF or ON	*/
			if ((!strcmp(curstr,"OFF") && !negate ) || (!strcmp(curstr,"ON") && negate ))
			{
				sprintf(tstr,"\n            NOT WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
			}
			else								/* Assume it is ON.			*/
			{
				sprintf(tstr,"\n            WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
			}

			strcat(ifbuff,tstr);

			*curstr = '\0';							/* Clear all the strings.		*/
			*oldstr = '\0';
			*lastr = '\0';
			bytenum++;
		}
		else
		{
			stredt(&inline[7],curstr,"");					/* remove them from the input		*/
			if (*lastnl) 	    strcat(ifbuff,"\n           ");		/* and the newline info.		*/
			else if (lastr[0]) strcat(ifbuff," ");
			strcat(ifbuff,lastr);						/* output the last string		*/
			tptr = lastr;
			tnl = lastnl;
			lastr = oldstr;							/* advance all the pointers.		*/
			lastnl = oldnl;
			oldstr = curstr;
			oldnl = curnl;
			curstr = tptr;
			curnl = tnl;
			*curstr = '\0';							/* blank out the current one.		*/
		}

	}
}
#endif /* OLD */


