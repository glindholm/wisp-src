/*
******************************************************************************
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
******************************************************************************
*/


#define EXT extern

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"
#include "pgcblsrc.h"
#include "pgextrct.h"

static char fldname1[FLDLEN];
static char fldliteral[10];
static int max_cobline = 55;

static void wrt_run(void);
static void wrt_key_params(void);							/* Write out the PUTPARM variable	*/
static void wrt_call_link(void);							/* Write out the Call to LINK.		*/
static void gen_compute(int gfl, int tfl, char htyp);		    			/* Generate the COMPUTE statement.	*/
static void concat_gfl_prefix(int fl);							/* Concatenate the global var's prefix.	*/
static void wrt_backref(int type);							/* Write out the backwards reference	*/
static void complete_assign(int ctyp, int num, char htyp,int gfl);			/* Next item is a string so generate	*/
static void wrt_print_kw(void);								/* Write the PRINT keyword stmnts.	*/
static void wrt_submit_kw(void);								/* Write the SUBMIT keyword stmnts.	*/
static void process_br_parms(void);							/* Check if any backwards ref.parms.	*/

void wrt_init_assign(void)
{
	int ostrt = 0, istrt, olen=0, gfl;
	char *tempfld, tstr[5];
	char ostpos[FLDLEN], olength[FLDLEN];
	char ospt[3], olt[3], htyp = 0;
	char in_fld[FLDLEN], in_type[3], in_strt[5], in_len[5];
	int str_fl = 0;
      
	write_log(util,'I','W',"WRITE","Writing the ASSIGN items.");

	if (*cur_assign->field1 == '&')
	{
		strcpy(fldname1,&cur_assign->field1[1]);				/* Hold the output string.		*/
		strcpy(fldliteral,cur_assign->literal);					/* Hold the output literal.		*/
		htyp = cur_assign->type[0];						/* Hold the output string type.		*/
	}
	else write_log(util,'W','W',"INVALID","Invalid ASSIGN, not a declare variable.");

	if (cur_assign->type[1] == 'E') gfl = 1;					/* See if an External Global.		*/
	else if (cur_assign->type[1] == 'G') gfl = 2;					/* else see if a Global.		*/
	else if (cur_assign->type[1] == 'L') gfl = 3;					/* else see if a Literal.		*/
	else if (cur_assign->type[1] == 'R') gfl = 4;					/* else see if a Return code.		*/
	else gfl = 0;									/* else see if a local indicator.	*/

	hld_assign = cur_assign;                    
	while (cur_assign)
	{
		if (cur_assign->br_flag) wrt_backref(4);				/* If have backwards ref., do first.	*/
		cur_assign = cur_assign->next_item;
	}
	cur_assign = hld_assign;

	if (*cur_assign->start_pos)							/* Generate a CALL STRING statement.	*/
	{
		hld_assign = cur_assign;
		cur_assign = cur_assign->next_item;					/* Don't do ASSIGN to yet.  So step to	*/
		while (cur_assign)							/*  the next item.			*/
		{
			if (cur_assign->sub_flag) wrt_subscript(1,2);			/* If substr, gen CALL STRING first.	*/
			cur_assign = cur_assign->next_item;
		}
		cur_assign = hld_assign;

		if (*cur_assign->field1 == '&') tempfld = cur_assign->field1+1;		/* Get the ptr to OUTPUT string.	*/
		else	tempfld = cur_assign->field1;

		if (*cur_assign->start_pos == '&')					/* Is the output start pos a var?	*/
		{
			strcpy(ostpos,cur_assign->start_pos);				/* Save the starting pos variable.	*/
			strcpy(ospt,cur_assign->stpos_type);				/* Save the type of var.		*/
		}
		else
		{
			ostrt = atoi(cur_assign->start_pos);				/* Convert output start pos to integer.	*/
			strcpy(ospt,"I ");
		}

		if (*cur_assign->length == '&')		 				/* Is the output length a var?		*/
		{
			strcpy(olength,cur_assign->length);				/* Save the length variable.		*/
			strcpy(olt,cur_assign->len_type);				/* Save the type of var.		*/
		}
		else
		{
			olen = len_to_int(cur_assign->length);				/* Convert output length to integer.	*/
			strcpy(olt,"I ");
		}

		str_fl = FALSE;								/* Set do not generate STRING.		*/
		hld_assign = cur_assign;
		cur_assign = cur_assign->next_item;					/* Move to INPUT struct.		*/
		if (cur_assign->next_item && !cur_assign->sub_flag)			/* If more than one field, gen STRING.	*/
		{
			assign_item *tmp_assign;
			int compute;

			str_fl = TRUE;
			compute = FALSE;						/* Assume is not a COMPUTE statement.	*/
			cur_assign = hld_assign;					/* Set ptr to ASSIGN to field.		*/
			strcpy(in_fld,cur_assign->field1);				/* Save fields for later processing.	*/
			strcat(in_fld,"-");
			strcpy(in_strt,cur_assign->start_pos);
			strcat(in_fld,in_strt);
			strcat(in_fld,"-");
			strcpy(in_len,cur_assign->length);
			strcat(in_fld,in_len);
			strcpy(in_type,cur_assign->type);

			cur_assign = cur_assign->next_item;
			tmp_assign = cur_assign;
			while (tmp_assign)
			{
				if (operand(*tmp_assign->field1)) compute = TRUE;
				tmp_assign = tmp_assign->next_item;
			}

			indent_line();							/* Add the correct indentation.		*/
			if (compute)
			{
				if (*in_type != 'I')
				{
					write_log(util,'E','W',"INVAL",
						"Invalid COMPUTE  - using string vars (%s,%s). Correction needed.",
							in_fld,cur_assign->field1);
				}
				strcat(cobline,"COMPUTE ");
				concat_var_prefix(in_type[1]);				/* Concatenate the var prefix.		*/
				strcat(cobline,&in_fld[1]);
				strcat(cobline," = ");
			}
			else strcat(cobline,"STRING ");

			while (cur_assign)
			{
				if (!cur_assign->sub_flag)
				{
					if (*cur_assign->field1 == '&')			/* Do I need to get rid of &.		*/
					{
						concat_var_prefix(cur_assign->type[1]);	/* Concatenate the var prefix.		*/
						strcat(cobline,&cur_assign->field1[1]);
					}
					else
					{
						if (!compute) strcat(cobline,"\"");	/* Load the quote.			*/
						strcat(cobline,cur_assign->field1);
						if (!compute) strcat(cobline,"\"");	/* Load the quote.			*/
					}

					if (compute)
					{
						strcat(cobline," ");
					}
					else
					{
						strcat(cobline," DELIMITED BY SIZE, ");
						end_prt_line(0);			/* Add end of line stuff and write line.*/
						indent_line();
						strcat(cobline,"       ");
					}
				}
				cur_assign = cur_assign->next_item;
			}

			if (!compute)
			{
				strcat(cobline,"INTO ");
				concat_var_prefix(in_fld[1]);				/* Concatenate the var prefix.		*/
				strcat(cobline,&in_fld[1]);
			}
			if (in_if) end_prt_line(0);					/* Add end of line stuff and write line.*/
			else end_prt_line(1);
		}

		cur_assign = hld_assign;
		if (*cur_assign->start_pos && cur_assign->next_item && cur_assign->next_item->sub_flag)
		{
			cur_assign = cur_assign->next_item;				/* If a substring on lhs and rhs.	*/
		}
		cur_assign = cur_assign->next_item;					/* Move to INPUT struct.		*/
		if (*cur_assign->start_pos == '&')					/* If a varialbe then need to subtract	*/
		{									/* one from value in variable to get	*/
			indent_line();							/* the correct OFFSET.			*/
			strcat(cobline,"COMPUTE ");
			concat_var_prefix(cur_assign->stpos_type[1]);
			strcat(cobline,&cur_assign->start_pos[1]);
			strcat(cobline," = ");
			concat_var_prefix(cur_assign->stpos_type[1]);
			strcat(cobline,&cur_assign->start_pos[1]);
			strcat(cobline," - 1");
			if (in_if) end_prt_line(0);					/* Add end of line stuff and write line.*/
			else end_prt_line(1);
		}

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (str_fl) strcat(cobline,"0");
		else if (*cur_assign->start_pos)					/* If have an INPUT substring.		*/
		{
			if (*cur_assign->start_pos == '&')
			{
				concat_var_prefix(cur_assign->stpos_type[1]);		/* Concatenate the INPUT prefix.	*/
				strcat(cobline,&cur_assign->start_pos[1]);		/* Load the INPUT start position.	*/
			}
			else
			{
				istrt = atoi(cur_assign->start_pos);			/* Convert start pos to integer.	*/
				sprintf(tstr,"%d",istrt - 1);
				strcat(cobline,tstr);					/* Load OFFSET into output.		*/
			}
		}
		else strcat(cobline,"0");

		strcat(cobline," TO STR-IN-INDEX-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (str_fl) strcat(cobline,in_len);					/* Load the length.			*/
		else if (*cur_assign->length == '(')
		{
			char *cstr, *tpos, tlen[FLDLEN];

			cstr = tlen;
			tpos = &cur_assign->length[1];
			while (*tpos != ')') *cstr++ = *tpos++;				/* Copy length without ().		*/
			*cstr = '\0';							/* Null terminate the string.		*/
			strcat(cobline,tlen);						/* Load the length.			*/
		}
		else if (*cur_assign->length == '&')					/* If the length is a variable.		*/
		{
			concat_var_prefix(cur_assign->len_type[1]);			/* Concatenate the INPUT prefix.	*/
			strcat(cobline,&cur_assign->length[1]);				/* Load the INPUT length position.	*/
		}
		else if (*cur_assign->type == 'I')
		{
			strcat (cobline,"1");						/* Is an integer.			*/
		}
		else strcat(cobline,cur_assign->length);				/* Load the length.			*/

		strcat(cobline," TO STR-IN-LEN-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		if (*ostpos == '&')
		{
			indent_line();							/* the correct OFFSET.			*/
			strcat(cobline,"COMPUTE ");
			concat_var_prefix(ospt[1]);					/* Concatenate the OUTPUT prefix.	*/
			strcat(cobline,&ostpos[1]);					/* Load the OFFSET position.		*/
			strcat(cobline," = ");
			concat_var_prefix(ospt[1]);
			strcat(cobline,&ostpos[1]);
			strcat(cobline," - 1");
			if (in_if) end_prt_line(0);					/* Add end of line stuff and write line.*/
			else end_prt_line(1);
		}

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (*ospt != 'I' || *ostpos == '&')
		{
			concat_var_prefix(ospt[1]);					/* Concatenate the OUTPUT prefix.	*/
			strcat(cobline,&ostpos[1]);					/* Load the OFFSET position.		*/
		}
		else
		{
		 	sprintf(tstr,"%d",ostrt - 1);
			strcat(cobline,tstr);						/* Load OFFSET into output.		*/
		}
		strcat(cobline," TO STR-OUT-INDEX-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (*olt != 'I')
		{
			concat_var_prefix(olt[1]);					/* Concatenate the OUTPUT prefix.	*/
			strcat(cobline,&olength[1]);					/* Load the LENGTH position.		*/
		}
		else
		{
			sprintf(tstr,"%d",olen);
			strcat(cobline,tstr);		 				/* Load length for output.		*/
		}
		strcat(cobline," TO STR-OUT-LEN-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"CALL \"STRING\" USING STR-FUNC, ");

		if (str_fl)
		{
			concat_var_prefix(in_type[1]);					/* Concatenate the INPUT var's prefix.	*/
			strcat(cobline,&in_fld[1]);					/* Load the INPUT var.			*/
		}
		else if (cur_assign->br_flag)
		{
			strcat(cobline,cur_assign->field1);				/* Load the INPUT var.			*/
		}
		else
		{                                                                                 
			concat_var_prefix(cur_assign->type[1]);				/* Concatenate the INPUT var's prefix.	*/
			strcat(cobline,&cur_assign->field1[1]);				/* Load the INPUT var.			*/
		}
		end_prt_line(2);							/* Add end of line stuff and write line.*/
		
		strcpy(cobline,"                          ");
		if (in_if) strcat(cobline,"     ");					/* Load the indent if in if statement.	*/
		strcat(cobline,"STR-INPUT-INDEX, STR-INPUT-LENGTH");
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"                          ");
		if (in_if) strcat(cobline,"     ");					/* Load the indent if in if statement.	*/
		concat_gfl_prefix(gfl);							/* Concatenate the global var's prefix.	*/
		strcat(cobline,tempfld);						/* Load the OUTPUT var.			*/
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"                          ");
		if (in_if) strcat(cobline,"     ");					/* Load the indent if in if statement.	*/
		strcat(cobline,"STR-OUTPUT-INDEX, STR-OUTPUT-LENGTH");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
	}
	else										/* else generate a STRING statement.	*/
	{
		int num, cfl, sfl, cnt;

		cfl = FALSE;								/* Set flag to assume no compute stmnt.	*/
		sfl = FALSE;								/* Set flag to assume no string stmnt.	*/
		cnt = 0;								/* Assume no substring vars.		*/
		cur_assign = cur_assign->next_item;
		hld_assign = cur_assign;						/* Keep for next loop.			*/
		num = 0;
		while (cur_assign)
		{
			num++;
			if (cur_assign->type[1] == 'C') cfl = TRUE;			/* Generate a COMPUTE statement.	*/
			else if (cur_assign->sub_flag)
			{
				sfl = TRUE;						/* Generate a CALL STRING statement.	*/
				cnt++;
			}
			cur_assign = cur_assign->next_item;
		}
		cur_assign = hld_assign;						/* Set ptr to back.			*/

		if (sfl)
		{
			wrt_subscript(cnt,2);						/* If substr, gen CALL STRING first.	*/
			cur_assign = hld_assign;					/* Set ptr back.			*/
			if (num == 2)
			{
				num--;							/* Is 1 field with a substring so set	*/
				cur_assign = cur_assign->next_item;			/* So will do a MOVE and not STRING.	*/
			}
		}
 
		if (cfl) gen_compute(gfl,0,htyp);					/* Complete the COMPUTE statement,	*/
		else 									/* else do a STRING or MOVE.		*/
		{
			complete_assign(0,num,htyp,gfl);				/* Complete the MOVE or STRING code for	*/
		}									/*  the ASSIGN statement.		*/
	}
	if (!str_fl && cur_assign && cur_assign->next_item)
	{
		if (*cur_assign->next_item->type == 'S') complete_assign(1,2,htyp,gfl);
		else gen_compute(gfl,1,htyp);						/* Complete the COMPUTE statement,	*/
	}
}

void wrt_call(void)									/* Handles syntax for call verbs.	*/
{
	write_log(util,'I','W',"WRITE","Writing the CALL syntax.");

	if (genccall)	strcpy(cobline,"      *%NeoMedia Migrations - Verification of ");
	else	strcpy(cobline,"%NeoMedia Migrations - Verification of ");

	strcat(cobline,cur_cmd->command);						/* Load the PERFORM.			*/
	strcat(cobline," P-");								/* Load the Prefix.			*/
	strcat(cobline,cur_cmd->call_para);						/* Load the paragraph name.		*/
	strcat(cobline," logic needed");
	end_prt_line(1);								/* Add end of line stuff and write line.*/

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,cur_cmd->command);						/* Load the PERFORM.			*/
	strcat(cobline," P-");								/* Load the Prefix.			*/
	strcat(cobline,cur_cmd->call_para);						/* Load the paragraph name.		*/
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

void wrt_goto(void)									/* Handles syntax for goto statement.	*/
{
	write_log("PROCTRAN",'I','W',"WRITE","Writing the GOTO items.");
                                    
	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,cur_cmd->command);						/* Load the GOTO.			*/
	if (*cur_cmd->goto_para == '&')							/* Do I need to get rid of &.		*/
	{
		write_log("PROCTRAN",'E','W',
			"INVALID","Invalid GOTO statement.  Canot use (%s) as GOTO label.",cur_cmd->goto_para);
		strcat(cobline," ");
		concat_var_prefix(cur_cmd->gotop_type[1]);				/* Concatenate the var prefix.		*/
		strcat(cobline,&cur_cmd->goto_para[1]);
	}
	else
	{
		strcat(cobline," P-");							/* Load the Prefix.			*/
		strcat(cobline,cur_cmd->goto_para);					/* Load the paragraph name.		*/
	}
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

void wrt_if(void)									/* Writes syntax for if statements.	*/
{
	char cobol_line[STRBUFF];
	int i, nfl, pf_fl;
	if_item *hld_if = NULL;

	write_log("PROCTRAN",'I','W',"WRITE","Writing the IF items.");

	if (*cur_if->cond == '1')
	{
		char *cstrp = NULL, ctype = '\0';
		int br;

		if (cur_if->sub_flag) wrt_subscript(1,6);				/* If substr, gen CALL STRING first.	*/
		if (cur_if->br_flag) wrt_backref(2);					/* If have backwards ref., do first.	*/

		for (i = 0; i < 3; i++)
		{
			strcpy(cobline,"           MOVE ");
			br = 0;								/* Set flag so not backwards ref.	*/
			if (i == 0)
			{
				if (cur_if->sub_flag)
				{
					cstrp = cur_if->var;				/* Assign the current file.		*/
					ctype = cur_if->type[1];			/* Assign current file type indicator.	*/
				}
				else
				{
					cstrp = cur_if->file;				/* Assign the current file.		*/
					ctype = cur_if->file_type[1];			/* Assign current file type indicator.	*/
				}
				if (cur_if->file_br) br = 1;
			}
			else if (i == 1)
			{
				cstrp = cur_if->lib;
				ctype = cur_if->lib_type[1];				/* Assign current file type indicator.	*/
				if (cur_if->lib_br) br = 1;
			}
			else if (i == 2)
			{
				cstrp = cur_if->vol;
				ctype = cur_if->vol_type[1];				/* Assign current file type indicator.	*/
				if (cur_if->vol_br) br = 1;
			}

			if (*cstrp == '&')						/* Get do i need to get rid of &.	*/
			{
				concat_var_prefix(ctype);				/* Concatenate the var prefix.		*/
				cstrp++;
				strcat(cobline,cstrp);
			}
			else if (br)							/* Is backwards referenced so no quptes.*/
			{
				strcat(cobline,cstrp);
			}
			else if (*cstrp != ' ' && *cstrp != '\0')			/* Do I have a variable.		*/
			{
				strcat(cobline,"\"");					/* Load the quote.			*/
				strcat(cobline,cstrp);
				strcat(cobline,"\"");					/* Load the quote.			*/
			}
			else	strcat(cobline,"SPACES");

			strcat(cobline," TO ");

			if (i == 0) strcat(cobline,"WSA-FILE-NAME");
			else if (i == 1) strcat(cobline,"WSA-LIB-NAME");
			else if (i == 2) strcat(cobline,"WSA-VOL-NAME");

			end_prt_line(1);						/* Add end of line stuff and write line.*/
		}
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"PERFORM FIND-FILE-INFO");
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		if (cur_if->cond[1] == '1') strcpy(cobline,"           IF ENTRY-COUNT = 0"); /* didn't find.			*/
		else	strcpy(cobline,"           IF ENTRY-COUNT > 0");		    /* found files.			*/
		end_prt_line(0);							/* Add end of line stuff and write line.*/
	}
	else
	{
		int cnt, sp;								/* Count of items with subscript.	*/

		cnt = 0;								/* Set to no items with subscript.	*/
		if (cur_if->sub_flag || (cur_if->next_item && cur_if->next_item->sub_flag)) /* If have subscript, do first.	*/
		{
			if (cur_if->sub_flag)
			{
				cnt = 1;						/* Set first field has subscript.	*/
				hld_if = cur_if->next_item;				/* Save ptr to current if structure.	*/
			}
			if (cur_if->next_item && cur_if->next_item->sub_flag)
			{
				if (cnt)
				{
					cnt = 3;					/* Set that both fields have subscript	*/
					hld_if = cur_if->next_item;			/* Save ptr to current if structure.	*/
				}
				else
				{
					cnt = 2;					/*  else set second field has subscript.*/
					hld_if = cur_if;				/* Save ptr to current if structure.	*/
				}
			}
			wrt_subscript(cnt,1);						/* Generate a CALL STRING for subscript.*/
			cur_if = hld_if;						/* Set ptr back to current structure.	*/
		}
		i = 0;									/* Set the counter to 0.		*/
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,cur_cmd->command);					/* Load the IF.				*/
		pf_fl = 0;								/* Set flag is not the pfkey variable.	*/
		while (cur_if)
		{
			i++;								/* Bump the count.			*/
			if (*cur_if->paren_value)
			{
				if (strcmp(cur_if->paren_value,")") &&  strcmp(cur_if->paren_value,"("))
			  	{							/* Put out error message if warning in if.*/
					write_log("PROCTRAN",'E','W',"INVALIF","Index found in IF statement. Correction needed.");
				}
				strcpy(cobol_line,"123456* FOUND an index ref in IF please check."); /* Load the Comment.	*/
				strcat(cobol_line,cur_if->paren_value);			/* Load the exception.			*/
				end_prt_line(1);					/* Add end of line stuff and write line.*/
			}

			if (!pf_fl && (sp=strpos(cur_if->var,"KEY") >= 0)) pf_fl = 1;	/* Set processing pfkey field.		*/

			if (*cur_if->var == '&')	
			{
				strcat(cobline," ");					/* Load the ".				*/
				if (!strcmp(cur_if->paren_value,"("))
				{
					strcat(cobline,"(");				/* Load the ( for logic.		*/
				}
				if (0==strcmp(cur_if->var,"&TIME") || 0==strcmp(cur_if->var,"&DATE") )
				{							/* Is the TIME or DATE function.	*/
					if (*cur_if->type == 'I') strcat(cobline,"WSA-N");/* Is the numeric type variable.	*/
					else	strcat(cobline,"WSA-A");		/* Is alpha type variable.		*/
				}
				else
				{
					concat_var_prefix(cur_if->type[1]);		/* Concatenate the var prefix.		*/
				}
				strcat(cobline,&cur_if->var[1]);
				if (cur_if->type[1] == 'R')
				{
					strcat(cobline,"-2");
				}
				if (!strcmp(cur_if->paren_value,")"))
				{
					strcat(cobline,")");				/* Load the ( for logic.		*/
				}
			}
			else
			{								/* Set nfl, test  if need quote.	*/
				if (pf_fl)
				{

					*cur_if->type ='S';				/* Make sure pfkey test is to a string.*/
					sprintf(cur_if->var,"%2s",cur_if->var);		/* Load the string variable.		*/
				}

				if (0==*cur_if->var || *cur_if->type == 'S') nfl = 0; 	/* No value or was typed as a string.	*/
				else nfl = chk_num(cur_if->var);			/* Check to see if number.		*/

				strcat(cobline," ");					/* Load the ".				*/
				if (!strcmp(cur_if->var,"RETURN-CODE")) nfl = 1;
				if (!strcmp(cur_if->paren_value,"("))
				{
					strcat(cobline,"(");				/* Load the ( for logic.		*/
				}
				if (!nfl) strcat(cobline,"\"");
				strcat(cobline,cur_if->var);				/* Load the string variable.		*/
				if (!nfl) strcat(cobline,"\"");
				if (!strcmp(cur_if->paren_value,")"))
				{
					strcat(cobline,")");				/* Load the ( for logic.		*/
				}
			}
			if (*cur_if->cond == 'A') strcat(cobline," AND");		/* Load the and.			*/
			else if (*cur_if->cond == 'B') strcat(cobline," NOT <");	/* Load the Not <.			*/
			else if (*cur_if->cond == 'C') strcat(cobline," NOT >");	/* Load the Not >.			*/
			else if (*cur_if->cond == 'D') strcat(cobline," >=");		/* Load the >=.				*/
			else if (*cur_if->cond == 'E') strcat(cobline," =");		/* Load the  =.				*/
			else if (*cur_if->cond == 'F') strcat(cobline," <=");		/* Load the <=.				*/
			else if (*cur_if->cond == 'G') strcat(cobline," >");		/* Load the  >.				*/
			else if (*cur_if->cond == 'L') strcat(cobline," <");		/* Load the  <.				*/
			else if (*cur_if->cond == 'N') strcat(cobline," NOT =");	/* Load the Not =.			*/
			else if (*cur_if->cond == 'O') strcat(cobline," OR");		/* Load the OR.				*/

			cur_if = cur_if->next_item;					/* Point to next one.			*/

			if (cur_if && cur_if->sub_flag) cur_if = cur_if->next_item;	/* Already processed in substring.	*/

			if (i == 2 || !cur_if)
			{
				i = 0;
				end_prt_line(0);					/* Add end of line stuff and write line.*/
				indent_line();						/* Add the correct indentation.		*/
			}
		}
	}
}

void wrt_program(void)									/* Writes out syntax for the run,	*/
{											/*  submit, print, commands.		*/
	int i;

	if (cur_prg->sub_flag)
	{
		cur_pp = cur_prg->pparm_area;
		while (cur_pp)
		{
			cur_pp_key = cur_pp->kwlist;
			while (cur_pp_key)
			{
				if (cur_pp_key->sub_flag)
				{
					wrt_subscript(1,3);				/* If substr, gen CALL STRING first.	*/
				}
				cur_pp_key = cur_pp_key->next_item;
			}
			cur_pp = cur_pp->next_item;
		}
	}

	if (cur_prg->pparm_area)							/* Do we have a PUTPARM call.		*/
	{										/* Write the PUTPARM call source for	*/
		if (cur_prg->func =='P') wrt_print_kw();				/* Is a PRINT command.			*/
		else if (cur_prg->func =='S') wrt_submit_kw();				/* Is a SUBMIT command.			*/
		else wrt_key_params();							/* the current command.			*/
	}

	if (cur_prg->br_flag) wrt_backref(7);						/* If have backwards ref., do next.	*/

	if (cur_prg->func == 'B')							/* Is is a CALL "SYSTEM" USING BUFFER?	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"CALL \"SYSTEM\" USING WCOPY-BUFFER");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
	}
	else if (cur_prg->func == 'R')							/* Is it a RUN command?			*/
	{
		wrt_run();								/* Write the RUN command.		*/
	}
	else										/* else write the SUBMIT or PRINT cmd.	*/
	{
		for (i = 0; i < 3; i++)
		{
			char *cstrp = NULL, ctype = '\0';
			int cbr =0;

			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"MOVE ");
			if (i == 0)
			{
				cstrp = cur_prg->name;
				ctype = cur_prg->name_type[1];
				cbr = cur_prg->name_br;
			}
			else if (i == 1)
			{
				cstrp = cur_prg->lib;
				ctype = cur_prg->lib_type[1];
				cbr = cur_prg->lib_br;
			}
			else if (i == 2) 
			{
				cstrp = cur_prg->vol;
				ctype = cur_prg->vol_type[1];
				cbr = cur_prg->vol_br;
			}

			if (*cstrp == '&')						/* Get do i need to get rid of &.	*/
			{
				concat_var_prefix(ctype);				/* Concatenate the var prefix.		*/
				cstrp++;
				strcat(cobline,cstrp);
			}
			else if (*cstrp != ' ' && *cstrp != '\0')			/* Do I have a variable.		*/
			{
				if (!cbr) strcat(cobline,"\"");				/* Load the quote.			*/
				strcat(cobline,cstrp);
				if (!cbr) strcat(cobline,"\"");				/* Load the quote.			*/
			}
			else	strcat(cobline,"SPACES");

			strcat(cobline," TO ");

			if (i == 0)	 strcat(cobline,"WSA-FILE-NAME");		/* Load the file.			*/
			else if (i == 1) strcat(cobline,"WSA-LIB-NAME");		/* Load the library.			*/
			else if (i == 2) strcat(cobline,"WSA-VOL-NAME");		/* Load the volume.			*/

			if (in_if) end_prt_line(0);					/* Add end of line stuff and write line.*/
			else end_prt_line(1);
		}

		indent_line();								/* Add the correct indentation.		*/
		if (cur_prg->func == 'S') strcat(cobline,"PERFORM SUBMIT-PROGRAM-AREA");
		else if (cur_prg->func == 'P') strcat(cobline,"PERFORM PRINT-FILE-SUB");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
	}
}

static void wrt_run(void)								/* Writes out syntax for the run command.*/
{
	int i;
	int br;

	if (cur_prg->using_name) process_br_parms();					/* Check if any backwards ref.parms.	*/
											/*  and process getparm in needed.	*/
	for (i = 0; i < 3; i++)
	{
		char *cstrp = NULL, ctype = 0;

		indent_line();								/* Add the correct indentation.		*/
		br = 0;
		if (i == 1 && (*cur_prg->lib2 != '\0')) strcat(cobline,"STRING ");
		else strcat(cobline,"MOVE ");
		if (i == 0)
		{
			cstrp = cur_prg->name;
			ctype = cur_prg->name_type[1];
			if (cur_prg->name_br) br = 1;
		}
		else if (i == 1)                                          
		{
			cstrp = cur_prg->lib;
			ctype = cur_prg->lib_type[1];
			if (cur_prg->lib_br) br = 1;
		}
		else if (i == 2) 
		{
			cstrp = cur_prg->vol;
			ctype = cur_prg->vol_type[1];
			if (cur_prg->vol_br) br = 1;
		}

		if (*cstrp == '&')							/* Do I need to get rid of &.		*/
		{
			concat_var_prefix(ctype);					/* Concatenate the var prefix.		*/
			cstrp++;
			strcat(cobline,cstrp);
		}
		else if (br)								/* Is backwards referenced so no quptes.*/
		{
			strcat(cobline,cstrp);
		}
		else if (*cstrp != ' ' && *cstrp != '\0')				/* Do I have a variable.		*/
		{
			strcat(cobline,"\"");						/* Load the quote.			*/
			strcat(cobline,cstrp);
			strcat(cobline,"\"");						/* Load the quote.			*/
		}
		else	strcat(cobline,"SPACES");

		if (i == 1 && (*cur_prg->lib2 != '\0'))
		{
			strcat(cobline,", ");
			cstrp = cur_prg->lib2;
			ctype = cur_prg->lib2_type[1];
			if (*cstrp == '&')						/* Get do i need to get rid of &.	*/
			{
				concat_var_prefix(ctype);				/* Concatenate the var prefix.		*/
				cstrp++;
				strcat(cobline,cstrp);
			}
			else
			{
				strcat(cobline,"\"");					/* Load the quote.			*/
				strcat(cobline,cstrp);
				strcat(cobline,"\"");					/* Load the quote.			*/
			}
			end_prt_line(0);						/* Add end of line stuff and write line.*/
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"   DELIMITED BY SIZE INTO ");
		}
		else strcat(cobline," TO ");

		if (i == 0)	 strcat(cobline,"LINK-TO-NAME");			/* Load the file.			*/
		else if (i == 1) strcat(cobline,"LINK-LIBRARY");			/* Load the library.			*/
		else if (i == 2) strcat(cobline,"LINK-VOLUME");				/* Load the volume.			*/
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
	}

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE ");
	if (0==strcmp(cur_prg->name,"SORT")) strcat(cobline,"\"S\"");			/* Set LINK-TYPE to S for SORT.		*/
	else if (0==strcmp(cur_prg->name,"COPY")) strcat(cobline,"\"S\"");		/* Set LINK-TYPE to S for COPY.		*/
	else if (*cur_prg->lib == '\0' && *cur_prg->vol == '\0')
	{
		if (genslink) strcat(cobline,"\"S\"");
		else strcat(cobline,"\" \"");
	}
	else strcat(cobline,"\"P\"");
	strcat(cobline," TO LINK-TYPE");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	wrt_call_link();
}

void wrt_return(void)									/* Handles syntax for the RETURN verb.	*/
{
	write_log("PROCTRAN",'I','W',"WRITE","Writing the RETURN items.");

	cur_rtrn = cur_cmd->return_area;						/* Set ptr to start of return list.	*/
	if (*cur_rtrn->var != ' ' && *cur_rtrn->var != '\0' && *cur_rtrn->var != 'N' && 
	     strcmp(cur_rtrn->var,"RETURN-CODE"))				 	/* If have a return field and field not	*/
	{										/*  equal to RETURN-CODE.		*/
		indent_line();								/* Add the correct indentation.		*/
		if (cur_rtrn->next_item)						/* If is an equation.			*/
		{
			strcat(cobline,"COMPUTE RETURN-CODE = ");
			while (cur_rtrn)
			{
				if (*cur_rtrn->var == '&')
				{
					concat_var_prefix(cur_rtrn->type[1]);		/* Concatenate the var prefix.		*/
					strcat(cobline,&cur_rtrn->var[1]);
				}
				else strcat(cobline,cur_rtrn->var);			/* Load the value.			*/
				strcat(cobline," ");
				cur_rtrn = cur_rtrn->next_item;				/* Set ptr to next item.		*/
			}
		}
		else
		{
			strcat(cobline,"MOVE ");
			if (*cur_prg->name != ' ' && *cur_prg->name != '\0')		/* Do I have a variable.		*/
			{
				if (*cur_rtrn->var == '&')
				{
					concat_var_prefix(cur_rtrn->type[1]);		/* Concatenate the var prefix.		*/
					strcat(cobline,&cur_rtrn->var[1]);
				}
				else strcat(cobline,cur_rtrn->var);			/* Load the value.			*/
			}
			strcat(cobline," TO RETURN-CODE");
		}
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		strcpy(cobline,"      *$UNIX_CODE  PROCESS");
		end_prt_line(0);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"      *         CALL \"SETRETCODE\" USING RETURN-CODE");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		strcpy(cobline,"      *$UNIX_END");
		end_prt_line(0);							/* Add end of line stuff and write line.*/
	}

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,cur_cmd->command);						/* Load the Command exit or stop.	*/
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

void wrt_rs(void)									/* Write out renames and scratchs.	*/
{
	char temp[40], *tptr, tmpvar[FLDLEN];
	int i;

	write_log("PROCTRAN",'I','W',"WRITE","Writing the RENAME/SCRATCH items.");

	if (cur_ren_sctch->sub_flag) wrt_subscript(1,5);				/* If substr, gen CALL STRING first.	*/
	if (cur_ren_sctch->br_flag) wrt_backref(3);					/* If have backwards ref., do first.	*/

	for (i = 0; i < 5; i++)
	{
		char *cstrp = NULL, ctype='\0';

		if (i > 2 && cur_ren_sctch->func == 'S') break;				/* If in scratch don't use 3 and 4.	*/

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (i == 0)
		{
			if (cur_ren_sctch->sub_flag)
			{
				strcpy(tmpvar,cur_ren_sctch->name);
				strcat(tmpvar,"-");
				strcat(tmpvar,cur_ren_sctch->start_pos);
				strcat(tmpvar,"-");
				strcat(tmpvar,cur_ren_sctch->length);
				cstrp = tmpvar;						/* Set ptr to the needed item string.	*/
				ctype = cur_ren_sctch->name_type[1];
			}
			else
			{
				cstrp = cur_ren_sctch->name;				/* Set ptr to the needed item string.	*/
				ctype = cur_ren_sctch->name_type[1];
			}
		}
		else if (i == 1)
		{
			cstrp = cur_ren_sctch->lib;
			ctype = cur_ren_sctch->lib_type[1];
		}
		else if (i == 2)
		{
			cstrp = cur_ren_sctch->vol;
			ctype = cur_ren_sctch->vol_type[1];
		}
		else if (i == 3)
		{
			cstrp = cur_ren_sctch->to_file;
			ctype = cur_ren_sctch->to_file_type[1];
		}
		else if (i == 4)
		{
			if (*cur_ren_sctch->to_lib == '\0')				/* If no library specified, use from	*/
			{								/*  library parameter.			*/
				cstrp = cur_ren_sctch->lib;
				ctype = cur_ren_sctch->lib_type[1];
			}
			else
			{
				cstrp = cur_ren_sctch->to_lib;
				ctype = cur_ren_sctch->to_lib_type[1];
			}
		}

		if (*cstrp == '&')							/* Get do i need to get rid of &.	*/
		{
			concat_var_prefix(ctype);					/* Concatenate the var prefix.		*/
			cstrp++;
			strcat(cobline,cstrp);
		}
		else if (ctype == 'B')							/* Do I have a backwards referenced var.*/
		{
			strcat(cobline,cstrp);
		}
		else if (*cstrp != ' ' && *cstrp != '\0')				/* Do I have a string?			*/
		{
			if (*cstrp == '\'')
			{
				tptr = temp;						/* Set prt to temp field.		*/
				cstrp++;						/* Step past the quote.			*/
				while (*cstrp != '\'') *tptr++ = *cstrp++;		/* Copy the string.			*/
				*tptr = '\0';						/* Null terminate the temp field.	*/
				cstrp = temp;						/* Set ptr to the temp field.		*/
			}

			strcat(cobline,"\"");						/* Load the quote.			*/
			strcat(cobline,cstrp);
			strcat(cobline,"\"");						/* Load the quote.			*/
		}
		else	strcat(cobline,"SPACES");

		strcat(cobline," TO ");

		if (i == 0)	 strcat(cobline,"WSA-FILE-NAME");
		else if (i == 1) strcat(cobline,"WSA-LIB-NAME");
		else if (i == 2) strcat(cobline,"WSA-VOL-NAME");
		else if (i == 3) strcat(cobline,"RENAME-FILE ");
		else if (i == 4) strcat(cobline,"RENAME-LIB ");

		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
	}
	indent_line();									/* Add the correct indentation.		*/
	if (cur_ren_sctch->opt == 'L')
	{
		if (cur_ren_sctch->func == 'R') strcat(cobline,"MOVE \"L\" TO RENAME-TYPE");
		else	strcat(cobline,"MOVE \"L\" TO SCRATCH-TYPE");
	}
	else if (*cur_ren_sctch->to_file != '\0' && *cur_ren_sctch->to_lib != '\0') 
	{
		strcat(cobline,"MOVE \"G\" TO RENAME-TYPE");
	}
	else 
	{ 	
		if (cur_ren_sctch->func == 'R') strcat(cobline,"MOVE \"F\" TO RENAME-TYPE");
		else	strcat(cobline,"MOVE \"F\" TO SCRATCH-TYPE");
	}
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	indent_line();									/* Add the correct indentation.		*/
	if (cur_ren_sctch->func == 'R') strcat(cobline,"PERFORM RENAME-FILE-SUB");
	else if (cur_ren_sctch->func == 'S') strcat(cobline,"PERFORM SCRATCH-FILE-SUB");
	end_prt_line(1);								/* Add end of line stuff and write line.*/

	if (*cur_ren_sctch->lbl)							/* If func has label assigned to it.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		if (cur_ren_sctch->func == 'R') strcat(cobline,"MOVE RENAME-R-2 TO ");
		else if (cur_ren_sctch->func == 'S') strcat(cobline,"MOVE SCRATCH-R-2 TO ");
		concat_var_prefix('R');							/* Concatenate the var prefix.		*/
		strcat(cobline,cur_ren_sctch->lbl);					/* Add the label assignmed to func.	*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/
	}
}

void wrt_set_extract(void)								/* Write out set and extract syntax.	*/
{
	set_extract_item *f_se, *h_se ;
	char cobol_line[STRBUFF];
	char hld_var[FLDLEN];
	char keywrd[3];
	int i, tndx, hndx, hld_type=0;

	write_log("PROCTRAN",'I','W',"WRITE","Writing the SET/EXTRACT items.");

	while (cur_set_extr)
	{
		hndx = -1;								/* Set hold index to not found.		*/
		f_se = cur_set_extr;							/* Save ptr for later.			*/
		for (i = 0; i < 2; i++)
		{
			if (cur_set_extr->br_flag) wrt_backref(8);			/* If have backwards ref., do first.	*/
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"MOVE ");
			tndx = find_keyword(cur_set_extr->var,search_set_extract);	/* Seacrh the table for keywords.	*/
			if (tndx >= 0)
			{
				strcpy(keywrd,keyword_set_extract[tndx]);		/* Hold the 2 char keyword to use.	*/
				hndx = tndx;						/* Hold the index in array.		*/
				strcat(cobline,keywrd);					/* Load the keyword.			*/
				strcat(cobline," TO WSA-KEYWORD");
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);
			}
			else
			{
				if (*cur_set_extr->key)
				{
					if (strcmp(cur_set_extr->key,")") &&  strcmp(cur_set_extr->key,"("))
		  			{						/* Put out error message .		*/
						write_log("PROCTRAN",'E','W',"INVALID",
							"Index found in SET/EXTRACT statement. Correction needed.");
					}
					strcpy(cobol_line,"123456* FOUND an index ref in SET/EXTRCT please check. ");
					strcat(cobol_line,cur_set_extr->key);		/* Load the exception.			*/
					end_prt_line(1);				/* Add end of line stuff and write line.*/
				}

				if (*cur_set_extr->var == '&')	 			/* Is a variable.			*/ 
				{
					concat_var_prefix(cur_set_extr->type[1]);	/* Concatenate the var prefix.		*/
					if (cur_set_extr->type[1] == 'E') hld_type = 1;	/* See if an External Global.		*/
					else if (cur_set_extr->type[1] == 'G') hld_type = 2; /* else see if a Global.		*/
					else if (cur_set_extr->type[1] == 'L') hld_type = 3; /* else see if a Literal.		*/
					else if (cur_set_extr->type[1] == 'R') hld_type = 4; /* else see if a Return code.	*/
					else hld_type = 0;				/* else see if a local indicator.	*/

					strcat(cobline,&cur_set_extr->var[1]);
					strcpy(hld_var,cur_set_extr->var);		/* Save var for move later on (EXTRACT)	*/
					if (*cur_set_extr->start_pos)			/* Is a substring so process accordingly*/
					{
						strcat(cobline,"-");
						strcat(hld_var,"-");
						strcat(cobline,cur_set_extr->start_pos);
						strcat(hld_var,cur_set_extr->start_pos);
						strcat(cobline,"-");
						strcat(hld_var,"-");
						strcat(cobline,cur_set_extr->length);
						strcat(hld_var,cur_set_extr->length);
					}
				}
				else if (cur_set_extr->br_flag)
				{
					strcat(cobline,cur_set_extr->var);
				}
				else if (*cur_set_extr->var == '\0') 			/* Is a null value.			*/
				{
					strcat(cobline,"SPACES");
				}
				else if (chk_num(cur_set_extr->var)) 			/* Is a number.				*/
				{
					strcat(cobline,cur_set_extr->var);
				}
				else 							/* Is an alpha numeric string.		*/ 
				{
					strcat(cobline,"\"");				/* Load the quote symbol.		*/
					strcat(cobline,cur_set_extr->var);		/* Load the value.			*/
					strcat(cobline,"\"");				/* Load the quote symbol.		*/
				}
				strcat(cobline," TO ");

				if (hndx  == FORM ||   hndx == JOBLMT || hndx == LINES || 
				     hndx == PRNTER || hndx == WS ||     hndx == SPLSRR)
				{							/* Is an integer keywrd.		*/
					strcat(cobline,"WSA-NUM-2");
				}
				else	strcat(cobline,"WSA-VARIABLE");
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);
			}
			cur_set_extr = cur_set_extr->next_item;				/* Point to next one.			*/
		}

		indent_line();								/* Add the correct indentation.		*/
		if (*cur_cmd->command == 'E')						/* Is is the EXTRACT command?		*/
		{
			if (hndx  == FORM ||   hndx == JOBLMT || hndx == LINES ||
                             hndx == PRNTER || hndx == WS ||     hndx == SPLSRR)
			{								/* Is an integer keyword.		*/
				strcat(cobline,"PERFORM EXTRACT-NUMERAL");
			}
			else	strcat(cobline,"PERFORM EXTRACT-VARIABLE");
		}
		else if (*cur_cmd->command == 'S')					/* Is is the SET command?		*/
		{
			if (hndx  == FORM ||   hndx == JOBLMT || hndx == LINES ||
			     hndx == PRNTER || hndx == WS ||     hndx == SPLSRR)
			{								/* Is an integer keyword.		*/
				strcat(cobline,"PERFORM SET-NUMERAL");
			}
			else	strcat(cobline,"PERFORM SET-VARIABLE");
		}
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		if (*cur_cmd->command == 'E' && *hld_var == '&') 			/* Is a variable and EXTRACT command?	*/
		{									/* Need to move return value into var.	*/
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"MOVE WSA-VARIABLE TO ");
			concat_gfl_prefix(hld_type);
			strcat(cobline,&hld_var[1]);
			end_prt_line(1);						/* Add end of line stuff and write line.*/
		}
		if (*f_se->start_pos)
		{
			h_se = cur_set_extr;
			cur_set_extr = f_se;
			wrt_subscript(1,4);						/* If substr, gen CALL STRING.		*/
			cur_set_extr = h_se;
		}
	}
}

void wrt_readfdr(void)									/* Write out readfdr syntax.		*/
{
	char *cstr, ctype;
	int i;

	write_log("PROCTRAN",'I','W',"WRITE","Writing the READFDR items.");

	if (cur_rfdr->br_flag) wrt_backref(5);						/* If have backwards ref., do first.	*/

	for (i = 0; i < 3; i++)
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");

		cstr = NULL;
		ctype = '\0';

		if (i == 0)
		{
			cstr = cur_rfdr->file;
			ctype = cur_rfdr->file_type[1];
		}
		else if (i == 1)
		{
			cstr = cur_rfdr->lib;
			ctype = cur_rfdr->lib_type[1];
		}
		else if (i == 2)
		{
			cstr = cur_rfdr->vol;
			ctype = cur_rfdr->vol_type[1];
		}

		if (*cstr == '&') 							/* Is a variable.			*/ 
		{
			concat_var_prefix(ctype);					/* Concatenate the var prefix.		*/
			strcat(cobline,cstr+1);
		}
		else if (*cstr == '\0') 
		{
			strcat(cobline,"SPACES");
		}
		else 									/* Is an alpha numeric string.		*/ 
		{
			strcat(cobline,"\"");						/* Load the quote symbol.		*/
			strcat(cobline,cstr);						/* Load the value.			*/
			strcat(cobline,"\"");						/* Load the quote symbol.		*/
		}

		strcat(cobline," TO READFDR-");
		if (i == 0) strcat(cobline,"FILE");
		else if (i == 1) strcat(cobline,"LIB");
		else if (i == 2) strcat(cobline,"VOL");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
	}

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE \"");
	strcat(cobline,cur_rfdr->id);							/* Load the FIELD ID.			*/
	strcat(cobline,"\" TO READFDR-FLD-ID");
	end_prt_line(1);								/* Add end of line stuff and write line.*/

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"PERFORM READFDR-FILE-INFO");
	end_prt_line(1);								/* Add end of line stuff and write line.*/

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE RFDR-RECVR-R-2 TO ");
	concat_var_prefix(cur_rfdr->type[1]);						/* Concatenate the var prefix.		*/
	strcat(cobline,&cur_rfdr->recvr[1]);
	end_prt_line(1);								/* Add end of line stuff and write line.*/

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"IF RFDR-RC-R-2 = 20");
	end_prt_line(0);								/* Add end of line stuff and write line.*/
	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"    MOVE -1 TO ");
	concat_var_prefix(cur_rfdr->type[1]);						/* Concatenate the var prefix.		*/
	strcat(cobline,&cur_rfdr->recvr[1]);
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

#ifdef NOT_USED
static void write_equatn(int numeric, int gfl)
{
	if (!numeric)
	{
		strcat(cobline,"COMPUTE ");
		if (*fldname1 == '&')
		{
			concat_gfl_prefix(gfl);						/* Concatenate the global var's prefix.	*/
			strcat(cobline,&fldname1[1]);
		}
		else strcat(cobline,fldname1);						/* Load the Out put string.		*/
		build_compute(cur_assign->literal);
	}
	else
	{
		strcat(cobline,"MOVE ");
		strcat(cobline,cur_assign->literal);					/* Load the starting position.		*/
		strcat(cobline," TO ");
		concat_var_prefix(cur_assign->type[1]);					/* Concatenate the var prefix.		*/
		strcat(cobline,fldname1);						/* Load the Out put string.		*/
	}
}

static void build_compute(char* clp)							/* Build the compute statement.		*/
											/* Current literal pointer.		*/
{
	strcat(cobline," = ");

	while (*clp)
	{
		if (*clp == '*' || *clp == '+' || *clp == '-' || *clp == '/')
		{
			strcat(cobline," ");						/* Load the space.			*/
			strcat(cobline,clp);						/* Load the operator.			*/
			strcat(cobline," ");						/* Load the space.			*/
		}
                else if (*clp == '&')
		{
			concat_var_prefix('L');						/* Load the Literal indicator.		*/
			clp++;								/* Increment past the '&' symbol.	*/
			strcat(cobline,clp);						/* Load the rest of the literal var.	*/
		}
		else strcat(cobline,clp);						/* Load the literal.			*/
	}
	return;
}
#endif /* NOT_USED */

void wrt_keyword_vars(void)								/* Write the working storage variables	*/
{											/* needed for all the PUTPARM calls.	*/
	register int i;
	int max, num, max_len;
	char snum[5], mnum[6];

	write_log("PROCTRAN",'I','W',"WRITE","Writing the PUTPARM variable items.");
	max = 0;									/* Set to no keywords in putparms.	*/
	max_len = 8;									/* Set to no max length for value.	*/
	cur_para = st_para;								/* Get the first paragraph.		*/
	while (cur_para)								/* For each paragraph.			*/
	{
		cur_cmd = cur_para->all_command;					/* Set ptr to commands in the paragraph.*/
		while (cur_cmd)								/* Get to maximum number of keywords	*/
		{									/* in ALL putparms.			*/
			if (cur_cmd->program_area)					/* If have program call.		*/
			{
				cur_prg = cur_cmd->program_area;			/* Set ptr to program item.		*/
				if (cur_prg->pparm_area)				/* If have a putparm in program item.	*/
				{
					cur_pp = cur_prg->pparm_area;			/* Set ptr to putparm item.		*/
					while (cur_pp)					/* Wile have a putparm.			*/
					{
						num = 0;
						cur_pp_key = cur_pp->kwlist;		/* Set pt to keyword list.		*/
						while (cur_pp_key)			/* If have any keywords.		*/
						{
							num++;				/* Increment the keyword count.		*/
							if (cur_pp_key->len > max_len)
							{ 				/* Set the max length of the value.	*/
								max_len = cur_pp_key->len;
							}
							cur_pp_key = cur_pp_key->next_item;
						}
						cur_pp->kwcnt = num;			/* Set the keyword count for later.	*/
						if (num > max) max = num;		/* Set the maximun number of keywords.	*/
						cur_pp = cur_pp->next_item;
					}
				}
			}
			cur_cmd = cur_cmd->next_item;
		}
		cur_para = cur_para->next_item;
	}
	sprintf(mnum,"(%d)",max_len);							/* Convert the maximum length to str.	*/
	if (max < 3 ) max = 3;								/* Set to at least 3 for backwards ref.	*/
	for (i = 0; i < max; i++)							/* Write the working storage variables	*/
	{										/* for the call to PUTPARM.		*/
		strcpy(cobline,"       01  PPARM-KW-");
		sprintf(snum,"%d",i+1);
		strcat(cobline,snum);							/* Add the number of the var.		*/
		if (i < 9) strcat(cobline,"          ");				/* Add spaces for visual.		*/
		else strcat(cobline,"         ");
		strcat(cobline,"           PIC X(8) VALUE SPACES");			/* Add the length of the keyword.	*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"       01  PPARM-VAL-");
		strcat(cobline,snum);							/* Add the number of the var.		*/
		if (i < 9) strcat(cobline,"         ");					/* Add spaces for visual.		*/
		else strcat(cobline,"        ");
		strcat(cobline,"           PIC X");
		strcat(cobline,mnum);							/* Add the maximum length of value.	*/
		strcat(cobline," VALUE SPACES");
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"       01  PPARM-LENGTH-");
		strcat(cobline,snum);							/* Add the number of the var.		*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"           05  FILLER                     BINARY VALUE 0");
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"           05  PPARM-LEN-");
		strcat(cobline,snum);							/* Add the number of the var.		*/
		if (i < 9) strcat(cobline,"     ");					/* Add spaces for visual.		*/
		else strcat(cobline,"    ");
		strcat(cobline,"           BINARY VALUE 0");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
	}	
}

static void wrt_key_params(void)							/* Write out the PUTPARM variable	*/
{											/* parameters for the CALL stmnt.	*/
	register int i;
	char cnt[FLDLEN];
	int csize;

	write_log("PROCTRAN",'I','W',"WRITE","Writing the CALL PUTPARM source.");

	cur_pp = cur_prg->pparm_area;
	while (cur_pp)
	{										/* First do all the MOVES to call vars.	*/
		if (cur_pp->br_flag) wrt_backref(1);					/* If have backwards ref., do first.	*/
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (cur_pp->func == 'E') strcat(cobline,"\"E\"");			/* Set to an ENTER putparm.		*/
		else 	strcat(cobline,"\"D\"");					/*  else is a DISPLAY putparm.		*/
		strcat(cobline," TO PPARM-FUNC");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE \"");
		strcat(cobline,cur_pp->prname);
		strcat(cobline,"\" TO PPARM-PRNAME");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (*cur_pp->label)
		{
			strcat(cobline,"\"");
			strcat(cobline,cur_pp->label);
			strcat(cobline,"\"");
		}
		else strcat(cobline,"SPACES");
		strcat(cobline," TO PPARM-LABEL");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		sprintf(cnt,"%d",cur_pp->kwcnt);					/* Load the MOVE count to KW-CNT.	*/
		strcat(cobline,cnt);
		strcat(cobline," TO KW-CNT");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		if (cur_pp->kwcnt)							/* If have keywords.			*/
		{
			cur_pp_key = cur_pp->kwlist;
			for (i = 0; i < cur_pp->kwcnt; i++)
			{
				char fldn[FLDLEN], len[FLDLEN];

				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"MOVE \"");
				strcat(cobline,cur_pp_key->keywrd);
				strcat(cobline,"\" TO PPARM-KW-");
				sprintf(fldn,"%d",i+1);
				strcat(cobline,fldn);
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);

				indent_line();						/* Add the correct indentation.		*/
				if (*cur_pp_key->val2 != '\0') strcat(cobline,"STRING ");
				else strcat(cobline,"MOVE ");

				if (*cur_pp_key->val == '&' && cur_pp_key->type[1] != 'S') /* Is a variable.			*/ 
				{
					concat_var_prefix(cur_pp_key->type[1]);		/* Concatenate the var prefix.		*/
					strcat(cobline,&cur_pp_key->val[1]);
				}
				else if (cur_pp_key->type[1] == 'B') 			/* Is a backwards reference variable.	*/ 
				{
					strcat(cobline,cur_pp_key->val);		/* Load the variable.			*/
				}
				else 							/* Is an alpha numeric string.		*/ 
				{
					strcat(cobline,"\"");				/* Load the quote symbol.		*/
					strcat(cobline,cur_pp_key->val);		/* Load the value.			*/
					strcat(cobline,"\"");				/* Load the quote symbol.		*/
				}

				if (*cur_pp_key->val2 != '\0')
				{
					strcat(cobline,", ");
					if (*cur_pp_key->val2 == '&')			/* Is a variable.			*/ 
					{
						concat_var_prefix(cur_pp_key->type2[1]); /* Concatenate the var prefix.		*/
						strcat(cobline,&cur_pp_key->val2[1]);
	 				}
					else 						/* Is an alpha numeric string.		*/ 
					{
						strcat(cobline,"\"");			/* Load the quote symbol.		*/
						strcat(cobline,cur_pp_key->val2);	/* Load the value.			*/
						strcat(cobline,"\"");			/* Load the quote symbol.		*/
					}
					end_prt_line(0);				/* Add end of line stuff and write line.*/

					indent_line();					/* Add the correct indentation.		*/
					strcat(cobline,"  DELIMITED BY SIZE INTO ");
				}
				else	strcat(cobline," TO ");

				if ( (csize = strlen(cobline)) >= max_cobline)		/* If buffer too long.			*/
				{
					end_prt_line(0);				/* Add end of line stuff and write line.*/
					indent_line();					/* Add the correct indentation.		*/
					strcat(cobline,"    ");					
				}

				strcat(cobline," PPARM-VAL-");
				strcat(cobline,fldn);
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);

				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"MOVE ");
				sprintf(len,"%d",cur_pp_key->len);
				strcat(cobline,len);
				strcat(cobline," TO PPARM-LEN-");
				strcat(cobline,fldn);
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);

				cur_pp_key = cur_pp_key->next_item;
			}
		}

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE PPARM-PFKEY-TABLE(");
		strcat(cobline,cur_pp->pfkey);
		strcat(cobline,") TO PPARM-PFKEY");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,call_putparm[0]);					/* Write the begining of CALL putparm.	*/
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		strcpy(cobline,call_putparm[1]);					/* Write the keyword count parameter.	*/
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		if (cur_pp->kwcnt)
		{
			for (i = 0; i < cur_pp->kwcnt; i++)				/* Set up call for each keyword.	*/
			{
				char fldn[FLDLEN];

				sprintf(fldn,"%d",i+1);
				strcpy(cobline,call_putparm[2]);			/* Write the PPARM-KW-			*/
				strcat(cobline,fldn);
				strcat(cobline,call_putparm[3]);			/* Write the , PPARM-VAL-		*/
				strcat(cobline,fldn);
				end_prt_line(2);					/* Add end of line stuff and write line.*/

				strcpy(cobline,call_putparm[4]);			/* Write the PPARM-LENGTH		*/
				strcat(cobline,fldn);
				end_prt_line(2);					/* Add end of line stuff and write line.*/
			}
		}
		check_after_bump(&call_putparm[5],"Perform Call Putparm end");		/* Write the end of CALL putparm.	*/
		strcpy(cobline,call_putparm[8]);					/* Write the PPARM-RETURN-CODE		*/
		end_prt_line(1);
		if (gendisplay)
		{
			for (i = 9; i < 11; i++)					/* Write the test of return code stmts.	*/
			{
				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,call_putparm[i]);
				if (in_if || i == 9 ) end_prt_line(0);			/* Add end of line stuff and write line.*/
				else end_prt_line(1);
			}
			if (in_if)							/* If already in IF then add line.	*/
			{
				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"END-IF");
				end_prt_line(0);					/* Add end of line stuff and write line.*/
			}
		}
		cur_pp = cur_pp->next_item;
	}
}


static void wrt_call_link(void)							/* Write out the Call to LINK.		*/
{
	int i;
	char snum[5];

	if (cur_prg->number_using > 32) write_log("PROCTRAN",'E','W',"MAXPARMS",
			"Exceeded maximum number of parameters allowed for CALLS.");
	
	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE ");
	sprintf(snum,"%d",cur_prg->number_using);
	strcat(cobline,snum);
	strcat(cobline," TO LINK-PCOUNT-NO");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	for (i = 0; i < 3; i++)								/* Write init statements for LINK call.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,call_link[i]);
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
	}	

	for (i = 3; i < 6; i++)								/* Write init statements for LINK call.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,call_link[i]);
		end_prt_line(0);							/* Add end of line stuff and write line.*/
	}	

	if (cur_prg->using_name)							/* Do we have a using clause.		*/
	{
		cur_using = cur_prg->using_name;					/* Set up the first one.		*/
		while (cur_using)
		{
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"                  ");
			if (*cur_using->var == '&')
			{
				concat_var_prefix(cur_using->type[1]);			/* Concatenate the var prefix.		*/
				strcat(cobline,&cur_using->var[1]);
			}
			else strcat(cobline,cur_using->var);
			end_prt_line(2);						/* Add end of line stuff and write line.*/

			cur_using = cur_using->next_item;				/* Go to the next one.			*/
		}
	}
											/* Write end of LINK call.		*/
	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,call_link[6]);
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);     

	if (*cur_prg->rtrn_cd_lbl)							/* Add setting of para label for RUN.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE LINK-R-2 TO ");
		concat_var_prefix('R');
		strcat(cobline,cur_prg->rtrn_cd_lbl);
		strcat(cobline,"-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
	}

	if (*cur_prg->error_lbl)							/* Add processing code for ERROR EXIT.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"IF LINK-CODE-VAL = 8");
		end_prt_line(0);							/* Add end of line stuff and write line.*/

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"    GO TO P-");
		strcat(cobline,cur_prg->error_lbl);
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
		if (in_if)								/* If already in IF then add line.	*/
		{
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"END-IF");
			end_prt_line(0);						/* Add end of line stuff and write line.*/
		}
	}
	if (*cur_prg->cancel_lbl)							/* Add processing code for CANCEL EXIT.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"IF LINK-CODE-VAL = 16");
		end_prt_line(0);							/* Add end of line stuff and write line.*/

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"    GO TO P-");
		strcat(cobline,cur_prg->cancel_lbl);
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
		if (in_if)								/* If already in IF then add line.	*/
		{
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"END-IF");
			end_prt_line(0);						/* Add end of line stuff and write line.*/
		}
	}

	if (*cur_prg->error_lbl == '\0')						/* If don't have processing code for	*/
	{										/*  ERROR EXIT then add some.		*/
		for (i = 8; i < 11; i++)						/* Write the DISPLAY message if error.	*/
		{
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,call_link[i]);
			end_prt_line(0);						/* Add end of line stuff and write line.*/
		}

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,call_link[11]);
		end_prt_line(1);							/* Add end of line stuff and write line.*/
	}
}

static void gen_compute(int gfl, int tfl, char htyp)		    			/* Generate the COMPUTE statement.	*/
{
	indent_line();									/* Add the correct indentation.		*/
	if (htyp != 'I')
	{
		write_log("PROCTRAN",'E','W',"INVAL",
			"Invalid COMPUTE  - using string vars (%s,%s). Correction needed.",
					fldname1,cur_assign->field1);
	}
	strcat(cobline,"COMPUTE ");
	concat_gfl_prefix(gfl);								/* Concatenate the global var's prefix.	*/
	strcat(cobline,fldname1);
	strcat(cobline," =");                                  
	if (*fldliteral)								/* If have value in literal then concat.*/
	{
		strcat(cobline," ");                 
		strcat(cobline,fldliteral);
	}
	if (cur_assign->sub_flag) cur_assign = cur_assign->next_item;			/* Step over the STRING from field.	*/
	while (cur_assign)								/* Write the rest of the statement.	*/
	{
		strcat(cobline," ");
		if (tfl)								/* If part of a complex equation.	*/
		{
			concat_var_prefix(' ');						/* Concatenate the var prefix.		*/
			strcat(cobline,fldname1);
			tfl = 0;							/* Set flag so uses next flds in list.	*/
		}
		else
		{
			if (*cur_assign->field1 == '&')					/* If a variable.			*/
			{
				concat_var_prefix(cur_assign->type[1]);			/* Concatenate the var prefix.		*/
				strcat(cobline,&cur_assign->field1[1]);
			}
			else if (cur_assign->type[1] == 'R')				/* If a label.				*/
			{
				concat_var_prefix(cur_assign->type[1]);			/* Concatenate the var prefix.		*/
				strcat(cobline,cur_assign->field1);
			}
			else	strcat(cobline,cur_assign->field1);
		}
		cur_assign = cur_assign->next_item;
	}
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

static void concat_gfl_prefix(int fl)							/* Concatenate the global var's prefix.	*/
{
	char gfltype;

	if (fl == 1) gfltype = 'E';							/* Get the type for first var.		*/
	else if (fl == 2) gfltype = 'G';
	else if (fl == 3) gfltype = 'L';
	else if (fl == 4) gfltype = 'R';
	else gfltype = ' ';

	concat_var_prefix(gfltype);							/* Concatenate the var prefix.		*/
}

void indent_line(void)									/* Add the correct indentation.		*/
{
	if (in_if) strcpy(cobline,"                ");					/* Load the indent if in if statement.	*/
	else	 strcpy(cobline,"           ");
}

void end_prt_line(int pfl)								/* Add period if needed and line feed,	*/
											/* then print line to the file.		*/
{
	if (pfl == 1) strcat(cobline,".\n");						/* Load the period line feed.	*/
	else if (pfl == 2) strcat(cobline,",\n");					/* else load the comma line feed.	*/
	else strcat(cobline,"\n");							/* just load line feed.			*/

	num_outlines++;									/* Increment the .WCB line count.	*/
	fputs(cobline,outfile);								/* Write the output for ws.		*/
}

static void wrt_backref(int type)							/* Write out the backwards reference	*/
											/* PUTPARM parameters for the CALL stmnt.*/
{
	struct pp_keywords *cur_kwl;
	char label[FLDLEN], *lptr, scnt[5];
	char fldn[FLDLEN], len[FLDLEN];
	char *cstrp=NULL, keyw[FLDLEN], *cstr;
	int cnt, i, j, br=0;

	if (type == 8 && cur_set_extr->br_flag == 2) return;				/* Don't need because already have.	*/

	write_log("PROCTRAN",'I','W',"WRITE","Writing the backwards reference CALL PUTPARM source.");
                                                               
	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE \"M\" TO PPARM-FUNC");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	if (type == 1)									/* Get info. from PUTPARM structure.	*/
	{
		cur_kwl = cur_pp->kwlist;						/* Set ptr to traverse putparm struct.	*/
		cnt = 0;								/* Set no kewords yet.			*/
		while (cur_kwl)
		{
			if (cur_kwl->br_flag)						/* Test if a backwards referenced key.	*/
			{
				cnt++;							/* Increment keyword count.		*/
				if (cnt == 1)						/* Get the label value.			*/
				{
					lptr = label;
					cstrp = cur_kwl->val;
					while(*cstrp != '-') *lptr++ = *cstrp++;	/* Copy the label.			*/
					*lptr = '\0';					/* Null terminate.			*/
				}
			}
			cur_kwl = cur_kwl->next_item;					/* Set to next keyword.			*/
		}
	}
	else if (type == 2 || type == 3 || type == 5 || type == 7)			/* Get info. from IF struct or  	*/
	{										/*  RENAME/SCRATCH struct. or READFDR 	*/
		cnt = 0;								/*  struct. or PROGRAM struct.		*/
		for (i = 0; i < 3; i++)							/* Do for each file spec. keyword.	*/
		{
			if (i == 0)
			{
				if (type == 2)
				{
					cstrp = cur_if->file;				/* Assign the current file.		*/
					br = cur_if->file_br;
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->name;
					br = cur_ren_sctch->name_br;
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->file;
					br = cur_rfdr->name_br;
				}
				else if (type == 7)
				{
					cstrp = cur_prg->name;
					br = cur_prg->name_br;
				}
			}
			else if (i == 1)
			{
				if (type == 2)
				{
					cstrp = cur_if->lib;				/* Assign the current library.		*/
					br = cur_if->lib_br;
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->lib;
					br = cur_ren_sctch->lib_br;
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->lib;
					br = cur_rfdr->lib_br;
				}
				else if (type == 7)
				{
					cstrp = cur_prg->lib;
					br = cur_prg->lib_br;
				}
			}
			else if (i == 2)
			{
				if (type == 2)
				{
					cstrp = cur_if->vol;				/* Assign the current volume.		*/
					br = cur_if->vol_br;
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->vol;
					br = cur_ren_sctch->vol_br;
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->vol;
					br = cur_rfdr->vol_br;
				}
				else if (type == 7)
				{
					cstrp = cur_prg->vol;
					br = cur_prg->vol_br;
				}
			}

			if (br)
			{
				cnt++;							/* Increment keyword count.		*/
				if (cnt == 1)						/* Assume is same as file .  Set if	*/
				{							/*  not set by the file.		*/
					lptr = label;
					while(*cstrp != '-') *lptr++ = *cstrp++;	/* Copy the label.			*/
					*lptr = '\0';					/* Null terminate.			*/
				}
			}
		}
	}
	else if (type == 4 || type == 6 || type == 8)					/* Get info. from ASSIGN struct,	*/
	{										/*  RUN USING struct. or SET struct.	*/
		cnt = 1;
		if (type == 4) cstrp = cur_assign->field1;				/* Assign the current field.		*/
		else if (type == 6) cstrp = cur_using->var;
		else if (type == 8) cstrp = cur_set_extr->var;

		lptr = label;
		while(*cstrp != '-') *lptr++ = *cstrp++;				/* Copy the label.			*/
		*lptr = '\0';								/* Null terminate.			*/
		cstrp++;								/* Step over the seperator.		*/
		strcpy(keyw,cstrp);							/* Copy the keyword.			*/
	}
	else /* invalid type */
	{
		write_log("PROCTRAN",'E','W',"BACKWARDS",
			"Invalid TYPE");
		return;
	}

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE \"");
	strcat(cobline,label);
	strcat(cobline,"\" TO PPARM-PRNAME");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE SPACES TO PPARM-LABEL");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"MOVE ");
	sprintf(scnt,"%d",cnt);								/* Convert cnt to string.		*/
	strcat(cobline,scnt);
	strcat(cobline," TO KW-CNT");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	if (type == 1)
	{
		cur_kwl = cur_pp->kwlist;						/* Set ptr to beginning of keys list.	*/
		j = 1;
		while (cur_kwl)
		{
			if (cur_kwl->br_flag)						/* Test if a backwards referenced key.	*/
			{
				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"MOVE \"");
				cstr = cur_kwl->val;
				while (*cstr != '-') cstr++;				/* Step to name of field back ref. 	*/
				cstr++;
				strcat(cobline,cstr);
				strcat(cobline,"\" TO PPARM-KW-");
				sprintf(fldn,"%d",j);
				strcat(cobline,fldn);
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);

				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"MOVE ");
				sprintf(len,"%d",cur_kwl->len);
				strcat(cobline,len);
				strcat(cobline," TO PPARM-LEN-");
				strcat(cobline,fldn);
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);

				j++;							/* Increment i for correct parameters.	*/
			}
			cur_kwl = cur_kwl->next_item;					/* Set to next keyword.			*/
		}
	}
	else if (type == 2 || type == 3 || type == 5 || type == 7)
	{
		j = 1;
		for (i = 0; i < 3; i++)							/* Do for each file spec. keyword.	*/
		{
			if (i == 0)
			{
				if (type == 2)
				{
					cstrp = cur_if->file;				/* Assign the current file.		*/
					br = cur_if->file_br;
					strcpy(keyw,"FILE");
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->name;
					br = cur_ren_sctch->name_br;
					strcpy(keyw,"FILE");
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->file;
					br = cur_rfdr->name_br;
					strcpy(keyw,"FILE");
				}
				else if (type == 7)
				{
					cstrp = cur_prg->name;
					br = cur_prg->name_br;
					strcpy(keyw,"FILE");
				}
			}
			else if (i == 1)
			{
				if (type == 2)
				{
					cstrp = cur_if->lib;				/* Assign the current library.		*/
					br = cur_if->lib_br;
					strcpy(keyw,"LIBRARY");
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->lib;
					br = cur_ren_sctch->lib_br;
					strcpy(keyw,"LIBRARY");
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->lib;
					br = cur_rfdr->lib_br;
					strcpy(keyw,"LIBRARY");
				}
				else if (type == 7)
				{
					cstrp = cur_prg->lib;
					br = cur_prg->lib_br;
					strcpy(keyw,"LIBRARY");
				}
			}
			else if (i == 2)
			{
				if (type == 2)
				{
					cstrp = cur_if->vol;				/* Assign the current volume.		*/
					br = cur_if->vol_br;
					strcpy(keyw,"VOLUME");
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->vol;
					br = cur_ren_sctch->vol_br;
					strcpy(keyw,"VOLUME");
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->vol;
					br = cur_rfdr->vol_br;
					strcpy(keyw,"VOLUME");
				}
				else if (type == 7)
				{
					cstrp = cur_prg->vol;
					br = cur_prg->vol_br;
					strcpy(keyw,"VOLUME");
				}
			}

			if (br)
			{
				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"MOVE \"");
				strcat(cobline,keyw);
				strcat(cobline,"\" TO PPARM-KW-");
				sprintf(fldn,"%d",j);
				strcat(cobline,fldn);
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);

				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"MOVE ");
				if (0==strcmp(keyw,"VOLUME")) strcat(cobline,"6");
				else	strcat(cobline,"8");
				strcat(cobline," TO PPARM-LEN-");
				strcat(cobline,fldn);
				if (in_if) end_prt_line(0);				/* Add end of line stuff and write line.*/
				else end_prt_line(1);

				j++;							/* Increment j for correct parameters.	*/
			}
		}
	}
	else if (type == 4 || type == 6 || type == 8)
	{
		j = 1;									/* Set because only one keyword.	*/
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE \"");
		strcat(cobline,keyw);
		strcat(cobline,"\" TO PPARM-KW-");
		sprintf(fldn,"%d",j);
		strcat(cobline,fldn);
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (0==strcmp(keyw,"VOLUME")) strcat(cobline,"6");
		else if (0==strcmp(keyw,"FILE")) strcat(cobline,"8");
		else if (0==strcmp(keyw,"LIBRARY")) strcat(cobline,"8");
		else
		{
			write_log("PROCTRAN",'W','W',"MODSIZE","Possible mods. to length needed for PUTPARM.");
			strcat(cobline,"8");
		}
		strcat(cobline," TO PPARM-LEN-");
		strcat(cobline,fldn);
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
	}

	indent_line();									/* Add the correct indentation.		*/
	if (type == 1)
	{
		strcat(cobline,"MOVE PPARM-PFKEY-TABLE(");
		strcat(cobline,cur_pp->pfkey);
	}
	else strcat(cobline,"MOVE PPARM-PFKEY-TABLE(1");

	strcat(cobline,") TO PPARM-PFKEY");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,call_putparm[0]);						/* Write the begining of CALL putparm.	*/
	end_prt_line(2);								/* Add end of line stuff and write line.*/

	strcpy(cobline,call_putparm[1]);						/* Write the keyword count parameter.	*/
	end_prt_line(2);								/* Add end of line stuff and write line.*/

	if (type == 1)
	{
		cur_kwl = cur_pp->kwlist;						/* Set ptr to beginning of keys list.	*/
		j = 1;
		while (cur_kwl)
		{                    
			if (cur_kwl->br_flag)						/* Test if a backwards referenced key.	*/
			{								/* Set up call for each keyword.	*/
				sprintf(fldn,"%d",j);
				strcpy(cobline,call_putparm[2]);			/* Write the PPARM-KW-			*/
				strcat(cobline,fldn);
				strcat(cobline,", ");					/* Add field seperator.			*/
				strcat(cobline,cur_kwl->val);				/* Copy the referenced variable.	*/
				end_prt_line(2);					/* Add end of line stuff and write line.*/

				strcpy(cobline,call_putparm[4]);			/* Write the PPARM-LENGTH		*/
				strcat(cobline,fldn);
				end_prt_line(2);					/* Add end of line stuff and write line.*/

				j++;							/* Increment i for correct parameters.	*/
			}
			cur_kwl = cur_kwl->next_item;					/* Set to next keyword.			*/
		}
	}
	else if (type == 2 || type ==3 || type == 5 || type == 7)
	{
		j = 1;
		for (i = 0; i < 3; i++)							/* Do for each file spec. keyword.	*/
		{
			if (i == 0)
			{
				if (type == 2)
				{
					cstrp = cur_if->file;				/* Assign the current file.		*/
					br = cur_if->file_br;
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->name;
					br = cur_ren_sctch->name_br;
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->file;
					br = cur_rfdr->name_br;
				}
				else if (type == 7)
				{
					cstrp = cur_prg->name;
					br = cur_prg->name_br;
				}
			}
			else if (i == 1)
			{
				if (type == 2)
				{
					cstrp = cur_if->lib;				/* Assign the current library.		*/
					br = cur_if->lib_br;
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->lib;
					br = cur_ren_sctch->lib_br;
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->lib;
					br = cur_rfdr->lib_br;
				}
				else if (type == 7)
				{
					cstrp = cur_prg->lib;
					br = cur_prg->lib_br;
				}
			}
			else if (i == 2)
			{
				if (type == 2)
				{
					cstrp = cur_if->vol;				/* Assign the current volume.		*/
					br = cur_if->vol_br;
				}
				else if (type == 3)
				{
					cstrp = cur_ren_sctch->vol;
					br = cur_ren_sctch->vol_br;
				}
				else if (type == 5)
				{
					cstrp = cur_rfdr->vol;
					br = cur_rfdr->vol_br;
				}
				else if (type == 7)
				{
					cstrp = cur_prg->vol;
					br = cur_prg->vol_br;
				}
			}

			if (br)
			{
				sprintf(fldn,"%d",j);
				strcpy(cobline,call_putparm[2]);			/* Write the PPARM-KW-			*/
				strcat(cobline,fldn);
				strcat(cobline,", ");					/* Add field seperator.			*/
				strcat(cobline,cstrp);					/* Copy the referenced variable.	*/
				end_prt_line(2);					/* Add end of line stuff and write line.*/

				strcpy(cobline,call_putparm[4]);			/* Write the PPARM-LENGTH		*/
				strcat(cobline,fldn);
				end_prt_line(2);					/* Add end of line stuff and write line.*/

				j++;							/* Increment j for correct parameters.	*/
			}
		}
	}
	else if (type == 4 || type == 6 || type == 8)
	{
		j = 1;
		if (type == 4) cstrp = cur_assign->field1;				/* Assign the current file.		*/
		else if (type == 6) cstrp = cur_using->var;
		else if (type == 8) cstrp = cur_set_extr->var;


		sprintf(fldn,"%d",j);
		strcpy(cobline,call_putparm[2]);					/* Write the PPARM-KW-			*/
		strcat(cobline,fldn);
		strcat(cobline,", ");							/* Add field seperator.			*/
		strcat(cobline,cstrp);							/* Copy the referenced variable.	*/
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		strcpy(cobline,call_putparm[4]);					/* Write the PPARM-LENGTH		*/
		strcat(cobline,fldn);
		end_prt_line(2);							/* Add end of line stuff and write line.*/
	}

	check_after_bump(&call_putparm[5],"Perform Call Putparm end");			/* Write the end of CALL putparm.	*/
	strcpy(cobline,call_putparm[8]);						/* Write the PPARM-RETURN-CODE		*/
	end_prt_line(1);
	if (gendisplay)
	{
		for (i = 9; i < 11; i++)						/* Write the test of return code stmts.	*/
		{
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,call_putparm[i]);
			if (in_if || i == 9 ) end_prt_line(0);				/* Add end of line stuff and write line.*/
			else end_prt_line(1);
		}
		if (in_if)								/* If already in IF then add line.	*/
		{
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"END-IF");
			end_prt_line(0);						/* Add end of line stuff and write line.*/
		}
	}
}

#ifdef NOT_USED
static void wrt_buffer(void)								/* Write out the MOVE to BUFFER stmnt.	*/
{
	register int i;
	int cnt, str_fl, num_param;

	write_log("PROCTRAN",'I','W',"WRITE","Writing the  -- TO WCOPY-BUFFER source.");

	str_fl = 0;
	cur_pp = cur_prg->pparm_area;
	while (cur_pp)
	{
		if (cur_pp->br_flag) wrt_backref(1);					/* If have backwards ref., do first.	*/
		cur_pp_key = cur_pp->kwlist;
		for (i = 0; i < cur_pp->kwcnt; i++)
		{
			if (*cur_pp_key->val == '&') str_fl++;				/* Is a variable.			*/ 
			cur_pp_key = cur_pp_key->next_item;
		}
		cur_pp = cur_pp->next_item;
	}

	indent_line();									/* Add the correct indentation.		*/
	if (str_fl)
	{
		strcat(cobline,"STRING \"wcopy LIBRARY \",");
		end_prt_line(0);							/* Add end of line stuff and write line.*/

		indent_line();
		strcat(cobline,"       ");
	}
	else strcat(cobline,"MOVE \"wcopy LIBRARY");
	
	cnt = 0;
	num_param = 0;
	cur_pp = cur_prg->pparm_area;							/* Write the actual MOVE.		*/
	while (cur_pp)
	{
		if (0==strcmp(cur_pp->prname,"INPUT") || 0==strcmp(cur_pp->prname,"OUTPUT"))
		{
			if (cur_pp->kwcnt)						/* If have keywords.			*/
			{
				cur_pp_key = cur_pp->kwlist;
				for (i = 0; i < cur_pp->kwcnt; i++)
				{

					if (0==strcmp(cur_pp_key->keywrd,"LIBRARY") || 0==strcmp(cur_pp_key->keywrd,"VOLUME"))
					{
						num_param += 1;
						if (!str_fl) strcat(cobline," ");	/* Add so spaces out correctly.		*/
						if (*cur_pp_key->val == '&') 		/* Is a variable.			*/ 
						{
							concat_var_prefix(cur_pp_key->type[1]);	/* Concatenate the var prefix.	*/
							strcat(cobline,&cur_pp_key->val[1]);
						}
						else
						{
							if (str_fl && !cur_pp_key->br_flag) strcat(cobline,"\"");
							strcat(cobline,cur_pp_key->val);/* Load the variable.			*/
							if (str_fl && !cur_pp_key->br_flag) strcat(cobline,"\"");
						}
						if (str_fl)
						{
							if (num_param < 4) strcat(cobline,", \" \",");
							end_prt_line(0);		/* Add end of line stuff and write line.*/

							indent_line();
							strcat(cobline,"       ");
						}
					}
					cur_pp_key = cur_pp_key->next_item;
				}
				cnt++;
				if (!str_fl && cnt < 2)
				{
					end_prt_line(0);				/* Add end of line stuff and write line.*/
					strcpy(cobline,"      -    \"");
				}
			}
		}
		cur_pp = cur_pp->next_item;
	}

	if (!str_fl)
	{
		strcat(cobline,"\"");
		end_prt_line(0);							/* Add end of line stuff and write line.*/
	}

	indent_line();									/* Add the correct indentation.		*/
	if (str_fl) strcat(cobline,"    DELIMITED BY SIZE INTO WCOPY-BUFFER");
	else strcat(cobline,"    TO WCOPY-BUFFER");
	if (in_if) end_prt_line(0);							/* Add end of line stuff and write line.*/
	else end_prt_line(1);
}
#endif /* NOT_USED */

static void complete_assign(int ctyp, int num, char htyp,int gfl)			/* Next item is a string so generate	*/
											/* a call to STRING.			*/
{
	char *cstr;
	int cpos;

	indent_line();									/* Add the correct indentation.		*/
	if (num > 1) strcat(cobline,"STRING ");
	else	strcat(cobline,"MOVE ");

	if (cur_assign->next_item && num <= 1 && (cur_assign->type[0] != cur_assign->next_item->type[0]))
	{
		write_log(util,'I','W',"WRITE","Writing the ASSIGN WITH CONVERSION syntax.");
		strcat(cobline,"WITH CONVERSION ");
	}

	while (cur_assign)
	{										/* If a variable or return code label.	*/
		if (*cur_assign->field1 == '&' || cur_assign->type[1] == 'R')
		{
			if (0==strcmp(cur_assign->field1,"&TIME") || 0==strcmp(cur_assign->field1,"&DATE") )
			{								/* Is the TIME or DATE function.	*/
				if (htyp == 'I') strcat(cobline,"WSA-N"); 		/* Is the numeric type variable.	*/
				else	strcat(cobline,"WSA-A");			/* Is alpha type variable.		*/
			}
			else if (ctyp == 1)						/* Is ASSIGN with substring as first	*/
			{								/*  field.				*/
				concat_gfl_prefix(gfl);					/* Concatenate the global var's prefix.	*/
			}
			else concat_var_prefix(cur_assign->type[1]);			/* Concatenate the var prefix.		*/

			if (cur_assign->type[1] == 'R') strcat(cobline,cur_assign->field1);
			else if (ctyp == 1)						/* Is ASSIGN with substring as first	*/
			{								/*  field.				*/
				strcat(cobline,fldname1);
			}
			else if (cur_assign->sub_flag)
			{
				cur_assign = cur_assign->next_item;
				strcat(cobline,&cur_assign->field1[1]);
			}
			else strcat(cobline,&cur_assign->field1[1]);
		}
		else if (cur_assign->br_flag)						/* Is a backwards referenced field.	*/
		{
			strcat(cobline,cur_assign->field1);
		}
		else
		{
			int tfld, ccnt;

			tfld = 0;							/* Assume is a NUMBER.			*/
			ccnt = 0;							/* Char count for test > 16.		*/
			cstr = cur_assign->field1;
			while (*cstr)
			{
				ccnt++;							/* Increment character count.		*/
				if (!number(*cstr) || (ccnt > 16) || 			/* If char is not a number or len > 16	*/
				    *cur_assign->prev_item->type == 'S')		/*  or item assigned is a string.	*/
				{							/* then set to a STRING.		*/
					tfld = 1;
					break;
				}
				cstr++;							/* test next char.			*/
			}
			if (tfld && *cur_assign->field1)				/* If is a string assigned.		*/
			{
				strcat(cobline,"\"");
			}
			cpos = 17;
			if (cpos + strlen(cur_assign->field1) >= (WPLMAX+1))		/* If text goes past valid column 	*/
			{								/* need to generate a continuation	*/
				char temp[60];						/* line.				*/

				cstr = cur_assign->field1;
				strncpy(temp,cstr,55);
				temp[55] = '\0';
				strcat(cobline,temp);					/* Load part of the value of the field.	*/
				end_prt_line(0);					/* Add end of line stuff and write line.*/

				strcpy(cobline,"      -    \"");			/* Load continuation indicator and ".	*/
				cstr += 55;						/* Set ptr to start for next line.	*/
				strcat(cobline,cstr);
			}
			else if (*cur_assign->field1) strcat(cobline,cur_assign->field1);
			else strcat(cobline,"SPACES");

			if (tfld && *cur_assign->field1) strcat(cobline,"\"");
		}

		if (num > 1)								/* IF is a STRING command.		*/
		{
			if (cur_assign->dlmtr == '*') strcat(cobline," DELIMITED BY DEL-BLANK");
			else strcat(cobline," DELIMITED BY SIZE");
		}

		cur_assign = cur_assign->next_item;
		if (cur_assign)								/* If have more than one item.		*/
		{
			strcat(cobline,", ");
			end_prt_line(0);						/* Add end of line stuff and write line.*/
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"       ");
		}
	}

	if (num > 1)
	{
		end_prt_line(0);							/* Add end of line stuff and write line.*/

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline," INTO ");
	}
	else
	{
		if ((int)strlen(cobline) > max_cobline)
		{
			end_prt_line(0);						/* Add end of line stuff and write line.*/

			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"    ");
		}
		strcat(cobline," TO ");
	}

	concat_gfl_prefix(gfl);								/* Concatenate the global var's prefix.	*/
	strcat(cobline,fldname1);
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

void wrt_subscript(int nfl,int caller)							/* Process the subscript.		*/
											/* Generate a CALL STRING		*/
{
	char invar[FLDLEN], ityp = '\0', *cptr = NULL, *tptr;
	int istrt, times, varcnt = 0;
	char tstr[5], *hstpos=NULL;
	char istpos[FLDLEN], ilength[FLDLEN];
	char temp[FLDLEN], tmpsub[FLDLEN], ttyp[3];
	int num_var, num_val, gfl;							/* Set so get_type_len() doesn't fail.	*/

	write_log("PROCTRAN",'I','W',"WRITE","Writing the SUBSTRING CALL STRING items.");
	num_var = 0;
	num_val = 0;

	if (caller == 1)
	{
		if (nfl == 3) times = 2;						/* Set the number of CALL STRING stmnts.*/
		else times = 1;								/*  to generate.			*/
	}
	else times = nfl;

	while (times)
	{
		if (caller == 1)							/* If an IF substring.			*/
		{
			if (nfl == 2) cur_if = cur_if->next_item;			/* Step to subscripted field.		*/
			cptr = cur_if->var;
			strcpy(invar,cptr);
			ityp = cur_if->type[1];
			varcnt = cur_if->sub_flag;
			cur_if = cur_if->next_item;					/* Step to the created item.		*/
			cptr = cur_if->var;
		}
		else if (caller == 2)							/* If an ASSIGN substring then step to	*/
		{									/*  the substring field.		*/
			while (cur_assign && !cur_assign->sub_flag) cur_assign = cur_assign->next_item; /* Step to subscrpt.	*/
			if (!cur_assign) break;						/* If step to end of list.		*/
			cptr = cur_assign->field1;
			strcpy(invar,cptr);
			ityp = cur_assign->type[1];
			varcnt = cur_assign->sub_flag;
			cur_assign = cur_assign->next_item;				/* Step to the created item.		*/
			cptr = cur_assign->field1;
		}
		else if (caller == 3)							/* If a PUTPARM substring then at the	*/
		{									/*  substring field.			*/
			cptr = cur_pp_key->val;						/* Get the field name of orig field.	*/
			tptr = invar;
			while (*cptr != '-') *tptr++ = *cptr++;
			*tptr = '\0';							/* Null terminate the string.		*/
			ityp = cur_pp_key->type[1];
			varcnt = cur_pp_key->sub_flag;
			cptr = cur_pp_key->val;
		}
		else if (caller == 4)							/* If an EXTRACT/SET substring.		*/
		{
			cptr = cur_set_extr->var;
			strcpy(invar,cptr);
			ityp = cur_set_extr->type[1];
			varcnt = cur_set_extr->sub_flag;
			strcpy(temp,&invar[1]);
			strcat(temp,"-");
			strcat(temp,cur_set_extr->start_pos);
			hstpos = cur_set_extr->start_pos;				/* Save pointer for later.		*/
			strcat(temp,"-");
			strcat(temp,cur_set_extr->length);
			cptr = temp;
		}
		else if (caller == 5)							/* If an RENAME/SCRATCH substring.	*/
		{
			cptr = cur_ren_sctch->name;
			strcpy(invar,cptr);
			ityp = cur_ren_sctch->name_type[1];
			varcnt = cur_ren_sctch->sub_flag;
			strcpy(temp,&invar[1]);
			strcat(temp,"-");
			strcat(temp,cur_ren_sctch->start_pos);
			hstpos = cur_ren_sctch->start_pos;				/* Hold for later.			*/
			strcat(temp,"-");
			strcat(temp,cur_ren_sctch->length);
			cptr = temp;
		}
		else if (caller == 6)							/* If an IF substring for the FILENAME.	*/
		{
			cptr = cur_if->file;
			strcpy(invar,cptr);
			ityp = cur_if->file_type[1];
			varcnt = cur_if->sub_flag;
			cptr = cur_if->var;
		}
		else if (caller == 7)							/* If a SCREEN field substring.		*/
		{
			cptr = cur_scn_fld->screen_fld;
			strcpy(invar,cptr);
			ityp = cur_scn_fld->scn_fld_type[1];
			varcnt = cur_scn_fld->sub_flag;
			strcpy(temp,&invar[1]);
			strcat(temp,"-");
			if (*cur_scn_fld->start_pos == '&') strcat(temp,&cur_scn_fld->start_pos[1]);
			else strcat(temp,cur_scn_fld->start_pos);
			hstpos = cur_scn_fld->start_pos;				/* Hold for later.			*/
			strcat(temp,"-");
			strcat(temp,cur_scn_fld->length);
			cptr = temp;
		}
		else /* UNKNOW CALLER */
		{
			write_log("PROCTRAN",'E','W',"WRITE",
				"Invalid caller Writing the SUBSTRING CALL STRING items");
			return;
		}
	
		while (*cptr != '-') cptr++;						/* Step to start pos.			*/
		cptr++;									/* Step past the dash.			*/
		if (caller == 1 || caller == 2)						/* If have - in orig. field name and 	*/
		{									/*  not at start position.		*/
			while (!number(*cptr))						/* if char is not a number.		*/
			{
				while (*cptr != '-') cptr++;				/* Step to start pos.			*/
				cptr++;							/* Step past the dash.			*/
			}
		}
		else if (caller == 4 || caller == 5 || caller == 7)			/* If have - in orig. field name and 	*/
		{									/*  not at start position.		*/
			while (*hstpos != '&' && !number(*cptr))			/* if char is not a number.		*/
			{
				while (*cptr != '-') cptr++;				/* Step to start pos.			*/
				cptr++;							/* Step past the dash.			*/
			}
		}

		tptr = istpos;
		while (*cptr != '-') *tptr++ = *cptr++;					/* Copy the INPUT starting offset.	*/
		*tptr = '\0';								/* Null terminate the string.		*/
		cptr++;									/* Step past the dash.			*/
		strcpy(ilength,cptr);							/* Copy the INPUT length.		*/

		indent_line();								/* Add the correct indentation.		*/
		if (varcnt == 2 || varcnt == 4)
		{
			strcpy(tmpsub,"&");						/* Setup to get type of stpos var.	*/
			strcat(tmpsub,istpos);
			get_type_len(tmpsub,ttyp,&num_var,&num_val,'T');
			if (ttyp[1] == 'E') gfl = 1;
			else if (ttyp[1] == 'G') gfl = 2;
			else gfl = 0;

			strcat(cobline,"COMPUTE ");
			concat_gfl_prefix(gfl);						/* Concatenate the global var's prefix.	*/
			strcat(cobline,&tmpsub[1]);
			strcat(cobline," = ");
			concat_gfl_prefix(gfl);						/* Concatenate the global var's prefix.	*/
			strcat(cobline,&tmpsub[1]);
			strcat(cobline," - 1");
			if (in_if) end_prt_line(0);					/* Add end of line stuff and write line.*/
			else end_prt_line(1);

			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"MOVE ");
			concat_var_prefix(*ttyp);					/* Concatenate the stpos var's prefix.	*/
			strcat(cobline,&tmpsub[1]);					/* Load OFFSET into output.		*/
		}
		else
		{
			strcat(cobline,"MOVE ");
			istrt = atoi(istpos);						/* Convert start pos to integer.	*/
			sprintf(tstr,"%d",istrt - 1);					/* Subtract on for correct offset.	*/
			strcat(cobline,tstr);						/* Load OFFSET into output.		*/
		}
		strcat(cobline," TO STR-IN-INDEX-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (varcnt == 3 || varcnt == 4)
		{
			strcpy(tmpsub,"&");						/* Setup to get type of stpos var.	*/
			strcat(tmpsub,ilength);
			get_type_len(tmpsub,ttyp,&num_var,&num_val,'T');
			concat_var_prefix(*ttyp);					/* Concatenate the stpos var's prefix.	*/
			strcat(cobline,tmpsub);						/* Load OFFSET into output.		*/
		}
		else strcat(cobline,ilength);						/* Load the length.			*/
		strcat(cobline," TO STR-IN-LEN-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE 0 TO STR-OUT-INDEX-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		strcat(cobline,ilength);			 			/* Load length for output.		*/
		strcat(cobline," TO STR-OUT-LEN-R-2");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"CALL \"STRING\" USING STR-FUNC, ");

		concat_var_prefix(ityp);						/* Concatenate the INPUT var's prefix.	*/
		if (caller == 4) strcat(cobline,temp);					/* Load the INPUT var.			*/
		else strcat(cobline,&invar[1]);
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"                          ");
		if (in_if) strcat(cobline,"     ");					/* Load the indent if in if statement.	*/
		strcat(cobline,"STR-INPUT-INDEX, STR-INPUT-LENGTH");
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"                          ");
		if (in_if) strcat(cobline,"     ");					/* Load the indent if in if statement.	*/
		concat_var_prefix(ityp);						/* Concatenate the OUTPUT var's prefix.	*/
		if (caller == 1 || caller == 6) strcat(cobline,&cur_if->var[1]);	/* Load the OUTPUT var.			*/
		else if (caller == 2) strcat(cobline,&cur_assign->field1[1]);
		else if (caller == 3) strcat(cobline,&cur_pp_key->val[1]);
		else if (caller == 4) strcat(cobline,&cur_set_extr->var[1]);
		else if (caller == 5 || caller == 7) strcat(cobline,temp);
		end_prt_line(2);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"                          ");
		if (in_if) strcat(cobline,"     ");					/* Load the indent if in if statement.	*/
		strcat(cobline,"STR-OUTPUT-INDEX, STR-OUTPUT-LENGTH");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		times--;								/* Decrement times in loop.		*/
		if (caller == 1 && nfl == 3) cur_if = cur_if->next_item;		/* Set ptr for second field.		*/
	}
}

static void wrt_print_kw(void)								/* Write the PRINT keyword stmnts.	*/
{
	
	cur_pp = cur_prg->pparm_area;							/* Set ptr to putparm item.		*/
	cur_pp_key = cur_pp->kwlist;							/* Set ptr to keyword list.		*/
	while (cur_pp_key)								/* If have keywords.			*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (0==strcmp(cur_pp_key->keywrd,"CLASS"))				/* Add code to set the print class.	*/
		{
			strcat(cobline,"\"");						/* Put the opening ".			*/
			strcat(cobline,cur_pp_key->val);
			strcat(cobline,"\"");						/* Put the closing ".			*/
			strcat(cobline," TO PRINT-CLASS");
		}
		else if (0==strcmp(cur_pp_key->keywrd,"STATUS"))			/* Add code to set the print mode.	*/
		{
			if (*cur_pp_key->val == 'H') strcat(cobline,"\"H\"");		/* Check the mode setting.		*/
			else strcat(cobline,"\"S\"");

			strcat(cobline," TO PRINT-MODE");
		}
		else if (0==strcmp(cur_pp_key->keywrd,"FORM#"))				/* Add code to set the form number.	*/
		{
			strcat(cobline,cur_pp_key->val);
			strcat(cobline," TO PRINT-F-2");
		}
		else if (0==strcmp(cur_pp_key->keywrd,"COPIES"))			/* Add code to set the copies.		*/
		{
			strcat(cobline,cur_pp_key->val);
			strcat(cobline," TO PRINT-N-2");
		}
		else if (0==strncmp(cur_pp_key->keywrd,"DISP",4))			/* Add code to set the disposition.	*/
		{
			if (0==strncmp(cur_pp_key->val,"SC",2)) strcat(cobline,"\"DX\""); /* Check the disposition setting.	*/
			else if (*cur_pp_key->val == 'R') strcat(cobline,"\"RS\"");
			else strcat(cobline,"\"DS\"");

			strcat(cobline," TO PRINT-DISPOSITION");
		}
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		cur_pp_key = cur_pp_key->next_item;
	}
}

static void wrt_submit_kw(void)								/* Write the SUBMIT keyword stmnts.	*/
{
	if (cur_prg->as_name[0]) /* GSL 6/8/92 */					/* Move the JOB name if has one.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (*cur_prg->as_name == '&')
		{
			concat_var_prefix(cur_prg->as_name_type[1]);			/* Concatenate the var prefix.		*/
			strcat(cobline,&cur_prg->as_name[1]);
		}
		else
		{
			strcat(cobline,"\"");						/* Put the opening ".			*/
			strcat(cobline,cur_prg->as_name);
			strcat(cobline,"\"");						/* Put the closing ".			*/
		}
		strcat(cobline," TO SUBMIT-JOB-NAME");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);
	}	
	cur_pp = cur_prg->pparm_area;							/* Set ptr to putparm item.		*/
	cur_pp_key = cur_pp->kwlist;							/* Set ptr to keyword list.		*/
	while (cur_pp_key)								/* If have keywords.			*/
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (0==strcmp(cur_pp_key->keywrd,"CLASS"))				/* Add code to set the submit class.	*/
		{
			strcat(cobline,"\"");						/* Put the opening ".			*/
			strcat(cobline,cur_pp_key->val);
			strcat(cobline,"\"");						/* Put the closing ".			*/
			strcat(cobline," TO SUBMIT-JOB-CLASS");
		}
		else if (0==strcmp(cur_pp_key->keywrd,"STATUS"))			/* Add code to set the submit status.	*/
		{
			if (*cur_pp_key->val == 'H') strcat(cobline,"\"H\"");		/* Check the mode setting.		*/
			else if (*cur_pp_key->val == 'R') strcat(cobline,"\"R\"");
			else strcat(cobline,"SPACES");

			strcat(cobline," TO SUBMIT-STATUS");
		}
		else if (0==strcmp(cur_pp_key->keywrd,"CPULIMIT"))			/* Add code to set the CPU limit.	*/
		{
			strcat(cobline,cur_pp_key->val);
			strcat(cobline," TO SUBMIT-CPU-2");
		}
		else if (0==strncmp(cur_pp_key->keywrd,"DISP",4))			/* Add code to set the disposition.	*/
		{
			if (*cur_pp_key->val == 'R') strcat(cobline,"\"R\"");		/* Check the disposition setting.	*/
			else if (*cur_pp_key->val == 'P') strcat(cobline,"\"P\"");
			else if (*cur_pp_key->val == 'O') strcat(cobline,"\"O\"");
			else strcat(cobline,"\"D\"");

			strcat(cobline," TO SUBMIT-DISPOSITION");
		}
		else if (0==strcmp(cur_pp_key->keywrd,"DUMP"))				/* Add code to set abort action.	*/
		{
			if (*cur_pp_key->val == 'D') strcat(cobline,"\"D\"");		/* Check the dump setting.		*/
			else if (*cur_pp_key->val == 'N') strcat(cobline,"\"N\"");
			else strcat(cobline,"\"R\"");

			strcat(cobline," TO SUBMIT-ABORT-ACTION");
		}
		else if (0==strcmp(cur_pp_key->keywrd,"ACTION"))			/* Add code to set limit flaf.		*/
		{
			if (*cur_pp_key->val == 'C') strcat(cobline,"\"C\"");		/* Check the action setting.		*/
			else if (*cur_pp_key->val == 'P') strcat(cobline,"\"P\"");
			else strcat(cobline,"\"W\"");

			strcat(cobline," TO SUBMIT-LIMIT-FLAG");
		}
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		cur_pp_key = cur_pp_key->next_item;
	}
}

static void process_br_parms(void)							/* Check if any backwards ref.parms.	*/
{											/*  and process getparm in needed.	*/
	int num;
	using_item *hld_use = NULL;


	num = 0;									/* Assume no backref. variables.	*/
	cur_using = cur_prg->using_name;						/* Set ptr to first one.		*/
	while (cur_using)
	{
		if (cur_using->br_flag)							/* Is backwards reference flag set?	*/
		{
			if (num == 0) hld_use = cur_using;				/* Save ptr to first one in list.	*/
			num++;
		}
		cur_using = cur_using->next_item;
	}
	if (!num) return;								/* If don't have any then return.	*/

	cur_using = hld_use;
	while (num && cur_using)							/* Do for each back ref. field.		*/
	{
		if (cur_using->br_flag)
		{
			wrt_backref(6);							/* If have backwards ref., do first.	*/
			num--;
		}
		cur_using = cur_using->next_item;
	}
}
/*
**	History:
**	$Log: ptwrite.c,v $
**	Revision 1.15  2003/02/20 19:29:55  gsl
**	fix -Wall warnings
**	
**	Revision 1.14  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.12  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.11  2002/07/30 19:12:42  gsl
**	SETRETCODE
**	
**	Revision 1.10  2002/07/29 21:13:28  gsl
**	setretcode -> SETRETCODE
**	
**	Revision 1.9  1997/04/21 15:25:54  scass
**	Corrected copyright.
**	
**	Revision 1.8  1996-12-12 13:32:48-05  gsl
**
**	Revision 1.7  1996-09-13 08:57:34-07  gsl
**	In gen_compute() fixed call to concat_var_prefix(' ') was passing fldname1.
**
**	Revision 1.6  1996-09-12 16:18:42-07  gsl
**	Fix prototypes
**
**
**
*/
