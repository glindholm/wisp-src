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

/*
**	File:		ptequatn.c
**
**	Purpose:	To process the  ASSIGN statement as well as the equation logic of the ASSIGN statement.
**
**	Routines:	p_assign()		Initial processing of the ASSIGN keyword.
**			assign_label()		See if is a label.
**			assign_length()		Assign the length parameter for the current substring.
**			check_assign_types()	Make sure have correct types for assignment.
**			test_assign_flds()	Decide if should create literal.
**			process_back_ref()	Process the backwards referenced field.
**
**
**	History:
**			08/06/92	Put correct header info. into source.	SMC
**
*/

#define EXT extern

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"
#include "pgeqtns.h"
#include "pgdefeqn.h"

static void assign_label(int* num_asn);							/* See if is a label.			*/
static void assign_length(int* num_var, int* num_val,assign_item *saptr);		/* Assign the length parameter for the	*/
static void check_assign_types(int* num_var,int* num_val);				/* Make sure have correct types.	*/
static void process_back_ref(int* num_asn,int* num_var,int* num_val);			/* Process the backwards ref. field.	*/

static int test_assign_flds();

void p_assign(int* num_asn,
	      int* num_var,
	      int* filler_num,
	      int* current_row,
	      int* num_val,
	      int* num_cmds)								/* Process the current assign equation.	*/
{
	int tndx, cont, subfl;
	char *cstr, name_buf[FLDLEN], *l_aptr;
	assign_item *astrt;								/* Ptr to current assign item.		*/
	int setp, nvar;									/* Flag if need to set ptr back one.	*/

	nvar = 0;
	astrt = NULL;	
	setp = 0;							/* Set so processes corrextly.		*/
	while ((tndx = find_char(*aptr,search_equations)) >= 0)				/* Loop until no more found in token.	*/
	{
		setp = 0;								/* Don't reset the pointer.		*/
		switch (tndx)
		{
			case ANDSIGN:
			{
				write_log(util,'I','R',"PROCESS","Processing AND sign for ASSIGN variable in equation.");
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
				check_assign_types(num_var, num_val);			/* Make sure have correct types.	*/
				get_type_len(cur_assign->field1,cur_assign->length,num_var,num_val,'L'); /* Get length of str.	*/
				next_ptr = aptr;
				if (*aptr == ' ') nexttok();
				if (*aptr == '(')					/* If a substring with assignment.	*/
				{
					cstr = aptr;					/* Set the current ptr.			*/
					cstr++;						/* Step past open parenthesis.		*/
					if (*cstr == '\0' || *cstr == LNFD)
					{
						char tmp_line[160];

						strcpy(tmp_line,paren_sym);		/* Set up the line so has (start,len)	*/
						get_next_line();			/* in same string.			*/
						nexttok();
						strcat(tmp_line,aptr);
						strcpy(linein,tmp_line);
						aptr = linein;
						cstr = aptr;				/* Set the current ptr.			*/
						cstr++;					/* Step past open parenthesis.		*/
					}

					if (*cstr == '1' && *(cstr+2) == '*')		/* If using the full value of var.	*/
					{
						cur_assign->dlmtr = '*';		/* Set so uses blank delimeter.		*/ 
						while (*cstr != ')') cstr++;		/* Step past (1,*			*/
						cstr++;					/* Step past closing ).			*/
						aptr = cstr;
						next_ptr = aptr;
					}
					else if (*cur_assign->start_pos || nvar)	/* If already assign substring or is	*/
					{						/*  more than one var in staement.	*/
						process_sub_var(*cur_assign->type,num_var,num_val,2,
								num_asn,filler_num,current_row,num_cmds);
					}
				}
				nvar++;							/* Increment vars in ASSIGN stmnt.	*/
				if (*aptr == ' ') nexttok();
				if (!strncmp(aptr,"EQ ",3))				/* If operation is =, change to symbol.	*/
				{
					l_aptr = aptr;
					*l_aptr++ = '=';
					*l_aptr = ' ';
				}
				if (*aptr != '\0' && *aptr != LNFD) setp = 1;		/* Set pointer back so processes ok.	*/
				break;
			}

			case OPNPRN:
			{
				aptr++;							/* Step past the open paren.		*/
				if (*aptr == '(')					/* Indicates that is complex eqn.	*/
				{							/* with psooible substring parameters.	*/
					astrt = cur_assign;				/* Save position for length parameter.	*/
					cstr = cur_assign->start_pos;
					strcpy(cstr,cur_assign->field1);		/* Set start pos to variable.		*/
					aptr++;						/* Step over the open paren.		*/
					break;
				}
				l_aptr = aptr;						/* Set the local pointer.		*/
				subfl = FALSE;						/* Flag for substring function.		*/
				while (*l_aptr != ')' && *l_aptr != '(')		/* Search string for closing paren and	*/
				{							/* test if a substring function.	*/
					if (*l_aptr == ',')
					{
						subfl = TRUE;				/* Set flag for substring function.	*/
						break;
					}
					l_aptr++;					/* Set to next char in string.		*/
				}
				if (*aptr == '&' && !subfl)
				{
					write_log(util,'I','R',"PROCESS","Processing ASSIGN equation variable.");
					strcpy(cur_assign->literal,paren_sym);		/* Set so puts ( in equation.		*/
					break;						/* Break out and process as variable.	*/
				}

				write_log(util,'I','R',"PROCESS","Processing ASSIGN substring parameters.");
				cstr = cur_assign->start_pos;
				while (*aptr != ',') *cstr++ = toupper(*aptr++);	/* Copy in the starting position.	*/
				*cstr = '\0';						/* Null terminate the string.		*/
				if (*cur_assign->start_pos == '&')
				{							 /* Get type of variable.		*/
					get_type_len(cur_assign->start_pos,cur_assign->stpos_type,num_var,num_val,'T');
				}
				aptr++;							/* Step over the comma.			*/
				assign_length(num_var,num_val,astrt);			/* Assign the length of substring.	*/
				next_ptr = aptr;
				if (*aptr == ' ') nexttok();
				if (*aptr != '\0' && *aptr != LNFD) setp = 1;		/* Set pointer back so processes ok.	*/
				break;
			}

			case EQUAL:
			{
				write_log(util,'I','R',"PROCESS","Processing EQUAL sign for equation.");
				aptr++;							/* Step past the equals sign.		*/
				next_ptr = aptr;
				if (*aptr == ' ') nexttok();

				if ( is_backref() ) process_back_ref(num_asn,num_var,num_val); /* Process backwards ref.	*/
				else if (letter(*aptr))
				{
					l_aptr = aptr;
					assign_label(num_asn);				/* See if is a label.			*/
					if (aptr == l_aptr)				/* If not processed then is a string.	*/
					{
						write_log(util,'I','R',"PROCESS","Processing ASSIGN literal for equation.");
						init_assign(num_asn);
						cstr = cur_assign->field1;
						while(*aptr != '\0' && *aptr != LNFD && *aptr != ' ' && (aptr - linein < WPLMAX))
						{
							*cstr++ = *aptr++;
						}
						*cstr = '\0';				/* Null the string.			*/
						strcpy(cur_assign->type,loc_str_typ);
						nvar++;					/* Increment fields in ASSIGN stmnt.	*/
						next_ptr = aptr;			/* Set so finds next token properly.	*/
						if (*aptr == ' ') nexttok();		/* Set up for next item.		*/
					}
				}
				break;							/* Break Out.				*/
			}
			case NUMBER0: case NUMBER1: case NUMBER2: case NUMBER3: case NUMBER4: case NUMBER5:
			case NUMBER6: case NUMBER7: case NUMBER8: case NUMBER9:
			case MULTIPLY: case DIVIDE: case MINUS: case PLUS:
			{
				write_log(util,'I','R',"PROCESS","Processing NUMBERS and OPERATIONS for ASSIGN.");
				if (!strcmp(cur_para->name,aptr))
				{
					write_log(util,'W','R',"INVALASGN","Invalid Assign TO Label may not be Correct.");
				}
				init_assign(num_asn);
				cstr = cur_assign->field1;
				if (operand(*aptr))
				{
					*cstr++ = toupper(*aptr++);			/* Copy operand value.			*/
				}
				else
				{
					while(tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Copy value.				*/
					nvar++;						/* Increment fields in ASSIGN stmnt.	*/
				}
				*cstr = '\0';						/* Null the string.			*/
				if (tndx == MULTIPLY || tndx == DIVIDE || tndx == MINUS || tndx == PLUS)
				{
					strcpy(cur_assign->type,int_compute);		/* Is an int and COMPUTE statement.	*/
				}
				else	strcpy(cur_assign->type,loc_int_typ);		/* Is an integer type .			*/

				next_ptr = aptr;
				if (*aptr == ' ') nexttok();
				if (*aptr == '(')					/* Paren for equation not substring.	*/
				{
					init_assign(num_asn);
					strcpy(cur_assign->field1,paren_sym);
					aptr++;						/* Step over paren.			*/
					next_ptr = aptr;
					if (*aptr == ' ') nexttok();
				}
				else if (*aptr == ',')					/* Length for the substring is next.	*/
				{
					aptr++;						/* Step past the comma seperator.	*/
					assign_length(num_var,num_val,astrt);
					next_ptr = aptr;
					if (*aptr == ' ') nexttok();
				}
				else if (letter(*aptr)) assign_label(num_asn);		/* See if is a label.			*/
				else if (*aptr != '\0' && *aptr != LNFD) setp = 1;	/* Set pointer back so processes ok.	*/
				break;
			} 								/* End of conector signs.		*/
			case OPNQUOTE:
			{

				write_log(util,'I','R',"PROCESS","Processing ASSIGN string for equation.");
				if (test_assign_flds())					/* Assign if should create literal.	*/
				{							/* Have substring field also.		*/
					if (*aptr == '\'')
					{
						init_assign(num_asn);
						aptr++;					/* Step past the open quote.		*/
						strcpy(cur_assign->type,str_literal);	/* Assign the type as a literal string.	*/
						(*filler_num)++;
						sprintf(name_buf,"%d",*filler_num);
						strcpy(cur_assign->field1,"&LITERAL-");	/* Set the assign field to literal.	*/
						strcat(cur_assign->field1,name_buf);
						init_current(num_var);			/* Add to the DECLARE list.		*/
						set_value(name_buf,num_asn,*current_row);
						sprintf(cur_assign->length,"%d",strlen(cur_decl->value));
						nvar++;					/* Increment fields in ASSIGN stmnt.	*/
						nexttok();
					}
					else break;					/* Is a variable so process.		*/
				}
				else
				{
					aptr++;						/* Step past the open quote.		*/
					init_assign(num_asn);
					cstr = cur_assign->field1;
					if (*aptr == '\0' || *aptr == LNFD)		/* If nothing between quotes then	*/
					{						/* save as a NULL.			*/
						cont = FALSE;
					}
					else 	cont = TRUE;

					while (cont)
					{
						while(*aptr != '\0' && *aptr != LNFD && *aptr != '\'' && (aptr - linein < WPLMAX))
						{
							if (*aptr == '"') *aptr = '\'';	/* Set quote withing string to single.	*/
							*cstr++ = *aptr++;
						}
						if (*aptr != '\'')			/* If not the end of string,		*/
						{					/* then read next line.			*/
							get_next_line();
						}
						else cont = FALSE;
					}
					*cstr = '\0';					/* Null the string.			*/
					strcpy(cur_assign->type,loc_str_typ);
					sprintf(cur_assign->length,"%d",strlen(cur_assign->field1));
					nvar++;						/* Increment fields in ASSIGN stmnt.	*/
					aptr++;						/* Step past the closing quote.		*/
					next_ptr = aptr;				/* Set so finds next token properly.	*/
					if (*aptr == ' ') nexttok();			/* Set up for next item.		*/
				}
				break;							/* Say good bye.			*/
			}
			default:
			{
				write_log(util,'E','R',"INVALID","Invalid ASSIGN equation key character.");
				break;							/* No default.				*/
			}
		}
	}
	if (nvar > 2)									/* If more than one field then is STRING*/
	{										/*  so need to create a receiver field	*/
		char newvar[FLDLEN];						 	/*  if not already there.		*/

		cur_assign = cur_cmd->assign_var;
		if (*cur_assign->start_pos)
		{
			strcpy(newvar,cur_assign->field1);
			strcat(newvar,"-");
			strcat(newvar,cur_assign->start_pos);
			strcat(newvar,"-");
			strcat(newvar,cur_assign->length);
			get_type_len(newvar,cur_assign->type,num_var,num_val,'C');
		}
	}
	if (setp) next_ptr = aptr-1;							/* Set back one so processes correctly.	*/
}


static void assign_label(int* num_asn)							/* See if is a label.			*/
{
	int fndlbl, len;
	char *lptr, *cstr;
	paragraph_item *pptr;

	lptr = aptr;
	len = 0;
	while (tststrt(lptr))
	{
		*lptr = toupper(*lptr);							/* Change to upper case.		*/
		len++;									/* Get the length of current token.	*/
		lptr++;
	}
	fndlbl = FALSE;
	pptr = cur_para;
	while (pptr)
	{
		if (!strncmp(aptr,pptr->name,len))					/* If var match the current label.	*/
		{
			fndlbl = TRUE;
			break;
		}
		pptr = pptr->prev_item;
	}
	if (fndlbl)									/* If var match a label.		*/
	{
		write_log(util,'I','R',"PROCESS","Processing LABEL for ASSIGN variable in equation.");
		init_assign(num_asn);
		cstr = cur_assign->field1;						/* Get the assign variable.		*/
		while(tststrt(aptr))
		{
			if (*aptr == '_') *aptr = '-';					/* Replace the underscore with dash.	*/
			else if (*aptr == '#') *aptr = '-';				/* Replace the # with dash.		*/
			*cstr++ = toupper(*aptr++);					/* Copy Variable.			*/
		}
		*cstr = '\0';								/* Null the string.			*/
		strcpy(cur_assign->type,int_return);					/* Is an integer return code var.	*/
		next_ptr = aptr;
		if (*aptr == ' ') nexttok();
	}
}

static void assign_length(int* num_var, int* num_val,assign_item *saptr)		/* Assign the length parameter for the	*/
											/* current substring.			*/
											/* Start of curr. list of assign items.	*/
{
	char *cstr, *pcstr;

	got_string = 1;									/* Set so will write CALL STRING source.*/
	if (*aptr == '*')								/* If no length given.			*/
	{
		while (*aptr != ')') aptr++;						/* Miss the equation in the ().		*/
		get_type_len(cur_assign->field1,cur_assign->length,num_var,num_val,'L');
	}
	else
	{
		if (saptr)	cstr = saptr->length;					/* Copy in length.			*/
		else	cstr = cur_assign->length;

		while (tststrt(aptr) && *aptr != ')') *cstr++ = *aptr++;
		*cstr = '\0';								/* Null terminate the string.		*/
		if (saptr && *saptr->length == '&')
		{									/* Get type of variable.		*/
			get_type_len(saptr->length,saptr->len_type,num_var,num_val,'T');
		}
		else if (*cur_assign->length == '&')
		{									 /* Get type of variable.		*/
			get_type_len(cur_assign->length,cur_assign->len_type,num_var,num_val,'T');
		}
	}
	aptr++;										/* Step past closing paren.		*/
	if (cur_assign->prev_item)							/* Check if assign to prev.		*/
	{
		pcstr = cur_assign->prev_item->start_pos;				/* Set pointer to previous start_pos.	*/
		if (!*pcstr)								/* If no values yet then copy in the	*/
		{									/* current vars values.			*/
			strcpy(pcstr,"1");						/* Start with 1st char.			*/
			pcstr = cur_assign->prev_item->length;				/* Set pointer to previous length.	*/
			strcpy(pcstr,cur_assign->length);
		}
	}
}

static void check_assign_types(int* num_var,int* num_val)				/* Make sure have correct types.	*/
{
	struct assign_item *prev;
	char typ;

	prev = cur_assign->prev_item;
	if (!prev || !prev->start_pos) return;						/* No prev item or substring field.	*/

	typ = *prev->type;								/* Get the type of previous field.	*/
	if (typ == *cur_assign->type) return;						/* Same type so ok.			*/

	write_log(util,'W','R',"ERRTYPE",
		"Different types used in ASSIGN statement for (%s) and (%s).",cur_assign->field1,prev->field1);
}

static int test_assign_flds()								/* Assign if should create literal.	*/
{
	struct assign_item *prev;
	char *hld_aptr;
	int tf_fl;

	prev = cur_assign->prev_item;
	if (!prev && *cur_assign->start_pos)
	{
		tf_fl = TRUE;
		hld_aptr = aptr;
		aptr++;									/* Step over open quote.		*/
		while (*aptr != '\'') aptr++;						/* Step past the string.		*/
		aptr++;				     					/* Step over closing quote.		*/
		while (*aptr == ' ') aptr++;						/* Step to next meaningful char.	*/
											/* Have more items so will be STRING.	*/
		if (*aptr != '[')
		{
			if (*aptr != '\0' && *aptr != LNFD && (aptr - linein <= WPLMAX)) tf_fl = FALSE;
		}
		aptr = hld_aptr;							/* Set the pointer back.		*/
		return(tf_fl);								/* Have sustring and no other fields.	*/
	}
	if (prev && *prev->field1 != '&') return(FALSE);				/* Have prev string and is quoted.	*/
	if (*cur_assign->start_pos) return(TRUE);					/* Have substring then create literal.	*/
	return(FALSE);									/* Is a quoted string field.		*/
}

static void process_back_ref(int* num_asn,int* num_var,int* num_val)			/* Process the backwards ref. field.	*/
{
	char *cstr;

	aptr++;										/* Step past the (.			*/
	write_log(util,'I','R',"PROCESS","Processing Backwards reference for ASSIGN variable in equation.");
	init_assign(num_asn);
	cur_assign->br_flag = 1;							/* Set have backwards reference.	*/ 
	cstr = cur_assign->field1;							/* Get the assign variable.		*/
	while(tststrt(aptr) && *aptr != ')')
	{
		if (*aptr == '.') *aptr = '-';						/* Replace the period with dash.	*/
		*cstr++ = toupper(*aptr++);						/* Copy Variable.			*/
	}
	*cstr = '\0';									/* Null the string.			*/
	get_type_len(cur_assign->field1,cur_assign->type,num_var,num_val,'A');
	get_type_len(cur_assign->field1,cur_assign->length,num_var,num_val,'L');
	strcpy(cur_assign->len_type,loc_int_typ);
	aptr++;										/* Step over the closing ).		*/
	next_ptr = aptr;
	if (*aptr == ' ') nexttok();
}
/*
**	History:
**	$Log: ptequatn.c,v $
**	Revision 1.8  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.7  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.6  1997/04/21 15:07:59  scass
**	Corrected copyright.
**	
**	Revision 1.5  1996-09-12 19:16:41-04  gsl
**	fix prototypes
**
**
**
*/
