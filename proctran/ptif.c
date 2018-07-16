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

/* PG_IF.C	*/

#include <stdio.h>
#include <ctype.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"

static int start_num = 1;
static int tstdel();
static void process_complex_sub(char type,
				int* num_var,
				int* num_val,
				int  caller,
				int* num_asn,
				int* filler_num,
				int* current_row,
				int* num_cmds,
				char* fld,
				char* tbuf);

void p_if_kw(int tndx,
	     int* num_var,
	     int* num_val,
	     int* filler_num,
	     int* current_row,
	     int* num_cmds)								/* Proccess IF keyword.			*/
{
	char cond, *cstr;

	write_log("PROCTRAN",'I','R',"PROCESS","Processing if keyword.");
	switch (tndx)
	{
		case AND: case OR: case IFLT: case IFGT: case IFNE: case IFEQ: case IFGE: case IFLE:
		case IFNEQ: case IFNLT: case IFNGT: 
		{
			if (tndx == AND)	cond = 'A';				/* Set the appropriate condition.	*/
			else if (tndx == OR)	cond = 'O';
			else if (tndx == IFLT)	cond = 'L';
			else if (tndx == IFGT)  cond = 'G';
			else if (tndx == IFNE)  cond = 'N';
			else if (tndx == IFEQ)  cond = 'E';
			else if (tndx == IFGE)  cond = 'B';
			else if (tndx == IFLE)  cond = 'C';
			else if (tndx == IFNEQ) cond = 'N';
			else if (tndx == IFNLT) cond = 'B';
			else if (tndx == IFNGT) cond = 'C';
			else
			{
				write_log("PROCTRAN",'E','R',"INVALID","Invalid if keyword.");
				break;
			}

			cur_if->cond[0] = cond;						/* Assign it to the structure.		*/
			cur_if->cond[1] = '\0';						/* Null terminate the string.		*/
			break;
		}
		case NOT:
		{
			if (!cur_cmd->if_area) init_if();				/* If No area then load one.		*/
			cur_if->cond[1] = '1';
			break;
		}
		case EXISTS:
		{
			if (!cur_cmd->if_area) init_if();				/* If No area then load one.		*/
			got_find = 1;
			*cur_if->cond = '1';
			break;
		}						
		case FILENAME:
		{
			char *l_aptr, ptyp;

			cur_if->cond[2] = '1';						/* Got an or load it.			*/
			if ( nexttok() )						/* Gets run program file name.		*/
			{
				cstr = cur_if->file;					/* Set ptr to file value.		*/
				if (*aptr == '\'') aptr++;				/* Step over the open quote.		*/
				if ( is_backref() )					/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					cur_if->br_flag = 1;				/* Set to indicate using backwards ref.	*/
					cur_if->file_br = 1;				/* Set to indicate field uses back ref.	*/
					aptr++;						/* Step over open paren.		*/
					while (tststrt(aptr) && *aptr != ')')
					{
						if (*aptr == '.') *aptr = '-';		/* Change . to -			*/
						*cstr++ = toupper(*aptr++); 		/* Copy the keyword value.		*/
					}
					aptr++;						/* Step over the closing paren.		*/
				}
				else
				{
					if (*aptr == '(') aptr++;			/* Step over open parenthesis.		*/
					while (tststrt(aptr) && *aptr != '\'') *cstr++ = toupper(*aptr++); /* Copy in file name.*/
					if (*aptr == ')') aptr++;			/* Step  over the closing paren.	*/
				}
				*cstr = '\0';						/* Null terminate the string.		*/
				if (cur_if->file_br) get_type_len(cur_if->file,cur_if->file_type,num_var,num_val,'A');
				else if (*cur_if->file == '&') get_type_len(cur_if->file,cur_if->file_type,num_var,num_val,'T');
				else strcpy(cur_if->file_type,"S ");

				l_aptr = aptr;
				while (*l_aptr == ' ') l_aptr++;			/* Step to next token.			*/ 
				if (*cur_if->file == '&' && *l_aptr == '(')		/* Have substring so notify.		*/
				{ 							/* Gen working storage substring.	*/
					aptr = l_aptr;
					next_ptr = aptr;
					ptyp = *cur_if->file_type;			/* Set type to S or I.			*/
					process_sub_var(ptyp,num_var,num_val,5,0,filler_num,current_row,num_cmds);
				}

			}
			else	write_log("PROCTRAN",'E','R',"NOFILENAME","No FILENAME to assign in if keyword.");
			break;
		}
		case LIBRARY: case IFIN:
		{
			if (tndx == LIBRARY) cur_if->cond[2] = '2';
			nexttok();
			if (!tststrt(aptr))						/* If no more tokens on line.		*/
			{
				get_next_line();
				if (*aptr == ' ') nexttok();				/* Move to first token on line.		*/
			}
			if (tststrt(aptr))						/* If tokens on line, get RUN program	*/
			{								/*  library name.			*/
				cstr = cur_if->lib;					/* Set ptr to lib value.		*/
				if ( is_backref() )					/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					cur_if->br_flag = 1;				/* Set to indicate using backwards ref.	*/
					cur_if->lib_br = 1;				/* Set to indicate field uses back ref.	*/
					aptr++;						/* Step over open paren.		*/
					while (tststrt(aptr) && *aptr != ')')
					{
						if (*aptr == '.') *aptr = '-';		/* Change . to -			*/
						*cstr++ = toupper(*aptr++); 		/* Copy the keyword value.		*/
					}
					aptr++;						/* Step over the closing paren.		*/
				}
				else
				{
					if (*aptr == '(') aptr++;			/* Step over the open parenthesis.	*/
					while (tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Copy in library name.		*/
					if (*aptr == ')') aptr++;			/* Step over the closing paren.		*/
				}
				*cstr = '\0';						/* Null terminate the string.		*/
				if (cur_if->lib_br) get_type_len(cur_if->lib,cur_if->lib_type,num_var,num_val,'A');
				else if (*cur_if->lib == '&') get_type_len(cur_if->lib,cur_if->lib_type,num_var,num_val,'T');
				else strcpy(cur_if->lib_type,"S ");
			}
			else	write_log("PROCTRAN",'E','R',"NOLIB","No LIBRARY name to assign in if keyword.");
			break;
		}
		case VOLUME: case IFON:
		{
			if (tndx == VOLUME) cur_if->cond[2] = '3';
			nexttok();
			if (!tststrt(aptr))						/* If no more tokens on line.		*/
			{
				get_next_line();
				if (*aptr == ' ') nexttok();				/* Move to first token on line.		*/
			}
			if (tststrt(aptr))						/* If tokens on line, get RUN program	*/
			{								/*  volume name.			*/
				cstr = cur_if->vol;					/* Set ptr to vol value.		*/
				if ( is_backref() )
				{
					cur_if->br_flag = 1;				/* Set to indicate using backwards ref.	*/
					cur_if->vol_br = 1;				/* Set to indicate field uses back ref.	*/
					aptr++;						/* Step over open paren.		*/
					while (tststrt(aptr) && *aptr != ')')
					{
						if (*aptr == '.') *aptr = '-';		/* Change . to -			*/
						*cstr++ = toupper(*aptr++); 		/* Copy the keyword value.		*/
					}
					aptr++;						/* Step over the closing paren.		*/
				}
				else
				{
					if (*aptr == '(') aptr++;			/* Step over open parenthesis.		*/
					while (tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Copy in volume name.		*/
					if (*aptr == ')') aptr++;			/* Step  over the closing paren.	*/
				}
				*cstr = '\0';						/* Null terminate the string.		*/
				if (cur_if->vol_br) get_type_len(cur_if->vol,cur_if->vol_type,num_var,num_val,'A');
				else if (*cur_if->vol == '&') get_type_len(cur_if->vol,cur_if->vol_type,num_var,num_val,'T');
				else strcpy(cur_if->vol_type,"S ");
			}
			else	write_log("PROCTRAN",'E','R',"NOVOL","No VOLUME to assign in if keyword.");
			break;
		}						
		default:
		{
			write_log("PROCTRAN",'E','R',"INVALID","Invalid if keyword.");
			break;
		}
	}
}

void save_if_var(int* num_var,
		 int* num_val,
		 int* filler_num,
		 int* current_row,
		 int* num_cmds)				/* Proccess IF variable.		*/
{
	char *lptr, *cstr, ptyp = '\0';
	int len, pos, fndlbl, cont;
	paragraph_item *pptr;
	int setfl;
	char *tptr, *hptr, tmpstr[FLDLEN];
	char temp[255];

	write_log("PROCTRAN",'I','R',"PROCESS","Processing if statement.");
	len = 0;
	lptr = aptr;
	if (*aptr == '(') lptr++;
	while (*lptr == ' ') lptr++;
	cstr = temp;									/* Set ptr to temp string.		*/
	while ((*lptr != '=' && *lptr != ')' && *lptr != ' ') && tststrt(lptr))
	{
		*cstr++ = *lptr++;
		len++;									/* Get the length of current token.	*/
	}
	*cstr = '\0';									/* Null terminate string.		*/
	upper_string(temp);								/* Convert string to upper case.	*/
	fndlbl = FALSE;
	pptr = cur_para;
	while (pptr && len)								/* While have list of labels and length	*/
	{										/*  to compare to.			*/
		if (!strncmp(temp,pptr->name,len))					/* If var match the current label.	*/
		{
			fndlbl = TRUE;
			break;
		}
		pptr = pptr->prev_item;
	}

	if (fndlbl)									/* If var match a label.		*/
	{
		init_if();								/* Allocate IF area.			*/
		cstr = cur_if->var;							/* Set ptr to variable.			*/
		*cstr++ = '&';								/* Set so is a variable.		*/
		if (*aptr == '(') aptr++;
		while (*aptr == ' ') aptr++;
		while (tststrt(aptr) && *aptr != ')') *cstr++ = toupper(*aptr++);	/* Copy in return code var.		*/
		*cstr = '\0';								/* Null terminate the string.		*/
		if (*aptr == ')') aptr++;
		strcpy(cur_if->type,"IR");
		return;
	}

	setfl = FALSE;									/* Set flag for type test.		*/
	if (cur_if) ptyp = *cur_if->type;						/* Init current type.			*/
	while (tstdel(aptr) || *aptr == '&' || *aptr == '\'' || *aptr == ')' || operand(*aptr) || number(*aptr))
	{
		if (*aptr == '&' || operand(*aptr) || number(*aptr))			/* Check if var, numeric, operand, or	*/
		{									/* letter.				*/
			init_if();							/* Allocate IF area.			*/
			cstr = cur_if->var;
			while (tststrt(aptr) && !tstdel(aptr) && *aptr != '=')
			{
				if (*aptr == '_') *aptr = '-';				/* Change the underscore to dash.	*/
				else if (*aptr == '#') *aptr = '-';			/* Change the # to dash.		*/
				*cstr++ = toupper(*aptr++);
			}
			*cstr = '\0';
			if (*cur_if->var == '&') get_type_len(cur_if->var,cur_if->type,num_var,num_val,'T');
			else if (letter(*cur_if->var)) strcpy(cur_if->type,"S ");
			else if (number(*cur_if->var) && ptyp == 'I') strcpy(cur_if->type,"I ");
			else if (*cur_if->var == '-' && ptyp == 'I') strcpy(cur_if->type,"I ");
			else strcpy(cur_if->type,"S ");
			if (!setfl)							/* If flag not set then set type.	*/
			{
				ptyp = *cur_if->type;					/* Set type to S or I.			*/
				setfl = TRUE;
			}
			else	setfl = FALSE;						/* Set back so assign next one ok.	*/
			if (*cur_if->var == '&' && *aptr == '(') 			/* Have substring so notify.		*/
			{ 								/* Gen working storage substring.	*/
				process_sub_var(ptyp,num_var,num_val,1,0,filler_num,current_row,num_cmds);
			}
			if (*aptr == ' ') nexttok();	
		}
		else if (*aptr == '\'')
		{
			aptr++;								/* Miss the open quote.			*/
			init_if();
			cstr = cur_if->var;
			cont = TRUE;
			lptr = aptr;
			if (*lptr == ' ')						/* Test for nothing between quotes then	*/
			{								/* save as a NULL.			*/
				int numq;

				numq = 1;
				while (*lptr != '\0' && *lptr != LNFD)			/* Count the num of quotes on line.	*/
				{
					if (*lptr++ == '\'') numq++;
				}
				if (numq % 2) cont = FALSE;				/* If uneven num of quotes.		*/
			}

			while (cont)
			{
				while(*aptr != '\0' && *aptr != LNFD && *aptr != '\'' && (aptr - linein < WPLMAX))
				{
					*cstr++ = *aptr++;
				}
				if (*aptr != '\'')					/* If not the end of string,		*/
				{							/* then read next line.			*/
					get_next_line();
				}
				else cont = FALSE;
			}
			*cstr = '\0';							/* Null the string.			*/
			if (*aptr == '\'') aptr++;					/* Step over closing quote.		*/
			strcpy(cur_if->type,"S ");					/* Set the type to a string.		*/
			next_ptr = aptr;
			nexttok();
		}
		else if (*aptr == ')')							/*  Check if got index with if, if so	*/
		{									/*   store it for comment.		*/
			pos = 0;
			while (cur_if->paren_value[pos++] != ' ');
			cur_if->paren_value[pos++] = *aptr;
			cur_if->paren_value[pos] = '\0';				/* Null terminate the string.		*/
			nexttok();
		}
		else if (*aptr == '(')
		{
			pos = 0;							/* Load to the first one.		*/
			while (tststrt(aptr) && *aptr != '>' && *aptr != '=' && *aptr != '<')
			{								/* If val in cond, Paren logic not index.*/
				if ( !cur_cmd->if_area ||				/* If var with paren, its around logic	*/
				    (!strncmp(cur_if->var,"           ",11) &&
				     !strncmp(cur_if->paren_value,"           ",11)) ||
				    strncmp(cur_if->cond,"  ",2) )
				{
				 	init_if();
					if (!pos)
					{
						cur_if->paren_value[pos++] = *aptr++;
						cur_if->paren_value[pos] = '\0';
					}
					else
					{
						cur_if->var[pos-1] = *aptr++;
						cur_if->var[pos++] = '\0';
					}
				}
				else	cur_if->paren_value[pos++] = *aptr++;
			}
			cur_if->paren_value[pos] = '\0';				/* Null terminate the string.		*/
			aptr++;								/* Step over the symbol.		*/
			if (*aptr == ' ') nexttok();	
		}
		else if (*aptr == '<')							/* Check for < sign, set condition.	*/
		{
			if (aptr[1] == '>' || aptr[1] == 'E' )
			{
				strcpy(cur_if->cond,"N");
				aptr++;							/* Step past the < symbol.		*/
			}
			else if (aptr[1] == '=')
			{
				strcpy(cur_if->cond,"F");
				aptr++;							/* Step past the = symbol.		*/
			}
			else	strcpy(cur_if->cond,"L");
			aptr++;								/* Step over the < or > symbol.		*/
			if (*aptr == ' ') nexttok();
		}
		else if (*aptr == '=')
		{
			strcpy(cur_if->cond,"E");					/* Check for = sign, set condition.	*/
			aptr++;								/* Step over the = symbol.		*/
			if (*aptr == ' ') nexttok();
		}
		else if (*aptr == '>')
		{
			if (aptr[1] == '=')
			{
				strcpy(cur_if->cond,"D");
				aptr++;							/* Step past the = symbol.		*/
			}
			else strcpy(cur_if->cond,"G");					/* Check for > sign, set condition.	*/
			aptr++;								/* Step over the > symbol.		*/
			if (*aptr == ' ') nexttok();
		}
	}

	hptr = aptr;									/* Save the current position ptr.	*/
	tptr = tmpstr;
	while (tststrt(aptr)) *tptr++ = toupper(*aptr++); 				/* Copy in temp string.			*/
	*tptr = '\0';									/* Null terminate the string.		*/

	if (!strcmp(tmpstr,"THEN")) nexttok();						/* Don't process the THEN keyword.	*/
	else aptr = hptr;

	if (*aptr != '\0' && *aptr != LNFD) next_ptr = aptr-1;				/* Set so loops properly.		*/
}

static int tstdel(cptr)									/* Test if is a delimeter.		*/
char * cptr;
{
	if (*cptr == '=' || *cptr == '(' || *cptr == '>' || *cptr == '<') return(TRUE);
	else return(FALSE);
}

void process_sub_var(char type,
		     int* num_var,
		     int* num_val,
		     int  caller,
		     int* num_asn,
		     int* filler_num,
		     int* current_row,
		     int* num_cmds)							/* Generate working storage for substr.	*/
{
	char start[FLDLEN], flen[5], *tptr;						/* Temp var to hold subscript values.	*/
	char newvar[FLDLEN], ntype[3];
	char *cvptr, *ctptr;

	write_log("PROCTRAN",'I','R',"SUBSCRIPT","Processing subscript variable (%s).",cur_if->var);

	if (caller == 1 || caller == 5) cur_if->sub_flag = 1;				/* Set the subscript flag.		*/
	else if (caller == 2) cur_assign->sub_flag = 1;
	else if (caller == 3) cur_set_extr->sub_flag = 1;
	else if (caller == 4) cur_ren_sctch->sub_flag = 1;
	else
	{
		write_log("PROCTRAN",'E','R',"ERRSUBSCRIPT","Processing unknown subscript variable.");
		return;
	}
	got_string = 1;									/* Set so will write CALL STRING source.*/
	aptr++;										/* Step over the open (.		*/
	if (*aptr == '(')								/* Complex start field.			*/
	{
		char tbuf[80], *tb;
		int nq;

		nq = 1;
		if (caller != 2)
		{
			write_log("PROCTRAN",'E','R',"INCORRASS",
				"Assumed only an ASSIGN statemen. Caller(%d)  Notify NeoMedia Migrations.",caller);
			return;
		}

		sprintf(start,"&START%d",start_num);
		start_num++;								/* Increment the generated start field.	*/
		strcpy(ntype,"I ");
		get_type_len(start,ntype,num_var,num_val,'C');				/* Add to declare list.			*/

		tb = tbuf;	
		while (*aptr != ',' || nq > 0)
		{
			*tb++ = *aptr++;						/* Save the complex start equation.	*/
			if (*aptr == '(') nq++;
			if (*aptr == ')') nq--;
		}
		*tb = '\0';								/* Null terminate the buffer.		*/
		process_complex_sub(type,num_var,num_val,caller,num_asn,filler_num,current_row,num_cmds,start,tbuf);
	}
	else										/* Simple start field.			*/
	{
		tptr = start;								/* Set ptr to start position.		*/
		while (*aptr != ',') *tptr++ = toupper(*aptr++);			/* Copy start position to start var.	*/
		*tptr = '\0';								/* Null terminate.			*/
	}
	aptr++;										/* Step over the comma.			*/
	tptr = flen;									/* Set ptr to length indicator.		*/
	while (*aptr != ')') *tptr++ = *aptr++;						/* Copy length to flen var.		*/
	*tptr = '\0';									/* Null terminate.			*/
	aptr++;										/* Step over the closing paren.		*/
	next_ptr = aptr;								/* Set ptr so processes correctly.	*/

	if (caller == 1)
	{
		strcpy(ntype,cur_if->type);						/* Init the type var.			*/
		strcpy(newvar,cur_if->var);						/* Start with the current var name.	*/
	}
	else if (caller == 2)
	{
		strcpy(ntype,cur_assign->type);						/* Init the type var.			*/
		strcpy(newvar,cur_assign->field1);					/* Start with the current var name.	*/
	}
	else if (caller == 3)
	{
		strcpy(ntype,cur_set_extr->type);					/* Init the type var.			*/
		strcpy(newvar,cur_set_extr->var);					/* Start with the current var name.	*/
	}
	else if (caller == 4)
	{
		strcpy(cur_ren_sctch->start_pos,start);
		strcpy(cur_ren_sctch->stpos_type,"I ");
		strcpy(cur_ren_sctch->length,flen);
		strcpy(cur_ren_sctch->len_type,"I ");
		strcpy(ntype,cur_ren_sctch->name_type);					/* Init the type var.			*/
		strcpy(newvar,cur_ren_sctch->name);					/* Start with the current var name.	*/
	}
	else if (caller == 5)
	{
		strcpy(ntype,cur_if->file_type);					/* Init the type var.			*/
		strcpy(newvar,cur_if->file);						/* Start with the current var name.	*/
	}

	cvptr = newvar;									/* Set the ptrs to var and type fields.	*/
	ctptr = ntype;
	strcat(newvar,"-");								/* Add seperator for clarity.		*/
	if (*start == '&')
	{
		strcat(newvar,&start[1]);
		if (caller == 1 || caller == 5) cur_if->sub_flag = 2;			/* Set flag that start is a variable.	*/
		else if (caller == 2) cur_assign->sub_flag = 2;
		else if (caller == 3) cur_set_extr->sub_flag = 2;
		else if (caller == 4) cur_ren_sctch->sub_flag = 2;
	}
	else strcat(newvar,start);
	strcat(newvar,"-");		     						/* Add seperator for clarity.		*/
	if (*flen == '&')
	{
		strcat(newvar,&flen[1]);
		if (caller == 1 || caller == 5)
		{
			if (cur_if->sub_flag == 2) cur_if->sub_flag = 4;		/* Set flag that length is a var.	*/
			else cur_if->sub_flag = 3;
		}
		else if (caller == 2)
		{
			if (cur_assign->sub_flag == 2) cur_assign->sub_flag = 4;	/* Set flag that length is a var.	*/
			else cur_assign->sub_flag = 3;
		}
		else if (caller == 3)
		{
			if (cur_set_extr->sub_flag == 2) cur_set_extr->sub_flag = 4;	/* Set flag that length is a var.	*/
			else cur_set_extr->sub_flag = 3;
		}
		else if (caller == 4)
		{
			if (cur_ren_sctch->sub_flag == 2) cur_ren_sctch->sub_flag = 4;	/* Set flag that length is a var.	*/
			else cur_ren_sctch->sub_flag = 3;
		}
	}
 	else strcat(newvar,flen);
	if (caller == 1 || caller == 5)
	{
		if (caller == 1)
		{
			init_if();	 	 				/* Allocate IF area.			*/
		}
		else if (*cur_if->var)							/* Display message if already have var.	*/
		{
			write_log("PROCTRAN",'E','R',"HAVEVAR",
				"Already have variable (%s) for this IF structure, Notify NeoMedia Migrations.",cur_if->var);
		}

		strcpy(cur_if->var,newvar);
		strcpy(cur_if->type,ntype);
		get_type_len(cur_if->var,cur_if->type,num_var,num_val,'C');		/* Add to declare list.			*/
	}
	else if (caller == 2)
	{
		init_assign(num_asn);							/* Allocate ASSIGN area.		*/
		strcpy(cur_assign->field1,newvar);
		strcpy(cur_assign->type,ntype);
		get_type_len(cur_assign->field1,cur_assign->type,num_var,num_val,'C');	/* Add to declare list.			*/
		sprintf(cur_assign->length,"(%s)",flen);
	}
	else if (caller == 3)
	{
		init_se();								/* Allocate SET/EXTRACT area.		*/
		strcpy(cur_set_extr->var,newvar);
		strcpy(cur_set_extr->type,ntype);
		get_type_len(cur_set_extr->var,cur_set_extr->type,num_var,num_val,'C');	/* Add to declare list.			*/
	}
	else if (caller == 4)
	{
		get_type_len(newvar,ntype,num_var,num_val,'C');				/* Add to declare list.			*/
	}										/*  structure list item.		*/
}                                                                                       
											 /* Process complex start/length field.	*/
static void process_complex_sub(char type,
				int* num_var,
				int* num_val,
				int  caller,
				int* num_asn,
				int* filler_num,
				int* current_row,
				int* num_cmds,
				char* fld,
				char* tbuf)
{
	assign_item *h_assign;
	command_item *h_cmd, *prev_cmd;	
	char l_buf[80], h_linein[STRBUFF];
	char *h_aptr;
	int h_num_asn;
	
	write_log("PROCTRAN",'I','R',"COMPLEXSUB","Processing complex subscript variable (%s).",fld);

	strcpy(l_buf,fld);								/* Generate an ASSIGN to process.	*/
	strcat(l_buf," = ");
	strcat(l_buf,tbuf);

	h_assign = cur_assign;								/* Save current pointers for later.	*/
	h_aptr = aptr;									/* Set pointers to process new ASSIGN.	*/
	strcpy(h_linein,linein);
	h_num_asn = *num_asn;
	h_cmd = cur_cmd;

	strcpy(linein,l_buf);
	aptr = linein;
	next_ptr = aptr;

	*num_asn = 0;									/* Count number of variables in field.	*/
	in_assign = 1;									/* In the assign clause.		*/
	init_cmd(num_cmds);								/* Allocate next command block.		*/
	strcpy(cur_cmd->command,"ASSIGN");						/* Initialize the command line.		*/
	p_assign(num_asn,num_var,filler_num,current_row,num_val,num_cmds);

	prev_cmd = h_cmd->prev_item;							/* Re-assign the COMMAND list pointers	*/
	if (prev_cmd)									/*  so that the complex ASSIGN is	*/
	{										/*  processed first when writing the	*/
		prev_cmd->next_item = cur_cmd;						/*  COBOL source.			*/
		cur_cmd->prev_item = prev_cmd;
	}
	else										/* Is the first command in paragraph	*/
	{										/*  so need to redefine ptr for cmds.	*/
		cur_para->all_command = cur_cmd;
		cur_cmd->prev_item = h_cmd->prev_item;
	}
	h_cmd->next_item = cur_cmd->next_item;
	h_cmd->prev_item = cur_cmd;
	cur_cmd->next_item = h_cmd;

	cur_cmd = h_cmd;								/* Set back to continue processing.	*/
	cur_assign = h_assign;
	*num_asn = h_num_asn;
	strcpy(linein,h_linein);
	aptr = h_aptr;
	next_ptr = aptr;
}
/*
**	History:
**	$Log: ptif.c,v $
**	Revision 1.11  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.10  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.9  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.8  1997/04/21 15:10:54  scass
**	Corrected copyright.
**	
**	Revision 1.7  1996-12-12 13:37:44-05  gsl
**
**	Revision 1.6  1996-09-12 16:17:13-07  gsl
**	fix prototypes
**
**
**
*/
