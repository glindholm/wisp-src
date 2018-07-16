#define EXT extern
			/************************************************************************/
			/*	   PROCTRAN - Wang Procedure Language to VS COBOL Translator	*/
			/*			Copyright (c) 1990				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* PG_IF.C	*/

#include <stdio.h>
#include <ctype.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"

p_if_kw(tndx,num_var,num_val)								/* Proccess IF keyword.			*/
int tndx, *num_var, *num_val;
{
	char cond, *cstr;

	write_log("PROCTRAN",'I',"PROCESS","Processing if keyword.");
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
			cur_if->cond[2] = '1';						/* Got an or load it.			*/
			if ( nexttok() )						/* Gets run program file name.		*/
			{
				cstr = cur_if->file;					/* Set ptr to file value.		*/
				if (*aptr == '\'') aptr++;				/* Step over the open quote.		*/
				if (*aptr == '(')					/* Is it backwards referenced?		*/
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
				else while (tststrt(aptr) && *aptr != '\'') *cstr++ = toupper(*aptr++); /* Copy in file name.	*/

				*cstr = '\0';						/* Null terminate the string.		*/
				if (cur_if->file_br) get_type_len(cur_if->file,cur_if->file_type,num_var,num_val,'A');
				else if (*cur_if->file == '&') get_type_len(cur_if->file,cur_if->file_type,num_var,num_val,'T');
				else strcpy(cur_if->file_type,"S ");
			}
			else	write_log("PROCTRAN",'E',"NOFILENAME","No FILENAME to assign in if keyword.");
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
				if (*aptr == '(')					/* Is it backwards referenced?		*/
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
				else while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Copy in library name.		*/

				*cstr = '\0';						/* Null terminate the string.		*/
				if (cur_if->lib_br) get_type_len(cur_if->lib,cur_if->lib_type,num_var,num_val,'A');
				else if (*cur_if->lib == '&') get_type_len(cur_if->lib,cur_if->lib_type,num_var,num_val,'T');
				else strcpy(cur_if->lib_type,"S ");
			}
			else	write_log("PROCTRAN",'E',"NOLIB","No LIBRARY name to assign in if keyword.");
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
				if (*aptr == '(')
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
				else while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Copy in volume name.			*/

				*cstr = '\0';						/* Null terminate the string.		*/
				if (cur_if->vol_br) get_type_len(cur_if->vol,cur_if->vol_type,num_var,num_val,'A');
				else if (*cur_if->vol == '&') get_type_len(cur_if->vol,cur_if->vol_type,num_var,num_val,'T');
				else strcpy(cur_if->vol_type,"S ");
			}
			else	write_log("PROCTRAN",'E',"NOVOL","No VOLUME to assign in if keyword.");
			break;
		}						
		default:
		{
			write_log("PROCTRAN",'E',"INVALID","Invalid if keyword.");
			break;
		}
	}
}

save_if_var(num_var,num_val)								/* Proccess IF variable.		*/
int *num_var, *num_val;
{
	char *lptr, *cstr, ptyp;
	int len, pos, fndlbl, cont;
	paragraph_item *pptr;
	int setfl;
	char *tptr, *hptr, tmpstr[FLDLEN];

	write_log("PROCTRAN",'I',"PROCESS","Processing if statement.");
	lptr = aptr;
	len = 0;
	while ((*lptr != '=') && tststrt(lptr++)) len++;				/* Get the length of current token.	*/
	fndlbl = FALSE;
	pptr = cur_para;
	while (pptr && len)								/* While have list of labels and length	*/
	{										/*  to compare to.			*/
		if (islower(*aptr) || islower(*(aptr+1)))				/* Is compare string lowercase?		*/
		{
			char temp[255], *cptr, *lptr;

			cptr = temp;							/* Set ptr to temp string.		*/
			lptr = aptr;
			while (*lptr != ' ') *cptr++ = *lptr++;
			*cptr = '\0';							/* Null terminate string.		*/
			upper_string(temp);						/* Convert string to upper case.	*/
			if (!strncmp(temp,pptr->name,len))				/* If var match the current label.	*/
			{
				fndlbl = TRUE;
				break;
			}
		}
		else if (!strncmp(aptr,pptr->name,len))					/* If var match the current label.	*/
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
		while (tststrt(aptr)) *cstr++ = toupper(*aptr++);			/* Copy in return code var.		*/
		*cstr = '\0';								/* Null terminate the string.		*/
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
			{
				process_sub_var(ptyp,num_var,num_val,1,0);		/* Generate working storage for substr.	*/
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
				int numq, rem;

				numq = 1;
				while (*lptr != '\0' && *lptr != LNFD)			/* Count the num of quotes on line.	*/
				{
					if (*lptr++ == '\'') numq++;
				}
				if (numq % 2) cont = FALSE;				/* If uneven num of quotes.		*/
			}

			while (cont)
			{
				while(*aptr != '\0' && *aptr != LNFD && *aptr != '\'' && (aptr - inline < WPLMAX))
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

process_sub_var(type,num_var,num_val,caller,num_asn)					/* Generate working storage for substr.	*/
char type;
int *num_var, *num_val, caller, *num_asn;
{
	char start[5], flen[5], *tptr;							/* Temp var to hold subscript values.	*/
	char newvar[FLDLEN], ntype[3];
	char *cvptr, *ctptr;

	write_log("PROCTRAN",'I',"SUBSCRIPT","Processing subscript variable (%s).",cur_if->var);

	if (caller == 1) cur_if->sub_flag = 1;						/* Set the subscript flag.		*/
	else if (caller == 2) cur_assign->sub_flag = 1;
	else
	{
		write_log("PROCTRAN",'E',"ERRSUBSCRIPT","Processing unknown subscript variable.");
		return;
	}
	aptr++;										/* Step over the open (.		*/
	tptr = start;									/* Set ptr to start position.		*/
	while (*aptr != ',') *tptr++ = *aptr++;						/* Copy start position to start var.	*/
	*tptr = '\0';									/* Null terminate.			*/
	aptr++;										/* Step over the open comma.		*/
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

	cvptr = newvar;									/* Set the ptrs to var and type fields.	*/
	ctptr = ntype;
	strcat(newvar,"-");								/* Add seperator for clarity.		*/
	if (*start == '&')
	{
		strcat(newvar,&start[1]);
		if (caller == 1) cur_if->sub_flag = 2;					/* Set flag that start is a variable.	*/
		else if (caller == 2) cur_assign->sub_flag = 2;
	}
	else strcat(newvar,start);
	strcat(newvar,"-");		     						/* Add seperator for clarity.		*/
	if (*flen == '&')
	{
		strcat(newvar,&flen[1]);
		if (caller == 1)
		{
			if (cur_if->sub_flag == 2) cur_if->sub_flag = 4;		/* Set flag that length is a var.	*/
			else cur_if->sub_flag = 3;
		}
		else if (caller == 2)
		{
			if (cur_assign->sub_flag == 2) cur_assign->sub_flag = 4;	/* Set flag that length is a var.	*/
			else cur_assign->sub_flag = 3;
		}
	}
 	else strcat(newvar,flen);
	if (caller == 1)
	{
		init_if();	  	     		 				/* Allocate IF area.			*/
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
	}
	sprintf(cur_decl->length,"(%s)",flen);						/* Set the length for declare.		*/
}                                                                                       
