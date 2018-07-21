/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

#define EXT extern

/* PG_EXTRACT.C	*/

#include <stdio.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"
#include "pgextrct.h"

static void process_back_ref(int* num_var, int* num_val);				/* Process the backwards reference	*/

void p_extract_kw(int tndx)								/* Process the extract code.	*/
{
	switch(tndx)
	{
		case USE:
		case USING:
		{
			setflag();
			write_log("PROCTRAN",'W','R',"NOTPORTABLE","EXTRACT USING is not portable.  Rest of line not processed.");
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
		case IN:
		{
			setflag();
			write_log("PROCTRAN",'W','R',"NOTPORTABLE","EXTRACT IN is not portable.  Rest of line not processed.");
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
		case ON:
		{
			setflag();
			write_log("PROCTRAN",'W','R',"NOTPORTABLE","EXTRACT ON is not portable.  Rest of line not processed.");
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
		default:
		{
			write_log("PROCTRAN",'E','R',"NOTVALID","Invalid extract keyword.");
			break;
		}
	}
}

void save_set_ext_var(int* num_var,
		      int* num_val,
		      int* num_asn,
		      int* filler_num,
		      int* current_row,
		      int* num_cmds)
{
	char *cstr, *l_aptr;
	int nvar;

	if (in_set) write_log("PROCTRAN",'I','R',"PROCESS","Processing a SET variable.");
	else write_log("PROCTRAN",'I','R',"PROCESS","Processing an EXTRACT variable.");

	nvar = 0;
	while( *aptr == '&' || *aptr == '\'' || *aptr == '=' || *aptr == ',' || *aptr == '#' || letter(*aptr) || number(*aptr))
	{
		if (*aptr == '&')
		{
			init_se();							/* Allocate set extract area.		*/
			cstr = cur_set_extr->var;					/* Move in the variable.		*/
			while (tststrt(aptr) && *aptr != '=' && *aptr != '(')
			{
				if (*aptr == '_') *aptr = '-';				/* Change underscore to dash.		*/
				else if (*aptr == '#') *aptr = '-';			/* Change # to dash.			*/
				*cstr++ = toupper(*aptr++);
			}
			*cstr = '\0';							/* Null terminate the string.		*/
			get_type_len(cur_set_extr->var,cur_set_extr->type,num_var,num_val,'T');
			next_ptr = aptr;
			if (*aptr == ' ') nexttok();
			if (*aptr == '(')						/* If a substring with EXTRACT.		*/
			{
				cstr = aptr;						/* Set the current ptr.			*/
				cstr++;							/* Step past open parenthesis.		*/
				if (*cstr == '1' && *(cstr+2) == '*')			/* If using the full value of var.	*/
				{
					cur_set_extr->dlmtr = '*';			/* Set so uses blank delimeter.		*/ 
					aptr = cstr+4;					/* Step past (1,*)			*/
					next_ptr = aptr;
				}
				else if (*cur_set_extr->start_pos || nvar)		/* If already assign substring or is	*/
				{							/*  more than one var in staement.	*/
					process_sub_var(*cur_set_extr->type,num_var,num_val,3,
							num_asn,filler_num,current_row,num_cmds);
				}
				else
				{
					char newvar[FLDLEN];

					aptr++;						/* Step past the open paren.		*/
					got_string = 1;					/* Set, will generate a CALL "STRING".	*/
					cstr = cur_set_extr->start_pos;	
					while (*aptr != ',') *cstr++ = *aptr++;		/* Copy in the starting position.	*/
					*cstr = '\0';					/* Null terminate the string.		*/
					if (*cur_set_extr->start_pos == '&')
					{						 /* Get type of variable.		*/
						get_type_len(cur_set_extr->start_pos,cur_set_extr->stpos_type,num_var,num_val,'T');
					}
					aptr++;						/* Step over the comma.			*/
					cstr = cur_set_extr->length;
					while (*aptr != ')') *cstr++ = *aptr++;		/* Copy in the length.			*/
					*cstr = '\0';					/* Null terminate the string.		*/
					if (*cur_set_extr->length == '&')
					{						 /* Get type of variable.		*/
						get_type_len(cur_set_extr->length,cur_set_extr->len_type,num_var,num_val,'T');
					}

					strcpy(newvar,cur_set_extr->var);
					strcat(newvar,"-");
					strcat(newvar,cur_set_extr->start_pos);
					strcat(newvar,"-");
					strcat(newvar,cur_set_extr->length);
					get_type_len(newvar,cur_set_extr->type,num_var,num_val,'C');

					aptr++;						/* Step over the closing paren.		*/
					next_ptr = aptr;
				}
			}
			nvar++;								/* Increment vars in EXTRACT/SET stmnt.	*/
			if (*aptr == ' ') nexttok();					/* Step to next token.			*/
			if (!strncmp(aptr,"EQ ",3) || !strncmp(aptr,"eq ",3))		/* If operation is =, change to symbol.	*/
			{
				l_aptr = aptr;
				*l_aptr++ = '=';
				*l_aptr = ' ';
			}
		}
		else if (*aptr == '\'')							/* Check if got a string var,		*/
		{									/*  if I do load it up.			*/
			init_se();							/* Allocate set extract area.		*/
			aptr++;								/* Step over the open quote.		*/
			cstr = cur_set_extr->var;					/* Move in the variable.		*/
			while (*aptr != '\'' && *aptr != '\0' && *aptr != LNFD) *cstr++ = *aptr++;
			*cstr = '\0';							/* Nullterminate the string.		*/
			aptr++;								/* Step over the closing quote.		*/
			next_ptr = aptr;
			nexttok();
		}
		else if (*aptr == '(')							/* Check if got an index with the if,	*/
		{									/*  if so store it for comment.		*/
			cstr = cur_set_extr->key; 					/* Move in the variable.		*/
			while (tststrt(aptr) && *aptr != '=') *cstr++ = toupper(*aptr++);
			*cstr = '\0';							/* Null terminate the string.		*/
			while (*aptr == ' ') aptr++;					/* Step to next token.			*/
		}
		else if (*aptr == '=' || *aptr == ',')					/* Check = sign, if so do for equation.	*/
		{
			if (*aptr == '=')						/* Check if value field given.		*/
			{
				l_aptr = aptr;
				l_aptr++;
				while (*l_aptr && !tststrt(l_aptr) && *l_aptr != ',') l_aptr++;
				if ( is_backref())					/* Have backwards reference?		*/
				{
					init_se();
					aptr = l_aptr;
					process_back_ref(num_var,num_val);
				}
				else if (*l_aptr == ',') init_se();			/* If run into a , then no value given.	*/

			}
			aptr++;								/* Step past the symbol.		*/
			while (*aptr == ' ') aptr++;					/* Step to next token.			*/
		}
		else if (letter(*aptr) || *aptr == '#')
		{
			init_se();							/* Allocate set extract area.		*/
			cstr = cur_set_extr->var;					/* Move in the variable.		*/
			while (tststrt(aptr) && *aptr != '=') *cstr++ = toupper(*aptr++);
			*cstr = '\0';							/* Null terminate the string.		*/
			while (*aptr == ' ') aptr++;					/* Step to next token.			*/
		}
		else if (operand(*aptr) || number(*aptr))				/* Check if numeric after the eqauls.	*/
		{
			init_se();							/* Allocate Set extract area.		*/
			cstr = cur_set_extr->var;
			while (tststrt(aptr) && *aptr != '=') *cstr++ = *aptr++;
			*cstr = '\0';							/* Null terminate the string.		*/
		}
		next_ptr = aptr;
		if (aptr - linein >= WPLMAX) break;					/* Don't process past MOD code.		*/
	}
}

void p_readfdr(int* num_var,int* num_val)
{
	char *cstr, *ctype, *scstr;
	int i, done, onf, nf, filefl;
	int *cbr, full_br;

	write_log("PROCTRAN",'I','R',"PROCESS","Processing an EXTRACT/READFDR variable.");

	init_rfdr();									/* Allocate readfdr area.		*/
	full_br = FALSE;								/* Assume is not backwards referenced.	*/
	if (*aptr == '&')
	{
		cstr = cur_rfdr->recvr;							/* Move in the variable.		*/
		while (tststrt(aptr) && *aptr != '=')
		{
			if (*aptr == '_') *aptr = '-';					/* Change underscore to dash.		*/
			else if (*aptr == '#') *aptr = '-';				/* Change # to dash.			*/
			*cstr++ = toupper(*aptr++);
		}
		*cstr = '\0';								/* Null terminate the string.		*/
		get_type_len(cur_rfdr->recvr,cur_rfdr->type,num_var,num_val,'T');
		while (*aptr == ' ') aptr++;						/* Step to next token.			*/

		if (*aptr == '=')							/* Check = sign.			*/
		{
			aptr++;								/* Step past the symbol.		*/
			while (*aptr == ' ') aptr++;					/* Step to next token.			*/
		}
		if (( (i=strpos(aptr,"RECORDS")) >= 0 && (i=strpos(aptr,"USED")) >= 0 && (i=strpos(aptr,"BY")) >= 0 ) ||
		    ( (i=strpos(aptr,"records")) >= 0 && (i=strpos(aptr,"used")) >= 0 && (i=strpos(aptr,"by")) >= 0 ))
		{
			strcpy(cur_rfdr->id,"RC");					/* Init keyword field request.		*/
			while (*aptr != 'Y' && *aptr != 'y') aptr++;			/* Step past phrase.			*/
			aptr++;
		}
		else if (( (i=strpos(aptr,"BLOCKS")) >= 0 && (i=strpos(aptr,"ALLOCATED")) >= 0 && (i=strpos(aptr,"FOR")) >= 0 ) ||
			 ( (i=strpos(aptr,"blocks")) >= 0 && (i=strpos(aptr,"allocated")) >= 0 && (i=strpos(aptr,"for")) >= 0 ))
		{
			strcpy(cur_rfdr->id,"BA");					/* Init keyword field request.		*/
			while (*aptr != 'R' && *aptr != 'r') aptr++;			/* Step past phrase.			*/
			aptr++;
		}
		else
		{
			write_log("PROCTRAN",'E','R',"INVALID","Invalid field keyword for READFDR.  Not supported.");
			strcpy(cur_rfdr->id,"  ");
			return;
		}
		next_ptr = aptr;							/* Set so processing is correct.	*/
		nexttok();
		done = FALSE;
		onf = FALSE;								/* Set that don't have ON phrase.	*/
		nf = FALSE;								/* Set that not next line flag.		*/
		filefl = FALSE;								/* Set that don't have file yet.	*/
		while (!done)
		{
			if (!tststrt(aptr))						/* If no more tokens on line.		*/
			{
				get_next_line();
				nf = TRUE;
				if (*aptr == ' ') nexttok();				/* Move to first token on line.		*/
			}

			if (!memcmp(aptr,"IN",2) || !memcmp(aptr,"in",2))
			{
				cstr = cur_rfdr->lib;					/* Assign the library value.		*/
				aptr += 2;
				ctype = cur_rfdr->lib_type;
				cbr = &cur_rfdr->lib_br;
			}
			else if (!memcmp(aptr,"ON",2) || !memcmp(aptr,"on",2))
			{
				onf = TRUE;						/* Set that have ON key word.		*/
				cstr = cur_rfdr->vol;					/* Assign the volume value.		*/
				aptr += 2;
				ctype = cur_rfdr->vol_type;
				cbr = &cur_rfdr->vol_br;
			}
			else if (nf && filefl)
			{
				break;							/* Break out because not part of	*/
			}								/*  current statement.			*/
			else
			{
				cstr = cur_rfdr->file;					/* Assign the file value.		*/
				ctype = cur_rfdr->file_type;
				cbr = &cur_rfdr->name_br;
				filefl = TRUE;						/* Set flag have file name.		*/
			}
			scstr = cstr;							/* Save start of cur string for later.	*/

			while (*aptr == ' ') aptr++;					/* Step to next token.			*/
			if (!tststrt(aptr))						/* If no more tokens on line.		*/
			{
				get_next_line();
				nf = TRUE;
				if (*aptr == ' ') nexttok();				/* Move to first token on line.		*/
			}

			if (*aptr == '\'')
			{
				aptr++;							/* Step over open quote.		*/
				while (tststrt(aptr) && *aptr != '\'') *cstr++ = *aptr++;
			}
			else
			{
				if ( is_backref() )					/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					full_br = TRUE;					/* Set so uses full back. ref.		*/
					cur_rfdr->br_flag = 1;				/* Set to indicate using backwards ref.	*/
					*cbr = 1;					/* Set to indicate field uses back ref.	*/
					aptr++;						/* Step over open paren.		*/
					while (tststrt(aptr) && *aptr != ')')
					{
						if (*aptr == '.')
						{
							full_br = FALSE;		/* Set so only name is back. ref'ed.	*/
							*aptr = '-';			/* Change . to -			*/
						}
						*cstr++ = toupper(*aptr++); 		/* Copy the keyword value.		*/
					}
					aptr++;						/* Step over the closing paren.		*/
				}
				else
				{
					if (*aptr == '(') aptr++;			/* Step over the open parenthesis.	*/
					while (tststrt(aptr)) *cstr++ = toupper(*aptr++);
					if (*aptr == ')') aptr++;			/* Step over the closing paren.		*/
				}
			}
			*cstr = '\0';							/* Null terminate the string.		*/
			if (*scstr == '&') get_type_len(scstr,ctype,num_var,num_val,'T');
			else if (*cbr && !full_br)					/* If have backwards referenced field.	*/
			{
				get_type_len(scstr,ctype,num_var,num_val,'A');
			}
			else strcpy(ctype,"S ");

			if (full_br)							/* Set the library and volume params.	*/
			{
				char tlbl[FLDLEN];

				strcpy(tlbl,cur_rfdr->file);				/* Get the putparm label.		*/
				strcat(cur_rfdr->file,"-FILE");				/* Set the file field.			*/
				get_type_len(cur_rfdr->file,cur_rfdr->file_type,num_var,num_val,'A');

				cur_rfdr->lib_br= 1;					/* Set to indicate field uses back ref.	*/
				strcpy(cur_rfdr->lib,tlbl);
				strcat(cur_rfdr->lib,"-LIBRARY");			/* Set the library field.		*/
				get_type_len(cur_rfdr->lib,cur_rfdr->lib_type,num_var,num_val,'A');

				cur_rfdr->vol_br= 1;					/* Set to indicate field uses back ref.	*/
				strcpy(cur_rfdr->vol,tlbl);
				strcat(cur_rfdr->vol,"-VOLUME");			/* Set the volume field.		*/
				get_type_len(cur_rfdr->vol,cur_rfdr->vol_type,num_var,num_val,'A');
			}

			next_ptr = aptr;						/* Set so nexttok functions properly.	*/
			nexttok();

			if (aptr - linein >= WPLMAX) done = TRUE;			/* Don't process past MOD code.		*/
			if ((*aptr == '\0' || *aptr == LNFD) && onf ) done = TRUE;	/* Set so get out of loop.		*/
		}
	}
	else
	{
		write_log("PROCTRAN",'E','R',"INVALID","Invalid variable assignment for READFDR.  Line not processed.");
		while (*aptr != '\0' && *aptr != LNFD) nexttok();			/* Step to end of line.			*/
	}
	setflag();
}

static void process_back_ref(int* num_var, int* num_val)				/* Process the backwards reference	*/
											/*  on the SET statement.		*/
{
	char *cstr, temp [FLDLEN];
	set_extract_item *hld_set_ext;
	int found;

	aptr++;										/* Step over open paren.		*/
	cstr = cur_set_extr->var;							/* Move in the variable.		*/
	while (tststrt(aptr) && *aptr != ')')
	{
		if (*aptr == '.') *aptr = '-';						/* Change . to -			*/
		*cstr++ = toupper(*aptr++); 						/* Copy the keyword value.		*/
	}
	*cstr = '\0';									/* Null terminate the string.		*/
	get_type_len(cur_set_extr->var,cur_set_extr->type,num_var,num_val,'A');
	strcpy(temp,cur_set_extr->var);
	hld_set_ext = cur_set_extr;
	found = FALSE;
	cur_set_extr = cur_cmd->set_extract_area;
	while (cur_set_extr)								/* See if already used back ref on this	*/
	{										/*  command.				*/
		if (0 == (strcmp(temp,cur_set_extr->var)) && cur_set_extr != hld_set_ext )
		{
			found = TRUE;
			break;
		}
		cur_set_extr = cur_set_extr->next_item;
	}
	cur_set_extr = hld_set_ext;							/* Set ptr back to current item.	*/

	if (found) cur_set_extr->br_flag = 2;						/* Set to indicate using backwards ref.	*/
	else cur_set_extr->br_flag = 1;							/*  2 means don't issue putparm.	*/
}
/*
**	History:
**	$Log: ptextrct.c,v $
**	Revision 1.7  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.6  1997/04/21 15:09:20  scass
**	Corrected copyright.
**	
**	Revision 1.5  1996-09-12 19:16:52-04  gsl
**	Fix prototypes
**
**
**
*/
