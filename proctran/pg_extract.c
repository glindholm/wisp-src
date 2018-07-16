#define EXT extern
			/************************************************************************/
			/*	   PROCTRAN - Wang Procedure Language to VS COBOL Translator	*/
			/*			Copyright (c) 1990				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* PG_EXTRACT.C	*/

#include <stdio.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"
#include "pgextrct.h"

p_extract_kw(tndx)										/* Process the extract code.	*/
int tndx;
{
	switch(tndx)
	{
		case USE:
		case USING:
		{
			setflag();
			write_log("PROCTRAN",'W',"NOTPORTABLE","EXTRACT USING is not portable.  Rest of line not processed.");
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
		case IN:
		{
			setflag();
			write_log("PROCTRAN",'W',"NOTPORTABLE","EXTRACT IN is not portable.  Rest of line not processed.");
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
		case ON:
		{
			setflag();
			write_log("PROCTRAN",'W',"NOTPORTABLE","EXTRACT ON is not portable.  Rest of line not processed.");
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
		default:
		{
			write_log("PROCTRAN",'E',"NOTVALID","Invalid extract keyword.");
			break;
		}
	}
}

save_set_ext_var(num_var,num_val)
int *num_var, *num_val;
{
	char *cstr;

	if (in_set) write_log("PROCTRAN",'I',"PROCESS","Processing a SET variable.");
	else write_log("PROCTRAN",'I',"PROCESS","Processing an EXTRACT variable.");

	while( *aptr == '&' || *aptr == '\'' || *aptr == '=' || *aptr == ',' || *aptr == '#' || letter(*aptr) || number(*aptr))
	{
		if (*aptr == '&')
		{
			init_se();							/* Allocate set extract area.		*/
			cstr = cur_set_extr->var;					/* Move in the variable.		*/
			while (tststrt(aptr) && *aptr != '=' && *aptr != '(')
			{
				if (*aptr == '_') *aptr = '-';				/* Change underscore to dash.		*/
				*cstr++ = toupper(*aptr++);
			}
			*cstr = '\0';							/* Null terminate the string.		*/
			get_type_len(cur_set_extr->var,cur_set_extr->type,num_var,num_val,'T');
			while (*aptr == ' ') aptr++;					/* Step to next token.			*/
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
		if (aptr - inline >= WPLMAX) break;					/* Don't process past MOD code.		*/
	}
}

p_readfdr(num_var,num_val)
int *num_var, *num_val;
{
	char *cstr, *ctype, *scstr;
	int i, done, onf, nf, filefl;
	int *cbr, full_br;

	write_log("PROCTRAN",'I',"PROCESS","Processing an EXTRACT/READFDR variable.");

	init_rfdr();									/* Allocate readfdr area.		*/
	if (*aptr == '&')
	{
		cstr = cur_rfdr->recvr;							/* Move in the variable.		*/
		while (tststrt(aptr) && *aptr != '=')
		{
			if (*aptr == '_') *aptr = '-';					/* Change underscore to dash.		*/
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
		if ((i=strpos(aptr,"RECORDS ")) >= 0 && (i=strpos(aptr,"USED ")) >= 0 && (i=strpos(aptr,"BY ")) >= 0)
		{
			strcpy(cur_rfdr->id,"RC");					/* Init keyword field request.		*/
			while (*aptr != 'Y') aptr++;					/* Step past phrase.			*/
			aptr++;
		}
		else if ((i=strpos(aptr,"BLOCKS ")) >= 0 && (i=strpos(aptr,"ALLOCATED ")) >= 0 && (i=strpos(aptr,"FOR ")) >= 0)
		{
			strcpy(cur_rfdr->id,"BA");					/* Init keyword field request.		*/
			while (*aptr != 'R') aptr++;					/* Step past phrase.			*/
			aptr++;
		}
		else
		{
			write_log("PROCTRAN",'E',"INVALID","Invalid field keyword for READFDR.  Not supported.");
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

			if (!memcmp(aptr,"IN ",3))
			{
				cstr = cur_rfdr->lib;					/* Assign the library value.		*/
				aptr += 3;
				ctype = cur_rfdr->lib_type;
				cbr = &cur_rfdr->lib_br;
			}
			else if (!memcmp(aptr,"ON ",3))
			{
				onf = TRUE;						/* Set that have ON key word.		*/
				cstr = cur_rfdr->vol;					/* Assign the volume value.		*/
				aptr += 3;
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
			if (*aptr == '\'')
			{
				aptr++;							/* Step over open quote.		*/
				while (tststrt(aptr) && *aptr != '\'') *cstr++ = *aptr++;
			}
			else
			{
				if (*aptr == '(')					/* Is it backwards referenced?		*/
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
				else while (tststrt(aptr)) *cstr++ = toupper(*aptr++);
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

			if (aptr - inline >= WPLMAX) done = TRUE;			/* Don't process past MOD code.		*/
			if ((*aptr == '\0' || *aptr == LNFD) && onf ) done = TRUE;	/* Set so get out of loop.		*/
		}
	}
	else
	{
		write_log("PROCTRAN",'E',"INVALID","Invalid variable assignment for READFDR.  Line not processed.");
		while (*aptr != '\0' && *aptr != LNFD) nexttok();			/* Step to end of line.			*/
	}
	setflag();
}
