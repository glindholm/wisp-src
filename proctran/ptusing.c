static char copyright[]="Copyright (c) 1988-1997 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";

#define EXT extern

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"
#include "pgeqtns.h"
#include "pgkeyw.h"

void p_prg_kw(int tndx, int* num_var, int* num_val)					/* Process program keywords.		*/
{
	char *hptr, *cstr;

	write_log("PROCTRAN",'I','R',"PROCESS","Processing program keyword.");
	switch (tndx)
	{
		case USING:								/* I'm passing parameters.		*/
		{
			in_using = 1;							/* Got a linkage section for next prog.	*/
			nexttok();
			break;								/* Get out.				*/
		}
		case ON:								/* Get volume name.			*/
		{
			nexttok();							/* Gets run program vol name.		*/
			if (tststrt(aptr))
			{
				char *cs;

				if ( is_backref() )					/* Process the backwards reference.	*/
				{
					aptr++;						/* Step over the (.			*/
					cur_prg->br_flag = 1;
					cur_prg->vol_br = 1;				/* Set flag.				*/
					cstr = cur_prg->vol;
					while(tststrt(aptr) && *aptr != ')')
					{
						if (*aptr == '.') *aptr = '-';		/* Change period to dash.		*/
						*cstr++ = toupper(*aptr++);		/* Copy var into structure.		*/
					}
					*cstr = '\0';					/* Null terminate it.			*/
					if (*aptr == ')') aptr++;			/* Step over closing ).			*/
					get_type_len(cur_prg->vol,cur_prg->vol_type,num_var,num_val,'A'); /* Get type of var.	*/
				}
				else
				{
					if (*aptr == '(') aptr++;			/* Step over the open parenthesis.	*/
					cs = cur_prg->vol;
					while (tststrt(aptr)) *cs++ = toupper(*aptr++);	/* Load the volume name.		*/
					if (*aptr == ')') aptr++;			/* Step over the closing paren.		*/
					*cs = '\0';					/* Null terminate the string.		*/
					cur_prg->vol_type[1] = 'V';			/* Set so know what to match to.	*/
					if (*cur_prg->vol == '&') get_type_len(cur_prg->vol,cur_prg->vol_type,num_var,num_val,'T');
					else strcpy(cur_prg->vol_type,"S ");
				}
			}
			else	write_log("PROCTRAN",'E','R',"NOVOL","No volume supplied for program keyword."); 
			break;								/* Get out.				*/
		}
		case IN:								/* Get the library name.		*/
		{
			char *tptr, *htptr, tststr[FLDLEN];

			nexttok();							/* Gets run program lib name.		*/
			if (tststrt(aptr))
			{
				char *cs;

				if ( is_backref() )					/* Process the backwards reference.	*/
				{
					aptr++;						/* Step over the (.			*/
					cur_prg->br_flag = 1;
					cur_prg->lib_br = 1;				/* Set flag.				*/
					cstr = cur_prg->lib;
					while(tststrt(aptr) && *aptr != ')')
					{
						if (*aptr == '.') *aptr = '-';		/* Change period to dash.		*/
						*cstr++ = toupper(*aptr++);		/* Copy var into structure.		*/
					}
					*cstr = '\0';					/* Null terminate it.			*/
					if (*aptr == ')') aptr++;			/* Step over closing ).			*/
					get_type_len(cur_prg->lib,cur_prg->lib_type,num_var,num_val,'A'); /* Get type of var.	*/
				}
				else
				{
					if (*aptr == '(') aptr++;			/* Step over the open parenthesis.	*/
					cs = cur_prg->lib;
					while (tststrt(aptr)) *cs++ = toupper(*aptr++); /* Load the library name.		*/
					if (*aptr == ')') aptr++;			/* Step over the closing paren.		*/
					*cs = '\0';					/* Null terminate the string.		*/
					cur_prg->lib_type[1] = 'L';			/* Set so know what to match to.	*/
					if (*cur_prg->lib == '&') get_type_len(cur_prg->lib,cur_prg->lib_type,num_var,num_val,'T');
					else strcpy(cur_prg->lib_type,"S ");
				}

				hptr = aptr;						/* Remember ptr.			*/
				nexttok();						/* Step to next token.			*/
				htptr = aptr;						/* Remember temp ptr.			*/
				tptr = tststr;
				while (tststrt(aptr)) *tptr++ = toupper(*aptr++); 	/* Load the temp token.			*/
				*tptr = '\0';						/* Null terminate the string.		*/
				
				if (!strcmp(tststr,"ON") || !strcmp(tststr,"USING"))	/* Test is it ON or USING key word?	*/
				{							/* Yes, so set pointers so will process	*/
					aptr = hptr;					/*  on the next nexttok() call.		*/
					next_ptr = aptr;
				}
				else
				{							/* NO, is concatenation so set field.	*/
					aptr = htptr;					/* Set ptr back to correct position.	*/
					if (*aptr == '\'') aptr++;			/* Step past the open quote.		*/
					cs = cur_prg->lib2;
					while (tststrt(aptr) && *aptr != '\'') *cs++ = toupper(*aptr++); /* Load the lib2 name.	*/
					*cs = '\0';					/* Null terminate the string.		*/
					if (*cur_prg->lib2 == '&')
					{
						get_type_len(cur_prg->lib2,cur_prg->lib2_type,num_var,num_val,'T');
					}
					else strcpy(cur_prg->lib2_type,"S ");
					if (*aptr == '\'') aptr++;			/* Step past the closing quote.		*/
				}
			}
			else	write_log("PROCTRAN",'E','R',"NOLIB","No library supplied for program keyword."); 
			break;								/* Get out.				*/
		}
		default:								/* Nothing found get out.		*/
		{
			write_log("PROCTRAN",'E','R',"INVALID","Invalid program keyword."); 
			break;								/* Get out.				*/
		}
	}
}

void save_using_item(int* num_asn,int* num_var,int* filler_num,int* current_row,int* num_val)	/* Process using item.		*/
{
	char *cstr, name_buf[FLDLEN];
	int cont, tndx;

	write_log("PROCTRAN",'I','R',"PROCESS","Processing using variable.");

	cont = TRUE;

	while (cont)									/* While still have USING items on line.*/
	{
		cur_prg->number_using++; 						/* Got a variable keep count.		*/

		if (cur_prg->number_using > 32)						/* Give a warning to users of limitation.*/
		{
			write_log("PROCTRAN",'E','R',"MAXPARMS",
				"Exceeded maximum number of parameters in CALL - %d.",cur_prg->number_using);
		}

		if ( is_backref() )							/* Process the backwards reference.	*/
		{
			aptr++;								/* Step over the (.			*/
			init_using();
			cur_using->br_flag = 1;						/* Set flag.				*/
			cstr = cur_using->var;
			while(tststrt(aptr) && *aptr != ')')
			{
				if (*aptr == '.') *aptr = '-';				/* Change period to dash.		*/
				*cstr++ = toupper(*aptr++);				/* Copy var into structure.		*/
			}
			*cstr = '\0';							/* Null terminate it.			*/
			if (*aptr == ')') aptr++;					/* Step over closing ).			*/
			get_type_len(cur_using->var,cur_using->type,num_var,num_val,'A'); /* Get the type of the var.		*/
		}
		else
		{
			if (*aptr == '(') aptr++;					/* Step over the open parenthesis.	*/
			while (*aptr == ' ') aptr++;

			if ( *aptr == '&')
			{
				init_using();
				cstr = cur_using->var;
				while(tststrt(aptr) && *aptr != ')')
				{
					if (*aptr == '_') *aptr = '-';			/* Change underscore to dash.		*/
					else if (*aptr == '#') *aptr = '-';		/* Change # to dash.			*/
					*cstr++ = toupper(*aptr++);			/* Copy var into structure.		*/
				}
				*cstr = '\0';						/* Null terminate it.			*/
				get_type_len(cur_using->var,cur_using->type,num_var,num_val,'T'); /* Get the type of the var.	*/
			}
			else if (*aptr == '\'' || *aptr == '"')
			{
				if (*aptr == '\'') aptr++;				/* Step past the open quote.		*/
				(*filler_num)++;					/* Count the literal strings.		*/
				sprintf(name_buf,"%d",*filler_num); 			/* Map for variable name.		*/
				init_current(num_var);
				set_value(name_buf,num_asn,*current_row);
			}
			else if (number(*aptr))
			{                        
				(*filler_num)++;					/* Count the literal strings.		*/
				sprintf(name_buf,"%d",*filler_num); 			/* Map for variable name.		*/
				init_current(num_var);
				makeint(aptr,name_buf,num_asn);
			}
			if (*aptr == ')') aptr++;					/* Step over the closing paren.		*/
		}

		nexttok();
		tndx = find_keyword( aptr, run_clause );				/* Look for a RUN clause keyword.	*/
		if ( tndx == CANCEL || tndx == DISPLAY || tndx == ENTER || tndx == ERROR ) /* If a valid RUN clause keyword;	*/
		{
			aptr--;								/* Set ptr so will process correctly.	*/
			next_ptr = aptr;
			cont = FALSE;							/* Set flag to get out of loop.		*/
		}

		if (!tststrt(aptr)) cont = FALSE;
	}
}
/*
**	History:
**	$Log: ptusing.c,v $
**	Revision 1.5  1997-04-21 11:23:51-04  scass
**	Corrected copyright.
**
**	Revision 1.4  1996-09-12 19:18:14-04  gsl
**	Fix prototypes
**
**
**
*/
