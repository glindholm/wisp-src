#define EXT extern
			/************************************************************************/
			/*	   PROCTRAN - Wang Procedure Language to VS COBOL Translator	*/
			/*			Copyright (c) 1990				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* PG_SCREEN.C	*/

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"
#include "pgkeyw.h"

p_scn_kw(tndx)										/* Process screen keywords.		*/
int tndx;
{
	setscreen();									/* Reset screen flags.			*/

	switch (tndx)
	{
		case CENTER: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing CENTER screen keyword.");
			cur_fac_msk |= FMCENTER;					/* Got to center line.			*/
			break;								/* Leave this option.			*/
		}
		case BLANK: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing BLANK screen keyword.");
			cur_fac_msk |= FMBLANK;						/* Got a lopro.				*/
			break;								/* Leave this option.			*/
		}
		case BLINK: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing BLINK screen keyword.");
			cur_fac_msk |= FMBLINK;						/* Set flash fac.			*/
			break;								/* Leave this option.			*/
		}
		case BRIGHT: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing BRIGHT screen keyword.");
			cur_fac_msk |= FMBRIGHT;					/* Set hi fac.				*/
			break;								/* Leave this option.			*/
		}
		case DIM: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing DIM screen keyword.");
			cur_fac_msk |= FMDIM;						/* set lofac.				*/
			break;								/* Leave this option.			*/
		}
		case LINE: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing LINE screen keyword.");
			cur_fac_msk |= FMLINE;						/* set undeline fac.			*/
			break;								/* Leave this option.			*/
		}
		case NUMERIC: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing NUMERIC screen keyword.");
			cur_mod_fac = 'N';						/* Set numeric fac.			*/
			break;								/* Leave this option.			*/
		}
		case TAB: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing TAB screen keyword.");
			cur_fac_msk |= FMTAB;						/* Set in tab fac.			*/
			break;								/* Leave this option.			*/
		}
		case UPLOW: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing UPLOW screen keyword.");
			cur_mod_fac = 'B';						/* Set field upper and lower case.	*/
			break;								/* Leave this option.			*/
		}
		case UPPER: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing UPPER screen keyword.");
			cur_mod_fac = 'U';						/* Set only upper case.			*/
			break;								/* Leave this option.			*/
		}
		case ROW: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing ROW screen keyword.");
			in_row = 1;							/* Found row keyword.			*/
			cur_scn->num_rows = -1;						/* Set so doesn't center vertically.	*/
			nexttok();
			break;								/* Leave this option.			*/
		}
		case ERASE: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing ERASE screen keyword.");
			in_erase = 1;							/* Got an erase command.		*/
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step past the keyword.		*/
			if (*aptr != '=') nexttok();
			break;								/* Leave this option.			*/
		}
		case ALARM: 
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing ALARM screen keyword.");
			in_alarm = 1;							/* Set the alarm command.		*/
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step past the keyword.		*/
			if (*aptr != '=') nexttok();
			break;								/* Leave this option.			*/
		}
		case PFKEY:
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing PFKEY screen keyword.");
			in_pfkey = 1;
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step to next token on line.		*/
			if (*aptr == ' ') nexttok();					/* If the equation is spaces out.	*/
			break;								/* Leave PF loop.			*/
		}
		case CURROW:
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing CURROW screen keyword.");
			in_currow = 1;
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step to next token on line.		*/
			if (*aptr == ' ') nexttok();					/* If the equation is spaces out.	*/
			break;								/* Leave PF loop.			*/
		}
		case CURCOL:
		{
			write_log("PROCTRAN",'I',"PROCESS","Processing CURCOL screen keyword.");
			in_curcol = 1;
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step to next token on line.		*/
			if (*aptr == ' ') nexttok();					/* If the equation is spaces out.	*/
			break;								/* Leave PF loop.			*/
		}
		default:				
		{
			write_log("PROCTRAN",'E',"INVALID","Invalid SCREEN keyword.");
			break;
		}
	}
}

save_screen_item(num_asn,num_var,num_val,current_row)					/* Save the screen expression.		*/
int *num_asn, *num_var, *current_row, *num_val;
{
	char *cstr;
	int crfm, ndx, semi_beg;							/* Current row FAC mask.		*/

	write_log("PROCTRAN",'I',"PROCESS","Processing a SCREEN variable.");
	crfm = cur_fac_msk;								/* Save for column assignment.		*/
	if (*aptr == ';') semi_beg = TRUE;
	else semi_beg = FALSE;								/* Assume ; are not at beginning.	*/

	while (*aptr == '&' || *aptr == '\'' || *aptr == ';' || *aptr == ',')	 	/* Loop while find info.		*/
	{
		while (*aptr == ';')							/* If ; so generate blank line.		*/
		{
			(*current_row)++;
			if (cur_scn->num_rows != -1) cur_scn->num_rows++;		/* Keep track of # displayable rows.	*/
			aptr++;								/* Step past the ;			*/
			cur_fac_msk = 0;						/* Clear the FACs for this line.	*/
		}
		while (*aptr == ',')
		{
			aptr++;								/* Step over the comma seperator.	*/
			next_ptr = aptr;						/* Set so nexttok() gets past string.	*/
			if (*aptr == ' ') nexttok();
			if (*aptr == '\0' || *aptr == LNFD)
			{
				get_next_line();					/* Continuation on next line.		*/
				if (*aptr == ' ') nexttok();
			}
		}

		if (*aptr == '\'')
		{
			aptr++;								/* Step past the opening quote.		*/
			if (*aptr != '\0' && *aptr != LNFD && *aptr != ';' && *aptr != ',')
			{
				init_screen_field(current_row);				/* Allocate field area for screen.	*/
				set_value("1",num_asn,*current_row);			/* Get value for picture.		*/
			}
			next_ptr = aptr;						/* Set so nexttok() gets past string.	*/
			if (*aptr == ' ') nexttok();
			if (*aptr == '\0' || *aptr == LNFD) cur_fac_msk = 0;		/* Clear the facs for this field.	*/
		}

		if (*aptr == '&')
		{
			init_screen_field(current_row);					/* Allocate field area for screen.	*/
			cstr = cur_scn_fld->screen_fld;					/* Set ptr to field.			*/

			while (tststrt(aptr) && *aptr != ';' && *aptr != '(')
			{
				if (*aptr == '_') *aptr = '-';				/* Replace the underscore with dash.	*/
				*cstr++ = toupper(*aptr++); 				/* Copy var name to screen field.	*/
			}
			*cstr = '\0';							/* Null terminate the string.		*/
			if (*aptr == '(')						/* Step past subscript stuff.		*/
			{
				write_log("PROCTRAN",'E',"SUBSCRIPT",
					"Subscripting of screen vaiable (%s). Manual changes required.",cur_scn_fld->screen_fld);
				while (*aptr != ')') aptr++;				/* Step over subsrcipt.			*/
				aptr++;							/* Step past the closing parenthesis.	*/
			}
			get_type_len(cur_scn_fld->screen_fld,cur_scn_fld->scn_fld_type,num_var,num_val,'T');
			cur_scn_fld->row = *current_row;				/* Load the current row.		*/
			cur_scn_fld->modf = cur_mod_fac;				/* Set the mod fac type.		*/
			cur_mod_fac = ' ';						/* Set back to no FAC.			*/

			if (*cur_scn_fld->scn_fld_type == 'S')
			{								/* Get the length of variable.		*/
				get_type_len(cur_scn_fld->screen_fld,cur_scn_fld->len,num_var,num_val,'S');
			}
			next_ptr = aptr;						/* Set so nexttok() gets past string.	*/
			if (*aptr == ' ') nexttok();
			if (*aptr == '\0' || *aptr == LNFD) cur_fac_msk = 0;		/* Clear the facs for this field.	*/
		}
		while ((ndx = find_keyword(aptr,screen_keywords)) >= 0)			/* Search for screen keywords.		*/
		{									/* Found a match so process.		*/
			p_scn_kw(ndx);
			nexttok();							/* Step past the screen keyword.	*/
			semi_beg = FALSE;						/* Set back because processing line.	*/
			crfm = cur_fac_msk;						/* Save for column assignment.		*/
		}
	}

	if (!semi_beg && cur_scn_fld) eval_column(cur_scn_fld->row,crfm);		/* Set the column positions for row.	*/
	next_ptr = aptr;								/* Set ptr to current position.		*/
}

static eval_column(crow,fac_msk)							/* Set the correct columns for current 	*/
int crow, fac_msk;									/* row of screen display.		*/
{
	scn_fld_item *stitem, *hld_scn_fld;
	int totlen, ccol, len;

	hld_scn_fld = cur_scn_fld;							/* store previous if item.		*/
	totlen = 0;									/* Set to 0 length.			*/
	cur_scn_fld = cur_scn->variable_area;						/* Load the variable area.		*/
	while (cur_scn_fld && cur_scn_fld->row != crow)
	{
		cur_scn_fld = cur_scn_fld->next_item;					/* Step to the current row to process.	*/
	}
	stitem = cur_scn_fld;								/* Save the starting item for later.	*/
	while (cur_scn_fld && cur_scn_fld->row == crow)					/* Get total length of fields on line.	*/
	{
		if (*cur_scn_fld->scn_fld_type == 'S') len = len_to_int(cur_scn_fld->len);
		else if (cur_scn_fld->modf == 'N' || cur_scn_fld->modf == 'U' || cur_scn_fld->modf == 'B')
		{									/* It it modifiable?			*/
			len = 11;							/* default mod int to 11 chars in len.	*/
		}
		else len = 4;								/* default int to 4 chars in length.	*/

		totlen += len;								/* Add the current fields length.	*/
		if (fac_msk != 0 && fac_msk != FMCENTER) totlen += 1; 			/* Add one for FAC char.		*/
		cur_scn_fld = cur_scn_fld->next_item;
	}
	if ((fac_msk & FMCENTER) && totlen != 80)
	{
		ccol = (80 - totlen) / 2;						/* Set to start col if centered.	*/
	}
	else ccol = 2;									/* Load the display column.		*/
	cur_scn_fld = stitem;								/* Set back to beginning item on line.	*/
	while (cur_scn_fld && cur_scn_fld->row == crow)
	{
		cur_scn_fld->fac_msk = fac_msk;						/* Set the FAC mask for each item.	*/
		if (cur_scn_fld->modf == 'N' || cur_scn_fld->modf == 'U' || cur_scn_fld->modf == 'B')
		{									/* It it modifiable?			*/
			cur_scn_fld->fac_msk |= FMBRIGHT;
			if (cur_scn_fld->modf == 'N') cur_scn_fld->fac_msk |= FMNUMERIC;
			else if (cur_scn_fld->modf == 'U') cur_scn_fld->fac_msk |= FMUPPER;
			else if (cur_scn_fld->modf == 'B') cur_scn_fld->fac_msk |= FMUPLOW;
		}

		if (ccol < 1)								/* If the calculated col is < 1.	*/
		{
			write_log("PROCTRAN",'W',"NEGCOL","Generated a negative display column (%d).",ccol);
			ccol = 1;							/* Set to default to 1.			*/
		}
		cur_scn_fld->col = ccol;						/* Load the display column.		*/

		if (*cur_scn_fld->scn_fld_type == 'S') len = len_to_int(cur_scn_fld->len);
		else if (cur_scn_fld->modf == 'N' || cur_scn_fld->modf == 'U' || cur_scn_fld->modf == 'B')
		{									/* It it modifiable?			*/
			len = 11;							/* default mod int to 11 chars in len.	*/
		}
		else len = 4;								/* default int to 4 chars in length.	*/

		ccol += len;								/* Add the curent field length.		*/
		if (cur_scn_fld->fac_msk != 0 && cur_scn_fld->fac_msk != FMCENTER) ccol += 1; /* Add one for FAC char.	*/
		cur_scn_fld = cur_scn_fld->next_item;
	}		
	cur_scn_fld = hld_scn_fld;							/* Set pointr back.			*/
}
