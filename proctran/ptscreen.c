/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/


#define EXT extern

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"
#include "pgkeyw.h"

static void eval_column(int crow, int fac_msk);						/* Set the correct columns for current 	*/
static int clear_attributes(long *fac_msk);						/* Attributes apply only to their	*/
static void generate_screen_field(int *num_var, int *num_val, int *slen);		/* Generate a screen field item in	*/
static void concat_screen_field(int *slen);						/* Now add the current field to previous*/
static void adjust_for_fac(int fac_msk, int *len, int *adjust);				/* Adjust length for FAC attributes.	*/

void p_scn_kw(int tndx)									/* Process screen keywords.		*/
{
	setscreen();									/* Reset screen flags.			*/

	switch (tndx)
	{
		case CENTER: 
		{
			write_log(util,'I','R',"PROCESS","Processing CENTER screen keyword.");
			cur_fac_msk |= FMCENTER;					/* Got to center line.			*/
			break;								/* Leave this option.			*/
		}
		case BLANK: 
		{
			write_log(util,'I','R',"PROCESS","Processing BLANK screen keyword.");
			if (cur_fac_msk & FMDIM) cur_fac_msk &= ~FMDIM;			/* Conflicting attrib., turn off DIM.	*/
			if (cur_fac_msk & FMBRIGHT) cur_fac_msk &= ~FMBRIGHT;		/* Conflicting attrib., turn off BRIGHT.*/
			if (cur_fac_msk & FMBLINK) cur_fac_msk &= ~FMBLINK;		/* Conflicting attrib., turn off BLINK.	*/
			cur_fac_msk |= FMBLANK;						/* Got a lopro.				*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case BLINK: 
		{
			write_log(util,'I','R',"PROCESS","Processing BLINK screen keyword.");
			if (cur_fac_msk & FMBLANK) cur_fac_msk &= ~FMBLANK;		/* Conflicting attrib., turn off BLANK.	*/
			cur_fac_msk |= FMBLINK;						/* Set flash fac.			*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case BRIGHT: 
		{
			write_log(util,'I','R',"PROCESS","Processing BRIGHT screen keyword.");
			if (cur_fac_msk & FMDIM) cur_fac_msk &= ~FMDIM;			/* Conflicting attrib., turn off DIM.	*/
			if (cur_fac_msk & FMBLANK) cur_fac_msk &= ~FMBLANK;		/* Conflicting attrib., turn off BLANK.	*/
			cur_fac_msk |= FMBRIGHT;					/* Set hi fac.				*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case DIM: 
		{
			write_log(util,'I','R',"PROCESS","Processing DIM screen keyword.");
			if (cur_fac_msk & FMBRIGHT) cur_fac_msk &= ~FMBRIGHT;		/* Conflicting attrib., turn off BRIGHT.*/
			if (cur_fac_msk & FMBLANK) cur_fac_msk &= ~FMBLANK;		/* Conflicting attrib., turn off BLANK.	*/
			cur_fac_msk |= FMDIM;						/* set lofac.				*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case LINE: 
		{
			write_log(util,'I','R',"PROCESS","Processing LINE screen keyword.");
			cur_fac_msk |= FMLINE;						/* set undeline fac.			*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case NUMERIC: 
		{
			write_log(util,'I','R',"PROCESS","Processing NUMERIC screen keyword.");
			cur_mod_fac = 'N';						/* Set numeric fac.			*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case TAB: 
		{
			write_log(util,'I','R',"PROCESS","Processing TAB screen keyword.");
			if (cur_fac_msk & FMBLANK) cur_fac_msk &= ~FMBLANK;		/* Conflicting attrib., turn off BLANK.	*/
			if (cur_fac_msk & FMBLINK) cur_fac_msk &= ~FMBLINK;		/* Conflicting attrib., turn off BLINK.	*/
			if (cur_fac_msk & FMBRIGHT) cur_fac_msk &= ~FMBRIGHT;		/* Conflicting attrib., turn off BRIGHT.*/
			if (cur_fac_msk & FMDIM) cur_fac_msk &= ~FMDIM;			/* Conflicting attrib., turn off DIM.	*/
			if (cur_fac_msk & FMLINE) cur_fac_msk &= ~FMLINE;		/* Conflicting attrib., turn off LINE.	*/
			cur_mod_fac = ' ';						/* Set to no fac.			*/
			cur_fac_msk |= FMTAB;						/* Set in tab fac.			*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case UPLOW: 
		{
			write_log(util,'I','R',"PROCESS","Processing UPLOW screen keyword.");
			cur_mod_fac = 'B';						/* Set field upper and lower case.	*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case UPPER: 
		{
			write_log(util,'I','R',"PROCESS","Processing UPPER screen keyword.");
			cur_mod_fac = 'U';						/* Set only upper case.			*/
			set_string = FALSE;
			break;								/* Leave this option.			*/
		}
		case ROW: 
		{
			write_log(util,'I','R',"PROCESS","Processing ROW screen keyword.");
			in_row = 1;							/* Found row keyword.			*/
			cur_scn->num_rows = -1;						/* Set so doesn't center vertically.	*/
			nexttok();
			break;								/* Leave this option.			*/
		}
		case ERASE: 
		{
			write_log(util,'I','R',"PROCESS","Processing ERASE screen keyword.");
			in_erase = 1;							/* Got an erase command.		*/
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step past the keyword.		*/
			if (*aptr != '=') nexttok();
			break;								/* Leave this option.			*/
		}
		case ALARM: 
		{
			write_log(util,'I','R',"PROCESS","Processing ALARM screen keyword.");
			in_alarm = 1;							/* Set the alarm command.		*/
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step past the keyword.		*/
			if (*aptr != '=') nexttok();
			break;								/* Leave this option.			*/
		}
		case PFKEY:
		{
			write_log(util,'I','R',"PROCESS","Processing PFKEY screen keyword.");
			in_pfkey = 1;
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step to next token on line.		*/
			if (*aptr == ' ') nexttok();					/* If the equation is spaces out.	*/
			break;								/* Leave PF loop.			*/
		}
		case CURROW:
		{
			write_log(util,'I','R',"PROCESS","Processing CURROW screen keyword.");
			in_currow = 1;
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step to next token on line.		*/
			if (*aptr == ' ') nexttok();					/* If the equation is spaces out.	*/
			break;								/* Leave PF loop.			*/
		}
		case CURCOL:
		{
			write_log(util,'I','R',"PROCESS","Processing CURCOL screen keyword.");
			in_curcol = 1;
			while (tststrt(aptr) && *aptr != '=') aptr++;			/* Step to next token on line.		*/
			if (*aptr == ' ') nexttok();					/* If the equation is spaces out.	*/
			break;								/* Leave PF loop.			*/
		}
		default:				
		{
			write_log(util,'E','R',"INVALID","Invalid SCREEN keyword.");
			break;
		}
	}
}

void save_screen_item(int* num_asn,int* num_var,int* num_val,int* current_row)		/* Save the screen expression.		*/
{
	char *cstr;
	int crfm, ndx, semi_beg;							/* Current row FAC mask.		*/
	int slen;

	write_log(util,'I','R',"PROCESS","Processing a SCREEN variable.");
	crfm = cur_fac_msk;								/* Save for column assignment.		*/
	if (*aptr == ';') semi_beg = TRUE;
	else semi_beg = FALSE;								/* Assume ; are not at beginning.	*/
	slen = 0;

	while (*aptr == '&' || *aptr == '\'' || *aptr == ';' || *aptr == ',')	 	/* Loop while find info.		*/
	{
		while (*aptr == ';')							/* If ; so generate blank line.		*/
		{
			(*current_row)++;
			if (cur_scn->num_rows != -1) cur_scn->num_rows++;		/* Keep track of # displayable rows.	*/
			aptr++;								/* Step past the ;			*/
			cur_fac_msk = 0;						/* Clear the FACs for this line.	*/
			set_string = FALSE;
			cnt_sf = 0;
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
			if (*aptr == '\0' || *aptr == LNFD)				/* Is it a string field on two lines?	*/
			{
				int num;
				char tmp_str[80];

				num = aptr - linein;					/* Concatenate spaces to end of line.	*/
				num = WPLMAX - num;
				memset(tmp_str,' ',num);
				tmp_str[num] = '\0';
				if (*aptr == LNFD) *aptr = '\0';			/* Set to NULL so strcat works.		*/
				strcat(aptr,tmp_str);
			}
			if (*aptr != '\0' && *aptr != LNFD && *aptr != ';' && *aptr != ',')
			{
				if (set_string) generate_screen_field(num_var, num_val,&slen);
				init_screen_field(current_row);				/* Allocate field area for screen.	*/
				set_value("1",num_asn,*current_row);			/* Get value for picture.		*/
				cur_scn_fld->fac_msk = cur_fac_msk;			/* Set the FAC mask for item.		*/
				if (set_string) concat_screen_field(&slen);		/* Now add current field to previous.	*/
                                set_string = clear_attributes(&cur_fac_msk);
			}
			next_ptr = aptr;						/* Set so nexttok() gets past string.	*/
			if (*aptr == ' ') nexttok();
			if (*aptr == '\0' || *aptr == LNFD) cur_fac_msk = 0;		/* Clear the facs for this field.	*/
		}

		if (*aptr == '&')
		{
			if (set_string) generate_screen_field(num_var, num_val,&slen);
			init_screen_field(current_row);					/* Allocate field area for screen.	*/
			cstr = cur_scn_fld->screen_fld;					/* Set ptr to field.			*/

			while (tststrt(aptr) && *aptr != ';' && *aptr != '(')
			{
				if (*aptr == '_') *aptr = '-';				/* Replace the underscore with dash.	*/
				else if (*aptr == '#') *aptr = '-';			/* Replace the # with dash.		*/
				*cstr++ = toupper(*aptr++); 				/* Copy var name to screen field.	*/
			}
			*cstr = '\0';							/* Null terminate the string.		*/
			get_type_len(cur_scn_fld->screen_fld,cur_scn_fld->scn_fld_type,num_var,num_val,'T');
			cur_scn_fld->fac_msk = cur_fac_msk;				/* Set the FAC mask for item.		*/
			cstr = aptr;							/* Save current position.		*/
			if (*cstr == ' ')
			{
				while (*cstr == ' ') cstr++;				/* Step over spaces.			*/
				if (*cstr == '(') aptr = cstr;				/* If subscript set pointer.		*/
			}
			if (*aptr == '(')						/* Step past subscript stuff.		*/
			{
				cstr = aptr;						/* Set the current ptr.			*/
				cstr++;							/* Step past open parenthesis.		*/
				if (*cstr == '1' && *(cstr+2) == '*')			/* If using the full value of var.	*/
				{
					cur_scn_fld->dlmtr = '*';			/* Set so uses blank delimeter.		*/ 
					aptr = cstr+4;					/* Step past (1,*)			*/
					next_ptr = aptr;
				}
				else
				{
					char newvar[FLDLEN];

					cur_scn_fld->sub_flag = 1;
					aptr++;						/* Step past open parenthesis.		*/
					got_string = 1;					/* Set, will generate a CALL "STRING".	*/
					cstr = cur_scn_fld->start_pos;	
					while (*aptr != ',') *cstr++ = toupper(*aptr++); /* Copy in the starting position.	*/
					*cstr = '\0';					/* Null terminate the string.		*/
					if (*cur_scn_fld->start_pos == '&')
					{						 /* Get type of variable.		*/
						get_type_len(cur_scn_fld->start_pos,cur_scn_fld->stpos_type,num_var,num_val,'T');
						cur_scn_fld->sub_flag = 2;		/* Set so know is a variable.		*/
					}
					aptr++;						/* Step over the comma.			*/
					cstr = cur_scn_fld->length;
					while (*aptr != ')') *cstr++ = *aptr++;		/* Copy in the length.			*/
					*cstr = '\0';					/* Null terminate the string.		*/
					if (*cur_scn_fld->length == '&')
					{						 /* Get type of variable.		*/
						get_type_len(cur_scn_fld->length,cur_scn_fld->len_type,num_var,num_val,'T');
					}

					strcpy(newvar,cur_scn_fld->screen_fld);
					strcat(newvar,"-");
					if (*cur_scn_fld->start_pos == '&') strcat(newvar,&cur_scn_fld->start_pos[1]);
					else strcat(newvar,cur_scn_fld->start_pos);
					strcat(newvar,"-");
					strcat(newvar,cur_scn_fld->length);
					get_type_len(newvar,cur_scn_fld->scn_fld_type,num_var,num_val,'C');

					aptr++;						/* Step over the closing paren.		*/
					next_ptr = aptr;
				}
			}
			cur_scn_fld->row = *current_row;				/* Load the current row.		*/
			cur_scn_fld->modf = cur_mod_fac;				/* Set the mod fac type.		*/

			if (*cur_scn_fld->scn_fld_type == 'S')
			{								/* Get the length of variable.		*/
				get_type_len(cur_scn_fld->screen_fld,cur_scn_fld->len,num_var,num_val,'S');
			}
			if (set_string) concat_screen_field(&slen);			/* Now add current field to previous.	*/
                        set_string = clear_attributes(&cur_fac_msk);
			cur_mod_fac = ' ';						/* Set back to no FAC.			*/
			next_ptr = aptr;						/* Set so nexttok() gets past string.	*/
			if (*aptr == ' ') nexttok();
			if (*aptr == '\0' || *aptr == LNFD) cur_fac_msk = 0;		/* Clear the facs for this field.	*/
		}
		if (*aptr == ',')
		{
			aptr++;								/* Step over the coma separator.	*/
			if (*aptr == ' ') nexttok();
			next_ptr = aptr;						/* Set so nexttok() gets past string.	*/
		}
		while ((ndx = find_keyword(aptr,screen_keywords)) >= 0)			/* Search for screen keywords.		*/
		{									/* Found a match so process.		*/
			p_scn_kw(ndx);
			aptr += strlen(screen_keywords[ndx]);
			if (*aptr == ' ') nexttok();					/* Step past the screen keyword.	*/
			semi_beg = FALSE;						/* Set back because processing line.	*/
			crfm = cur_fac_msk;						/* Save for column assignment.		*/
		}
	}

	if (!semi_beg && cur_scn_fld) eval_column(cur_scn_fld->row,crfm);		/* Set the column positions for row.	*/
	
	next_ptr = aptr;								/* Set ptr to current position.		*/
}

static void eval_column(int crow, int fac_msk)						/* Set the correct columns for current 	*/
											/* row of screen display.		*/
{
	scn_fld_item *stitem, *hld_scn_fld;
	int totlen, ccol, len, adj_att;

	hld_scn_fld = cur_scn_fld;							/* store previous if item.		*/
	totlen = 0;									/* Set to 0 length.			*/
	adj_att = 0;									/* Set flag to no adjustments.		*/
	cur_scn_fld = cur_scn->variable_area;						/* Load the variable area.		*/
	while (cur_scn_fld && cur_scn_fld->row != crow)
	{
		cur_scn_fld = cur_scn_fld->next_item;					/* Step to the current row to process.	*/
	}
	stitem = cur_scn_fld;								/* Save the starting item for later.	*/
	while (cur_scn_fld && cur_scn_fld->row == crow)					/* Get total length of fields on line.	*/
	{
		if (*cur_scn_fld->scn_fld_type == 'S' && cur_scn_fld->sub_flag) len = len_to_int(cur_scn_fld->length);
		else if (*cur_scn_fld->scn_fld_type == 'S') len = len_to_int(cur_scn_fld->len);
		else if (cur_scn_fld->modf == 'N' || cur_scn_fld->modf == 'U' || cur_scn_fld->modf == 'B')
		{									/* It it modifiable?			*/
			len = 11;							/* default mod int to 11 chars in len.	*/
		}
		else len = 4;								/* default int to 4 chars in length.	*/

		totlen += len;								/* Add the current fields length.	*/
		adjust_for_fac(cur_scn_fld->fac_msk,&totlen,&adj_att);			/* Adjust length for FAC attributes.	*/
		cur_scn_fld = cur_scn_fld->next_item;
	}
	if ((fac_msk & FMCENTER) && totlen != 80)
	{
		if (totlen > 77 && totlen < 80)
		{
			if (totlen == 78) ccol = 2;
			if (totlen == 79) ccol = 1;
		}
		else ccol = (79 - totlen) / 2;						/* Set to start col if centered.	*/
	}
	else ccol = 2;									/* Load the display column.		*/
	cur_scn_fld = stitem;								/* Set back to beginning item on line.	*/
	while (cur_scn_fld && cur_scn_fld->row == crow)
	{
		if (cur_scn_fld->modf == 'N' || cur_scn_fld->modf == 'U' || cur_scn_fld->modf == 'B')
		{									/* It it modifiable?			*/
			cur_scn_fld->fac_msk |= FMBRIGHT;
			if (cur_scn_fld->modf == 'N') cur_scn_fld->fac_msk |= FMNUMERIC;
			else if (cur_scn_fld->modf == 'U') cur_scn_fld->fac_msk |= FMUPPER;
			else if (cur_scn_fld->modf == 'B') cur_scn_fld->fac_msk |= FMUPLOW;
		}

		if (ccol < 1)								/* If the calculated col is < 1.	*/
		{
			write_log(util,'W','R',"NEGCOL","Generated a negative display column (%d).",ccol);
			ccol = 1;							/* Set to default to 1.			*/
		}
		adjust_for_fac(cur_scn_fld->fac_msk,&ccol,&adj_att);			/* Adjust length for FAC attributes.	*/
		cur_scn_fld->col = ccol;						/* Load the display column.		*/

		if (*cur_scn_fld->scn_fld_type == 'S' && cur_scn_fld->sub_flag) len = len_to_int(cur_scn_fld->length);
		else if (*cur_scn_fld->scn_fld_type == 'S') len = len_to_int(cur_scn_fld->len);
		else if (cur_scn_fld->modf == 'N' || cur_scn_fld->modf == 'U' || cur_scn_fld->modf == 'B')
		{									/* It it modifiable?			*/
			len = 11;							/* default mod int to 11 chars in len.	*/
		}
		else len = 4;								/* default int to 4 chars in length.	*/

		ccol += len;								/* Add the curent field length.		*/
		cur_scn_fld = cur_scn_fld->next_item;
	}		
	cur_scn_fld = hld_scn_fld;							/* Set pointr back.			*/
}

static int clear_attributes(long *fac_msk)						/* Attributes apply only to their	*/
											/*  associated field so clear for next	*/
{											/*  item.				*/
	long l_fac_msk;
	int  no_attr;

	l_fac_msk = *fac_msk;								/* Set the local copy of field.		*/
	no_attr = TRUE;

	if (l_fac_msk & FMBLANK)
	{
		l_fac_msk &= ~FMBLANK;							/* If have BLANK then turn off.		*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMBLINK)
	{
		l_fac_msk &= ~FMBLINK;							/* If have BLINK then turn off.		*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMBRIGHT)
	{
		l_fac_msk &= ~FMBRIGHT;							/* If have BRIGHT then turn off.	*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMDIM)
	{
		l_fac_msk &= ~FMDIM;							/* If have DIM then turn off.		*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMLINE)
	{
		l_fac_msk &= ~FMLINE;							/* If have LINE then turn off.		*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMNUMERIC)
	{
		l_fac_msk &= ~FMNUMERIC;						/* If have NUMERIC then turn off.	*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMTAB)
	{
		l_fac_msk &= ~FMTAB;							/* If have TAB then turn off.		*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMUPLOW)
	{
		l_fac_msk &= ~FMUPLOW;							/* If have UPLOW then turn off.		*/
		no_attr = FALSE;
	}
	if (l_fac_msk & FMUPPER)
	{
		l_fac_msk &= ~FMUPPER;							/* If have UPPER then turn off.		*/
		no_attr = FALSE;
	}

	if (cur_mod_fac == 'N' || cur_mod_fac == 'B' || cur_mod_fac == 'U') no_attr = FALSE;

	*fac_msk = l_fac_msk;								/* Set back to passed in field.		*/

	return(no_attr);
}

static void adjust_for_fac(int fac_msk, int *len, int *adjust)				/* Adjust length for FAC attributes.	*/
{											/* Add one for FAC char.		*/

	if ( (fac_msk & FMBLANK) || (fac_msk & FMBLINK) || (fac_msk & FMBRIGHT) || (fac_msk & FMDIM) ||
	     (fac_msk & FMNUMERIC) || (fac_msk & FMTAB) || (fac_msk & FMUPLOW) || (fac_msk & FMUPPER) ||
	     (cur_scn_fld->modf == 'N') || (cur_scn_fld->modf == 'B') || (cur_scn_fld->modf == 'U') )
	{
		*len += 1;
		*adjust = 1;
	}
	else
	{
		if (*adjust) *len += 1;

		*adjust = 0;
	}
}

static void generate_screen_field(int *num_var, int *num_val, int *slen)		/* Generate a screen field item in	*/
											/*  declare list and use in screen.	*/
{
	if ( cur_scn_fld->row != num_sf_var ||
	     (cur_scn_fld->row == num_sf_var && cnt_sf &&
	      strncmp(cur_scn_fld->screen_fld,"&SCN",4)) )
	{
		init_current(num_var);							/* Alloc next node for declare.		*/
		(*num_val)++;								/* Keep track of init stmnt role back.	*/
		if (cnt_sf)
		{
			sprintf(cur_decl->field1,"SCN%d-FLD%d-%d",
				cur_scn->num_screen,cur_scn_fld->row,cnt_sf);
		}
		else
		{
			sprintf(cur_decl->field1,"SCN%d-FLD%d",
				cur_scn->num_screen,cur_scn_fld->row);
		}
		cnt_sf++;
		strcpy(cur_decl->value,cur_scn_fld->screen_fld);
		strcpy(cur_decl->type,"S ");
		*slen = atoi(cur_scn_fld->len);
		num_sf_var = cur_scn_fld->row;						/* Save for next string processed.	*/
	}
	else *slen = atoi(cur_scn_fld->len);						/* Set the current length.		*/
}

static void concat_screen_field(int *slen)						/* Now add the current field to previous*/
											/* field parameters.			*/
{
	struct scn_fld_item *hld_scn_fld;

	if (cur_scn_fld->prev_item->str_params || *cur_scn_fld->screen_fld == '&'	/* If var and no attributes set and	*/
						|| *cur_decl->value == '&')		/*  have another string before then	*/
	{										/*  need to use STRING func.		*/
		hld_scn_fld = cur_scn_fld;
		cur_scn_fld = cur_scn_fld->prev_item;					/* Point to previous field.		*/
		init_string_param();
		if (cur_scn_fld->str_params == cur_str_param)				/* If is the first field to STRING.	*/
		{
			strcpy(cur_str_param->field1,cur_decl->value);
			*cur_decl->value = '\0';
			init_string_param();
		}

		strcpy(cur_str_param->field1,hld_scn_fld->screen_fld);			/* Now add the current variable.	*/
		cur_scn_fld = hld_scn_fld;						/* Set ptr back.			*/
	}

	*slen += len_to_int(cur_scn_fld->len);
	sprintf(cur_decl->length,"(%d)",*slen);
	if (!cur_scn_fld->prev_item->str_params) strcat(cur_decl->value,cur_scn_fld->screen_fld);
	cur_scn_fld = cur_scn_fld->prev_item;
	if (strncmp(cur_scn_fld->screen_fld,"&SCN",4)) sprintf(cur_scn_fld->screen_fld,"&%s",cur_decl->field1);
	sprintf(cur_scn_fld->len,"%d",*slen);
	cur_scn_fld->next_item = '\0';
}
/*
**	History:
**	$Log: ptscreen.c,v $
**	Revision 1.7  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.6  1997/04/21 15:21:31  scass
**	Corrected copyright.
**	
**	Revision 1.5  1996-09-12 19:18:01-04  gsl
**	Fix prototypes
**
**
**
*/
