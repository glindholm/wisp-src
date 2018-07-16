static char copyright[]="Copyright (c) 1988-1997 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";
#define EXT extern

/* PG_RS.C	*/

#include <stdio.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"

void p_rs_kw(int  tndx,
	     int* num_var,
	     int* num_val,
	     int* filler_num,
	     int* current_row,
	     int* num_cmds)
{
	char *cstr;
	int full_br;

	write_log("PROCTRAN",'I','R',"PROCESS","Processing the RENAME or SCRATCH keyword.");
	switch (tndx)
	{
		case RSON:								/* Get volume name.			*/
		{
			nexttok();							/* Sets ptr to volume name.		*/
			if (tststrt(aptr))
			{
				cstr = cur_ren_sctch->vol;
				if ( is_backref() )					/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					cur_ren_sctch->br_flag = 1;			/* Set to indicate using backwards ref.	*/
					cur_ren_sctch->vol_br = 1;			/* Set to indicate field uses back ref.	*/
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
					while (tststrt(aptr) && *aptr != ')') *cstr++ = toupper(*aptr++); /* Load volume name.	*/
					if (*aptr == ')' ) aptr++;			/* Step over the closing paren.		*/
				}
				*cstr = '\0';						/* NUll terminate the string.		*/
				if (*cur_ren_sctch->vol == '&')
				{
					get_type_len(cur_ren_sctch->vol,cur_ren_sctch->vol_type,num_var,num_val,'T');
				}
				else if (cur_ren_sctch->vol_br)
				{
					get_type_len(cur_ren_sctch->vol,cur_ren_sctch->vol_type,num_var,num_val,'A');
				}
				else strcpy(cur_ren_sctch->vol_type,"S ");
			}
			else	write_log("PROCTRAN",'E','R',"NOVOL","Volume name not supplied for rename or scratch keyword.");
			break;
		}
		case RSIN:								/* Get the library name.		*/
		{
			nexttok();							/* Sets ptr to library name.		*/
			if (tststrt(aptr))
			{
				if (!in_library) cstr = cur_ren_sctch->lib;
				else	cstr = cur_ren_sctch->to_lib;

				if (!in_library && is_backref() )			/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					cur_ren_sctch->br_flag = 1;			/* Set to indicate using backwards ref.	*/
					cur_ren_sctch->lib_br = 1;			/* Set to indicate field uses back ref.	*/
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
					if (*aptr == '(' ) aptr++;			/* Step over the open parenthesis.	*/
				 	while (tststrt(aptr) && *aptr != ')') *cstr++ = toupper(*aptr++); /* Load library name.	*/
					if (*aptr == ')' ) aptr++;			/* Step over the closing paren.		*/
				}
				*cstr = '\0';						/* Null terminate the string.		*/
				if (*cur_ren_sctch->lib == '&')
				{
					get_type_len(cur_ren_sctch->lib,cur_ren_sctch->lib_type,num_var,num_val,'T');
				}
				else if (cur_ren_sctch->lib_br)
				{
					get_type_len(cur_ren_sctch->lib,cur_ren_sctch->lib_type,num_var,num_val,'A');
				}
				else strcpy(cur_ren_sctch->lib_type,"S ");
			}
			else	write_log("PROCTRAN",'E','R',"NOLIB","Library name not supplied for rename or scratch keyword.");
			break;								/* Get out.				*/
		}
		case RSTO:								/* Get the TO criteria.			*/
		{
			in_library = 1;
			nexttok();							/* Sets ptr to library name.		*/
			if (tststrt(aptr))
			{
				if (cur_ren_sctch->opt == 'L') cstr = cur_ren_sctch->to_lib;
				else	cstr = cur_ren_sctch->to_file;

				while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Load the lib/file name.		*/
				*cstr = '\0';						/* NUll terminate the string.		*/
				if (*cur_ren_sctch->to_lib == '&')
				{
					get_type_len(cur_ren_sctch->to_lib,cur_ren_sctch->to_lib_type,num_var,num_val,'T');
				}
				else strcpy(cur_ren_sctch->to_lib_type,"S ");
			}
			else	write_log("PROCTRAN",'E','R',"NOLIB","Library name not supplied for rename or scratch keyword.");
			break;
		}
		case RSLIB:								/* Just in case abriviated.		*/
		case RSLIBRARY:								/* Get the library criteria.		*/
		{
			cur_ren_sctch->opt = 'L';					/* Set library options.			*/
			nexttok();							/* Sets ptr to library name.		*/
			if (tststrt(aptr))
			{
				cstr = cur_ren_sctch->lib;
				if ( is_backref() )					/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					cur_ren_sctch->br_flag = 1;			/* Set to indicate using backwards ref.	*/
					cur_ren_sctch->lib_br = 1;			/* Set to indicate field uses back ref.	*/
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
					if (*aptr == '(' ) aptr++;			/* Step over the open parenthesis.	*/
					while (tststrt(aptr) && *aptr != ')') *cstr++ = toupper(*aptr++); /* Load library name.	*/
					if (*aptr == ')' ) aptr++;			/* Step over the closing paren.		*/
				}
				*cstr = '\0';						/* NUll terminate the string.		*/
				if (*cur_ren_sctch->lib == '&')
				{
					get_type_len(cur_ren_sctch->lib,cur_ren_sctch->lib_type,num_var,num_val,'T');
				}
				else if (cur_ren_sctch->lib_br)
				{
					get_type_len(cur_ren_sctch->lib,cur_ren_sctch->lib_type,num_var,num_val,'A');
				}
				else strcpy(cur_ren_sctch->lib_type,"S ");
			}
			else	write_log("PROCTRAN",'E','R',"NOLIB","Library name not supplied for rename or scratch keyword.");
			break;
		}
		default:
		{
			char *l_aptr, ptyp;

			if (*cur_ren_sctch->name == '\0')
			{
				cstr = cur_ren_sctch->name;
				if ( is_backref() )					/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					full_br = TRUE;					/* Set so uses full back. ref.		*/
					cur_ren_sctch->br_flag = 1;			/* Set to indicate using backwards ref.	*/
					cur_ren_sctch->name_br = 1;			/* Set to indicate field uses back ref.	*/
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
					*cstr = '\0';					/* NUll terminate the string.		*/
					if (full_br)					/* Set the library and volume params.	*/
					{
						char tlbl[FLDLEN];

						strcpy(tlbl,cur_ren_sctch->name);	/* Get the putparm label.		*/
						strcat(cur_ren_sctch->name,"-FILE");	/* Set the file field.			*/

						cur_ren_sctch->lib_br= 1;		/* Set to indicate field uses back ref.	*/
						strcpy(cur_ren_sctch->lib,tlbl);
						strcat(cur_ren_sctch->lib,"-LIBRARY");	/* Set the library field.		*/
						get_type_len(cur_ren_sctch->lib,cur_ren_sctch->lib_type,num_var,num_val,'A');

						cur_ren_sctch->vol_br= 1;		/* Set to indicate field uses back ref.	*/
						strcpy(cur_ren_sctch->vol,tlbl);
						strcat(cur_ren_sctch->vol,"-VOLUME");	/* Set the volume field.		*/
						get_type_len(cur_ren_sctch->vol,cur_ren_sctch->vol_type,num_var,num_val,'A');
					}
				}
				else
				{
					if (*aptr == '(' ) aptr++;			/* Step over the open parenthesis.	*/
					while (tststrt(aptr) && *aptr != ')') *cstr++ = toupper(*aptr++); /* Load the name.	*/
					*cstr = '\0';					/* NUll terminate the string.		*/
					if (*aptr == ')' ) aptr++;			/* Step over the closing paren.		*/
				}
				if (*cur_ren_sctch->name == '&')
				{
					get_type_len(cur_ren_sctch->name,cur_ren_sctch->name_type,num_var,num_val,'T');
				}
				else if (cur_ren_sctch->name_br)
				{
					get_type_len(cur_ren_sctch->name,cur_ren_sctch->name_type,num_var,num_val,'A');
				}
				else strcpy(cur_ren_sctch->name_type,"S ");

				l_aptr = aptr;
				while (*l_aptr == ' ') l_aptr++;			/* Step to next token.			*/ 
				if (*cur_ren_sctch->name == '&' && *l_aptr == '(')	/* Have substring so notify.		*/
				{ 							/* Gen working storage substring.	*/
					aptr = l_aptr;
					next_ptr = aptr;
					ptyp = *cur_ren_sctch->name_type;		/* Set type to S or I.			*/
					process_sub_var(ptyp,num_var,num_val,4,0,filler_num,current_row,num_cmds);
				}
			}
			break;
		}
	}
}
/*
**	History:
**	$Log: ptrens.c,v $
**	Revision 1.5  1997/04/21 15:18:57  scass
**	Corrected copyright.
**	
**	Revision 1.4  1996-09-12 19:17:44-04  gsl
**	Fix prototypes
**
**
**
*/
