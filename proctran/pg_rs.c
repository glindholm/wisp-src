#define EXT extern
			/************************************************************************/
			/*	PROCTRAN - Wang Procedure Language to VS COBOL Translator	*/
			/*			Copyright (c) 1990				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* PG_RS.C	*/

#include <stdio.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"

p_rs_kw(tndx,num_var,num_val)
int tndx, *num_var, *num_val;
{
	char *cstr;
	int full_br;

	write_log("PROCTRAN",'I',"PROCESS","Processing the RENAME or SCRATCH keyword.");
	switch (tndx)
	{
		case RSON:								/* Get volume name.			*/
		{
			nexttok();							/* Sets ptr to volume name.		*/
			if (tststrt(aptr))
			{
				cstr = cur_ren_sctch->vol;
				if (*aptr == '(')					/* Is it backwards referenced?		*/
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
				else	while (tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Load the volume name.		*/

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
			else	write_log("PROCTRAN",'E',"NOVOL","Volume name not supplied for rename or scratch keyword.");
			break;
		}
		case RSIN:								/* Get the library name.		*/
		{
			nexttok();							/* Sets ptr to library name.		*/
			if (tststrt(aptr))
			{
				if (!in_library) cstr = cur_ren_sctch->lib;
				else	cstr = cur_ren_sctch->to_lib;

				if (!in_library && *aptr == '(')			/* Is it backwards referenced?		*/
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
				else 	while (tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Load the library name.		*/

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
			else	write_log("PROCTRAN",'E',"NOLIB","Library name not supplied for rename or scratch keyword.");
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
			else	write_log("PROCTRAN",'E',"NOLIB","Library name not supplied for rename or scratch keyword.");
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
				if (*aptr == '(')					/* Is it backwards referenced?		*/
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
				else	while (tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Load the library name.		*/

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
			else	write_log("PROCTRAN",'E',"NOLIB","Library name not supplied for rename or scratch keyword.");
			break;
		}
		default:
		{
			if (*cur_ren_sctch->name == '\0')
			{
				cstr = cur_ren_sctch->name;
				if (*aptr == '(')					/* Is it backwards referenced?		*/
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
					while (tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Load the name.			*/
					*cstr = '\0';						/* NUll terminate the string.		*/
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
			}
			break;
		}
	}
}
