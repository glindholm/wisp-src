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
#include "pgglobal.h"
#include "pgstruct.h"
#include "pgcblsrc.h"

void do_start_screen(void)
{
	char scrn_num[FLDLEN], row_num[FLDLEN], col_num[FLDLEN];
	int i, current_col, prev_row = 0, adj, ilen;
	char tmp_fld[FLDLEN], tmp_type[3];

	i = 0;
	while(*displ_def[i])								/* Write the Display Definitions source.*/
	{
		num_outlines++;								/* Increment the .WCB line count.	*/
		fputs(displ_def[i++],outfile); 
	}

	cur_scn = start_screen;								/* Set the original screen.		*/
	while (cur_scn)
	{
		sprintf(scrn_num,"%d",cur_scn->num_screen);				/* Load the screen number.		*/
		strcpy(cobline,"       01  DISPLAY-REC-");
		strcat(cobline,scrn_num);						/* Load the screen number.		*/
		strcat(cobline," USAGE IS DISPLAY-WS");
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		cur_scn_fld = cur_scn->variable_area;					/* Load the variable area.		*/
		if (cur_scn->num_rows != -1) adj = ((24 - cur_scn->num_rows) / 2);	/* Calculate the vertical adjustment	*/
		else	adj = 0;							/* else no vertical adjustment needed.	*/
		if (cur_scn_fld) prev_row = cur_scn_fld->row + adj;
		current_col = 2;
		if (!cur_scn_fld)							/* If no screen fields defined then	*/
		{									/* Generate a dummy one.		*/
			write_log("PROCTRAN",'W','W',"NOSCNDEF","No screen definition, creating FILLER field.");
			strcpy(cobline,"           05  FILLER        PIC X(10)    ROW 01 COLUMN 01");
			end_prt_line(0);						/* Add end of line stuff and write line.*/

			indent_line();
			strcat(cobline," VALUE \"          \"");
			end_prt_line(1);						/* Add end of line stuff and write line.*/
		}

		while (cur_scn_fld)							/* While have screen definition fields	*/
		{									/*  then process them.			*/
			sprintf(row_num,"%d",cur_scn_fld->row + adj);
			if (cur_scn_fld->row + adj == prev_row && cur_scn_fld->col == 2)
			{
				sprintf(col_num,"%d",current_col);
				current_col++;
			}
			else	sprintf(col_num,"%d",cur_scn_fld->col);

			strcpy(cobline,"           05  ROW");
			strcpy(cur_scn_fld->map_fld,"ROW");

			strcat(cobline,scrn_num);					/* Load the screen number.		*/
			strcat(cur_scn_fld->map_fld,scrn_num);

			strcat(cobline,row_num);					/* Load the row number.			*/
			strcat(cur_scn_fld->map_fld,row_num);				/* Load the row number.			*/

			strcat(cobline,"-COL");
			strcat(cobline,col_num);					/* Load the row number.			*/
			strcat(cur_scn_fld->map_fld,"-COL");
			strcat(cur_scn_fld->map_fld,col_num);				/* Load the colume number.		*/
			if (*cur_scn_fld->scn_fld_type == 'I') strcat(cobline," PIC 9(4"); /* If a displayed integer field.	*/
			else strcat(cobline," PIC X(");
			if (cur_scn_fld->sub_flag) strcat(cobline,cur_scn_fld->length);	/* Load the length.			*/
			else if (*cur_scn_fld->len == '(')
			{
				char *cstr, *tpos, tlen[FLDLEN];

				cstr = tlen;
				tpos = &cur_scn_fld->len[1];
				while (*tpos != ')') *cstr++ = *tpos++;			/* Copy length without ().		*/
				*cstr = '\0';						/* Null terminate the string.		*/
				strcat(cobline,tlen);					/* Load the length.			*/
			}
			else strcat(cobline,cur_scn_fld->len);				/* Load the length.			*/

			strcat(cobline,") ROW ");
			strcat(cobline,row_num);					/* Load the row number.			*/
			strcat(cobline," COLUMN ");
			strcat(cobline,col_num);					/* Load the col number.			*/
			ilen = len_to_int(cur_scn_fld->len);				/* Convert length str to int.		*/
			current_col += ilen;

			if (*cur_scn_fld->scn_fld_type == 'S' &&
				(*cur_scn_fld->screen_fld != '&' || cur_scn_fld->scn_fld_type[1] == 'S'))
			{
				int cpos, p_in_73;

				p_in_73 = FALSE;
				strcat(cobline," VALUE");
				end_prt_line(0);					/* Add end of line stuff and write line.*/

				indent_line();
				strcat(cobline,"\"");
				cpos = 12;

				if ((int)(cpos+strlen(cur_scn_fld->screen_fld)) == WPLMAX) /* IF . would be generated in col 73	*/
				{
					p_in_73 = TRUE;
					strcat(cobline,cur_scn_fld->screen_fld);	/* Load the value of the field.		*/
					strcat(cobline,"\"");
					end_prt_line(0);				/* Add end of line stuff and write line.*/
					indent_line();				
				}
				else if ((int)(cpos+strlen(cur_scn_fld->screen_fld)) > WPLMAX) /* If text goes past valid col.	*/
				{							/* need to generate a continuation	*/
					char temp[62], *cstr;				/* line.				*/
					int numc;

					numc = 60;
					cstr = cur_scn_fld->screen_fld;
					strncpy(temp,cstr,numc);
					temp[numc] = '\0';
					strcat(cobline,temp);				/* Load part of the value of the field.	*/
					end_prt_line(0);				/* Add end of line stuff and write line.*/

					strcpy(cobline,"      -    \"");		/* Load continuation indicator and ".	*/
					cstr += numc;					/* Set ptr to start for next line.	*/
					strcat(cobline,cstr);
				}
				else strcat(cobline,cur_scn_fld->screen_fld);		/* Load the value of the field.		*/

				if (!p_in_73) strcat(cobline,"\"");			/* Load close quote.			*/
				end_prt_line(1);					/* Add end of line stuff and write line.*/
			}
			else
			{
				end_prt_line(0);					/* Add end of line stuff and write line.*/

				indent_line();
				strcat(cobline," SOURCE ");
				if (cur_scn_fld->sub_flag)
				{
					strcpy(tmp_fld,cur_scn_fld->screen_fld);
					strcat(tmp_fld,"-");
					if (*cur_scn_fld->start_pos == '&') strcat(tmp_fld,&cur_scn_fld->start_pos[1]);
					else strcat(tmp_fld,cur_scn_fld->start_pos);
					strcat(tmp_fld,"-");
					strcat(tmp_fld,cur_scn_fld->length);
					strcpy(tmp_type,cur_scn_fld->scn_fld_type);

					concat_var_prefix(tmp_type[1]);
					if (*tmp_fld == '&')
					{
						strcat(cobline,&tmp_fld[1]);
					}
					else 	strcat(cobline,tmp_fld);		/* Load the value of the field.		*/
				}
				else
				{
					concat_var_prefix(cur_scn_fld->scn_fld_type[1]);
					if (*cur_scn_fld->screen_fld == '&')
					{
						strcat(cobline,&cur_scn_fld->screen_fld[1]);
					}
					else 	strcat(cobline,cur_scn_fld->screen_fld); /* Load the value of the field.	*/
				}

				if (cur_scn_fld->modf == 'U' || cur_scn_fld->modf == 'B' || cur_scn_fld->modf == 'N')
				{
					strcat(cobline," OBJECT ");
					if (cur_scn_fld->sub_flag)
					{
						concat_var_prefix(tmp_type[1]);
						if (*tmp_fld == '&')
						{
							strcat(cobline,&tmp_fld[1]);
						}
						else 	strcat(cobline,tmp_fld);		/* Load the value of the field.		*/
					}
					else
					{
						concat_var_prefix(cur_scn_fld->scn_fld_type[1]);
						if (*cur_scn_fld->screen_fld == '&')
						{
							strcat(cobline,&cur_scn_fld->screen_fld[1]);
						}
						else 	strcat(cobline,cur_scn_fld->screen_fld);
					}
				}
				end_prt_line(1);					/* Add end of line stuff and write line.*/
			}
			prev_row = cur_scn_fld->row + adj;				/* Set row of current item.		*/
			cur_scn_fld = cur_scn_fld->next_item;
		}
		num_outlines++;								/* Increment the .WCB line count.	*/
		fputs("      *\n",outfile); 						/* Write a blank line.			*/
		cur_scn = cur_scn->next_item;						/* Point to the next one.		*/
	}
}
/*
**	History:
**	$Log: ptscrdef.c,v $
**	Revision 1.7  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.6  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.5  1997/04/21 15:20:28  scass
**	Corrected copyright.
**	
**	Revision 1.4  1996-09-12 19:17:53-04  gsl
**	fix prototypes
**
**
**
*/
