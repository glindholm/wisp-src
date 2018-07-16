#define EXT extern
			/************************************************************************/
			/*	   PROCTRAN - Wang Procedure Language to VS COBOL Translator	*/
			/*			Copyright (c) 1990				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* PG_STRTSCN.C	*/

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"
#include "pgcblsrc.h"

do_start_screen()
{
	char scrn_num[FLDLEN], row_num[FLDLEN], col_num[FLDLEN];
	int i, current_col, prev_row, adj, ilen;

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
			write_log("PROCTRAN",'W',"NOSCNDEF","No screen definition, creating FILLER field.");
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
			if (*cur_scn_fld->len == '(')
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
			end_prt_line(0);						/* Add end of line stuff and write line.*/

			indent_line();
			if (*cur_scn_fld->scn_fld_type == 'S' && *cur_scn_fld->screen_fld != '&')
			{
				int cpos;

				strcat(cobline," VALUE \"");
				cpos = 19;
				if ((int)(cpos+strlen(cur_scn_fld->screen_fld)) >= 72)	/* If text goes past column 72		*/
				{							/* need to generate a continuation	*/
					char temp[55], *cstr;				/* line.				*/

					cstr = cur_scn_fld->screen_fld;
					strncpy(temp,cstr,53);
					temp[53] = '\0';
					strcat(cobline,temp);				/* Load part of the value of the field.	*/
					end_prt_line(0);				/* Add end of line stuff and write line.*/

					strcpy(cobline,"      -    \"");		/* Load continuation indicator and ".	*/
					cstr += 53;					/* Set ptr to start for next line.	*/
					strcat(cobline,cstr);
				}
				else strcat(cobline,cur_scn_fld->screen_fld);		/* Load the value of the field.		*/
				strcat(cobline,"\"");					/* Load close quote.			*/
				end_prt_line(1);					/* Add end of line stuff and write line.*/
			}
			else
			{
				strcat(cobline," SOURCE ");
				concat_var_prefix(cur_scn_fld->scn_fld_type[1]);
				if (*cur_scn_fld->screen_fld == '&') strcat(cobline,&cur_scn_fld->screen_fld[1]);
				else 	strcat(cobline,cur_scn_fld->screen_fld);	/* Load the value of the field.		*/
				strcat(cobline," OBJECT ");
				concat_var_prefix(cur_scn_fld->scn_fld_type[1]);
				if (*cur_scn_fld->screen_fld == '&') strcat(cobline,&cur_scn_fld->screen_fld[1]);
				else 	strcat(cobline,cur_scn_fld->screen_fld);	/* Load the value of the field.		*/
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
