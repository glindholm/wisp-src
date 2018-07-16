			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern
#include "wisp.h"
#include "crt.h"

#include <ctype.h>

/*						Process REWRITE CRT-FILE statements						*/
#define PROC_ALARM	0
#define	PROC_SETTING	6
#define	PROC_CURSOR	14
#define	PROC_COLUMN	21
#define	PROC_ROW	28
#define	PROC_LINE	32
#define PROC_ROLL	37
#define	PROC_ERASE	42
#define PROC_FROM	48
#define PROC_AFTER	53

crt_rewrite(crt_num)
int crt_num;										/* the crt-file number being rewritten	*/
{
	int kword,j;
	int wcc_byte;
	char from_item[40],row_field[40],col_field[40];

	row_field[0] = 0;
	col_field[0] = 0;
	kword = 0;
	wcc_byte = 0;
	from_item[0] = '\0';

	write_log("WISP",'I',"REWRITECRT","Rewrite of a crt file record.");
	if (ptype != -1)								/* could have parameters		*/
	{
		do									/* scan for them...			*/
		{
			if (ptype != -1)						/* not the end yet			*/
			{
				ptype = get_param(templine);				/* get a new parm			*/

	 	                        /*  0123456789012345678901234567890123456789012345678901234567890			*/
				kword = strpos("ALARM SETTING CURSOR COLUMN ROW LINE ROLL ERASE FROM AFTER",templine);

				if (kword != -1) stredt(inline,templine,"");		/* Remove it from the input.		*/

				switch (kword)						/* do the appropriate thing		*/
				{
					case PROC_ALARM:				/* process ALARM			*/
					{
						wcc_byte |= 0x0C0;			/* Set ALARM bit to WCC.		*/
						write_log("WISP",'I',"ALARM","ALARM found.");
						break;
					}

					case PROC_AFTER:
					{
						break;
					}

					case PROC_SETTING:
					{
						break;
					}

					case PROC_CURSOR:
					{
						wcc_byte |= 0x0A0;			/* Set CURSOR-POS bit.			*/
						break;
					}

					case PROC_COLUMN:				/* Process "COLUMN"			*/
					{
						ptype = get_param(col_field);		/* Actually read in the COLUMN value	*/
						stredt(inline,col_field,"");		/* Remove it.				*/
						stredt(col_field,".","");
						write_log("WISP",'I',"COLUMN","COLUMN found.");
						break;
					}

					case PROC_FROM:					/* Process "FROM"			*/
					{
						ptype = get_param(from_item);		/* Actually read in the FROM item	*/
						stredt(inline,templine,"");		/* Remove it.				*/

						write_log("WISP",'I',"FROMITM","FROM item found, %s.",from_item);
						break;
					}

					case PROC_ROW:					/* Process "ROW"			*/
					case PROC_LINE:					/* or "LINE"				*/
					{
						ptype = get_param(row_field);		/* Actually read in a ROW value		*/
						stredt(inline,row_field,"");		/* Remove it.				*/
						stredt(row_field,".","");		/* Fixup possible period.		*/
						write_log("WISP",'I',"ROW","ROW found.");
						break;
					}

					case PROC_ROLL:					/* Process "ROLL"			*/
					{
						ptype = get_param(templine);		/* Actually read in the UP/DOWN value	*/
						stredt(inline,templine,"");		/* Remove it.				*/
						write_log("WISP",'I',"ROLL","ROLL found.");
						break;
					}

					case PROC_ERASE:				/* Process "ERASE"			*/
					{
						ptype = get_param(templine);		/* Actually read in PROTECT/MODIFY	*/
						stredt(inline,templine,"");		/* Remove it.				*/
						write_log("WISP",'I',"ERASE","ERASE found.");
						break;
					}
				}
			}								/* end of if ptype == -1		*/
		} while ((kword != -1) && (ptype != -1));				/* quit when no matches			*/
	}


	write_log("WISP",'I',"CRTREWRITE","REWRITE of %s repaired.",crt_record[crt_num]); 	/* now fix it			*/

											/* Go till we go beyond it.		*/
	for ( cur_crt=0; cur_crt<crt_fcount; cur_crt++) 
	{
		if (crt_prime_rec[cur_crt] > crt_num) 
		{
			cur_crt--; 
			break;
		}
	}

	if (cur_crt == crt_fcount) cur_crt--;

	if (from_item[0])								/* Handle the REWRITE FROM phrase.	*/
	{
		tput_line_at(12, "MOVE %s TO",from_item);
		tput_clause (16, "%s,",crt_record[crt_num]);
	}

	if (crt_relative[cur_crt][0])
	{										/* write what line to start at		*/
		tput_line_at(12, "CALL \"xx2byte\" USING %s,",crt_relative[cur_crt]);
		tput_clause (16, "%s",crt_record[crt_num]);
	}

	tput_line_at(12, "MOVE %s TO WISP-CRT-RECORD,",crt_record[crt_num]);

	if (wcc_byte)									/* Do a special WCC function?		*/
	{
		tput_line_at(12, "MOVE DEC-BYTE-%d TO WISP-CRT-ORDER-AREA-2,",wcc_byte);
	}

	if (row_field[0])								/* Set the row byte			*/
	{
		if (isdigit(row_field[0]))						/* If it's numeric, move the DEC-BYTE.	*/
		{
			j = row_field[0] - '0';
			if (row_field[1]) j = (j * 10) + (row_field[1] - '0');		/* Scan out the digits.			*/
			tput_line_at(12, "MOVE DEC-BYTE-%d TO WISP-CRT-ORDER-AREA-4",j);
		}
		else									/* Must be a field name.		*/
		{
			tput_line_at(12, "MOVE %s TO WISP-DFIELD-0",row_field);
			tput_line_at(12, "CALL \"xx2byte\" USING WISP-DFIELD-0, WISP-CRT-ORDER-AREA-4");
		}
	}

	if (col_field[0])								/* Set the col byte.			*/
	{

		if (isdigit(col_field[0]))						/* If it's numeric, move the DEC-BYTE.	*/
		{
			j = col_field[0] - '0';
			if (col_field[1]) j = (j * 10) + (col_field[1] - '0');		/* Scan out the digits.			*/
			tput_line_at(12, "MOVE DEC-BYTE-%d TO WISP-CRT-ORDER-AREA-3",j);
		}
		else									/* Must be a field name.		*/
		{
			tput_line_at(12, "MOVE %s TO WISP-DFIELD-0",col_field);
			tput_line_at(12, "CALL \"xx2byte\" USING WISP-DFIELD-0, WISP-CRT-ORDER-AREA-3");
		}
	}

	if (wcc_byte || row_field[0] || col_field[0])
	{
		tput_line_at	(12, "MOVE 4 TO WISP-LONGWORD");
		tput_line_at	(12, "CALL \"wmemcpy\" USING %s,",crt_record[crt_num]);
		tput_clause	(16, "WISP-CRT-O-A, WISP-LONGWORD");
	}

	tput_line_at(12, "MOVE DEC-BYTE-%d TO VWANG-LINES,\n",(crt_record_size[crt_num]-4)/80);
	tput_line_at(12, "MOVE SPACES TO %s,\n",crt_status[cur_crt]);
	tput_line_at(12, "MOVE \"X\" TO WISP-ALLOWABLE-PF-KEYS,\n");
	tput_line_at(12, "CALL \"vwang\" USING VWANG-WRITE-ALL,");
	tput_clause (16, "WISP-CRT-RECORD,");
	tput_clause (16, "VWANG-LINES,");				/* Write full screen for now	*/
	tput_clause (16, "WISP-ALLOWABLE-PF-KEYS,");
	tput_clause (16, "%s,",crt_pfkey[cur_crt]);
	tput_clause (16, "%s",crt_status[cur_crt]);


	if (ptype == -1)									/* has a period in it...	*/
	{
		tput_clause(12, ".");
	}

	if ((ptype == 1) || (kword == -1)) hold_line();						/* Ended on a new line		*/
}


