#define EXT extern
#include "wisp.h"
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
	for ( cur_crt=0; cur_crt<crt_fcount; cur_crt++) if (crt_prec[cur_crt] > crt_num) {cur_crt--; break;}

	if (cur_crt == crt_fcount) cur_crt--;

	put_line		("\n");
	if (from_item[0])								/* Handle the REWRITE FROM phrase.	*/
	{
		write_line	("           MOVE %s TO\n",from_item);
		write_line	("                %s,\n",crt_record[crt_num]);
	}

	if (crt_relative[cur_crt][0])
	{										/* write what line to start at		*/
		write_line	("           CALL \042xx2byte\042 USING %s,\n",crt_relative[cur_crt]);
		write_line  	("                                %s\n",crt_record[crt_num]);
	}

	write_line		("           MOVE %s TO WISP-CRT-RECORD,\n",crt_record[crt_num]);

	if (wcc_byte)									/* Do a special WCC function?		*/
	{
#ifdef OLD
		put_line  	("           MOVE WISP-CRT-ORDER-AREA-2 TO WISP-SAVE-WCC,\n");	/* Save the order area.		*/
		write_line	("           MOVE DEC-BYTE-%d TO WISP-SET-BYTE,\n",wcc_byte);
		put_line  	("           CALL \042bit_on\042 USING\n");
		put_line  	("                WISP-SET-BYTE,\n");
		put_line  	("                WISP-CRT-ORDER-AREA-2\n");
#endif
		write_line	("           MOVE DEC-BYTE-%d TO WISP-CRT-ORDER-AREA-2,\n",wcc_byte);
	}

	if (row_field[0])								/* Set the row byte			*/
	{
		if (isdigit(row_field[0]))						/* If it's numeric, move the DEC-BYTE.	*/
		{
			j = row_field[0] - '0';
			if (row_field[1]) j = (j * 10) + (row_field[1] - '0');		/* Scan out the digits.			*/
			write_line("           MOVE DEC-BYTE-%d TO WISP-CRT-ORDER-AREA-4\n",j);
		}
		else									/* Must be a field name.		*/
		{
			write_line("           MOVE %s TO WISP-DFIELD-0\n",row_field);
			put_line  ("           CALL \"XX2BYTE\" USING WISP-DFIELD-0\n");
			put_line  ("                                WISP-CRT-ORDER-AREA-4\n");
		}
	}

	if (col_field[0])								/* Set the col byte.			*/
	{

		if (isdigit(col_field[0]))						/* If it's numeric, move the DEC-BYTE.	*/
		{
			j = col_field[0] - '0';
			if (col_field[1]) j = (j * 10) + (col_field[1] - '0');		/* Scan out the digits.			*/
			write_line("           MOVE DEC-BYTE-%d TO WISP-CRT-ORDER-AREA-3\n",j);
		}
		else									/* Must be a field name.		*/
		{
			write_line("           MOVE %s TO WISP-DFIELD-0\n",col_field);
			put_line  ("           CALL \"XX2BYTE\" USING WISP-DFIELD-0\n");
			put_line  ("                                WISP-CRT-ORDER-AREA-3\n");
		}
	}

	if (wcc_byte || row_field[0] || col_field[0])
	{
		put_line	("           MOVE 4 TO WISP-LONGWORD\n");
		write_line	("           CALL \"wmemcpy\" USING %s,\n",crt_record[crt_num]);
		put_line	("                 WISP-CRT-O-A, WISP-LONGWORD\n");
	}

	write_line("           MOVE DEC-BYTE-%d TO VWANG-LINES,\n",(crt_size[crt_num]-4)/80);
	write_line("           MOVE SPACES TO %s,\n",crt_status[cur_crt]);
	put_line  ("           MOVE \"X\" TO WISP-ALLOWABLE-PF-KEYS,\n");
	put_line  ("           CALL \042vwang\042 USING VWANG-WRITE-ALL,\n");
	put_line  ("                              WISP-CRT-RECORD,\n");
	put_line  ("                              VWANG-LINES,\n");				/* Write full screen for now	*/
	put_line  ("                              WISP-ALLOWABLE-PF-KEYS,\n");
	write_line("                              %s,\n",crt_pfkey[cur_crt]);
	write_line("                              %s",crt_status[cur_crt]);

#ifdef OLD
	if (wcc_byte)										/* do a special WCC function?	*/
	{
		put_line  ("\n           MOVE WISP-SAVE-WCC TO WISP-CRT-ORDER-AREA-2");		/* restore the order area	*/
	}
#endif


	if (ptype == -1)									/* has a period in it...	*/
	{
		put_line(".\n");
	}
	else
	{
		put_char('\n');
	}
	if ((ptype == 1) || (kword == -1)) hold_line();						/* Ended on a new line		*/
}


