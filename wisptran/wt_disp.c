			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"

static char *d_ptr;									/* pointer to current buffer in use	*/
static char on_str[72];									/* Buffer for the ON PFKEY key list	*/
static int chk_decl();

static char *disp_keywords[] =	
{	
	"NO-MOD",
	"ON",
	"ONLY",
	"PFKEY",
	"PFKEYS",
	""	
};
static int disp_kwcount = 5;

t_pfkeys(t_line)						/* test for the PFKEYS keyword in a DISPLAY AND READ statement	*/
char t_line[];							/* if we find it or not, set the pfkey flag			*/
{
	char the_field[32][34];
	char outline[132],savchr;
	int num_fields,done,i, boundary;

	/*
		[ [ONLY] {PFKEY|PFKEYS} {ident} [ident ...]  ]
	*/

	num_fields = 0;									/* no fields in the phrase		*/
	pfkeys = 1;									/* flag it				*/

	if (!strcmp("ONLY",t_line))							/* Look for ONLY PFKEYS			*/
	{
		strcpy(outline,"\"");							/* leading quotes for pfkeys string	*/
		d_period = get_param(t_line);						/* Skip over it...			*/
	}
	else
	{
		strcpy(outline,"\"");							/* leading quotes for pfkeys string	*/
		strcat(outline,"00");							/* Start the PFKEYS string with ENTER	*/
	}

	if (!strcmp("PFKEYS",t_line) || !strcmp("PFKEY",t_line))			/* Look for the PFKEYS keyword		*/
	{
		done = 0;								/* not done				*/
		do
		{
			d_period = get_param(t_line);					/* get the various pfkeys		*/
			if (t_line[0] == 0)						/* null param means separate period	*/
			{
				done = 1;
			}
			else if (isdigit(t_line[0]))					/* if 1st char is a digit, its a pfkey	*/
			{
				if (!t_line[1])						/* single digit key, put in a leading 0	*/
				{
					strcat(outline,"0");
				}
				strcat(outline,t_line);					/* copy pfkey to output			*/
			}
			else if (!proc_keyword(t_line) && !keyword(t_line,disp_keywords)) /* it's not a keyword,*/
			{								/* must be a field			*/
				strcpy(the_field[num_fields++],t_line);			/* save it				*/
				write_log("WISP",'I',"FLDFOUND","Field %s found in PFKEYS phrase.",t_line);
			}
			else
			{
				done = 1;						/* all done				*/
			}
		} while (!done && (d_period != -1));					/* stop					*/
	}

	strcat(outline,"X\"");

	if (num_fields)									/* had some fields in it		*/
	{
		for (i=0; i<num_fields; i++)
		{
			tput_line	("           MOVE %s TO WISP-DFIELD-%d",the_field[i],i);/* move to our PIC 99 field	*/
		}

		tput_line  		("           STRING");

		for (i=0; i<num_fields; i++)						/* use the STRING statement to add them	*/
		{
			tput_line	("               WISP-DFIELD-%d DELIMITED BY SIZE,",i);
		}
	}
	else
	{
		tput_line		("           MOVE");
	}
#ifdef DEBUG
put_line("123456*8901234567890123456789012345678901234567890123456789012345678901234567890\n");
#endif
	boundary=72-15;
	if (strlen(outline) > boundary)							/* Won't fit on one line		*/
	{
		savchr = outline[boundary];						/* save the char			*/
		outline[boundary] = '\0';
		tput_line		("               %s",outline);			/* write first part			*/
		outline[boundary] = savchr;
		tput_line		("      -        \"%s",&outline[boundary]);	/* write the rest			*/
	}
	else
	{
		tput_line		("               %s",outline);			/* just write it			*/
	}

	if (num_fields)
	{
		tput_line		("               DELIMITED BY SIZE,\n");	/* delimit the string			*/
		tput_line  		("               INTO WISP-ALLOWABLE-PF-KEYS,\n");/* now finish it			*/
	}
	else
	{
		tput_line  		("               TO WISP-ALLOWABLE-PF-KEYS,\n");/* just finish the MOVE			*/
	}

	strcpy(on_str,"\"X\"");								/* Set on pfkeys string to none		*/

	if (d_period == -1)								/* if there is a period then		*/
	{
		pmode = 0;								/* end of DISPLAY mode			*/
		write_log("WISP",'I',"DISPEND","DISPLAY ended, PFKEYS verb followed by a period.");
	}
}	/*	upon exit, we can assume that t_line is holding the next parm to parse if pmode is still DISPLAY mode	*/

t_onpfkeys(t_line)
char t_line[];
{
	char outline[132];
	int done;

	if (!strcmp(t_line,"ON"))
	{
		ptype = get_param(o_parms[1]);
		if (ptype != -1 && (!strcmp(o_parms[1],"PFKEYS") || !strcmp(o_parms[1],"PFKEY")))	/* is an ON PFKEYS	*/
		{
			on_pfkeys = 1;							/* indicate that we found it		*/
			d_ptr = pf_str;							/* set the display buffer pointer	*/
			strcpy(on_str,"\"");						/* start the on pfkey string		*/
											/* on_str contains the ON PFKEYS literal*/
											/* pf_str is the IF statement		*/
											/* start the IF				*/
			stredt(inline,"ON","");
			if (!strcmp(o_parms[1],"PFKEYS"))
			{
				stredt(inline,"PFKEYS","");
			}
			else
			{
				stredt(inline,"PFKEY","");
			}


			sprintf(outline,"           IF %s IS EQUAL TO ",crt_pfkey[cur_crt]);
			done = 0;
			do
			{
				d_period = get_param(t_line);				/* get the various pfkeys		*/

				if (isdigit(t_line[0]))					/* if 1st char is a digit, its a pfkey	*/
				{
					stredt(&inline[6],t_line,"");

					if (!t_line[1])					/* single digit key, put in a leading 0	*/
					{
						strcat(outline,"0");
						strcat(on_str,"0");
					}
					strcat(outline,t_line);				/* copy pfkey to output			*/
					strcat(on_str,t_line);				/* keep adding keys to the list		*/

					strcat(pf_str,outline);
					strcat(pf_str,"\n");
					sprintf(outline,"           OR %s IS EQUAL TO ",crt_pfkey[cur_crt]);
				}
				else if (!proc_keyword(t_line) && !keyword(t_line,disp_keywords))
				{							/* must be a field			*/
					strcat(outline,t_line);				/* copy pfkey to output			*/

					strcat(pf_str,outline);				/* put OR phrase into list		*/
					strcat(pf_str,"\n");				/* start a new one			*/
					sprintf(outline,"           OR %s IS EQUAL TO ",crt_pfkey[cur_crt]);
					write_log("WISP",'I',"FLDINONPFKEY","Field %s found in ON PFKEYS phrase.",t_line);
				}
				else
				{
					done = 1;					/* all done				*/
				}
			} while (!done && (d_period != -1));				/* stop					*/

			strcat(on_str,"X\"");						/* terminate the on pfkey list		*/
			strcat(pf_str,"               CONTINUE,\n");			/* in case there are no action phrases	*/

			if (d_period == 0)						/* still more on the line		*/
			{								/* copy the rest of the line to output	*/
				strcat(pf_str,"               ");
				strcat(pf_str,t_line);

				do
				{
					d_period = get_param(t_line);
					if (d_period != 1)
					{
						strcat(pf_str," ");
						strcat(pf_str,t_line);
					}
				} while (!d_period);					/* do it till we hit EOL or EOPhrase	*/

				if (d_period == -1)
				{
					pmode = 0;					/* end of DISPLAY mode			*/
					write_log("WISP",'I',"DISPEND","DISPLAY ended, ON PFKEYS followed by a period.");
				}
				strcat(pf_str,",\n");					/* just end this line, already set up	*/
											/* for NO-MOD etc...			*/
			}
		}
		else
		{
			write_log("WISP",'F',"ERRORONPFKEY","Error in ON PFKEYS phrase.\n");
			write_log("",' ',"","%s",inline);
			exit_wisp(EXIT_WITH_ERR);
		}
	}
}	/*	upon exit, we can assume that t_line is holding the next parm to parse if pmode is still DISPLAY mode	*/

t_nomod(t_line)
char t_line[];
{
	if (!strcmp("NO-MOD",t_line))						/* found a NO-MOD phrase			*/
	{
		no_mod = 1;							/* flag it					*/

		d_ptr = nm_str;								/* set DISPLAY string pointer		*/

		strcat(nm_str,"           IF ");
		strcat(nm_str,crt_status[cur_crt]);
		strcat(nm_str," IS GREATER THAN \"N?\"\n");

		strcat(nm_str,"               ");					/* start a blank line			*/
		do
		{
			d_period = get_param(t_line);
			if (d_period != 1) 						/* as long as not a new line		*/
			{
				strcat(nm_str," ");
				strcat(nm_str,t_line);
			}
		} while (!d_period);							/* do it till we hit EOL or EOPhrase	*/

		if (d_period == -1)
		{
			pmode = 0;							/* end of DISPLAY mode			*/
			write_log("WISP",'I',"DISPENDED","DISPLAY ended, NO-MOD followed by a period.");
		}
		strcat(nm_str,",\n");
	}
}




t_d_exit(t_line)
char t_line[];
{
	char tstr[16],scrtch[48],savchr;

	if (pmode == DISPLAY)								/* If we are still in display mode	*/ 
	{
		if (strpos(" NO-MOD ON ",t_line) != -1)					/* and there is a keyword to parse	*/
		{
			hold_line();							/* hold the line and repeat the cycle	*/
			return(0);
		}
		else if (strpos(" ELSE ELSE, IF ",t_line) != -1)			/* is it an IF or ELSE? if so, exit	*/
		{
			pmode = 0;							/* No more DISPLAY mode, hold the line	*/
			hold_line();
			write_log("WISP",'I',"DISPENDED","DISPLAY ended with ELSE or IF phrase.");
		}
		else if (!no_mod && !on_pfkeys)						/* we haven't found anything after the	*/
		{									/* PFKEYS verb, so this is the end	*/
			pmode = 0;
			hold_line();
			write_log("WISP",'I',"DISPENDED","DISPLAY ended, PFKEYS verb without ON PFKEYS or NO-MOD verbs.");
		}
		else if (strlast(inline,'.'))						/* see if it ends with a period		*/
		{									/* if it does, then terminate pmode,	*/
			pmode = 0;
			stredt(inline,".","");						/* remove the period,			*/
			d_period = -1;							/* remember it,				*/
			strcat(d_ptr,inline);						/* and write the phrase out		*/
			write_log("WISP",'I',"DISPENDED","DISPLAY ended with a period.");
		}
		else
		{
			strcat(d_ptr,inline);						/* just copy the line			*/
			return(0);							/* and do it again			*/
		}
	}
											/* we did find a termination state	*/
	if (strlen(on_str) > 54)							/* Won't fit on one line		*/
	{
		savchr = on_str[54];							/* save the char			*/
		on_str[54] = '\0';
		tput_line	("           MOVE   %s\n",on_str);			/* write first part			*/
		on_str[54] = savchr;
		tput_line	("      -     \"%s\n",&on_str[54]);			/* write the rest			*/
		tput_line	("               TO WISP-ON-PF-KEYS,\n");
	}
	else if (strlen(on_str) < 32)
	{
		tput_line	("           MOVE %s TO WISP-ON-PF-KEYS,\n",on_str);
	}
	else
	{
		tput_line	("           MOVE %s\n",on_str);
		tput_line	("               TO WISP-ON-PF-KEYS,\n");
	}


	if (in_decl)
	{
		make_fld(scrtch,scrn_name[scount],"DWDR-");
	}
	else
	{
		make_fld(scrtch,scrn_name[scount],"WDR-");
	}

	tput_line("           PERFORM %s,\n",scrtch);

	chk_decl();									/* Check DECLARATIVES interaction.	*/

	if (on_pfkeys)
	{
		tput_block(pf_str);							/* write the ON PFKEYS phrases		*/
	}

	if (no_mod)
	{
		if (on_pfkeys) 								/* following an ON PFKEYS		*/
		{
			tput_line("           ELSE\n");					/* so write an else			*/
		}
		tput_block(nm_str);							/* and write the NO-MOD phrase		*/
	}

	if (no_mod || on_pfkeys)							/* following something			*/
	{
		tput_line	("           ELSE\n");					/* so write an else			*/
	}

											/* now move all the screen variables to	*/
	if (in_decl)
	{
		make_fld(scrtch,scrn_name[scount],"DWGS-");
	}
	else
	{
		make_fld(scrtch,scrn_name[scount],"WGS-");
	}

	tput_line("           PERFORM %s\n",scrtch);
	tput_line("           MOVE WISP-CRT-RECORD TO %s",crt_record[crt_prime_rec[cur_crt]]);
											/* their target object fields		*/
	if (no_mod || on_pfkeys)							/* we are following something		*/
	{
		tput_line("           END-IF");						/* write an end-if			*/
	}

	if (d_period == -1) 	tput_clause(12, ".");
	else			tput_clause(12, ",");
}


n_pfkeys(q_period)									/* write the empty PFKEYS phrase	*/
int q_period;										/* flag to use a period or not		*/
{
	char wdr[48],wgs[48];

	tput_line  ("           MOVE \"00X\" TO WISP-ALLOWABLE-PF-KEYS,\n");		/* set ENTER as the only pfkey		*/
	tput_line  ("           MOVE \"X\"   TO WISP-ON-PF-KEYS,\n");			/* set ENTER as the only pfkey		*/
	if (in_decl)
	{
		make_fld(wdr,scrn_name[scount],"DWDR-");
		make_fld(wgs,scrn_name[scount],"DWGS-");
	}
	else
	{
		make_fld(wdr,scrn_name[scount],"WDR-");
		make_fld(wgs,scrn_name[scount],"WGS-");
	}
	tput_line("           PERFORM %s\n",wdr);					/* do the display and read		*/
	tput_line("           PERFORM %s\n",wgs);					/* now move all the screen var		*/
											/* their target object fields		*/

	tput_line("           MOVE WISP-CRT-RECORD TO %s",crt_record[crt_prime_rec[cur_crt]]);
	chk_decl();									/* Check DECLARATIVES interaction.	*/

	if (q_period)	tput_clause(12, ".");
	else		tput_clause(12, ",");

}

static chk_decl()
{
	char tstr[16];

	if (copy_to_dcl_file && !(scrn_flags[scount] & SCRN_IN_DECLARATIVES))		/* If currently copying paragraphs.	*/
	{										/* And this screen is not in the DECLAR	*/
		sprintf(tstr,"%d",scount);						/* Add it to the paragraph list.	*/
		add_perf(tstr);								/* S we can fix it later.		*/
	}
}

