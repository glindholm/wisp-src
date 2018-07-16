static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* Routines to process the READ  statements.										*/

#define EXT extern
#include "wisp.h"
#include "wispfile.h"
#include "crt.h"
#include "cobfiles.h"

#define READ_FILE "$WRXXXXXX"

int	unterminated_if = 0;

static char rd_str[STRING_BUFFER_SIZE];							/* Buffer for READ statements.		*/
static char tm_str[STRING_BUFFER_SIZE];							/* Buffer for TIMEOUT statements.	*/
static int adjcol();
static void addlockclause();

p_read()
{
	int i,j,hold,fnum,invkey_clause,atend_clause,t_out,t_flag,is_crt,did_lockclause,indexed_file;
	char  	*crt_type;
	char  	tstr[132],into_rec[40];
	int	col, savecol, maincol, addnl, savenl;
	static  int readnum = 0;							/* current paragraph number for READ's	*/

	char hstr[40];									/* Holder-ID string.			*/
	char timstr[40];								/* TIMEOUT value string.		*/

	static FILE	*read_temp_file;

	write_log("WISP",'I',"PROCREAD","Processing Read Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be READ)	*/
	col   = 1 + get_ppos();								/* Find out its column location.	*/
	maincol = col;
	j = peek_param(o_parms[1]);							/* peek at the file name		*/

	cur_crt = crt_index(o_parms[1]);
	is_crt = (-1 != cur_crt);

	if (is_crt)									/* is it a crt record?			*/
	{
		if (acn_cobol)
		{
			write_log("WISP",'W',"NATIVE","Workstation READ %s uses WISP Screens",crt_file[cur_crt]);
		}
		
		write_log("WISP",'I',"FIXREAD","READ of %s repaired.",crt_file[cur_crt]); /* yes, fix it			*/
		ptype = get_param(o_parms[1]);						/* Really get it now.			*/

		crt_type = "VWANG-READ-ALL";						/* Set type of read to READ-ALL.	*/
		into_rec[0] = 0;

		stredt(linein,"READ","");
		stredt(linein,o_parms[1],"");

		j = ptype;

		if (ptype != -1)							/* No period means could have INTO.	*/
		{
			ptype = get_param(o_parms[0]);					/* check for INTO.			*/

			if (!strcmp(o_parms[0],"RECORD"))				/* Found a RECORD keyword, remove it.	*/
			{
				j = ptype;						/* Remember punctuation.		*/
				ptype = get_param(o_parms[0]);
			}

			if (!strcmp(o_parms[0],"MODIFIABLE"))				/* Found a MODIFIABLE keyword, remove it.*/
			{
				crt_type = "VWANG-READ-MODIFIABLE";			/* Set type string.			*/
				j = ptype;						/* Remember punctuation.		*/
				ptype = get_param(o_parms[0]);
			}

			if (!strcmp(o_parms[0],"ALTERED"))				/* Found a ALTERED keyword, remove it.	*/
			{
				crt_type = "VWANG-READ-ALTERED";
				j = ptype;						/* Remember punctuation.		*/
				ptype = get_param(o_parms[0]);
			}

			if (!strcmp(o_parms[0],"INTO"))					/* Is it?				*/
			{
				ptype = get_param(into_rec);				/* Get the field name.			*/
				j = ptype;						/* Remember the item type.		*/
			}
			else hold_line();						/* Otherwise hold on to the line.	*/
		}

					/*123456789012345678901234567890123456789012345678901234567890123456789012*/
		tput_blank();

		if (crt_relative[cur_crt][0])						/* Load relative key into order-area	*/
		{
			tput_line_at	(12, "CALL \"xx2byte\" USING %s,",crt_relative[cur_crt]);
			tput_clause  	(16, "%s",crt_record[crt_prime_rec[cur_crt]]);
		}
											/* Copy the Order-Area for the read	*/
		tput_line_at		(12, "MOVE %s TO WISP-CRT-ORDER-AREA\n",crt_record[crt_prime_rec[cur_crt]]);
											/* Load number of lines			*/
		tput_line_at		(12, "MOVE DEC-BYTE-%d TO VWANG-LINES,\n",
									(crt_record_size[crt_prime_rec[cur_crt]]-4)/80);
		tput_line_at		(12, "MOVE SPACES TO %s,\n",crt_status[cur_crt]);/* Must clear status field	*/
		tput_line_at  		(12, "MOVE \"A\" TO WISP-ALLOWABLE-PF-KEYS\n");
		tput_line_at		(12, "CALL \"vwang\" USING %s,",crt_type);
		tput_clause		(16, "WISP-CRT-RECORD,");
		tput_clause		(16, "VWANG-LINES,");
		tput_clause		(16, "WISP-ALLOWABLE-PF-KEYS,");
		tput_clause		(16, "%s,",crt_pfkey[cur_crt]);
		tput_clause		(16, "%s",crt_status[cur_crt]);
		tput_line_at		(12, "MOVE WISP-CRT-RECORD TO %s",crt_record[crt_prime_rec[cur_crt]]);

		if (into_rec[0])							/* Had an INTO phrase.			*/
		{
			tput_line_at	(12, "MOVE %s TO",crt_record[crt_prime_rec[cur_crt]]);
			tput_clause	(16, "%s",into_rec);
		}

		if (crt_cursor[cur_crt][0])						/* if there is a cursor			*/
		{
			tput_line_at  	(12, "CALL \"w2rowcol\" USING WISP-CRT-ORDER-AREA-3,");
			tput_clause	(16, "%s",crt_cursor[cur_crt]);
		}

		if (j == -1)								/* has a period in it...		*/
		{
			tput_clause(12,".");
		}

		tput_flush();
		return(0);								/* exit the FOR loop			*/
	}
	else /* Not a CRT read */
	{
		hstr[0] = '\0';
		strcpy(timstr,"9999");							/* Default timeout.			*/

		i = 0;
		o_parms[5][0] = '\0';							/* no status field yet			*/

		fnum = file_index(o_parms[1]);

		if (fnum == -1)								/* no file matched, error		*/
		{
			write_log("WISP",'F',"READFNF",
			"Error -- File %s, referenced by READ statement but not Declared.",o_parms[1]);
			exit_wisp(EXIT_WITH_ERR);
		}
		else
		{
			write_log("WISP",'I',"HANDLEREAD","Handling READ of %s.",prog_files[fnum]);
			strcpy(o_parms[5],prog_fstats[fnum]);				/* copy the status field		*/
		}

		strcpy(tm_str,"");							/* Need to init timeout string.		*/

		strcpy(rd_str,"");							/* start a new line			*/

		if (!read_file_ptr)							/* if there has not yet been a file made*/
		{
			read_file_ptr = open_cob_file(read_fname,FOR_OUTPUT,1);		/* open for write			*/
			if (!read_file_ptr) exit_with_err();
			read_temp_file = read_file_ptr->a_file->file;
			fprintf(read_temp_file,"***********  READ STATEMENTS\n");
			read_file_ptr->line_count++;
											/* put a line in it			*/
		}

		hold = 0;								/* no hold yet...			*/
		invkey_clause = 0;							/* no INVALID KEY yet...		*/
		atend_clause = 0;							/* no AT END yet...			*/
		t_out = 0;								/* No TIMEOUT phrase...			*/
		t_flag = 0;								/* Global flag for timeout phrase.	*/
		did_lockclause = 0;							/* Wrote the locking clause		*/
		addnl = 0;


		if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))			/* If using automatic record locking	*/
		{									/* then don't supply an "ALLOWING" 	*/
			did_lockclause = 1;						/* clause. (Trick it into not writing	*/
		}									/* the ALLOWING clause.)		*/

		indexed_file = (prog_ftypes[fnum] & INDEXED_FILE);

		do
		{
			if (t_out)							/* If timeout found.			*/
			{
				add2buff(tm_str,o_parms[0],col,addnl);
			}
			else 								/* Put it in temp string		*/
			{
				add2buff(rd_str,o_parms[0],col+4,addnl);
			}

			if (o_parms[0][0]) stredt(linein,o_parms[0],"");		/* Remove it from the input line	*/
			ptype = get_param(o_parms[0]);					/* get a new parm....			*/
			adjcol(ptype,&col,&addnl);

			if (ptype == 1)							/* first parm on a new line		*/
			{
				if (has_cont)						/* Force in the continuation		*/
				{
					strcpy(tstr,"\n      -");

					if (t_out)
					{
						strcat(tm_str,tstr);			/* start a new line			*/
					}
					else
					{
						strcat(rd_str,tstr);			/* start a new line			*/
					}
					addnl = 0;
				}
				else
				{
					addnl = 1;
				}
			}

			savenl = addnl;
			savecol = col;

			if (!strcmp(o_parms[0],"WITH") || !strcmp(o_parms[0],"HOLD"))
			{
				stredt(linein,o_parms[0],"");
				o_parms[0][0] = 0;					/* clear it				*/
				write_log("WISP",'I',"FIXWITHHOLD","Corrected WITH HOLD in READ.");
				hold = 1;						/* flag it				*/
			}
			else if (t_out && !strcmp(o_parms[0],"NEXT"))			/* Handle NEXT SENTENCE in timeout	*/
			{
				if (o_parms[0][0]) stredt(linein,o_parms[0],"");	/* Remove it from the input line	*/
				ptype = get_param(o_parms[0]);				/* get a new parm....			*/
				adjcol(ptype,&col,&addnl);
				if ( !strcmp(o_parms[0],"SENTENCE") )
				{
					o_parms[0][0] = '\0';
					add2buff(tm_str,"CONTINUE",col,addnl);
				}
				else
				{
					add2buff(tm_str,"NEXT",col,addnl);
					add2buff(tm_str,o_parms[0],col,0);
					o_parms[0][0] = '\0';
					write_log("WISP",'E',"READ","Unable to parse keyword NEXT in READ.");
				}
				
			}
			else if (!strcmp(o_parms[0],"NEXT"))				/* handle NEXT special because		*/
			{								/* it is in the procedure division	*/
											/* keyword list and will exit the while.*/
				stredt(o_parms[0],"N"," N");				/* Just put a space in, it won't match	*/
											/* the keyword list any more!		*/
			}
			else if (!strcmp(o_parms[0],"INTO"))				/* handle INTO special because		*/
			{								/* it signals a NO LOCK for LPI		*/
				if (lpi_cobol || acu_cobol)
				{							/* NO LOCK must precede INTO.		*/
					addlockclause(&did_lockclause,indexed_file,hold,rd_str,col+8);
				}
			}
			else if (!strcmp(o_parms[0],"INVALID"))				/* INVALID KEY phrase?			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,savecol+8);

				stredt(linein," INVALID"," ");				/* remove INVALID			*/
				if (ptype != -1)
				{
					peek_param(o_parms[7]);				/* get "KEY"				*/
					if (!strcmp(o_parms[7],"KEY"))			/* Was it KEY?				*/
					{
						ptype = get_param(o_parms[0]);		/* Get it, and remove it.		*/
						adjcol(ptype,&col,&addnl);
						if (addnl) { savenl = 1; savecol = col; }
						stredt(linein," KEY"," ");		/* remove KEY				*/
					}
				}

				if (ptype == -1)					/* Premature period!			*/
				{
					write_log("WISP",'I',"BADINVKEY",
					"Bad READ syntax, INVALID KEY followed by a period.");
					o_parms[0][0] = '\0';
				}
				else
				{
					ptype = get_param(o_parms[0]);			/* what to do?				*/
					adjcol(ptype,&col,&addnl);
				}

				add2buff(rd_str,"INVALID KEY",savecol+4,savenl);	/* write it out				*/

				if (hold) add2buff(rd_str,"MOVE \"Y\" TO WISP-TEST-BYTE.",maincol+8,1);

				t_out = 0;						/* Clear any TIMEOUT flag.		*/
				invkey_clause = 1;					/* flag it				*/
			}
			else if (!strcmp(o_parms[0],"AT") || !strcmp(o_parms[0],"END")) /* AT END phrase?			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,savecol+8);

				if (o_parms[0][0] == 'A')
				{
					stredt(linein," AT "," ");			/* remove AT				*/
					ptype = get_param(o_parms[0]);			/* get "END"				*/
					adjcol(ptype,&col,&addnl);
					if (addnl) { savenl = 1; savecol = col; }
				}
				stredt(linein," END"," ");				/* remove END				*/
				if (ptype == -1)					/* Prematur period.			*/
				{
					write_log("WISP",'I',"BADATEND",
					"Bad READ syntax, AT END followed by a period.");
					o_parms[0][0] = '\0';
				}
				else
				{
					ptype = get_param(o_parms[0]);			/* what to do?				*/
					adjcol(ptype,&col,&addnl);
				}

				add2buff(rd_str,"AT END",savecol+4,savenl);		/* write it out				*/

				if (hold) add2buff(rd_str,"MOVE \"Y\" TO WISP-TEST-BYTE.",maincol+8,1);

				t_out = 0;						/* Clear any TIMEOUT flag.		*/
				atend_clause = 1;					/* flag it				*/
			}
			else if (!strcmp(o_parms[0],"TIMEOUT"))				/* TIMEOUT phrase?			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,savecol+8);

				write_log("WISP",'I',"READWTIME","READ with TIMEOUT modified.");
				stredt(linein," TIMEOUT"," ");				/* remove TIMEOUT			*/
				ptype = get_param(timstr);				/* Get the timeout value.		*/
				adjcol(ptype,&col,&addnl);
				if (!strcmp(timstr,"OF"))
				{							/* Skip over OF keyword.		*/
					stredt(linein,"OF","");
					ptype = get_param(timstr);
					adjcol(ptype,&col,&addnl);
				}

				t_out = 1;						/* Flag it.				*/
				t_flag = 1;

				o_parms[0][0] = '\0';

			}
			else if (!strcmp(o_parms[0],"HOLDER-ID"))			/* HOLDER-ID phrase?			*/
			{
				stredt(linein," HOLDER-ID"," ");			/* remove HOLDER-ID			*/
				ptype = get_param(hstr);				/* Get the holder field.		*/
				if (!addnl) adjcol(ptype,&col,&addnl);
				if (!strcmp(hstr,"IN"))
				{							/* Skip over IN keyword.		*/
					stredt(linein,"IN","");
					ptype = get_param(hstr);
					if (!addnl) adjcol(ptype,&col,&addnl);
				}
				o_parms[0][0] = '\0';
			}
			else if (!strcmp(o_parms[0],"SECOND") || !strcmp(o_parms[0],"SECONDS"))
			{
				o_parms[0][0] = '\0';					/* Just remove seconds.			*/
			}
			else if (!strcmp(o_parms[0],"KEY"))				/* Handle KEY phrase.			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,col+8);

				stredt(linein,o_parms[0],"");				/* Remove "KEY" from line		*/
				ptype = get_param(o_parms[0]);
				adjcol(ptype,&col,&addnl);
				if (!strcmp(o_parms[0],"IS"))
				{
					if (addnl) { savenl = 1; savecol = col; }
					stredt(linein,o_parms[0],"");			/* Remove "IS" from line		*/
					ptype = get_param(o_parms[0]);
					adjcol(ptype,&col,&addnl);
				}

				add2buff(rd_str,"KEY IS",savecol+4,savenl);

				key_name(o_parms[0],0);					/* Key name conversion			*/

			}
		} while ((ptype != -1) && (!proc_keyword(o_parms[0]) || t_out));
											/* do till we hit a LAST parm or keyword*/

		if (ptype == -1)							/* a LAST parm				*/
		{
			strcpy(tstr,o_parms[0]);
		}
		else
		{
			*tstr = 0;							/* Fake the last parameter		*/
		}

		if (t_out)								/* If there is a timeout, (also a hold)	*/
		{
			add2buff(tm_str,tstr,col,addnl);
		}
		else
		{	
			add2buff(rd_str,tstr,col+4,addnl);				/* Put out the last parameter		*/

			addlockclause(&did_lockclause,indexed_file,hold,rd_str,col+8);

		}

		if (hold)								/* If going to hold the record,		*/
		{									/* Wait till no soft/hard lock		*/
			sprintf(tstr, "CALL \"wfwait\" USING %s",o_parms[5]);
			add2buff(rd_str,tstr,maincol+4,1);
			add2buff(rd_str,"WISP-FILE-TIMEOUT",maincol+8,0);
		}

		strcat(rd_str,"\n");							/* finish this line			*/
		strcat(tm_str,"\n");							/* finish this line			*/

										 	/* Now write everything out to the file.*/

		tput_line          ("           MOVE \"RD\" TO WISP-DECLARATIVES-STATUS\n");

		if ( hold && (invkey_clause || atend_clause) )
		{
			tput_line  ("           MOVE \"N\" TO WISP-TEST-BYTE\n"); 	/* Set up the INVALID KEY flag.		*/
		}

		if (t_flag)
		{
			tput_line("           MOVE %s TO WISP-FILE-TIMEOUT\n",timstr); /* Store the TIMEOUT value.		*/
			tput_line("           MULTIPLY WISP-FILE-TIMEOUT BY 100 GIVING WISP-FILE-TIMEOUT\n");
		}
		else if (hold)
		{
			tput_line("           MOVE 0 TO WISP-FILE-TIMEOUT\n"); 	/* Store the TIMEOUT value.		*/
		}




		if (hold)			/* Insert all the special logic to handle WITH HOLD			*/
						/*    - if INVALID KEY or AT END then put READ into separate para	*/
						/*    - keep trying to read until you get the record			*/
						/*    - handle TIMEOUT logic						*/
						/*    - add UNLOCK logic						*/

		{								/* Gonna do a hold, free last file.	*/
			set_lock(fnum,12);					/* Set new lock.			*/

			if (invkey_clause || atend_clause)			/* If HOLD and INVALID KEY or AT END use*/
			{							/* Paragraph performs.			*/
				if (copylib && writing_copybook())		/* Are we in a copy lib?		*/
				{
					tput_line("           PERFORM WISP-READ-%s-%d",cpy_file,cpy_seq);
					if (!re_copy)				/* Is this a second time?		*/
					{
						fprintf(read_temp_file,"\n       WISP-READ-%s-%d.\n",cpy_file,cpy_seq);
						read_file_ptr->line_count += 2;
						fprintf(read_temp_file,"%s",rd_str); /* Now write out the whole thing.	*/
						read_file_ptr->line_count += strchrcnt(rd_str,'\n');
						fprintf(read_temp_file,"           CONTINUE.\n");
						read_file_ptr->line_count++;
					}
					cpy_seq++;
				}
				else
				{
					tput_line("           PERFORM WISP-READ-NUMBER-%d",readnum);
										/* write the paragraph name to the file	*/
					fprintf(read_temp_file,"\n       WISP-READ-NUMBER-%d.\n",readnum);
					read_file_ptr->line_count += 2;
					fprintf(read_temp_file,"%s",rd_str);	/* Now write out the whole thing.	*/
					read_file_ptr->line_count += strchrcnt(rd_str,'\n');
					fprintf(read_temp_file,"           CONTINUE.\n");
					read_file_ptr->line_count++;
					readnum++;				/* increment the counter		*/
				}
			}
			else							/* No INVALID KEY, use linein PERFORM.	*/
			{
				tput_line  	("           PERFORM WITH");
			}

			tput_clause		(16, "TEST AFTER");			/* Wait for hard locks.			*/

			tput_line		("                   UNTIL (%s IS NOT = \"%s\")\n",o_parms[5],hard_lock);

			if (t_flag) tput_line  	("                   OR (WISP-FILE-TIMEOUT IS < 1)\n");

			if (!(invkey_clause || atend_clause))
			{
				tput_block(rd_str);					/* Now write out the whole thing.	*/
				tput_line	("           END-PERFORM\n");
			}
		}
		else /* no WITH HOLD */
		{
			tput_block(rd_str);						/* Now write out the whole thing.	*/
		}

		if (t_flag)
		{
			tput_line		("           IF ( %s = \"%s\" AND\n",o_parms[5],hard_lock);
			tput_clause		(16,"WISP-FILE-TIMEOUT < 1)");
			clear_locking();						/* Add locking logic			*/

			tput_block(tm_str);
			tput_flush();
		}

		if (ptype == -1)
		{
			if (t_flag)
			{
				tput_line  	("           END-IF.\n");
			}
			else
				tput_line	("           CONTINUE.\n");
		}
		else
		{                                                                            
			if (t_flag)
			{
				tput_line  	("           ELSE\n");
			}

			if ( hold && (invkey_clause || atend_clause) ) 
			{
				tput_line  	("           IF WISP-TEST-BYTE = \"Y\" THEN\n");
				clear_locking();					/* Add locking logic			*/
				unterminated_if = 1;					/* Weve added an IF statement		*/
			}
		}


		if (ptype != -1) hold_line();

		if (!vax_cobol && hold && did_lockclause)
		{
			write_log("WISP",'F',"READERR","NO LOCK inserted when HOLD was specified, Keywords out of order.");
		}
		write_log("WISP",'I',"READDONE","Completed READ analysys");
	}

}

/*
	add2buff:	Add a string to a buffer keeping track of the last line size (from newline) and spliting the line
			if it would go over 72.  If the size is less then col it will pad out to col. The addstr does not 
			need to have spaces surrounding it, they are added automatically.
*/
add2buff(buff,addstr,col,newline)
char	*buff;							/* The buffer -- it can hold multiple lines.			*/
char	*addstr;						/* The string to add to buffer.					*/
int	col;							/* The column to start in if line is split.			*/
int	newline;						/* Force to a new line.						*/
{
	char	spaces[80];
	int	size, i, len;

	len = strlen(addstr);

	if (len < 1) return 0;

	for ( size = 0, i = strlen(buff)-1; i >= 0 && buff[i] != '\n'; size++, i--);

	if ( col + len > 71 )
	{
		col = 12;
	}

	if ( newline || (size + len + 1 > 71) )
	{
		strcat(buff,"\n");							/* Start a new line			*/
		memset(spaces,' ',col-1);
		spaces[col-1] = '\0';
	}
	else if ( size < col-1 )
	{
		i = col - 1 - size;							/* pad out to col			*/
		memset(spaces, ' ', i);
		spaces[i] = '\0';
	}
	else
	{
		strcpy(spaces," ");							/* A one space between tokens		*/
	}

	strcat(buff,spaces);

	strcat(buff,addstr);								/* Append the string			*/
}

static adjcol(ptype,col,addnl)
int	ptype;
int	*col;
int	*addnl;
{
	if (ptype == 1)
	{
		*col = 1 + get_ppos();							/* Find out its column location.	*/
		*addnl = 1;
	}
	else
	{
		*addnl = 0;
	}
	return 0;
}

static void addlockclause(did_lockclause,indexed_file,hold,buff,col)
int	*did_lockclause;
int	indexed_file;
int	hold;
char	*buff;
int	col;
{
	if ( *did_lockclause )
	{
		return;
	}

	if ( vax_cobol )
	{
		if ( hold ) add2buff(buff,"ALLOWING NO OTHERS",col,0);
		else	    add2buff(buff,"REGARDLESS OF LOCK",col,0);
		*did_lockclause = 1;
	}
	else
	{
		if (!hold)
		{
			if (mf_cobol && indexed_file)
			{
				add2buff(buff,"IGNORE LOCK",col,0);
				*did_lockclause = 1;
			}
			else
			{
				add2buff(buff,"NO LOCK",col,0);
				*did_lockclause = 1;
			}
		}
	}
}
/*
**	History:
**	$Log: wt_read.c,v $
**	Revision 1.13  1997-09-15 13:57:09-04  gsl
**	fix warning
**
**	Revision 1.12  1997-09-12 14:02:13-04  gsl
**	change native warning
**
**	Revision 1.11  1997-09-12 13:19:18-04  gsl
**	Add warning for Native Screens with WOrkstation READ
**
**	Revision 1.10  1996-08-30 21:56:23-04  gsl
**	drcs update
**
**
**
*/
