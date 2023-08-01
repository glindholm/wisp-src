/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		ptdoit.c
**
**	Project:	wisp/proctran
**
**	Routines:	
**	doit()
**	check_after_bump()	Write COBOL source to output file
**	initialize()
**	process_linein()	Process the input line
**	write_str_int_type()	Write out the working storage source depending on the variable type
**	write_link_type()	Write out working storage source depending on the var type
*/

#define EXT extern

#include <stdio.h>
#include <string.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"
#include "pgcblsrc.h"
#include "pgkeyw.h"
#include "pgeqtns.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
EXT char cli_infile[STRBUFF];								/* Wang Procedure Language file name.	*/
EXT char out_fname[STRBUFF];								/* VMS COBOL generated output file name.*/

int pcnt;										/* Position count of chars in linein.	*/

/*
**	Static data
*/
static int max_cobline = 55;

/*
**	Static Function Prototypes
*/
static void initialize(int *num_cmds,int *num_var,int *scn_num,int *filler_num,int *current_row,int *num_link_var);
static void process_linein(int *num_cmds,int *num_var,int *scn_num,int *filler_num,int *current_row,int *num_link_var);
static void write_str_int_type();
static void write_link_type();
static void write_proc_link_src();
static void write_screen_info();
static void write_environ_vars();
static int kw_assign();
static void skip_comment();
static void p_putparm_keys(int *num_var,int *num_val);
static void check_write_string();
  
/*
**	ROUTINE:	doit()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
int doit(void)										/* Do the COBOL generation.		*/
{
	int num_par;
	int i, num_cmds, num_var, num_link_var;
	int scn_num, filler_num, current_row;
	char *cstr;
	int endfl;

	initialize(&num_cmds,&num_var,&scn_num,&filler_num,&current_row,&num_link_var);	/* Initialize all of the flags and vars.*/
	while (fgets(rline,STRBUFF,infile))						/* Read in STRBUFF chars until EOF.	*/
	{
		num_lineins++;
		if (*rline == LNFD || *rline == '\r' || *rline == '\0') *linein = '\0';			/* If an empty line.			*/
		else
		{
			strcpy(linein,rline);						/* Copy in local variables.		*/
			cstr = linein;
			if (*cstr == ' ') cstr++;					/* If is a comment line.		*/
			if (*cstr == '*') *linein = '\0';
			else
			{
				cstr = linein;
				setup_line(cstr);					/* Put linein into expected format.	*/
				process_linein(&num_cmds,&num_var,&scn_num,&filler_num,&current_row,&num_link_var);
			}
		}
	}
	if (feof(infile))								/* End of file indicator.		*/
	{
		write_log(util,'I','R',"EOF","End of file found, source: %s.\n",cli_infile);
	}										/* Is an error!				*/
	else	write_log(util,'E','R',"ERROR","Some kind of error reading %s source file.\n",cli_infile);

	fclose(infile);									/* Close the input file.		*/

	lncnt_type = 0;									/* Set to print line out cnt.		*/
	check_after_bump(env_div,"Environment Division");				/* Write out COBOL source.		*/
	check_after_bump(i_o_div,"Input-Output Section");
	check_after_bump(data_div,"Data Division");
	check_after_bump(work_stg_sec,"Working Storage");				/* Write out ALL working storage vars.	*/

	hld_table = cur_decl;								/* Make sure you hold the pointer.	*/
	if (cur_decl) while (cur_decl->prev_item) cur_decl = cur_decl->prev_item;	/* Traverse list to first declare item.	*/
	while (cur_decl)								/* While have declare.			*/
	{
		if (*cur_decl->type == 'S' || *cur_decl->type == 'I') write_str_int_type(); /* Write out working storage source.*/
		else	write_log(util,'E','W',"ERRTYPE",
				"Invalid type %c for declare variable %s.",*cur_decl->type,cur_decl->field1);
		cur_decl = cur_decl->next_item;						/* Set to the next structure.		*/
	}
	cur_decl = hld_table;								/* Put the pointer back if multiple.	*/

	if (start_screen) do_start_screen();						/* Process screen variables.		*/
	if (got_run)	check_after_bump(link_parms,"LINK Parameters");			/* Write out working storage source.	*/
	if (got_submit)	check_after_bump(cobol_submit,"SUBMIT sub Definitions");
	if (got_set || got_extract) check_after_bump(set_extct,"SET & EXTRACT variables");
	if (got_print)	check_after_bump(cobol_print,"PRINT sub Definitions");
	if (got_scratch) check_after_bump(cobol_scratch,"SCRATCH sub Definitions");
	if (got_rename)	check_after_bump(cobol_rename,"RENAME sub Definitions");
	if (got_find)	check_after_bump(cobol_find,"FIND Parameters");
	if (got_string)	check_after_bump(cobol_string,"STRING Parameters");
	if (got_readfdr) check_after_bump(cobol_readfdr,"READFDR Parameters");
	if (got_putparm)
	{
		check_after_bump(cobol_putparm,"PUTPARM Parameters");
		wrt_keyword_vars();							/* Write out the max possible keyword	*/
	}										/* vars for putparm call.		*/
	if (got_link)
	{
		check_after_bump(link_sec,"Linkage Section Header");			/* Write out link section header.	*/
		write_link_type();							/* Write out working storage source.	*/
		write_proc_link_src();							/* Write out procedure division source.	*/
	}
	else	check_after_bump(proc_div,"Procedure Division Header");			/* Else write out procedure div header.	*/

	check_after_bump(init_src,"Initialization Header");				/* Write out Initialization header.	*/
	write_environ_vars();								/* Write all ACCEPT FROM ENVORINMENT	*/

	setflag();									/* Start with no "in" flags set.	*/
	check_after_bump(main_process,"Main Process Header");				/* Write out main process header.	*/

	cur_cmd = st_para->all_command;							/* Write out initialization commands.	*/
	while (cur_cmd)									/* If no next item stop it.		*/
	{
		chk_cmd(&num_cmds,&num_var);						/* If got cmd, process it.		*/
		cur_cmd = cur_cmd->next_item;						/* Point to next command.		*/
	}

	cur_para = st_para->next_item;							/* Set ptr to para after init para.	*/
	num_par = 0;									/* Set the number of paragraphs.	*/
	while (cur_para)								/* If I got a paragraph go for it.	*/
	{
		num_par++;								/* Up the paragraph count.		*/
		strcpy(cobline,"       P-");						/* Load the Prefix.			*/
		strcat(cobline,cur_para->name);						/* Load the paragraph name.		*/
		num_cmds = 0;								/* First command of paragraph.		*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		setflag();								/* Start with no "in" flags set.	*/
		cur_cmd = cur_para->all_command;					/* Point to the first command.		*/
		endfl = FALSE;								/* Assume no END in para.		*/
		while (cur_cmd)								/* If no next item stop it.		*/
		{
			if (cur_cmd->end_cmd) endfl = TRUE;				/* Set so gens EXIT stuff for para.	*/
			chk_cmd(&num_cmds,&num_var);					/* If got cmd, process it.		*/
			cur_cmd = cur_cmd->next_item;					/* Point to next command.		*/
		}
		if (endfl)
		{
			strcpy(cobline,"       P-");					/* Load the Prefix.			*/
			strcat(cobline,cur_para->name); 				/* Load the paragraph name.		*/
			strcat(cobline,"-EXIT");
			end_prt_line(1);						/* Add end of line stuff and write line.*/

			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline," EXIT");
			end_prt_line(1);						/* Add end of line stuff and write line.*/
			setflag();
		}
		cur_para = cur_para->next_item;						/* Point to the next paragraph.		*/
	}

	check_after_bump(main_exit,"Main Exit Footer");					/* Write out main exit footer source.	*/

	if (start_screen)								/* Write out DISPLAY and READ source.	*/
	{
		i = 0;
		while (*disp_read[i])							/* Check address.			*/
		{
			if (i == 4)
			{
				fputs("           MOVE HEX-01          TO ROW-NUMBER.\n",outfile);
				fputs("           MOVE HEX-00          TO CURSOR-COLUMN.\n",outfile);
				fputs("           MOVE HEX-01          TO CURSOR-ROW.\n",outfile); 
				num_outlines += 3;					/* Increment .WCB line number.		*/
			}
			num_outlines++;							/* Increment .WCB line number.		*/
			fputs(disp_read[i++],outfile); 
		}
		write_screen_info();							/* Write out screen info source.	*/
	}

	if (got_submit)
	{
		check_after_bump(call_submit,"Perform Call SUBMIT");			/* Write out COBOL source.		*/
		if (gendisplay) check_after_bump(&call_submit[11],"DISPLAY error for SUBMIT");
	}
	if (got_set)	check_after_bump(call_set,"Call SET");
	if (got_extract) check_after_bump(call_extract,"Call EXTRACT");
	if (got_print)
	{
		check_after_bump(call_print,"Perform Call PRINT");
		if (gendisplay) check_after_bump(&call_print[10],"DISPLAY error for PRINT");
	}
	if (got_find)	check_after_bump(call_find,"Perform Call FIND");
	if (got_logoff)	check_after_bump(call_logoff,"Perform Call LOGOFF");
	if (got_scratch)
	{
		check_after_bump(call_scratch,"Perform Call SCRATCH");
		if (gendisplay) check_after_bump(&call_scratch[9],"DISPLAY error for SCRATCH");
	}
	if (got_rename)
	{
		check_after_bump(call_rename,"Perform Call RENAME");
		if (gendisplay) check_after_bump(&call_rename[9],"DISPLAY error for RENAME");
	}
	if (got_readfdr)
	{
		check_after_bump(call_readfdr,"Perform Call READFDR");
		if (gendisplay) check_after_bump(&call_readfdr[8],"DISPLAY error for READFDR");
	}

	fclose(outfile);								/* Close the output file.		*/
	return(0);
}

/*
**	ROUTINE:	check_after_bump()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void check_after_bump(char **value,char *text_info)					/* Write COBOL source to output file.	*/
{
	int i;

	write_log(util,'I','W',"WRITE","Writing the %s COBOL source.",text_info);
	i = 0;
	while (*value[i])								/* While still a line to output.	*/
	{
		num_outlines++;								/* Increment .WCB line number.		*/
		fputs(value[i],outfile);						/* Write to output file.		*/
		i++;
	}
}

/*
**	ROUTINE:	initialize()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	Init all of the flags and var so can begin the generation.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void initialize(int *num_cmds,int *num_var,int *scn_num,int *filler_num,int *current_row,int *num_link_var)
{
	setflag();									/* Set all "in" flags to zero.		*/
	setgot();									/* Set all "got" flags to zero.		*/
	num_lineins = 0;								/* Set to 0 lines read.			*/
	num_outlines = 0;								/* Set to 0 lines written.		*/
	lncnt_type = 1;									/* Set to display read in lines cnt.	*/
	*num_var = 0;									/* Set counter to zero.			*/
	*num_link_var = 0;								/* Set counter to zero.			*/
	*scn_num = 0;									/* No screens to start.			*/
	*filler_num = 0;								/* Keep count for filler.		*/
	*current_row = 0;
	start_screen = NULL;								/* Set start screen = 0			*/
	st_para = NULL;
	init_para(num_cmds);								/* Init the paragraph item.		*/
	strcpy(cur_para->name,"INITIALIZATION");			 		/* If cmds w/out paragraph name need it.*/
	num_sf_var = 0;									/* Init generated screen fields count.	*/
	set_string = FALSE;
	cnt_sf = 0;
}

/*
**	ROUTINE:	process_linein()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void process_linein(int *num_cmds,int *num_var,int *scn_num,int *filler_num,int *current_row,int *num_link_var)
{
	int num_par, tndx, num_val;
	char *start_ptr, *cstr;
	int num_asn, more;

	cstr = NULL;
	lcnt = 0;									/* Init the looping field indicators.	*/
	captr = (char *)0;
	num_par = 0;									/* Init number of parameters.		*/
	num_val = 0;
	start_ptr = linein;								/* Save starting pos of linein.		*/
	aptr = linein;
	pcnt = 0;									/* Set to first position in linein.	*/
	next_ptr = linein;
	while ( nexttok() )								/* While a token then process it.	*/
	{
		pcnt = aptr - linein;							/* Calculate position in linein.	*/
		if (pcnt >= WPLMAX) break;						/* Don't process past meaningful column.*/
		if (in_trace)								/* Process keywords for TRACE, if given.*/
		{
			char *tptr, temp[15];

			tndx = find_keyword(aptr,trace_keywords);			/* Seacrh for match of trace_keyword.	*/
			if (tndx >= 0 )							
			{
				tptr = temp;
				while (tststrt(aptr)) *tptr++ = *aptr++;		/* Copy keyword to temp string.		*/
				*tptr = '\0';						/* Null terminate the string.		*/
				write_log(util,'I','R',"NOTSUPPRTD","TRACE keyword %s not supported.  Line not processed.",temp);
				while (*aptr != '\0' && *aptr != LNFD) nexttok();	/* Step to end of line.			*/
				break;
			}
			else in_trace = FALSE;
		}

		tndx = find_keyword(aptr,proc_keywords);				/* Seacrh for match of proc_keyword.	*/
		if ((tndx >= 0 && !in_putparm) || (tndx >= 0 && (in_putparm && !kw_assign())) )
		{
			in_putparm = 0;							/* Set so not in PUTPARM anymore.	*/
			p_pl_kw(tndx,&num_val,&num_asn,num_cmds,scn_num,current_row,num_var); /* Found match so process it.	*/
		}
		else
		{
			more = test_para_name(num_par,num_cmds);			/* Else see if it is a paragraph name.	*/
			if (more)
			{
				tndx = find_keyword(aptr,link_keywords);		/* Search for match of link keywords.	*/
				if (tndx >= 0 && in_proc)
				{
					p_link_kw(tndx,num_link_var); 			/* Found match so process it.		*/
				}
				else if ( (*aptr == '&' || *aptr == '(') && in_proc) 	/* Else see if it is a variable.	*/
				{
					save_link_var(num_link_var);			/* Save variable in link list.		*/
				}
				else
				{							/* Search for match of declare_keywords.*/
					tndx = find_keyword(aptr,declare_keywords);
					if (tndx >= 0 && in_declare)
					{						 /* Found match so process it.		*/
						p_declare(tndx,&num_val,num_var,&num_asn);
					}
					else if (*aptr == '&' && in_declare)		 /* Save var in declare list.		*/
					{
						save_declare_var(&num_asn,num_var,&num_val,num_cmds);
					}
				}
				num_par++;						/* Increment the parameter count.	*/
			}
		}
		if (*aptr == '\0' || *aptr == LNFD) break;				/* Nothing left to process on line.	*/
		if (in_assign)
		{
			write_log(util,'I','R',"INPROC","Processing in ASSIGN.");
			p_assign(&num_asn,num_var,filler_num,current_row,&num_val,num_cmds);
		}
		if (in_run || in_print || in_submit)					/* Got a run, print, or submit command.	*/
		{
			if (in_submit && 0==strncmp(aptr,"AS ",3))			/* Set the submitted job name.		*/
			{
				aptr += 3;						/* Step past the AS symbol.		*/
				next_ptr = aptr;
				if (*aptr == ' ') nexttok();
				cstr = cur_prg->as_name;
				while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Load the job name.			*/
				*cstr = '\0';						/* Null terminate the string.		*/
				*cur_prg->as_name_type = 'S';
				next_ptr = aptr;
			}
		
			tndx = find_keyword(aptr,program_keywords); 			/* Search the table for keywords.	*/
			if (tndx >= 0)
			{								/* Found a match so process it.		*/
				p_prg_kw(tndx,num_var,&num_val);
			} 								/* Search table for run keyword.	*/
			else if (in_run && ((tndx = find_keyword(aptr,run_clause)) >= 0))
			{
				p_run_clause_kw(tndx);					/* Found match so process it.		*/
			}
			else if (in_run && in_np_putparm)				/* In no process putparm so don't parse.*/
			{
				while (*aptr != '\0' && *aptr != LNFD) nexttok();	/* Step to end of line.			*/
			}								/* Search table for print/submit keyword.*/
			else if ( (in_print && ((tndx = find_keyword(aptr,print_keyword)) >= 0)) ||
				  (in_submit && ((tndx = find_keyword(aptr,submit_keyword)) >= 0)) )
			{
				if (!in_putparm)
				{
					int caller;

					if (in_print) caller = 1;
					else caller = 2;

					p_print_submit_kw(caller,tndx);			/* Found match so process it else 	*/
				}							/*  continue keyword processing.	*/
			}
			if (in_using && *aptr != '\0' && *aptr != LNFD)			/* If in using and not end of line.	*/
			{
				save_using_item(&num_asn,num_var,filler_num,current_row,&num_val);
			}
		}
		if (in_if)
		{
			tndx = find_keyword(aptr,if_keywords);				/* Seacrh the table for keywords.	*/
			if (tndx >= 0)
			{
				p_if_kw(tndx,num_var,&num_val,filler_num,current_row,num_cmds);	/* Found a match so process it.	*/
			}
			else
			{
				tndx = find_keyword(aptr,built_in_funcs);		/* Seacrh the table for keywords.	*/
				if (tndx >= 0)
				{
					char *tptr, temp[15];

					tptr = temp;
					while (tststrt(aptr) && *aptr != '(') *tptr++ = *aptr++; /* Copy keyword to temp string.*/
					*tptr = '\0';					/* Null terminate the string.		*/
					if (tndx == 2 || tndx == 10)			/* If function &DATE or &TIME		*/
					{
						write_log(util,'I','R',"PROCESS",
								"Processing IF statement with %s function.",temp);
						upper_string(temp);
						init_if();				/* Allocate IF area.			*/
						if (tndx == 2)
						{
							strcpy(cur_if->var,"&DATE");	/* Set for local date string.		*/
						}
						else	strcpy(cur_if->var,"&TIME");	/* Set for local time string.		*/

						strcpy(cur_if->type,"P ");
					}
					else
					{
						write_log(util,'E','R',"NOTSUPPRTD",
							"Function %s not supported.  Line not processed.",temp);
						while (*aptr != '\0' && *aptr != LNFD) nexttok(); /* Step to end of line.		*/
					}
				}
				else	save_if_var(num_var,&num_val,filler_num,current_row,num_cmds);
			}
		}
		if (in_extract || in_set)
		{
			tndx = find_keyword(aptr,extract_keywords);			/* Seacrh the table for keywords.	*/
			if (tndx >= 0)
			{
				p_extract_kw(tndx);					/* Process the extract keyword.		*/
			}
			else	save_set_ext_var(num_var,&num_val,&num_asn,filler_num,current_row,num_cmds);
		}
		if (in_readfdr)
		{
			p_readfdr(num_var,&num_val);					/* Process EXTRACT->READFDR keyword.	*/
		}
		if (in_return)
		{
			char *lptr, *tptr, temp2[FLDLEN];
			register int i;

			write_log(util,'I','R',"INPROC","Processing in RETURN.");
			lptr = aptr;							/* Set local variable to work with.	*/
			for (i = 0; i < 4; i++)
			{
				*lptr = toupper(*lptr);					/* Convert to upper for match test.	*/
				lptr++;
			}
			if (!strncmp(aptr,"CODE",4))					/* If a match.				*/
			{
				aptr += 4;						/* Step over CODE clause.		*/
			}
			if (*aptr == ' ') nexttok();					/* If equation spaced out.		*/
			if (*aptr == '=') aptr++;					/* Step over the = symbol.		*/
			if (*aptr == ' ') nexttok();					/* If equation spaced out.		*/
			if (*aptr == '&' || operand(*aptr) || number(*aptr))		/* If &, operand or num.		*/
			{  								/* Check to see if variable.		*/
				init_return();
				lptr = cur_rtrn->var;					/* Get the addrs of string.		*/

				while (tststrt(aptr) && (*aptr == '&' || operand(*aptr) || number(*aptr) || letter(*aptr)))
				{
					*lptr++ = toupper(*aptr++);			/* Copy char to string.			*/
				}
				*lptr = '\0'; 						/* Null terminate the value.		*/
				if (*cur_rtrn->var == '&')
				{
					get_type_len(cur_rtrn->var,cur_rtrn->type,num_var,&num_val,'T');
				}
				else strcpy(cur_rtrn->type,"I ");			/* Set to an integer.			*/
			}
			else if (*aptr == '\'')						/* Is it a quote?			*/
			{
				lptr = aptr;
				tptr = temp2;
				while (*lptr != ' ' && *lptr != '\0' && *lptr != LNFD) *tptr++ = *lptr++;
				write_log(util,'E','R',"INVALID",
					"Invalid RETURN CODE (%s). Must be integer expression",temp2);
				nexttok();						/* Step past invalid string.		*/
			}
			else								/* Check to see if a label.		*/
			{
				int fndlbl;
				char *lptr, *tprt, temp[FLDLEN];
				paragraph_item *pptr;

				lptr = aptr;						/* Set local pointer for test of token.	*/
				tprt = temp;						/* Set ptr to temp string.		*/
				while (tststrt(lptr)) *tprt++ = toupper(*lptr++);	/* Get the current token in upper case.	*/
				*tprt = '\0';						/* Null terminate the string.		*/
				fndlbl = FALSE;
				pptr = cur_para;
				while (pptr)
				{
					if (!strcmp(temp,pptr->name))			/* If var match the current label.	*/
					{
						fndlbl = TRUE;
						break;
					}
					pptr = pptr->prev_item;
				}

				if (fndlbl)						/* If var match a label.		*/
				{
					aptr = lptr;					/* Set the ptr past the label.		*/
					init_return();
					if (pptr->rlbl == 'R')
					{
						strcpy(cur_rtrn->var,"&");		/* Load the Return code indicator.	*/
						strcat(cur_rtrn->var,pptr->name);
						strcpy(cur_rtrn->type,"IR");		/* Set type to integer return.		*/
					}
					else strcpy(cur_rtrn->var,"RETURN-CODE");
				}
				else
				{
					lptr = aptr;
					tptr = temp2;
					while (*lptr != ' ' && *lptr != '\0' && *lptr != LNFD) *tptr++ = *lptr++;
					write_log(util,'E','R',"INVALID","Invalid assignment (%s) for RETURN CODE.",temp2);
				}
			}
			if (*aptr == ' ') nexttok();					/* If equation spaced out.		*/
			if (*aptr != '[' && *aptr != '\0' && *aptr != LNFD) next_ptr = aptr-1; /* Test if comment, else set ptr.*/
		}
		if (in_rename || in_scratch)						/* Check in rename or in scratch cmds.	*/
		{
			tndx = find_keyword(aptr,rs_keywords);				/* Search the table for keywords.	*/
			p_rs_kw(tndx,num_var,&num_val,filler_num,current_row,num_cmds);
		}
		if (in_prompt || in_message)
		{
			tndx = find_keyword(aptr,screen_keywords);			/* Search for screen keywords.		*/
			if (tndx >= 0)
			{								/* Found a match so process.		*/
				p_scn_kw(tndx);
			}
			else	save_screen_item(&num_asn,num_var,&num_val,current_row);

			if (in_erase || in_alarm)
			{
				if (*aptr == '=')					/* Assign the message option.		*/
				{
					aptr++;						/* Step over the equals symbol.		*/
					if (*aptr == ' ') nexttok();			/* If equation spaced out.		*/
					if (in_erase) cstr = &cur_scn->screen_erase;
					else	cstr = &cur_scn->screen_alarm;

					if (*aptr  == 'Y' || *aptr == 'y') *cstr = 'Y';
					else if (*aptr == 'N' || *aptr == 'n')
					{
						*cstr = 'N';
						if (in_erase) write_log(util,'W','R',"ERASE",
							"Screen will be erased.  Change format for full screen I/O.");
					}
					else write_log(util,'E','R',"NOVAL","Not a valid assignment for message option.");

					if (*cstr != '\0') setscreen();			/* Reset the flags for variables.	*/
				}
				else	write_log(util,'E','R',"NOASSIGN","No assignment for message option.");
			}
			if (in_pfkey || in_currow || in_curcol)
			{
				if (*aptr == '=')					/* Need = to set prompt option.		*/
				{
					aptr++;						/* Step over the = symbol.		*/
					if (*aptr == ' ') nexttok();			/* If equation spaced out.		*/
					if (*aptr == '&')
					{
						if (in_pfkey)	    cstr =cur_scn->pf_key;
						else if (in_curcol) cstr = cur_scn->cur_col;
						else if (in_currow) cstr = cur_scn->cur_row;
											/* Save the variable name.		*/
						while (tststrt(aptr) && *aptr != ';') *cstr++ = toupper(*aptr++);
						*cstr = '\0';				/* Null terminate the string.		*/

						if (in_pfkey)
						{
							get_type_len(cur_scn->pf_key,cur_scn->pfk_type,num_var,&num_val,'T');
						}
						else if (in_curcol) 
						{
							get_type_len(cur_scn->cur_col,cur_scn->cc_type,num_var,&num_val,'T');
						}
						else if (in_currow)
						{
							get_type_len(cur_scn->cur_row,cur_scn->cr_type,num_var,&num_val,'T');
						}
					}
					else write_log(util,'E','R',"NOVAR","No variable for assignment.");
				}
				else	write_log(util,'E','R',"NOASSIGN","No assignment for PROMPT option");
				setscreen();						/* Reset the flags for variables.	*/
			}
			if (in_row)
			{
				char * cp, temp[FLDLEN];

				cp = temp;
				while (tststrt(aptr)) *cp++ = *aptr++;			/* Copy row value in temp string.	*/
				*cp = '\0';						/* Null terminate the string.		*/
				*current_row = atoi(temp);				/* Save the row assignmnet.		*/
				setscreen();						/* Re-set the screen flags bacj to 0.	*/
			}
		}
		if (in_putparm)								/* Process keywords or pfkey, if given.	*/
		{
			if (*aptr == ' ') nexttok();					/* Step to next token.			*/
			if (!tststrt(aptr));						/* If nothing to process, do nothing.	*/
			else if (*aptr == '&' || operand(*aptr) || number(*aptr))	/* Is the pfkey.			*/
			{
				if (number(*aptr))					/* Set up for value + 1 so assigns the	*/
				{							/*  index into the table.		*/
					char tpfk[FLDLEN];
					int ipf;

					cstr =tpfk;
					while (tststrt(aptr)) *cstr++ = *aptr++;	/* Copy value.				*/
					*cstr = '\0';					/* Null terminate the string.		*/
					ipf = atoi(tpfk) + 1;
					sprintf(cur_pp->pfkey,"%d",ipf);		/* Assign the pfkey string value.	*/
				}
				else
				{
					cstr = cur_pp->pfkey;
					while (tststrt(aptr)) *cstr++ = toupper(*aptr++); /* Copy variable or expresion.	*/
					strcat(cstr,"+1");
					cstr += 2;
					*cstr = '\0';					/* Null terminate the string.		*/
				}
				in_putparm = 0;						/* Set flag to stop processing putparm.	*/
			}
			else if (*aptr == '[');
			else p_putparm_keys(num_var,&num_val);				/* Else is a keyword.			*/
		}
		if (*aptr == '[') skip_comment();					/* Don't process the comment.		*/
	}
}

/*
**	ROUTINE:	write_str_int_type()
**
**	FUNCTION:	Write out the working storage source depending on the variable type.
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void write_str_int_type()
{
	int i, len;
	char *cstr;

	write_log(util,'I','W',"WRITE","Writing Working storage for %c - %s.",cur_decl->type[1],cur_decl->field1);

	strcpy(cobline,"       01  ");
	concat_var_prefix(cur_decl->type[1]);						/* Concatenate the variable prefix.	*/
	if (*cur_decl->field1 == '&') strcat(cobline,&cur_decl->field1[1]);		/* Load the name.			*/
	else	strcat(cobline,cur_decl->field1);
	len = strlen(cobline);								/* Get the current length of buffer.	*/
	cstr = &cobline[len];								/* Point to end of curr output buffer.	*/
	if (*cur_decl->type != 'I')
	{
		for (i = len; i < 31; i++) *cstr++ = ' ';				/* Space out to 40 char to match code.	*/
	}
	*cstr = '\0';									/* Make sure has NULL at end of buffer.	*/
	if (*cur_decl->type == 'S')
	{
		int spfl;

		strcat(cobline,"           PIC X");
		strcat(cobline,cur_decl->length);					/* load the (XXX).			*/
		strcat(cobline," VALUE ");
		spfl = TRUE;								/* Assume is SPACES.			*/
		cstr = cur_decl->value;
		while (*cstr)								/* Test string value to see if the	*/
		{									/* SPACES keyword fits initialization.	*/
			if (*cstr != ' ' && *cstr != '\0')
			{
				spfl = FALSE;
				break;
			}
			cstr++;
		}
		if (spfl) strcat(cobline,"SPACES");
		else
		{
			int cpos;

			end_prt_line(0);						/* Add end of line stuff and write line.*/

			strcpy(cobline,"           ");
			strcat(cobline,"\"");						/* load the quote.			*/
			cpos = 12;
			if ((int)(cpos + strlen(cur_decl->value)) >= (WPLMAX+1))	/* If text goes past valid column 	*/
			{								/* need to generate a continuation	*/
				char temp[65];						/* line.				*/

				cstr = cur_decl->value;
				strncpy(temp,cstr,60);
				temp[60] = '\0';
				strcat(cobline,temp);					/* Load part of the value of the field.	*/
				end_prt_line(0);					/* Add end of line stuff and write line.*/

				strcpy(cobline,"      -    \"");			/* Load continuation indicator and ".	*/
				cstr += 60;						/* Set ptr to start for next line.	*/
				strcat(cobline,cstr);
			}
			else strcat(cobline,cur_decl->value);				/* Load the value of the field.		*/

			strcat(cobline,"\"");						/* load quote.				*/
		}
	}
	if (*cur_decl->type == 'I')
	{
		end_prt_line(1);							/* Add end of line stuff and write line.*/
		strcpy(cobline,"           03 FILLER                      BINARY VALUE 0");
		end_prt_line(1);
		strcpy(cobline,"           03 ");
		if (cur_decl->type[1] == 'R')
		{
			concat_var_prefix(cur_decl->type[1]);				/* Concatenate the variable prefix.	*/
			if (*cur_decl->field1 == '&') strcat(cobline,&cur_decl->field1[1]);/* Load the name.			*/
			else	strcat(cobline,cur_decl->field1);
			strcat(cobline,"-2");
			len = strlen(cobline);						/* Get the current length of buffer.	*/
			cstr = &cobline[len];						/* Point to end of curr output buffer.	*/
			for (i = len; i < 42; i++) *cstr++ = ' ';			/* Space out to match code.		*/
			*cstr = '\0';							/* Make sure has NULL at end of buffer.	*/
		}
		else	strcat(cobline,"FILLER                      ");

		strcat(cobline,"BINARY VALUE ");
		if (*cur_decl->value) strcat(cobline,cur_decl->value);			/* Load the initial clause		*/
		else strcat(cobline,"0");						/* Init to 0.				*/
	}
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

/*
**	ROUTINE:	write_link_type()
**
**	FUNCTION:	Write out working storage source depending on the var type.
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void write_link_type()
{
	int i, len;
	char *cstr;

	hld_link = cur_link;								/* Make sure you hold the pointer.	*/
	cur_link = stlinklst;
	while (cur_link)
	{
		write_log(util,'I','W',"WRITE","Writing working storage for link type %c - %s.",cur_link->type[1],cur_link->field1);

		strcpy(cobline,"       01  ");
		concat_var_prefix(cur_link->type[1]);					/* Concatenate the variable prefix.	*/
		if (*cur_link->field1 == '&') strcat(cobline,&cur_link->field1[1]);	/* Load the name.			*/
		else	strcat(cobline,cur_link->field1);
		len = strlen(cobline);							/* Get the current length of buffer.	*/
		cstr = &cobline[len];							/* Point to end of curr output buffer.	*/
		for (i = len; i < 29; i++) *cstr++ = ' ';				/* Space out to 40 char to match code.	*/
		*cstr = '\0';								/* Make sure has NULL at end of buffer.	*/

		if (*cur_link->type == 'S')
		{
			strcat(cobline,"           PIC X");				/* load the picture clause.		*/
			strcat(cobline,cur_link->length);				/* load the (XXX).			*/
			end_prt_line(1);						/* Add end of line stuff and write line.*/
		}
		else if (*cur_decl->type == 'I')
		{
			end_prt_line(1);						/* Add end of line stuff and write line.*/
			strcat(cobline,"           03 FILLER           BINARY VALUE 0.");
			end_prt_line(1);
			strcat(cobline,"           03 FILLER           BINARY VALUE 0");
			end_prt_line(1);						/* Add end of line stuff and write line.*/
		}
		else	write_log(util,'E','W',"ERRTYPE",
				"Invalid type %c for link variable %s.",*cur_link->type,cur_link->field1);
		cur_link= cur_link->next_item;						/* Set to the previous structure.	*/
	}
	cur_link = hld_link;								/* Put the pointer back if multiple.	*/
}

/*
**	ROUTINE:	write_proc_link_src()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void write_proc_link_src()
{
	write_log(util,'I','W',"WRITE","Writing procedure division USING COBOL source beginning.");

	num_outlines++;									/* Increment .WCB line number.		*/
	fputs(proc_div[0],outfile);							/* Write the output for header.		*/ 

	num_outlines++;									/* Increment .WCB line number.		*/
	fputs(proc_div[11],outfile);							/* Write the output for header.		*/ 

	hld_link = cur_link;								/* Make sure you hold the pointer.	*/
	cur_link = stlinklst;
	while (cur_link)								/* If last item address are eqaul.	*/
	{
		indent_line();								/* Add the correct indentation.		*/
		concat_var_prefix(cur_link->type[1]);					/* Concatenate the variable prefix.	*/
		if (*cur_link->field1 == '&') strcat(cobline,&cur_link->field1[1]);	/* Load the name.			*/
		else	strcat(cobline,cur_link->field1);
		if (!cur_link->next_item) end_prt_line(1);				/* Check to see if last item.		*/
		else	end_prt_line(2);						/* Add end of line stuff and write line.*/

		cur_link = cur_link->next_item;
	}
	cur_link = hld_link;								/* Put the pointer bake if multiple.	*/
	check_after_bump(&proc_div[2],"Procedure Division Header end");			/* Write out rest of proc. div header .	*/
}

static void write_screen_info()								/* Write out the COBOL source for	*/
{											/*  the screen information.		*/
	char scrn_num[FLDLEN];

	write_log(util,'I','W',"WRITE","Writing screen information.");
	cur_scn = start_screen;								/* set the original screen.		*/
	while (cur_scn)
	{
		memset(scrn_num,'\0',FLDLEN);						/* Clean the field.			*/
		sprintf(scrn_num,"%d",cur_scn->num_screen);				/* load the screen number.		*/
		strcpy(cobline,"       A-DISPLAY-AND-READ-");				/* Paragraph name for display.		*/
		strcat(cobline,scrn_num);						/* Load the screen number.		*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		fputs("           MOVE HEX-01          TO ROW-NUMBER.\n",outfile);
		fputs("           MOVE HEX-00          TO CURSOR-COLUMN.\n",outfile);
		fputs("           MOVE HEX-01          TO CURSOR-ROW.\n",outfile);
 		num_outlines += 3;							/* Increment .WCB line number.		*/

		strcpy(cobline,"           MOVE ORDERAREA    TO ORDER-AREA OF DISPLAY-REC-");
		strcat(cobline,scrn_num);						/* Load the screen number.		*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		strcpy(cobline,"           DISPLAY AND READ DISPLAY-REC-");
		strcat(cobline,scrn_num);						/* Load the screen number.		*/
		strcat(cobline," ON CRT");
		end_prt_line(0);							/* Add end of line stuff and write line.*/

		fputs("                 PFKEY 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16\n",outfile);
		fputs("                 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32.\n",outfile);
		num_outlines += 2;							/* Increment .WCB line number.		*/

		if (*cur_scn->pf_key == '&' || *cur_scn->cur_row == '&' || *cur_scn->cur_col == '&')
		{
			char *cstr, ctyp;
			int cnt, cl, cfl;

			for (cnt = 0; cnt < 3; cnt++)
			{
				cstr = NULL;
				ctyp = '\0';

				cfl = 0;
				cl = 24 + cnt;
				if (cnt == 0 && *cur_scn->pf_key == '&')
				{
					cstr = cur_scn->pf_key;
					ctyp = cur_scn->pfk_type[1];
					cfl = 1;
				}
				else if (cnt == 1 && *cur_scn->cur_row == '&')
				{
					cstr = cur_scn->cur_row;
					ctyp = cur_scn->cr_type[1];
					cfl = 1;
				}
				else if (cnt == 2 && *cur_scn->cur_col == '&')
				{
					cstr = cur_scn->cur_col;
					ctyp = cur_scn->cc_type[1];
					cfl = 1;
				}

				if (cfl)
				{
					indent_line();					/* Add the correct indentation.		*/
					strcat(cobline,"MOVE ");

					if (cnt == 0) strcat(cobline," PF-KEY");
					else if (cnt == 1) strcat(cobline," MOD-ROW");
					else if (cnt == 2) strcat(cobline," MOD-COL");

					strcat(cobline," TO ");
					
					concat_var_prefix(ctyp);			/* Concatenate the variable prefix.	*/
					if (*cstr == '&') cstr++;
					strcat(cobline,cstr);				/* Load the variable NAME.		*/

					end_prt_line(1);				/* Add end of line stuff and write line.*/
				}
			}
		}
		fputs("      *\n",outfile);			 			/* Write the first line.		*/
		num_outlines++;								/* Increment .WCB line number.		*/

		strcpy(cobline,"       A-INIT-FAC-");					/* Paragraph name for init-fac.		*/
		strcat(cobline,scrn_num);						/* Load the screen number.		*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/

		cur_scn_fld = cur_scn->variable_area; 					/* Load the variable area.		*/
		while (cur_scn_fld)
		{
			char tmp_fac[10];						/* Hold generated FAC assignment.	*/

			check_write_string();						/* Check/write STRING cmd.		*/
			memset(tmp_fac,'\0',10);					/* Set FAC field to NULLS.		*/
			if (cur_scn_fld->fac_msk & FMBLINK) strcpy(tmp_fac,"BLI");
			else if (cur_scn_fld->fac_msk & FMBLANK) strcpy(tmp_fac,"BLA");
			else if (cur_scn_fld->fac_msk & FMBRIGHT) strcpy(tmp_fac,"BR");
			else strcpy(tmp_fac,"DIM");

			if ((cur_scn_fld->fac_msk & FMNUMERIC) || (cur_scn_fld->fac_msk & FMUPLOW) ||
				(cur_scn_fld->fac_msk & FMUPPER))
			{ 
				strcat(tmp_fac,"MOD");
			}
			else	strcat(tmp_fac,"PRO");

			if (cur_scn_fld->fac_msk & FMNUMERIC) strcat(tmp_fac,"NUM");
			if (cur_scn_fld->fac_msk & FMUPPER) strcat(tmp_fac,"UPR");
			if (cur_scn_fld->fac_msk & FMLINE) strcat(tmp_fac,"UL");
			if (cur_scn_fld->fac_msk & FMTAB)				/* No other attributes available when	*/
			{								/*  TAB is set.				*/
				strcpy(tmp_fac,"DIMPRONUM");
			}

			if (0 != strcmp(tmp_fac,"DIMPRO"))
			{
				indent_line();						/* Add the correct indentation.		*/
				strcat(cobline,"MOVE ");
				strcat(cobline,tmp_fac);
				strcat(cobline," TO FAC OF ");
				strcat(cobline,cur_scn_fld->map_fld); 			/* Load the screen variable.		*/
				end_prt_line(1);					/* Add end of line stuff and write line.*/
			}

			cur_scn_fld = cur_scn_fld->next_item;
		}
		fputs("      *\n",outfile);
		num_outlines++;								/* Increment .WCB line number.		*/

		cur_scn = cur_scn->next_item; 						/* Point to next.			*/
	}
}

static void write_environ_vars()							/* Write out all ACCEPT vars FROM	*/
{											/*  ENVIRONMENT for undeclared vars.	*/
	int uf, csize;									/* Flag if need unix_code flags.	*/

	hld_table = cur_decl;								/* Make sure you hold the pointer.	*/
	uf = FALSE;									/* Assume you don't have any.		*/
	while (cur_decl)
	{
		if (cur_decl->type[1] == 'E')
		{
			if (!uf)							/* Write the opening directive.		*/
			{
				strcpy(cobline,"      *$ACU_CODE");
				end_prt_line(0);					/* Add end of line stuff and write line.*/
			}
			uf = TRUE;

			write_log(util,'I','W',"WRITE","Writing ACCEPT %s FROM ENVIRONMENT.",cur_decl->field1);
			strcpy(cobline,"037200*    ACCEPT ");
			concat_var_prefix('E');						/* Load the External Global indicator.	*/
			if (*cur_decl->field1 == '&') strcat(cobline,&cur_decl->field1[1]);
			else	strcat(cobline,cur_decl->field1);			/* Load the name.			*/
			strcat(cobline," FROM ENVIRONMENT");
			if ( (csize = strlen(cobline)) >= max_cobline)			/* If buffer too long.			*/
			{
				end_prt_line(0);					/* Add end of line stuff and write line.*/
				strcpy(cobline,"037200*         ");			/* Add the correct indentation.		*/
			}

			strcat(cobline," \"");
			if (*cur_decl->field1 == '&') strcat(cobline,&cur_decl->field1[1]);
			else	strcat(cobline,cur_decl->field1);			/* Load the name.			*/
			strcat(cobline,"\"");
			end_prt_line(1);						/* Add end of line stuff and write line.*/
		}
		cur_decl = cur_decl->prev_item;						/* Set to the previous structure.	*/
	}
	cur_decl = hld_table;
	if (uf)										/* Write the closing directive.		*/
	{
		strcpy(cobline,"      *$ACU_END");
		end_prt_line(0);							/* Add end of line stuff and write line.*/
	}
}

static int kw_assign()									/* Return TRUE if keyword assignment	*/
{											/* found for PUTPARM else return FALSE.	*/
	char *laptr;

	laptr = aptr;									/* Set the local ptr to position.	*/
	while (tststrt(laptr))
	{
		if (*laptr == '=') return(TRUE);					/* Test for the = symbol.		*/
		laptr++;
	}
	while (*laptr == ' ') laptr++;							/* Step to check if spaced equation.	*/
	if (*laptr == '=') return(TRUE);
	else return(FALSE);
}

static void skip_comment()								/* Don't process the comment.		*/
{
	int cnt;

	cnt = 0;
	while (*aptr != ']' || cnt != 0)						/* Step over the scope/comment stuff.	*/
	{
		if (*aptr == '\0' || *aptr == LNFD)
		{
			get_next_line();
		}
		else
		{
			if (*aptr == '[') cnt++;
			aptr++;
			if (*aptr == ']') cnt--;
		}
	}
	aptr++;										/* Step over the closing ].		*/
	next_ptr = aptr;
}


static void p_putparm_keys(int *num_var,int *num_val)					/* Process the putparm keywords.	*/
{
	char *cstr, len[10], *lptr;
	char *hptr, *tptr, temp[FLDLEN];
	char tlbl[FLDLEN];
	int full_br, is_string;

	full_br = FALSE;								/* Assume is not full backwards ref.	*/
	if ( is_backref() )								/* Is backwards reference.		*/
	{

		full_br = TRUE;								/* Set so uses full back. ref.		*/
		cur_pp->br_flag = 1;							/* Set putparm uses backwards ref.	*/
		aptr++;									/* Step over open paren.		*/
		cstr = tlbl;								/* Copy the putparm label.		*/
		while (tststrt(aptr) && *aptr != ')')
		{
			if (*aptr == '.')
			{
				full_br = FALSE;					/* Set so only name is back. ref'ed.	*/
				*aptr = '-';						/* Change . to -			*/
			}
			*cstr++ = toupper(*aptr++); 					/* Copy the keyword value.		*/
		}
		*cstr = '\0';								/* Null terminate the string.		*/
		aptr++;									/* Step over the closing paren.		*/
	}
	else if (*aptr == '(')	write_log(util,'E','R',"NOTEXPECT","Not expecting a ( symbol.  Notify WISP Support");

	if (full_br)									/* Set the file, lib and volume params.	*/
	{
		init_ppkw();
		cur_pp_key->br_flag = 1;						/* Set to indicate field uses back ref.	*/
		strcpy(cur_pp_key->keywrd,"FILE");
		strcpy(cur_pp_key->val,tlbl);
		strcat(cur_pp_key->val,"-FILE");					/* Set the file field.			*/
		cur_pp_key->len = 8;
		get_type_len(cur_pp_key->val,cur_pp_key->type,num_var,num_val,'A');

		init_ppkw();								/* Get memory for values.		*/
		cur_pp_key->br_flag = 1;						/* Set to indicate field uses back ref.	*/
		strcpy(cur_pp_key->keywrd,"LIBRARY");
		strcpy(cur_pp_key->val,tlbl);
		strcat(cur_pp_key->val,"-LIBRARY");					/* Set the file field.			*/
		cur_pp_key->len = 8;
		get_type_len(cur_pp_key->val,cur_pp_key->type,num_var,num_val,'A');

		init_ppkw();								/* Get memory for values.		*/
		cur_pp_key->br_flag = 1;						/* Set to indicate field uses back ref.	*/
		strcpy(cur_pp_key->keywrd,"VOLUME");
		strcpy(cur_pp_key->val,tlbl);
		strcat(cur_pp_key->val,"-VOLUME");					/* Set the file field.			*/
		cur_pp_key->len = 6;
		get_type_len(cur_pp_key->val,cur_pp_key->type,num_var,num_val,'A');

		return;
	}


	lptr = aptr;									/* Set local pointer for test of token.	*/
	tptr = temp;									/* Set ptr to temp string.		*/
	while (tststrt(lptr) && *lptr != '=') *tptr++ = toupper(*lptr++); 		/* Get token in upper case.		*/
	*tptr = '\0';									/* Null terminate the string.		*/

	if (*aptr != '\'')								/* Test if concatenation of prev value.	*/
	{
		init_ppkw();								/* Get memory for values.		*/
		cstr = cur_pp_key->keywrd;
		while (tststrt(aptr) && *aptr != '=') *cstr++ = toupper(*aptr++);	/* Copy keyword.			*/
		*cstr = '\0';								/* Null terminate the string.		*/
		if (*aptr != '=') nexttok();
		aptr++;									/* Step past the = symbol.		*/
		if (!tststrt(aptr)) nexttok();
		cstr = cur_pp_key->val;
		is_string = FALSE;							/* Assume is not a string field.	*/
		if ( is_backref() )
		{
			cur_pp->br_flag = 1;						/* Set to indicate using backwards ref.	*/
			cur_pp_key->br_flag = 1;					/* Set to indicate field uses back ref.	*/
			aptr++;								/* Step over open paren.		*/
			while (tststrt(aptr) && *aptr != ')')
			{
				if (*aptr == '.') *aptr = '-';				/* Replace the . with dash.		*/
				else if (*aptr == '#') *aptr = '-';			/* Replace the # with dash.		*/
				else if (*aptr == '_') *aptr = '-';			/* Replace the underscore with dash.	*/
				*cstr++ = toupper(*aptr++);			 	/* Copy the keyword value.		*/
			}
			aptr++;								/* Step over the closing paren.		*/
		}
		else if (*aptr == '\'')							/* Test if a quoted string.		*/
		{                                              
			aptr++;								/* Step past the open quote.		*/
			while (*aptr != '\'')
			{
				if (*aptr == '"') *aptr = '\'';				/* Change double quotes back to single	*/
				*cstr++ = toupper(*aptr++); 				/* Copy the keyword value.		*/
			}
			aptr++;								/* Step past the closing quote.		*/
			next_ptr = aptr;						/* Set ptr so nexttok() processes ok.	*/
			if (*cur_pp_key->val == '&') is_string = TRUE;
		}
		else
		{
			if (*aptr == '(') aptr++;					/* Step over the open parenthesis.	*/
			while (tststrt(aptr) && *aptr != '(' && *aptr != ')' )
			{
				if (*aptr == '.') *aptr = '-';				/* Replace the . with dash.		*/
				else if (*aptr == '#')
				{							/* If not FILE or LIBRARY keyword,	*/
					if ( strcmp(cur_pp_key->keywrd,"LIBRARY") && strcmp(cur_pp_key->keywrd,"FILE") )
					{
						*aptr = '-'; 				/* Replace the # with dash.		*/
					}
				}
				else if (*aptr == '_') *aptr = '-';			/* Replace the underscore with dash.	*/
				*cstr++ = toupper(*aptr++);				/* Copy the keyword value.		*/
			}
			if (*aptr == ')') aptr++;					/* Step over the closing paren.		*/
		}
		*cstr = '\0';								/* Null terminate the string.		*/

		if (*aptr == '(')							/* Is a subscripted field.		*/
		{
			char temp[5];
			int len;

			write_log(util,'I','W',"SUBSCRIPT","Processing subscript variable (%s).",cur_pp_key->val);
			cur_prg->sub_flag = 1;						/* Get the type of the variable.	*/
			get_type_len(cur_pp_key->val,cur_pp_key->type,num_var,num_val,'T');
			strcat(cur_pp_key->val,"-");
			aptr++;								/* Step over open paren.		*/
			cstr = temp;
			while (*aptr != ',') *cstr++ = *aptr++;				/* Copy start position as variable name.*/
			*cstr = '\0';							/* Null terminate the temp string.	*/
			strcat(cur_pp_key->val,temp);
			strcat(cur_pp_key->val,"-");
			aptr++;								/* Step over open comma.		*/
			cstr = temp;
			while (*aptr != ')') *cstr++ = *aptr++;				/* Copy length as variable name.	*/
			*cstr = '\0';							/* Null terminate the temp string.	*/
			aptr++;								/* Step over closing paren.		*/
			strcat(cur_pp_key->val,temp);
			len = atoi(temp);						/* Convert the length to integer.	*/
			cur_pp_key->len = len;
			cur_pp_key->sub_flag = 1;
			get_type_len(cur_pp_key->val,cur_pp_key->type,num_var,num_val,'C');
			next_ptr = aptr;						/* Set so processes correctly.		*/
		}
		else if (is_string)							/* Have & symbol in a string so set	*/
		{									/* fields accordingly.			*/
			strcpy(cur_pp_key->type,"SS");					/* Set the type to a string.		*/
			cur_pp_key->len = strlen(cur_pp_key->val);			/* Get length of string.		*/
		}
		else if (*cur_pp_key->val == '&')
		{									/* Get the type and size of variable.	*/
			get_type_len(cur_pp_key->val,cur_pp_key->type,num_var,num_val,'T');
			get_type_len(cur_pp_key->val,len,num_var,num_val,'S');
			cur_pp_key->len = len_to_int(len);				/* Convert length to integer.		*/
		}
		else if (cur_pp_key->br_flag == 1)
		{									/* Add the field to DECLARE list var.	*/
			get_type_len(cur_pp_key->val,cur_pp_key->type,num_var,num_val,'A');
			get_type_len(cur_pp_key->val,len,num_var,num_val,'S');
			cur_pp_key->len = len_to_int(len);				/* Convert length to integer.		*/
		}
		else
		{
			strcpy(cur_pp_key->type,"S ");					/* Set the type to a string.		*/
			cur_pp_key->len = strlen(cur_pp_key->val);			/* Get length of string.		*/
		}
	}

	hptr = aptr;									/* Set ptr to where we are.		*/
	if (!tststrt(aptr)) nexttok();							/* Step to next token.			*/

	if (*aptr == '[');
	else if (*aptr != '&' && *aptr != '\'')						/* Test if is concatenation.		*/
	{
		aptr = hptr;								/* Set the pts so will be correct for	*/
		next_ptr = aptr;							/*  the next nettok() call.		*/
		if (*aptr == ',')
		{
			nexttok();
			aptr = aptr - 1;
			next_ptr = aptr;
		}
	}					
	else
	{										/* YES, is concatenation so set field.	*/
		if (*aptr == '\'') aptr++;						/* Step past the open quote.		*/
		cstr = cur_pp_key->val2;			 			/* Copy the concat keyword value.	*/
		while (*aptr != '\'' && *aptr != LNFD && *aptr != '\0') *cstr++ = toupper(*aptr++);
		*cstr = '\0';								/* Null terminate the string.		*/
		if (*cur_pp_key->val2 == '&')
		{									/* Get the type and size of variable.	*/
			get_type_len(cur_pp_key->val2,cur_pp_key->type2,num_var,num_val,'T');
			get_type_len(cur_pp_key->val2,len,num_var,num_val,'S');
			cur_pp_key->len2 = len_to_int(len);				/* Convert length to integer.		*/
		}
		else cur_pp_key->len2 = strlen(cur_pp_key->val2);			/* Get length of string.		*/
		if (*aptr == '\'') aptr++;						/* Step past the closing quote.		*/
		next_ptr = aptr;
	}
}

static void check_write_string()							/* Check if need to write the STRING 	*/
{											/*  before displaying screen.		*/
	if (!cur_scn_fld->str_params) return;						/* Are no STRING parameters.		*/

	indent_line();									/* Add the correct indentation.		*/
	strcat(cobline,"STRING ");
	cur_str_param = cur_scn_fld->str_params;					/* Set pointer to list of fields.	*/

	while (cur_str_param)
	{
		if (*cur_str_param->field1 == '&')					/* Do I need to get rid of &.		*/
		{
			concat_var_prefix(' ');						/* Concatenate the var prefix.		*/
			strcat(cobline,&cur_str_param->field1[1]);
		}
		else
		{
			strcat(cobline,"\"");
			strcat(cobline,cur_str_param->field1);
			strcat(cobline,"\"");
		}

		strcat(cobline," DELIMITED BY SIZE");
		if (cur_str_param->next_item) strcat(cobline,", ");
		end_prt_line(0);							/* Add end of line stuff and write line.*/
		indent_line();
		strcat(cobline,"       ");

		cur_str_param = cur_str_param->next_item;
	}

	strcat(cobline,"INTO ");
	concat_var_prefix(cur_scn_fld->screen_fld[1]);					/* Concatenate the var prefix.		*/
	strcat(cobline,&cur_scn_fld->screen_fld[1]);
	end_prt_line(1);								/* Add end of line stuff and write line.*/
}

/*
**	History:
**	$Log: ptdoit.c,v $
**	Revision 1.13  2010/01/10 16:04:27  gsl
**	Shell Stream
**	
**	Revision 1.12  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.10  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.9  1997/04/21 15:06:17  scass
**	Corrected copyright.
**	
**	Revision 1.8  1996-12-12 13:36:36-05  gsl
**
**	Revision 1.7  1996-09-12 16:15:20-07  gsl
**	All the calls to get_type_len() were passing &num_val as the 4th arg
**	when they should have been passing num_val (int* vs int**)
**	Fixed prototypes
**
**	Revision 1.6  1995-10-11 00:17:00-07  scass
**	Added prototyping.
**
**
**
*/
