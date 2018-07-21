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

#define EXT extern

/* PTPDIV.C	*/

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"
#include "pgcblsrc.h"
#include "pgkeyw.h"

EXT char out_fname[STRBUFF];								/* Output file name for COBOL program.	*/
char br_label[20];									/* Backwards reference label.		*/
static void add_decl(char* keyword_lbl, int* num_val,int* num_var);			/* If have a label then declare		*/

int p_pl_kw(int  ndx,
	    int* num_val,
	    int* num_asn,
	    int* num_cmds,
	    int* scn_num,
	    int* current_row,
	    int* num_var)								/* Process the WPL keywords.		*/
{
	char *cstr, proc_id[81];							/* The procedure id of this program	*/
	register int i;
	int ret_lbl;

	if (in_label) ret_lbl = TRUE;
	else ret_lbl = FALSE;

	switch (ndx)
	{
		case ASSIGN:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing ASSIGN keyword.");
			setflag();							/* Turn flags off.			*/
			*num_asn = 0;							/* Count number of variables in field.	*/
			in_assign = 1;							/* In the assign clause.		*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"ASSIGN");				/* Initialize the command line.		*/
			nexttok();
			break;	
		}
		case CALL:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing CALL keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"PERFORM");		 		/* Initialize the command line.		*/
			nexttok();							/* Gets perform program name.		*/
			cstr = cur_cmd->call_para;
			if (tststrt(aptr))
			{
				while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Load the paragraph name.		*/
				*cstr = '\0';						/* Null terminate the string.		*/
			}
			else	write_log("PROCTRAN",'E','R',"NONAME","No Paragraph name supplied.");
			break;	
		}
		case DECLARE:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing DECLARE keyword.");
			setflag();							/* Turn off flags.			*/
			in_declare = 1;							/* I'm in a declare statement.		*/
			*num_val = 0;							/* Count back for value.		*/
			break;	
		}
		case END:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing END keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"-END");				/* Initialize the command line.		*/
			cur_cmd->end_cmd = (char *) 1;					/* Turn end switch on.			*/
			break;
		}
		case EXTRACT:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing EXTRACT keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			if (( (i=strpos(aptr,"SET")) >= 0 && (i=strpos(aptr,"TYPE")) >= 0 && (i=strpos(aptr,"OF")) >= 0 ) ||
			    ( (i=strpos(aptr,"set")) >= 0 && (i=strpos(aptr,"type")) >= 0 && (i=strpos(aptr,"of")) >= 0  ))
			{
				write_log("PROCTRAN",'W','R',"NOTSUPPRTD","SET TYPE OF not supported.  Line not processed.");
				while (*aptr != '\0' && *aptr != LNFD) nexttok();	/* Step to end of line.			*/
			}
			else if (( (i=strpos(aptr,"RECORDS"))>=0 && (i=strpos(aptr,"USED"))>=0 && (i=strpos(aptr,"BY"))>=0 )
			  || ( (i=strpos(aptr,"records"))>=0 && (i=strpos(aptr,"used"))>=0 && (i=strpos(aptr,"by"))>=0 )
			  || ( (i=strpos(aptr,"BLOCKS"))>=0 && (i=strpos(aptr,"ALLOCATED"))>=0 && (i=strpos(aptr,"FOR"))>=0 )
			  || ( (i=strpos(aptr,"blocks"))>=0 && (i=strpos(aptr,"allocated"))>=0 && (i=strpos(aptr,"for"))>=0 ))
			{
				in_readfdr = 1;
				got_readfdr = 1;
				strcpy(cur_cmd->command,"READFDR");			/* Initialize the command line.		*/
			}
			else
			{
				in_extract = 1;						/* Got an extract statement.		*/
				got_extract = 1;					/* set got call to extract.		*/
				strcpy(cur_cmd->command,"EXTRACT");			/* Initialize the command line.		*/
			}
			nexttok();
			break;	
		}
		case GOTO:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing GOTO keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"GO TO");				/* Initialize the command line.		*/
			nexttok();							/* Gets goto name.			*/
			if (tststrt(aptr))
			{
				cstr = cur_cmd->goto_para;
				while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Load the paragraph name.		*/
				*cstr = '\0';						/* Null terminate the string.		*/
				if (*cur_cmd->goto_para == '&')
				{
					get_type_len(cur_cmd->goto_para,cur_cmd->gotop_type,num_var,num_val,'T');
				}
				else strcpy(cur_cmd->gotop_type,"S "); 
			}
			else	write_log("PROCTRAN",'E','R',"NONAME","No Paragraph name supplied.");
			break;	
		}
		case IF:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing IF keyword.");
			setflag();							/* Turn flags off.			*/
			in_if = 1;							/* Got an is statement.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"IF");					/* Initialize the command line.		*/
			nexttok();							/* Set ptr to next token.		*/
			break;	
		}
		case LOGOFF:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing LOGOFF keyword.");
			setflag();							/* Turn flags off.			*/
			in_logoff = 1;							/* Got an logoff statement.		*/
			got_logoff = 1;							/* set got call to logoff.		*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"LOGOFF");				/* Initialize the command line.		*/
			cur_cmd->logoff_cmd = (char *) 1;
			break;	
		}
		case MESSAGE:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing MESSAGE keyword.");
			setflag();							/* Turn flags off.			*/
			in_message = 1;							/* In the message clause.		*/
			got_message = 1;						/* Got the message clause.		*/
			*current_row = 1;						/* Set it to the first 1.		*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			init_screen(scn_num);						/* Create screen area.			*/
			strcpy(cur_cmd->command,"MESSAGE");				/* Initialize the command line.		*/
			nexttok();
			break;	
		}
		case PRINT:
		{
			int full_br, *cbr;

			write_log("PROCTRAN",'I','W',"PROCESS","Processing PRINT keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"PRINT");				/* Initialize the command line.		*/
			in_print = 1;							/* In A print command lets doit.	*/
			got_print = 1;							/* Found a print command.		*/
			cur_cmd->program_area = (struct program_item *)init_prg();	/* Create a program map.		*/
			aptr += 5;							/* Step over the PRINT keyword.		*/
			while (*aptr == ' ') nexttok();
			full_br = FALSE;						/* Assume is not backwards referenced.	*/
			if (tststrt(aptr))
			{
				cstr = cur_prg->name;
				cbr = &cur_prg->name_br;				/* Set to indicate field uses back ref.	*/
				if ( is_backref() )					/* Is it backwards referenced?		*/
				{							/* Yes.					*/
					full_br = TRUE;					/* Set so uses full back. ref.		*/
					cur_prg->br_flag = 1;
					*cbr = 1;					/* Set to indicate field uses back ref.	*/
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
				}
				else
				{
					if (*aptr == '(') aptr++;			/* Step over the open parenthesis.	*/
					while (tststrt(aptr) && *aptr != ')') *cstr++ = toupper(*aptr++); /* Load the file name.*/
					if (*aptr == ')') aptr++;			/* Step over the closing paren.		*/
				}
				*cstr = '\0';						/* Null terminate the string.		*/
				cur_prg->func = 'P';					/* Program func = Print.		*/
				if (*cur_prg->name == '&') get_type_len(cur_prg->name,cur_prg->name_type,num_var,num_val,'T');
				else if (*cbr && !full_br)				/* If have backwards referenced field.	*/
				{
					get_type_len(cur_prg->name,cur_prg->name_type,num_var,num_val,'A');
				}
				else strcpy(cur_prg->name_type,"S ");

				if (full_br)						/* Set the library and volume params.	*/
				{
					char tlbl[FLDLEN];

					strcpy(tlbl,cur_prg->name);			/* Get the putparm label.		*/
					strcat(cur_prg->name,"-FILE");			/* Set the file field.			*/
					get_type_len(cur_prg->name,cur_prg->name_type,num_var,num_val,'A');

					cur_prg->lib_br= 1;				/* Set to indicate field uses back ref.	*/
					strcpy(cur_prg->lib,tlbl);
					strcat(cur_prg->lib,"-LIBRARY");		/* Set the library field.		*/
					get_type_len(cur_prg->lib,cur_prg->lib_type,num_var,num_val,'A');

					cur_prg->vol_br= 1;				/* Set to indicate field uses back ref.	*/
					strcpy(cur_prg->vol,tlbl);
					strcat(cur_prg->vol,"-VOLUME");			/* Set the volume field.		*/
					get_type_len(cur_prg->vol,cur_prg->vol_type,num_var,num_val,'A');
				}

				next_ptr = aptr;					/* Set so nexttok functions properly.	*/
				nexttok();
			}
			else	write_log("PROCTRAN",'E','R',"NONAME","No file name supplied to PRINT.");
			break;	
		}
		case PROMPT:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing PROMPT keyword.");
			setflag();							/* Turn flags off.			*/
			in_prompt = 1;							/* In the prompt clause.		*/
			got_prompt = 1;							/* got the prompt clause.		*/
			*current_row = 1;						/* Set it to the first 1.		*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			init_screen(scn_num);						/* Creat a screen area.			*/
			strcpy(cur_cmd->command,"PROMPT");				/* Initialize the command line.		*/
			cur_mod_fac = ' ';						/* No mod FACs set yet.			*/
			nexttok();							/* Step to next token.			*/
			break;	
		}
		case RENAME:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing RENAME keyword.");
			setflag();							/* Turn flags off.			*/
			in_rename = 1;							/* Got an rename statement.		*/
			got_rename = 1;							/* set got call to rename.		*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			cur_cmd->rs_area = (struct rs_item *)init_rs();			/* Make rename area.			*/
			strcpy(cur_cmd->command,"RENAME");				/* Initialize the command line.		*/
			cur_ren_sctch->func = 'R';					/* Initialize the type of command.	*/
			nexttok();
			if (ret_lbl) add_decl(cur_ren_sctch->lbl,num_val,num_var);	/* Add return code to declare list.	*/
			break;	
		}
		case RETURN:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing RETURN keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"GO TO MAIN-EXIT");			/* Initialize the command line.		*/
			in_return = 1;							/* Set flag  that I am in RETURN.	*/
			nexttok();
			if (*aptr == '\0' || *aptr == LNFD)				/* Is RETURN only so need to set	*/
			{								/*  cur_rtn->var so gets processed.	*/
				init_return();
				strcpy(cur_rtrn->var,"N");				/* Set to no return codes.		*/
			}
			break;	
		}
		case RUN:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing RUN keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"RUN");					/* Initialize the command line.		*/
			in_run = 1;							/* In A run command lets doit.		*/
			got_run = 1;							/* Found a run command.			*/
			cur_cmd->program_area = (struct program_item *)init_prg();	/* Create a program map.		*/
			nexttok();							/* Gets run program name.		*/
			if (tststrt(aptr))
			{
				cstr = cur_prg->name;
				while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Load the program name.		*/
				*cstr = '\0';						/* NUll terminate the string.		*/
				*cur_prg->name_type = 'S';
				cur_prg->func = 'R';					/* Program func = Run.			*/
				nexttok();
			}
			else	write_log("PROCTRAN",'E','R',"NONAME","No RUN program name supplied.");
			if (ret_lbl)
			{
				cur_para->rlbl = 'R';					/* Is a para label for RUN statement.	*/
				add_decl(cur_prg->rtrn_cd_lbl,num_val,num_var);		/* Add return code to declare list.	*/
			}
			break;	
		}
		case SCRATCH:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing SCRATCH keyword.");
			setflag();							/* Turn flags off.			*/
			in_scratch = 1;							/* Got an scratch statement.		*/
			got_scratch = 1;						/* set got call to scratch.		*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			cur_cmd->rs_area = (struct rs_item *)init_rs();
			strcpy(cur_cmd->command,"SCRATCH");				/* Initialize the command line.		*/
			cur_ren_sctch->func = 'S';					/* Initialize the func of command.	*/
			nexttok();
			if (ret_lbl)
			{
				cur_para->rlbl = 'R';					/* Is a para label for RUN statement.	*/
				add_decl(cur_ren_sctch->lbl,num_val,num_var);		/* Add return code to declare list.	*/
			}
			break;	
		}
		case SET:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing SET keyword.");
			setflag();							/* Turn flags off.			*/
			in_set = 1;							/* Got an extract statement.		*/
			got_set = 1;							/* set got call to set.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"SET");
			nexttok();
			break;								/* Initialize the command line.		*/
		}
		case SUBMIT:
		{
			write_log("PROCTRAN",'I','R',"PROCESS","Processing SUBMIT keyword.");
			setflag();							/* Turn flags off.			*/
			init_cmd(num_cmds);						/* Allocate next command block.		*/
			strcpy(cur_cmd->command,"SUBMIT");				/* Initialize the command line.		*/
			in_submit = 1;							/* In A submit command lets doit.	*/
			got_submit = 1;							/* Found a submit command.		*/
			cur_cmd->program_area = (struct program_item *)init_prg();	/* Create a program map.		*/
			nexttok();							/* Gets submitted program name.		*/
			if (tststrt(aptr))
			{
				cstr = cur_prg->name;
				while (tststrt(aptr)) *cstr++ = toupper(*aptr++);	/* Load the paragraph name.		*/
				*cstr = '\0';						/* Null terminate the string.		*/
				*cur_prg->name_type = 'S';
				cur_prg->func = 'S';					/* Program func = submit.		*/
			}
			else	write_log("PROCTRAN",'E','R',"NONAME","No program name supplied to SUBMIT.");
			if (ret_lbl) add_decl(cur_prg->rtrn_cd_lbl,num_val,num_var);	/* Add return code to declare list.	*/
			break;	
		}
		case PROC:
		case PROCEDURE:
		{
			char *cpos;
			int cnt;
			time_t clock;

			write_log("PROCTRAN",'I','R',"PROCESS","Processing PROCEDURE keyword.");
			if (!got_name)
			{
				got_name = 1;						/* Found the procedure name set it.	*/
				strcpy (proc_id,"000000      ");			/* Initialize the bufer.		*/
				nexttok();						/* Set ptr to procedure name.		*/
				cstr = &proc_id[12];
				cnt = 0;						/* Init lenght of proc_id.		*/
				if (tststrt(aptr))					/* If a procedure name is given.	*/
				{
					while (tststrt(aptr) && cnt < 8)
					{
						*cstr++ = toupper(*aptr++);		/* Copy procedure name to proc_id str.	*/
						cnt++;
					}
					*cstr = '\0';					/* Null terminate the proc id buffer.	*/
				}
				else							/* else generate one.			*/
				{
					char l_prog[STRBUFF];

					write_log("PROCTRAN",'W','R',"NOPROCID","No procedure id supplied.");
					strcpy(l_prog,out_fname);			/* Use file name as default program id.	*/
					cpos = strrchr(l_prog,'.');
					*cpos = '\0';					/* Null terminate so don't get ext.	*/
					go_upper(l_prog);				/* Make sure is upper case.		*/
					strcpy(cstr,l_prog);
				}
				strcat(proc_id,(".\n\0"));				/* Concatenate with .<cr><lf> 		*/
				while (*aptr != '\0' && *aptr != LNFD) nexttok();	/* Set position for processing.		*/
											/* Rest of line is comment.		*/
				i = 0;
				write_log("PROCTRAN",'I','W',"WRITE","Writing Identification Division.");
				while (*ident_div[i])					/* Write out identification division.	*/
				{
					if (i == 8)
					{
						num_outlines++;
						fputs(proc_id,outfile);			/* Write out the procedure id buffer.	*/
					}
					num_outlines++;
					fputs(ident_div[i++],outfile);			/* Write out index array line.		*/
				}							/* Set up to write out date info.	*/
				strcpy (proc_id,"001210      ");			/* Initialize the bufer.		*/
				clock = time(0);
				cpos = ctime(&clock);
				strcat(proc_id,cpos);
				cpos = strrchr(proc_id,'\n');
				*cpos = '\0';						/* Null terminate so don't get <cr>.	*/
				strcat(proc_id,(".\n\0"));				/* Concatenate with .<cr><lf> 		*/
				num_outlines++;
				fputs(proc_id,outfile);					/* Write out the date written buffer.	*/

				in_proc = 1;						/* Set procedure flag.			*/
			}
			break;
		} 
		default:
		{
			char *tptr, temp[15];
			int tndx;

			setflag();							/* Turn flags off.			*/
			tndx = find_keyword(aptr,vs_sys_util);				/* Seacrh for match of vs_sys_util.	*/
			if (tndx >= 0)							/* Found a match for VS system util.	*/
			{
				init_cmd(num_cmds);					/* Allocate next command block.		*/
				strcpy(cur_cmd->command,vs_sys_util[tndx]);		/* Initialize the command line.		*/
				init_return();						/* Allocate return area.		*/
				strcpy(cur_rtrn->type,"VS");				/* Set to VS System Utility.		*/
				if (!strcmp(cur_cmd->command,"TRACE")) in_trace = TRUE;	/* Set to in TRACE command.		*/
			}
			tptr = temp;
			while (tststrt(aptr)) *tptr++ = *aptr++;			/* Copy keyword to temp string.		*/
			*tptr = '\0';							/* Null terminate the string.		*/
			write_log("PROCTRAN",'W','R',"NOTSUPPRTD","Procedure keyword %s not supported.  Line not processed.",temp);
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
	}

	if (*aptr == '\0' || *aptr == LNFD) return(0);
	else	return(1);
}

int test_para_name(int num_par, int* num_cmds)						/* See if it is a paragraph name.	*/
{
	char *lptr, *cstr;
	char *save_aptr;								/* Save aptr in case not a RUN label.	*/
	int	tndx;									/* Type of keyword found.		*/
	register int i;
	paragraph_item *hld_para;

	if (num_par || *aptr == '\'') return(TRUE);					/* Already into line or quoted string	*/
											/*  so not para name.			*/
	lptr = aptr;
	*br_label = '\0';								/* Set the label to null.		*/
	if (find_colin(&lptr))								/* Look for colin to find paragraph name.*/
	{										/* Yes, it is.				*/
		if ( in_run )								/* If within a RUN statement;		*/
		{
			save_aptr = aptr;						/* Save aptr to restore later.		*/
			aptr = lptr + 1;						/* Set aptr to just after colin.	*/
			if (*aptr == ' ') nexttok();					/* Step to next token.			*/
			if ( '\0' != *aptr )						/* If not at the end of the line;	*/
			{
				tndx = find_keyword( aptr, run_clause );		/* Look for a RUN clause keyword.	*/
				if ( tndx == CANCEL
				  || tndx == DISPLAY
				  || tndx == ENTER
				  || tndx == ERROR )					/* If a valid RUN clause keyword;	*/
				{
					write_log("PROCTRAN",'I','R',"PROCESS","Processing Backwards reference label.");
					aptr = save_aptr;				/* restore aptr value.			*/
					cstr = br_label;				/* Load the backwarde ref label.	*/
					while (tststrt(aptr) && *aptr != ':') *cstr++ = toupper(*aptr++);
					*cstr = '\0';					/* Null terminate the string.		*/
					if (*aptr == ':') aptr++;			/* Step over the :			*/
					next_ptr = aptr;				/* Set so processes correctly.		*/
					if (*aptr == ' ') nexttok();			/* Step to next token.			*/
					return ( TRUE );				/* Return to continue RUN processing.	*/
				}
			}
			aptr = save_aptr;						/* Not RUN; restore aptr value.		*/
			next_ptr = aptr;						/* Set so processes next correctly.	*/
		}
		write_log("PROCTRAN",'I','R',"PROCESS","Processing paragraph name.");
		init_para(num_cmds);							/* New paragraph start keeping track.	*/
		setflag();								/* Got a new paragraph.			*/
		in_label = 1;
		i = 0;
		while (tststrt(aptr) && *aptr != ':')					/* While not space, Null, or LNFD or :	*/
		{
			cur_para->name[i++] = toupper(*aptr++);				/* Load the paragraph name.		*/
		}
		cur_para->name[i] = '\0';						/* Null terminate the string.		*/
		hld_para = cur_para;
		cur_para = st_para;
		while (cur_para)							/* Test if label already exists.	*/
		{
			if (cur_para != hld_para && 0 == strcmp(cur_para->name,hld_para->name))
			{
				write_log("PROCTRAN",'E','R',"AMBIGUOUS",
					"Ambiguous label (%s) referenced.  Modifications needed.",hld_para->name);
				break;
			}
			cur_para = cur_para->next_item;
		}
		cur_para = hld_para;
		if (*aptr == ':') aptr++;						/* Step past the colon.			*/
		next_ptr = aptr;							/* Set ptr so will process correctly.	*/
		return(FALSE);								/* Found the paragraph name so no more.	*/
	}
	return(TRUE);									/* Need to still test.			*/
}


void p_run_clause_kw(int ndx)								/* Process RUN Clause keyword.		*/
{
	char *cstr;
	char *l_aptr;

	switch (ndx)
	{
		case DISPLAY: case ENTER:
		{
			char temp[FLDLEN];

			l_aptr = aptr;							/* Test keyword to see if a PUTPARM 	*/
			if (ndx == DISPLAY) l_aptr += 7;				/*  keyword or a RUN keyword.		*/
			else l_aptr += 5;
			while (*l_aptr == ' ') l_aptr++;
			if (*l_aptr == '=') break;					/* Is a PUTPARM keyword so exit.	*/

			if (ndx == DISPLAY) write_log("PROCTRAN",'I','R',"PROCESS","Processing DISPLAY clause.");
			else write_log("PROCTRAN",'I','R',"PROCESS","Processing ENTER clause.");
			in_np_putparm = 0;						/* Set flag back so can process.	*/
			nexttok();							/* Gets PRNAME parameter.		*/
			if (tststrt(aptr))
			{
				cstr = temp;
				while (tststrt(aptr) && *aptr != '(') *cstr++ = toupper(*aptr++); /* Load prname into temp var.	*/
				*cstr = '\0';						/* Null terminate the string.		*/
			}
			else
			{
				write_log("PROCTRAN",'E','R',"NOPRNAME","No Parameter Reference name supplied.");
				break;
			}

			in_using = 0;							/* Reset so processes properly.		*/
			in_putparm = 1;							/* In a putparm command lets doit.	*/
			got_putparm = 1;						/* Found a putparm command.		*/
			init_pparm(ndx);						/* Create a putparm.			*/
			strcpy(cur_pp->prname,temp);					/* Copy PRNAME to structure.		*/
			if (*br_label != '\0') strcpy(cur_pp->label,br_label);		/* Copy label to structure.		*/

			break;	
		}
		case ERROR:
		{
			in_using = 0;							/* Reset so processes properly.		*/
			nexttok();							/* Step past the ERROR keyword.		*/
			nexttok();							/* Step past the EXIT keyword.		*/
			if (!strncmp(aptr,"IS ",3)) nexttok();				/* Step over the IS keyword.		*/
			if (tststrt(aptr))
			{
				cstr = cur_prg->error_lbl;
				while (tststrt(aptr)) *cstr++ = toupper(*aptr++); 	/* Load the label.			*/
				*cstr = '\0';						/* Null terminate the string.		*/
			}
			else	write_log("PROCTRAN",'E','R',"NOLABEL","No label supplied for GOTO on ERROR EXIT.");
			break;
		}
		case CANCEL:
		{
			in_using = 0;							/* Reset so processes properly.		*/
			nexttok();							/* Step past the CANCEL keyword.	*/
			nexttok();							/* Step past the EXIT keyword.		*/
			if (!strncmp(aptr,"IS ",3) || !strncmp(aptr,"is ",3)) nexttok(); /* Step over the IS keyword.		*/
			if (tststrt(aptr))
			{
				cstr = cur_prg->cancel_lbl;
				while (tststrt(aptr)) *cstr++ = toupper(*aptr++); 	/* Load the label.			*/
				*cstr = '\0';						/* Null terminate the string.		*/
			}
			else	write_log("PROCTRAN",'E','R',"NOLABEL","No label supplied for GOTO on CANCEL EXIT.");
			break;
		}
		default:
		{
			char *tptr, temp[15];

			tptr = temp;
			while (tststrt(aptr)) *tptr++ = toupper(*aptr++);		/* Copy keyword to temp string.		*/
			*tptr = '\0';							/* Null terminate the string.		*/
			write_log("PROCTRAN",'E','R',"NOTSUPPRTD","RUN clause %s not supported.  Line not processed.",temp);
			while (*aptr != '\0' && *aptr != LNFD) nexttok();		/* Step to end of line.			*/
			break;
		}
	}
}

void p_print_submit_kw(int caller,int ndx)						/* Process PRINT/SUBMIT keyword.	*/
{
	char *cstr;
	char *tptr, temp[15];

	switch (ndx)
	{
		case CLASS: case DISP:
		{
			tptr = temp;
			cstr = aptr;
			while (tststrt(cstr)) *tptr++ = toupper(*cstr++);		/* Copy keyword to temp string.		*/
			*tptr = '\0';							/* Null terminate the string.		*/
			if (caller == 1) write_log("PROCTRAN",'I','R',"PROCESS","Processing PRINT keyword %s.",temp);
			else	write_log("PROCTRAN",'I','R',"PROCESS","Processing SUBMIT keyword %s.",temp);

			in_putparm = 1;							/* In A putparm command lets doit.	*/
			init_pparm(ndx);						/* Create a putparm.			*/
			if (caller == 1) strcpy(cur_pp->prname,"PRINT");
			else if (caller == 2) strcpy(cur_pp->prname,"SUBMIT");
			break;	
		}

		case STATUS: case DUMP: case CPULIMIT: case ACTION:
		{
			tptr = temp;
			cstr = aptr;
			while (tststrt(cstr)) *tptr++ = toupper(*cstr++);		/* Copy keyword to temp string.		*/
			*tptr = '\0';							/* Null terminate the string.		*/
			write_log("PROCTRAN",'I','R',"PROCESS","Processing SUBMIT keyword %s.",temp);

			in_putparm = 1;							/* In A putparm command lets doit.	*/
			init_pparm(ndx);						/* Create a putparm.			*/
			strcpy(cur_pp->prname,"SUBMIT");
			break;	
		}

		default:
		{
			tptr = temp;
			while (tststrt(aptr)) *tptr++ = toupper(*aptr++);		/* Copy keyword to temp string.		*/
			*tptr = '\0';							/* Null terminate the string.		*/
			if (caller == 1)
			{
				write_log("PROCTRAN",'E','R',"NOTSUPPRTD",
						"PRINT keyword %s not supported.  Keyword not processed.",temp);
			}
			else write_log("PROCTRAN",'E','R',"NOTSUPPRTD",
				"SUBMIT keyword %s not supported.  Keyword not processed.",temp);

			while (*aptr != ',' && *aptr != '\0' && *aptr != LNFD) nexttok(); /* Step to next set of keywords or	*/
			break;								/*    the end of line.			*/
		}
	}
}

static void add_decl(char* keyword_lbl, int* num_val,int* num_var)			/* If have a label then declare		*/
											/* var to hold RETURN-CODE value.	*/
{
	init_current(num_var);								/* Alloc next node for declare.		*/
	(*num_val)++;									/* Keep track of init stmnt role back.	*/
	strcpy(cur_decl->field1,cur_para->name);
	strcpy(cur_decl->type,"IR");							/* Set the var type indicator.		*/
	strcpy(keyword_lbl,cur_para->name);						/* Set var to return value to.		*/
}
/*
**	History:
**	$Log: ptpdiv.c,v $
**	Revision 1.7  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.6  1997/04/21 15:17:42  scass
**	Corrected copyright.
**	
**	Revision 1.5  1996-09-12 19:17:36-04  gsl
**	fix prototypes
**
**
**
*/
