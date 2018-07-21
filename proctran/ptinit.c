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

/* PG_INIT.C   Contains all of the initialization sub-routines.									*/

#include <stdio.h>

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"

static void memerr(void);								/* Memory error.			*/

void setflag(void)									/* Init all of the "in" flags.		*/
{											
	in_proc = 0;
	in_prompt = 0;
	in_pfkey = 0;
	in_curcol = 0;
	in_currow = 0;
	in_message = 0;
	in_declare = 0;
	in_assign = 0;
	in_print = 0;
	in_run = 0;
	in_if = 0;
	in_submit = 0;
	in_using = 0;
	in_extract = 0;
	in_set = 0;
	in_return = 0;
	in_logoff = 0;
	in_scratch = 0;
	in_rename = 0;
	in_library = 0;
	in_putparm = 0;
	in_readfdr = 0;
	in_label = 0;
	in_global = 0;
	in_trace = 0;
	in_np_putparm = 0;
	set_string = 0;
}

void setscreen(void)									/* Initialize the screen flags.		*/
{
	in_pfkey = 0;
	in_row = 0;
	in_curcol = 0;
	in_currow = 0;
	in_erase = 0;
	in_alarm = 0;
}

void setgot(void)									/* Init all of the "got" flags.		*/
{
	got_link = 0;									/* Set no link as default.		*/
	got_prompt = 0;									/* Set no prompt as default.		*/
	got_name = 0;									/* Set no Program name.			*/
	got_message = 0;								/* Set no message as default.		*/
	got_print = 0;									/* Set no print as default.		*/
	got_run = 0;									/* Set no run as default.		*/
	got_submit = 0;									/* Set no submit as default.		*/
	got_find = 0;									/* Found a call find.			*/
	got_extract = 0;								/* Set no extract as default.		*/
	got_set = 0;									/* Set no set as default.		*/
	got_logoff = 0;									/* Set no logoff as default.		*/
	got_scratch = 0;								/* Set no scratch as default.		*/
	got_rename = 0;									/* Set no rename as default.		*/
	got_string = 0;									/* Set no stringas default.		*/
	got_backref = 0;								/* Set no backwards ref. as default.	*/
}

void init_assign(int* num)								/* Standard init of command buffer.	*/
{
	assign_item *hld_cur_assign;

	hld_cur_assign = cur_assign;							/* Hold the previous address.		*/
	if (!(cur_assign = (assign_item *) malloc(sizeof(assign_item)))) memerr();	/* Init assign for first time.		*/

	if (*num == 0)									/* First variable of assign.		*/
	{
		cur_cmd->assign_var = cur_assign;					/* Init pointer to assign var for cmds.	*/
		cur_assign->prev_item = NULL;
	}
	else
	{
		cur_assign->prev_item = hld_cur_assign;
		hld_cur_assign->next_item = cur_assign;					/* Point previous to current.		*/
	}
	(*num)++;
	cur_assign->sub_flag	= 0;							/* Init the assign structure.		*/
	cur_assign->br_flag	= 0;
	*cur_assign->field1	= '\0';
	*cur_assign->type	= '\0';
	*cur_assign->length	= '\0';
	*cur_assign->len_type	= '\0';
	cur_assign->dlmtr	= '\0';
	*cur_assign->start_pos	= '\0';
	*cur_assign->stpos_type	= '\0';
	*cur_assign->literal	= '\0';
	cur_assign->next_item	= NULL;
}

void init_current(int* num_var)								/* Standard init of command buffer.	*/
{
	declare_item *hld_cur_item;

	hld_cur_item = cur_decl;							/* if second variable store pointer.	*/
	if (!(cur_decl = (declare_item *) malloc(sizeof(declare_item)))) memerr();	/* Get next for Working storage sec.	*/

	if (*num_var == 0)
	{
		cur_decl->prev_item = NULL;						/* Set ptr for prev to NULL.		*/
	}
	else
	{
		cur_decl->prev_item = hld_cur_item;					/* Hold value of previous Table.	*/
		hld_cur_item->next_item = cur_decl;					/* Set pointer for previous item.	*/
	}

	(*num_var)++;
	*cur_decl->field1   = '\0';							/* Init the declare structure.		*/
	strcpy(cur_decl->type,"  ");
	*cur_decl->length   = '\0';
	*cur_decl->value    = '\0';
	cur_decl->next_item = NULL;
}

void init_link(int* num_link_var)							/* Standard init of program link.	*/
{
	link_item *hld_cur_link;

	hld_cur_link = cur_link;			 				/* if second variable store pointer.	*/
	if (!(cur_link = (link_item *) malloc(sizeof(link_item)))) memerr();		/* Get next table for linkage sect.	*/

	if (*num_link_var == 0)
	{
		stlinklst = cur_link;							/* Hold first link pointer.		*/
		cur_link->prev_item = NULL; 						/* Set pointer for prev to NULL.	*/
	}
	else
	{
		cur_link->prev_item = hld_cur_link; 					/* Hold value of previous Table.	*/
		hld_cur_link->next_item = cur_link; 					/* Set pointer for previous item.	*/
	}

	(*num_link_var)++;								/* Got a link variable keep count.	*/
	*cur_link->field1	= '\0';
	strcpy(cur_link->type,"  ");
	*cur_link->length	= '\0';
	cur_link->next_item	= NULL;
}

void init_para(int* num_cmds)							/* Standard init of command buffer.	*/
{
	paragraph_item *hld_cur_para;

	hld_cur_para = cur_para;							/* Store the previous paragraph.	*/
	*num_cmds = 0;									/* No Commands to start.		*/
	if (!(cur_para = (paragraph_item *) malloc(sizeof(paragraph_item)))) memerr();	/* Init Procedure division cmd table.	*/
	if (!st_para)									/* If first paragraph.			*/
	{
		st_para = cur_para;							/* Hold the starting point for later.	*/
		cur_para->prev_item = NULL;
	}
	else
	{
		cur_para->prev_item = hld_cur_para;
		hld_cur_para->next_item = cur_para;					/* Load next pragraph on prev paragraph.*/
	}

	*cur_para->name 	= '\0';							/* Initialize paragraph structure.	*/
	cur_para->rlbl		= '\0';
	cur_para->all_command	= NULL;
	cur_para->next_item	= NULL;
}

void init_cmd(int* num_cmds)								/* Standard init of command buffer.	*/
{
	command_item *hld_cur_cmd;

	hld_cur_cmd = cur_cmd;								/* Hold the previous address.		*/
	if (!(cur_cmd = (command_item *) malloc(sizeof(command_item)))) memerr();	/* Init commands for fisrt paragraph.	*/

	if (*num_cmds == 0)
	{
		cur_para->all_command = cur_cmd;					/* Init pointer to commands first para.	*/
		cur_cmd->prev_item = NULL;						/* Init prev ptr to NULL.		*/
	}
	else
	{
		cur_cmd->prev_item = hld_cur_cmd;					/* Store previous address.		*/
		hld_cur_cmd->next_item = cur_cmd;					/* Point previous to current.		*/
	}

	(*num_cmds)++;									/* Increment num cmds in paragraph.	*/
	*cur_cmd->command	  = '\0';						/* Initialize the command structure.	*/
	*cur_cmd->goto_para	  = '\0';
	*cur_cmd->gotop_type	  = '\0';
	cur_cmd->return_area	  = NULL;
	*cur_cmd->call_para	  = '\0';
	cur_cmd->end_cmd	  = NULL;
	cur_cmd->logoff_cmd	  = NULL;
	cur_cmd->assign_var	  = NULL;
	cur_cmd->if_area	  = NULL;
	cur_cmd->set_extract_area = NULL;
	cur_cmd->rfdr_area	  = NULL;
	cur_cmd->program_area	  = NULL;
	cur_cmd->rs_area	  = NULL;
	cur_cmd->screen_area	  = NULL;
	cur_cmd->next_item	  = NULL;
}

void init_screen(int* screen_num)							/* Standard init of screen buffer.	*/
{
	screen_item *hld_cur_scn;

	hld_cur_scn = cur_scn;								/* store previous if item.		*/
	if (!(cur_scn = (screen_item *) malloc(sizeof(screen_item)))) memerr();		/* Initialize paragraph for screen.	*/

	if (!*screen_num)								/* first screen ?			*/
	{
		start_screen = cur_scn;							/* Store it.				*/
	}
	else hld_cur_scn->next_item = cur_scn;						/* Set the next screen item.		*/

	cur_cmd->screen_area	= cur_scn;
	(*screen_num)++;								/* Up the count always.			*/
	cur_scn->num_screen	= *screen_num;						/* Number of screen for ws section.	*/
	cur_scn->num_rows	= 0;
	*cur_scn->pf_key	= '\0';
	*cur_scn->pfk_type	= '\0';	
	*cur_scn->cur_row	= '\0';
	*cur_scn->cr_type	= '\0';	
	*cur_scn->cur_col	= '\0';
	*cur_scn->cc_type	= '\0';	
	cur_scn->screen_erase	= '\0';							/* Init to a single NULL char.		*/
	cur_scn->screen_alarm	= '\0';
	cur_scn->variable_area	= NULL;
	cur_scn->next_item	= NULL;
}

void init_screen_field(int* current_row)						/* Standard init of screen buffer.	*/
{
	scn_fld_item *hld_scn_fld;

	hld_scn_fld = cur_scn_fld;							/* store previous if item.		*/
	if (!(cur_scn_fld = (scn_fld_item *) malloc(sizeof(scn_fld_item)))) memerr(); 	/* Init paragraph for screen.		*/

	if (!cur_scn->variable_area)							/* First time thru, init first pointer.	*/
	{
		cur_scn->variable_area = cur_scn_fld;
		cur_scn_fld->prev_item = NULL;
	}
	else
	{
		cur_scn_fld->prev_item = hld_scn_fld;					/* Set the previous ptr.		*/
		hld_scn_fld->next_item = cur_scn_fld;					/* Point previous to current.		*/
	}

	cur_scn_fld->row		= *current_row;					/* Initialize screen field items.	*/
	cur_scn_fld->col		= 2;
	cur_scn_fld->sub_flag		= 0;
	cur_scn_fld->fac_msk		= 0;
	*cur_scn_fld->screen_fld	= '\0';
	cur_scn_fld->modf		= '\0';
	*cur_scn_fld->scn_fld_type	= '\0';
	*cur_scn_fld->map_fld		= '\0';
	*cur_scn_fld->len		= '\0';
	cur_scn_fld->dlmtr		= '\0';
	*cur_scn_fld->start_pos		= '\0';
	*cur_scn_fld->stpos_type	= '\0';
	*cur_scn_fld->length		= '\0';
	*cur_scn_fld->len_type		= '\0';
	cur_scn_fld->str_params		= NULL;
	cur_scn_fld->next_item		= NULL;
}

void init_string_param(void)								/* Standard init of STRING parameter	*/
{											/*  field.				*/
	string_item *hld_str;

	hld_str = cur_str_param;							/* store previous STRING item.		*/
	if (!(cur_str_param = (string_item *) malloc(sizeof(string_item)))) memerr(); 	/* Init for STRING parameter.		*/

	if (!cur_scn_fld->str_params)							/* First time thru, init first pointer.	*/
	{
		cur_scn_fld->str_params = cur_str_param;				/* Store the first pointer.		*/
	}
	else	hld_str->next_item = cur_str_param;

	*cur_str_param->field1		= '\0';
	cur_str_param->next_item	= NULL;
}

void init_using(void)									/* Standard init of program using.	*/
{
	using_item *hld_use;

	hld_use = cur_using;								/* Store the previous item;		*/
	if (!(cur_using = (using_item *) malloc(sizeof(using_item)))) memerr(); 	/* Initialize variable name area.	*/

	if (!cur_prg->using_name)							/* Check to see if first time thru.	*/
	{
		cur_prg->using_name = cur_using;					/* Store the first pointer.		*/
	}
	else	hld_use->next_item = cur_using;

	cur_using->br_flag	= 0;
	*cur_using->var 	= '\0';
	*cur_using->type	= '\0';
	cur_using->next_item	= NULL;
}

void init_se(void)									/* Standard init of command buffer.	*/
{
	set_extract_item *hld_se;

	hld_se = cur_set_extr;								/* store previous if item.		*/
	if (!(cur_set_extr = (set_extract_item *) malloc(sizeof(set_extract_item)))) memerr(); /* Init for set and extract.	*/

	if (!cur_cmd->set_extract_area)							/* First time thru, init first pointer.	*/
	{
		cur_cmd->set_extract_area = cur_set_extr;
	}
	else	hld_se->next_item = cur_set_extr;					/* Set the set/extract next item.	*/

	cur_set_extr->sub_flag		= 0;						/* Initialize SET/EXTRACT field items.	*/
	cur_set_extr->br_flag		= 0;
	*cur_set_extr->var		= '\0';
	*cur_set_extr->type		= '\0';
	*cur_set_extr->key		= '\0';
	cur_set_extr->dlmtr		= '\0';
	*cur_set_extr->start_pos	= '\0';
	*cur_set_extr->stpos_type	= '\0';
	*cur_set_extr->length		= '\0';
	*cur_set_extr->len_type		= '\0';
	cur_set_extr->next_item		= NULL;
}

void init_rfdr(void)								/* Standard init of command buffer.	*/
{
	readfdr_item *hld_rfdr;

	hld_rfdr = cur_rfdr;								/* store previous if item.		*/
	if (!(cur_rfdr = (readfdr_item *) malloc(sizeof(readfdr_item)))) memerr(); 	/* Init for readfdr.			*/

	if (!cur_cmd->rfdr_area)							/* First time thru, init first pointer.	*/
	{
		cur_cmd->rfdr_area = cur_rfdr;
	}

	cur_rfdr->br_flag	= 0;							/* Initialize to no backwards ref.	*/
	cur_rfdr->name_br	= 0;
	cur_rfdr->lib_br	= 0;
	cur_rfdr->vol_br	= 0;
	*cur_rfdr->recvr	= '\0';
	*cur_rfdr->type		= '\0';
	*cur_rfdr->file		= '\0';
	*cur_rfdr->file_type	= '\0';
	*cur_rfdr->lib		= '\0';
	*cur_rfdr->lib_type	= '\0';
	*cur_rfdr->vol		= '\0';
	*cur_rfdr->vol_type	= '\0';
	*cur_rfdr->id		= '\0';
}

void init_if(void)									/* Standard init of command buffer.	*/
{
	if_item *hld_if;

	hld_if = cur_if;								/* store previous if item.		*/
	if (!(cur_if = (if_item *) malloc(sizeof(if_item)))) memerr();			/* Initialize paragraph for goto.	*/

	if (!cur_cmd->if_area)								/* First time thru, init the first ptr.	*/
	{
		cur_cmd->if_area = cur_if;
	}
	else hld_if->next_item = cur_if;						/* Make the next if item.		*/

	cur_if->br_flag		= 0;							/* Init the if_item structure.		*/
	cur_if->sub_flag	= 0;
	cur_if->lib_br		= 0;
	cur_if->vol_br		= 0;
	*cur_if->paren_value	= '\0';
	*cur_if->var		= '\0';
	*cur_if->type		= '\0';
	*cur_if->cond		= '\0';
	*cur_if->file		= '\0';
	*cur_if->file_type	= '\0';
	*cur_if->lib		= '\0';
	*cur_if->lib_type	= '\0';
	*cur_if->vol		= '\0';
	*cur_if->vol_type	= '\0';
	cur_if->dlmtr		= '\0';
	cur_if->next_item	= NULL;
}

program_item *init_prg(void)								/* Standard init of program buffer.	*/
{
	if (!(cur_prg = (program_item *) malloc(sizeof(program_item)))) memerr(); 	/* Initialize Program area.		*/

	cur_prg->number_using	= 0;					    		/* Initialize num of program variables.	*/
	cur_prg->sub_flag	= 0;
	cur_prg->br_flag	= 0;
	cur_prg->name_br	= 0;
	cur_prg->lib_br		= 0;
	cur_prg->vol_br		= 0;
	cur_prg->func		= ' ';
	*cur_prg->name		= '\0';
	*cur_prg->name_type	= '\0';
	*cur_prg->as_name	= '\0';
	*cur_prg->as_name_type	= '\0';
	*cur_prg->lib		= '\0';
	*cur_prg->lib2		= '\0';
	*cur_prg->lib_type	= '\0';
	*cur_prg->lib2_type	= '\0';
	*cur_prg->vol		= '\0';
	*cur_prg->vol2		= '\0';
	*cur_prg->vol_type	= '\0';
	*cur_prg->vol2_type	= '\0';
	*cur_prg->error_lbl	= '\0';
	*cur_prg->cancel_lbl	= '\0';
	*cur_prg->rtrn_cd_lbl	= '\0';
	cur_prg->pparm_area	= NULL;
	cur_prg->using_name	= NULL;

	return(cur_prg);								/* Point subroutine to current pointer.	*/
}

rs_item *init_rs(void)									/* Standard init of program buffer.	*/
{
	if (!(cur_ren_sctch = (rs_item *) malloc(sizeof(rs_item)))) memerr(); 		/* Init for RENAME and SCRATCH.		*/

	if (!cur_cmd->rs_area)								/* First time thru, init first pointer.	*/
	{
		cur_cmd->rs_area = cur_ren_sctch;
	}
	else
	{
		write_log("PROCTRAN",'F','R',"NOSTRUCT","No structure availbale for allocation in RENAME/SCRATCH list.");
	}

	cur_ren_sctch->br_flag	     = 0;						/* Initialize to no backwards ref.	*/
	cur_ren_sctch->name_br	     = 0;
	cur_ren_sctch->lib_br	     = 0;
	cur_ren_sctch->vol_br	     = 0;
	cur_ren_sctch->sub_flag	     = 0;
	cur_ren_sctch->func	     = ' ';		    				/* Initialize func type. 		*/
	cur_ren_sctch->opt	     = ' ';	   					/* Initialize library options.		*/
	*cur_ren_sctch->name	     = '\0';
	*cur_ren_sctch->name_type    = '\0';
	*cur_ren_sctch->lib	     = '\0';
	*cur_ren_sctch->lib_type     = '\0';
	*cur_ren_sctch->vol	     = '\0';
	*cur_ren_sctch->vol_type     = '\0';
	*cur_ren_sctch->to_file	     = '\0';
	*cur_ren_sctch->to_file_type = '\0';
	*cur_ren_sctch->to_lib	     = '\0';
	*cur_ren_sctch->to_lib_type  = '\0';
	*cur_ren_sctch->lbl	     = '\0';
	cur_ren_sctch->dlmtr	     = '\0';
	*cur_ren_sctch->start_pos    = '\0';
	*cur_ren_sctch->stpos_type   = '\0';
	*cur_ren_sctch->length	     = '\0';
	*cur_ren_sctch->len_type     = '\0';

	return(cur_ren_sctch);								/* Point subroutine to current pointer.	*/
}

void init_pparm(int func)								/* Standard init of putparm buffer.	*/
{
	putparm_item *hld_pp;

	hld_pp = cur_pp;								/* store previous if item.		*/
	if (!(cur_pp = (putparm_item *) malloc(sizeof(putparm_item)))) memerr(); 	/* Initialize Putparm area.		*/

	if (!cur_prg->pparm_area)							/* First time thru, init first pointer.	*/
	{
		cur_prg->pparm_area = cur_pp;
		cur_pp->prev_item	= NULL;
	}
	else
	{
		cur_pp->prev_item = hld_pp;						/* Make the next putparm item.		*/
		hld_pp->next_item = cur_pp;						/* Point previous to current.		*/
	}

	cur_pp->kwcnt 		= 0;			    				/* Initialize putparm variables.	*/
	cur_pp->br_flag		= 0;
	if (func == DISPLAY) cur_pp->func = 'D';
	else	cur_pp->func = 'E';
	*cur_pp->prname		= '\0';
	*cur_pp->label		= '\0';
	strcat(cur_pp->pfkey,"1");
	cur_pp->kwlist		= NULL;
	cur_pp->next_item	= NULL;
}

void init_ppkw(void)									/* Standard init of putparm keyword.	*/
{
	pp_keywords *hld_kw;

	hld_kw = cur_pp_key;								/* store previous if item.		*/
	if (!(cur_pp_key = (pp_keywords *) malloc(sizeof(pp_keywords)))) memerr(); 	/* Initialize Putparm keyword area.	*/

	if (!cur_pp->kwlist)								/* First time thru, init first pointer.	*/
	{
		cur_pp->kwlist = cur_pp_key;
	}
	else	hld_kw->next_item = cur_pp_key;						/* Set the next keyword item.		*/

	cur_pp_key->len	      = 0;							/* Initialize the keyword values.	*/
	cur_pp_key->len2      = 0;
	cur_pp_key->br_flag   = 0;
	cur_pp_key->sub_flag  = 0;
	*cur_pp_key->keywrd   = '\0';
	*cur_pp_key->val      = '\0';
	*cur_pp_key->type     = '\0';
	*cur_pp_key->val2     = '\0';
	*cur_pp_key->type2    = '\0';
	cur_pp_key->next_item = NULL;
}
void init_return(void)									/* Standard init of return buffer.	*/
{
	return_item *hld_rtrn;

	hld_rtrn = cur_rtrn;								/* store previous return item.		*/
	if (!(cur_rtrn = (return_item *) malloc(sizeof(return_item)))) memerr(); 	/* Init for return.			*/

	if (!cur_cmd->return_area)							/* First time thru, init first pointer.	*/
	{
		cur_cmd->return_area = cur_rtrn;
	}
	else	hld_rtrn->next_item = cur_rtrn;						/* Set the return next item.		*/

	*cur_rtrn->var		= '\0';
	*cur_rtrn->type		= '\0';
	cur_rtrn->next_item	= NULL;
}

void init_literals(void)								/* Initialize the common literal strings.*/
{
	strcpy(util,"PROCTRAN");
	strcpy(paren_sym,"(");
	strcpy(loc_str_typ,"S ");
	strcpy(loc_int_typ,"I ");
	strcpy(int_compute,"IC");
	strcpy(int_return,"IR");
	strcpy(str_literal,"SL");
	strcpy(len_one,"(1)");
	strcpy(len_two,"(2)");
}

static void memerr(void)								/* Memory error.			*/
{
	write_log("PROCTRAN",'F','R',"NOMEM","No memory availbale for allocation of PROCTRAN structures.");
	exit(1);									/* Unconditional exit.			*/
}
/*
**	History:
**	$Log: ptinit.c,v $
**	Revision 1.6  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.5  1997/04/21 15:13:49  scass
**	Corrected copyright.
**	
**	Revision 1.4  1996-09-12 19:17:20-04  gsl
**	fix prototypes
**
**
**
*/
