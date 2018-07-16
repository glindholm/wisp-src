/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

/*
**	File:		ptcheck.c
**
**	Project:	wisp/proctran
**
**	RCS:		$Source:$
**
**	Purpose:	check syntax routines.
**
**	Routines:	
**	chk_cmd()	Checks tables for a valid cmd then call correct format routine.
**	chk_num()	Return FALSE if instr contains anything but  * + , - . / or a number from 0 - 9
**	chk_num1()	Return FALSE if instr contains anything but numbers (0-9)
**
*/

#define EXT extern

#include <stdio.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgcblsrc.h"
#include "pgstruct.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/


/*
**	ROUTINE:	chk_cmd()
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
void chk_cmd(int *num_cmds,int *num_var)						/* Routine checks tables for a valid	*/
{											/* cmd then call correct format routine.*/
	char scrn_num[FLDLEN];

	(*num_cmds)++;									/* Keep track of number of commands.	*/
	cur_rtrn = cur_cmd->return_area;
	if (cur_rtrn && !strcmp(cur_rtrn->type,"VS"))
	{
		write_log("PROCTRAN",'W','W',"VSSYSUTIL","Writing VS System Utility %s NOT SUPPORTED message.",cur_cmd->command);
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline," DISPLAY \"");
		strcat(cobline,cur_cmd->command);
		strcat(cobline," not supported.\"");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
		setflag();
		return;
	}

	write_log("PROCTRAN",'I','W',"CHKCMD","Checking for a valid CALL command");
	if (cur_cmd->assign_var)
	{
 		cur_assign = cur_cmd->assign_var;					/* Load the current assign table.	*/
		wrt_init_assign();
		setflag();
	}
	if (*cur_cmd->goto_para)
	{
		wrt_goto();								/* Write goto command to cobol program.	*/
		setflag();
	}
	if (cur_cmd->rs_area)
	{
		cur_ren_sctch = cur_cmd->rs_area;					/* Point the local variable to the area.*/
		wrt_rs();								/* Write rename scratch cmd to output.	*/
		setflag();
	}
	if (cur_cmd->program_area)
	{
		cur_prg = cur_cmd->program_area; 					/* Point the local variable to the area.*/
		wrt_program();								/* Write RUN, PRINT, SUBMIT calls.	*/
		setflag();
	}
	if (cur_cmd->if_area)
	{
		cur_if = cur_cmd->if_area;						/* Point the local variable to the area.*/
		wrt_if();								/* Write if command to cobol program.	*/
		in_if = 1;								/* Set if is statement.			*/
	}
	if (cur_cmd->set_extract_area)
	{
		cur_set_extr = cur_cmd->set_extract_area; 				/* Point the local variable to the area.*/
		wrt_set_extract();							/* Write Calls to set extract.		*/
		setflag();
	}
	if (cur_cmd->rfdr_area)
	{
		cur_rfdr = cur_cmd->rfdr_area; 						/* Point the local variable to the area.*/
		wrt_readfdr();								/* Write Calls to readfdr.		*/
		setflag();
	}
	if (*cur_cmd->call_para)
	{
		if (!genccall)
		{
			write_log("PROCTRAN",'E','W',"VERIFY_CALL",
				"CALL %s converted to PERFORM. Logic verification needed.",cur_cmd->call_para);
		}
		wrt_call();								/* Write PERFORM command to cobol	*/
		setflag();								/*  paragraph.				*/
	}
	if (cur_cmd->return_area)
	{
		wrt_return();								/* Write return command to cobol program.*/
		setflag();
	}
	if (cur_cmd->end_cmd)
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline," GO TO P-");						/* Load the Prefix.			*/
		strcat(cobline,cur_para->name); 					/* Load the paragraph name.		*/
		strcat(cobline,"-EXIT");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
		setflag();
	}
	if (cur_cmd->logoff_cmd)
	{
		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"PERFORM LOGOFF-USER");
		end_prt_line(1);							/* Add end of line stuff and write line.*/
		setflag();
	}
	if (cur_cmd->screen_area)
	{
		cur_scn = cur_cmd->screen_area;		 				/* set the current screen item.		*/
		cur_scn_fld = cur_scn->variable_area;
		while (cur_scn_fld)							/* Process subscripted fields.		*/
		{
			if (cur_scn_fld->sub_flag) wrt_subscript(1,7);			/* If substr, gen CALL STRING first.	*/
			cur_scn_fld = cur_scn_fld->next_item;
		}
		
		if (cur_scn->screen_erase == 'Y')
		{
			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"MOVE KEYBRDLOCK           TO WRITE-CONTROL");
			if (in_if) end_prt_line(0);					/* Add end of line stuff and write line.*/
			else end_prt_line(1);

			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"MOVE SPACES               TO WSA-SCREEN-TOP");
			if (in_if) end_prt_line(0);					/* Add end of line stuff and write line.*/
			else end_prt_line(1);

			indent_line();							/* Add the correct indentation.		*/
			strcat(cobline,"PERFORM A-DISPLAY-AND-READ");
			end_prt_line(1);						/* Add end of line stuff and write line.*/
		}

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"MOVE ");
		if (*cur_cmd->command == 'M')						/* If is MESSAGE command.		*/
		{
			if (cur_scn->screen_alarm == 'Y') strcat(cobline,"LOCKALARM");
			else	strcat(cobline,"KEYBRDLOCK");
		}
		else
		{
			if (cur_scn->screen_alarm == 'Y') strcat(cobline,"ALARMPOSN");
			else strcat(cobline,"POS-CURSOR");
		}
		strcat(cobline," TO WRITE-CONTROL");
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"PERFORM A-INIT-FAC-");
		memset(scrn_num,'\0',FLDLEN);						/* Clean the field.			*/
		sprintf(scrn_num,"%d",cur_scn->num_screen);
		strcat(cobline,scrn_num);						/* Load the the perform init fac.	*/
		if (in_if) end_prt_line(0);						/* Add end of line stuff and write line.*/
		else end_prt_line(1);

		indent_line();								/* Add the correct indentation.		*/
		strcat(cobline,"PERFORM A-DISPLAY-AND-READ-");
		strcat(cobline,scrn_num);						/* Load the the perform init fac.	*/
		end_prt_line(1);							/* Add end of line stuff and write line.*/
		setflag();
	}
}

/*
**	ROUTINE:	chk_num()
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
int chk_num(char *instr)								/* Return FALSE if instr contains 	*/
{											/*  anything but  * + , - . / 		*/
	register int i;									/*  or a number from 0 - 9.		*/

	for (i = 0; ((i < FLDLEN) && (instr[i] != '\0')); i++)
	{
		if (!operand(instr[i])  && !number(instr[i])) return(FALSE);
	}
	return(TRUE);
}

/*
**	ROUTINE:	chk_num1()
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
int chk_num1(char *instr)								/* Return FALSE if instr contains 	*/
{											/* anything but numbers (0-9).		*/
	register int i;

	if (*instr == '\0') return(FALSE);						/* If no value in instr.		*/

	for (i = 0; ((i < FLDLEN) && (instr[i] != '\0')); i++)
	{
		if (!number(linein[i])) return(FALSE);
	}
	return(TRUE);
}

/*
**	History:
**	$Log: ptcheck.c,v $
**	Revision 1.10  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.9  1997/04/21 15:02:46  scass
**	Corrected copyright.
**	
**	Revision 1.8  1996-09-12 19:14:27-04  gsl
**	Remove unused data items
**
**	Revision 1.7  1995-09-22 07:10:23-07  scass
**	Moved defines of EXT to top of file before includes
**
 * Revision 1.6  1995/09/22  13:59:23  scass
 * Removed history that was not needed.
 *
 * Revision 1.5  1995/09/22  13:57:58  scass
 * Added standard DTMI headers
 *
#
**
**
*/
