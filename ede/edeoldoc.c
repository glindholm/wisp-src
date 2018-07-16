			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		edeoldoc.c
**
**	Purpose:	To hold frontend/user-definable interface to EDE's ON-LINE DOC feature.
**			This source file is distributed as part of the EDE kit to allow the user
**			to hook-in there own ON-LINE doc routine.
**
**	Routines:	
**	EDEOLDOC()	The routine called to provide help text.
**
**
**	History:
**	04/15/93	Split out of edehelp.c to provide a more generic interface.
**
*/

/*
**	Routine:	EDEOLDOC()
**
**	Function:	Frontend to EDE ON-LINE DOC feature
**
**	Description:	This routine is called to provide the help text based on the
**			current cursor position and the context.
**
**	Arguments:	
**	cursor_pos	The coded current cursor position.
**	context		The context as set by A_WSLINK.
**	help_text	The help text is return here.
**	help_cnt	??
**	help_rc		The return code
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void EDEOLDOC(cursor_pos, context, help_text, help_cnt, help_rc)
void	*cursor_pos;		/*   4 	bytes 	*/
void	*context;		/* 167 	bytes 	*/
void	*help_text;		/* 642	bytes 	*/
void	*help_cnt;		/*   4	bytes	*/
void	*help_rc;		/*   4	bytes	*/
{
/* #define STERLING */
#ifdef STERLING
#if defined(VMS) || defined(MF)
#define ON_LINE_DOC_RTN
	SFREU041(cursor_pos, context, help_text, help_cnt, help_rc);
#endif /* VMS or MF */
#ifdef ACU
#define ON_LINE_DOC_RTN
	void	*parms[5];
	int	lens[5];
	int	rc;
	char	*program;

	program = "SFREU041";
	parms[0] = cursor_pos;	lens[0] = 4;
	parms[1] = context;	lens[1] = 167;
	parms[2] = help_text;	lens[2] = 642;
	parms[3] = help_cnt;	lens[3] = 4;
	parms[4] = help_rc;	lens[4] = 4;
	call_acucobol(program,(long)5,parms,lens,&rc);
	if (rc)
	{
		vre_window("%%EDE-E-EDEOLDOC Call to %s failed rc=%d",program,rc);
		*((long *)help_rc) = 1;  /* no help */
	}
#endif /* ACU */
#endif /* STERLING */

#ifndef ON_LINE_DOC_RTN
	/*
	**	If a on-line doc routine has not been supplied then display an error message.
	*/
	vre_window("%%EDE-E-EDEOLDOC No on-line doc routine has been defined.");
#endif /* !ON_LINE_DOC_RTN */
}
