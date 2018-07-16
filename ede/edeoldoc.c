static char copyright[]="Copyright (c) 1988-1997 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";

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
**	EDECUSTM()	Specify a custom memu item
**	EDECUSTX()	The routine which is called when the custom memu item is selected.
**
**
*/

#ifdef MSDOS
typedef long  		 int4;
#else
typedef int  		 int4;
#endif

extern int vre_window(char*, ...);


/*
**	Routine:	EDEOLDOC()
**
**	Function:	Frontend to EDE ON-LINE DOC feature
**
**	Description:	This routine is called to provide the help text based on the
**			current cursor position and the context.
**
**	Arguments:	
**	cursor_pos	The current cursor position as identified by video & EDE.
**	context		The values loaded into EDE by A_WSLINK through the users
**			application program.
**	help_text	HELP text as returned here from the user HELP document program.
**	help_cnt	Old Wang interface argument, MUST be zero.
**	help_rc		The return code from user HELP document program.
**				0 = request complete, documetation text in help_text.
**				1 = request NOT complete, no help document available.
**				2 = request complete, user HELP document program displayed
**				    help information full screen.
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
	call_acucobol(program,(int4)5,parms,lens,&rc);
	if (rc)
	{
		vre_window("%%EDE-E-EDEOLDOC Call to %s failed rc=%d",program,rc);
		*((int4 *)help_rc) = 1;  /* no help */
	}
#endif /* ACU */
#endif /* STERLING */

/* #define RSS */
#ifdef RSS 

#if defined(VMS) || defined(MF)
#define ON_LINE_DOC_RTN
	UTDOCMNT(cursor_pos, context, help_text, help_cnt, help_rc);
#endif /* VMS or MF */

#ifdef ACU
#define ON_LINE_DOC_RTN
	void	*parms[5];
	int	lens[5];
	int	rc;
	char	*program;

	program = "UTDOCMNT";
	parms[0] = cursor_pos;	lens[0] = 4;
	parms[1] = context;	lens[1] = 167;
	parms[2] = help_text;	lens[2] = 642;
	parms[3] = help_cnt;	lens[3] = 4;
	parms[4] = help_rc;	lens[4] = 4;
	call_acucobol(program,(int4)5,parms,lens,&rc);
	if (rc)
	{
		vre_window("%%EDE-E-EDEOLDOC Call to %s failed rc=%d",program,rc);
		*((int4 *)help_rc) = 1;  /* no help */
	}
#endif /* ACU */
#endif /* RSS */

#ifndef ON_LINE_DOC_RTN
	/*
	**	If a on-line doc routine has not been supplied then display an error message.
	*/
	vre_window("%%EDE-E-EDEOLDOC No on-line doc routine has been defined.");
#endif /* !ON_LINE_DOC_RTN */
}

/*
**	ROUTINE:	EDECUSTM()
**
**	FUNCTION:	Specify a custom menu item.
**
**	DESCRIPTION:	This routine is called to check if there is a custom menu item 
**			and get the menu item title.
**			The item will be inserted between the "Environment" and "Goodies".
**
**			If the user wants a custom menu item you modify this routine.
**
**	ARGUMENTS:	
**	custtitle	The menu item title is returned here if there is one.
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		No custom menu item
**	1		There is a custom menu item
**
**	WARNINGS:	None.
**
*/
int EDECUSTM(char custtitle[64])
{
#ifdef STERLING
#define DEF_EDECUSTM
	/*
	**	Add custom menu item "Lookups".
	*/
	strcpy(custtitle,"Lookups");
	return 1;
#endif /* STERLING */

#ifndef DEF_EDECUSTM
	/*
	**	There is no custom menu item defined.
	*/
	return 0;
#endif
}

/*
**	ROUTINE:	EDECUSTX()
**
**	FUNCTION:	The custom menu processing routine
**
**	DESCRIPTION:	If the custom menu item is selected then this routine is called.
**
**			If you add a custom menu item then modify this routine to
**			preform any desired processing. Usually you would simply call
**			you custom routine.
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
void EDECUSTX(void)
{
#ifdef STERLING
#define DEF_EDECUSTX
	/*
	**	For custom menu item "Lookups" call SFREU100()
	*/
	SFREU100();
#endif /* STERLING */

#ifndef DEF_EDECUSTX
	/*
	**	If a custom menu item routine has not been supplied then display an error message.
	*/
	vre_window("%%EDE-E-EDECUSTX No custom menu item routine has been defined.");
#endif
}

/*
**	History:
**	$Log: edeoldoc.c,v $
**	Revision 1.8  1997/06/10 19:18:17  gsl
**	Change long to int4
**	
**	Revision 1.7  1997-01-02 18:59:42-05  gsl
**	Removed the include <vutil.h> as the customer would not have the
**	header files source.
**
**	Revision 1.6  1997-01-02 14:06:54-08  gsl
**	Add EDE CUSTOM MENU ITEM logic and code for STERLING
**
**	Revision 1.5  1996-09-13 10:54:23-07  gsl
**	Add missing includes
**
**
**	04/15/93	Split out of edehelp.c to provide a more generic interface. GSL
**      05/18/94        Added ede & user program documentation, added RSS help program. JAK
**
**
*/
