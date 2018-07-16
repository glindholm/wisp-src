static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		customvw.c
**
**	Project:	wisp/lib
**
**	Purpose:	To hold stub routines that will allow custom screen handling by
**			applications that normally use vwang().
**			This is to allow linking to resolve undefines that are only used
**			when a custom screen handler is included. 
**
**	Routines:	
**	use_custom_vwang()	Stub used in LINK normally found in user defined routine.
**	custom_vwang()		Stub used in LINK normally found in user defined routine.
*/

/*
**	Includes
*/

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
**	ROUTINE:	use_custom_vwang()
**
**	FUNCTION:	DevTech stub for test if want to use custom screen handling.
**
**	DESCRIPTION:	Function will test if a custom screen handler should be
**			used instead of the standard vwang() routine.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		0	= standard vwang() screen handling should occur
**			value 	= custom screen handling should occur.
**
**	WARNINGS:	None
**
*/
int use_custom_vwang(void)
{
	return(0);
}

/*
**	ROUTINE:	custom_vwang()
**
**	FUNCTION:	DevTech stub for custom screen handling routine.
**
**	DESCRIPTION:	Pass in same parameters as vwang() but do own custom
**			screen handling.  If set a functional return then 
**			processing will fall through and attempt a vwang() call.
**
**	ARGUMENTS:	Are the same as for the vwang() routine:
**	function	code indicating the Wang workstation function to perform
**	wsb		Wang style screen block - 4 byte order-area, 1920 byte screen data
**	lines		number of text lines to write
**	terminate_list	PFkeys allowed to terminate the screen read
**	term		return PFkey value which terminated screen read
**	no_mod		return value to determine if screen was modified
**
**	GLOBALS:	None
**
**	RETURN:		0	= successful call of custom screen handler
**			value	= error while calling custom screen handler so
**				  fall through to vwang() screen handler.
**
**	WARNINGS:	None
**
*/
int custom_vwang(unsigned char *function,unsigned char *wsb,unsigned char *lines,unsigned char *terminate_list,
        unsigned char *term,unsigned char *no_mod)
{
	return(1);
}

/*
**	History:
**	$Log: customvw.c,v $
**	Revision 1.2  1995/07/06 07:43:23  scass
**	Added routine comments for DevTech stubs for custom screen handler
**	(SFG enhancement request)
**	
 * Revision 1.1  1995/07/05  15:38:17  scass
 * Initial revision
 *
**
**
*/
