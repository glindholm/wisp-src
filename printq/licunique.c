			/************************************************************************/
			/*									*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#include "wlicense.h"

/*
**	Routine:	product_name()
**
**	Function:	Return the product name.
**
**	Description:	Returns a pointer to a static data area that contains the product name.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to product name.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *product_name()
{
	static	char	name[] = {"UniQue"};

	return(name);
}

/*
**	Routine:	license_filepath()
**
**	Function:	Return the filepath to the license file.
**
**	Description:	Returns a pointer to a static data area that contains the license filepath.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to license filepath.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *license_filepath()
{
	static	char	path[] = {"/lib/UniQue.license"};

	return(path);
}

/*
**	Routine:	x_license_filepath()
**
**	Function:	Return the filepath to the hidden temporary license file.
**
**	Description:	Returns a pointer to a static data area that contains the filepath
**			for the hidden temporary file.  This is used to keep the inode
**			on systems that don't have a machine id.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to hidden temporary license filepath.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *x_license_filepath()
{
	static	char	path[] = {"/lib/.UniQue.license"};

	return(path);
}

/*
**	Routine:	lic_trantable()
**
**	Function:	Return the license encoding translation table.
**
**	Description:	Returns a pointer to a static data area that contains the license
**			encoding translation table.  The table must contains:
**				letters:	A-Z
**				Numbers:	1-9
**			It is 35 bytes long and the characters are scrabbled.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to license encoding translation table.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *lic_trantable()
{
	static	char	tt_tran[] = {"YL5MJN6PH7SFT8VDWKBQAZ9CX1EU2GR3IO4"};

	return(tt_tran);
}

/*
**	Routine:	authlogfile()
**
**	Function:	Return the authorize log filename.
**
**	Description:	Returns a pointer to a static data area that contains the file name.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to name.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *authlogfile()
{
	static	char	name[] = {"authorize.log"};

	return(name);
}
