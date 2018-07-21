/*
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
*/


/*
**	wcmatch.c
*/


/*
 * WL_wcmatch()	-- wild-card string matching routine..
 *
 * FUNCTIONAL DESCRIPTION:
 *		This function attempts to match the pattern string specified by the pat'
 *		argument against a target string specified by the argument 'str'. If 'sex'
 *		is TRUE, the pattern match is case-sensitive.
 *
 *		In the Macintosh implementation the wildcard characters are as follows:
 *		'Å'	-- match zero or more of any character in this position.
 *		'?'	-- match any one character in this position.
 *
 *              In the Wisplib version (here), code has been modified as follows:
 *		'?'	-- match zero or more of any character in this position.
 *		'*'	-- match any one character in this position.
 *
 *		Further modified to pass the wildcard characters.
 *
 * CALLING SEQUENCE:
 *		match.w.u = WL_wcmatch(pat, str, sex, MATCHONE, MATCHMANY);
 *
 * FORMAL PARAMETERS:
 *		char		*pat		- pointer to pattern C-string
 *		char		*str		- pointer to target C-string
 *		bool		sex		- boolean for case sensitivity.
 *		char		MATCHONE	- wildcard char to match one character
 *		char		MATCHMANY	- wildcard char to match many character
 *
 * IMPLICIT INPUTS:
 *		None.
 *
 * IMPLICIT OUTPUTS:
 *		None.
 *
 * SIDE EFFECTS:
 *		None.
 *
 * STATUS RETURNS:
 *		TRUE		- there was a match of the pattern against the target
 *		FALSE		- there was no match.
 *
 * Author:
 *		Original version by J. Dundas, Jan 1989.
 *		'sex' parameter added by D. Stine, Feb 1989.
 *              misc changes by J. Cooper, Sep 1989.
 *		Passing of MATCHONE/MANY by G. Lindholm Feb 1990
 */

#include <ctype.h>
#include "idsistd.h"
#include "wdefines.h"

int WL_wcmatch(pat, str, sex, MATCHONE, MATCHMANY)
	register char	*pat;								/* IN -- pointer to the pattern string */
	register char	*str;								/* IN -- pointer to the target string */
		int	sex;								/* IN -- boolean for case sensitivity */
		char	MATCHONE;							/* IN -- one char WILDCARD CHAR		*/
		char	MATCHMANY;							/* IN -- many char WILDCARD CHAR	*/
{
	register char	c;								/* Test character from pattern string */
	register char	s;								/* Test character from target string */
	register char	*strend;							/* Computed pointer to end of string */


	/* ** Execution begins ** */
	while (*pat != '\0')
		{
		c = *pat++;
		if (c == MATCHMANY)						/* Match zero or more characters in this place */
			{
			if (*str == '\0')						/* Made it to the end of the target? */
				return (TRUE);						/* yes, so its a match */
			for (strend = str; *strend != '\0'; strend++)
				;
			while (strend >= str)						/* Recurse to match rest of string */
				{
				if (WL_wcmatch(pat, strend, sex, MATCHONE, MATCHMANY))
					return (TRUE);
				strend--;
				}
			return (FALSE);
			}								/* end if (match Å) */
		else if (c == MATCHONE)							/* Match one character in this position */
			{
			if (*str != '\0')
				{
				++str;                           
				continue;
				}
			else
				{
				return (FALSE);						/* At end of string, so return */
				}
			}
		else									/* Match character-for-character */
			{
			s = *str;
			if (sex == FALSE && isalpha((int)c) && isalpha((int)s))			/* Case insensitive? */
				{
				if (!islower((int)c))
					c = tolower(c);
				if (!islower((int)s))
					s = tolower(s);
				}
			if (s != '\0' && c == s)
				++str;
			else
				return (FALSE);
			}
			
		}									/* end while() */
	if (*str == '\0')
		return (TRUE);
	else
		return (FALSE);
}




/*
**	History:
**	$Log: wcmatch.c,v $
**	Revision 1.12  2003/02/04 18:43:32  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.10  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.9  2002/07/10 21:05:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.8  1996/08/19 22:33:08  gsl
**	drcs update
**	
**
**
*/
