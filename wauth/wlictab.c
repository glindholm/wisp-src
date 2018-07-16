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
**	File:		wlictab.c
**
**	Project:	WISP licensing
**
**	RCS:		$Source:$
**
**	Purpose:	Convert wisp license log to tab file
**
**
**	Usage:		wlictab <wauthorize.log >wauthorize.tab
**
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
**	ROUTINE:	main()
**
**	FUNCTION:	Convert the wauthorize.log file to a tab file
**
**	DESCRIPTION:	Read stdin write stdout.
**
**			Each record written has tab separated fields with the first line being field names.
**
**			A "****" line starts a record.
**			A blank line ends a record.
**			Replace all tabs with a space because tabs would corrupt the output.
**
**			Minimum error checking is done becuase the log
**			file is written by wauthorize so the data is
**			absolutely consistant and very clean. 
**
**
*/

int main(int argc, char* argv[])
{
	char	inbuf[1024];
	char	*ptr;
	char	*rhs, *lhs;
	int	len;

	char *r_type = "";
	char f_operator[256] = "";
	char f_time[256] = "";
	char f_custname[256] = "";
	char f_custnum[256] = "";
	char f_platform[256] = "";
	char f_lictype[256] = "";
	char f_licdate[256] = "";
	char f_expdate[256] = "";
	char f_lickey[256] = "";
	char f_machid[256] = "";
	char f_valcode[256] = "";
	
	printf("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
	       "TYPE",
	       "OPERATOR",
	       "TIME",
	       "CUSTNAME",
	       "CUSTNUM",
	       "PLATFORM",
	       "LICTYPE",
	       "LICDATE",
	       "EXPDATE",
	       "LICKEY",
	       "MACHID",
	       "VALCODE");

	while(ptr = gets(inbuf))
	{
		len = strlen(inbuf);
		
		if (0 == memcmp(inbuf, "**********", 10))
		{
			/* New entry */
			r_type = "";
			f_operator[0] = '\0';
			f_time[0] = '\0';
			f_custname[0] = '\0';
			f_custnum[0] = '\0';
			f_platform[0] = '\0';
			f_lictype[0] = '\0';
			f_licdate[0] = '\0';
			f_expdate[0] = '\0';
			f_lickey[0] = '\0';
			f_machid[0] = '\0';
			f_valcode[0] = '\0';
		}
		else if (0 == len)
		{
			/* End of entry */
			printf("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
			       r_type,
			       f_operator,
			       f_time,
			       f_custname,
			       f_custnum,
			       f_platform,
			       f_lictype,
			       f_licdate,
			       f_expdate,
			       f_lickey,
			       f_machid,
			       f_valcode);
		}
		else if (0 == strcmp(inbuf,"<VALIDATION-CODE-ENTRY>"))
		{
			r_type = "VALCODE";
		}
		else if (0 == strcmp(inbuf,"<LICENSE-KEY-ENTRY>"))
		{
			r_type = "LICKEY";
		}
		else
		{
#define MIN_REC_LEN 20
			if (len < MIN_REC_LEN)
			{
				r_type = "BADDATA";
				continue;
			}

			/* Strip tabs, replace with a space */
			while(ptr = strchr(inbuf,'\t'))
			{
				*ptr = ' ';
				len = strlen(inbuf);
			}

			/* Strip trailing spaces */
			while(len > MIN_REC_LEN && ' ' == inbuf[len-1])
			{
				len--;
				inbuf[len] = '\0';
			}

			/* Point to the rhs value */
			rhs = &inbuf[MIN_REC_LEN-1];

			/* Null terminate the field name */
			lhs = inbuf;
			ptr = strchr(inbuf,' ');
			*ptr = '\0';

			if (0 == strcmp(lhs,"OPERATOR"))
			{
				strcpy(f_operator,rhs);
			}
			else if (0 == strcmp(lhs,"TIME"))
			{
				strcpy(f_time,rhs);
			}
			else if (0 == strcmp(lhs,"CUSTOMER-NAME"))
			{
				strcpy(f_custname,rhs);
			}
			else if (0 == strcmp(lhs,"CUSTOMER-NUMBER"))
			{
				strcpy(f_custnum,rhs);
			}
			else if (0 == strcmp(lhs,"PLATFORM"))
			{
				strcpy(f_platform,rhs);
			}
			else if (0 == strcmp(lhs,"LICENSE-TYPE"))
			{
				strcpy(f_lictype,rhs);
			}
			else if (0 == strcmp(lhs,"LICENSE-DATE"))
			{
				strcpy(f_licdate,rhs);
			}
			else if (0 == strcmp(lhs,"EXPIRATION-DATE"))
			{
				strcpy(f_expdate,rhs);
			}
			else if (0 == strcmp(lhs,"LICENSE-KEY"))
			{
				strcpy(f_lickey,rhs);
			}
			else if (0 == strcmp(lhs,"MACHINE-ID"))
			{
				strcpy(f_machid,rhs);
			}
			else if (0 == strcmp(lhs,"VALIDATION-CODE"))
			{
				strcpy(f_valcode,rhs);
			}
			else 
			{

			}
			
			
			
		}
		
		

	}
	

	
}


/*
**	History:
**	$Log: wlictab.c,v $
**	Revision 1.2  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.1  1998/12/18 17:59:50  gsl
**	Initial revision
**	
**
**
**
**
*/
