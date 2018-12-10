/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
**	File:		linkvect.c
**
**	Purpose:	To handle vectoring call "LINK"s into call sub's for internal routines.
**
**	Routines:	
**	WL_islinkvector()	Test if routine can be vectored.
**	WL_linkvector()		Vectors to the routine.
**
**
*/

#include <string.h>

#include "idsistd.h"
#include "linkvect.h"
#include "link.h"
#include "wperson.h"
#include "wisplib.h"

/*
**	Declare all the routines that are used in vectorlist.
*/
#include "vssubs.h"


static struct 
{
	char	*name;
	void	(*vector)();
} vectorlist[] = 
{
	{"BELL    ",	(void (*)()) BELL},
	{"BITPACK ",	(void (*)()) BITPACK},
	{"BITUNPK ",	(void (*)()) BITUNPK},
	{"DATE    ",	(void (*)()) WISPDATE},
	{"DATE2   ",	(void (*)()) DATE2},
	{"DATE4   ",	(void (*)()) DATE4},
	{"DATE6   ",	(void (*)()) DATE6},
	{"DAY     ",	(void (*)()) DAY},
	{"EXTRACT ",	(void (*)()) EXTRACT},
	{"FIND    ",	(void (*)()) FIND},
	{"HEXPACK ",	(void (*)()) HEXPACK},
	{"HEXUNPK ",	(void (*)()) HEXUNPK},
	{"LOGOFF  ",	(void (*)()) LOGOFF},
	{"MESSAGE ",	(void (*)()) MESSAGE},
	{"PAUSE   ",	(void (*)()) PAUSE},
	{"PRINT   ",	(void (*)()) PRINT},
	{"READFDR ",	(void (*)()) READFDR},
	{"READFDR4",	(void (*)()) READFDR4},
	{"READVTOC",	(void (*)()) READVTOC},
	{"RENAME  ",	(void (*)()) RENAME},
	{"SCRATCH ",	(void (*)()) SCRATCH},
	{"SCREEN  ",	(void (*)()) SCREEN},
	{"SEARCH  ",	(void (*)()) SEARCH},
	{"SET     ",	(void (*)()) SET},
/*	{"SORT    ",	(void (*)()) SORT},		This mucks up a link to SORT/WSORT */
	{"STRING  ",	(void (*)()) STRING},
	{"WSXIO   ",	(void (*)()) WSXIO},

	{NULL,		NULL}
};

/*
**	Routine:	WL_islinkvector()
**
**	Function:	Test if a routine can be used in a call to WL_linkvector().
**
**	Description:	Search the vectorlist[] to see if routine is in it.
**			If the LINKVECTOROFF option is used this will always return 0.
**
**	Arguments:
**	routine		The routine name to check. It should be left justified
**			and blank padded to 8 chars.
**
**	Globals:
**	vectorlist	The list of known routine names and vectors.
**
**	Return:
**	1		Routine is known and can be used in WL_linkvector().
**	0		Not found.
**
**	Warnings:	None
**
**	History:	
**	04/27/94	Written by GSL
**
*/
int WL_islinkvector(routine)
char	routine[8];
{
	int	idx;

	if (WL_opt_linkvectoroff()) return 0;

	for(idx=0; vectorlist[idx].name; idx++)
	{
		if (0==memcmp(vectorlist[idx].name,routine,8))
		{
			return 1;
		}
	}
	return 0;
}

/*
**	Routine:	WL_linkvector()
**
**	Function:	To vector a call "LINK" to a subroutine call.
**
**	Description:	Find the routine vector in the list then call it with parameters.
**
**	Arguments:
**	routine		The routine name to vector to.
**	parm_cnt	The number of parmameters.
**	parm_list	The list of parameters (by reference).
**
**	Globals:
**	vectorlist	The list of known routine names and vectors.
**
**	Return:
**	0		Vector was successful, (check routine return code also)
**	20		Routine not found in vector list.
**	81		Too many parameters
**
**	Warnings:	None
**
**	History:	
**	04/27/94	Written by GSL
**
*/
int WL_linkvector(routine,parm_cnt,parm_list)
char	routine[8];
int	parm_cnt;
struct str_parm *parm_list;
{
	int	idx;
	void	(*vector)();
	char	*p[MAX_LINK_PARMS];
	int4	vacnt;

	vector = NULL;

	/*
	**	Search the list for routine.
	*/
	for(idx=0; vectorlist[idx].name; idx++)
	{
		if (0==memcmp(vectorlist[idx].name,routine,8))
		{
			vector = vectorlist[idx].vector;
			break;
		}
	}

	if (!vector)
	{
		/*
		**	No vector for this routine.
		*/
		return 20;
	}

	if (parm_cnt > MAX_LINK_PARMS)
	{
		/*
		**	Too many parmameters
		*/
		return 81;
	}

	/*
	**	Load up the parmameters into a temp area.
	*/
	for(idx=0; idx<parm_cnt; idx++)
	{
		p[idx] = parm_list->parm[idx];
	}

	/*
	**	Set the vararg count.
	*/
	vacnt = parm_cnt;
	WL_set_va_count(vacnt);

	switch(parm_cnt)
	{
	case 0:
		(*vector)();
		break;
	case 1:
		(*vector)(p[0]);
		break;
	case 2:
		(*vector)(p[0],p[1]);
		break;
	case 3:
		(*vector)(p[0],p[1],p[2]);
		break;
	case 4:
		(*vector)(p[0],p[1],p[2],p[3]);
		break;
	case 5:
		(*vector)(p[0],p[1],p[2],p[3],p[4]);
		break;
	case 6:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5]);
		break;
	case 7:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6]);
		break;
	case 8:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7]);
		break;
	case 9:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8]);
		break;
	case 10:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9]);
		break;
	case 11:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10]);
		break;
	case 12:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11]);
		break;
	case 13:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12]);
		break;
	case 14:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13]);
		break;
	case 15:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14]);
		break;
	case 16:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15]);
		break;
	case 17:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16]);
		break;
	case 18:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17]);
		break;
	case 19:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18]);
		break;
	case 20:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19]);
		break;
	case 21:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20]);
		break;
	case 22:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21]);
		break;
	case 23:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22]);
		break;
	case 24:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23]);
		break;
	case 25:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24]);
		break;
	case 26:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24],p[25]);
		break;
	case 27:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24],p[25],p[26]);
		break;
	case 28:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24],p[25],p[26],p[27]);
		break;
	case 29:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24],p[25],p[26],p[27],p[28]);
		break;
	case 30:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24],p[25],p[26],p[27],p[28],p[29]);
		break;
	case 31:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24],p[25],p[26],p[27],p[28],p[29],
			  p[30]);
		break;
	case 32:
		(*vector)(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],
			  p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18],p[19],
			  p[20],p[21],p[22],p[23],p[24],p[25],p[26],p[27],p[28],p[29],
			  p[30],p[31]);
		break;
	}

	return 0;
}

/*
**	History:
**	$Log: linkvect.c,v $
**	Revision 1.22  2003/03/24 22:41:20  gsl
**	Add DATE6
**	
**	Revision 1.21  2003/03/20 19:05:14  gsl
**	Change references fo DATE to WISPDATE
**	
**	Revision 1.20  2003/01/31 21:40:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.19  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.18  2003/01/29 16:18:35  gsl
**	Include vssubs.h to define VSSUBS
**	
**	Revision 1.17  2003/01/28 19:56:51  gsl
**	fix warnings
**	
**	Revision 1.16  2002/07/23 21:24:56  gsl
**	wrename -> RENAME
**	
**	Revision 1.15  2002/07/16 16:24:54  gsl
**	Globals
**	
**	Revision 1.14  2002/07/12 19:10:13  gsl
**	Global unique WL_ changes
**	
**	Revision 1.13  2002/07/12 17:00:58  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.12  2002/07/10 21:05:19  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.11  1999/09/08 23:42:53  gsl
**	Add READFDR4
**	
**	Revision 1.10  1999-09-08 15:39:36-04  gsl
**	Add DATE4
**
**	Revision 1.9  1999-01-04 17:47:13-05  gsl
**	fix warning
**
**	Revision 1.8  1997-05-13 16:15:52-04  scass
**	Added DATE2
**
**	Revision 1.7  1996-08-19 18:32:27-04  gsl
**	drcs update
**
**
**
*/
