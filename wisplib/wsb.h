/* 
	Copyright (c) 1997 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

/*
**	File:		wsb.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Header for wsb.c
**
*/

#ifndef wsb_H
#define wsb_H
/*
**	Includes
*/
#include "vwang.h"

/*
**	Structures and Defines
*/
typedef unsigned short HWSB;
typedef struct s_wsbvwang 
{
	struct
	{
		unsigned char	row;			/* oa.row 	The starting row (normaly 1) 	*/
		unsigned char	wcc;			/* oa.wcc 	The Write Control Character	*/
		unsigned char	curcol;			/* oa.curcol 	The cursor column		*/
		unsigned char	currow;			/* oa.currow 	The cursor row			*/
	} oa;
	char	data[WSB_ROWS][WSB_COLS];		/* data[row][col] The array of screen lines	*/
} wsbvwang_t;

/*
**	Function Prototypes
*/
HWSB wsb_new(void);
void wsb_delete(HWSB hWsb);
void wsb_add_text(HWSB hWsb, int row, int col, const char *text);
void wsb_add_field(HWSB hWsb, int row, int col, fac_t fac, const char *field, int len);
void wsb_add_menu_item(HWSB hWsb, int row, int col, int pfkey, const char *text);
void wsb_add_tabstop(HWSB hWsb, int row, int col);
void wsb_get_field(HWSB hWsb, int row, int col, char *field, int len);
void wsb_set_alarm(HWSB hWsb);
void wsb_display_and_read(HWSB hWsb, const char* keylist, int *piPfkey, int *piCurrow, int *piCurcol);

#endif /* wsb_H */

/*
**	History:
**	$Log: wsb.h,v $
**	Revision 1.3  1998/03/13 23:02:23  gsl
**	Add wsb_set_alarm()
**	and wsb_add_tabstop()
**	
**	Revision 1.2  1997-10-29 12:00:44-05  gsl
**	write
**
**	Revision 1.1  1997-10-24 15:08:33-04  gsl
**	Initial revision
**
**
**
**
**
**
*/
