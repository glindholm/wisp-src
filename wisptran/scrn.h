/*
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
*/


/*
**	File:		scrn.h
**
**	Purpose:	To hold Screen header info
**
**
*/

#ifndef SCRN_H
#define SCRN_H

#include "node.h"

#define		MAX_SCREENS	200							/* the maximum number of screens	*/

#define		SCRN_IN_DECLARATIVES		0x01					/* flag if screen used in declaratives	*/
#define		SCRN_IN_PROCDIV			0x02					/* flag screen used in procedure div.	*/

struct item_record
	{
		struct item_record *next_item;						/* A ptr to the next item		*/
		short	x_level;							/* The level				*/
		short	x_occurs;							/* The occurs clause			*/
		short	row;								/* The calculated row 			*/
		short	x_row;								/* The row clause			*/
		short	col;								/* The calculated column		*/
		short	x_col;								/* The column clause			*/
		short	vert_1;								/* First vertical occurs count		*/
		short	vert_2;								/* Second vertical occurs count		*/
		short	horiz;								/* Horizontal occurs count		*/
		short	vert_off1;							/* Number of lines between vertical OCC	*/
		short	vert_off2;							/* Number of lines between vertical OCC	*/
		short	num_range;							/* The number of range items		*/
		char	item_num[4];							/* Cobol ref number example = 01 or 05	*/
		char	name[40];							/* The variable name			*/
		char	pic[40];							/* The pic definition for the var	*/
		NODE	source_clause;							/* The var that is the source for output*/
		NODE	object_clause;							/* The var that is the target for input */
		NODE	lo_range_clause;						/* The low end range item		*/
		NODE	hi_range_clause;						/* The High end range item		*/
		short	x_level_col;							/* The level column number		*/
		short	size;								/* The size based on the PIC clause	*/
	};
typedef struct item_record item_record;

EXT item_record *screen_item[MAX_SCREENS];						/* A list of screen items		*/
EXT item_record *this_item INIT_NULL;							/* A ptr to the current screen item	*/
EXT item_record *last_item INIT_NULL;							/* A ptr to the previous screen item	*/

EXT char scrn_name[MAX_SCREENS][40];							/* The names of screen records		*/
EXT int  scrn_crt[MAX_SCREENS];								/* The crt files they are using.	*/
EXT char scrn_flags[MAX_SCREENS];							/* 8 bit flags				*/

EXT int num_screens INIT_ZERO;								/* How many screen items so far		*/

extern item_record *find_item(char *the_name);

#endif /* SCRN_H */
/*
**	History:
**	$Log: scrn.h,v $
**	Revision 1.10  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.9  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.8  1997/09/09 21:55:04  gsl
**	Change needed for ACN code
**	
**	Revision 1.7  1996-08-30 21:56:09-04  gsl
**	drcs update
**
**
**
*/
