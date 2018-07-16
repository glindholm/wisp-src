			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		scrn.h
**
**	Purpose:	To hold Screen header info
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef SCRN_H
#define SCRN_H

#include "node.h"

#define		MAX_SCREENS	100							/* the maximum number of screens	*/

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
		short	vert_off;							/* Number of lines between vertical OCC	*/
		short	vert_off2;							/* Number of lines between vertical OCC	*/
		short	num_range;							/* The number of range items		*/
		char	item_num[4];							/* Cobol ref number example = 01 or 05	*/
		char	name[40];							/* The variable name			*/
		char	pic[40];							/* The pic definition for the var	*/
		NODE	source_clause;							/* The var that is the source for output*/
		NODE	object_clause;							/* The var that is the target for input */
		NODE	lo_range_clause;						/* The low end range item		*/
		NODE	hi_range_clause;						/* The High end range item		*/
	};
typedef struct item_record item_record;

EXT item_record *screen_item[MAX_SCREENS];						/* A list of screen items		*/
EXT item_record *this_item INIT_NULL;							/* A ptr to the current screen item	*/
EXT item_record *last_item INIT_NULL;							/* A ptr to the previous screen item	*/

EXT char scrn_name[MAX_SCREENS][40];							/* The names of screen records		*/
EXT int  scrn_crt[MAX_SCREENS];								/* The crt files they are using.	*/
EXT char scrn_flags[MAX_SCREENS];							/* 8 bit flags				*/

EXT int num_screens INIT_ZERO;								/* How many screen items so far		*/
EXT int scount;										/* global screen counter		*/

#endif /* SCRN_H */
