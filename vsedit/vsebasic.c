static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*****************************************************************************
                 C. I. S.   P R O G R A M   M O D U L E
          (c)Copyright 1991 Corporate Information Systems, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   N A R A T I V E
------------------------------------------------------------------------------

	This module contains the functions used to maintain a list of where
	line numbers are located inside of BASIC statements and update those
	numbers when the BASIC source is renumbered.

==============================================================================
                      O T H E R   N O T E S
------------------------------------------------------------------------------

	A state machine is used to do a simple parse of a line and figure out
	if a number is a reference to a line number or in fact a numerical
	value. To determine the next state, the current state and the current
	input character are used to index the array "machine" and get the next
	state.

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO		DESCRIPTION
 	--------	----	----------------------------------------------
	08/10/93	AJA		Initial version
	11/11/93	AJA		Fixed bug where ON GOTO/GOSUB #, #, # : the last number
						before th colon was not getting recognized, also if a
						line needs to be split and it starts with a *, the new
						line must start with a *
	11/12/93	AJA		Compare prev_ptr->line to current->line in update_
						linenum instead of comparing linenos since two
						different lines can have the same line number during
						renumbering and this will cause info of the wrong line
						to be altered
	12/13/93	AJA		Implemented a simple hash to speed up renumbering
						process

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include "vsebasic.h"
#include "vsetxt.h"

#define SPACES	"                                                                                "
#define ZERO			48
#define NUM_LEN			30
#define HASH_LIMIT		1000
#define MAX_LINE_LEN	vse_edit_width+1

static char start_state=0;	/* Starting state of machine depending on context */
static line_num *hash[HASH_LIMIT];	/* Hash table of elements */

/* Array "machine" is a representation of a state machine that will recognize
   a line number in a BASIC program given the context of line numbers
   following the words ELSE, GOSUB, GOTO, LINE =, ON <exp>, THEN, and USING.
   The first index value is provided by the current state and the second
   index value is provided by a character in a input line. Depending on the
   current state and the input character, a new state is found. */

static char machine[][128]=
	{
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 0 */	  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  2, 1,10, 1, 1, 1, 1,15, 1, 1,20, 1, 1, 1, 1,37,40, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 1 */	  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 2 */	  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 3 */   0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 4 */	  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 6,50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 5 */	  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 6,50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 6 */	  0, 0, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  2, 1,10, 1, 1, 1, 1,15, 1, 1,20, 1, 1, 1, 1,37,40, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 9,60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0,
/* 7 */	  0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,44, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 9,60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0,
/* 8 */	  0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,44, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 9 */	  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  2, 1,10, 1, 1, 1, 1,15, 1, 1,20, 1, 1, 1, 1,37,40, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 10 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,11, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 11 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,12,14, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 12 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,13, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 13 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 5, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 14 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 15 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1,16, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 16 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1,17, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 17 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		 18, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0,19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0,19,51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 18 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 6, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0,19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0,19,51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 19 */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 1, 1, 1, 1,
		  2, 1,10, 1, 1, 1, 1,15, 1, 1,20, 1, 1, 1, 1,37,40, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 20 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1,21, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0,22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0,22,52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 21 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{23,23,23,23,23,23,23,23,23,22,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,22,53,23,23,23,23,23,23,23,23,23,23,23,23,
/* 22 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,24,54,23,23,23,23,23,23,23,23,23,23,23,23,
/* 23 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,25,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,24,55,23,23,23,23,23,23,23,23,23,23,23,23,
/* 24 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,25,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,24,54,23,23,23,23,23,23,23,23,23,23,23,23,
/* 25 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,26,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,24,54,23,23,23,23,23,23,23,23,23,23,23,23,
/* 26 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,27,30,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,24,54,23,23,23,23,23,23,23,23,23,23,23,23,
/* 27 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,28,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,24,54,23,23,23,23,23,23,23,23,23,23,23,23,
/* 28 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,29,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,31,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,31,56,23,23,23,23,23,23,23,23,23,23,31,23,
/* 29 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,24,54,23,23,23,23,23,23,23,23,23,23,23,23,
/* 30 */ 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,29,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{23,23,23,23,23,23,23,23,23,31,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,31,56,23,23,23,23,23,23,23,23,23,23,31,23,
/* 31 */ 23,23,32,32,32,32,32,32,32,32,32,32, 0,23,23,23,23,23,23,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
		 23,23,23,23,23,23,23,23,23,23,23,23,23},
		{36,36,36,36,36,36,36,36,36,45,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,45,58,36,36,36,36,36,36,36,36,36,36,34,36,
/* 32 */ 36,36,33,33,33,33,33,33,33,33,33,33,44,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36},
		{36,36,36,36,36,36,36,36,36,45,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,45,58,36,36,36,36,36,36,36,36,36,36,34,36,
/* 33 */ 36,36,33,33,33,33,33,33,33,33,33,33,44,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36},
		{36,36,36,36,36,36,36,36,36,35,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,35,58,36,36,36,36,36,36,36,36,36,36,35,36,
/* 34 */ 36,36,32,32,32,32,32,32,32,32,32,32, 0,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36},
		{36,36,36,36,36,36,36,36,36,35,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,35,58,36,36,36,36,36,36,36,36,36,36,35,36,
/* 35 */ 36,36,32,32,32,32,32,32,32,32,32,32, 0,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36},
		{36,36,36,36,36,36,36,36,36,35,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,35,58,36,36,36,36,36,36,36,36,36,36,35,36,
/* 36 */ 36,36,36,36,36,36,36,36,36,36,36,36, 0,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 37 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1,38, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 38 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		 39, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 39 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 40 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,41, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 41 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1,42, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 42 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 1, 1, 1, 1, 1, 1, 1,43, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 43 */  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 44 */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
		  2, 1,10, 1, 1, 1, 1,15, 1, 1,20, 1, 1, 1, 1,37,40, 1, 1, 1, 1, 1, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		{36,36,36,36,36,36,36,36,36,45,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,45,61,36,36,36,36,36,36,36,36,36,36,34,36,
/* 45 */ 36,36,36,36,36,36,36,36,36,36,36,36,44,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36}
	};

static void add_linenum( TEXT *txt, char state, int start, int len, int4 line_number );
static void split_linenum( line_num *old );
static void int_to_string( char *s, int4 n );

/* init_hash initializes hash to NULL */

void init_hash(void)

{
	int i;	/* Index */

	for ( i = 0; i < HASH_LIMIT; i++ )
		hash[i] = NULL;
}

/* find_linenum will use "machine" to find line numbers in txt->text */

void find_linenum( TEXT *txt )

{
	char init_state=0;	/* State machine should be initialized to */
	char state=0;		/* State machine is in */
	char start_pos=0;	/* Position in line where line number starts */
	char i=0;			/* Counter */

	int4 line_number=0;	/* Holds line number */

	state = init_state = start_state;
	start_state = 0;

	/* This is the driver loop for "machine" */
	for ( i = 0; txt->text[i]; i++ )
	{
		state = machine[state][txt->text[i]];
		switch ( state )
		{
			case	0:
			case	1:
			case	2:
			case   10:
			case   15:
			case   20:
			case   37:
			case   40:	if ( line_number )
						{
							add_linenum( txt, init_state, start_pos,
								i - 1 - start_pos, line_number );
						   	line_number = 0;
						}
						break;

			case	7:
			case   32:	start_pos = i;
			case    8:
			case   33:	line_number = (line_number * 10) + (txt->text[i] - ZERO);
					  	break;

			case   34:
			case   44:	add_linenum( txt, init_state, start_pos,
							i - start_pos, line_number );
					  	line_number = 0;
					  	break;

			/* States 50..58 are not realy states, but pseudo states used when
			   a continuation occurs */
			case   50:
			case   51:
			case   52:	if ( i == (vse_edit_width - 1) )
							switch ( state )
							{
								case	50:	start_state = 6;
											break;

								case	51:	start_state = 19;
											break;

								default	  :	start_state = 22;
							}
						state = 0;
						break;

			case   53:
			case   54:
			case   55:
			case   56:	if ( i == (vse_edit_width - 1) )
							switch ( state )
							{
								case	53:	start_state = 22;
											break;

								case	54:	start_state = 23;
											break;

								case	55:	start_state = 24;
											break;

								default	  :	start_state = 31;
							}
						state = 23;
						break;

			case   57:
			case   58:	if ( i == (vse_edit_width - 1) )
						{
							if ( state == 57 )
								start_state = 31;
							else
								start_state = 35;
						}
						state = 36;
						break;

			/* In states 60 and 61, if the exclamation mark is in the correct
			   position, we have a valid line number, otherwise, it is junk */
			case   60:	if ( i == (vse_edit_width - 1) )
						{
							add_linenum( txt, init_state, start_pos,
								i - start_pos, line_number );
							line_number = 0;
						}
						state = 0;
						break;

			case   61:	if ( i == (vse_edit_width - 1) )
						{
							add_linenum( txt, init_state, start_pos,
								i - start_pos, line_number );
							line_number = 0;
							start_state = 35;
						}
						state = 36;
						break;

			default	 :	break;
		}
	}

	switch ( state )
	{

		/* If the end of line was found while in one of these states, there
		   is a valid line number */
		case	 8:
		case	 9:
		case	33:
		case	45:	add_linenum( txt, init_state, start_pos, i - start_pos,
						line_number );
					break;

		default	  : break;
	}
}


/* add_linenum will add an entry for the line number into the list */

static void add_linenum( TEXT *txt, char state, int start, int len, int4 line_number )

{
	line_num *tmp=NULL;		/* Temporary pointer to line_num element */

	tmp = (line_num *) malloc( sizeof( line_num ));
	tmp->last = NULL;
	tmp->line = txt;
	tmp->start_state = state;
	tmp->start_pos = start;
	tmp->length = len;
	tmp->branch = line_number;
	tmp->old_branch = line_number;
	tmp->next = hash[line_number%HASH_LIMIT];
	if ( hash[line_number%HASH_LIMIT] )
		hash[line_number%HASH_LIMIT]->last = tmp;
	hash[line_number%HASH_LIMIT] = tmp;
}


/* update_linenum will update a line number referenced in a BASIC statement
   to a new value when renumbering occurs */

void update_linenum( int4 from, int4 to )

{
	int i;							/* Index */
	int white_space=0;				/* Position of first non space from end of
									   the string */
	char *build;					/* New string with updated line numbers */
	char *str_to;					/* String representation of "to" */
	char *str_from;					/* String representation of "from" */

	line_num *current=hash[from%HASH_LIMIT];	/* Pointer to list */
	line_num *prev_ptr=NULL;		/* Pointer to elements preceding current */
	line_num *next_ptr=NULL;		/* Pointer to next element */

	str_to = (char *) calloc( NUM_LEN, sizeof( char ) );
	str_from = (char *) calloc( NUM_LEN, sizeof( char ) );
	int_to_string( str_to, to );
	int_to_string( str_from, from );

	while ( current )
	{
		if ( current->old_branch == from )
		{
			build = (char *) calloc( MAX_LINE_LEN, sizeof( char ) );

			/* Copy chars from text up to line number into build */
			strncpy( build, current->line->text, current->start_pos );

			/* If the current length of the line number is greater than or
			   equal to the length of the new line number or if the new line
			   number's length is greater than the current length and the
			   new line number can fit on the line, insert it */
			if ( (current->length >= (int)strlen( str_to )) ||
				((current->length < (int)strlen( str_to )) &&
				((int)(strlen( current->line->text ) +
				 strlen( str_to ) - current->length) < vse_edit_width)) )
			{

				/* Add string rep. of new line number to build */
				strncat( build, str_to, strlen( str_to ) );

				/* If the current length is greater than the length of the new
				   line number, i.e. 0400 changed to 500, pad with spaces */
				if ( current->length > (int)strlen( str_to ) )
					strncat( build, SPACES, current->length - strlen( str_to ) );

				/* Skip over old line number in the text and add the rest
			   	   of text to build */
				strncat( build, &(current->line->text
					[current->start_pos+current->length]),
			 		strlen( &(current->line->text
					[current->start_pos+current->length]) ) );
				over_text( current->line, build );
				if ( current->length < (int)strlen( str_to ) )
				{
					for ( i = 0; i < HASH_LIMIT; i++ )
					{
						prev_ptr = hash[i];
						while ( prev_ptr )
						{
							if ( (prev_ptr->line == current->line) &&
								 (prev_ptr->start_pos > current->start_pos) )
								prev_ptr->start_pos += (strlen( str_to ) -
									current->length);
							prev_ptr = prev_ptr->next;
						}
					}
					current->length = strlen( str_to );
				}
				current->branch = to;
				next_ptr = current->next;

				/* If the two hash values are equal, there is no need to move
				   the element */
				if ( (from%HASH_LIMIT) != (to%HASH_LIMIT) )
				{

					/* Remove the element from the current hash value */
					if ( current->next )
						current->next->last = current->last;
					if ( current->last )
						current->last->next = current->next;
					else
						hash[from%HASH_LIMIT] = current->next;

					/* Insert the element into the new hash value */
					current->last = NULL;
					current->next = hash[to%HASH_LIMIT];
					if ( hash[to%HASH_LIMIT] )
						hash[to%HASH_LIMIT]->last = current;
					hash[to%HASH_LIMIT] = current;
				}

				/* Set current to the next element in the current hash value */
				current = next_ptr;
			}

			/* The new line number may not fit without splitting the line, so
			   see if there are enough white spaces to accomodate the new line
			   number, if so insert it, if not split the line */
			else
			{
				if ( (int)strlen( current->line->text ) == vse_edit_width )
				{

					/* Find out how many white spaces are at the end of the
					   line */
					if ( current->line->text[vse_edit_width-1] == '!' )
						for ( white_space = vse_edit_width;
							(current->line->text[white_space-2] == ' ') &&
							(white_space > 2); white_space-- );
					else
						for ( white_space = vse_edit_width;
							(current->line->text[white_space-1] == ' ') &&
							white_space; white_space-- );
					if ( vse_edit_width - white_space >=
						 (int)strlen( str_to ) - current->length )
					{

						/* Added line number to build */
						strncat( build, str_to, strlen( str_to ) );

						/* Added the rest of text minus the amount of white
						   spaces that the new line number takes up */
						strncat( build, &(current->line->text
							[current->start_pos+current->length]),
							strlen( &(current->line->text
							[current->start_pos+current->length]) ) - 
							(strlen( str_to ) - current->length) );

						/* Added the exclamation mark to the end of build */
						if ( current->line->text[vse_edit_width-1] == '!' )
							build[vse_edit_width-1] = '!';
						over_text( current->line, build );
						for ( i = 0; i < HASH_LIMIT; i++ )
						{
							prev_ptr = hash[i];
							while ( prev_ptr )
							{
								if ( (prev_ptr->line == current->line) &&
									 (prev_ptr->start_pos > current->start_pos) )
								{
									
									if ( (prev_ptr->start_pos+prev_ptr->length) == vse_edit_width )
										prev_ptr->length -= ( strlen( str_to ) -
											current->length );
									prev_ptr->start_pos += (strlen( str_to ) -
										current->length);
								}
								prev_ptr = prev_ptr->next;
							}
						}
						current->length = strlen( str_to );
						current->branch = to;
						next_ptr = current->next;

						/* If the two hash values are equal, there is no need
						   to move the element */
						if ( (from%HASH_LIMIT) != (to%HASH_LIMIT) )
						{

							/* Remove the element from the current hash value */
							if ( current->next )
								current->next->last = current->last;
							if ( current->last )
								current->last->next = current->next;
							else
								hash[from%HASH_LIMIT] = current->next;

							/* Insert the element into the new hash value */
							current->last = NULL;
							current->next = hash[to%HASH_LIMIT];
							if ( hash[to%HASH_LIMIT] )
								hash[to%HASH_LIMIT]->last = current;
							hash[to%HASH_LIMIT] = current;
						}

						/* Set current to the next element in the current hash
						   value */
						current = next_ptr;
					}

					/* Not enough white spaces to acommodate line number */
					else
					{
						split_linenum( current );
						current = hash[from%HASH_LIMIT];
					}
				}

				/* New line number will push the length of line past
				   vse_edit_width */
				else
				{
					split_linenum( current );
					current = hash[from%HASH_LIMIT];
				}
			}

			/* Free dynamically allocated storage space */
			free( build );
		}
		else
			current = current->next;
	}

	free( str_to );
	free( str_from );
}


/* split_linenum will split a line in two if the new line number forces the
   length of the line to exceed vse_edit_width */

static void split_linenum( line_num *old )

{
	TEXT *first_part=old->line;		/* Fisrt part of text */
	TEXT *second_part;				/* Second part of text */

	char *first;					/* First string */
	char *second;					/* Second string */

	first = (char *) calloc( MAX_LINE_LEN, sizeof( char ) );
	second = (char *) calloc( MAX_LINE_LEN, sizeof( char ) );

	/* If the original line starts with a *, the new line must also */
	if ( old->line->text[0] == '*' )
		strcpy ( second, "* " );

	/* Build the new string */
	if ( old->line->text[vse_edit_width-1] == '!' )
	{
		strncat( second, &(old->line->text[old->start_pos]),
			strlen( &(old->line->text[old->start_pos]) )-1 );
		strncat( second, SPACES, vse_edit_width-strlen( second )-1 );
		second[vse_edit_width-1] = '!';
	}
	else
	{
		strncat( second, &(old->line->text[old->start_pos]),
			strlen( &(old->line->text[old->start_pos]) ) );
		strncat( second, SPACES, vse_edit_width-strlen( second ) );
	}
	second_part = new_text( second );

	/* Insert the new line into the linked list of lines */
	second_part->prev = first_part;
	second_part->next= first_part->next;
	first_part->next->prev = second_part;
	first_part->next = second_part;

	second_part->lineno = first_part->lineno + 1;

	/* Modify the original string to contain everything up to the position
	   of the embedded line number */
	strncat( first, old->line->text, old->start_pos );
	strncat( first, SPACES, vse_edit_width-strlen( first )-1 );
	first[vse_edit_width-1] = '!';
	over_text( first_part, first );

	/* Delete the old references for the line and look at the two new lines
	   to find embedded line numbers */
	start_state = old->start_state;
	delete_linenum( old->line->lineno );
	find_linenum( first_part );
	find_linenum( second_part );
}


/* delete_linenum will delete an entry if the line number of BASIC code that
   contains a line number reference is deleted */

void delete_linenum( int4 lineno )

{
	int i;							/* Index */
	line_num *current=NULL;			/* Pointer to current element of list */
	line_num *next=NULL;			/* Pointer to next element in list */

	for ( i = 0; i < HASH_LIMIT; i++ )
	{
		current = hash[i];
		while ( current )
		{
			if ( current->line->lineno == lineno )
			{
				if ( current->next )
					current->next->last = current->last;
				if ( current->last )
					current->last->next = current->next;
				else
					hash[i] = current->next;
				next = current->next;
				free( current );
				current = next;
			}
			else
				current = current->next;
		}
	}
}

/* int_to_string converts a integer into it's string representation */

static void int_to_string( char *s, int4 n ) 

{
	int i;				/* Counter */
	int j;				/* Counter */
	int length=0;		/* Length of string that holds converted number */

	char tmp[NUM_LEN];	/* String to hold converted number */

	/* Convert the number to a string in reverse order */
	i = 0;
	do
	{
		tmp[i++] = n % 10 + ZERO;
	} while ( (n /= 10) > 0 );
	tmp[i] = '\0';

	length = strlen( tmp );

	/* Now reverse the string that represents the number in reverse order */
	for ( i = length - 1, j = 0; i >= 0; )
		s[j++] = tmp[i--];
}

/* update_branch will store the new branch line number into old_branch so
   that when renumbering occurs again, it will look for the correct line
   numbers */

void update_branch(void)

{
	int i;						/* Index */
	line_num *current=NULL;		/* Pointer into list */

	for ( i = 0; i < HASH_LIMIT; i++ )
	{
		current = hash[i];
		while ( current )
		{
			current->old_branch = current->branch;
			current = current->next;
		}
	}
}

/* free_linenum will delete the list and free it's memory */

void free_linenum(void)

{
	int i;							/* Index */
	line_num *current=NULL;			/* Pointer to current element */
	line_num *next=NULL;			/* Pointer to next element in list */

	for ( i = 0; i < HASH_LIMIT; i++ )
	{
		current = hash[i];
		while ( current )
		{
			next = current->next;
			free( current );
			current = next;
		}
		hash[i] = NULL;
	}
}
/*
**	History:
**	$Log: vsebasic.c,v $
**	Revision 1.7  1996/09/03 22:23:59  gsl
**	drcs update
**	
**
**
*/
