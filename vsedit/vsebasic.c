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

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/

#include <stdio.h>
#include "vseglb.h"

#define SPACES	"                                                                                "
#define ZERO			48
#define NUM_LEN			30
#define MAX_LINE_LEN	vse_text_width+1

static char start_state=0;	/* Starting state of machine depending on context */

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
/* 45 */ 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,
		 36,36,36,36,36,36,36,36,36,36,36,36,36}
	};

void find_linenum( TEXT *txt );
void add_linenum( TEXT *txt, char state, int start, int len, long line_number );
void update_linenum( long from, long to );
void split_linenum( line_num *old );
void delete_linenum( long lineno );
void itoa( char *s, long n );
void update_branch();
void free_linenum();


/* find_linenum will use "machine" to find line numbers in txt->text */

void find_linenum( TEXT *txt )

{
	char init_state=0;	/* State machine should be initialized to */
	char state=0;		/* State machine is in */
	char start_pos=0;	/* Position in line where line number starts */
	char i=0;			/* Counter */

	long line_number=0;	/* Holds line number */

	state = init_state = start_state;
	start_state = 0;

	/* This is the driver loop for "machine" */
	for ( i = 0; txt->text[i] != NULL; i++ )
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
			case   52:	if ( i == (vse_text_width - 1) )
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
			case   56:	if ( i == (vse_text_width - 1) )
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
			case   58:	if ( i == (vse_text_width - 1) )
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
			case   60:	if ( i == (vse_text_width - 1) )
						{
							add_linenum( txt, init_state, start_pos,
								i - start_pos, line_number );
							line_number = 0;
						}
						state = 0;
						break;

			case   61:	if ( i == (vse_text_width - 1) )
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

void add_linenum( TEXT *txt, char state, int start, int len, long line_number )

{
	line_num *tmp=NULL;		/* Temporary pointer to line_num element */

	tmp = (line_num *) malloc( sizeof( line_num ));
	tmp->line = txt;
	tmp->start_state = state;
	tmp->start_pos = start;
	tmp->length = len;
	tmp->branch = line_number;
	tmp->old_branch = line_number;
	tmp->next = head_linenum;
	head_linenum = tmp;
}


/* update_linenum will update a line number referenced in a BASIC statement
   to a new value when renumbering occurs */

void update_linenum( long from, long to )

{
	int white_space=0;				/* Position of first non space from end of
									   the string */
	char *build;					/* New string with updated line numbers */
	char *str_to;					/* String representation of "to" */
	char *str_from;					/* String representation of "from" */

	line_num *current=head_linenum;	/* Pointer to list */
	line_num *prev_ptr=NULL;		/* Pointer to elements preceding current */

	str_to = (char *) calloc( NUM_LEN, sizeof( char ) );
	str_from = (char *) calloc( NUM_LEN, sizeof( char ) );
	itoa( str_to, to );
	itoa( str_from, from );

	while ( current )
	{
		if ( current->old_branch == from )
		{
			build = (char *) calloc( MAX_LINE_LEN, sizeof( char ) );

			/* Copy chars from text up to line number into build */
			strncpy( build, current->line->text, current->start_pos );

			if ( (current->length >= strlen( str_to )) ||
				((current->length < strlen( str_to )) &&
				((strlen( current->line->text ) +
				strlen( str_to ) - current->length) < vse_text_width)) )
			{

				/* Add string rep. of new line number to build */
				strncat( build, str_to, strlen( str_to ) );

				/* If the current length is greater than the length of the new
				   line number, i.e. 0400 changed to 500, pad with spaces */
				if ( current->length > strlen( str_to ) )
					strncat( build, SPACES, current->length - strlen( str_to ) );

				/* Skip over old line number in the text and add the rest
			   	   of text to build */
				strncat( build, &(current->line->text
					[current->start_pos+current->length]),
			 		strlen( &(current->line->text
					[current->start_pos+current->length]) ) );
				over_text( current->line, build );
				if ( current->length < strlen( str_to ) )
				{
					for ( prev_ptr = head_linenum; prev_ptr != current;
						  prev_ptr = prev_ptr->next )
						if ( prev_ptr->line->lineno == current->line->lineno )
							prev_ptr->start_pos += (strlen( str_to ) - current->length);
					current->length = strlen( str_to );
				}
				current->branch = to;
				current = current->next;
			}
			else
			{
				if ( strlen( current->line->text ) == vse_text_width )
				{

					/* Find out how many white spaces are at the end of the
					   line */
					if ( current->line->text[vse_text_width-1] == '!' )
						for ( white_space = vse_text_width;
							(current->line->text[white_space-2] == ' ') &&
							(white_space > 2); white_space-- );
					else
						for ( white_space = vse_text_width;
							(current->line->text[white_space-1] == ' ') &&
							white_space; white_space-- );
					if ( vse_text_width - white_space >=
						 strlen( str_to ) - current->length )
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
						if ( current->line->text[vse_text_width-1] == '!' )
							build[vse_text_width-1] = '!';
						over_text( current->line, build );
						for ( prev_ptr = head_linenum; prev_ptr != current;
							  prev_ptr = prev_ptr->next )
							if ( prev_ptr->line->lineno == current->line->lineno )
							{
								if ( (prev_ptr->start_pos+prev_ptr->length) ==
									vse_text_width )
									prev_ptr->length -= ( strlen( str_to ) -
										current->length );
								prev_ptr->start_pos += (strlen( str_to ) - current->length);
							}
						current->length = strlen( str_to );
						current->branch = to;
						current = current->next;
					}

					/* Not enough white spaces to acommodate line number */
					else
					{
						split_linenum( current );
						current = head_linenum;
					}
				}

				/* New line number will push the length of line past
				   vse_text_width */
				else
				{
					split_linenum( current );
					current = head_linenum;
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
   length of the line to exceed vse_text_width */

void split_linenum( line_num *old )

{
	TEXT *first_part=old->line;		/* Fisrt part of text */
	TEXT *second_part;				/* Second part of text */
	TEXT *new_text();

	char *first;					/* First string */
	char *second;					/* Second string */

	first = (char *) calloc( MAX_LINE_LEN, sizeof( char ) );
	second = (char *) calloc( MAX_LINE_LEN, sizeof( char ) );

	/* Build the new string */
	if ( old->line->text[vse_text_width-1] == '!' )
	{
		strncat( second, &(old->line->text[old->start_pos]),
			strlen( &(old->line->text[old->start_pos]) )-1 );
		strncat( second, SPACES, vse_text_width-strlen( second )-1 );
		second[vse_text_width-1] = '!';
	}
	else
	{
		strncat( second, &(old->line->text[old->start_pos]),
			strlen( &(old->line->text[old->start_pos]) ) );
		strncat( second, SPACES, vse_text_width-strlen( second ) );
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
	strncat( first, SPACES, vse_text_width-strlen( first )-1 );
	first[vse_text_width-1] = '!';
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

void delete_linenum( long lineno )

{
	line_num *last=NULL;			/* Pointer to last looked at element of
									   list */
	line_num *current=head_linenum;	/* Pointer to current element of list */
	line_num *next=NULL;			/* Pointer to next element in list */

	while ( current )
	{
		if ( current->line->lineno == lineno )
		{
			if ( last )
				last->next = current->next;
			else
				head_linenum = current->next;
			next = current->next;
			free( current );
			current = next;
		}
		else
		{
			last = current;
			current = current->next;
		}
	}
}

/* itoa converts a integer into it's string representation */

void itoa( char *s, long n ) 

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

void update_branch()

{
	line_num *current=head_linenum;		/* Pointer into list */

	while ( current )
	{
		current->old_branch = current->branch;
		current = current->next;
	}
}

/* free_linenum will delete the list and free it's memory */

void free_linenum()

{
	line_num *next=NULL;			/* Pointer to next element in list */

	while ( head_linenum )
	{
		next = head_linenum->next;
		free( head_linenum );
		head_linenum = next;
	}
}
