			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		crt.h
**
**	Purpose:	To hold CRT defines
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef CRT_H
#define CRT_H

#define MAX_CRT_FILES	8

EXT char crt_file[MAX_CRT_FILES][40];							/* the local filename of the screen	*/
EXT char crt_pfkey[MAX_CRT_FILES][40];							/* the local name of the pfkey field	*/
EXT char crt_status[MAX_CRT_FILES][40];							/* the local name of the ret status	*/
EXT char crt_cursor[MAX_CRT_FILES][40];							/* the local name of the cursor position*/
EXT char crt_relative[MAX_CRT_FILES][40];						/* the local name of the relative key	*/
EXT int  crt_prime_rec[MAX_CRT_FILES];							/* The array item of primary crt record.*/

EXT int cur_crt		INIT_ZERO;							/* The current crt being referenced.	*/
EXT int crt_fcount	INIT_ZERO;							/* How many crt files are there?	*/

#define MAX_CRT_RECORDS		40

EXT char crt_record[MAX_CRT_RECORDS][40];						/* the list of possible crt records	*/
EXT int  crt_record_size[MAX_CRT_RECORDS];						/* the size of these records		*/
EXT int  crt_record_count INIT_ZERO;


#endif /* CRT_H */
