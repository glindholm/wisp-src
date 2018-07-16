/*
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
*/


/*
**	File:		crt.h
**
**	Purpose:	To hold CRT defines
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
/*
**	History:
**	$Log: crt.h,v $
**	Revision 1.7  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.6  2003/02/04 17:33:20  gsl
**	fix copyright header
**	
**	Revision 1.5  1996/08/31 01:56:02  gsl
**	drcs update
**	
**
**
*/
