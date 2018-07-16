/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
#ifndef MOVEBIN_INCLUDED
#define MOVEBIN_INCLUDED

#include <string.h>

/*
	GETBIN and PUTBIN are macros that get/put a 2 or 4 byte binary
        from/to a possible unalign memory location.

	D = long/short ptr to destination.
	S = long/short ptr to source.
	L = length; it must be 2 or 4.

	If mem is aligned then time is saved by doing a simple assignment.
*/

#define PUTBIN(D,S,L) { memcpy((D),(S),(L)); }
#define GETBIN(D,S,L) { memcpy((D),(S),(L)); } 

#endif 	/* MOVEBIN_INCLUDED	*/

/*
**	History:
**	$Log: movebin.h,v $
**	Revision 1.9  1996-08-19 18:32:32-04  gsl
**	drcs update
**
**
**
*/
