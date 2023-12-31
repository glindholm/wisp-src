/* 
	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
*/

/*
**	File:		wt_locks.h
**
**	Project:	WISP/TRAN
**
**
**
**	Purpose:	Record locking
**
*/

#ifndef wt_locks_H
#define wt_locks_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int gen_unlocks(void);
void set_lock_holder_id(int fnum, int col);
void if_file_clear_lock_holder_id(int fnum, int col);
void clear_lock_holder_id(int col);
void unlock_record(int col);

#endif /* wt_locks_H */

/*
**	History:
**	$Log: wt_locks.h,v $
**	Revision 1.1  1998/06/09 14:06:06  gsl
**	Initial revision
**	
**
**
**
*/
