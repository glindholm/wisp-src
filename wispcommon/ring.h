/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		ring.h
**
**	Project:	wisp/common
**
**	RCS:		$Source:$
**
**	Purpose:	Ring header
**
*/

#ifndef RING_H
#define RING_H

#ifndef RING_C
#define ring_struct	void
#endif

extern int ring_open(char **id, int esize, int alloc1, int alloc2, int (*ecompare)(), int sorted_flag);
extern int ring_close(ring_struct *ring_ptr);
extern int ring_add(ring_struct *ring_ptr, int position, void *element);
extern int ring_replace(ring_struct *ring_ptr, int position, void *element);
extern int ring_remove(ring_struct *ring_ptr, int position, void *element);
extern int ring_get(ring_struct *ring_ptr, int position, void *element);
extern int ring_find(ring_struct *ring_ptr, void *match, int *position, void *element);
extern int ring_count(ring_struct *ring_ptr, int *count);
extern int ring_push(ring_struct *ring_ptr, void *element);
extern int ring_pop(ring_struct *ring_ptr, void *element);
extern int ring_top(ring_struct *ring_ptr, void *element);
extern int ring_que(ring_struct *ring_ptr, void *element);
extern int ring_unque(ring_struct *ring_ptr, void *element);
extern int ring_front(ring_struct *ring_ptr, void *element);
extern int ring_back(ring_struct *ring_ptr, void *element);
extern char *ring_error(int rc);

#endif /* RING_H */
/*
**	History:
**	$Log: ring.h,v $
**	Revision 1.7  1996-07-09 19:41:49-04  gsl
**	fix newline
**
**	Revision 1.6  1996-07-09 16:41:06-07  gsl
**	fix headrs
**
**
**
*/

