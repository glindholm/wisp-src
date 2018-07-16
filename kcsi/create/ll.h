/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef _LL_H
#define _LL_H

#define	PROTOTYPING		/* Comment out for porting */

typedef struct _ll{
    struct _ll *prev;
    struct _ll *next;
    }LL;

/* #define	SUPERSAFE	1	*/	/* How safe do ya wanna be*/
#ifdef	SUPERSAFE 				/*Awesomely safe*/
typedef struct _lls{
	LL	ll;
	}LLS;

#define	LLTYPE	LLS

#define	NEXT(base)	(LLS*) base->ll.next
#define	PREV(base)	(LLS*) base->ll.prev
#else 						/*Pretty darn Safe */
#define	LLTYPE	LL

#define	NEXT(base)	base->next
#define	PREV(base)	base->prev

#endif	/*SUPERSAFE*/




#define NULL_LL {NULL,NULL}

#define ll_add(where,new)	ll_add_next(where,new)

#ifdef	PROTOTYPING

void	ll_swap(LLTYPE*,LLTYPE*);

void	*ll_bubble_sort(LLTYPE*,int(*)()),
	*ll_all(void *base, void(*func)()),
	*ll_next_select(LLTYPE*,int(*)()),
	*ll_prev_select(LLTYPE*,int(*)()),
	*ll_add_sorted(LLTYPE*,LLTYPE*,int(*)(LLTYPE*,LLTYPE*)),
	*ll_next(LLTYPE*),
	*ll_circ_next(LLTYPE*),
	*ll_circ_prev(LLTYPE*),
	*ll_prev(LLTYPE*),
	*ll_first(LLTYPE*),
	*ll_last(LLTYPE*),
	*ll_unlink(LLTYPE*),
	*ll_add_next(LLTYPE*,LLTYPE*),
	*ll_add_prev(LLTYPE*,LLTYPE*),
	*ll_append(LLTYPE*,LLTYPE*)
	;
#else

void	ll_swap();

void	*ll_bubble_sort(),
	*ll_all(),
	*ll_next_select(),
	*ll_prev_select(),
	*ll_add_sorted(),
	*ll_next(),
	*ll_circ_next(),
	*ll_circ_prev(),
	*ll_prev(),
	*ll_first(),
	*ll_last(),
	*ll_unlink(),
	*ll_add_next(),
	*ll_add_prev(),
	*ll_append()
	;
#endif



#endif	/* _LL_H */
/*
**	History:
**	$Log: ll.h,v $
**	Revision 1.7  1999-01-19 11:11:42-05  gsl
**	fix warning
**
**	Revision 1.6  1997-10-02 10:44:06-04  gsl
**	FIx warnings
**
**	Revision 1.5  1997-10-02 10:14:46-04  gsl
**	Move from wisp/kcsi/common to wisp/kcsi/create
**
**	Revision 1.4  1996-10-02 18:11:27-04  gsl
**	fix ll_all() and ll_add_sorted() prototypes
**
**	Revision 1.3  1996-09-17 16:34:13-07  gsl
**	drcs update
**
**
**
*/
