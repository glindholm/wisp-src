/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef _LL_H
#define _LL_H

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

#define ll_add(where,l_new)	ll_add_next(where,l_new)


void	ll_swap(LLTYPE*,LLTYPE*);

void	*ll_bubble_sort(LLTYPE*,int(*)());
void	*ll_all(void *base, void(*func)());
void	*ll_next_select(LLTYPE*,int(*)());
void	*ll_prev_select(LLTYPE*,int(*)());
void	*ll_add_sorted(LLTYPE*,LLTYPE*,int(*)(LLTYPE*,LLTYPE*));
void	*ll_next(LLTYPE*);
void	*ll_circ_next(LLTYPE*);
void	*ll_circ_prev(LLTYPE*);
void	*ll_prev(LLTYPE*);
void	*ll_first(LLTYPE*);
void	*ll_last(LLTYPE*);
void	*ll_unlink(LLTYPE*);
void	*ll_add_next(LLTYPE*,LLTYPE*);
void	*ll_add_prev(LLTYPE*,LLTYPE*);
void	*ll_append(LLTYPE*,LLTYPE*);
void	*ll_select(void *base, int (*func) ());



#endif	/* _LL_H */
/*
**	History:
**	$Log: ll.h,v $
**	Revision 1.7.2.1  2002/11/12 15:56:30  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.9  2002/10/21 18:29:20  gsl
**	cleanup
**	
**	Revision 1.8  2002/10/17 17:56:18  gsl
**	Rename variables new to l_new
**	
**	Revision 1.7  1999/01/19 16:11:42  gsl
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
