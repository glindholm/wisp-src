/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
Standard Linked list functions.
An LL struct should be declared as the first element
of a struct, and then the actual structs can be passed
to these functions.
------*/
#include <stdio.h>
#include "ll.h"


/*----
Atomic functions for moving backward and forward thru a linked list.
------*/

/*----
Return Next after the base

Returns NULL if the base is NULL or there is no next.
------*/
void *ll_next(LL *base)
{
    	if(base == 0)
		return(NULL);
	return((void*) NEXT(base));
}
/*----
Return Previous before the base.

Returns NULL if the base is NULL or there is no previous.
------*/
void *ll_prev(LL *base)
{
    	if(!base)
		return(NULL);
	return((void*) PREV(base));
}

/*----
Atomic functions for adding before and after the passed base
returns the address of base.
------*/
/*----
Insert after base.

Returns base
------*/
void *ll_add_next(LL *base, LL *l_new)
{
    	LLTYPE *below;

    	if(base == 0)
		{
		return((void*) l_new);
		}
	if((below = NEXT(base)) != NULL)
	    PREV(below) = l_new;
	NEXT(l_new) = NEXT(base);
	NEXT(base) = l_new;
	PREV(l_new) = base;
    	return((void*)base);
}
/*----
Insert before base

Returns new base
------*/
void *ll_add_prev(LL *base, LL *l_new)
{
    	LLTYPE *above;

    	if(base == 0)
		{
		return((void*) l_new);
		}
	if((above = PREV(base)) != NULL)
	    NEXT(above) = l_new;
	PREV(l_new) = PREV(base);
	PREV(base) = l_new;
	NEXT(l_new) = base;
    	return(ll_first(l_new));
}

/*----
Find the last entry in the list or NULL if none
------*/
void *ll_last(LL *base)
{
    LLTYPE *newbase;
    while((newbase = (LLTYPE*) ll_next(base)) != NULL)
	base = newbase;
    return((void*)base);
}

/*----
Returns the first entry in the list or NULL if none
------*/

void *ll_first(LL *base)
{
    LLTYPE *newbase;
    while((newbase = (LLTYPE*) ll_prev(base)) != NULL)
	base = newbase;
    return((void*)base);
}

/*----
Append to the end of a linked list that starts at base

Return a pointer to base
------*/
void *ll_append(LL *base, LL *l_new)
{
    	LLTYPE *newbase;

    	if(base == 0)
		return((void*) l_new);
    	newbase = (LLTYPE*) ll_last(base);
    	ll_add_next(newbase,l_new);
    	return((void*) base);
}

/*----
Execute a function on all lls from base to end of the list.
Execution stops when the called function returns a non zero value.
Or when the list is exhausted. Returns a pointer to the element
if the function returned non-zero.
A return of NULL indicates either all elements processed, or
there were no elements to process.
------*/
void *ll_select(void *base, int (*func)())
{
	LLTYPE *nbase;

	while(base != 0)
		{
		nbase = (LLTYPE*) ll_next(base);
		if((*func)(base) != 0)
			break;
		base = nbase;
		}
	return((void*) base);
}
/*----
Execute a function on all lls from base to end of the list.
Like ll_select, but ignores return values
------*/
void *ll_all(void *base, void (*func)())
{
	LLTYPE *nbase;

	while(base != 0)
	{
		nbase = (LLTYPE*) ll_next((LL *)base);
		(*func)(base);
		base = nbase;
	}
	return(base);
}
/*----
Add to the list in sorted order. Returns the base of the list.
------*/
void *ll_add_sorted(LL *base, LL *l_new, int (*compare)(LL *,LL *))
{
	LLTYPE *nbase;

    	base = (LLTYPE*) ll_first(base);
	if(base == 0)
		{
		return((void*) l_new);
		}
    	while(1)
		{
		if( (*compare)(base,l_new) )
	    		{
	    		if((nbase = (LLTYPE*)ll_prev(base)) != 0)
				ll_add_next(nbase,l_new);
	    		else
				ll_add_prev(base,l_new);
	    		break;
	    		}
		if( ( nbase = (LLTYPE*) ll_next(base)) == 0)
	    		{
	    		ll_add_next(base,l_new);
	    		break;
	    		}
		base = nbase;
		}
    	return(ll_first(base));
}


/*----
Bubble sort on linked list elements.
------*/
void *ll_bubble_sort(LL *base, int (*compare) (/* ??? */))
{
    int changing;
    LLTYPE *f1,*f2;

    changing = 1;
    while(changing)
	{
	changing = 0;
	f1 = base;
	while( (f2 = (LLTYPE*) ll_next(f1)) != NULL )
	    {
	    if( (*compare)(f1,f2) != 0)
		{
		changing = 1;
		ll_swap(f1,f2);
		if(base == f1)
		    base = f2;
		}
	    f1 = f2;
	    }
	}
    return((void*) base);
}

/*----
Swap two elements
------*/
void ll_swap(LL *one, LL *two)
{
    if(PREV(one))
	NEXT((PREV(one))) = two;
    if(NEXT(two))
	PREV((NEXT(two)))= one;
    PREV(two) = PREV(one);
    NEXT(one) = NEXT(two);
    PREV(one) = two;
    NEXT(two) = one;
}

/*----
Select a field excluding the starting base
Process the list as if it were circular. in either prev or next mode
when code == -1 use prev or 1 use next.
------*/
static void *ll_circ_select(LL *base, int (*select) (/* ??? */), int code)
{
	LLTYPE *nbase;

	nbase = base;
	while(1)
		{
		nbase = (code == -1) 
			? (LLTYPE*) ll_circ_prev(nbase)
			: (LLTYPE*) ll_circ_next(nbase);
		if(nbase == 0)
			break;
		if(nbase == base)
			{
			nbase = 0;
			break;
			}
		if((*select)(nbase))
			break;
		}
	return((void*) nbase);
}

/*----
Return next link as if list were circular
------*/
void *ll_circ_next(LL *base)
{
	LLTYPE *nbase;
	
	nbase = (LLTYPE*) ll_next(base);
	if(nbase == 0)
		nbase = (LLTYPE*) ll_first(base);
	return((void*) nbase);
}


/*----
Return prev link as if list were circular
------*/
void *ll_circ_prev(LL *base)
{
	LLTYPE *nbase;
	
	nbase = (LLTYPE*) ll_prev(base);
	if(nbase == 0)
		nbase = (LLTYPE*) ll_last(base);
	return((void*) nbase);
}



/*----
Select a field excluding the starting base
Process the list as if it were circular.
------*/
void *ll_next_select(LL *base, int (*select) (/* ??? */))
{
	return(ll_circ_select(base,select,1));
}

/*----
Select a field excluding the starting base
Process the list as if it were circular.
------*/
void *ll_prev_select(LL *base, int (*select) (/* ??? */))
{
	return(ll_circ_select(base,select,-1));

}

/*----
Unlink an entry from the linked list. Return the new top of the list.
------*/
void *ll_unlink(LL *base)
{
    LLTYPE *prev;
    LLTYPE *next;
    LLTYPE *first;

    if(base == 0)
	return(NULL);
    prev = PREV(base);
    next = NEXT(base);
    if(prev)
	{
	NEXT(prev) = next;
	first = ll_first(prev);
	}
    else
	first = next;
    if(next)
	PREV(next) = prev;
    PREV(base) = 0;
    NEXT(base) = 0;
    return((void*) first);
}
/*
**	History:
**	$Log: ll.c,v $
**	Revision 1.8  2003/02/20 19:29:55  gsl
**	fix -Wall warnings
**	
**	Revision 1.7  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.6  2002/10/17 17:56:18  gsl
**	Rename variables new to l_new
**	
**	Revision 1.5  1997/10/02 14:44:20  gsl
**	fix warnings
**	
**	Revision 1.4  1996-10-02 18:10:28-04  gsl
**	Fix ll_all() arguments to fix warnings
**
**	Revision 1.3  1996-10-02 09:06:29-07  gsl
**	Add standard headers
**	Fix prototypes
**
**
**
*/
