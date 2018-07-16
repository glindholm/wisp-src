static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
	ring:	An multi-use data structure that implements 
			a Linked-List
			a Stack (FILO)
			a Queue (FIFO)

	ROUTINES:
	int ring_open   (idp,esize,alloc1,alloc2,[ecompare],sorted)	- open and initialize a ring
	int ring_close  (id)					- close and destroy a ring

	int ring_add    (id,position,element)			- add an element to ring
	int ring_remove (id,position,[element])			- remove an element from ring
	int ring_get    (id,position,[element])			- get an element from a ring
	int ring_find   (id,match,[positionp],[element])	- find an element from a ring using ecompare
	int ring_replace(id,position,element)			- replace the element 
	int ring_count  (id,count)				- get number of elements in ring
	char *ring_error  (returncode)				- get the error message associated with ring return codes

	int ring_push   (id,element)				- add element to top of stack			ring_add(0)
	int ring_pop    (id,[element])				- get top element and remove it from stack	ring_remove(0)
	int ring_top    (id,[element])				- get top element of stack			ring_get(0)

	int ring_que    (id,element)				- add element to back of queue			ring_add(-1)
	int ring_unque  (id,[element])				- get element from front of queue		ring_remove(0)
	int ring_front  (id,[element])				- look at front element of queue		ring_get(0)
	int ring_back   (id,[element])				- look at back element of queue			ring_get(-1)

	ARGUMENTS:
	char 	**idp; 			- pointer to ring id (to be returned).
	int	esize;			- byte size of an element, 0 for variable length (null terminated).
	int	alloc1, alloc2;		- Primary and secondary allocations in number of elements (ignored if esize==0).
	int	(*ecompare)();		- pointer to element comparision routine for sorted list and ring_find() routine. This
					  may be NULL if list is unsorted and ring_find() is not used. Returns: negative, zero,
					  or positive for less then, equal, or greater then.
	int	sorted;			- 1=sort the list; 0=unsorted.
	char 	*id;			- ring identifier.
	int	position;		- position in ring, may be positive or negative. E.g. 0=first -1=last.
	char	*element;		- pointer to element, this may be NULL if you don't want the element returned.
	char	*match;			- pointer to element to use in (*ecompare)() call to "match" against.
	int	*positionp;		- pointer to position of found element (to be returned), this may be NULL.
	int	*count;			- number of elements in the ring.
	int	returncode;		- return code from previous ring routine.

	RETURNCODE:
	All routines (except ring_error) return an int returncode.  A value of 0 is SUCCESS, a negative value is a critical
	error that should never occur under normal circumstances, a positve value indicates the routine failed in an expected
	mannor e.g. ring_find() couldn't find the requested element in the ring.

	INTERNALS:
	Ring maintains elements in a doubly linked circular list.  If the elements are of fixed size (esize != 0) a block list
	and a free list are also kept.  The initial block is large enough to hold ALLOC1 number of elements. When this is
	exhausted additional blocks are malloced, each able to hold ALLOC2 elements. 

	If the ring is SORTED then the position is ignored by ring_add() and the element is inserted into the ring in sorted
	order.

	The user is never given access to the internals.  On a ring_add() the element is copied into ring memory and on a 
	ring_get() the element is copied out of ring memory.  The user is never given a pointer into ring memory so can never
	change a ring element.  To modify an element you must remove it then re-add it or use ring_replace().
	
*/

#include <stdlib.h>
#include <string.h>

#define	ERR_MALLOC	-1
#define	ERR_ID		-2
#define ERR_CORRUPT	-3
#define ERR_COMPARE	-4
#define ERR_REPLACE	-5

#define ERR_NOTFOUND	1
#define ERR_POSITION	2


struct list_struct
{
	struct list_struct *self;					/* Pointer to self to detect corruption			*/
	struct list_struct *next;					/* Pointer to next list item				*/
	struct list_struct *prev;					/* Pointer to previous list item			*/
};
typedef struct list_struct list_struct;

struct ring_struct
{
	struct ring_struct *self;					/* Pointer to self to detect corruption			*/
	int	esize;							/* Size of an element.					*/
	int	xsize;							/* Extended size (esize + pointer + roundup)		*/
	int	alloc1;							/* Initial allocation in number of elements		*/
	int	alloc2;							/* Secondary allocation					*/
	int	(*ecompare)();						/* Comparision routine  (*ecompare)(e1,e2)		*/
	int	sorted;							/* Should this ring be sorted ?				*/
	int	ecnt;							/* Count of number of elements used			*/
	int	ealloc;							/* Number of elements allocated				*/
	list_struct *blocklist;						/* Pointer to first block				*/
	list_struct *freelist;						/* Pointer to free element list				*/
	list_struct head;						/* Dummy head of linked list				*/
};
typedef struct ring_struct ring_struct;


#define RING_C
#include "ring.h"


static list_struct *pointpos(list_struct* list_head,int position);
static int real_get(ring_struct* ring_ptr, int position, char* element, list_struct** list_rtn_ptr);
static unload_element(char* element, list_struct* list_ptr, int size);
static int free_element(ring_struct* ring_ptr, list_struct* list_ptr);
static int validatepos(int cnt, int position);
static int quitpath(int cnt, int position);
static int realadd( list_struct* list_head, list_struct* list_new, int position);
static int findpos(ring_struct* ring_ptr, char* match, int* position, list_struct** list_rtn_ptr);
static int check_id(ring_struct* ring_ptr);
static int new_block( ring_struct* ring_ptr, int num);
static int list_corrupt(list_struct* list_ptr);

/*********************************************************************************************************************************

	ring_open:	Open and initialize a ring.

*********************************************************************************************************************************/
int ring_open(
	char	**id,				/* The pointer to ring_struct used for id		*/
	int	esize,				/* Size of an element					*/
	int	alloc1,				/* Initial allocation					*/
	int	alloc2,				/* Secondary allocation					*/
	int	(*ecompare)(),			/* Element comparison routine				*/
	int	sorted_flag)			/* Should this ring be sorted ?				*/
{
	ring_struct	*ring_ptr;
	int		rc;

	if ( sorted_flag && !ecompare ) return(ERR_COMPARE);

	ring_ptr = (ring_struct *)malloc( sizeof(ring_struct) );
	if (!ring_ptr)	return(ERR_MALLOC);

	*id = (char *) ring_ptr;

	ring_ptr->self		= ring_ptr;
	ring_ptr->esize    	= esize;
	ring_ptr->xsize    	= (esize + sizeof(list_struct) + 7) & ~(7);	/* Calc extended size and round up to 8		*/
	ring_ptr->alloc1   	= alloc1;
	ring_ptr->alloc2   	= alloc2;
	ring_ptr->ecompare 	= ecompare;
	ring_ptr->sorted	= sorted_flag;
	ring_ptr->ecnt     	= 0;
	ring_ptr->ealloc	= 0;
	ring_ptr->blocklist	= 0;
	ring_ptr->freelist	= 0;
	ring_ptr->head.self	= &ring_ptr->head;
	ring_ptr->head.next	= &ring_ptr->head;
	ring_ptr->head.prev	= &ring_ptr->head;

	if ( rc = new_block( ring_ptr, alloc1 ) )
	{
		free(ring_ptr);
		return(rc);
	}

	return(0);
}


/*********************************************************************************************************************************

	ring_close:	Close a ring and free all memory used.

*********************************************************************************************************************************/
int ring_close(ring_struct* ring_ptr)
{
	list_struct *curr_block, *next_block;
	int	rc;

	if ( rc = check_id(ring_ptr) ) return(rc);

	curr_block = ring_ptr->blocklist;
	
	while(curr_block)
	{
		if (rc = list_corrupt(curr_block)) return(rc);
		next_block = curr_block->next;
		free((char *)curr_block);
		curr_block = next_block;
	}
	free((char *)ring_ptr);
	return(0);
}

/*********************************************************************************************************************************

	ring_add:	Add element to the ring at the given position. (If a sorted ring then position is ignored.)
			Position can be positive or negative, a positive number means insert before that item, a negative
			position means insert after the item.

			  0         1         2         3	positive  Position
			  |         |         |         |
		     head--->[0/-3]--->[1/-2]--->[2/-1]--->tail
			  |         |         |         |
			 -4        -3        -2        -1	negative Position

*********************************************************************************************************************************/
int ring_add(ring_struct *ring_ptr, int position,void *element)
{
	int	rc;
	int	realpos;
	list_struct *list_ptr;

	if ( rc = check_id(ring_ptr) ) return(rc);

	if ( ring_ptr->sorted )
	{
		findpos(ring_ptr,element,&position,0);
	}
	else
	{
		if      ( (position > 0) && (position - ring_ptr->ecnt ==  0) ) position = -1;
		else if ( (position < 0) && (position + ring_ptr->ecnt == -1) ) position =  0;

		if ( position != 0 && position != -1 )				/* 0 && -1 are always valid for add		*/
		{
			if ( rc = validatepos(ring_ptr->ecnt, position) ) return(rc);
		}
	}

	realpos = quitpath(ring_ptr->ecnt,position);
	if      ( position > 0 && realpos < 0 ) realpos--;			/* Adjust for sign change			*/
	else if ( position < 0 && realpos > 0 ) realpos++;

	if ( ring_ptr->esize )							/* If fixed length elements			*/
	{
		if ( !ring_ptr->freelist )
		{
			if ( rc = new_block( ring_ptr, ring_ptr->alloc2 ) ) return(rc);
		}

		list_ptr = ring_ptr->freelist;					/* Get an element off the freelist		*/
		if ( rc = list_corrupt(list_ptr) ) return(rc);

		ring_ptr->freelist = list_ptr->next;				/* Update freelist pointer.			*/

		memcpy(list_ptr+1,element,ring_ptr->esize);			/* Load the element				*/
	}
	else									/* Variable length (null term) elements		*/
	{
		list_ptr = (list_struct *)malloc(sizeof(list_struct) + strlen(element) + 1);
		if (!list_ptr) return(ERR_MALLOC);
		strcpy((char *)(list_ptr+1),element);
	}

	if ( rc = realadd(&ring_ptr->head,list_ptr,realpos) ) return(rc);

	ring_ptr->ecnt += 1;
	return(0);
}

/*********************************************************************************************************************************

INTERNAL	pointpos:	Return a list pointer that points to the position.

*********************************************************************************************************************************/
static list_struct *pointpos(list_struct* list_head,int position)
{
	list_struct	*list_ptr;

	if ( position >= 0 )	list_ptr = list_head->next;
	else			list_ptr = list_head;

	while(position)
	{
		if ( position > 0 )
		{
			list_ptr = list_ptr->next;
			position--;
		}
		else
		{
			list_ptr = list_ptr->prev;
			position++;
		}
	}

	return(list_ptr);
}

/*********************************************************************************************************************************

	ring_replace:	Replace an element in a ring.

*********************************************************************************************************************************/
int ring_replace(ring_struct *ring_ptr, int position, void *element)
{
	int	rc;
	list_struct *list_ptr, *list_new;
	int	realpos;

	if ( rc = check_id(ring_ptr) ) return(rc);

	if ( ring_ptr->sorted )
	{
		return(ERR_REPLACE);
	}
	else
	{
		if ( rc = validatepos(ring_ptr->ecnt, position) ) return(rc);
	}

	realpos = quitpath(ring_ptr->ecnt,position);

	list_ptr = pointpos(&ring_ptr->head,realpos);

	if ( ring_ptr->esize )							/* If fixed length elements			*/
	{
		memcpy((char *)(list_ptr+1),element,ring_ptr->esize);	/* Load the element				*/
	}
	else									/* Variable length (null term) elements		*/
	{
		list_new = (list_struct *)malloc(sizeof(list_struct) + strlen(element) + 1);	/* Malloc a new element		*/
		if (!list_new) return(ERR_MALLOC);

		list_new->self = list_new;
		list_new->next = list_ptr->next;
		list_new->prev = list_ptr->prev;
		strcpy((char *)(list_new+1),element);

		(list_new->prev)->next = list_new;				/* Link it into the list			*/
		(list_new->next)->prev = list_new;

		free((char *)list_ptr);						/* Free old one.				*/
	}

	return(0);
}

/*********************************************************************************************************************************

	ring_remove:	Remove element at position from the ring.  If element is not null then the ring element is copied
			to element before removal. (Element must be at lease esize bytes.)

*********************************************************************************************************************************/
int ring_remove(ring_struct *ring_ptr, int position, void *element)
{
	int	rc;
	list_struct *list_ptr;

	if ( rc = check_id(ring_ptr) ) return(rc);

	if ( rc = real_get(ring_ptr,position,element,&list_ptr) ) return(rc);

	if ( rc = free_element(ring_ptr, list_ptr) ) return(rc);

	return(0);
}

/*********************************************************************************************************************************

	ring_get:	Get the contents of an element.

			      0      1      2      	positive  Position
			      |      |      |      
		     head--->[0]--->[1]--->[2]--->tail
			      |      |      |
			     -3     -2     -1		negative Position

*********************************************************************************************************************************/
int ring_get(ring_struct *ring_ptr, int position, void *element)
{
	int	rc;
	list_struct *list_ptr;

	if ( rc = check_id(ring_ptr) ) return(rc);

	if ( rc = real_get(ring_ptr,position,element,&list_ptr) ) return(rc);
	return(0);
}

/*********************************************************************************************************************************

	ring_find:	Search for an element and return its contents.

*********************************************************************************************************************************/
int ring_find(ring_struct *ring_ptr, void *match, int *position, void *element)
{
	int	rc;
	list_struct *list_ptr;
	int	temppos;

	if ( rc = check_id(ring_ptr) ) return(rc);

	if ( rc = findpos(ring_ptr,match,&temppos,&list_ptr) ) return(rc);

	if (position)
	{
		*position = temppos;
	}
	unload_element(element,list_ptr,ring_ptr->esize);
	return(0);
}

/*********************************************************************************************************************************

	ring_count:	Return the number of elements in the ring.

*********************************************************************************************************************************/
int ring_count(ring_struct* ring_ptr, int* count)
{
	int	rc;

	if ( rc = check_id(ring_ptr) ) return(rc);

	*count = ring_ptr->ecnt;
	return(0);
}

/*********************************************************************************************************************************

	STACK PRIMATIVES

	ring_push:	Push an element onto the stack.

	ring_pop:	Pop an element of the stack.

	ring_top:	Get the top element of the stack.

*********************************************************************************************************************************/
int ring_push(ring_struct *ring_ptr, void *element)
{
	return( ring_add(ring_ptr,0,element) );
}

int ring_pop(ring_struct *ring_ptr, void *element)
{
	return( ring_remove(ring_ptr,0,element) );
}

int ring_top(ring_struct *ring_ptr, void *element)
{
	return( ring_get(ring_ptr,0,element) );
}


/*********************************************************************************************************************************

	QUEUE PRIMATIVES

	ring_que:	Put an element into the queue.

	ring_unque:	Remove an element from the queue.

	ring_front:	Get the next element to come off the queue.

	ring_back:	Get the last elemetn put onto the queue.

*********************************************************************************************************************************/
int ring_que(ring_struct *ring_ptr, void *element)
{
	return( ring_add(ring_ptr,-1,element) );
}

int ring_unque(ring_struct *ring_ptr, void *element)
{
	return( ring_remove(ring_ptr,0,element) );
}

int ring_front(ring_struct *ring_ptr, void *element)
{
	return( ring_get(ring_ptr,0,element) );
}

int ring_back(ring_struct *ring_ptr, void *element)
{
	return( ring_get(ring_ptr,-1,element) );
}

/*********************************************************************************************************************************

INTERNAL	real_get:	Really do the extract.

*********************************************************************************************************************************/
static int real_get(ring_struct* ring_ptr, int position, char* element, list_struct** list_rtn_ptr)
{
	int	rc;
	list_struct *list_ptr;
	int	realpos;

	if ( rc = validatepos(ring_ptr->ecnt, position) ) return(rc);

	realpos = quitpath(ring_ptr->ecnt,position);

	list_ptr = pointpos(&ring_ptr->head,realpos);

	unload_element(element,list_ptr,ring_ptr->esize);

	*list_rtn_ptr = list_ptr;
	return(0);
}

/*********************************************************************************************************************************

INTERNAL	unload_element:	Copy out the contents of a ring element.

*********************************************************************************************************************************/
static unload_element(char* element, list_struct* list_ptr, int size)
{
	if ( element )								/* If element then copy it out			*/
	{
		if ( size )
			memcpy(element, (char *)(list_ptr+1), size);
		else
			strcpy(element, (char *)(list_ptr+1));
	}
	return 0;
}

/*********************************************************************************************************************************

INTERNAL	free_element:	Unlink an element from the ring and put it on the freelist (or free it).

*********************************************************************************************************************************/
static int free_element(ring_struct* ring_ptr, list_struct* list_ptr)
{
	list_struct *list_prev, *list_next;

	list_prev = list_ptr->prev;						/* Unlink the element				*/
	list_next = list_ptr->next;

	list_prev->next = list_ptr->next;
	list_next->prev = list_ptr->prev;

	if ( ring_ptr->esize )
	{
		list_ptr->next = ring_ptr->freelist;				/* Put element onto free list			*/
		ring_ptr->freelist = list_ptr;
	}
	else									/* If variable length (null term) then each	*/
	{									/* element was malloced and needs to be freed	*/
		free((char *)list_ptr);	
	}

	ring_ptr->ecnt -= 1;							/* Decrement element count			*/
	return(0);
}

/*********************************************************************************************************************************

INTERNAL	validatepos:	Check if the position is in the ring.

*********************************************************************************************************************************/
static int validatepos(int cnt, int position)						/* Check if position is in ring.		*/
{
	if ( position >= 0 )							/* Check if position is valid			*/
	{
		if ( position >= cnt ) return(ERR_POSITION);
	}
	else
	{
		if ( position + cnt < 0 ) return(ERR_POSITION);
	}
	return(0);
}

/*********************************************************************************************************************************

INTERNAL	quitpath:	Given a position find the quickest direction to that element (positive or negative).

*********************************************************************************************************************************/
static int quitpath(int cnt, int position)
{
	int	pospos;

	if ( position == -1 || position == 0 )
		return(position);

	if ( position > 0 )
	{
		pospos = position;
	}
	else
	{
		pospos = position + cnt;
	}

	if ( pospos >= (cnt/2) )						/* Calc the quickest path			*/
		return(pospos - cnt);
	else
		return(pospos); 
}

/*********************************************************************************************************************************

INTERNAL	realadd:	Link an element into the ring at the given position.
				Positive position = insert before
				Negative position = insert after

*********************************************************************************************************************************/
static int realadd( list_struct* list_head, list_struct* list_new, int position)
{
	list_struct	*list_next;
	list_struct	*list_prev;

	if      (position ==  0) list_prev = list_head;
	else if (position == -1) list_prev = list_head->prev;
	else if (position <   0) list_prev = pointpos(list_head,position);
	else
	{
		list_next = pointpos(list_head,position);
		list_prev = list_next->prev;
	}

	list_next = list_prev->next;
	list_new->next = list_next;
	list_new->prev = list_prev;

	list_next->prev = list_new;
	list_prev->next = list_new;
	return(0);
}

/*********************************************************************************************************************************

INTERNAL	findpos:	Find the position of match in the ring.
				Returns the position in the list of match OR where to insert element.

*********************************************************************************************************************************/
static int findpos(ring_struct* ring_ptr, char* match, int* position, list_struct** list_rtn_ptr)
{
	list_struct	*list_ptr;
	int	cmp, i;

	if (!ring_ptr->ecompare)
	{
		return(ERR_COMPARE);
	}
	if ( ring_ptr->ecnt < 1 )
	{
		*position = 0;
		return(ERR_NOTFOUND);
	}

	list_ptr = &ring_ptr->head;
	list_ptr = list_ptr->next;

	if (ring_ptr->sorted)							/* If a sorted list then do a BINARY search	*/
	{
		int	low,high,curr,new;

		low = 0;
		high = ring_ptr->ecnt - 1;
		curr = 0;

		for(;;)
		{
			new = (low == high) ? low : ((high - low)/2 + low);	/* Calc the next element to compare		*/
			while(curr != new)					/* loop until pointing to new 			*/
			{
				if ( new > curr )
				{
					list_ptr = list_ptr->next;
					curr++;
				}
				else
				{
					list_ptr = list_ptr->prev;
					curr--;
				}
			}

			cmp = (*ring_ptr->ecompare)(match, (char *)(list_ptr+1));	/* Do the compare			*/
			if ( cmp == 0 )							/* Found the match			*/
			{
				*position = curr;
				if ( list_rtn_ptr ) *list_rtn_ptr = list_ptr;
				return(0);
			}
			if ( cmp > 0 )  low  = curr + 1;			/* If (match > curr) reset low			*/
			else		high = curr - 1;			/* else	reset high				*/

			if ( low > high )					/* Not found					*/
			{
				*position = ( cmp > 0 ) ? (curr+1) : curr;
				if ( *position >= ring_ptr->ecnt ) *position = -1;
				return(ERR_NOTFOUND);
			}
		}
	}
	else /* not sorted */
	{
		for(i=0; i<ring_ptr->ecnt; i++)					/* Loop thru full list				*/
		{
			cmp = (*ring_ptr->ecompare)(match, (char *)(list_ptr+1));	/* Do the compare			*/
			if ( cmp == 0 )						/* Found it					*/
			{
				*position = i;
				if ( list_rtn_ptr ) *list_rtn_ptr = list_ptr;
				return(0);
			}
			list_ptr = list_ptr->next;
		}

		*position = -1;
		return(ERR_NOTFOUND);
	}
}

/*********************************************************************************************************************************

INTERNAL	check_id:	Test if id is valid.

*********************************************************************************************************************************/
static int check_id(ring_struct* ring_ptr)
{
	if ( !ring_ptr ) return(ERR_ID);
	if ( ring_ptr != ring_ptr->self ) return(ERR_CORRUPT);
	return(0);
}

/*********************************************************************************************************************************

INTERNAL	new_block:	Alloc a new block and initialize the freelist on it.

*********************************************************************************************************************************/
static int new_block( ring_struct* ring_ptr, int num)
{
	list_struct	*block_ptr;					/* Pointer to new block.				*/
	char	*ptr;
	list_struct	*list_ptr;
	int	i;

	if ( ring_ptr->esize == 0 ) return(0);				/* Variable length (null terminated) elements		*/

	block_ptr = (list_struct *)malloc(sizeof(list_struct) + (ring_ptr->xsize * num)); /* Malloc new block of num elements	*/
	if (!block_ptr) return(ERR_MALLOC);

	block_ptr->self = block_ptr;					/* Init block list					*/
	block_ptr->next = ring_ptr->blocklist;
	block_ptr->prev = 0;

	ring_ptr->blocklist = block_ptr;				/* Update ring structure fields				*/
	ring_ptr->freelist = block_ptr+1;
	ring_ptr->ealloc += num;

	ptr = (char *)(block_ptr+1);					/* Point to first element in block (after list ptrs)	*/
	for( i=0; i<num; i++ )						/* Initialize the freelist in this block		*/
	{
		list_ptr = (list_struct *)ptr;				/* Point to current list element			*/
		ptr += ring_ptr->xsize;					/* Calc pointer to next element				*/
		list_ptr->self = list_ptr;				/* Add corruption detection				*/
		list_ptr->next = (list_struct *)ptr;			/* Point to next element				*/
		list_ptr->prev = 0;					/* Only a singley linked list				*/
	}
	list_ptr->next = 0;						/* Set last elements next pointer			*/

	return(0);
}

/*********************************************************************************************************************************

INTERNAL	list_corrupt:	Test for corruption.

*********************************************************************************************************************************/
static int list_corrupt(list_struct* list_ptr)
{
	if ( list_ptr != list_ptr->self ) return(ERR_CORRUPT);
	return(0);
}

/*********************************************************************************************************************************

INTERNAL	ring_error:	Return a pointer to the error message for a given returncode.

*********************************************************************************************************************************/
char *ring_error(int rc)
{
	char	*ptr;

	switch(rc)
	{
	case	0:		ptr = "SUCCESS";				break;
	case	ERR_MALLOC:	ptr = "Malloc failed";				break;
	case	ERR_ID:		ptr = "Invalid ring identifier";		break;
	case	ERR_CORRUPT:	ptr = "Ring is corrupt";			break;
	case	ERR_COMPARE:	ptr = "No COMPARE routine for sorting or find";	break;
	case	ERR_REPLACE:	ptr = "Replace is invalid for sorted ring";	break;
	case	ERR_NOTFOUND:	ptr = "Element not found";			break;
	case	ERR_POSITION:	ptr = "Position out of ring boundaries";	break;
	default:		ptr = "Unknown error";				break;
	}

	return(ptr);
}

/*
**	History:
**	$Log: ring.c,v $
**	Revision 1.9  1996-07-23 14:17:50-04  gsl
**	drcs update
**
**
**
*/
