#ifndef lint
static char rcsid[]="$Header: /disk1/users/jockc/unique/RCS/jlist.c,v 1.4 1993/05/28 21:48:47 jockc V3_03 $";
static char copyright[]="Copyright 1991 Jock Cooper";
#endif
/*
 * list management functions
 *
 * $Log: jlist.c,v $
 * Revision 1.4  1993/05/28  21:48:47  jockc
 * using gmem not calloc
 * .,
 *
 * Revision 1.3  1993/03/26  00:15:24  jockc
 * fixed slightly broke cut_node
 *
 * Revision 1.2  1993/01/12  02:11:43  jockc
 * revised this broken (simple) list manager
 *
 * Revision 1.1  1992/05/07  22:30:50  jockc
 * Initial revision
 *
 *
 */
#define LISTMAN
#include "jlist.h"

/* 
 * add a node onto a generic list
 */
add_node(listp,node)
LIST **listp; /* pointer to pointer to list head .. null if empty */
void *node;   /* data for new node */
{
  char *gmem();
  LIST *tmp;

  if (*listp)
    {
      tmp = (*listp)->list_tail; /* find end of list */
      tmp->next = (LIST*)gmem(1,sizeof(LIST)); /* alloc a new thingy */
      tmp->next->prev = tmp;  /* new thingy's prev must point to current node */
      tmp= tmp->next; /* now ready to work on new item */
      tmp->data = node; /* stash data in there */
      tmp->next = NULL; /* no next node */
      (*listp)->list_tail = tmp; /* update the last pointer */
    }
  else
    {
      *listp=(LIST*)gmem(1,sizeof(LIST)); /* get space for head */
      (*listp)->next=NULL;
      (*listp)->list_tail = (*listp); 
      (*listp)->data = node;
    }
}
/*
 * cut a node out of a generic list
 *
 */
cut_node(listp,data)
LIST **listp; /* pointer to head */
void *data; /* pointer to data we want to cut */
{
	LIST *tmp;
	for (tmp= (*listp); tmp && tmp->data != data; tmp=tmp->next)
	{
		;
	}
	if (tmp)
	{
		if (tmp == *listp)
		{
			if (tmp->next)
			{
				tmp->next->prev = tmp->prev;
			}
			(*listp)        = tmp->next;
			free(tmp->data);
			free(tmp);
		}
		else
		{
			if (tmp->next) 
			{
				tmp->next->prev = tmp->prev;
			}
			if ((*listp)->list_tail == tmp)
			{
				(*listp)->list_tail = tmp->prev;
			}
			tmp->prev->next = tmp->next;
			free(tmp->data);
			free(tmp);
		}
	}
}
list_size(listp)
LIST *listp;
{
  int tmp;
  if (!listp) return 0;
  for(tmp=1; listp->next; ++tmp, listp=listp->next);
  return tmp;
}
sort_list(listp,sortroutine)  /* reorder a linked list. */
LIST *listp; /* list head */
int (*sortroutine)();   /* routine for item comparison...*/
			/*  receives two void ** pointers (called by qsort)*/
                        /* routine must deref twice to get to data item */
{
  void **ptrs;
  char *gmem();
  int cnt, i;
  LIST *tmp;

  cnt=list_size(listp); /* find out how many items to sort */
  ptrs=(void**)gmem(cnt,sizeof(void*)); /* alloc array space for sort */
  for (tmp=listp->next, i=0; tmp;  tmp=tmp->next, ++i) /* load array with values */
    *(ptrs+i) = tmp->data;
  qsort((char*)ptrs,cnt,sizeof(tmp->data),sortroutine); /* sort array */
  for (tmp=listp->next, i=0; tmp;  tmp=tmp->next, ++i) /* copy sorted array back into list */
     tmp->data = *(ptrs+i);
  free(ptrs); /* free space we grabbed for array */
}
kill_list(listp,flag) /* kill an entire list */ 
LIST **listp;
int flag;    /* 1=free data and list nodes, 0=free only list nodes */
{
  LIST *tmp, *prv;

  if (! *listp) return;
  for (tmp = (*listp)->list_tail; tmp != (*listp); tmp = prv)
    {
      if (flag) { if (tmp->data) free(tmp->data); }
      prv=tmp->prev;
      free(tmp);
    }
  free(*listp);
  *listp=NULL;
}
