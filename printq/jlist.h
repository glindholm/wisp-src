#ifndef _listh_
#define _listh_
/*
 * list related defines
 *
 */
typedef struct linked_list
{
  struct linked_list *next, *prev; /* next and prev pointers for each node */
  void *data; /* data for each node, last pointer for head node */
} linked_list;

#define LIST linked_list
#define list_tail data
#ifndef NULL
#define NULL 0
#endif
#endif
