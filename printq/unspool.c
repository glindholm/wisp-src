#ifndef lint
static char rcsid[]="$Header: unspool.c,v 1.2 91/07/24 14:39:57 jockc Locked $";
static char copyright[]="Copyright 1991 Jock Cooper";
#endif
/*
 * unspool module
 *
 * $Log$
 *
 */
#define DECLEXT
#include "ppdefs.h"
#include "daemon.h"

/*
 * take an item off of the queue and send it to a device
 */
unspool(dest,item)
printer_ent *dest;
queue_ent *item;
{
  int to, from;
  int unsp_process();

  item->dest = dest;
  subprocess(&to,&from,unsp_process,(char*)item);
  new_unsp_node(to,from,item->queue_id);
}
new_unsp_node(to,from,qid)
int to,from,qid;
{
  char *gmem();
  struct unsp_ent *unsp;

  unsp=(struct unsp_ent *)gmem(1,sizeof(struct unsp_ent));
  unsp->to = to;
  unsp->from = from;
  unsp->qid = qid;
  add_node(&unsp_head,(void*)unsp);
}
/*
 * entry point for unspool process.
 *
 */
unsp_process(item)
queue_ent *item;
{
  
}
