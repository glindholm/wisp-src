struct foo
{
	int x;
	char foo[10];
	int y;
	char zzz[20];
	char yyy[20];
	int q;
};
#define OFS(x,y) (int)(  (char*) &( ( (x *)0 )->y ) -  (char*)((x *)0)  )
#define F struct foo
int zzz[]=
{
	OFS(F,x),
	OFS(F,foo),
	OFS(F,y),
	OFS(F,q),
	OFS(F,zzz),
	OFS(F,yyy),
	0
};
main(c,v)
int c;
char *v[];
{
	printf("%d\n",zzz[5]);
}
/*
  $Id:$

  $Log: x.c,v $
 * Revision 1.2  1993/08/13  00:17:06  jockc
 * first log in blah blah blah
 *
 * Revision 1.1  1993/08/13  00:15:48  jockc
 * Initial revision
 *

*/
/*
 * $Log: x.c,v $
 * Revision 1.2  1993/08/13  00:17:06  jockc
 * first log in blah blah blah
 *
 * Revision 1.1  1993/08/13  00:15:48  jockc
 * Initial revision
 *
 *
 */
