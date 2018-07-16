#include <stdio.h>
#define FALSE 0
#define TRUE !FALSE
int pagecount=1;

main()
{
	FILE *f;
	char buf[200];
	
	f=fopen("/usr2/jockc/xxxx","r");
	while (myfgets(buf,200,f,6))
	{
		printf("buf=[%s] pagecount=%d\n",buf,pagecount);
		memset(buf,0,sizeof(buf));
	}
	
}

int myfgets(buf,cnt,file,pagesize)
char buf[];
int cnt,pagesize;
FILE *file;
{
	static char localbuf[1024];
	static int lpos=0, end=0;

	static int curline = 0;
	static int eopage= 0;

	extern int pagecount;

	int pos, done;
	
	
	for (done=pos=0;!done;)
	{
		if (end - lpos == 0)
		{
			lpos=0;
			end = fread(localbuf,1,1024,file);
			if (end==0)
			  return 0;
		}
		while (lpos < end)
		{
			buf[pos++] = localbuf[lpos++];
			
			if (localbuf[lpos-1] == '\f')
			{
				if (eopage)
				{
					--pos;
					eopage=FALSE;
					++pagecount;
					continue;
				}
				curline = 0;
				++pagecount;
				done=TRUE;
				break;
			}
			else if ((localbuf[lpos-1] == '\n')||(pos >= cnt))
			{
				++curline;
				if (curline >= pagesize)
				{
					eopage=TRUE;
					curline = 0;
				}
				else
				  eopage=FALSE;
				done=TRUE;
				break;
			}
			else if (eopage)
			{
				eopage=FALSE;
				++pagecount;
			}
		}
	}
	return pos;
}
