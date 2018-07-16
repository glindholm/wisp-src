/**
 **     cols.c          cols [-c#] [-w#] [-l#] [-i infile] [-o outfile]
 **                             -c#             Number of columns per page, default 2
 **                             -w#             Width of each column, default 80
 **                             -l#             Lines per page, default 66
 **                             -i infile       name of input file, default stdin
 **                             -o outfile      name of output file, default stdout
 **/

static char *copyright="Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char *id="@(#)cols.c	1.1 (c)IDSI 3/13/91";
#define SCCSVERS "1.1"
#define SCCSDATE "3/13/91"

static char *vers=SCCSVERS;
static char *date=SCCSDATE;

#define VER(x) x[0]=='%'?"0.0":x
#define DAT(x) x[0]=='%'?"19xx":x
#define VERSION VER(vers)
#define MODDATE DAT(date)

#include <stdio.h>

int cols_per=2;
int col_width=80;
int lines_per=66;
int usable_lines=64;
int column_margin=5;
int top_margin=1;
int bottom_margin=1;
int vert_line=0;

char *infile=NULL, *outfile=NULL;
FILE *in,*out;

char **pagebuf;
char linebuf[201];

main(argc,argv)
int	argc;
char	*argv[];
{
	int i;
	
	char *calloc();

	parseopts(argc,argv);
	
	if (infile)
	  in=fopen(infile,"r");
	else
	  in=fdopen(0,"r");

	if (outfile)
	  out=fopen(outfile,"w");
	else
	  out=fdopen(1,"w");

	
	if (!in)
	{
		fprintf(stderr,"%s: can't open input file %s\n",argv[0],infile?infile:"<stdin>");
		exit(1);
	}
	if (!out)
	{
		fprintf(stderr,"%s: can't open output file %s\n",argv[0],outfile?outfile:"<stdout>");
		exit(1);
	}
	pagebuf = (char**)calloc(usable_lines+1,sizeof(char*));
	for(i=0;i<usable_lines;++i)
	  *(pagebuf+i) = (char*)calloc(cols_per*(col_width+1),sizeof(char));
	doit();
	fclose(in);
	fclose(out);
	
}
parseopts(cnt,v)
int cnt;
char *v[];
{
	extern char *optarg;
	extern int  optind;
	extern int  opterr;
	int c;
	char *strdup();
	
	while ((c=getopt(cnt,v,"c:w:l:i:o:t:b:m:v"))!=EOF)
	{
		switch(c)
		{
			
		      case 'c':
			cols_per=atoi(optarg);
			break;
		      case 'w':
			col_width=atoi(optarg);
			break;
		      case 'l':
			lines_per=atoi(optarg);
			break;
		      case 't':
			top_margin=atoi(optarg);
			break;
		      case 'b':
			bottom_margin=atoi(optarg);
			break;
		      case 'm':
			column_margin=atoi(optarg);
			break;
		      case 'i':
			infile=strdup(optarg);
			break;
		      case 'o':
			outfile=strdup(optarg);
			break;
		      case 'v':
			vert_line=1;
			break;
		      default:
			usage();
			
		}
		
	}
	usable_lines=lines_per-(top_margin+bottom_margin);
	
}
char *strdup(s)
char *s;
{
	char *malloc(),*tmp;
	
	tmp=malloc(strlen(s)+1);
	strcpy(tmp,s);
	return tmp;
}
doit()
{
	int line,col,total,max;
	char *repspace();
	char tmpbuf[201];
	
	line=col=total=0;
	max=usable_lines*cols_per;

	while (fgets(tmpbuf,200,in))
	{
		detabify(tmpbuf,linebuf);
		
		if (linebuf[strlen(linebuf)-1]=='\n') linebuf[strlen(linebuf)-1]=(char)0;
		
		if (strlen(linebuf)>col_width) linebuf[col_width]=(char)0;
		else if (strlen(linebuf)<col_width)
		{
			strcat(linebuf,repspace(col_width-strlen(linebuf),0));
		}
		
		strcat(*(pagebuf+line),linebuf);
		if (col < (cols_per-1)) strcat(*(pagebuf+line),repspace(column_margin,1));
		
		++line;
		if(line==usable_lines)
		{
			line=0;
			++col;
		}
		++total;
		if (total>=max) 
		{
			dumppage();
			line=col=total=0;
		}
	}
	dumppage();
}
dumppage()
{
	char **p;
	int i;
	
	for (i=0;i<top_margin;++i)
	  fprintf(out,"\n");
	
	for (p=pagebuf; *p; ++p)
	{
		if (strlen(*p)) fprintf(out,"%s\n",*p);
		else fprintf(out,"\n");
		
		memset(*p,'\0',cols_per*col_width+1);
	}

	for (i=0;i<bottom_margin;++i)
	  fprintf(out,"\n");
}
char *
repspace(cnt,mode)
int cnt,mode;
{
	static char spaces[200];
	
	memset(spaces,' ',cnt);
	spaces[cnt]=(char)0;
	if(mode && vert_line) spaces[cnt/2]='|';
	return spaces;

}
usage()
{
	fprintf(stderr,"cols V%s %s\n",VERSION,MODDATE);
	fprintf(stderr,"usage: cols [-c#] [-w#] [-l#] [-i infile] [-o outfile]                \n");
	fprintf(stderr,"        -c#             Number of columns per page, default 2  \n");
	fprintf(stderr,"        -w#             Width of each column, default 80       \n");
	fprintf(stderr,"        -l#             Lines per page, default 66             \n");
	fprintf(stderr,"        -t#             Top margin (lines), default 1          \n");
	fprintf(stderr,"        -b#             Bottom margin (lines), default 1       \n");
	fprintf(stderr,"        -m#             Spaces between columns, default 5      \n");
	fprintf(stderr,"        -v              insert vertical bar between columns    \n"); 
	fprintf(stderr,"        -i infile       name of input file, default stdin      \n");
	fprintf(stderr,"        -o outfile      name of output file, default stdout    \n");
	exit(0);
	
}
detabify(src,dest)
char *src,*dest;
{
	int i=0;
	
	while (*src)
	{
		if (*src=='\t')
		{
			do 
			{
				*dest++ = ' ';
				++i;
			}
			while (i%8);
			
			++src;
		}
		else
		{
			*dest++ = *src++;
			++i;
		}
		
	}
	*dest=(char)0;
	
}
