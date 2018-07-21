/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id: cols.c,v 1.8 2003/02/04 18:50:26 gsl Exp $
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
**
** $Date: 2003/02/04 18:50:26 $
** $Revision: 1.8 $
******************************************************************************
*/

/**
 **     cols.c          cols [-c#] [-w#] [-l#] [-i infile] [-o outfile]
 **                             -c#             Number of columns per page, default 2
 **                             -w#             Width of each column, default 80
 **                             -l#             Lines per page, default 66
 **                             -i infile       name of input file, default stdin
 **                             -o outfile      name of output file, default stdout
 **
 **
 ** $Log: cols.c,v $
 ** Revision 1.8  2003/02/04 18:50:26  gsl
 ** fix copyright header
 **
 ** Revision 1.7  1995/04/25 09:58:07  gsl
 ** drcs state V3_3_15
 **
 * Revision 1.6  1995/04/17  11:50:34  gsl
 * drcs state V3_3_14
 *
 * Revision 1.5  1995/02/06  15:07:59  gsl
 * fix for RCS
 *
 * Revision 1.2  1992/04/03  23:53:27  jockc
 * strip out ^L's (formfeeds) which defeat the cols mechanism
 *
 * Revision 1.1  1991/04/18  23:09:11  jockc
 * Initial revision
 *
 **
 **/

#define RCSVERS "$Revision: 1.8 $"
#define RCSDATE "$Date: 2003/02/04 18:50:26 $"

static char *vers=RCSVERS;
static char *date=RCSDATE;

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
		if (*src=='\f')
		{
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
