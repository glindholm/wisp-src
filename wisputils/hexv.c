static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
 * Program: hexv
 *  Module: $RCSfile: hexv.c,v $
 * 
 * $Log: hexv.c,v $
 * Revision 1.7  1995-04-25 05:58:21-04  gsl
 * drcs state V3_3_15
 *
 * Revision 1.6  1995/04/17  11:50:46  gsl
 * drcs state V3_3_14
 *
 * Revision 1.5  1995/02/06  15:13:24  gsl
 * updated
 * ,.
 *
 * Revision 1.1  1991/04/18  23:17:29  jockc
 * Initial revision
 *
 *
 *
 */

#include <stdio.h>

#define RCSVERS "$Revision:$"
#define RCSDATE "$Date:$"

static char *vers=RCSVERS;
static char *date=RCSDATE;

#define VER(x) x[0]=='%'?"0.0":x
#define DAT(x) x[0]=='%'?"19xx":x
#define VERSION VER(vers)
#define MODDATE DAT(date)

#define RECLEN 2022

typedef unsigned char uchar;
typedef unsigned int uint;

uint start_pos=0,end_pos=0x7fffffff;
char *infile=NULL, *outfile=NULL;

main(c,v)
uchar **v;
{
	FILE *in,*out;
	uchar buf[200];
	uchar dbuf[200];
	register int i;
	uint cnt, total=0, rec=0;
	uchar tbuf[100];
	uchar *filename;

	parseopts(c,v);

	if (infile)
	  in=fopen(infile,"r");
	else
	  in=fdopen(0,"r");

	if (outfile)
		out=fopen(outfile,"w");
	else
		out=fdopen(1,"w");

	if (start_pos)
	{
		fseek(in,start_pos,0);
		total=start_pos;
	}

	while ((cnt=fread(buf,sizeof(uchar),16,in)) && (total <= end_pos))
	{
		memset(dbuf,0,sizeof(dbuf));
		sprintf(dbuf,"%07X:  ",total);
		for (i=0;i<cnt;++i) 
		{
			sprintf(tbuf,"%02X ",buf[i]);
			strcat(dbuf,tbuf);
		}
		strcat(dbuf,"  ");
		for (i=0;i<cnt;++i) 
		{
			if (buf[i]>0x20 && buf[i]<0x7f) 
			{
				tbuf[0]=buf[i];
				tbuf[1]=(uchar)0;
				strcat(dbuf,tbuf);
			}
			else
				strcat(dbuf,".");
		}
		fprintf(out,"%s\n",dbuf);
		total += cnt;
	}
	fclose(in); fclose(out);
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
	
	while ((c=getopt(cnt,v,"s:e:i:o:h"))!=EOF)
	{
		switch(c)
		{
			
		      case 's':
			start_pos=toint(optarg);
			break;
		      case 'e':
			end_pos=toint(optarg);
			break;
		      case 'i':
			infile=strdup(optarg);
			break;
		      case 'o':
			outfile=strdup(optarg);
			break;
		      case 'h':
		      default:
			usage();
			
		}
		
	}
}
char *strdup(s)
char *s;
{
	char *malloc(),*tmp;
	
	tmp=malloc(strlen(s)+1);
	strcpy(tmp,s);
	return tmp;
}
usage()
{
	fprintf(stderr,"hexdump V%s %s\n",VERSION,MODDATE);

	fprintf(stderr,"usage: hexv [-s#] [-e#] [-i infile] [-o outfile]\n");
	fprintf(stderr,"       -s#         specify starting position\n");
	fprintf(stderr,"       -e#         specify ending position\n");
	fprintf(stderr,"       -i infile   specify input file (default: stdin)\n");
	fprintf(stderr,"       -o outfile  specify output file (default: stdout)\n");
	exit(1);	
}
toint(p)
char *p;
{
	unsigned int xtoi();
	
	if (p[0]=='0'&&(p[1]=='X'||p[1]=='x'))
	  return xtoi(&p[2]);
	else 
	  return atoi(p);
}
unsigned int xtoi(tmp)
char *tmp;
{
	char *p;
	int l;
	unsigned int res=0,pow=1;
	
	l=strlen(tmp)-1;
	p=tmp+l;
	while (l>=0)
	{
		res += pow*hexdig(*p);
		pow *= 16;
		p--;
		l--;
	}
	return res;
}       
hexdig(pch)
char pch;
{
	int ch;
	
	ch=toupper(pch);
	if(ch>='0'&&ch<='9') { ch-='0'; return ch; }
	if(ch>='A'&&ch<='F') { ch=ch-'A'+0x0a; return ch; }

	return 0;
}
