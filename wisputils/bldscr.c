/*
 * Copyright 1990 IDSI - All Rights Reserved 
 *
 */

static char *ident="@(#)bldscr.c  1.0   (UNIX/VMS)  11/1/90   (c) IDSI 1990";

#include <stdio.h>
#include <errno.h>

char PROGNAME[16];

#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
#define FALSE 0
#define TRUE !FALSE

#define T_DISP 1
#define T_MOD_X 2
#define T_MOD_9 3

struct list 
{
	struct list *next;
	char data[80];
};

struct list *scrdef=NULL, *wsdef=NULL;
struct list *scrp=NULL, *wsp=NULL;

#define NEWNODE (struct list *)calloc(1,sizeof(struct list))
#define NEXTNODE(x) ((x)->next)=NEWNODE, (x)=(x)->next 
	
main(c,v)
char **v;
{
	FILE *in, *out;
	char filebuf[24][81],inname[64],outname[64],rdbuf[80];
	char keybuf[80],fieldbuf[80];
	int i,j,k,type,len,pos,noteof;
	char *calloc(),*curlinep;
	char *reptstr();
	char source[32],object[32];
	
	scrp=scrdef=NEWNODE;
	wsp=wsdef=NEWNODE;

	strcpy(PROGNAME,v[0]);
	if (c>1) strcpy(inname,v[1]);
	else usage();
	if (c>2) strcpy(outname,v[2]);
	else 
	{
		strcpy(outname,inname);
		strcat(outname,".wcb");
	}
	
	if ((in=fopen(inname,"r"))==NULL)
	{
		fprintf(stderr,"error: could not open input file %s. reason=%d\n",PROGNAME,inname,errno);
		exit(1);
	}
	if ((out=fopen(outname,"w"))==NULL)
	{
		fprintf(stderr,"error: could not open output file %s. reason=%d\n",PROGNAME,outname,errno);
		exit(1);
	}
	
	memset(filebuf,' ',sizeof(filebuf));
	
	for (i=0,noteof=TRUE;i<24 && noteof; ++i)
	{
		if ((fgets(rdbuf,80,in))==NULL) 
		{
			fprintf(stderr,"warning: less than 24 lines in input file\n");
			noteof=FALSE;
			continue;
		}
		for (j=0,k=0;rdbuf[j]&&rdbuf[j]!='\n';++j)
		{
			if (rdbuf[j]=='\t') 
			  while (k%8) filebuf[i][k++]=' ';
			else filebuf[i][k++]=rdbuf[j];
		}
		filebuf[i][80]=(char)0;
		
	}
	printf("enter screen name->");
	gets(keybuf);
	upcase(keybuf);
	sprintf(scrp->data,"       01  %s USAGE IS DISPLAY-WS.\n",keybuf);
	NEXTNODE(scrp);
	sprintf(scrp->data,"\n");
	NEXTNODE(scrp);

	for (i=0; i<24; ++i)
	{
		pos=0;
		while (getfield(filebuf[i],fieldbuf,&type,&len,&pos))
		{
			switch (type)
			{
			      case T_DISP:
			      {
				      sprintf(scrp->data,"           03  FILLER PIC X(%d) ROW %d COL %d\n",
					      len,i+1,pos);
				      NEXTNODE(scrp);
				      sprintf(scrp->data,"               VALUE \"%s\".\n",fieldbuf);
				      NEXTNODE(scrp);
				      sprintf(scrp->data,"\n");
				      NEXTNODE(scrp);
				      break;
			      }
			    case T_MOD_X: case T_MOD_9:
			    {
				    printf("\nProcessing line:\n%s\n%sfield^\n",
					   filebuf[i],reptstr(' ',pos-5));
				    printf("Please enter SOURCE for this item: ");
				    gets(source);
				    upcase(source);
				    printf("Please enter OBJECT for this item\n[default:%s, for nonmodifiable enter \"-\"]: ",source);
				    gets(object);
				    upcase(object);
				    if (strlen(object)==0) strcpy(object,source);
				    sprintf(scrp->data,"           03  FILLER PIC %s(%d) ROW %d COL %d\n",
					    type==T_MOD_X?"X":"9",len,i+1,pos);
				    NEXTNODE(scrp);
				    if (object[0]!='-')
				    {
					    sprintf(scrp->data,"               SOURCE %s OBJECT %s.\n\n",source,object);
					    NEXTNODE(scrp);
				    }
				    else
				    {
					    sprintf(scrp->data,"               SOURCE %s.\n\n",source);
					    NEXTNODE(scrp);
				    }					    
				    sprintf(wsp->data,"       01  %s PIC %s(%d) VALUE %s.\n",source,type==T_MOD_X?"X":"9",len,
					    type==T_MOD_X?"SPACES":"ZERO");
				    NEXTNODE(wsp);
				    if (strcmp(source,object) && object[0]!='-')
				    {
					    sprintf(wsp->data,"       01  %s PIC %s(%d) VALUE %s.\n",
						    object,type==T_MOD_X?"X":"9",len,type==T_MOD_X?"SPACES":"ZERO");
					    NEXTNODE(wsp);
				    }
				    break;
				    
			    }
		      }
			pos += len;
		}
	}
	for (wsp=wsdef; wsp; wsp=wsp->next)
	  fprintf(out,"%s",wsp->data);
	fprintf(out,"\n");
	for (scrp=scrdef; scrp; scrp=scrp->next)
	  fprintf(out,"%s",scrp->data);
	fclose(in);
	fclose(out);
	
}
getfield(line,field,type,len,pos)
char *line,*field;
int *type,*len,*pos;
{
	int savepos;
	
	while (*(line+(*pos))==' ' && *(line+(*pos))) ++(*pos);
	savepos=(*pos);
	if (!(*(line+(*pos)))) return FALSE;
	if (*(line+(*pos))=='_' || *(line+(*pos))=='=')
	{
		if (*(line+(*pos))=='_') *type = T_MOD_X;
		else *type = T_MOD_9;
		
		while (*(line+(*pos)) == '_' || *(line+(*pos)) == '=') ++(*pos);
		*len=(*pos)-savepos;
		*pos = savepos;
		
		return TRUE;
	}
	else
	{
		*type=T_DISP;
		
		while (!((*(line+(*pos))==' ')&&(*(line+(*pos)+1)==' ')&&(*(line+(*pos)+2)==' ')) && 
		       ((*(line+(*pos))!='_')&&(*(line+(*pos))!='='))) ++(*pos);
		*len=(*pos)-savepos;
		strncpy(field,line+savepos,*len);
		field[*len]=(char)0;
		*pos=savepos;
		return TRUE;
	}
}
usage()
{
	fprintf(stderr,"\nusage: %s infile [outfile]\n",PROGNAME);
	fprintf(stderr,"If outfile is not specified, \".wcb\" is appended\nto infile to generate the output filename\n\n");
	exit(1);
}
upcase(p)
char *p;
{
	while (*p)
	{
		*p = toupper(*p);
		++p;
	}
}
char *reptstr(ch,cnt)
char ch;
int cnt;
{
	static char buf[200];
	
	memset(buf,ch,sizeof(buf));
	buf[cnt]=(char)0;
	return buf;
}

