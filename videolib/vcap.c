			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

static char *ident = "@(#)vcap.c   1.0    IDSI  Unix/VMS  04/18/90";

/* #define TEST */

#include <stdio.h>
#include <errno.h>
#ifdef TEST
#ifdef unix
#include <termio.h>
#endif
#endif
#ifdef VMS
#include <descrip.h>
#include <lnmdef.h>
#include <psldef.h>
#include <ttdef.h>
#include <stdlib.h>
#endif
#ifdef unix
#include <signal.h>
#include <malloc.h>
int resptimeout;
#endif
#ifdef MSDOS
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#endif

#define VCAP

#include "video.h"
#include "vcap.h"

#ifdef MSDOS
#define	EXT_PREFIX	((unsigned char)29)	/* Extended key code prefix	*/
#else	/* VMS or unix */
extern	int	errno ;
#endif

static int loaded=0;
static int pop();
static int push();
static char *gmem();

vcapload()										/* load up the vcap file */
{
	char *wc,*term_t;
	char *getenv();
	FILE *vcfile;
	int vcsort();
	int i;
#if VMS
	char wterm_t[10];
	int tttype;
#endif

	if (loaded) return 0;

/*	for (i=0; i<VC_CAP_COUNT; ++i) vcapdef[i]="";*/
#ifdef VMS
#ifndef TT$_VT400_SERIES
#define TT$_VT400_SERIES 99999
#endif
/* kludge ^^^^ */
	tttype=osd_ttype();
	switch (tttype)
	{
		case TT$_VT100:
			term_t = "vt100";
			break;
		case TT$_VT200_SERIES:
			term_t = "vt220";
			break;
		case TT$_VT300_SERIES:
			term_t = "vt320";
			break;
		case TT$_VT400_SERIES:
			term_t = "vt420";
			break;
               	case TT$_VT52:
			term_t = "vt52";
			break;
		case TT$_VT125:
			term_t = "vt125";
			break;
                case TT$_VT132:
			term_t = "vt132";
			break;
		default:
			term_t = "unknown";
			break;
	}
	get_log("WISP$TERM",wterm_t,sizeof(wterm_t));
	if (strlen(wterm_t))
		sprintf(vcpath,vcpathfmt,wterm_t);
	else
		sprintf(vcpath,vcpathfmt,term_t);
#endif	/*  VMS */
	
#ifndef VMS	/* unix	or MSDOS */
	if (!(wc=getenv("VIDEOCAP")))
	{
		if (!(wc=getenv("WISPCONFIG")))							/* get $WISPCONFIG */
		{
			wc=DEFWISPCONFIG;
		}
		else
		{
#ifdef unix
			sprintf(vcdir,"%s/videocap",wc);
#else	/* MSDOS */
			sprintf(vcdir,"%s\\videocap",wc);
#endif	/* MSDOS */
		}
	}
	else strcpy(vcdir,wc);
	if (!(term_t=getenv("WISPTERM")))						/* and $WISPTERM */
	{
		if (!(term_t=getenv("TERM")))
		{
#ifdef unix
			printf("VIDEO-E-NOTERM $WISPTERM and/or $TERM variable not defined");
#endif	/* unix */
			term_t=DEFTERM;
		}
	}
#ifdef unix
	sprintf(vcpath,"%s/%s",vcdir,term_t);						/* build the vc path */
#else	/* MSDOS */
	sprintf(vcpath,"%s\\%s",vcdir,term_t);						/* build the vc path */
#endif	/* MSDOS */
#endif	/* unix or MSDOS */

	qsort(vc_load_defs,								/* sort vc_load_defs array */
	      sizeof(vc_load_defs)/sizeof(vc_load)-1,					/* sizeof less one (don't sort NULL elem)*/
	      sizeof(vc_load),								/* sizeof one element */
	      vcsort);									/* sort routine */

	if (!(vcfile=fopen(vcpath,"r")))						/* open videocap file */
	{
		int ttype;
		
#ifndef VMS
		printf("\n\rVIDEOCAP-E-OPEN Error %d opening videocap file %s\n\r",errno,vcpath);
		sleep((unsigned)4);
#endif

		memset(vkeydef,0,sizeof(vkeydef));
#ifdef unix
		memcpy(vkeydef,vt220def,sizeof(vt220def));
#endif
#ifdef DOS_FUTURE
		memcpy(vkeydef,ansidef,sizeof(ansidef));
#endif

#ifdef VMS
#if 0
		if (tttype==TT$_VT200_SERIES || tttype==TT$_VT300_SERIES )
#endif	/*  0	*/
		  memcpy(vkeydef,vt220def,sizeof(vt220def));
#if 0
		else
		{
			memcpy(vkeydef,vt100def,sizeof(vt100def));
#ifndef MSDOS	/* VMS or unix */
			vcapdef[INIT_TERMINAL] = 
			  "\033[1;24r\033[24;1H\033[?3l\033[?7l\033[?20l\033<\033=";
#endif	/* VMS or unix */
		}
#endif	/*  0	*/
#endif	/*  VMS */

	}
	else
	{
		doload(vcfile);
		fclose(vcfile);
#ifdef MSDOS
		sprintf(vcpath,"%s\\stddef",vcdir);
#endif
#ifdef unix
		sprintf(vcpath,"%s/std.def",vcdir);					/* Try OLD std.def format		*/
#endif
#ifdef VMS
		sprintf(vcpath,vcpathfmt,"stddef");
#endif	
		if (!(vcfile=fopen(vcpath,"r")))					/* open videocap file 			*/
		{
#ifdef unix
			sprintf(vcpath,"%s/stddef",vcdir);				/* Try NEW stddef format		*/
			if (!(vcfile=fopen(vcpath,"r")))				/* open videocap file 			*/
#endif
			{
				printf("\n\rVIDEOCAP-E-STDDEF Error %d opening videocap file %s\n\r",errno,vcpath);
				sleep((unsigned)4);
			}
		}
		
		if (vcfile)
		{
			doload(vcfile);
			fclose(vcfile);
		}
	}
	build_vc_meta();
	++loaded;
	if (vcapdef[PAD]) 
	  padding=atoi(vcapdef[PAD]);
	else 
	  padding=0;
}
doload(vcfile)
FILE *vcfile;
{
	unsigned char name[64], value[128], subval[64];
	int type,cap,eval;
	char *gmem();
	
#ifndef MSDOS	/* VMS or unix */
	char *strdup();
#endif
	      
	while (nextfield(vcfile,name,value))						/* load fields */
	{
		cap=matchcap(name);							/* match our vc_load_defs array */
		if (cap<0)								/* capability unknown */
		{
			printf("VIDEO-C-BADCAP bad key name (lvalue) [%s line %d]",vcpath,vcline);
			exit(0);							/* c ya */
		}
		switch (vc_load_defs[cap].type)						/* check type key/cap/bool/num */
		{
		      case ISKEY:							/* is a key def */
			while (rvalue(subval,value,&type))
			{
				vkp = &vkeydef[vc_load_defs[cap].index];		/* this keys structure */
				
				switch (type)						/* type of value specified */
				{
				      case TYPE_LIT:					/* key sequence description */
					vkp->id = vc_load_defs[cap].index;		/* setup the key id field */
					if (!vkp->value) 
					{
						vkp->value = strdup(subval);		/* dup the string containing sequence */
					}
					else
					{
						vkp = &vkeydef[vkey_ext++];		/* sequence already defined, get extra  */
						vkp->id = vc_load_defs[cap].index;
						vkp->value = strdup(subval);		/* structure, attach sequence to it */
						vkp->eval_to = &vkeydef[vc_load_defs[cap].index]; /* and point to current */
					}	
					break;
				      case TYPE_SYM:					/* symbol: logical xlation */
					vkp->id = vc_load_defs[cap].index;		/* init index */
					eval=matchcap(subval);				/* find struct for symbol specified */
					if (eval<0)					/* bad */
					{
						printf("VIDEO-C-BADCAP bad key name (rvalue) [%s line %d]",vcpath,vcline);
						exit(0);
					}
					vkeydef[vc_load_defs[eval].index].eval_to = vkp; /* make it point here */
					break;
				}
			}
			break;
		      case ISCAP:
			rvalue(subval,value,&type);					/* cut out the field */
			switch (type)
			{
			      case TYPE_LIT:						/* dup the literal sequence */
				vcapdef[vc_load_defs[cap].index] = strdup(subval);
				fix_seq(vcapdef[vc_load_defs[cap].index]);
				break;
			      case TYPE_SYM:
				eval=matchcap(value);						/* symbol: find it */
				vcapdef[vc_load_defs[cap].index] = strdup(vcapdef[eval]);	/* copy his sequence */
				break;
			}
			break;
		      case ISNUM:
			rvalue(subval,value,&type);
			vcapdef[vc_load_defs[cap].index] = gmem(sizeof(int));
			*(vcapdef[vc_load_defs[cap].index]) = atoi(subval);
			break;
		}
	}
}
rvalue(sub,val,type)
unsigned char *sub,*val;
int *type;
{
	int state=S_START;									/* machine state		*/
	int event, ch;										/* event and current input char	*/
	unsigned char *valsave;									/* save start of field buf	*/

	valsave=val;

       	while (state!=S_END)
	{
		ch = *val++;
		switch (ch)								/* decide what event */
		{
		      case C_QUOTE: 
			event=E_QUOTE;
			break;
		      case C_PIPE:
			event=E_PIPE;
			break;
		      case C_ESC:
			event=E_ESC;
			break;
		      case C_CTL:
			event=E_CTL;
			break;
		      case C_NULL:
			event=E_NULL;
			--val;
			break;
		      default:
			event=E_NORM;
			break;
		}
		
		state = stab[event][state];						/* find next state */
		switch (state)
		{
		      case S_LIT0:
			*type=TYPE_LIT;							/* init the type */
			break;
		      case S_SYM:
			*type=TYPE_SYM;							/* fall thru */
		      case S_LIT: 
			*sub++ = ch;							/* copy back */
			break;
		      case S_ESC:							/* is it 'E'? if so, pass ESC */
			switch (ch)
			{
#ifdef	MSDOS
			      case 'X': case 'x':
				*sub++ = EXT_PREFIX;
				break;
#endif
			      case 'E': case 'e':
				*sub++ = 27;
				break;
			      case 'n':
				*sub++ = '\n';
				break;
			      case 'l':
				*sub++ = '\012';
				break;
			      case 'r':
				*sub++ = '\015';
				break;
			      case 't':
				*sub++ = '\011';
				break;
			      case 'b':
				*sub++ = '\010';
				break;
			      case 'f':
				*sub++ = '\014';
				break;
			      case 's':
				*sub++ = ' ';
				break;
			      default:
				if (ch >= '0' && ch <= '9')
				{
					int tmp;
					
					tmp = (ch-0x30)*64;
					tmp += (*val++ -0x30)*8;
					tmp += (*val++ -0x30);
					*sub++ = tmp?tmp:0x80;
				}
				else
				  *sub++ = ch;
				break;
			}
			break;
		      case S_CTL:
		      {
			      int tmp;
			      tmp= ch=='?'?127: toupper(ch) & 0xbf;			/* is it '?'? if so, pass DEL */
			      *sub++ = tmp?tmp:0x80;
			      break;			
		      }
		      case S_END:
			continue;							/* end of field */
		      case S_TERM:
			return FALSE;							/* no more data */
			break;
		      default:
			break;
		}
	}
	*sub=(char)0;
	strcpy(valsave,val);
	return TRUE;
}
build_vc_meta()										/* build the vc_meta parse tree */
{
	char *strchr();
	int i,j,ch,dummy;
	char ch_list[256];
	VC_META_NODE **build_vc_node();

	memset(&meta_top,0,sizeof(VC_META_NODE));					/* zed mem for top node  */

	for (i=0,j=0,memset(ch_list,0,sizeof(ch_list));i<VC_KEY_COUNT+VC_KEY_EXTRA;++i)	
	{
		if (!vkeydef[i].value) continue;					/* null value, skip */
		ch = *(vkeydef[i].value);						/* get 1st byte */
		if (!strchr(ch_list,ch))						/* don't already have it */
		  ch_list[j++]=ch;
	}
	j--;
	
	meta_top.ch= -1;
	meta_top.key_id= -1;
	meta_top.next_hash= j;
	meta_top.next = build_vc_node(ch_list,0,"");
}
VC_META_NODE **build2_vc_node();
VC_META_NODE **build_vc_node(chars,depth,prev_seq)
char *chars;
int depth;
char *prev_seq;
{
	char ch_list[256];
  	int cnt,i,j,k,ch,gotkey,len,key;
	VC_META_NODE **arr,*node;
	int vcm_sort_node();
	char cur_seq[128];
	char *strchr();
	
	cnt=strlen(chars);
	arr=(VC_META_NODE**)gmem(sizeof(VC_META_NODE*)*cnt);
	for (i=0; i<cnt; ++i)
	{
		for (gotkey=0,j=0,k=0,memset(ch_list,0,sizeof(ch_list));j<VC_KEY_COUNT+VC_KEY_EXTRA;++j)
		{
			if ( !vkeydef[j].value ) continue;				/* null value, skip */
			len=strlen(vkeydef[j].value);
			if (depth>(len-1)) continue;
			if (*(vkeydef[j].value+depth)!=chars[i]) continue;
			if (depth) 
			  if (strncmp(vkeydef[j].value,prev_seq,depth)) continue;	/* char not equal */
			if (depth==(len-1))						/* a complete key */
			{
				node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
				node->ch= chars[i];
				if (!vkeydef[j].eval_to) node->key_id = vkeydef[j].id;
				else
				{
					for (vkp= &vkeydef[j]; vkp->eval_to; vkp=vkp->eval_to);
					node->key_id = vkp->id;
				}
				node->next_hash = -1;
				node->next = (VC_META_NODE **)NULL;
				++gotkey;
				key = vkeydef[j].id;
				break;
			}
			ch = *(vkeydef[j].value+depth+1);				/* get nth byte */
			if (!strchr(ch_list,ch))					/* don't already have it */
			  ch_list[k++]=ch;
		}
		if (!gotkey)
		{
			sprintf(cur_seq,"%s%c",prev_seq,chars[i]);
			node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
			node->ch= chars[i];
			node->key_id = 0;
			node->next_hash= k-1;
			node->next = build2_vc_node(ch_list,depth+1,cur_seq);
		}
		else gotkey=0;
	}
	qsort(arr,cnt,sizeof(VC_META_NODE*),vcm_sort_node);
	return arr;
	
}
VC_META_NODE **build2_vc_node(chars,depth,prev_seq)
char *chars;
int depth;
char *prev_seq;
{
	char ch_list[256];
  	int cnt,i,j,k,ch,gotkey,len,key;
	VC_META_NODE **arr,*node;
	int vcm_sort_node();
	char cur_seq[128];
	char *strchr();
	
	cnt=strlen(chars);
	arr=(VC_META_NODE**)gmem(sizeof(VC_META_NODE*)*cnt);
	for (i=0; i<cnt; ++i)
	{
		for (gotkey=0,j=0,k=0,memset(ch_list,0,sizeof(ch_list));j<VC_KEY_COUNT+VC_KEY_EXTRA;++j)
		{
			if ( !vkeydef[j].value ) continue;				/* null value, skip */
			len=strlen(vkeydef[j].value);
			if (depth>(len-1)) continue;
			if (*(vkeydef[j].value+depth)!=chars[i]) continue;
			if (depth) 
			  if (strncmp(vkeydef[j].value,prev_seq,depth)) continue;	/* char not equal */
			if (depth==(len-1))						/* a complete key */
			{
				node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
				node->ch= chars[i];
				if (!vkeydef[j].eval_to) node->key_id = vkeydef[j].id;
				else
				{
					for (vkp= &vkeydef[j]; vkp->eval_to; vkp=vkp->eval_to);
					node->key_id = vkp->id;
				}
				node->next_hash = -1;
				node->next = (VC_META_NODE **)NULL;
				++gotkey;
				key = vkeydef[j].id;
				break;
			}
			ch = *(vkeydef[j].value+depth+1);				/* get nth byte */
			if (!strchr(ch_list,ch))					/* don't already have it */
			  ch_list[k++]=ch;
		}
		if (!gotkey)
		{
			sprintf(cur_seq,"%s%c",prev_seq,chars[i]);
			node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
			node->ch= chars[i];
			node->key_id = 0;
			node->next_hash= k-1;
			node->next = build_vc_node(ch_list,depth+1,cur_seq);
		}
		else gotkey=0;
	}
	qsort(arr,cnt,sizeof(VC_META_NODE*),vcm_sort_node);
	return arr;
	
}
vcm_sort_node(p1,p2)
VC_META_NODE **p1,**p2;
{
	return ((*p1)->ch)-((*p2)->ch);
}
#define VCPOUT(x) vcpbuf[opos++]=(x)
#define TOGGLE(x) (x) = 1-(x)
char *vcparm(str,a1,a2)									/* instantiate a parameterized string */
char *str;										/* format string  */
int a1,a2;										/* integer arguments */
{
	static char vcpbuf[128];
	register int opos, ch, perc;
	int chs[2], item;
	char tmp[4], *p;
	FILE *log;
	
	chs[0]=a1;
	chs[1]=a2;
	perc=opos=item=0;
	while (ch = *str++)
	{
		switch (ch)
		{
		      case '%': 
			if (perc) 
			{
				VCPOUT(ch);              /* is %% */
				perc=0;
			}
			else 
			  perc++;                  /* % leadin */
			break;
		      case '+':
			if (perc)                  /* is %+ */
			{
				ch = *str++;       /* get next */
				chs[item]+=ch;     /* add it */
				VCPOUT(chs[item]); /* output */
				TOGGLE(item);      /* other item */
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case '.':
			if (perc)
			{
				VCPOUT(chs[item]);
				TOGGLE(item);
				perc=0;
			}
			else 
			  VCPOUT(ch);
			break;
		      case '2':
			if (perc)
			{
				sprintf(tmp,"%02d",chs[item]);
				VCPOUT(tmp[0]);
				VCPOUT(tmp[1]);
				TOGGLE(item);
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case '3':
			if (perc)
			{
				sprintf(tmp,"%03d",chs[item]);
				VCPOUT(tmp[0]);
				VCPOUT(tmp[1]);
				VCPOUT(tmp[2]);
				TOGGLE(item);
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case '>':
			if (perc)
			{
				tmp[0] = *str++;
				tmp[1] = *str++;
				if (chs[item]>tmp[0]) chs[item]+=tmp[1];
				VCPOUT(chs[item]);
				TOGGLE(item);
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case 'B':
			if (perc)
			{
				tmp[0] = chs[item];
				chs[item] = (tmp[0]/10)*16 + (tmp[0]%10);
				perc=0;
		}
			else
			  VCPOUT(ch);
			break;
		      case 'd':
			if (perc)
			{
			        sprintf(tmp,"%d",chs[item]);
				for (p=tmp; *p; ++p) VCPOUT(*p);
				TOGGLE(item);
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case 'i':
			if (perc)
			{
				++chs[0];
				++chs[1];
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case 'n':
			if (perc)
			{
				chs[0] ^= 0140;
				chs[1] ^= 0140;
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case 'r':
			if (perc)
			{
				TOGGLE(item);
				perc=0;
			}
			else VCPOUT(ch);
			break;
		      default:
			VCPOUT(ch);
		}
	}
        VCPOUT('\0');
	return vcpbuf;
}

vgetm()											/* get a key, meta or normal */
{
	int ch;

	ch = vgetmeta(meta_top.next,meta_top.next_hash);				/* get a new key, use top of tree */
	if (ch<0) ch = pop();
	if (ch==GENERIC_REFRESH+VMBIAS)
	{
		vrefresh(HARD_REFRESH);
		return vgetm();
	}
	return ch;
}

vgetmeta(node,size)									/* recursively called func to traverse */
											/* the vc_meta tree */
VC_META_NODE **node;									/* starting point for current level */
int size;
{
	unsigned char vgetc();
	int ch,ret;
	static int reading_kbd=TRUE;
	register int low,high,median;

	ch=pop();
	if (ch<0) ch=0x00FF & (int)vgetc();							/* grab a char from kbd */

#ifdef _AIX
	if (ch==(char)0) ch = 0x80;
#else
#ifdef	MSDOS
	if (ch==(char)0) ch = 0x80;
#else
	if (ch==(char)0) ch = -128;
#endif
#endif
	
	for (low=0,high=size,meta_p=(VC_META_NODE *)NULL,meta_pp=node,median=(low+high)/2;
		(meta_pp[median]->ch) != ch && low<=high;)
	{
		median = (low+high)/2;
		if ((meta_pp[median]->ch)>ch) high = median-1; 

		if ((meta_pp[median]->ch)<ch) low = median+1;
	}
	if (meta_pp[median]->ch == ch) meta_p = *(meta_pp+median);
       
	if (!meta_p)									/* no match */
	{
		push(ch);
		return -1;
	}
	if (meta_p->key_id)								/* got a key */
	{
		push(ST_ZED);								/* can discard keystrokes in the stack */
		return meta_p->key_id+VMBIAS;
	}
	ret = vgetmeta(meta_p->next,meta_p->next_hash);
	if (ret<0) push(ch);
	return ret;
}
static int pop()
{
	if (cstackp>=0)
	{
		return cstack[cstackp--];
	}
	else return -1;
}
static int push(ch)
int ch;
{
	if (ch==ST_ZED) cstackp = -1;
	else cstack[++cstackp]=ch;
}
nextfield(fp,name,cap)
FILE *fp;
char *name,*cap;
{
	char inbuf[80];
	int npos,cpos,part;
	char *status,*p,*strchr();
	
	memset(inbuf,0,sizeof(inbuf));							/* zed field buffer */
	do 
	{
		if ((status=fgets(inbuf,80,fp))==0) return 0;
		p=strchr(inbuf,'\n');
		if (p) *p=(char)0;
		++vcline;
		
	} while (inbuf[0]=='#'||inbuf[0]=='\n');					/* get records, skipping comments */
	for (p=inbuf,npos=cpos=part=0; *p;)
	{
		if (part==0 && (*p=='#' || *p=='=') )
		{
			++p;
			++part;
			while (*p==' '||*p=='\t') ++p;
			continue;
		}
		if (part==0) name[npos++]= *p++;
		else cap[cpos++]= *p++;
	}
	name[npos]=(char)0;
	cap[cpos]= (char)0;
	--cpos;
	while (cap[cpos]==' '||cap[cpos]=='\t') cap[cpos--]=(char)0;
	p=strchr(name,' ');
	if (p) *p=(char)0;
	return TRUE;	
}
vcsort(p1,p2)
vc_load *p1, *p2;
{
	return strcmp(p1->name,p2->name);
}

#ifndef MSDOS	/* VMS or unix - This routine already exists for MSDOS.	*/
static char *strdup(str)
unsigned char *str;
{
	static char *mem=(char *)NULL;
	static char *cur;
	int len;
	char *ret;
	
	if (!mem)
	{
		cur=mem=(char *)calloc(VCMEMSIZE,sizeof(char));
		if (!mem) 
		{
			printf("VIDEO-C-VCNOMEM vcap can't calloc %d bytes",VCMEMSIZE);
			exit(0);
		}
	}
	len=strlen(str)+1;
	ret = cur;
	cur += len;
	if (cur > mem+VCMEMSIZE)
	{
		printf("VIDEO-C-MEMUSED vcap used %d bytes. increase VCMEMSIZE",VCMEMSIZE);
		exit(0);
	}
	strcpy(ret,str);
	return ret;
}
#endif	/* VMS or unix */

static char *gmem(size)
int size;
{
	char *ret;
	
	ret=(char *)calloc(size,sizeof(char));
	if (!ret) 
	{
		printf("VIDEO-C-VCNOMEM vcap can't calloc %d bytes",size);
		exit(0);
	}
	return ret;
}
/*
rvalue_type(p)
char *p;
{
	static char *symb="^[a-z0-9_][a-z0-9_]*$";
	static char *lit="^\".*\"$";
        char *c_symb,*c_lit;
	char *strchr();
	
	c_symb=regcmp(symb,(char *)NULL);
	if (regex(c_symb,p)) return TYPE_SYM;
	c_lit=regcmp(lit,(char *)NULL);
	if (regex(c_lit,p)) return TYPE_LIT;

	if (strchr(p,'|')) return TYPE_MLT;

	printf("error rvalue type. field = %s\n",p);
	exit(0);
}
*/
matchcap(name)
unsigned char *name;
{
	register int low,high,median;

	for (low=0,high=sizeof(vc_load_defs)/sizeof(vc_load)-1,median=(low+high)/2;
	     strcmp(name,vc_load_defs[median].name) && low<=high;
	     )
	{
		median=(low+high)/2;
		if (strcmp(name,vc_load_defs[median].name)<0) high=median-1;
		if (strcmp(name,vc_load_defs[median].name)>0) low=median+1;
	}
	if (strcmp(name,vc_load_defs[median].name)==0) return median;
	else 
	{
		printf("VIDEO-E-BADCAPNAME undefined field name at line %d\n",vcline);
		exit(2);
	}
	

}
fix_seq(p)
char *p;
{
	char temp[100],*savep;
	int i,esc,circ;
	
	for (i=0,circ=esc=0,savep=p,memset(temp,0,sizeof(temp)); *p; ++p)
	{
		switch (*p)
		{
		      case '^':
			if (!esc) circ++;
			else { temp[i++] = *p; esc=0; }
			break;
		      case '\\':
			if (!esc) esc++;
			else { temp[i++] = *p; esc=0; }
			break;
		      case 'E':
			if (esc) 
			{
				temp[i++] = 27;
				esc=0;
			}
			else if (circ)
			{
				temp[i++] = 'E' & 0xbf;
				circ=0;
			}
			else temp[i++] = *p;
			break;
		      default:
			if (esc)
			{
				temp[i++] = *p;
				esc=0;
			}
			else if (circ)
			{
				temp[i++] = toupper(*p) & 0xbf;
				circ=0;
			}			
			else temp[i++] = *p;
		}
	}
	strcpy(savep,temp);
}

/*
**	The following are support routines
*/

#undef vre

vcapnull( control_string, cap_string, dispfl )						/* Test if control is defined and warn	*/
char *control_string;
char *cap_string;
int dispfl;
{
	if ( ! control_string || ! *control_string )
	{
		if (dispfl) vre("%%VIDEOCAP-W-NOTDEFINED Capability %s is not defined",cap_string);
		return( 1 );
	}
	else return(0);
}

#ifdef VMS
get_log(name,value,maxl)
char *name;
char *value;
int maxl;
{
	long attr;
	int ret;
	char acc;
	char logbuf[256];
	char tab_name[32];
	struct itemlst {
		short buf_len;
		short i_code;
		char *buf_addr;
		long *len;
		int end;
	};
	struct itemlst items;
	char result[255];
	long act_item_len;
$DESCRIPTOR(log_desc,logbuf);
$DESCRIPTOR(tab_desc,tab_name);

	strcpy(tab_name,"LNM$PROCESS_TABLE");	tab_desc.dsc$w_length   = strlen(tab_name);
	strcpy(logbuf,name);  		log_desc.dsc$w_length   = strlen(logbuf);

	attr = LNM$M_CASE_BLIND;						/* setup attribute value -- ign case */
	acc = PSL$C_USER;							/* set priv level */
	items.i_code = LNM$_STRING;						/* LNM$_STRING = return string value of logical */
	items.buf_len = sizeof(result);						/* build the items list */
	items.buf_addr = result;
	items.len = &act_item_len;

	ret = SYS$TRNLNM(&attr,&tab_desc,&log_desc,&acc,&items);
	strncpy(value,result,*items.len > maxl ? maxl : *items.len);
	value[*items.len]=(char)0;
}

#endif

#ifdef TEST
main()
{
	char foo[10];
	int ch;
#ifdef unix
	struct termio save,new;
#endif
	
	vcapload();
	build_vc_meta();

#ifdef unix
	ioctl(0,TCGETA,&save);
	ioctl(0,TCGETA,&new);
	new.c_cc[VMIN]=1;
	new.c_cc[VTIME]=0;
	new.c_lflag &= ~(ICANON|ECHO);

	ioctl(0,TCSETA,&new);
#endif

	while ((ch=vgetm())!='Q') 
	{
		if (ch<128) 
		{  
			if (ch>=' '&& ch<='~') printf("%c\n",ch); 
			else printf("%d\n",ch);
		}
		else
		switch (ch-128)
		{
		      case GENERIC_PF1:
			printf("pf1\n");
			break;
		      case GENERIC_PF2:
			printf("pf2\n");
			break;
		      case GENERIC_PF3:
			printf("pf3\n");
			break;
		      case GENERIC_PF4:
			printf("pf4\n");
			break;
		      case GENERIC_PF5:
			printf("pf5\n");
			break;
		      case GENERIC_PF6:
			printf("pf6\n");
			break;
		      case GENERIC_PF7:
			printf("pf7\n");
			break;
		      case GENERIC_PF8:
			printf("pf8\n");
			break;
		      case GENERIC_PF16:
			printf("pf16\n");
			break;
		      case GENERIC_HELP:
			printf("help\n");
			break;
		      case GENERIC_UP:
			printf("up\n");
			break;
		      case GENERIC_DOWN:
			printf("down\n");
			break;
		      case GENERIC_LEFT:
			printf("left\n");
			break;
		      case GENERIC_RIGHT:
			printf("right\n");
			break;
		      default:
			printf("<%d>\n",ch-128);
			break;
			
		      
		}
		
	}
#ifdef unix
	ioctl(0,TCSETA,&save);
#endif		
}
getch()
{
	static int i=0;
	static char foo[]="\033[28~\033[33~QQQQQQ";
	
	return(foo[i++]);
}
#endif /*TEST*/
