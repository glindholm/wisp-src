			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

static char *ident = "@(#)vcap.c   1.0    IDSI  Unix/VMS  04/18/90";

#include <stdio.h>
#include <errno.h>

/* #define TEST */
#ifdef TEST
#ifdef unix
#include <termio.h>
#endif
#endif /* TEST */

#ifdef VMS
#include <descrip.h>
#include <lnmdef.h>
#include <psldef.h>
#include <ttdef.h>
#include <stdlib.h>
#endif /* VMS */

#ifdef unix
#include <signal.h>
#include <malloc.h>
#include <curses.h>
#include <term.h>
int resptimeout;
#endif

#ifdef MSDOS
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#endif

#include <errno.h>

#define VCAP

#include "video.h"
#include "vlocal.h"
#include "vcap.h"

static int loaded=0;
static int pop();
static int push();
static char *gmem();

static int vcloadbehavior = VCAP_NEED_NOWRAP|VCAP_NEED_FKEYS1_32|VCAP_WARN_PRMSG;

#ifndef MSDOS	/* VMS or unix */
static	char *strdup();
#endif

vcapload()										/* load up the vcap file */
{
	char *wc,*term_t,*override;
	char *getenv();
	int i,ret=0;
	char wterm_t[80];
	int ti, vi;
	
	if (loaded) return 0;

#ifdef VMS
	/*
	**	Find the terminal type.
	**	wterm_t		Terminal type
	**	vcpath		Full path to videocap file
	*/
	switch (osd_ttype())
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
#ifdef VMS_5_5
		case TT$_VT400_SERIES:
#endif
		case 113:	/* TT$_VT400_SERIES */
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
	if (0==strlen(wterm_t))
	{
		strcpy(wterm_t,term_t);
	}
	sprintf(vcpath,vcpathfmt,wterm_t);
#endif	/*  VMS */
	
#ifndef VMS
	/*
	**	Find the terminal type.
	**	wterm_t		Terminal type
	**	vcpath		Full path to videocap file
	*/

	/*
	**	Check WISPTERM first, if not set then check TERM.
	*/
	if (!(term_t=getenv("WISPTERM")))
	{
		if (!(term_t=getenv("TERM")))
		{
#ifdef unix
			if (! (vcloadbehavior & VCAP_NOWARN_RETSTAT) )
			{
				printf("\n\rVIDEO-E-NOTERM $WISPTERM and/or $TERM variable not defined\n\r");
			}
			ret |= VCAP_WARN_NOWCONFIG;
#endif
			term_t=DEFTERM;
		}
	}
	strcpy(wterm_t,term_t);

#ifdef unix
	memset(vkeydef,0,sizeof(vkeydef));
	memset(vcapdef,0,sizeof(vcapdef));
#endif
	override=getenv("VNOTERMINFO");
	if (override==NULL)
	{
		ti = vcloadterminfo();
	}
	if (wc=getenv("VIDEOINFO"))
	{
		strcpy(vcdir,wc);
	}
	else if (wc=getenv("VIDEOCAP"))
		{
			strcpy(vcdir,wc);
		}
	else if (wc=getenv("WISPCONFIG"))
	{
		vbldfilepath(vcdir,wc,"videocap");
	}
	else
	{
		strcpy(vcdir,VIDEOINFODIR);
	}
	
	vbldfilepath(vcpath,vcdir,wterm_t);						/* build the vc path */


#endif /* !VMS */

	vi=vcloadvideocap(vcpath,wterm_t);

	vc_add_stddefs();

	vc_map_generics();
	
	build_vc_meta();
	++loaded;
	if (vcapdef[PAD]) 
	  padding=atoi(vcapdef[PAD]);
	else 
	  padding=0;
}
vc_map_generics()
{
}
vcloaded()
{
	loaded=TRUE;
}

vcloadvideocap(vcpath,wterm_t)
char *vcpath;
char *wterm_t;
{
	FILE *vcfile;
	int vcsort();

	qsort(vc_load_defs,								/* sort vc_load_defs array */
	      sizeof(vc_load_defs)/sizeof(vc_load)-1,				       /* sizeof less one (don't sort NULL elem)*/
	      sizeof(vc_load),								/* sizeof one element */
	      vcsort);									/* sort routine */

	if ( !(vcfile=fopen(vcpath,"r")))						/* open videocap file */
	{
		int ttype;
		
#ifdef unix
		if (!(vcloadbehavior & VCAP_NOWARN_RETSTAT))
		{
			printf("\n\rVIDEOCAP-E-OPEN Error %d opening videocap file %s\n\r",errno,vcpath);
			sleep((unsigned)4);
		}
#endif
#ifdef MSDOS
		if (0!=strcmp(wterm_t,DEFTERM))
		{
			printf("\n\rVIDEOCAP-E-OPEN Error %d opening videocap file %s\n\r",errno,vcpath);
			sleep((unsigned)4);
		}
#endif
#ifdef MSDOS
		memcpy(vkeydef,msdosdef,sizeof(msdosdef));
#endif
#ifdef VMS
		if (0==strcmp(wterm_t,"vt100") ||
		    0==strcmp(wterm_t,"vt52")  ||
		    0==strcmp(wterm_t,"vt125") ||        
		    0==strcmp(wterm_t,"vt132")   )
		{
			memcpy(vkeydef,vt100def,sizeof(vt100def));
		}
		else
		{
			memcpy(vkeydef,vt220def,sizeof(vt220def));
		}
#endif
	}
	else 
	{
		doload(vcfile);
		fclose(vcfile);
	}
}

vbldfilepath(path,dir,file)
char	*path, *dir, *file;
{
#ifdef unix
	sprintf(path,"%s/%s",dir,file);
#endif
#ifdef MSDOS
	sprintf(path,"%s\\%s",dir,file);
#endif
#ifdef VMS
	sprintf(path,"%s%s",dir,file);
#endif
}
vc_add_stddefs()
{
	extern struct keylist stdkeys[];
	register int idx;
	
	for (idx=0; stdkeys[idx].value; ++idx)
	{
		vc_add_key(stdkeys[idx].index,stdkeys[idx].value,NOMAPPING,SRC_STDDEFS);
	}
}
vc_add_key(index,value,symbidx,source)
int index;
char *value;
int symbidx;                                 	      /* -1 for literal key, index of dest key if mapped */
int source;	      
{	  
	VKEY *key_main, *key_symb;
	
	key_main = &vkeydef[index];		      /* this keys structure */
	if (symbidx != NOMAPPING)
	{
		key_symb = &vkeydef[symbidx];
	}
	else
	{
		key_symb = NULL;
	}

	if (symbidx == NOMAPPING)        	      /* is key literal, ie (does it have a value, byte sequence) */
	{
#ifdef unix
		if (source==SRC_VIDEOINFO)
		{
			register idx;
			for (idx=0; idx<vkey_ext; ++idx)
			{
				if (vkeydef[idx].value && !strcmp(value,vkeydef[idx].value))
				{
					vkeydef[idx].value=NULL;
					vkeydef[idx].id=0;
					vkeydef[idx].source=SRC_EMPTY;
					vkeydef[idx].eval_to=NULL;
				}
			}
		}
#endif
		if (key_main->source == SRC_EMPTY || (key_main->source == SRC_TERMINFO && source==SRC_VIDEOINFO))
		{	  
			key_main->source = source;
			key_main->id = index;  	      /* no, so give it an index (means this key can be returned by vgetm() */
			key_main->value = strdup(value);
		}	  
		else
		{
			vkp = &vkeydef[vkey_ext++];   /* grab the extra struct */
			vkp->id = 0;		      /* id is 0, this sequence generates an id from the other struct  */
			vkp->value = strdup(value);   /* copy value */
			vkp->eval_to = key_main;      /* and point to requested struct */
		}	
	}
	else					      /* or a 'mapped' key */
	{
		if (key_symb->source == SRC_EMPTY)
		{	  
			key_main->id = index;
			key_symb->id = 0;		      /* clear it's id */
			key_symb->eval_to = key_main; /* and point to current */
		}	
		else if (key_symb->eval_to==NULL)
		{
			key_main->id = index;
			key_symb->id = 0;		      /* clear it's id */
			key_symb->eval_to = key_main;
		}
			
	}
}
vc_map_key(index1,index2)
int index1,index2;
{
	vkeydef[index1].eval_to = &vkeydef[index2];
	vkeydef[index1].id = 0;
}
int vc_gotkey(index)
int index;
{
	register int scan;
	register VKEY *keyscanp;
	
	if (vkeydef[index].value && strlen(vkeydef[index].value))
	{
		return TRUE;
	}
	if (vkeydef[index].id)
	{
		for (scan=0; scan<vkey_ext; ++scan)
		{
			if (vkeydef[scan].value == NULL || strlen(vkeydef[scan].value)==0)
			{
				continue;
			}
			keyscanp= &vkeydef[scan];
			while (keyscanp->eval_to)
			{
				keyscanp = keyscanp->eval_to;
			}
			if (keyscanp->id == vkeydef[index].id)
			{
				return TRUE;
			}
		}
	}
	return FALSE;
}
doload(vcfile)
FILE *vcfile;
{
	unsigned char name[64], value[128], subval[64];
	int type,cap,eval;
	char *gmem();
	
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
				switch (type)						/* type of value specified */
				{
				      case TYPE_LIT:					/* key sequence description */
					vc_add_key(vc_load_defs[cap].index,
						    subval,NOMAPPING,SRC_VIDEOINFO);
					break;
				      case TYPE_SYM:					/* symbol: logical xlation */
					eval=matchcap(subval);				/* find struct for symbol specified */
					if (eval<0)					/* bad */
					{
						printf("VIDEO-C-BADCAP bad key name (rvalue) [%s line %d]",vcpath,vcline);
						exit(0);
					}
					vc_add_key(vc_load_defs[cap].index,NULL,
						   vc_load_defs[eval].index,SRC_VIDEOINFO);
					
					break;
				}
			}
			break;
		      case ISCAP:
			rvalue(subval,value,&type);					/* cut out the field */
			switch (type)
			{
#ifdef unix
				char *fixtcti, *vtctoti();
#endif
			      case TYPE_LIT:						/* dup the literal sequence */
#ifdef unix
				if (vc_load_defs[cap].index == CHANGE_SCROLL_REGION ||
				    vc_load_defs[cap].index == CURSOR_ADDRESS)
				{
					fixtcti = vtctoti(subval);
					vcapdef[vc_load_defs[cap].index] = strdup(fixtcti);
				}
				else
				{
					vcapdef[vc_load_defs[cap].index] = strdup((char *)subval);
				}
#else
				vcapdef[vc_load_defs[cap].index] = strdup((char *)subval);				
#endif
				fix_seq(vcapdef[vc_load_defs[cap].index]);
				break;
			      case TYPE_SYM:
				eval=matchcap(value);						/* symbol: find it */
				vcapdef[vc_load_defs[cap].index] = strdup(vcapdef[eval]);/* copy his sequence */
				break;
			}
			break;
		      case ISNUM:
			rvalue(subval,value,&type);
			vcapdef[vc_load_defs[cap].index] = gmem(sizeof(int));
			*(vcapdef[vc_load_defs[cap].index]) = atoi((char *)subval);
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
	strcpy((char *)valsave,(char *)val);
	return TRUE;
}
build_vc_meta()										/* build the vc_meta parse tree */
{
	char *strchr();
	int i,j,ch,dummy;
	unsigned char ch_list[256];
	VC_META_NODE **build_vc_node();

	memset(&meta_top,0,sizeof(VC_META_NODE));					/* zed mem for top node  */

	for (i=0,j=0,memset(ch_list,0,sizeof(ch_list));i<VC_KEY_COUNT+VC_KEY_EXTRA;++i)	
	{
		if (!vkeydef[i].value) continue;					/* null value, skip */
		ch = *((unsigned char *)vkeydef[i].value);				/* get 1st byte */
		if (!strchr((char *)ch_list,(char)ch))					/* don't already have it */
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
unsigned char *chars;
int depth;
unsigned char *prev_seq;
{
	unsigned char ch_list[256];
  	int cnt,i,j,k,ch,gotkey,len,key;
	VC_META_NODE **arr,*node;
	int vcm_sort_node();
	unsigned char cur_seq[128];
	char *strchr();
	
	cnt=strlen((char *)chars);
	arr=(VC_META_NODE**)gmem(sizeof(VC_META_NODE*)*cnt);
	for (i=0; i<cnt; ++i)
	{
		for (gotkey=0,j=0,k=0,memset(ch_list,0,sizeof(ch_list));j<VC_KEY_COUNT+VC_KEY_EXTRA;++j)
		{
			if ( !vkeydef[j].value ) continue;				/* null value, skip */
			len=strlen(vkeydef[j].value);
			if (depth>(len-1)) continue;
			if (*((unsigned char *)vkeydef[j].value+depth)!=chars[i]) continue;
			if (depth) 
			  if (strncmp((char *)vkeydef[j].value,(char *)prev_seq,depth)) continue;	/* char not equal */
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
			ch = *((unsigned char*)vkeydef[j].value+depth+1);		/* get nth byte */
			if (!strchr((char *)ch_list,(char)ch))				/* don't already have it */
			  ch_list[k++]=ch;
		}
		if (!gotkey)
		{
			sprintf((char *)cur_seq,"%s%c",prev_seq,chars[i]);
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
unsigned char *chars;
int depth;
unsigned char *prev_seq;
{
	unsigned char ch_list[256];
  	int cnt,i,j,k,ch,gotkey,len,key;
	VC_META_NODE **arr,*node;
	int vcm_sort_node();
	unsigned char cur_seq[128];
	char *strchr();
	
	cnt=strlen((char *)chars);
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
			  if (strncmp((char *)vkeydef[j].value,(char *)prev_seq,depth)) continue;	/* char not equal */
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
			if (!strchr((char *)ch_list,(char)ch))				/* don't already have it */
			  ch_list[k++]=ch;
		}
		if (!gotkey)
		{
			sprintf((char *)cur_seq,"%s%c",prev_seq,chars[i]);
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
/*
 * vcparm is no longer used under Unix only. Other OSs use it. since terminfo has been integrated, it was necessary to
 * choose either termcap style parameterized strings (which vcparm decodes) or 
 * terminfo style (which tparm decodes).  Since we could possibly have both types
 * at once (we load terminfo and videoinfo), we either need to convert the videocap (termcap style)
 * strings to terminfo or vise versa.  We are converting the videocap defs to terminfo style
 * and using the tparm instead of vcparm.  The routine vtctoti() converts the videocap strings
 * into a tparm digestible form.
 *
 */
#define VCPOUT(x) vcpbuf[opos++]=(x)
#define TOGGLEITEM(x) (x) = 1-(x)
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
				TOGGLEITEM(item);      /* other item */
				perc=0;
			}
			else
			  VCPOUT(ch);
			break;
		      case '.':
			if (perc)
			{
				VCPOUT(chs[item]);
				TOGGLEITEM(item);
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
				TOGGLEITEM(item);
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
				TOGGLEITEM(item);
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
				TOGGLEITEM(item);
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
				TOGGLEITEM(item);
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
				TOGGLEITEM(item);
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
vgetm_timed(seconds,status)
int seconds;
int *status;
{
	int ch;

	vtimeout(seconds);
	ch = vgetmeta(meta_top.next,meta_top.next_hash);				/* get a new key, use top of tree */
	if (*status = vtimeout_check())
	{
		vtimeout_clear();
		ch = 0;
	}
	if (ch<0) ch = pop();
	vtimeout(0);
	return ch;
}
vgetm()											/* get a key, meta or normal */
{
	int ch;

	ch = vgetmeta(meta_top.next,meta_top.next_hash);				/* get a new key, use top of tree */
	if (ch<0) ch = pop();
	return ch;
}

vgetmeta(node,size)									/* recursively called func to traverse */
											/* the vc_meta tree */
VC_META_NODE **node;									/* starting point for current level */
int size;
{
        char vgetc();
	int ch,ret;
	static int reading_kbd=TRUE;
	register int low,high,median;

	ch=pop();
	if (ch<0) ch=(unsigned char)vgetc();						/* grab a char from kbd */

	if (ch==(char)0)
	{
		if (vtimeout_check())
		{
			return -1;
		}
		else
		{
			ch = 0x80;
		}
	}
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
		/* 
		**	part==0  LHS	"name"
		**	part==1  RHS	"cap"
		*/
		if (part==0 && (*p=='#' || *p=='=') )		
		{
			++p;							/* Point past the "="				*/
			++part;							/* Switch to LHS "cap"				*/
			while (*p==' '||*p=='\t') ++p;				/* Skip whitespace after "="			*/
			continue;						/* continue in case we've got a NULL		*/
		}
		if (part==0) 
			name[npos++]= *p++;					/* Build the "name"				*/
		else 
			cap[cpos++]= *p++;					/* Build the "cap"				*/
	}
	name[npos]=(char)0;							
	cap[cpos]= (char)0;
	--cpos;
	--npos;
	while ( cap[cpos]==' '||  cap[cpos]=='\t')  cap[cpos--]=(char)0;	/* remove trailing whitespace			*/
	while (name[npos]==' '|| name[npos]=='\t') name[npos--]=(char)0;
#ifdef OLD
	p=strchr(name,' ');
	if (p) *p=(char)0;
#endif /* OLD */

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
	     strcmp((char *)name,vc_load_defs[median].name) && low<=high;
	     )
	{
		median=(low+high)/2;
		if (strcmp((char *)name,vc_load_defs[median].name)<0) high=median-1;
		if (strcmp((char *)name,vc_load_defs[median].name)>0) low=median+1;
	}
	if (strcmp((char *)name,vc_load_defs[median].name)==0) return median;
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
vcloadsetup(flag)
int flag;
{
	vcloadbehavior = flag;
}
vcloadterminfo()
{
#ifdef unix
	int rc;
	int size;
	char tmpbuf[1024];
	char *trm, *getenv();
	
	trm=getenv("TERM");
	setupterm(trm,0,&rc);
	if (rc==1)
	{
		if (cursor_address)
		{
			vcapdef[CURSOR_ADDRESS] = strdup((char*) (cursor_address));
			fix_seq(vcapdef[CURSOR_ADDRESS]);
		}
		vcapdef[INIT_TERMINAL] = gmem(1024);
		vcapdef[RESET_TERMINAL] = gmem(1024);		
		if (init_1string)
		{
			strcat(vcapdef[INIT_TERMINAL],init_1string);
		}
		if (init_2string)
		{
			strcat(vcapdef[INIT_TERMINAL],init_2string);
		}
		if (init_3string)
		{
			strcat(vcapdef[INIT_TERMINAL],init_3string);
		}
		if (enter_ca_mode)
		{
			strcat(vcapdef[INIT_TERMINAL],enter_ca_mode);
		}
		if (!init_1string && !init_2string && !init_3string && !enter_ca_mode)
		{
			if (reset_1string)
			{
				strcat(vcapdef[INIT_TERMINAL],reset_1string);
			}
			if (reset_2string)
			{
				strcat(vcapdef[INIT_TERMINAL],reset_2string);
			}
			if (reset_3string)
			{
				strcat(vcapdef[INIT_TERMINAL],reset_3string);
			}
		}
		if (keypad_xmit)
		{
			strcat(vcapdef[INIT_TERMINAL],keypad_xmit);
		}
		if (reset_1string)
		{
			strcat(vcapdef[RESET_TERMINAL],reset_1string);
		}
		if (reset_2string)
		{
			strcat(vcapdef[RESET_TERMINAL],reset_2string);
		}
		if (reset_3string)
		{
			strcat(vcapdef[RESET_TERMINAL],reset_3string);
		}
		if (exit_ca_mode)
		{
			strcat(vcapdef[RESET_TERMINAL],exit_ca_mode);
		}
		if (keypad_local)
		{
			strcat(vcapdef[RESET_TERMINAL],keypad_local);
		}
		
#define VC_COPY_TI(def,cap) if (cap) vcapdef[def] = strdup((char *)cap); else vcapdef[def]=""

		VC_COPY_TI(SAVE_CURSOR,save_cursor);
		VC_COPY_TI(RESTORE_CURSOR,restore_cursor);
		VC_COPY_TI(CHANGE_SCROLL_REGION,change_scroll_region);

		if (clear_screen)
		{
			char *p;
			
			p=strdup(clear_screen);
			vcapdef[19]=p;
			
		}
		
#ifdef clr_bol
		VC_COPY_TI(CLEAR_BOL,clr_bol);
#endif
		VC_COPY_TI(CLEAR_EOL,clr_eol);
		VC_COPY_TI(CLEAR_EOS,clr_eos);
		VC_COPY_TI(CURSOR_DOWN,cursor_down);
		VC_COPY_TI(CURSOR_UP,cursor_up);
		VC_COPY_TI(CURSOR_LEFT,cursor_left);
		VC_COPY_TI(CURSOR_RIGHT,cursor_right);
		VC_COPY_TI(CURSOR_HOME,cursor_home);
		if (cursor_visible)
		{
			VC_COPY_TI(CURSOR_VISIBLE,cursor_visible);
		}
		else
		{
			VC_COPY_TI(CURSOR_VISIBLE,cursor_normal);
		}
		VC_COPY_TI(CURSOR_INVISIBLE,cursor_invisible);		
		VC_COPY_TI(ENTER_BLINK_MODE,enter_blink_mode);
		VC_COPY_TI(ENTER_BOLD_MODE,enter_bold_mode);
		VC_COPY_TI(ENTER_REVERSE_MODE,enter_reverse_mode);
		VC_COPY_TI(ENTER_STANDOUT_MODE,enter_standout_mode);
		VC_COPY_TI(ENTER_UNDERLINE_MODE,enter_underline_mode);		
		VC_COPY_TI(EXIT_ATTRIBUTE_MODE,exit_attribute_mode);
		VC_COPY_TI(SCROLL_REVERSE,scroll_reverse);
		if (strncmp(trm,"vt1",3)==0||
		    strncmp(trm,"vt2",3)==0||
		    strncmp(trm,"vt3",3)==0)
		{
			/* use vt style default line and pblanks stuff here */
		}
		else
		{
			/* use regular ascii chars and/or flag to warn */
		}
		VC_COPY_TI(REVERSE_INDEX,scroll_reverse);
		VC_COPY_TI(ENTER_GRAPHICS_MODE,enter_alt_charset_mode);
		VC_COPY_TI(EXIT_GRAPHICS_MODE,exit_alt_charset_mode);		
		
		vcapdef[GRAPHSTR] = gmem(14);

		vcapdef[NARROW_MODE]="";
		vcapdef[WIDE_MODE]="";
                vcapdef[SCREEN_NORMAL_MODE]="";
	        vcapdef[SCREEN_REVERSE_MODE]="";
		vcapdef[ENTER_AM_MODE]="";
		vcapdef[EXIT_AM_MODE]="";
		
		VC_COPY_TI(ENTER_INSERT_MODE,enter_insert_mode);
		VC_COPY_TI(EXIT_INSERT_MODE,exit_insert_mode);
		

#ifdef box_chars_1
		if (box_chars_1)
		{
			vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER]  = box_chars_1[0];
			vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR]     = box_chars_1[1];
			vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER] = box_chars_1[2];
			vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR]       = box_chars_1[3];
			vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER] = box_chars_1[4];
			vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER]  = box_chars_1[5];
			vcapdef[GRAPHSTR][SINGLE_UPPER_TEE]          = box_chars_1[6];
			vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE]          = box_chars_1[7];
			vcapdef[GRAPHSTR][SINGLE_LOWER_TEE]          = box_chars_1[8];
			vcapdef[GRAPHSTR][SINGLE_LEFT_TEE]           = box_chars_1[9];
			vcapdef[GRAPHSTR][SINGLE_CROSS]              = box_chars_1[10];
		}
		else
		{
			vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER]  = '*';
			vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR]     = '-';
			vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER] = '*';
			vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR]       = '|';
			vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER] = '*';
			vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER]  = '*';
			vcapdef[GRAPHSTR][SINGLE_UPPER_TEE]          = '*';
			vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE]          = '*';
			vcapdef[GRAPHSTR][SINGLE_LOWER_TEE]          = '*';
			vcapdef[GRAPHSTR][SINGLE_LEFT_TEE]           = '*';
			vcapdef[GRAPHSTR][SINGLE_CROSS]              = '*';
			vcapdef[ENTER_GRAPHICS_MODE] = "";
			vcapdef[EXIT_GRAPHICS_MODE] = "";
		}
#else
		vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER]  = '*';
		vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR]     = '-';
		vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER] = '*';
		vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR]       = '|';
		vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER] = '*';
		vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER]  = '*';
		vcapdef[GRAPHSTR][SINGLE_UPPER_TEE]          = '*';
		vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE]          = '*';
		vcapdef[GRAPHSTR][SINGLE_LOWER_TEE]          = '*';
		vcapdef[GRAPHSTR][SINGLE_LEFT_TEE]           = '*';
		vcapdef[GRAPHSTR][SINGLE_CROSS]              = '*';
		vcapdef[ENTER_GRAPHICS_MODE] = "";
		vcapdef[EXIT_GRAPHICS_MODE] = "";
#endif
		if (key_down)   vc_add_key(VKEY_DOWN_ARROW,key_down,NOMAPPING,SRC_TERMINFO);
		if (key_up)     vc_add_key(VKEY_UP_ARROW,key_up,NOMAPPING,SRC_TERMINFO);
		if (key_left)   vc_add_key(VKEY_LEFT_ARROW,key_left,NOMAPPING,SRC_TERMINFO);
		if (key_right)  vc_add_key(VKEY_RIGHT_ARROW,key_right,NOMAPPING,SRC_TERMINFO);
		if (key_f1)     vc_add_key(VKEY_F1,key_f1,NOMAPPING,SRC_TERMINFO);
		if (key_f2)     vc_add_key(VKEY_F2,key_f2,NOMAPPING,SRC_TERMINFO);
		if (key_f3)     vc_add_key(VKEY_F3,key_f3,NOMAPPING,SRC_TERMINFO);
		if (key_f4)     vc_add_key(VKEY_F4,key_f4,NOMAPPING,SRC_TERMINFO);
		if (key_f5)     vc_add_key(VKEY_F5,key_f5,NOMAPPING,SRC_TERMINFO);
		if (key_f6)     vc_add_key(VKEY_F6,key_f6,NOMAPPING,SRC_TERMINFO);
		if (key_f7)     vc_add_key(VKEY_F7,key_f7,NOMAPPING,SRC_TERMINFO);
		if (key_f8)     vc_add_key(VKEY_F8,key_f8,NOMAPPING,SRC_TERMINFO);
		if (key_f9)     vc_add_key(VKEY_F9,key_f9,NOMAPPING,SRC_TERMINFO);
		if (key_f10)    vc_add_key(VKEY_F10,key_f10,NOMAPPING,SRC_TERMINFO);
#ifdef key_f11
		if (key_f11)    vc_add_key(VKEY_F11,key_f11,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f12
		if (key_f12)    vc_add_key(VKEY_F12,key_f12,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f13
		if (key_f13)    vc_add_key(VKEY_F13,key_f13,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f14
		if (key_f14)    vc_add_key(VKEY_F14,key_f14,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f15
		if (key_f15)    vc_add_key(VKEY_F15,key_f15,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f16
		if (key_f16)    vc_add_key(VKEY_F16,key_f16,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f17
		if (key_f17)    vc_add_key(VKEY_F17,key_f17,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f18
		if (key_f18)    vc_add_key(VKEY_F18,key_f18,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f19
		if (key_f19)    vc_add_key(VKEY_F19,key_f19,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f20
		if (key_f20)    vc_add_key(VKEY_F20,key_f20,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f21
		if (key_f21)    vc_add_key(VKEY_F21,key_f21,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f22
		if (key_f22)    vc_add_key(VKEY_F22,key_f22,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f23
		if (key_f23)    vc_add_key(VKEY_F23,key_f23,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f24
		if (key_f24)    vc_add_key(VKEY_F24,key_f24,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f25
		if (key_f25)    vc_add_key(VKEY_F25,key_f25,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f26
		if (key_f26)    vc_add_key(VKEY_F26,key_f26,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f27
		if (key_f27)    vc_add_key(VKEY_F27,key_f27,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f28
		if (key_f28)    vc_add_key(VKEY_F28,key_f28,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f29
		if (key_f29)    vc_add_key(VKEY_F29,key_f29,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f30
		if (key_f30)    vc_add_key(VKEY_F30,key_f30,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f31
		if (key_f31)    vc_add_key(VKEY_F31,key_f31,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_f32
		if (key_f32)    vc_add_key(VKEY_F32,key_f32,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_ppage
		if (key_ppage)  vc_add_key(VKEY_PREV_SCR,key_ppage,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_npage
		if (key_npage)  vc_add_key(VKEY_NEXT_SCR,key_npage,NOMAPPING,SRC_TERMINFO);
#endif
#ifdef key_ic
		if (key_ic)     vc_add_key(VKEY_INSERT,key_ic,NOMAPPING,SRC_TERMINFO);
#endif
		return TRUE;
	}
	else
	{
		return FALSE;
	}
#endif
}

char *vtctoti(strin)
char *strin;
{
	static char buffer[128];
	char tmpbuf[128];
	int parm=0, rflag=FALSE;
	register char *scanidx;
	
	for (scanidx=strin; *scanidx; ++scanidx)
	{
		if (*scanidx == '%') 
		{
			scanidx++;
			switch (*scanidx) 
			{
			      case 'r':
				rflag = TRUE;
				break;
			      default:
				break;	/* ignore */
			}
		}
	}

	buffer[0]=(char)0;
	while (*strin) 
	{
		switch (*strin) 
		{
		      case '%':
			strin++;
			switch (*strin) 
			{
			      case '%':
				strcat(buffer,"%%");
				break;
			      case 'i':
				strcat(buffer,"%i");
				break;			
			      case 'd':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%d");
					}
					else
					{
						strcat(buffer,"%p1%d");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%d", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			      case '2':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%02d");
					}
					else
					{
						strcat(buffer,"%p1%02d");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%02d", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			      case '3':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%03d");
					}
					else
					{
						strcat(buffer,"%p1%03d");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%03d", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			      case '.':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%c");
					}
					else
					{
						strcat(buffer,"%p1%c");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%c", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			      case '+':
				strin++;
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						sprintf(tmpbuf,"%%p2%%'%c'%%+%%c", *strin);
						strcat(buffer,tmpbuf);
					}
					else
					{
						sprintf(tmpbuf,"%%p1%%'%c'%%+%%c", *strin);
						strcat(buffer,tmpbuf);
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%'%c'%%+%%c", parm, *strin);
					strcat(buffer,tmpbuf);
				}
				break;
			      default:
				break;
			}
			++strin;
			break;
		      default:
			tmpbuf[0]= *strin;
			tmpbuf[1]= (char)0;
			strcat(buffer,tmpbuf);
			++strin;
			break;
		}
	}
	return buffer;
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
	static char logbuf[256];
	static char tab_name[32];
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

#include "inc:vcap.d"

	strcpy(tab_name,"LNM$PROCESS_TABLE");	tab_desc.dsc$w_length   = strlen(tab_name);
	strcpy(logbuf,name);  		log_desc.dsc$w_length   = strlen(logbuf);

	attr = LNM$M_CASE_BLIND;						/* setup attribute value -- ign case */
	acc = PSL$C_USER;							/* set priv level */
	items.i_code = LNM$_STRING;						/* LNM$_STRING = return string value of logical */
	items.buf_len = sizeof(result);						/* build the items list */
	items.buf_addr = result;
	items.len = &act_item_len;

	ret = SYS$TRNLNM(&attr,&tab_desc,&log_desc,&acc,&items);
	strncpy(value,result,*items.len > maxl-1 ? maxl-1 : *items.len);
	value[*items.len > maxl-1 ? maxl-1 : *items.len ]=(char)0;
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
