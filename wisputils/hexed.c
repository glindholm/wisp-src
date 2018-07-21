/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/

static char rcsid[]="$Id: hexed.c,v 1.32 2003/06/20 15:37:44 gsl Exp $"; /* USED BELOW */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <io.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include "vlocal.h"
#include "video.h"
#include "vcap.h"
#include "vdata.h"
#include "intdef.h"

#define RCSVERS "$Revision: 1.32 $"
#define RCSDATE "$Date: 2003/06/20 15:37:44 $"

static char VERSION[5];
static char MODDATE[20];
static int display_8bit = 0;

#define D_LINES 20
#define D_COLS 16

FILE *fil;
char linebuf[200],
     tmpbuf[120],
     filebuf[D_COLS*D_LINES+1],
     curfile[100];

#define ABS_OFS 0
#define REL_OFS 1
#define END_OFS 2


extern int VL_vgets0(char* string, int count);

static void dopage(void);
static int doedit(void);

static int mv_left();
static int mv_right();
static int mv_down();
static int mv_up();
static int getyn(char* str);
static int goto_ofs();
static unsigned int xtoi(char* tmp);
static int hexdig(char pch);
static int errstatus(char* str);
static void clear_errstatus();
static int bldline();
static int memsetnull(char* buf,int ch,int cnt);
static void title();
static void show_cmds();
static void decode_desc();
static void ver_status();
static int hexed_status(void);
static int getsize(char *p);
static void modbuffer(int data);
static int next_screen(void);
static void gethexpat(char *pat);
static void getascpat(char *pat);
static void do_find(char *pattern);
static int match(unsigned char *buf, unsigned char *pat);
static char *reptstr(int cnt, int ch);

static int write_allowed = 0;

struct cmd 
{
	int ch;
	int (*fn)();
	char *desc;
};

static int	find(void), 
	toggle_verify(void),
	toggle_8bit(void),
	new_file(void),
	quit_hexed(void),
	goto_ofs(void),
	toggle_hex_asc(void), 
	prev_screen(void),
	mv_left(void),
	mv_right(void),
	mv_up(void),
	mv_down(void),
  write_buf(void), read_buf(void);
 
#define CTRL(x) ((x)&0x1f)
struct cmd cmds[]=
{
	{ GENERIC_PF1+VMBIAS, read_buf, "F1\\-ReRead" },
	{ GENERIC_PF2+VMBIAS, write_buf, "F2\\-Write" },	      
	{ GENERIC_LEFT+VMBIAS, mv_left, "" },
	{ GENERIC_DOWN+VMBIAS, mv_down, "" },
	{ GENERIC_UP+VMBIAS, mv_up, "" },
	{ GENERIC_RIGHT+VMBIAS, mv_right, "" },
	{ GENERIC_PF3+VMBIAS, find, "F3\\-Find" },
	{ GENERIC_PF4+VMBIAS, goto_ofs, "F4\\-Goto" },
	{ GENERIC_PF5+VMBIAS, toggle_verify, "F5\\-Verify" },
	{ GENERIC_PF7+VMBIAS, new_file, "F7\\-NewFile" },
	{ GENERIC_PF8+VMBIAS, toggle_8bit, "F8\\-8Bit" },
	{ CTRL('i'), toggle_hex_asc, "TAB\\-Hex/Ascii" },
	{ GENERIC_TAB+VMBIAS, toggle_hex_asc, "" },
	{ GENERIC_PF9+VMBIAS, prev_screen, "F9\\-Prev" },
	{ GENERIC_PF10+VMBIAS, next_screen, "F10\\-Next" },
	{ CTRL('n'), next_screen, "" },
	{ CTRL('p'), prev_screen, "" },
	{ GENERIC_PF16+VMBIAS, quit_hexed, "F16\\-Quit" },
	{ 0,0,0 }
};
static struct cmd *cmdptr;

#define D_BYTE 0
#define D_WORD 1
#define D_LONG 2
#define N_HIGH 0
#define N_LOW 1

static unsigned long filepos=0;
static unsigned long filesize;

static unsigned long ofilepos= 999999; /* Start at an arbitrary and unlikely position. */
static int scrx,scry,nib;
static int quitflag=0;
static int verify=1;
static int bufmod=0;
static int hexasc=0;

static int edl,edc;

#define FOREVER while(1)
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

int main(int c, char **v)
{
	char *p,*e;
	char the_title[80];
	
	memset(VERSION,0,sizeof(VERSION));
	p=strchr(rcsid,' ');
	p=strchr(++p,' ');
	e=strchr(++p,' ');
	
	memcpy(VERSION,p,e-p);
	memset(MODDATE,0,sizeof(MODDATE));
	p=strchr(++e,' ');
	memcpy(MODDATE,e,p-e);

	if (c==1) 
	{
		printf("Usage:    hexed {filename}\n");
		exit(0);
	}
	
	strcpy(curfile,v[1]);
	fil=fopen(v[1],"rb+");
	if (fil)
	{
		write_allowed = 1;
	}
	else
	{
		write_allowed = 0;
		fil=fopen(v[1],"rb");
		if (!fil) 
		{
			printf("hexed: Unable to open %s\n",v[1]);
			exit(1);
		}
	}
	filesize=getsize(curfile);
	
	sprintf(the_title,"HEXED - %s", curfile);
	VL_vtitle(the_title);
	VL_vstate(0);
	VL_verase(FULL_SCREEN);
	title();
	edl=0;edc=10;
	scrx=0; scry=0; nib=N_HIGH;
	while (!quitflag)
	{
		if (filepos != ofilepos)
		{		
			if (bufmod)
			{
				write_buf();
			}
			read_buf();
		}
		dopage();
		hexed_status();
		quitflag=doedit();
		edl=scry;
		if (hexasc==0)
		{
			edc=10+(scrx*3)+nib;
		}
		else
		{
			edc=59+scrx;
		}	
		VL_vmove(edl,edc);
	}
	VL_vexit();
	fclose(fil);
	return(0);
	
}
static int write_buf(void)
{
	int inkey;

	if (!write_allowed) 
	{
		VL_vmove(23,60);
		VL_vmode(VMODE_BOLD);
		VL_vprint("READ ONLY!");
		VL_vmode(VMODE_CLEAR);
		return 1;
	}
	
	if (verify)	
	{
		inkey=getyn("write buffer? (y/n)");
	}
	
	if (!verify || inkey=='y' || inkey=='Y')
	{
		fseek(fil,ofilepos,ABS_OFS);
		fwrite(filebuf,sizeof(filebuf),1,fil);
	}

	bufmod=FALSE;
	return 0;
}
static int read_buf(void)
{
	memset(filebuf,0,sizeof(filebuf));
	fseek(fil,filepos,ABS_OFS);
	fread(filebuf,sizeof(filebuf),1,fil);
	bufmod=FALSE;
	ofilepos=filepos;
	return 0;
}
static void dopage(void)
{
	register int i;
	
	for (i=0; i<D_LINES; ++i)
	{
		bldline(filepos+(i*D_COLS),filebuf+i*D_COLS,linebuf);
		VL_vmove(i,0);
		VL_vprint(linebuf);
	}
}
static int doedit(void)
{
	int key,data;

	VL_vmove(edl,edc);
	key=VL_vgetm();
	if (key >='a' && key <='z')
		key = tolower(key);
	data=1;
	for (cmdptr=cmds; cmdptr->ch; ++cmdptr)
	{
		if (key==cmdptr->ch) 
		{
			(*(cmdptr->fn))();
			data=0;
			break;
		}
	}
	if (data) { modbuffer(key); mv_right(); }
	if (quitflag) return TRUE;
	else return FALSE;

}
static int next_screen(void)
{
	if (filepos + D_LINES*D_COLS >filesize) 
	{
		VL_vbell();
		return 0;
	}
	filepos+= D_LINES*D_COLS;
	return 0;
}
static int prev_screen(void)
{
	filepos-=  D_LINES*D_COLS;
	if (filepos&0x80000000) filepos=0;
	return 0;
}

static int toggle_hex_asc(void)
{
	hexasc=1-hexasc;
	return 0;
}	      
static void modbuffer(int data)
{
	int val;

	if (hexasc==0)
	{
		data=toupper(data);
		if (data>='0'&&data<='9') val=data-'0';
		else if (data>='A'&&data<='F') val=data-'A'+10;
		else return;
		if (nib==N_HIGH) 
		{
			filebuf[scry*16+scrx] &= 0x0f;
			filebuf[scry*16+scrx] |= val << 4;
		}
		else
		{
			filebuf[scry*16+scrx] &= 0xf0;
			filebuf[scry*16+scrx] |= (val&0x0f) ;
		}
		bufmod=TRUE;
	}
	else
	{
		filebuf[scry*16+scrx]=data;
		bufmod=TRUE;
	}
}
static int find(void)
{
	int ch;
	static char pat[64];
	static int havepat=FALSE;
	
	VL_vmode(VMODE_CLEAR);
	VL_vmove(D_LINES+2,0);
	VL_verase(TO_EOL);
loop:	VL_vprint("Find (F3=repeat, H=Hex, A=ASCII, X=abort) ->");
	ch=VL_vgetm();	
	VL_vmove(D_LINES+2,0);
	VL_verase(TO_EOL);
/*	show_cmds();*/
	switch (ch)
	{
		case 'h': case 'H': 
		  gethexpat(pat);
		  hexasc=0;
		  havepat=TRUE;
		  break;
		case 'a': case 'A':
		  getascpat(pat);
		  hexasc=1;
		  havepat=TRUE;
		  break;
		case 'x': case 'X':
		  show_cmds();
		  return 0;
		case GENERIC_PF3+VMBIAS:
		  break;
		default:
		  VL_vbell();
		  goto loop;

	}
	show_cmds();
	if (havepat) do_find(pat);
	return 0;
}
static void do_find(char *pattern)
{
	int searching=1,spos;
	int sfilepos,sspos,sscry,sscrx;
	int sedl,sedc;
	
 	spos=scry*16+scrx + 1;

	sfilepos=filepos;
	sspos=spos;
	sscry=scry;
	sscrx=scrx;
	sedl=edl;
	sedc=edc;
	
	while (searching)
	{
		while (spos < D_COLS*D_LINES)
		{
			if (match((unsigned char *)&filebuf[spos],(unsigned char *)pattern)) 
			{
/*				filepos += spos;*/
				dopage();
				scry = spos / 16;
				scrx = spos % 16;
				return ;
			}
			else ++spos;
		}
		if (filepos+D_COLS*D_LINES>filesize)
		{
			VL_vbell();
			filepos=sfilepos;
			spos=sspos;
			sscrx=scrx;
			sscry=scry;
			edl=sedl;
			edc=sedc;
			VL_vmove(edl,edc);
			read_buf();
			dopage();
			errstatus("pattern not found");
#ifdef unix			
			signal(SIGALRM,clear_errstatus);
			alarm(3);
#endif
			
			return ;
		}
		filepos+= D_COLS*D_LINES;
		read_buf();
		spos=0;
	}
}
static int match(unsigned char *buf, unsigned char *pat)
{
	for (;*pat;++pat,++buf)
	{
		if (*pat == 0xff) continue;
		if (*pat != *buf) return 0;
	}
	return 1;
}
static void gethexpat(char *pat)
{
	char tmp[64],*p=tmp, *patsav=pat;
	char d1,d2,v1,v2;
	int bad=FALSE;
	
	
	VL_vmove(D_LINES+2,0); VL_verase(TO_EOL);
	VL_vprint("Enter Hex pattern [\?\?=wildcard]->");
/*                        1         2         3         4         5         6         7     */
/*              01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
	VL_vmove(D_LINES+2,33);
	VL_vgets0(tmp,63);
	VL_vmove(D_LINES+2,0); VL_verase(TO_EOL);
	VL_vmode(VMODE_CLEAR);
	VL_vmove(edl,edc);
	while (*p)
	{
		d1 = *p;
		if (*(p+1)) d2 = *(p+1);
		else { bad=TRUE; break; }
		p += 2;
		if ((d1=='?' && d2!='?') ||
		    (d1!='?' && d2=='?') ) { bad=TRUE; break; }
		if (d1=='?' && d2=='?')
		{
			*pat++ = (char)0xff;
		}
		else
		{
			int i1, i2;
			
			i1=hexdig(d1);
			i2=hexdig(d2);

			v1 = i1;
			v2 = i2;

			if ((i1<0) || (i2<0)) { bad=TRUE; break; }
			
			*pat++ = v1<<4 | v2;
		}
	}
	
	if (bad==TRUE)
	{
		errstatus("bad hex data");
#ifdef unix
		signal(SIGALRM,clear_errstatus);
		alarm(3);
#endif
		*patsav=0;
	}
	else *pat=0;
	
}
static void getascpat(char *pat)
{
	int done,i,ch;
	char *strchr(const char *, int);

/*                        1         2         3         4         5         6         7     */
/*              01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
	VL_vmove(D_LINES+2,0); VL_verase(TO_EOL);

	VL_vprint("Enter string [^F=wildcard]->");
	for(done=i=0;!done;)
	{
		char tmp[2];
		
		ch=VL_vgetc();
		switch(ch)
		{
		      case 0x0a: case 0x0d:
			++done;
			break;
		      case 0x06:
			pat[i++]=(char)0xff;
			VL_vmode(VMODE_REVERSE);
			VL_vprint("*");
			VL_vmode(VMODE_CLEAR);
			break;
		      case 0x7f: case 0x08:
			if (i)
			{
				--i;
				VL_vmove(D_LINES+2,28+i);
				VL_vprint("  ");
				VL_vmove(D_LINES+2,28+i);
			}				      
			break;
		      default:
			if (ch>=' '&&ch<0x7f)
			{
				tmp[0]=pat[i++]=ch;
				tmp[1]=(char)0;
				VL_vprint(tmp);
			}
			break;
		}
	}
	pat[i]=(char)0;
	VL_vmove(D_LINES+2,0); VL_verase(TO_EOL);
	VL_vmode(VMODE_CLEAR);
	VL_vmove(edl,edc);
}
static int toggle_verify(void)
{
	verify=1-verify;
	ver_status();
	return 0;
}
static int toggle_8bit(void)
{
	display_8bit = 1-display_8bit;
	dopage();
	
	return 0;
}

static int  new_file(void)
{
	char save[100];
	
	strcpy(save,curfile);

	VL_vmode(VMODE_CLEAR);
	VL_vmove(D_LINES+2,0);
	VL_verase(TO_EOL);

	VL_vprint("new file->");
	VL_vgets0(curfile,99);
	
	VL_vmove(D_LINES+2,0);
	VL_verase(TO_EOL);
	show_cmds();
	fclose(fil);
	if ((fil=fopen(curfile,"rb+"))==NULL)
	{
		errstatus("can't open file");
#ifdef unix
		signal(SIGALRM,clear_errstatus);
		alarm(3);
#endif
		strcpy(curfile,save);
		fil=fopen(curfile,"rb+");
		return 0;
	}
	filesize=getsize(curfile);
	edl=0;edc=10;
	scrx=0; scry=0; nib=N_HIGH;
	filepos=0;
	read_buf();
	title();
	dopage();
	hexed_status();
	return 0;
}	
static int  mv_left(void)
{
	if (hexasc==0)
	{
		if (nib==N_LOW) { nib=N_HIGH; return 0; }
		if (scrx==0) { scrx = 15; nib=N_LOW; mv_up(); return 0; }
		--scrx; nib=N_LOW;
	}
	else
	{
		if (scrx==0) { scrx=15; mv_up(); }
		else --scrx;
	}
	return 0;
}
static int  mv_right(void)
{
	if (hexasc==0)
	{
		if (nib==N_HIGH) { nib=N_LOW; return 0; }
		if (scrx==15) { scrx = 0; nib=N_HIGH; mv_down(); return 0; }
		++scrx; nib=N_HIGH;
	}
	else
	{
		if (scrx==15) { scrx=0; mv_down(); }
		else ++scrx;
	}
	return 0;
}
static int mv_down(void)
{
	scry = scry==(D_LINES-1)?0:scry+1;
	return 0;
}
static int mv_up(void)
{
	scry = scry==0?D_LINES-1:scry-1;
	return 0;
}
static int quit_hexed(void)
{
/*
	int ch;

	ch=getyn("      quit? (y/n):  "); 
	if (ch=='y' || ch=='Y') 
*/
	{
		++quitflag;
	}
	
	return 0;
}

static int getyn(char *str)
{
	int ch;
	VL_vmove(23,60);
	VL_vmode(VMODE_BOLD);
	VL_vprint(str);
	VL_vmove(23,79);
	ch=VL_vgetc();
	VL_vmove(23,60);
	VL_vprint("                    ");
	VL_vmode(VMODE_CLEAR);
	VL_vmove(edl,edc);
	return ch;
}
static int goto_ofs(void)
{
	char kbuf[20],tmp[20];
	unsigned int xtoi(char *tmp);
	int relofs;
	
	VL_vmove(23,56);
	VL_vmode(VMODE_BOLD);
	VL_vprint("position: ");
	VL_vmove(23,66);
	VL_vgets0(kbuf,12);
	if (strlen(kbuf)==0) goto skip;
	
	if (kbuf[0]=='+' || kbuf[0]=='-')
	{
		relofs = TRUE;
		strcpy(tmp,kbuf+1);
	}
	else
	{
		relofs=FALSE;
		strcpy(tmp,kbuf);
	}
	if(!relofs)
	{
		if (tmp[0]=='0'&&(tmp[1]=='X'||tmp[1]=='x'))
		  filepos = xtoi(tmp+2);
		else
		  filepos = atoi(tmp);
	}
	else
	{
		if (tmp[0]=='0'&&(tmp[1]=='X'||tmp[1]=='x'))
		  filepos += xtoi(tmp+2)*(kbuf[0]=='+'?1: -1);
		else
		  filepos += atoi(tmp)*(kbuf[0]=='+'?1: -1);
	}
      skip:
	
	VL_vmove(23,56);
	VL_vprint("                    ");
	VL_vmode(VMODE_CLEAR);
	hexed_status();
	VL_vmove(edl,edc);
	return 0;
}
static unsigned int xtoi(char *tmp)
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
static int hexdig(char pch)
{
	int ch;
	
	ch=toupper(pch);
	if(ch>='0'&&ch<='9') { ch-='0'; return ch; }
	if(ch>='A'&&ch<='F') { ch=ch-'A'+0x0a; return ch; }

	return -1;
}
static int  errstatus(char *str)
{
	VL_vmove(23,60);
	VL_vmode(VMODE_BOLD);
	VL_vprint(str);
	VL_vmode(VMODE_CLEAR);
	VL_vmove(edl,edc);
	return 0;
}
static void clear_errstatus(void)
{
	int sc,sl;
	sc=VL_vcur_col;
	sl=VL_vcur_lin;
	
	VL_vmove(23,60);
	VL_vmode(VMODE_CLEAR);
	VL_vprint("                   ");
	VL_vmove(sl,sc);
	VL_vdefer_restore();
	
}

static int bldline(int addr, unsigned char *data, unsigned char *buf)
{
	int i;
	
	memsetnull((char *)buf,' ',199);
	sprintf(tmpbuf,"%08X:",addr);
	memcpy(buf,tmpbuf,9);
	sprintf(tmpbuf," %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X  ",
		*(data+0),*(data+1),*(data+2),*(data+3),*(data+4),*(data+5),*(data+6),*(data+7),*(data+8),
		*(data+9),*(data+10),*(data+11),*(data+12),*(data+13),*(data+14),*(data+15));
	memcpy(buf+9,tmpbuf,50);
	memset(tmpbuf,(char)0,sizeof(tmpbuf));
	for(i=0;i<16;++i)
	{
		char chbuf[3];
		
		if ( data[i]=='%' ) 
		{
			strcpy(chbuf,"%%");
		}
		else
		{
			if ( data[i] >= 0x20 && data[i] < 0x7f ) 
			{
				chbuf[0]=data[i];
			}
			else if (display_8bit && data[i] >= 0x80 )
			{
				chbuf[0]=data[i];
			}
			else
			{
				chbuf[0]='.';
			}

			chbuf[1]=(char)0;
		}
		strcat(tmpbuf,chbuf);
	}
	
	memcpy(buf+59,tmpbuf,strlen(tmpbuf));
	return 0;
}
static int  memsetnull(char *buf, int ch, int cnt)
{
	memset(buf,ch,cnt);
	*(buf+cnt)=(char)0;
	return 0;
}
static void title(void)
{
	char out[200];
	
/*	sprintf(out,"--%s-HexEd: %s %s",bufmod?"**":"--",curfile,reptstr(68-strlen(curfile),'-'));*/
	sprintf(out,"--%s-HexEd V%s: %s  size: %ld %s",
		bufmod?"**":"--",
		VERSION,curfile,(long)filesize,reptstr(80,'-'));
	out[80]=0;
	
	VL_vmove(D_LINES,0);
	VL_vmode(VMODE_REVERSE);
	VL_vprint(out);
	VL_vmode(VMODE_CLEAR);
	                                               /*  ddd 0xXX oooo */
                                                       /*  ddddd 0xXXXX 0oooooo */
                                                       /*  dddddddddd 0xXXXXXXXX 0ooooooooooo */
/*                        00000000 0x00000000  */
	VL_vmove(D_LINES+1,0);
	VL_vprint("Position:");
	VL_vmove(D_LINES+1,31);
	VL_vprint("Value:");
	VL_vmove(D_LINES+1,63);
                           /* xxx */
	VL_vprint("Verify write:");
	VL_vmode(CLEAR);
	
/*                        1         2         3         4         5         6         7         */
/*              01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
	show_cmds();
	ver_status();
	hexed_status();
}
static void show_cmds(void)
{
	int x,y,newx,highx,normx;
	char high[32], norm[32];
	
	VL_vmode(CLEAR);
	
	for (y=D_LINES+2,x=0,cmdptr=cmds; cmdptr->ch ;)
	{
		if (cmdptr->desc[0])
		{
			decode_desc(cmdptr->desc,x,&newx,&highx,&normx,high,norm);
			VL_vmode(BOLD); VL_vmove(y,highx);
			VL_vprint(high);
			VL_vmode(CLEAR); VL_vmove(y,normx);
			VL_vprint(norm);
			++cmdptr;
			if (cmdptr->ch && 78-newx > (int)(strlen(high)+strlen(norm)))
			  x=newx;
			else
			{
				++y;
				x=0;
			}
		}
		else ++cmdptr;
		
	}
	
}
static void decode_desc(char *str, int pos, int *newpos, int *highpos, int *normpos, char *highstr, char *normstr)
{
	char *p,*strchr(const char *, int);
	char tmp[64];
	
	strcpy(tmp,str);
	p=strchr(tmp,'\\');
	if (p) *p++ =(char)0;
	strcpy(highstr,tmp);
	strcpy(normstr,p);
	*highpos = pos;
	*normpos = pos+strlen(highstr);
	*newpos = pos+strlen(highstr)+strlen(normstr)+2;
}	
static void ver_status(void)
{
	VL_vmode(CLEAR);
	VL_vmode(BOLD);
	VL_vmove(D_LINES+1,77);
	if (verify) VL_vprint("on ");
	else VL_vprint("off");
	VL_vmode(CLEAR);
	VL_vmove(edl,edc);
}
static int hexed_status(void)
{
	short tmpshort;
	int tmpint;
	
	VL_vmode(VMODE_CLEAR); 
	VL_vmode(VMODE_REVERSE);
	VL_vmove(D_LINES,2);
	if (bufmod) VL_vprint("**");
	else VL_vprint("--");
	VL_vmove(D_LINES+1,10);
	VL_vmode(CLEAR);
	VL_vmode(BOLD);
	sprintf(tmpbuf,"%8.8ld 0x%08lX",filepos+scry*16+scrx,filepos+scry*16+scrx);
	VL_vprint(tmpbuf);
	VL_vmove(D_LINES+1,38);
	memcpy((char*)&tmpshort,&filebuf[scry*16+scrx],2);
	memcpy((char*)&tmpint,&filebuf[scry*16+scrx],4);
	sprintf(tmpbuf,"% 4d % 6d % 11d ",filebuf[scry*16+scrx],tmpshort,tmpint);
	VL_vprint(tmpbuf);
	VL_vmode(CLEAR);
	VL_vmove(edl,edc);
	return 0;
}
static char *reptstr(int cnt, int ch)
{
	static char repbuf[1024];
	
	memsetnull(repbuf,ch,cnt);
	return repbuf;
}

static int getsize(char *p)
{
	struct stat statbuf;
	
	if (stat(p,&statbuf)<0) return 0;
	else return statbuf.st_size;
}

/**
 ** Program: hexed
 **  Module: $RCSfile: hexed.c,v $
 ** 
 ** $Log: hexed.c,v $
 ** Revision 1.32  2003/06/20 15:37:44  gsl
 ** VL_ globals
 **
 ** Revision 1.31  2003/02/04 21:42:48  gsl
 ** fix -Wall warnings
 **
 ** Revision 1.30  2003/02/04 21:32:36  gsl
 ** fix -Wall warnings
 **
 ** Revision 1.29  2003/02/04 21:23:44  gsl
 ** fix -Wall warnings
 **
 ** Revision 1.28  2003/02/04 19:19:09  gsl
 ** fix header
 **
 ** Revision 1.27  2003/02/04 18:50:26  gsl
 ** fix copyright header
 **
 ** Revision 1.26  2002/08/08 13:48:40  gsl
 ** set the title
 **
 ** Revision 1.25  2002/07/18 21:04:23  gsl
 ** Remove MSDOS code
 **
 ** Revision 1.24  2002/07/16 14:11:44  gsl
 ** VL_ globals
 **
 ** Revision 1.23  2002/07/15 20:56:41  gsl
 ** Videolib VL_ gobals
 **
 ** Revision 1.22  2002/07/15 20:16:16  gsl
 ** Videolib VL_ gobals
 **
 ** Revision 1.21  2002/07/15 17:52:56  gsl
 ** Videolib VL_ gobals
 **
 ** Revision 1.20  2002/06/26 17:27:35  gsl
 ** fix warnings for unsigned chars on AIX
 **
 ** Revision 1.19  2001/10/16 15:25:55  gsl
 ** Fixed the Value: display. It was reversing the bytes?
 **
 ** Revision 1.18  1999-09-13 15:47:24-04  gsl
 ** fix modbuffer() return type
 **
 ** Revision 1.17  1999-01-19 11:02:40-05  gsl
 ** fix warnings
 **
 ** Revision 1.16  1999-01-19 11:00:26-05  gsl
 ** fix warning
 **
 ** Revision 1.15  1998-10-15 10:06:48-04  gsl
 ** fix type warnings
 **
 ** Revision 1.14  1998-04-22 15:54:16-04  gsl
 ** fix uninit var warning
 **
 ** Revision 1.13  1997-12-05 09:44:25-05  gsl
 ** Add support for displaying 8bit characters
 **
 ** Revision 1.12  1997-07-12 18:57:10-04  gsl
 ** Removed obsolete codde
 ** change to use new video.h interface
 ** fixed all compiler warnings
 ** Changed so you can view a read-only file.
 ** Changed so doesn't verify Quit command
 **
 ** Revision 1.11  1997-06-10 15:43:44-04  scass
 ** Changed long to int4
 **
 ** Revision 1.10  1996-09-10 12:13:34-04  gsl
 ** Fix some compiler warnings
 **
 ** Revision 1.9  1996-07-19 17:04:42-07  gsl
 ** Fix for NT
 **
 ** Revision 1.8  1995-04-25 02:58:20-07  gsl
 ** drcs state V3_3_15
 **
 * Revision 1.7  1995/04/17  11:50:45  gsl
 * drcs state V3_3_14
 *
 * Revision 1.6  1995/02/06  15:19:36  gsl
 * fixed copyrights
 * ,
 *
 * Revision 1.5  1995/01/27  23:30:37  gsl
 * drcs load
 *
 * Revision 1.6  1994/03/15  20:05:06  jockc
 * synch hexed.c with SCS version
 *
 * Revision 1.4  1991/08/26  20:22:10  jockc
 * did newfile, reread and write keys, find hex, f9 and f10 movement
 * check bounds
 *
 * Revision 1.3  1991/04/29  18:27:14  jockc
 * fixed new find behavior
 *
 * Revision 1.2  1991/04/29  18:09:03  jockc
 * changing behavior of search:
 *  searches from the current byte plus one
 *  search string positioned at 0,0 on screen
 *
 * Revision 1.1  1991/04/18  23:15:26  jockc
 * Initial revision
 *
 **
 **/
