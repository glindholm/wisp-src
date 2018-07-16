/**
 ** Program: hexed
 **  Module: $RCSfile: hexed.c,v $
 ** 
 ** $Log: hexed.c,v $
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

#include <stdio.h>
#include <v/vlocal.h>
#include <v/video.h>
#include <v/vcap.h>
#include <v/vdata.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";
#define RCSVERS "$Revision:$"
#define RCSDATE "$Date:$"

static char VERSION[5];
static char MODDATE[20];

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

struct cmd 
{
	int ch;
	int (*fn)();
	char *desc;
};

int	find(), 
	toggle_verify(),
	new_file(),
	quit_hexed(),
	goto_ofs(),
	toggle_hex_asc(), 
	help_screen(),
	next_screen(),
	prev_screen(),
	start_line(),
	end_line(),
	mv_left(),
	mv_right(),
	mv_up(),
	mv_down(),
        print_screen(),
  write_buf(), read_buf();
 
#define CTRL(x) ((x)&0x1f)
#if 0
struct cmd cmds[]=
{
	{ CTRL('a'), start_line, "" },
	{ CTRL('e'), end_line, "" },	      
	{ CTRL('h'), mv_left, "" },
	{ CTRL('j'), mv_down, "" },
	{ CTRL('k'), mv_up, "" },
	{ CTRL('l'), mv_right, "" },
	{ CTRL('f'), find, "^F\\ind" },
	{ CTRL('v'), toggle_verify, "^V\\erify" },
	{ CTRL('n'), new_file, "^N\\ewFile" },
	{ CTRL('g'), goto_ofs, "^G\\oto" },
	{ CTRL('i'), toggle_hex_asc, "" },
	{ CTRL('h'), help_screen, "" },
	{ '=', next_screen, "" },
	{ '-', prev_screen, "" },
	{ CTRL('d'), print_screen, "^D\\umpScreen" },
	{ CTRL('x'), quit_hexed, "^X\\ to quit" },
	{ 0,0,0 }
};
#endif
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
	{ CTRL('i'), toggle_hex_asc, "TAB\\-Hex/Ascii" },
	{ GENERIC_TAB+VMBIAS, toggle_hex_asc, "" },
/*	{ CTRL('h'), help_screen, "^H\\elp" },*/
	{ GENERIC_PF9+VMBIAS, prev_screen, "F9\\-Prev" },
	{ GENERIC_PF10+VMBIAS, next_screen, "F10\\-Next" },
	{ CTRL('n'), next_screen, "" },
	{ CTRL('p'), prev_screen, "" },
/*	{ GENERIC_PF6+VMBIAS, print_screen, "F6-\\DumpScreen" },*/
	{ GENERIC_PF16+VMBIAS, quit_hexed, "F16\\-Quit" },
	{ 0,0,0 }
};
struct cmd *cmdptr;

#define D_BYTE 0
#define D_WORD 1
#define D_LONG 2
#define N_HIGH 0
#define N_LOW 1

unsigned long filepos=0;
long filesize;

long ofilepos= -1;
int scrx,scry,nib;
int quitflag=0;
int verify=1;
int bufmod=0;
int hexasc=0;
int dispmode=D_BYTE;

int edl,edc;

#define FOREVER while(1)
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

main(c,v)
char **v;
{
	char *p,*e,*strchr();
	
	memset(VERSION,0,sizeof(VERSION));
	p=strchr(rcsid,' ');
	p=strchr(++p,' ');
	e=strchr(++p,' ');
	
	memcpy(VERSION,p,e-p);
	memset(MODDATE,0,sizeof(MODDATE));
	p=strchr(++e,' ');
	memcpy(MODDATE,e,p-e);

	if (c==1) exit(0);
	
	strcpy(curfile,v[1]);
	fil=fopen(v[1],"r+");
	if (!fil) exit(0);
	filesize=getsize(curfile);
	
	vstate(0);
	verase(FULL_SCREEN);
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
		vmove(edl,edc);
	}
	vexit(0);
	fclose(fil);
}
write_buf()
{
	int inkey;
	
	if (verify)	
	{
		inkey=getyn("write buffer? (y/n)");
		if (inkey=='y'||inkey=='Y')
		{
			fseek(fil,ofilepos,ABS_OFS);
			fwrite(filebuf,sizeof(filebuf),1,fil);
		}
	}
	else
	{
		fseek(fil,ofilepos,ABS_OFS);
		fwrite(filebuf,sizeof(filebuf),1,fil);
	}
	bufmod=FALSE;
}
read_buf()
{
	memset(filebuf,0,sizeof(filebuf));
	fseek(fil,filepos,ABS_OFS);
	fread(filebuf,sizeof(filebuf),1,fil);
	bufmod=FALSE;
	ofilepos=filepos;
}
dopage()
{
	register int i;
	
	for (i=0; i<D_LINES; ++i)
	{
		bldline(filepos+(i*D_COLS),filebuf+i*D_COLS,linebuf);
		vmove(i,0);
		vprint(linebuf);
	}
}
doedit()
{
	int key,data;

	vmove(edl,edc);
	key=vgetm();

	data=1;
	for (cmdptr=cmds; cmdptr->ch; ++cmdptr)
	{
		if (tolower(key)==cmdptr->ch) 
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
next_screen()
{
	if (filepos + D_LINES*D_COLS >filesize) 
	{
		vbell();
		return;
	}
	filepos+= D_LINES*D_COLS;
}
prev_screen()
{
	filepos-=  D_LINES*D_COLS;
	if (filepos&0x80000000) filepos=0;
}
print_screen()
{
	char fname[100];
	static int seq=0;
	int x,i,skip=0;
	FILE *out;

	vmove(D_LINES+2,0);
/*	vprint("File->");
	vgets0(fname,64);
*/	sprintf(fname,"dump%-d",seq++);
	vprint("dumped to file->%s",fname);
#if 0	
	if (fname[strlen(fname)-1]=='+') ++skip;
#endif
	out=fopen(fname,"w");
	for (x=0;x<24;++x)
	{
		for (i=0;i<80;++i) if (!vchr_map[x][i]) vchr_map[x][i]=' ';
		fwrite(vchr_map[x],80,1,out);
		fprintf(out,"\n");
		if (skip)
		  fprintf(out,"\n");
	}
	fclose(out);
}
toggle_hex_asc()
{
	hexasc=1-hexasc;
}	      
start_line()
{
	scrx=0;
}
end_line()
{
	scrx=15;
}
modbuffer(data)
int data;
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
find()
{
	int ch;
	static char pat[64];
	static int havepat=FALSE;
	
	vmode(CLEAR);
	vmove(D_LINES+2,0);
	verase(TO_EOL);
loop:	vprint("Find (F3=repeat, H=Hex, A=ASCII, X=abort) ->");
	ch=vgetm();	
	vmove(D_LINES+2,0);
	verase(TO_EOL);
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
		  return;
		case GENERIC_PF3+VMBIAS:
		  break;
		default:
		  vbell();
		  goto loop;
		  return;
	}
	show_cmds();
	if (havepat) do_find(pat);
}
do_find(pattern)
char *pattern;
{
	int searching=1,spos;
	int clear_errstatus();
	int sfilepos,sspos,sscry,sscrx;
	int sedl,sedc;
	
	sfilepos=filepos;
	sspos=spos;
	sscry=scry;
	sscrx=scrx;
	sedl=edl;
	sedc=edc;
	
 	spos=scry*16+scrx + 1;
	while (searching)
	{
		while (spos < D_COLS*D_LINES)
		{
			if (match(&filebuf[spos],pattern)) 
			{
/*				filepos += spos;*/
				dopage();
				scry = spos / 16;
				scrx = spos % 16;
				return;
			}
			else ++spos;
		}
		if (filepos+D_COLS*D_LINES>filesize)
		{
			vbell();
			filepos=sfilepos;
			spos=sspos;
			sscrx=scrx;
			sscry=scry;
			edl=sedl;
			edc=sedc;
			vmove(edl,edc);
			read_buf();
			dopage();
			errstatus("pattern not found");
			signal(SIGALRM,clear_errstatus);
			alarm(3);
			
			return;
		}
		filepos+= D_COLS*D_LINES;
		read_buf();
		spos=0;
	}
}
match(buf,pat)
unsigned char *buf,*pat;
{
	for (;*pat;++pat,++buf)
	{
		if (*pat == 0xff) continue;
		if (*pat != *buf) return 0;
	}
	return 1;
}
gethexpat(pat)
char *pat;
{
	char tmp[64],*p=tmp, *patsav=pat;
	int _nib=N_HIGH,i;
	char d1,d2,v1,v2;
	int bad=FALSE;
	
int clear_errstatus();
	
	vmove(D_LINES+2,0); verase(TO_EOL);
	vprint("Enter Hex pattern [??=wildcard]->");
/*                        1         2         3         4         5         6         7     */
/*              01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
	vmove(D_LINES+2,33);
	vgets0(tmp,63);
	vmove(D_LINES+2,0); verase(TO_EOL);
	vmode(CLEAR);
	vmove(edl,edc);
	while (*p)
	{
		d1 = *p;
		if (*(p+1)) d2 = *(p+1);
		else { bad=TRUE; break; }
		p += 2;
		if (d1=='?' && d2!='?' ||
		    d1!='?' && d2=='?' ) { bad=TRUE; break; }
		if (d1=='?' && d2=='?')
		{
			*pat++ = 0xff;
		}
		else
		{
			v1=hexdig(d1);
			v2=hexdig(d2);
			if (v1<0 || v2<0) { bad=TRUE; break; }
			*pat++ = v1<<4 | v2;
		}
	}
	
	if (bad==TRUE)
	{
		errstatus("bad hex data");
		signal(SIGALRM,clear_errstatus);
		alarm(3);
		*patsav=0;
	}
	else *pat=0;
	
}
getascpat(pat)
char *pat;
{
	int case_sens,done,i,ch;
	char *strchr();
	char string[64];

#if 0	
	vmove(D_LINES+2,0);
	vprint("Case sensitive search? (y/n):");
	
	case_sens=vgetc();
	if (!strchr("yYnN",case_sens)) 
	{
		vmove(D_LINES+2,0);
		verase(TO_EOL);
		vmode(CLEAR);
		vmove(edl,edc);
		return;
	}
#endif
/*                        1         2         3         4         5         6         7     */
/*              01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
	vmove(D_LINES+2,0); verase(TO_EOL);

	vprint("Enter string [^F=wildcard]->");
	for(done=i=0;!done;)
	{
		char tmp[2];
		
		ch=vgetc();
		switch(ch)
		{
		      case 0x0a: case 0x0d:
			++done;
			break;
		      case 0x06:
			pat[i++]=0xff;
			vmode(REVERSE);
			vprint("*");
			vmode(CLEAR);
			break;
		      case 0x7f: case 0x08:
			if (i)
			{
				--i;
				vmove(D_LINES+2,28+i);
				vprint("  ");
				vmove(D_LINES+2,28+i);
			}				      
			break;
		      default:
			if (ch>=' '&&ch<0x7f)
			{
				tmp[0]=pat[i++]=ch;
				tmp[1]=(char)0;
				vprint(tmp);
			}
			break;
		}
	}
	pat[i]=(char)0;
	vmove(D_LINES+2,0); verase(TO_EOL);
	vmode(CLEAR);
	vmove(edl,edc);
}
toggle_verify()
{
	verify=1-verify;
	ver_status();
}
new_file()
{
	char save[100];
	int clear_errstatus();
	
	strcpy(save,curfile);

	vmode(CLEAR);
	vmove(D_LINES+2,0);
	verase(TO_EOL);

	vprint("new file->");
	vgets0(curfile,99);
	
	vmove(D_LINES+2,0);
	verase(TO_EOL);
	show_cmds();
	close(fil);
	if ((fil=fopen(curfile,"r+"))==NULL)
	{
		errstatus("can't open file");
		signal(SIGALRM,clear_errstatus);
		alarm(3);
		strcpy(curfile,save);
		fil=fopen(curfile,"r+");
		return;
	}
	filesize=getsize(curfile);
	edl=0;edc=10;
	scrx=0; scry=0; nib=N_HIGH;
	filepos=0;
	read_buf();
	title();
	dopage();
	hexed_status();
}	
mv_left()
{
	if (hexasc==0)
	{
		if (nib==N_LOW) { nib=N_HIGH; return; }
		if (scrx==0) { scrx = 15; nib=N_LOW; mv_up(); return; }
		--scrx; nib=N_LOW;
	}
	else
	{
		if (scrx==0) { scrx=15; mv_up(); }
		else --scrx;
	}
}
mv_right()
{
	if (hexasc==0)
	{
		if (nib==N_HIGH) { nib=N_LOW; return; }
		if (scrx==15) { scrx = 0; nib=N_HIGH; mv_down(); return; }
		++scrx; nib=N_HIGH;
	}
	else
	{
		if (scrx==15) { scrx=0; mv_down(); }
		else ++scrx;
	}
}
mv_down()
{
	scry = scry==(D_LINES-1)?0:scry+1;
}
mv_up()
{
	scry = scry==0?D_LINES-1:scry-1;
}
quit_hexed()
{
	int ch;

	ch=getyn("      quit? (y/n):  ");
	if (ch=='y' || ch=='Y') ++quitflag;
}

getyn(str)
char *str;
{
	int ch;
	vmove(23,60);
	vmode(BOLD);
	vprint(str);
	vmove(23,79);
	ch=vgetc();
	vmove(23,60);
	vprint("                    ");
	vmode(CLEAR);
	vmove(edl,edc);
	return ch;
}
goto_ofs()
{
	char kbuf[20],tmp[20];
	unsigned int xtoi();
	int relofs;
	
	vmove(23,56);
	vmode(BOLD);
	vprint("position: ");
	vmove(23,66);
	vgets0(kbuf,12);
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
	
	vmove(23,56);
	vprint("                    ");
	vmode(CLEAR);
	hexed_status();
	vmove(edl,edc);
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

	return -1;
}
toggle_mode()
{
	noimp();
}
help_screen()
{
	noimp();
}
noimp()
{
	int clear_errstatus();
	
	errstatus("Not implemented yet");
	signal(SIGALRM,clear_errstatus);
	alarm(3);
}	
errstatus(str)
char *str;
{
	vmove(23,60);
	vmode(BOLD);
	vprint(str);
	vmode(CLEAR);
	vmove(edl,edc);
}
clear_errstatus()
{
	extern int vcur_col, vcur_lin;
	int sc,sl;
	sc=vcur_col;
	sl=vcur_lin;
	
	vmove(23,60);
	vmode(CLEAR);
	vprint("                   ");
	vmove(sl,sc);
	vdefer(RESTORE);
	
}
#define FIX(x) ((((x)>31)&&((x)<0x7f))?(x):'.')
bldline(addr,data,buf)
int addr;
unsigned char *data,*buf;
{
	int i;
	
	memsetnull(buf,' ',199);
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
		
		if ( data[i]=='%' ) strcpy(chbuf,"%%");
		else
		{
			if ( data[i] > 31 && data[i] < 0x7f ) chbuf[0]=data[i];
			else
			  chbuf[0]='.';
			chbuf[1]=(char)0;
		}
		strcat(tmpbuf,chbuf);
	}
	
	memcpy(buf+59,tmpbuf,strlen(tmpbuf));
}
memsetnull(buf,ch,cnt)
char *buf;
int ch,cnt;
{
	memset(buf,ch,cnt);
	*(buf+cnt)=(char)0;
}
title()
{
	char out[200],*reptstr();
	
/*	sprintf(out,"--%s-HexEd: %s %s",bufmod?"**":"--",curfile,reptstr(68-strlen(curfile),'-'));*/
	sprintf(out,"--%s-HexEd V%s: %s  size: %d %s",
		bufmod?"**":"--",
		VERSION,curfile,filesize,reptstr(80,'-'));
	out[80]=0;
	
	vmove(D_LINES,0);
	vmode(REVERSE);
	vprint(out);
	vmode(CLEAR);
	                                               /*  ddd 0xXX oooo */
                                                       /*  ddddd 0xXXXX 0oooooo */
                                                       /*  dddddddddd 0xXXXXXXXX 0ooooooooooo */
/*                        00000000 0x00000000  */
	vmove(D_LINES+1,0);
	vprint("Position:");
	vmove(D_LINES+1,31);
	vprint("Value:");
	vmove(D_LINES+1,63);
                           /* xxx */
	vprint("Verify write:");
	vmode(CLEAR);
	
/*                        1         2         3         4         5         6         7     
/*              01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
	show_cmds();
	ver_status();
	hexed_status();
}
show_cmds()
{
	int x,y,newx,highx,normx;
	char high[32], norm[32];
	
	vmode(CLEAR);
	
	for (y=D_LINES+2,x=0,cmdptr=cmds; cmdptr->ch ;)
	{
		if (cmdptr->desc[0])
		{
			decode_desc(cmdptr->desc,x,&newx,&highx,&normx,high,norm);
			vmode(BOLD); vmove(y,highx);
			vprint(high);
			vmode(CLEAR); vmove(y,normx);
			vprint(norm);
			++cmdptr;
			if (cmdptr->ch && 78-newx > strlen(high)+strlen(norm))
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
decode_desc(str,pos,newpos,highpos,normpos,highstr,normstr)
char *str,*highstr,*normstr;
int pos, *newpos,*highpos, *normpos;
{
	char *p,*strchr();
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
ver_status()
{
	vmode(CLEAR);
	vmode(BOLD);
	vmove(D_LINES+1,77);
	if (verify) vprint("on ");
	else vprint("off");
	vmode(CLEAR);
	vmove(edl,edc);
}
hexed_status()
{
	short tmpshort;
	int tmpint;
	
	vmode(CLEAR); vmode(REVERSE);
	vmove(D_LINES,2);
	if (bufmod) vprint("**");
	else vprint("--");
	vmove(D_LINES+1,10);
	vmode(CLEAR);
	vmode(BOLD);
	sprintf(tmpbuf,"%8.8d 0x%08X",filepos+scry*16+scrx,filepos+scry*16+scrx);
	vprint(tmpbuf);
	vmove(D_LINES+1,38);
	memrcpy(&tmpshort,&filebuf[scry*16+scrx],2);
	memrcpy(&tmpint,&filebuf[scry*16+scrx],4);
	sprintf(tmpbuf,"% 4d % 6d % 11d ",filebuf[scry*16+scrx],tmpshort,tmpint);
	vprint(tmpbuf);
	vmode(CLEAR);
	vmove(edl,edc);
}
char *reptstr(cnt,ch)
{
	static char repbuf[1024];
	
	memsetnull(repbuf,ch,cnt);
	return repbuf;
}
memrcpy(dest,src,cnt)
char *dest,*src;
int cnt;
{
	--cnt;
	while (cnt>=0)
	{
		*dest++ = *(src+cnt);
		--cnt;
	}
}

getsize(p)
char *p;
{
	struct stat statbuf;
	
	if (stat(p,&statbuf)<0) return 0;
	else return statbuf.st_size;
}
