static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
#include <fcntl.h>
#include "brando.h"

static char sccs_id[]="@(#)brando.c	1.5 1/10/93";

 BRANDO*idl;


BRANDO lazyq = {
	{"LAZYQ"},
	{"hello"}};

static int testing,verbose,checksum,errflag;

void usage(int rc);
void brando(char *fil,char *flg,char *brnd);
int do_open(char *fil,int mode);
static long look_for(int handle,char *flg);
void checksumit(char *fil);
static int next_char(int handle);
static long got_where(int handle);
void brandit(int handle,long pos,char *brnd);

main(argc,argv)
int argc;
char **argv;
{
	int ch;

	extern int optind;

	while(	(ch = getopt(argc,argv,"ctv")) != -1)
		{
		switch(ch)
			{
			case 't':
				++testing;
				break;
			case 'v':
				++verbose;
				break;
			case 'c':
				++checksum;
				break;
			default:
				++errflag;
				break;
			}
		}
	if(errflag)
		usage(1);
	
	if(testing)
		{
		printf("%s\n%s\n",lazyq.flag,lazyq.brand);
		exit(0);
		}
	if(checksum)
		{
		checksumit(argv[optind]);
		exit(0);
		}
	if((argc - optind) != 3)
		usage(1);
	brando(argv[optind],argv[optind + 1],argv[optind + 2]);
	
}

void usage(int rc)
{
	fprintf(stderr,"Usage: brando [-v] filename flag brand\n");
	fprintf(stderr," filename executable, .o or .a image.\n");
	fprintf(stderr," flag is that character string to search for.\n");
	fprintf(stderr," brand is the string to be written to the file.\n");
	fprintf(stderr," flags: -v verbose\n");
	fprintf(stderr,"        -c checksum only on filename\n");
	exit(rc);
}

static char brand[BRANDO_SIZE];
void brando(char *fil,char *flg,char *brnd)
{
	long at,look_for();
	int handle;

	if(strlen(brnd) > BRANDO_SIZE)
		memcpy(brand,brnd,BRANDO_SIZE);
	else
		strcpy(brand,brnd);
	handle = do_open(fil,O_RDWR);
	if(handle < 0)
		return;
	if(verbose)
		printf("Searching for %s in %s.\n",flg,fil);
	at = look_for(handle,flg);
	if(at < 0)
		{
		fprintf(stderr,"\nFlag %s not found in %s\n\n",
				flg,fil);
		close(handle);
		usage(1);
		}
	/*for now */
	if(verbose)
		{
		printf("Found %s at %ld\n",flg,at);
		printf("Installing brand %s\n",brand);
		}
	brandit(handle,at,brand);
	close(handle);
}

int do_open(char *fil,int mode)
{
	int handle;
	handle = open(fil,mode);
	if(handle < 0)
		{
		fprintf(stderr,
			"\nFile %s not accessible or not found.\n\n",fil);
		usage(1);
		}
	return(handle);
}

static char flag[BRANDO_SIZE];
	
static long look_for(int handle,char *flg)
{
	int ch1,ch2,count;
	long pos,got_where();

	count = 0;

	if(strlen(flg) > BRANDO_SIZE)
		memcpy(flag,flg,BRANDO_SIZE);
	else
		strcpy(flag,flg);

	while((ch1 = next_char(handle)) != EOF)
		{
		ch1 &= 0xff;
		ch2 = flag[count] & 0xff;
		if(ch1 == ch2)
			{
			++count;
			if(count == BRANDO_SIZE)
				{
				pos = got_where(handle);
				return(pos);
				}
			}
		else
			{
			count = 0;
			}
		}
	return(-1L);
}

void checksumit(char *fil)
{
	int handle,ch;
	long sum;

	handle = do_open(fil,O_RDONLY);
	if(handle < 0)
		return;
	if(verbose)
		printf("Running checksum\n");
	sum = 0;
	while((ch = next_char(handle)) != EOF)
		{
		sum += ch;
		}
	printf("Checksum = %ld\n",sum);
	close(handle);
}


#define	BUFSIZE	64
static char buf[BUFSIZE];
int bidx,got;

static int next_char(int handle)
{
	if(bidx == got)
		{
		got = read(handle,buf,BUFSIZE);
		if(got == 0)
			return(EOF);
		bidx = 0;
		}
	return(0xff & buf[bidx++]);
	
}

static long got_where(int handle)
{
	long pos;

	pos = lseek(handle,0,SEEK_CUR);
	pos -= got;
	pos += bidx;
	return(pos);
}

void brandit(int handle,long pos,char *brnd)
{
	lseek(handle,pos,SEEK_SET);
	write(handle,brnd,BRANDO_SIZE);
}

/*
**	History:
**	$Log: brando.c,v $
**	Revision 1.3  1996-09-17 19:45:26-04  gsl
**	drcs update
**
**
**
*/
