static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/* VSLOAD for unix ..  probably will require slight modification on machines other than 3b2					*/
#ifdef unix
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#ifdef u3b2
#include <sys/st00_ioctl.h>
#endif

#include <signal.h>
#include <ctype.h>

#include "vsload.h"

int tape;
#define ZEDTMP memset(tmp,0,sizeof(tmp))
#define ZEDBUF(x) memset((x),0,sizeof(x))

main(c,v)
int c;
char **v;
{
	FILE 	*out;								/* current output file				*/
	char 	*recbuf,							/* record buffer 				*/
		*buf,								/* block buffer					*/
		*calloc();							/* std memory getter				*/
	int 	cnt,								/* bytes read by fread()			*/
		rec,								/* record size					*/
		blk,								/* block size					*/
		recs_per, 							/* records per block				*/
		recbytes;							/* bytes in current record			*/
	int 	bytes,								/* file byte count				*/
		readvol;						 	/* expect volume header block 			*/
	char 	*p,								/* scratch pointer				*/
		*strchr();							/* char index routine				*/
	char 	name[20];							/* name name					*/
	char 	tmp[100];							/* temporary buffer				*/
	char 	ext[4];								/* output files extension			*/
	char 	outpath[100];							/* output path					*/
	char 	outroot[100];							/* output root					*/
	char	tapedev[100];							/* filename of tape device			*/
	int 	trim;								/* trim files?					*/
	int 	rew();								/* function to rewind tape & exit		*/

	ZEDBUF(name); ZEDBUF(ext); ZEDBUF(outpath); ZEDBUF(tapedev);
	signal(SIGINT,rew);							/* rewind on interrupt				*/
	printf("output path [.]: ");
	gets(outroot);
	if (!strlen(outroot)) strcpy(outroot,".");				/* no path entered				*/
	printf("output file extension [wcb]: ");
	gets(tmp);
	strncpy(ext,tmp,3);
	if (!strlen(ext)) strcpy(ext,"wcb");					/* no extension entered				*/
	printf("tape device [/dev/rmt/c1t7d0hn]: ");
	gets(tapedev);
	if (strlen(tapedev)==1) strcpy(tapedev,"/dev/rmt/c1t7d0hn");		/* no device path entered			*/
	printf("trim each file? [y]: ");
	gets(tmp);
	if (tmp[0]=='n'||tmp[0]=='N') trim=0;
	else trim=1;	

	for (readvol=0;;)
	{
		if ((tape=open(tapedev,O_RDONLY))<0)
		{
			perror("error on open");
			exit(2);
		}
		if (!readvol)		
		{
			ZEDTMP; 
			cnt = read(tape,&svol,sizeof(svol)); 			/* get volume header				*/
			if (cnt != sizeof(svol))				/* bad size					*/
			{
				fprintf(stderr,"error reading volume header");
				rew(tapedev);
			}
			++readvol;						/* got vol, set flag				*/
			strncpy(tmp,svol.label,sizeof(svol.label));		/* extract the interesting parts		*/
			printf("Volume name=%-6s\t",tmp);			/* and display them				*/
			strncpy(tmp,svol.owner,sizeof(svol.owner));
			printf("owner=%-14s\n",tmp);
		}
		cnt=read(tape,&shead1,sizeof(shead1));				/* get file header1				*/
		if (cnt==0) 
		{
			printf("end of data...");
			rew(tapedev);
		}
		if (cnt != sizeof(shead1))
		{
			fprintf(stderr,"error reading file header #1");
			rew(tapedev);
		}
		cnt=read(tape,&shead2,sizeof(shead2));				/* get file header2				*/
		if (cnt != sizeof(shead2))
		{
			fprintf(stderr,"error reading file header #2");
			rew(tapedev);
		}
		ZEDTMP;
		strncpy(tmp,shead1.name,sizeof(shead1.name));			/* get the filename part			*/
		printf("file %-17s: ",tmp);
		strcpy(name,tmp);
		lower(name);
		ZEDTMP;
		strncpy(tmp,shead2.blocklen,sizeof(shead2.blocklen));		/* get blocksize				*/
		blk=atoi(tmp);
		strncpy(tmp,shead2.reclen,sizeof(shead2.reclen));		/* and recsize					*/
		rec=atoi(tmp);
		recs_per = blk/rec;						/* compute approx records per block		*/
		printf("reclen=%05d  ",rec);					/* show reclen 					*/
		close(tape);							/* done with first part				*/
		if ((tape=open(tapedev,O_RDONLY))<0)				/* open to read next file			*/
		{
			perror("error on open");
			exit(2);
		}
		buf=calloc(blk+1,sizeof(char));					/* grab a buffer				*/
		recbuf=calloc(rec+1,sizeof(char));				/* and another 					*/
		p=strchr(name,' ');						/* truncate any spaces in the filename		*/
		if (p) *p=(char)0;						/* by replacing with null			*/
		p=strchr(name,'.');						/* period separates lib and filename		*/
		*p++ = (char)0;							/* put in a null to make a double string	*/
		sprintf(outpath,"%s/%s",outroot,name);				/* build dir path to check dir existance	*/
		if (access(outpath,0)) mkdir(outpath,0777);			/* if dir not exist make it			*/
		sprintf(outpath,"%s/%s/%s.%s",outroot,name,p,ext);		/* now build the path				*/
		if ((out=fopen(outpath,"w"))==NULL)				/* open it					*/
		{
			perror(outpath);
			rew(tapedev);
		}
		bytes=0;
		while (cnt=read(tape,buf,blk))					/* start reading blocks from the tape		*/
		{
			int i;

			if (cnt == -1) 						/* error					*/
			{
				fclose(out);
				perror("error on read");
				break;	
			}
			bytes += cnt;						/* inc our byte count for this file		*/
			for (i=0; i<= recs_per; ++i)				/* write out the records			*/
			{
				if (recbytes==0)				/* last record was complete			*/
				{
					ZEDBUF(recbuf);	
					if (cnt > rec)				/* more than 1 record left in block		*/
					{
						strncpy(recbuf,buf+i*rec,rec);	/* copy the record into the rec buf		*/
						fprintf(out,"%s\n",recbuf);	/* now spew it out				*/
						cnt -= rec;			/* cnt is bytes left in block			*/
					}
					else 					/* not enough bytes in block to make a complete	*/
					{					/* record					*/
						strncpy(recbuf,buf+i*rec,cnt);	/* copy in partial record			*/
						recbytes=cnt;			/* remember how many bytes we have		*/
						cnt=0;				/* 0 bytes left in block			*/
					}
				}
				else						/* last record only partially filled		*/
				{
					strncpy(recbuf+recbytes,buf+i*rec,	/* copy enough bytes from block buffer to	*/
						80-recbytes);			/* fill the record buf				*/
					cnt -= (80-recbytes);			/* decrement cnt by that much			*/
					fprintf(out,"%s\n",recbuf);		/* spew it					*/
					recbytes=0;				/* no unwritten bytes in recbuf			*/
				}
			}
		}	
		printf("bytes read=%d\n",bytes);				/* info message for operator			*/
		free(buf);							/* tidy up					*/
		free(recbuf);
		fclose(out);
		if (trim)
		{
			char cmd[100];

			sprintf(cmd,"trim %s 72",outpath);
			system(cmd);
		}
		close(tape);							/* done with the current file			*/
		if ((tape=open(tapedev,O_RDONLY))<0)				/* open to read eof trailers			*/
		{
			perror("error on open");
			exit(2);
		}
		cnt=read(tape,&shead1,sizeof(shead1));				/* get first eof				*/
		if (cnt != sizeof(shead1))
		{
			fprintf(stderr,"error reading file header #1");
			rew(tapedev);
		}
		cnt=read(tape,&shead2,sizeof(shead2));				/* get second eof				*/ 
		if (cnt != sizeof(shead2)) 
		{
			fprintf(stderr,"error reading file header #2");
			rew(tapedev);
		}
		close(tape);
	}
}
rew(tapedev)
char *tapedev;
{
	printf("rewinding...\n");
	close(tape);
	tape=open(tapedev,O_RDONLY);
	close(tape);
	exit(2);
}
lower(p)
char *p;
{
	while (*p)
	{
		*p = tolower(*p);
		++p;
	}
}
#endif
/*
**	History:
**	$Log: vsload.c,v $
**	Revision 1.7  1996/07/23 18:13:02  gsl
**	drcs update
**	
**
**
*/
