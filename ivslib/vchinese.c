/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
** CVS
**
**
**
**
******************************************************************************
*/

/*
**	File:		vchinese.c
**
**	Project:	video/ivs
**
**
**
**	Purpose:	Chinese character support
**
**	Routines:	
*/


#define IVS__CHINESE__


/*
**	Includes
*/

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "vchinese.h"
#include "vutil.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

static	int f1indsz, f2indsz;
static	int f1size, f2size;

static	int max1[3],max2[3];
static	int mult1[3], mult2[3];

static	int i,j,k;

struct xfileheader header;
static int byteorder;
static unsigned char *f1indbuf=NULL, *f2indbuf=NULL;
static unsigned char *f1tof2buf=NULL, *f2tof1buf=NULL;
static char vlangfile[MAX_FILENAME_LEN];


static struct xlat_s charsets[CS_TYPE_MAX]=
{
 { "ivs", 3 },
 { "gw",  2 },
 { "b5",  2 },
 { "hp",  2 },
 { "ccdc",2 },
 { "et",  2 },
};

/*
**	Static Function Prototypes
*/

static int setord(),getbuf(),fixbyteorder();

/*
**      Routine:        IVS_xlat_stream()
**
**      Function:       this acts as a xlating filter to the character stream
**
**      Description:    is inserted as layers in the I and O stream
**                      examines the stream and makes substitutions on the fly.
**                      it is basically an enhanced xlat_chars in that it
**                      can handle partial chinese characters--it keeps track
**                      of the char stream between calls.  If the passed context
**                      is NULL (IVS_vlanguage was never called or was called with null 
**                      pathname), IVS_xlat_stream will pass bytes thru transparently.
**                      
**      Input:          globals from chinese.h:
**                        in                input stream buf
**			  out               output stream buf
**			  ctx    	    context struct containing buffers and
**                                          char data for a particular stream.
**                                          set to null on first call
**                        dir               dir to xlat; only checked when ctx==NULL
**
**      Output:         appropriate bytes are put into out buffer
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        written 11/6/92 JEC
**
*/

void IVS_xlat_stream(unsigned char *instr, int insz, unsigned char *out, int* outsz, struct xlcontext **ctx)
{
	static int address=0;
	
	register int inpos=0, outpos=0, ch;
	static int held;

#ifdef LOGXL
	char errbuf[80];
	static int log= -1;

	if (log== -1)
	{
		log=creat("xlat.log",0666);
	}
	write(log,"IVS_xlat_stream: f1==>f2:\nsrc=/",27);
	write(log,instr,insz);
#endif
	
	
#define ibuf ((*ctx)->indbuf)
#define xbuf ((*ctx)->xlatbuf)
#define mult ((*ctx)->multvalues)
#define ssz ((*ctx)->srcsize)
#define dsz ((*ctx)->dstsize)
#define chpos ((*ctx)->multipos)
	
#define HIDX(x) ((x) % HOLDSZ)
#define GETBYTE  (((((*ctx)->holdpos)>((*ctx)->holdstart)) && !chpos)? held=1,(*ctx)->hold[HIDX(((*ctx)->holdstart++)+1)] :\
		  ( held=0,(inpos < insz) ) ? instr[inpos++] : -1 )
#define STOREBYTE(x) ++((*ctx)->holdpos); (*ctx)->hold[HIDX(((*ctx)->holdpos))] = x
#define RELEASEBYTE ++((*ctx)->holdstart)
#define TOPBYTE  (*ctx)->hold[HIDX(((*ctx)->holdstart)+1)] 

        if (insz == 0)
	{
		*outsz = insz;
		out[0]=(char)0;
		return;
	}
	if (*ctx ==NULL  )                        /* if context is not initialized, just pass */
	{					  /* bytes through */
		memcpy(out,instr,insz);
		*outsz = insz;
		out[insz]=(char)0;
		return;
	}
	for(;;)
	{
		ch = GETBYTE;                     /* get a byte.. either a held byte or from the input buffer */
		if (ch== -1)                      /* last byte? */
		  break;

		if (ibuf[ch+chpos*256] == 0xff)   /* 0xff in the indbuf means its not a valid byte */
		{				  /* for this format.. so pass through saved bytes and this one */

			if (chpos)                /* if chpos is nonzero, it means we are in the middle of what */
			{                         /* we thought was a good sequence. */
				STOREBYTE(ch);    /* store this one */
				out[outpos++] = TOPBYTE;  /* and send out the oldest saved one */
				RELEASEBYTE;      /* release the one we just sent */
				address=0;        /* clear the accumulated address */
				chpos=0;          /* and the chpos */
			}
			else
			  out[outpos++] = ch;     /* no held bytes so pass this one */
		}
		else
		{
			if (!held)                /* only hold bytes that came from input buffer */
			  STOREBYTE(ch);
			address += ibuf[ch+256*chpos] * mult[chpos];    /* begin computing address */
			if (++chpos == ssz)       /* finished with a sequence? */
			{
				((*ctx)->holdstart)=((*ctx)->holdpos);  /* yup, clear the held bytes */
				address *= dsz;   /* finish computing address */
				memcpy(&out[outpos],xbuf+address,dsz);  /* access the xlated sequence at that address */
				outpos += dsz;    /* adjust the outpos */
				if (ssz > dsz)    /* and add a space if necessary */
				{
					out[outpos++] = ' ';
				}
				chpos=address=0;
			}
			else
			  continue;
		}
	}
	out[(*outsz)=outpos]=(char)0;
#ifdef LOGXL
	write(log,"/\ndst=/",7);
	write(log,out,outpos);
	sprintf(errbuf,"/\nheld=%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x\nstart=%d, pos=%d\n",
		((*ctx)->hold)[0],
		((*ctx)->hold)[1],
		((*ctx)->hold)[2],
		((*ctx)->hold)[3],
		((*ctx)->hold)[4],
		((*ctx)->hold)[5],
		((*ctx)->hold)[6],
		((*ctx)->hold)[7],
		((*ctx)->hold)[8],
		((*ctx)->hold)[9],
		((*ctx)->holdstart),
		((*ctx)->holdpos));
	write(log,errbuf,strlen(errbuf));
#endif	
	
}
/*
**      Routine:        init_xlat_ctx()
**
**      Function:       initializes a xlation context.  must be called
**                      AFTER init_globals
**
**      Description:    alloc a context and setup it's pointers
**                      
**      Input:          ctx         the context to be created
**                      function    direction of translate (to IVS, from 
**                                  IVS) for this context
**
**      Output:         creates the context
**
**      Return:         None
**
**      Warnings:       None
**
**      History:      
**
*/
static int init_xlat_ctx(ctx,xfile,function)
struct xlcontext **ctx;
char xfile[];
int function;
{
	*ctx = (struct xlcontext *)calloc(1,sizeof(struct xlcontext));
	if (function==XL_FROM_IVS)
	{
		ibuf= f1indbuf;
		xbuf= f1tof2buf;
		mult= mult1;
		ssz=  f1size;
		dsz=  f2size;
	}
	else if (function==XL_TO_IVS)
	{
		ibuf= f2indbuf;
		xbuf= f2tof1buf;
		mult= mult2;
		ssz=  f2size;
		dsz=  f1size;
	}

	return 0;
	
}


/*
**      Routine:        init_globals()
**
**      Function:       loads the indicated file, and sets some global table
**                      pointers.  The caller (IVS_xlat_stream()) will attach 
**                      the table pointers to its context struct
**
**      Description:    load the file header, then each section.  Allocate
**                      buffers as necessary
**                      
**      Input:          xfile       name of the xlation file
**
**      Output:         f1indbuf    index buffer for format one (IVS) 
**                      f2indbuf    index buffer for format two
**                      f1tof2buf   xlation buffer 1==>2
**                      f2tof1buf   xlation buffer 2==>1
**                      f1size      size in bytes of format one
**                      f2size      size in bytes of format two
**
**      Return:         None
**
**      Warnings:       None
**
**      History:      
**
*/
static int init_globals(xfile)
char xfile[];
{	
	int f1tof2sz,f2tof1sz;
	int last;
	int bytesread;
	char xlatpath[200];
	int xlatfd;
	static int global_init_flag= -1;
	
	if (global_init_flag != -1)
	  return 0;
	else
	  global_init_flag=1;
	
	setord();
	strcpy(xlatpath,xfile);

	if ((xlatfd = open(xlatpath,O_RDONLY))<0)
	{
		/* this should never happen -- access has been verified by IVS_vlanguage 
                   which calls this function */
		VL_vre_window("Unable to open file %s: %s",xlatpath,strerror(errno));
		return -1;
	}
	bytesread=read(xlatfd,&header,sizeof(header));
	if (bytesread != sizeof(header))
	{
		VL_vre_window("Error processing file %s section header",xlatpath);
		close(xlatfd);
		return -1;
	}
	fixbyteorder(&header);
	if (header.magic != CHINESEMAGIC)
	{
		VL_vre_window("File %s does not appear to be a translation file",
			   xlatpath);
		close(xlatfd);
		return -1;
	}
	if (header.version != CHINESEVERSION)
	{
		VL_vre_window("File %s is the wrong version",
			   xlatpath);
		close(xlatfd);
		return -1;
	}
	f1indsz = (f1size=charsets[header.fmt1].bytesper) * 256;
	f2indsz = (f2size=charsets[header.fmt2].bytesper) * 256;
	
	getbuf(&f1indbuf,header.f1_ind_len);
	getbuf(&f2indbuf,header.f2_ind_len);
	getbuf(&f1tof2buf,header.f1_to_f2_len);
	getbuf(&f2tof1buf,header.f2_to_f1_len);

	lseek(xlatfd,header.f1_ind_offs,0);
	if ((bytesread=read(xlatfd,f1indbuf,header.f1_ind_len))!=header.f1_ind_len)
	{
		VL_vre_window("Error processing file %s section 1",xlatpath);
		close(xlatfd);
		return -1;
	}
	lseek(xlatfd,header.f2_ind_offs,0);
	if ((bytesread=read(xlatfd,f2indbuf,header.f2_ind_len))!=header.f2_ind_len)
	{
		VL_vre_window("Error processing file %s section 2",xlatpath);
		close(xlatfd);
		return -1;
	}
	lseek(xlatfd,header.f1_to_f2_offs,0);
	if ((bytesread=read(xlatfd,f1tof2buf,header.f1_to_f2_len))!=header.f1_to_f2_len)
	{
		VL_vre_window("Error processing file %s section 3",xlatpath);
		close(xlatfd);
		return -1;
	}
	lseek(xlatfd,header.f2_to_f1_offs,0);
	if ((bytesread=read(xlatfd,f2tof1buf,header.f2_to_f1_len))!=header.f2_to_f1_len)
	{
		VL_vre_window("Error processing file %s section 4",xlatpath);
		close(xlatfd);
		return -1;
	}	
	close(xlatfd);

	/* now we want to figure the number of valid bytes per position 
           for each format.  This number is used to calculate addresses */
	for (i=0; i< f1indsz; i+= 256)
	{
		for (j=255; j>0; --j)
		  if ((k = f1indbuf[i+j]) != 0xff)
		  {
			  max1[i/256]= f1indbuf[i+j]+1;
			  break;
		  }
	}
	for (i=0; i< f2indsz; i+= 256)
	{
		for (j=255; j>0; --j)
		  if ((k = f2indbuf[i+j]) != 0xff)
		  {
			  max2[i/256]= f2indbuf[i+j]+1;
			  break;
		  }
	}
	/* using the max value, we build a list of multipliers for the 
           address generation.  Arrays mult1 and mult2 are indexed by the 
           byte-position-in-the-sequence, and the value there is multiplied
           by the actual byte in the sequence to produce the address */
	for (last=1,i=f1size-1; i>=0; --i)
	{
		mult1[i]=last;
		last *= max1[i];
	}
	for (last=1,i=f2size-1; i>=0; --i)
	{
		mult2[i]=last;
		last *= max2[i];
	}

	/* now generate some table sizes */
	f1tof2sz= max1[0];
	for (i=1; i<f1size; ++i)
	  f1tof2sz *= max1[i];
	f1tof2sz *= f2size;
	f2tof1sz= max2[0];
	for (i=1; i<f2size; ++i)
	  f2tof1sz *= max2[i];
	f2tof1sz *= f1size;

	return 0;
}
/*
** function:  fixlong()
** 
**    toggle a longword between neutral and native format
**
*/
static int fixlong(dest)
char *dest;
{
	char tmp[4];
	char *p;
	int i;
	
	if (byteorder==ORDER_NORMAL)
	  return 0;

	for (i=3,p=tmp; i>=0; --i)
	{
		*p++ = *(dest+i);
	}
	memcpy(dest,tmp,4);

	return 0;
	
}
/*
** function: setord()
**
**    set the byte order for this machines
**/
static int setord()
{
	int foo=0x12345678;
	if ((((char*)&foo)[0])==0x78) byteorder=ORDER_REVERSE;
	else byteorder=ORDER_NORMAL;

	return 0;
	
}
/*
** function: getbuf()
**
**   allocate a buffer of sz bytes.
*/
static int getbuf(buf,sz)
char **buf;
int sz;
{
	if (*buf)
	  free(*buf);
	*buf = malloc(sz);

	return 0;
	
}
/*
** function: fixbyteorder()
**
**   fix the byte order of every field in the header
**
*/
static int fixbyteorder(header)
struct xfileheader *header;
{
	setord();
	fixlong(&(header->magic));	        fixlong(&(header->version));	
	fixlong(&(header->fmt1));	        fixlong(&(header->fmt2));	
	fixlong(&(header->f1_ind_offs));	fixlong(&(header->f1_ind_len));
	fixlong(&(header->f2_ind_offs));	fixlong(&(header->f2_ind_len));
	fixlong(&(header->f1_to_f2_offs));	fixlong(&(header->f1_to_f2_len));
	fixlong(&(header->f2_to_f1_offs));	fixlong(&(header->f2_to_f1_len));

	return 0;
	
}
/* 
** function: IVS_vlanguage()
** 
** arguments:
**   path -  full path of the language file 
**
** desc:
**   this function is called from the outside world to initialize
**   the translator.  Afterwords IVS_xlat_stream is called to do the
**   actual work.
**/
int IVS_vlanguage(const char* path)
{
	if (!strlen(path) || access(path,R_OK)== -1)
	  return -1;
	strcpy(vlangfile,path);
	if (init_globals(vlangfile)== -1)
	  return -1;
	init_xlat_ctx(&IVS_outctx,vlangfile,XL_FROM_IVS);
	init_xlat_ctx(&IVS_inctx,vlangfile,XL_TO_IVS);
	return 0;
}

/*
**	History:
**	$Log: vchinese.c,v $
**	Revision 1.15  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.14  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.13  2003/01/31 19:38:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.12  2002/10/16 20:35:03  gsl
**	no message
**	
**	Revision 1.11  2002/09/30 21:02:02  gsl
**	update
**	
**	Revision 1.10  2002/09/04 18:14:51  gsl
**	LINUX sys_errlist
**	
**	Revision 1.9  2002/07/15 20:16:17  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.8  2002/07/15 13:29:02  gsl
**	IVS_ globals
**	
**	Revision 1.7  2002/07/12 20:40:43  gsl
**	Global unique WL_ changes
**	
**	Revision 1.6  2002/07/09 19:54:40  gsl
**	fix warnings
**	
**	Revision 1.5  1996-07-26 13:18:52-04  gsl
**	fix video include
**
**	Revision 1.4  1995-04-25 02:50:07-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.3  1995/04/17  11:43:48  gsl
 * drcs state V3_3_14
 *
 * Revision 1.2  1995/04/10  09:09:29  gsl
 * fix compiler warnings and added headers.
 *
**
**
*/
