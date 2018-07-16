/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
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
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/********************************************************************************************************************************
*																*
*	openacp.c	Initialize logical connection between application program and telecommunications device.		*
*			If first time in subroutine, generate static linked list from asynchronous communications		*
*			device map file, acpmap.dat.  Use input terminal name to search list, and obtain the VAX		*
*			device id number.  Open the device using $ASSIGN system service.					*
*			input:	destination	terminal name, up to 16 chars							*
*																*
*			output: rel_line	relative line number, (1-6), in WANG 4-byte binary format (WL_wswap an int)	*
*				ret_code	WANG 4-byte binary format return code (WL_wswap an int)				*
*																*
********************************************************************************************************************************/

#if defined(unix) 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "idsistd.h"
#include "wispcfg.h"
#include "paths.h"
#include "wisplib.h"

#define INIT_ACP							/* Declare global variable definitions from this module.*/
#include "acp.h"
#include "wmalloc.h"							/* Header file containing global variable definitions.	*/


#include <sys/types.h>
#include <fcntl.h>

struct matchstruc 
{
	char *string;
	int val;
};

static char the_ttyname[32];


static char cfgpath[100];

static char lname[17];
static char ldevice[65];
static char lweor[7];
static char lreor1[7];  
static char lreor2[7];  
static char lreor3[7];  
static char lbaud[6];   
static char lparity;    
static char lbits;      
static char lstop;      
static char lduplex;    
static char lflow;     

static int parity,baud,stop,size,duplex,flow;
static struct matchstruc parval[]=
{
	{ "-", 0 },
	{ "n", 0 },
	{ "N", 0 },
	{ "e", PARENB },
	{ "o", PARENB|PARODD },
	{ "E", PARENB },
	{ "O", PARENB|PARODD },
	{ 0, 0 }
};
static struct matchstruc dupval[]=
{
	{ "f", ECHO },
	{ "h", 0 },
	{ "F", ECHO },
	{ "H", 0 },
        { 0, 0 }
};        
static struct matchstruc flowval[]=
{
	{ "X", 'x' },	/* Use XON/XOFF */
	{ "x", 'x' },
	{ "-", 'x' },
	{ "N", 'n' },	/* Don't use XON/XOFF */
	{ "n", 'n' },
	{ "U", 'u' },	/* Don't change XON/XOFF */
	{ "u", 'u' },
        { 0, 0 }
};        
static struct matchstruc  baudval[]=
{
	{ "50",	   B50	  },
	{ "75",	   B75	  },
	{ "110",   B110   },
	{ "150",   B150   },
	{ "200",   B200   },
	{ "300",   B300   },
	{ "600",   B600   },
	{ "1200",  B1200  },
	{ "1800",  B1800  },
	{ "2400",  B2400  },
	{ "4800",  B4800  },
	{ "9600",  B9600  },
	{ "19200", B19200 },
	{ "38400", B38400 },
	{ 0, 0 }
};
static struct matchstruc  sizeval[]=
{
	{ "5", CS5 },
	{ "6", CS6 },
	{ "7", CS7 },
	{ "8", CS8 },
	{ 0, 0 }
};
static struct matchstruc  stopbval[]=
{
	{ "2", CSTOPB },
	{ "1", 0 },
	{ 0, 0 }
};

static int loadcfg(char *pname);
static char **splitstr(char *string);
static int match(char *str, struct matchstruc *list);
static void hexcnv(char *dest,char *src);
static int hexdig(char ch);
static void chopspace(char *p);
static int spaces(char *p);


void OPENACP(
	char	*destination,	/* WANG terminal name, up to 16 chars.			*/
	int4	*rel_line,	/* Index from 1 to 6 for later access to acp_term[].	*/
	int4	*ret_code)	/* WANG ACP return code.				*/
{
	acp_term_id	*term_ptr;					/* Pointer to the linked list.				*/
	int		i,j,k;						/* Array indeces.					*/
	int4		status;						/* Return status from SYS$ASSIGN			*/
	int fd,cfgstatus;

	*rel_line = 0;							/* Initialize the return values.			*/
	*ret_code = 0;



	if ((cfgstatus=loadcfg(destination))<0)				/* load cfgfile and get info for list destination */
	{
		*ret_code = -cfgstatus;
		WL_wswap(ret_code);
		return;
	}
	term_ptr = &acp_term_struct; /* was acp_getterm() */
	if ((fd=open(the_ttyname,O_RDWR))<0) 
	{
		perror("open");
		status=errno;						/* open tty */
 	}
	else
	{
		status=0;
		if (lockf(fd,F_TLOCK,0)== -1) 
		{
			status = EPERM;
			close(fd);
		}
		else
		{
			ioctl(fd,TCGETA,&acp_oterm);					/* save settings for restore at close */
			ioctl(fd,TCGETA,&acp_nterm);					/* make a copy to play with  */
			switch (flowval[flow].val)
			{
			default:
			case 'x':
				acp_nterm.c_iflag = IGNBRK|IXON|IXOFF;			/* Use XON/XOFF				*/
				break;
			case 'n':
				acp_nterm.c_iflag = IGNBRK;				/* Don't use XON/XOFF			*/
				break;
			case 'u':							/* Don't change config			*/
				/* Don't change input config */
				break;
			}
			acp_nterm.c_oflag = 0;
			acp_nterm.c_cflag = baudval[baud].val|sizeval[size].val|stopbval[stop].val|parval[parity].val|CLOCAL|CREAD;
			acp_nterm.c_lflag = 0 /* dupval[duplex].val */ ;
			acp_nterm.c_cc[VTIME] = 0;
			acp_nterm.c_cc[VMIN] = 1;
			ioctl(fd,TCSETA,&acp_nterm);
		}
	}
	

	switch (status)							/* Finish processing according to possible statuses.	*/
	{
	        case 0:
		{
			for (i=0;i<ACPMAXDEV;i++)			/* Point i to first available element in array.		*/
			{
				if (acp_term[i]==0) break;
			}
			acp_term[i] = term_ptr->acp_termnum;		/* Add new id to acp_term[].				*/
			for(j=0;j<4;j++)				/* Set write EOR sequences.				*/
				acp_weor[i][j] = term_ptr->acp_weorseq[j];
			for(j=0;j<3;j++)				/* Set read EOR sequences.				*/
				for(k=0;k<4;k++)
					 acp_reor[i][j][k] = term_ptr->acp_reorseq[j][k];
			acp_ch[i] = fd;					/* Add new channel to acp_ch[].				*/
			acp_blockmode[i] = BLOCKING;
			acp_devname[i] = wisp_strdup(the_ttyname);
			for(j=0;j<4;j++) acp_iosb[i][j] = 0;		/* Clear I/O status block.				*/
			acp_ef[i] = 0;					/* Clear event flag.					*/
			*rel_line = i+1;				/* Set the relative line number.			*/
			WL_wswap(rel_line);				/* Swap words so the WANG can read it.			*/
			acp_allocated++;				/* Increment number of lines allocated.			*/
			*ret_code = 0;					/* Return to application program successful completion.	*/
			break;
		}
		case ENOENT:
		case ENXIO:
		case ENODEV:
		case ENOTTY:
		{
			*ret_code = 43;					/* 43 = Invalid or non-existent device.			*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	        case EPERM:
	        case EACCES:
		{
			*ret_code = 45;					/* 45 = Device in use.					*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		default:
		{
			*ret_code = 14+ (status <<8);			/* 14 = Connection failure.				*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	}
}

static int loadcfg(char *pname)
{
	char *tmp,linebuf[200],**fields;
	FILE *cfg;
	int len,foundit=0;
	char name[17];
	
	memcpy(name,pname,16);
        name[16]=(char)0;
	tmp=strchr(name,' ');
	if (tmp) *tmp=(char)0;
	len=strlen(name);

	strcpy(cfgpath,WL_acpconfigdir());
	strcat(cfgpath,DIR_SEPARATOR_STR);
	strcat(cfgpath,WL_acpmapfile());
	if ((cfg=fopen(cfgpath,"r"))==NULL)
	{
		return -14;						/* no config file */
	}
	while (fgets(linebuf,200,cfg))
	{
		if (strncmp(linebuf,name,len)) continue;		/* match wang name */
		else 
		{
			++foundit;
			break;
		}
	}
	fclose(cfg);
	if (!foundit) return -40;					/* no match */
	fields=splitstr(linebuf);					/* divide record into fields */
	strcpy(the_ttyname,fields[TTYNAME_FIELD]);
	baud=match(fields[BAUD_FIELD],baudval);				/* digest each field */
	parity=match(fields[PARITY_FIELD],parval);
	stop=match(fields[STOPB_FIELD],stopbval);
	size=match(fields[SIZE_FIELD],sizeval);
	duplex=match(fields[DUPLEX_FIELD],dupval);
	flow=match(fields[FLOW_FIELD],flowval);
	
	strcpy(acp_term_struct.acp_termname,name);
	strcpy(acp_term_struct.acp_termnum,the_ttyname);
	if (strcmp(fields[WEOR_FIELD],"-"))				/* now convert the hex fields into their values */
	{
		hexcnv(&acp_term_struct.acp_weorseq[1],fields[WEOR_FIELD]);
		acp_term_struct.acp_weorseq[0]=(char)strlen(fields[WEOR_FIELD])/2;
	}
	else acp_term_struct.acp_weorseq[0]=0;
	if (strcmp(fields[EOR1_FIELD],"-")) 
	{
		hexcnv(&acp_term_struct.acp_reorseq[0][1],fields[EOR1_FIELD]);
		acp_term_struct.acp_reorseq[0][0]=(char)strlen(fields[EOR1_FIELD])/2;
	}
	else acp_term_struct.acp_reorseq[0][0]=0;
	if (strcmp(fields[EOR2_FIELD],"-")) 
	{
		hexcnv(&acp_term_struct.acp_reorseq[1][1],fields[EOR2_FIELD]);
		acp_term_struct.acp_reorseq[1][0]=(char)strlen(fields[EOR2_FIELD])/2;
	}
	else acp_term_struct.acp_reorseq[1][0]=0;
	if (strcmp(fields[EOR3_FIELD],"-")) 
	{
		hexcnv(&acp_term_struct.acp_reorseq[2][1],fields[EOR3_FIELD]);
		acp_term_struct.acp_reorseq[2][0]=(char)strlen(fields[EOR3_FIELD])/2;
	}
	else acp_term_struct.acp_reorseq[2][0]=0;

	return 0;
}
static void hexcnv(char *dest,char *src)
{
	while (*src)
	{
		*dest = hexdig(*src)<<4 | hexdig(*(src+1));
		++dest;
		++src; ++src;
	}
}
static int hexdig(char ch)
{
	if (ch>='0' && ch<='9') 
	{
		return ch-'0';
	}
	if (ch>='A' && ch<='F')
	{
		return ch-'A'+10;
	}
	if (ch>='a' && ch<='f')
	{
		return ch-'a'+10;
	}
	return 0;
}
static char **splitstr(char *string)
{
	static char *ptr[NUMFIELDS],linebuf[200];
	char *p;
	int i;

	for(i=0; i<NUMFIELDS;i++)
	{
		ptr[i] = (char *)0;
	}

	strcpy(linebuf,string);							/* make local copy */
	for (i=0,p=linebuf; *p && i<NUMFIELDS; )				/* build list of pointers */
	{
		while (*p == ' ' || *p=='\t') ++p;				/* skip over white space to next field */
		if (*p) ptr[i++]=p;						/* if at field start, save pointer */
		while (*p && *p != ' ' && *p != '\t' && *p != '\n') ++p;	/* now skip field, look for white space */
		*p++ =(char)0;							/* null term the field  */
	}
	
	return ptr;
}
static int match(char *str, struct matchstruc *list)
{
	int 	i;
	char 	tmp[20],*p;

	/*
	**	On error or no match return 0 index as a default.
	*/
	if (!str) return 0;							/* Null string					*/

	p=str;
	while (*p == '0') ++p;
	strcpy(tmp,p);
	p=strchr(tmp,' ');
	if (p) *p=(char)0;
	
	for (i=0;list->string;++list,++i)				/* loop thru struct array, looking for matching value */
	{
		if (0==strcmp(list->string,tmp))
		{
			return i;						/* Found it 					*/
		}
	}
	return 0;								/* No match.					*/
}

void SETACP(struct acpinfo_cbl *info, int *ret)
{
	FILE *acpmap,*tmpfile;
	char tmppath[64],*tmp,linebuf[200];
	int len,notfound;
	int intbaud;
	int	rc;
	int fd;
	
	memset(lname,0,sizeof(lname));
	memset(ldevice,0,sizeof(ldevice));
	memset(lweor,0,sizeof(lweor));
	memset(lreor1,0,sizeof(lreor1));
	memset(lreor2,0,sizeof(lreor2));
	memset(lreor3,0,sizeof(lreor3));
	memset(lbaud,0,sizeof(lbaud));
	lparity=(char)0;    
	lbits=(char)0;      
	lstop=(char)0;      
	lduplex=(char)0;    
	lflow=(char)0;     
	strncpy(lname,   info->name,   sizeof(info->name));
	strncpy(ldevice, info->device, sizeof(info->device));
	strncpy(lweor,   info->weor,   sizeof(info->weor));
	strncpy(lreor1,	 info->reor1,  sizeof(info->reor1));
	strncpy(lreor2,	 info->reor2,  sizeof(info->reor2));
	strncpy(lreor3,	 info->reor3,  sizeof(info->reor3));
	strncpy(lbaud,   info->baud,   sizeof(info->baud));

	intbaud=atoi(lbaud);
	sprintf(lbaud,"%d",intbaud);

	lparity=info->parity;
	lbits=info->bits;
	lstop=info->stop;
	lduplex=info->duplex;
	lflow=info->flow;
	
	chopspace(lname);
	chopspace(ldevice);
	chopspace(lweor);
	chopspace(lreor1);
	chopspace(lreor2);
	chopspace(lreor3);
	chopspace(lbaud);
	
	strcpy(cfgpath,WL_acpconfigdir());
	sprintf(tmppath,"%s%sacp%ld",cfgpath,DIR_SEPARATOR_STR,(long)getpid());
	strcat(cfgpath,DIR_SEPARATOR_STR);
	strcat(cfgpath,WL_acpmapfile());

	if ((fd=open(cfgpath,O_RDONLY))<0)    /* create the ACPMAP file if it doesn't exist */
	{
		if (errno==ENOENT)
		{
			fd=creat(cfgpath,0666);
			if (fd<0)
			{
				rc = 14;
				memcpy(ret,&rc,4);					/* no config file */
				WL_wswap(ret);
				return;
			}
			close(fd);
		}
	}
	else
	{
		close(fd);
	}
	if ((acpmap=fopen(cfgpath,"r"))==NULL)
	{
		rc = 14;
		memcpy(ret,&rc,4);					/* no config file */
		WL_wswap(ret);
		return;
	}  
	if ((tmpfile=fopen(tmppath,"w"))==NULL)
	{
		rc = 1;
		memcpy(ret,&rc,4);					/* can't create tmp file */
		WL_wswap(ret);
		return;
	}
	tmp=strchr(lname,' ');
	if (tmp) *tmp=(char)0;
	
	len=strlen(lname); 
	notfound=1;
	while (fgets(linebuf,200,acpmap))
	{
		if (strncmp(linebuf,lname,len)) 
		{
			fputs(linebuf,tmpfile);
		}
		else 
		{
			notfound=0;
			fprintf(tmpfile,
				"%s %s %s %s %s %s %s %c %c %c %c %c\n",
				lname,ldevice,lweor,lreor1,lreor2,lreor3,lbaud,
				lparity,lbits,lstop,lduplex,lflow);
		}
	}
	if (notfound)
	{
		fprintf(tmpfile,
			"%s %s %s %s %s %s %s %c %c %c %c %c\n",
			lname,ldevice,lweor,lreor1,lreor2,lreor3,lbaud,
			lparity,lbits,lstop,lduplex,lflow);
	}
	fclose(acpmap);
	fclose(tmpfile);
	
	wisp_unlink(cfgpath);
	link(tmppath,cfgpath);
	wisp_unlink(tmppath);

	rc = 0;
	memcpy(ret,&rc,4);
	WL_wswap(ret);
	return;
}
void GETACP(struct acpinfo_cbl *info,int *ret)
{
	FILE *acpmap;
	char *tmp,**fields,name[17],linebuf[200];
	int foundit,len;
	int intbaud;
	int	rc;
	
	memset(name,0,sizeof(name));
	memcpy(name,info->name,16);
	tmp=strchr(name,' ');
	if (tmp) *tmp=(char)0;
	len=strlen(name);

	strcpy(cfgpath,WL_acpconfigdir());
	strcat(cfgpath,DIR_SEPARATOR_STR);
	strcat(cfgpath,WL_acpmapfile());
	if ((acpmap=fopen(cfgpath,"r"))==NULL)
	{
		rc = 14;
		memcpy(ret,&rc,4);					/* no config file */
		WL_wswap(ret);
		return;
	}
	foundit=0;
	while (fgets(linebuf,200,acpmap))
	{
		if (strncmp(linebuf,name,len)) 
		{
			continue;
		}
		else 
		{
			++foundit;
			break;
		}
	}
	fclose(acpmap);
	if (!foundit) 
	{
		rc = 4;
		memcpy(ret,&rc,4);					/* NODE not found	*/
		WL_wswap(ret);
		return;
	}

	fields=splitstr(linebuf);

	memset(info,' ',sizeof(struct acpinfo_cbl));
	memcpy(info->name, name, strlen(name));
	
	memcpy(info->device, fields[TTYNAME_FIELD],strlen(fields[TTYNAME_FIELD]));
	memcpy(info->weor,   fields[WEOR_FIELD   ],strlen(fields[WEOR_FIELD   ]));
	memcpy(info->reor1,  fields[EOR1_FIELD   ],strlen(fields[EOR1_FIELD   ]));
	memcpy(info->reor2,  fields[EOR2_FIELD   ],strlen(fields[EOR2_FIELD   ]));
	memcpy(info->reor3,  fields[EOR3_FIELD   ],strlen(fields[EOR3_FIELD   ]));

	strncpy(lbaud, fields[BAUD_FIELD],strlen(fields[BAUD_FIELD]));
	intbaud=atoi(lbaud);

	sprintf(info->baud,"%05d",intbaud);
/*	memcpy(info->baud,   fields[BAUD_FIELD   ],strlen(fields[BAUD_FIELD   ])); */

	info->parity = *fields[PARITY_FIELD];
	info->bits   = *fields[SIZE_FIELD  ];
	info->stop   = *fields[STOPB_FIELD ];
	info->duplex = *fields[DUPLEX_FIELD];
	info->flow   = *fields[FLOW_FIELD];

	rc = 0;
	memcpy(ret,&rc,4);
	WL_wswap(ret);
	return;
}
static void chopspace(char *p)
{
	char *tmp;
	
	if (spaces(p))
	{
		strcpy(p,"-");
		return;
	}
	tmp=strchr(p,' ');
	if (tmp) *tmp=(char)0;
}	
static int spaces(char *p)
{
	do
	{
		if (*p != ' ') return 0;
		
	}
	while (*(++p));
	return 1;
}

#endif	/* unix */

/*
**	History:
**	$Log: openacp.c,v $
**	Revision 1.25  2003/02/04 16:30:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.24  2003/01/31 19:02:51  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.23  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.22  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.21  2002/12/11 17:03:07  gsl
**	use wisp_unlink()
**	
**	Revision 1.20  2002/07/12 17:00:58  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.19  2002/07/11 14:41:07  gsl
**	Fix acp getterm()
**	
**	Revision 1.18  2002/07/11 14:33:19  gsl
**	Cleanup ACP globals
**	
**	Revision 1.17  2002/07/10 21:05:21  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.16  2002/07/02 21:15:27  gsl
**	Rename wstrdup
**	
**	Revision 1.15  2002/06/26 01:42:46  gsl
**	Remove VMS code
**	
**	Revision 1.14  1996/10/10 16:07:01  gsl
**	fix prototype errors
**	
**	Revision 1.13  1996-10-08 17:23:20-07  gsl
**	replace getenv() with WL_acpconfigdir() and WL_acpmapfile() calls
**
**	Revision 1.12  1996-08-19 15:32:36-07  gsl
**	drcs update
**
**
**
*/
