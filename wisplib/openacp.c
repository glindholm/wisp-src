/********************************************************************************************************************************
*																*
*	openacp.c	Initialize logical connection between application program and telecommunications device.		*
*			If first time in subroutine, generate static linked list from asynchronous communications		*
*			device map file, acpmap.dat.  Use input terminal name to search list, and obtain the VAX		*
*			device id number.  Open the device using $ASSIGN system service.					*
*			input:	destination	terminal name, up to 16 chars							*
*																*
*			output: rel_line	relative line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*				ret_code	WANG 4-byte binary format return code (wswap an int)				*
*																*
********************************************************************************************************************************/

#ifndef MSDOS								/* Defined for unix and VMS only.			*/

#define INIT_ACP							/* Declare global variable definitions from this module.*/
#define __OPENACP
#include "acp.h"							/* Header file containing global variable definitions.	*/

#ifdef VMS
#include <descrip.h>							/* SYS$ASSIGN requires string descriptors.		*/
#endif

#ifdef unix
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
struct termio otermacp,ntermacp;
char the_ttyname[32];
char *mystrdup();
char *getenv();
#endif	/* unix */


OPENACP(destination,rel_line,ret_code)

char	*destination;							/* WANG terminal name, up to 16 chars.			*/
int	*rel_line;							/* Index from 1 to 6 for later access to acp_term[].	*/
int	*ret_code;							/* WANG ACP return code.				*/

{
	acp_term_id	*term_ptr,*getterm();				/* Pointer to the linked list.				*/
	char 		*dest_term = 0;					/* VAX terminal line to be opened.			*/
	char 		device_name[64];				/* VAX terminal line to be opened for descriptor.	*/
	short		acp_channel;					/* I/O channel returned from SYS$ASSIGN.		*/
	int		i,j,k;						/* Array indeces.					*/
	long		status;						/* Return status from SYS$ASSIGN			*/
#ifdef unix
	int fd,cfgstatus;
	
#endif	

#ifdef VMS
$DESCRIPTOR(d_desc,device_name);					/* Descriptor for SYS$ASSIGN.				*/
#endif
	*rel_line = 0;							/* Initialize the return values.			*/
	*ret_code = 0;


#ifdef VMS								/* acp_allocated == number of terminal lines allocated.	*/
	if (!acp_allocated) bldacp();					/* Build the linked list from the file.			*/

	if (!(term_ptr = getterm(destination)))				/* Get the term id, if NULL, destination is in error.	*/
	{
		*ret_code = 43;						/* 43 = Invalid or non-existent device.			*/
		wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return(0);						/* Return to application program.			*/
	}
	dest_term = term_ptr->acp_termnum;				/* Set pointer to VAX term id.				*/

	/************************************************************************************************************************
	*	Check if the requested acp term is in the array BEFORE checking max number of lines allocated, because if it	*
	*	IS in the array even if the array is full, we want to tell them it's already opened and return successfully.	*
	************************************************************************************************************************/

	j = -1;								/* Give it an initial value.				*/

	for(i=0;i<ACPMAXDEV;i++)					/* Search the whole array for matching destination line.*/
		if (acp_term[i] != 0)
			if ((j=strcmp(dest_term,acp_term[i])) == 0) break;

	if (j == 0)							/* It is already in the array.				*/
	{
		*ret_code = 44;						/* 44 = Destination in use (previously opened).		*/
		wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		*rel_line = i+1;					/* Return its relative line number too.			*/
		wswap(rel_line);					/* Swap words so the WANG can read it.			*/
		return(0);						/* Return to application program.			*/
	}

	if (acp_allocated == ACPMAXDEV)					/* All lines already allocated.				*/
	{
		*ret_code = 24;						/* 24 = Maximum number of destination lines in use.	*/
		wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return(0);						/* Return to application program.			*/
	}

	strcpy(device_name,dest_term);					/* Prepare the descriptor.				*/
	status=sys$assign(&d_desc,&acp_channel,0,0);			/* Assign I/O channel.					*/
#endif
#ifdef unix
	if ((cfgstatus=loadcfg(destination))<0)				/* load cfgfile and get info for list destination */
	{
		*ret_code = -cfgstatus;
		wswap(ret_code);
		return(0);
	}
	term_ptr=getterm();
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
			ioctl(fd,TCGETA,&otermacp);					/* save settings for restore at close */
			ioctl(fd,TCGETA,&ntermacp);					/* make a copy to play with  */
			ntermacp.c_iflag = IGNBRK|IXON|IXOFF;				/*  */
			ntermacp.c_oflag = 0;
			ntermacp.c_cflag = baudval[baud].val|sizeval[size].val|stopbval[stop].val|parval[parity].val|CLOCAL|CREAD;
			ntermacp.c_lflag = 0 /* dupval[duplex].val */ ;
			ntermacp.c_cc[VTIME] = 0;
			ntermacp.c_cc[VMIN] = 1;
			ioctl(fd,TCSETA,&ntermacp);
		}
	}
	
#endif	

	switch (status)							/* Finish processing according to possible statuses.	*/
	{
#ifdef VMS		
		case SS$_NORMAL:					/* I/O channel successfully allocated.			*/
		case SS$_REMOTE:					/* I/O channel successfully allocated on remote node.	*/
#endif
#ifdef unix
	        case 0:
#endif		
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
#ifdef VMS
			acp_ch[i] = acp_channel;			/* Add new channel to acp_ch[].				*/
#endif
#ifdef unix
			acp_ch[i] = fd;					/* Add new channel to acp_ch[].				*/
			acp_blockmode[i] = BLOCKING;
			acp_devname[i] = mystrdup(the_ttyname);
#endif
			for(j=0;j<4;j++) acp_iosb[i][j] = 0;		/* Clear I/O status block.				*/
			acp_ef[i] = 0;					/* Clear event flag.					*/
			*rel_line = i+1;				/* Set the relative line number.			*/
 wswap(rel_line);				/* Swap words so the WANG can read it.			*/
			acp_allocated++;				/* Increment number of lines allocated.			*/
			*ret_code = 0;					/* Return to application program successful completion.	*/
			break;
		}
#ifdef VMS
		case SS$_ACCVIO:
		case SS$_DEVACTIVE:
		case SS$_DEVNOTMBX:
		case SS$_IVDEVNAM:
		case SS$_IVLOGNAM:
		case SS$_NOSUCHDEV:
		case SS$_NOSUCHNODE:
#endif
#ifdef unix
		case ENOENT:
		case ENXIO:
		case ENODEV:
		case ENOTTY:
#endif
		{
			*ret_code = 43;					/* 43 = Invalid or non-existent device.			*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
#ifdef VMS		
		case SS$_DEVALLOC:
		case SS$_FILALRACC:
#endif 
#ifdef unix
	        case EPERM:
	        case EACCES:
#endif		
		{
			*ret_code = 45;					/* 45 = Device in use.					*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		default:
		{
			*ret_code = 14+ status <<8;			/* 14 = Connection failure.				*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	}
}

#ifdef unix
static int loadcfg(pname)
char *pname;
{
	char *tmp,linebuf[200],**fields,**splitstr();
	char *strchr();
	FILE *cfg;
	int len,foundit=0,i;
	char name[17];
	
	memcpy(name,pname,16);
        name[16]=(char)0;
	tmp=strchr(name,' ');
	if (tmp) *tmp=(char)0;
	len=strlen(name);
	tmp=getenv(ACPMAP_PATH);					/* get path */
	strcpy(cfgpath,tmp?tmp:"./");
	strcat(cfgpath,"/");
	strcat(cfgpath,ACPMAP_FILE);					/* add file name */
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
}
hexcnv(dest,src)
char *dest,*src;
{
	while (*src)
	{
		*dest = hexdig(*src)<<4 | hexdig(*(src+1));
		++dest;
		++src; ++src;
	}
}
hexdig(ch)
char ch;
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
}
static char **splitstr(string)
char *string;
{
	static char *ptr[13],linebuf[200];
	char *p;
	int i;

	strcpy(linebuf,string);						/* make local copy */
	for (i=0,p=linebuf; *p && i<11; )				/* build list of pointers */
	{
		if (*p != ' ' && *p != '\t') ptr[i++]=p;		/* if at field start, save pointer */
		while (*p != ' ' && *p!='\t') ++p;			/* now skip field, look for white space */
		*p++ =(char)0;						/* null term the field  */
		while (*p == ' ' || *p=='\t') ++p;			/* skip over white space to next field */
	}
	
	return ptr;
}
static int match(str,list)
char *str;
struct matchstruc *list;
{
	int foundit=0,i;
	char tmp[20],*p,*strchr();

	p=str;
	while (*p == '0') ++p;
	strcpy(tmp,p);
	p=strchr(tmp,' ');
	if (p) *p=(char)0;
	
	for (i=0;list->string;++list,++i)				/* loop thru struct array, looking for matching value */
	{
		if (strcmp(list->string,tmp))continue;
		else { ++foundit; break; }
	}
	if (!foundit)return -1;
	else return i;
}
SETACP(info,ret)
struct acpinfo_cbl *info;
int *ret;
{
	FILE *acpmap,*tmpfile;
	char tmppath[64],*tmp,linebuf[200],*strchr();
	int len,notfound;
	int intbaud;
	
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
	lfoo=(char)0;     
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
	
	chopspace(lname);
	chopspace(ldevice);
	chopspace(lweor);
	chopspace(lreor1);
	chopspace(lreor2);
	chopspace(lreor3);
	chopspace(lbaud);
	
	tmp=getenv(ACPMAP_PATH);					/* get path */
	strcpy(cfgpath,tmp?tmp:"./");
	strcat(cfgpath,"/");
	strcat(cfgpath,ACPMAP_FILE);					/* add file name */
	if ((acpmap=fopen(cfgpath,"r"))==NULL)
	{
		int tmp=14;
		
		memcpy(ret,&tmp,4);					/* no config file */
		wswap(ret);
		return;
	}
	sprintf(tmppath,"acp%d",getpid());
	if ((tmpfile=fopen(tmppath,"w"))==NULL)
	{
		int tmp=1;
		
		memcpy(ret,&tmp,4);					/* can't create tmp file */
		wswap(ret);
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
				"%s %s %s %s %s %s %s %c %c %c %c -\n",
				lname,ldevice,lweor,lreor1,lreor2,lreor3,lbaud,
				lparity,lbits,lstop,lduplex);
		}
	}
	if (notfound)
	{
		fprintf(tmpfile,
			"%s %s %s %s %s %s %s %c %c %c %c -\n",
			lname,ldevice,lweor,lreor1,lreor2,lreor3,lbaud,
			lparity,lbits,lstop,lduplex);
	}
	fclose(acpmap);
	fclose(tmpfile);
	
	unlink(cfgpath);
	link(tmppath,cfgpath);
	unlink(tmppath);
}
GETACP(info,ret)
struct acpinfo_cbl *info;
int *ret;
{
	FILE *acpmap,*debug;
	char *tmp,**fields,**splitstr(),name[17],*strchr(),linebuf[200];
	int foundit,len;
	int intbaud;
	
	memset(name,0,sizeof(name));
	memcpy(name,info->name,16);
	tmp=strchr(name,' ');
	if (tmp) *tmp=(char)0;
	len=strlen(name);

	tmp=getenv(ACPMAP_PATH);					/* get path */
	strcpy(cfgpath,tmp?tmp:"./");
	strcat(cfgpath,"/");
	strcat(cfgpath,ACPMAP_FILE);					/* add file name */
	if ((acpmap=fopen(cfgpath,"r"))==NULL)
	{
		int tmp=14;
		
		memcpy(ret,&tmp,4);					/* no config file */
		wswap(ret);
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
	if (!foundit) return;

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

}
chopspace(p)
char *p;
{
	char *strchr(),*tmp;
	
	if (spaces(p))
	{
		strcpy(p,"-");
		return;
	}
	tmp=strchr(p,' ');
	if (tmp) *tmp=(char)0;
}	
spaces(p)
char *p;
{
	do
	{
		if (*p != ' ') return 0;
		
	}
	while (*(++p));
	return 1;
}

static char *
mystrdup(p)
char *p;
{
	char *malloc(),*tmp;
	
	tmp=malloc(strlen(p)+1);
	strcpy(tmp,p);
	return tmp;
}

#endif	/* unix */

#endif	/* unix or VMS */

#ifdef MSDOS	/* stub section.	*/

int	OPENACP(destination,rel_line,ret_code)

char	*destination;							/* WANG terminal name, up to 16 chars.			*/
int	*rel_line;							/* Index from 1 to 6 for later access to acp_term[].	*/
int	*ret_code;							/* WANG ACP return code.				*/

{
	vre("\n  Call to OPENACP invalid for MSDOS.  \n");
	return(0);							/* Return to application program.			*/
}

#endif	/* MSDOS */

