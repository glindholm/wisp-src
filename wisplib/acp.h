/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/**
 **																*
 **	acp.h	Header file for (WANG) Asynchronous Communications Program.							*
 **
 ** 																*
 **/
#ifndef ACP_H
#define ACP_H
static char *ident="@(#)acp 1.0   [Asynch Comm Prog]   IDSI Unix/VMS xx/xx/90";

#ifdef unix
#define BLOCKING 0
#define NONBLOCKING 1
#endif

#define		ACPMAXDEV	6					/* Maximum number of devices to allocate.		*/
#define		ACPMAXRECLEN	2048					/* Maximum length of I/O buffer.			*/

typedef struct	{							/* Structure representing records in file.		*/
			char			acp_termname[16];	/* Name given to terminal line.				*/
			char			acp_termnum[64];	/* VAX terminal id.					*/
			char			acp_weorseq[4];		/* EOR sequence for writeacp.				*/
			char			acp_reorseq[3][4];	/* Three EOR sequences for readacp.			*/
			struct	acp_term_id	*next;			/* Pointer to next element in linked list.		*/
		} acp_term_id;

#ifdef INIT_ACP								/* Declare variables as global with initialization.	*/
INIT_ACP	acp_term_id	*acp_term_list = 0;			/* Pointer to linked list of file records.		*/
INIT_ACP	char		*acp_term[ACPMAXDEV] = {0};		/* Static array to contain up to ACPMAXDEV line numbers.*/
INIT_ACP	char		acp_weor[ACPMAXDEV][4] = {0};		/* Static array to contain write EOR sequence.		*/
INIT_ACP	char		acp_reor[ACPMAXDEV][3][4] = {0};	/* Static array to contain read EOR sequences.		*/
INIT_ACP	short		acp_ch[ACPMAXDEV] = {0};		/* Static array to contain I/O channels.		*/
INIT_ACP	short		acp_iosb[ACPMAXDEV][4] = {0};		/* Static array to contain I/O status blocks.		*/
INIT_ACP	int4		acp_ef[ACPMAXDEV] = {0};		/* Static array to contain event flags.			*/
INIT_ACP	int		acp_allocated = 0;			/* Number of lines allocated.				*/
#ifdef unix
INIT_ACP	int		acp_blockmode[ACPMAXDEV] = {0};		/* Mode of line : blocking vs. nonblocking		*/
INIT_ACP        char            *acp_devname[ACPMAXDEV] = {0};
#endif
#else
#define INIT_ACP extern							/* Declare variables as extern with no initialization.	*/
INIT_ACP	acp_term_id	*acp_term_list;				/* Pointer to linked list of file records.		*/
INIT_ACP	char		*acp_term[ACPMAXDEV];			/* Static array to contain up to ACPMAXDEV line numbers.*/
INIT_ACP	char		acp_weor[ACPMAXDEV][4];			/* Static array to contain write EOR sequence.		*/
INIT_ACP	char		acp_reor[ACPMAXDEV][3][4];		/* Static array to contain read EOR sequences.		*/
INIT_ACP	short		acp_ch[ACPMAXDEV];			/* Static array to contain I/O channels.		*/
INIT_ACP	short		acp_iosb[ACPMAXDEV][4];			/* Static array to contain I/O status blocks.		*/
INIT_ACP	int4		acp_ef[ACPMAXDEV];			/* Static array to contain event flags.			*/
INIT_ACP	int		acp_allocated;				/* Number of lines allocated.				*/
#ifdef unix
INIT_ACP	int		acp_blockmode[ACPMAXDEV];		/* Mode of line : blocking vs. nonblocking		*/
INIT_ACP        char            *acp_devname[ACPMAXDEV];
#endif
#endif


#ifdef VMS
#include <ssdef.h>
#endif
#ifdef unix
#include <errno.h>
#include <unistd.h>
#endif

#ifdef unix
#include <termio.h>
acp_term_id acp_term_struct;
struct matchstruc 
{
	char *string;
	int val;
};
struct acpinfo_cbl 
{
	char 
	  name[16],
	  device[64],
	  weor[6],
	  reor1[6],
	  reor2[6],
	  reor3[6],
	  baud[5],
	  parity,
	  bits,
	  stop,
	  duplex,
	  flow;
};
char lname[17];
char ldevice[65];
char lweor[7];
char lreor1[7];  
char lreor2[7];  
char lreor3[7];  
char lbaud[6];   
char lparity;    
char lbits;      
char lstop;      
char lduplex;    
char lflow;     
     
#define NAME_FIELD    0
#define TTYNAME_FIELD 1
#define WEOR_FIELD   2
#define EOR1_FIELD   3
#define EOR2_FIELD   4
#define EOR3_FIELD   5
#define BAUD_FIELD   6
#define PARITY_FIELD 7
#define SIZE_FIELD   8
#define STOPB_FIELD  9
#define DUPLEX_FIELD 10
#define FLOW_FIELD   11
#define NUMFIELDS    12

#ifdef __OPENACP
int parity,baud,stop,size,duplex,flow;
int eor_timeout;
struct matchstruc parval[]=
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
struct matchstruc dupval[]=
{
	{ "f", ECHO },
	{ "h", 0 },
	{ "F", ECHO },
	{ "H", 0 },
        { 0, 0 }
};        
struct matchstruc flowval[]=
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
struct matchstruc  baudval[]=
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
struct matchstruc  sizeval[]=
{
	{ "5", CS5 },
	{ "6", CS6 },
	{ "7", CS7 },
	{ "8", CS8 },
	{ 0, 0 }
};
struct matchstruc  stopbval[]=
{
	{ "2", CSTOPB },
	{ "1", 0 },
	{ 0, 0 }
};
#else
extern int eor_timeout;
/*
extern struct matchstruc parval[];
extern struct matchstruc  baudval[];
extern struct matchstruc  sizeval[];
extern struct matchstruc  stopbval[];
*/
#endif
#endif

#endif /*ACP_H*/
/*
**	History:
**	$Log: acp.h,v $
**	Revision 1.9  1996/10/09 00:18:07  gsl
**	Removed unneeded defines and moved cfgpath[] to openacp.c
**	
**	Revision 1.8  1996-08-19 15:32:07-07  gsl
**	drcs update
**
**
**
*/
