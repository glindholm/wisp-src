static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*  SYSINF.C - Get the terminal information from the system.									*/

#ifdef MSDOS
#include <stdlib.h>
#endif

#include <stdio.h>

#include "idsistd.h"
#include "wglobals.h"
#include "werrlog.h"


#ifdef VMS

#include <iodef.h>
#include <string.h>

#include <ssdef.h>
#include <dvidef.h>
#include <jpidef.h>
#include <stdlib.h>
#include <errno.h>
#include "quidef.h"

#include "wperson.h"
#include "wdefines.h"

/*
**	*** WARNING ***
**	The following variable VINPUT_CHAN_NUM must be declared globalref.
**	This does produce a NON-PORTABLITY informational message on compile 
**	as it is a non-portable feature of the C interface to the VMS 
**	operating system.  If you change it to "extern" then the VMS linker
**	does not properly resolve it and the osd_term() routine fails.
**
**	If you have any question about this see me. GSL.
*/
globalref unsigned short VINPUT_CHAN_NUM;

static struct	{
		unsigned short		buflen;						/* the length of the buffer		*/
		unsigned short	 	item_code;					/* the code for the request to GETDVI	*/
		char 			*bufptr;					/* a pointer to the buffer		*/
		unsigned short		*retlen;					/* the return length of the buffer	*/
		int4 		endbuf;						/* the end of the buffer		*/
	} mybuf;

static struct	{
		unsigned short		buflen0;					/* the length of the buffer		*/
		unsigned short	 	item_code0;					/* the code for the request to GETDVI	*/
		char 			*bufptr0;					/* a pointer to the buffer		*/
		unsigned short		*retlen0;					/* the return length of the buffer	*/
		unsigned short		buflen1;					/* the length of the buffer		*/
		unsigned short 		item_code1;					/* the code for the request to GETDVI	*/
		char 			*bufptr1;					/* a pointer to the buffer		*/
		unsigned short		*retlen1;					/* the return length of the buffer	*/
		int4 		endbuf;						/* the end of the buffer		*/
	} quibuf;


static	int cur_term = -1;								/* Current terminal id.			*/
static	int cur_flags = -1;								/* Current flags.			*/

	/* #ifdef vax11c */

int osd_term(int4* tnum, int *flags)
{
#define		ROUTINE		65500
	char retbuf[65];
	short unsigned int length;
	int td_num,td_flags;								/* Value for default TD			*/
	int lt_num,lt_flags;								/* Value for default LT			*/
	int qn_num,qn_flags;								/* Value for default QN			*/
	int qt_num,qt_flags;								/* Value for default QT			*/
	int nv_num,nv_flags;								/* Value for default NV			*/
	int tw_num,tw_flags;								/* Value for default TW			*/
	int nd_num,nd_flags;								/* Value for default ND			*/
	int kt_num,kt_flags;								/* Value for default KT			*/
	int tn_num,tn_flags;								/* Value for default TN			*/
	int ft_num,ft_flags;								/* Value for default FT			*/
	char ch,latname[32],portname[32];
	int j,i;
	term_id *term_ptr;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	if (cur_term != -1)								/* We already know the terminal id.	*/
	{
		*tnum = cur_term;							/* So don't bother to search.		*/
		*flags = cur_flags;
		return;
	}

	lt_num = 255;	lt_flags = 0;
	td_num = 255;	td_flags = 0;
	qn_num = 255;	qn_flags = 0;
	qt_num = 255;	qt_flags = 0;
	nv_num = 255;	nv_flags = 0;
	tw_num = 255;	tw_flags = 0;
	nd_num = 255;	nd_flags = 0;
	kt_num = 255;	kt_flags = 0;
	tn_num = 255;	tn_flags = 0;
	ft_num = 255;	ft_flags = 0;

	osd_mode(&ch);									/* find out if we can have a terminal	*/
	if (ch != 'F')									/* not in foreground, no terminal	*/
	{
		*tnum = -1;								/* set term to -1			*/
		return;
	}

	ch = vcheck();									/* Initialize the input channels.	*/
	if (ch) vpushc(ch);								/* If have char then put back in buffer.*/

	mybuf.item_code = DVI$_TT_PHYDEVNAM;						/* ask for the physical device name	*/
	mybuf.buflen = 64;								/* in a 64 byte buffer			*/
	mybuf.bufptr = &retbuf[0];							/* which is retbuf			*/
	mybuf.retlen = &length;								/* return length address		*/
	mybuf.endbuf = 0;								/* mark end of buffers			*/

	sys$getdviw((long) 0,(short) VINPUT_CHAN_NUM,0,&mybuf,0,0,0,0);			/* get the physical terminal name	*/

	wpload();

	*tnum = 255;									/* unknown term is 255			*/

	term_ptr = get_term_list();

	if (!term_ptr)									/* Just set to 0 now.  Terminal Type	*/
	{										/* is not used anymore.			*/
		*tnum = 255;
		*flags = 0;
		cur_term = *tnum;
		cur_flags = *flags;
		return;
	}

	do
	{
		if (!strcmp(term_ptr->termname,retbuf))				/* found the terminal name?		*/
		{
			*tnum = term_ptr->termnum;					/* save the number			*/
			cur_term = *tnum;
			*flags = term_ptr->flags;					/* and the flags			*/
			cur_flags = *flags;
			return;								/* all done				*/
		}
		else if (!strcmp(term_ptr->termname,"_TDA0:"))				/* Save default TD device.		*/
		{
			td_num = term_ptr->termnum;
			td_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_TWA0:"))				/* Save default TW device.		*/
		{
			tw_num = term_ptr->termnum;
			tw_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_NDA0:"))				/* Save default ND device.		*/
		{
			nd_num = term_ptr->termnum;
			nd_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_QNA0:"))				/* Save default QN device.		*/
		{
			qn_num = term_ptr->termnum;
			qn_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_QTA0:"))				/* Save default QT device.		*/
		{
			qt_num = term_ptr->termnum;
			qt_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_NVA0:"))				/* Save default NV device.		*/
		{
			nv_num = term_ptr->termnum;
			nv_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_KTA0:"))				/* Save default KT device.		*/
		{
			kt_num = term_ptr->termnum;
			kt_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_TNA0:"))				/* Save default TN device.		*/
		{
			tn_num = term_ptr->termnum;
			tn_flags = term_ptr->flags;
		}
		else if (!strcmp(term_ptr->termname,"_FTA0:"))				/* Save default FT device.		*/
		{
			ft_num = term_ptr->termnum;
			ft_flags = term_ptr->flags;
		}
		term_ptr = (term_id *)term_ptr->next;
	} while (term_ptr);								/* go till none left to check		*/
											/* No match, check for pseudo-devs.	*/
	if (retbuf[1] == 'L' && retbuf[2] == 'T')					/* special case for LT devices		*/
	{
		lat_id *lat_ptr;

		get_lat_id(VINPUT_CHAN_NUM,latname,portname);

		lt_num = 255;								/* Set the default to 255.		*/
		lt_flags = 0;

		lat_ptr = get_lat_list();
		while (lat_ptr)
		{
			if (!lat_ptr->latname[0])					/* This is the default for unknowns.	*/
			{
				lt_num = lat_ptr->termnum;				/* i.e. LTA0: with no LAT_XXXX value.	*/
				lt_flags = lat_ptr->flags;
			}
			else if (!strcmp(latname,lat_ptr->latname))			/* Found it!.				*/
			{
				i = 0;
				j = 5;
				while(portname[j])					/* Get PORT_XXXX			*/
				{
					i = i * 10;					/* Shift left 1 digit			*/
					i = i + (portname[j++] - '0');			/* Get next digit.			*/
				}
				*tnum = i + lat_ptr->termnum;				/* Add amount to basic port number.	*/
				*flags = lat_ptr->flags;
				cur_term = *tnum;
				cur_flags = *flags;
				return;
			}
			lat_ptr = (lat_id *)lat_ptr->next;
		}
		*tnum = lt_num;
		*flags = lt_flags;
		cur_term = *tnum;
		cur_flags = *flags;
		return;
	}
	else if (retbuf[1] == 'T' && retbuf[2] == 'D')					/* special for TD devices.		*/
	{
		*tnum = td_num;
		*flags = td_flags;
	}
	else if (retbuf[1] == 'T' && retbuf[2] == 'W')					/* special for TW devices.		*/
	{
		*tnum = tw_num;
		*flags = tw_flags;
	}
	else if (retbuf[1] == 'N' && retbuf[2] == 'D')					/* special for ND devices.		*/
	{
		*tnum = nd_num;
		*flags = nd_flags;
	}
	else if (retbuf[1] == 'Q' && retbuf[2] == 'N')					/* special for QN devices.		*/
	{
		*tnum = qn_num;
		*flags = qn_flags;
	}
	else if (retbuf[1] == 'Q' && retbuf[2] == 'T')					/* special for QT devices.		*/
	{
		*tnum = qt_num;
		*flags = qt_flags;
	}
	else if (retbuf[1] == 'N' && retbuf[2] == 'V')					/* special for NV devices.		*/
	{
		*tnum = nv_num;
		*flags = nv_flags;
	}
	else if (retbuf[1] == 'K' && retbuf[2] == 'T')					/* special for KT devices.		*/
	{
		*tnum = kt_num;
		*flags = kt_flags;
	}
	else if (retbuf[1] == 'T' && retbuf[2] == 'N')					/* special for TN devices.		*/
	{
		*tnum = tn_num;
		*flags = tn_flags;
	}
	else if (retbuf[1] == 'F' && retbuf[2] == 'T')					/* special for FT devices.		*/
	{
		*tnum = ft_num;
		*flags = ft_flags;
	}
	else
	{
		*tnum = 255;								/* Not found so set to 255.		*/
		*flags = 0;
	}
	cur_term = *tnum;								/* Save what we know.			*/
	cur_flags = *flags;
}

	/* #ifdef VMS */

osd_mode(the_mode)									/* return the current execution mode,	*/
char *the_mode;										/* Foreground, Background		*/
{
#undef          ROUTINE
#define		ROUTINE		65600
	int4 retbuf;									/* buffer for return value		*/
	short length;									/* length of data returned		*/
	int4 retcod;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	mybuf.item_code = JPI$_MODE;							/* ask for the execution mode		*/
	mybuf.buflen = 4;								/* in a 4 byte buffer			*/
	mybuf.bufptr = (char *)&retbuf;							/* which is retbuf			*/
	mybuf.retlen = (unsigned short *)&length;					/* return length address		*/
	mybuf.endbuf = 0;								/* mark end of buffers			*/

	retcod = sys$getjpiw((long) 0, (long) 0, (long) 0, &mybuf, (long) 0, (long) 0, (long) 0);

	switch (retbuf)
	{
		case JPI$K_OTHER:
		{
			*the_mode = 'B';						/* other = background			*/
			break;
		}

		case JPI$K_NETWORK:
		{
			*the_mode = 'B';						/* network process = background		*/
			break;
		}

		case JPI$K_BATCH:
		{
			*the_mode = 'B';						/* batch process			*/
			break;
		}

		case JPI$K_INTERACTIVE:
		{
			*the_mode = 'F';						/* foreground process			*/
			break;
		}

		default:
		{
			*the_mode = 'B';						/* something weird, = background	*/
			break;
		}
	}
}

	/* #ifdef VMS */

osd_jname(the_name)									/* Return the job name (8) in background*/
char *the_name;
{
#undef          ROUTINE
#define		ROUTINE		65700
	char retbuf[40];								/* buffer for return value		*/
	short length;									/* length of data returned		*/
	int4 retcod;
	int4 flags;
	char the_mode;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	osd_mode(&the_mode);

	memset(the_name,' ',8);								/* If not batch mode, return spaces.	*/

	if (the_mode == 'B') 								/* Other wise get the name.		*/
	{
		flags = QUI$M_SEARCH_THIS_JOB;
		quibuf.item_code0 = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
		quibuf.buflen0 = 4;							/* in a 4 byte buffer			*/
		quibuf.bufptr0 = (char *)&flags;					/* which is flags.			*/
		quibuf.retlen0 = 0;							/* return length address		*/

		quibuf.item_code1 = QUI$_JOB_NAME;					/* Ask for the job name.		*/
		quibuf.buflen1 = 39;							/* Up to 39 bytes.			*/
		quibuf.bufptr1 = retbuf;						/* in retbuf.				*/
		quibuf.retlen1 = (unsigned short *)&length;				/* Store length.			*/

		quibuf.endbuf = 0;							/* mark end of buffers			*/
		retcod = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, &quibuf, (long) 0, (long) 0, (long) 0);

		if (retcod == SS$_NORMAL)						/* Got the name, copy it.		*/
		{
			if (length > 8) length = 8;					/* Only 8 chars though.			*/
			if (length) memcpy(the_name,retbuf,length);
		}
		else
		{
			printf("\n  SYSINF - OSD_JNAME error, status is %x (hex)  \n",retcod);
		}
	}
}

	/* #ifdef VMS */

get_lat_id(channel,latname,portname)							/* Find out the ID of the LAT on a chan	*/
unsigned short channel;
char latname[32],portname[32];
{
	struct                                                                                                            
	{										/* IO status block 			*/
		long status;
		long stat1;
	} iosb;

	uint4 status;
	int i,j;
	char buf[256];

	status = sys$qiow(0,channel,IO$_TTY_PORT|IO$M_LT_READPORT,&iosb,0,0,buf,100,0,0,0,0);	/* Ask driver for server, port */

	j=1;
	for(i=0; i<buf[0]; i++) portname[i] = buf[j++];					/* Copy portname (counted string)	*/
	portname[i] = '\0';
	j=0;
	for(i=buf[0]+2; j <buf[buf[0]+1]; i++) latname[j++] = buf[i];			/* Copy LAT name.			*/
	latname[j] = '\0';
}
#endif	/* #ifdef VMS */


/*
**	History:
**	$Log: sysinf.c,v $
**	Revision 1.10  1996/08/27 00:09:07  gsl
**	fix prototype
**	
**	Revision 1.9  1996-08-19 15:33:01-07  gsl
**	drcs update
**
**
**
*/
