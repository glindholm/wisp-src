/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/

/**
 **																*
 **	acp.h	Header file for (WANG) Asynchronous Communications Program.							*
 **
 ** 																*
 **/
#ifndef ACP_H
#define ACP_H

#ifdef unix

#include <errno.h>
#include <unistd.h>
#include <termio.h>

#define BLOCKING 0
#define NONBLOCKING 1

#define		ACPMAXDEV	6					/* Maximum number of devices to allocate.		*/
#define		ACPMAXRECLEN	2048					/* Maximum length of I/O buffer.			*/

typedef struct	{							/* Structure representing records in file.		*/
			char			acp_termname[16];	/* Name given to terminal line.				*/
			char			acp_termnum[64];	/* VAX terminal id.					*/
			char			acp_weorseq[4];		/* EOR sequence for writeacp.				*/
			char			acp_reorseq[3][4];	/* Three EOR sequences for readacp.			*/
			struct	acp_term_id	*next;			/* Pointer to next element in linked list.		*/
		} acp_term_id;

#ifndef INIT_ACP
#define INIT_ACP extern							/* Declare variables as extern with no initialization.	*/
#endif /* INIT_ACP */
INIT_ACP	acp_term_id	*acp_term_list;				/* Pointer to linked list of file records.		*/
INIT_ACP	char		*acp_term[ACPMAXDEV];			/* Static array to contain up to ACPMAXDEV line numbers.*/
INIT_ACP	char		acp_weor[ACPMAXDEV][4];			/* Static array to contain write EOR sequence.		*/
INIT_ACP	char		acp_reor[ACPMAXDEV][3][4];		/* Static array to contain read EOR sequences.		*/
INIT_ACP	short		acp_ch[ACPMAXDEV];			/* Static array to contain I/O channels.		*/
INIT_ACP	short		acp_iosb[ACPMAXDEV][4];			/* Static array to contain I/O status blocks.		*/
INIT_ACP	int4		acp_ef[ACPMAXDEV];			/* Static array to contain event flags.			*/
INIT_ACP	int		acp_allocated;				/* Number of lines allocated.				*/
INIT_ACP	int		acp_blockmode[ACPMAXDEV];		/* Mode of line : blocking vs. nonblocking		*/
INIT_ACP        char            *acp_devname[ACPMAXDEV];
INIT_ACP	struct termio acp_oterm;
INIT_ACP	struct termio acp_nterm;
INIT_ACP	acp_term_id acp_term_struct;





     
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

#endif /* unix */

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


void OPENACP(
	char	*destination,	/* WANG terminal name, up to 16 chars.			*/
	int4	*rel_line,	/* Index from 1 to 6 for later access to acp_term[].	*/
	int4	*ret_code);	/* WANG ACP return code.				*/

void SETACP(struct acpinfo_cbl *info, int *ret);
void GETACP(struct acpinfo_cbl *info,int *ret);

#endif /*ACP_H*/
/*
**	History:
**	$Log: acp.h,v $
**	Revision 1.16  2003/02/05 15:54:49  gsl
**	oops
**	
**	Revision 1.15  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.14  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.13  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.12  2002/07/11 14:41:07  gsl
**	Fix acp getterm()
**	
**	Revision 1.11  2002/07/11 14:33:19  gsl
**	Cleanup ACP globals
**	
**	Revision 1.10  2002/06/26 01:42:44  gsl
**	Remove VMS code
**	
**	Revision 1.9  1996/10/09 00:18:07  gsl
**	Removed unneeded defines and moved cfgpath[] to openacp.c
**	
**	Revision 1.8  1996-08-19 15:32:07-07  gsl
**	drcs update
**
**
**
*/
