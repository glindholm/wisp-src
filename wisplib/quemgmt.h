/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/*
**	File:		quemgmt.h
**
**	Purpose:	To define stuff for queue management routines.
**
**	History:
**	10/21/94	Written by SMC
**
*/

#ifndef QUEMGMT_H
#define QUEMGMT_H

#include <ctype.h>
#include <time.h>
#include <ssdef.h>
#include "quidef.h"                    
#include <jbcmsgdef.h>
#include <sjcdef.h>
#include <descrip.h>
#include <stdio.h>
#include <math.h>
#include <rmsdef.h>
#include <libdef.h>

#include "werrlog.h"
#include "video.h"
#include "vlocal.h"
#include "vdata.h"

#include "vwang.h"
#include "scnfacs.h"
#include "que_jobs.h"
#include "wperson.h"

struct qui_itmlst_struct {								/* $GETQUI itmlst parameter structure.	*/
				short unsigned int	buflen;				/* The length of the buffer.		*/
				short unsigned int	item_code;			/* The code for the request to GETDVI	*/
				long			*bufptr;			/* A pointer to the buffer.		*/
				long unsigned int	*retlen;			/* The return length of the buffer.	*/
			};

#define ANY_QUEUE	1
#define BATCH_QUEUE	2
#define PRINT_QUEUE	3

#define MENU_LIST	1
#define PQUEUE_LIST	2
#define BQUEUE_LIST	3
#define EMPTY_LIST	4

#define SEARCH_BATCH_ENABLED	0x00001000						/* Flag to allow access to batch queues.*/
#define SEARCH_GENERIC_ENABLED	0x00002000						/* Flag to allow access to generic queues.*/
#define SEARCH_OUTPUT_ENABLED	0x00004000						/* Flag to allow access to output queues.*/
#define RESTRICT_JOBS_DISABLED 	0x00008000						/* Disable restriction access to jobs 	*/
											/*  owned by user.			*/
#endif /* QUEMGMT_H */
/*
**	History:
**	$Log: quemgmt.h,v $
**	Revision 1.10  1996-08-19 18:32:44-04  gsl
**	drcs update
**
**
**
*/
