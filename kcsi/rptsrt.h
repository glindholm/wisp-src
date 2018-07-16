/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		rptsrt.h
**
**	Project:	KCSI
**
**	RCS:		$Source:$
**
**	Purpose:	Generic prototype header
**
*/

#ifndef rptsrt_H
#define rptsrt_H
/*
**	Includes
*/
#include "kcsio.h"

#define	MAX_SORTS	8
#define	MAX_KEY		132
#define	MAX_LEN		3090

typedef struct _asort{
	int _pos;
	int _len;
	int _order;		/*D = decending A = acending*/
	int _numeric;		/*1 = numeric (sign leading) 0 = char */
	int _type;		/* As in WISPSORT */
	}SORT;

typedef	struct _sort_file{
	int _channel;		/*or vector below*/
	int _is_open;		/*2 = input 1 = output 0 = closed */
	int _rec_len;
	int _sort_rec_len;	/*_rec_len + length of the key area */
	char _name[64];
	char *_vector;
	int _key_len;
	}SORT_FILE;

void rpt_sort_clear(SORT *sr);
int  rpt_sort_init(SORT *sr,int rl);
void rpt_sort_squeeze(SORT *srt);
void rpt_sort_release(char *r);
int  rpt_sort_return(char *rec);

void KCSI_call_ext_sort(SORT *sr,KCSIO_BLOCK *kfb);
void KCSI_ext_sort_close(KCSIO_BLOCK *kfb);

#endif /* rptsrt_H */

/*
**	History:
**	$Log: rptsrt.h,v $
**	Revision 1.4.2.1  2002/11/12 15:56:35  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.6  2002/10/24 15:48:31  gsl
**	Make globals unique
**	
**	Revision 1.5  2002/10/23 20:39:06  gsl
**	make global name unique
**	
**	Revision 1.4  1996/09/17 23:34:18  gsl
**	drcs update
**	
**
**
*/
