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

void sort_clear(SORT *sr);
int sort_init(SORT *sr,int rl);
void sort_squeeze(SORT *srt);
void sort_release(char *r);
int sort_return(char *rec);

void call_ext_sort(SORT *sr,KCSIO_BLOCK *kfb);
void ext_sort_close(KCSIO_BLOCK *kfb);

#endif /* rptsrt_H */

/*
**	History:
**	$Log: rptsrt.h,v $
**	Revision 1.4  1996-09-17 19:34:18-04  gsl
**	drcs update
**
**
**
*/
