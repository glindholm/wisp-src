/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


/*
**	File:		rptsrt.h
**
**	Project:	KCSI
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
	}RPT_ASORT;

typedef	struct _sort_file{
	int _channel;		/*or vector below*/
	int _is_open;		/*2 = input 1 = output 0 = closed */
	int _rec_len;
	int _sort_rec_len;	/*_rec_len + length of the key area */
	char _name[64];
	char *_vector;
	int _key_len;
	}SORT_FILE;

void rpt_sort_clear(RPT_ASORT *sr);
int  rpt_sort_init(RPT_ASORT *sr,int rl);
void rpt_sort_squeeze(RPT_ASORT *srt);
void rpt_sort_release(char *r);
int  rpt_sort_return(char *rec);

void KCSI_call_ext_sort(RPT_ASORT *sr,KCSIO_BLOCK *kfb);
void KCSI_ext_sort_close(KCSIO_BLOCK *kfb);

#endif /* rptsrt_H */

/*
**	History:
**	$Log: rptsrt.h,v $
**	Revision 1.8  2003/02/17 22:05:58  gsl
**	Fix ambiguous SORT reference
**	
**	Revision 1.7  2003/02/05 15:50:11  gsl
**	Fix copyright headers
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
