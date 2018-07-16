/*
**	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
**
**	Project:	WPROC
**	Id:		$Id:$
**	RCS:		$Source:$
**	
**
** Copyright (c) Lexical Software, 1992.  All rights reserved.
**
** Module : video.h
** Author : Janice Kenyon
** Date   : 27 May 1992
*/

#if WANG

#ifndef VIDEO_RT__H
#define VIDEO_RT__H

#ifdef __cplusplus
extern "C" {
#endif

#include "video.h"
#include "vlocal.h"
#include "vdata.h"


char vcheck();

int verase(int control);

void vexit();

void vrawtimeout(int time);
int  vrawtimeout_check();
void vrawtimeout_clear();

char vgetc();

int vgetm();

int vline(int type, int length);

int vmode(int control);

int vmove(int line, int column);

int vonexit(int option);

int vprint(char *text, ...);

int vrefresh(int what);

int vrss(unsigned char *region);

int vset(int item, int state);

unsigned char *vsss(int row, int col, int rows, int cols);

int vstate(int action);

int vtext(int display, int row, int column, char *text, ...);

#ifdef __cplusplus
}
#endif

#endif
#endif


/*
**	History:
**	$Log: video_rt.h,v $
**	Revision 1.9  1998/08/31 19:50:40  gsl
**	drcs update
**	
**	Revision 1.8  1998-08-31 15:14:27-04  gsl
**	drcs update
**
**	----------------------------
**	revision 1.7
**	date: 1997-07-08 15:56:09-04;  author: gsl;  state: V4_3_00;  lines: +0 -6
**	remove redefined routines which are in video.h
**	----------------------------
**	revision 1.6
**	date: 1996-11-13 17:00:38-05;  author: gsl;  state: V4_0_00;  lines: +3 -3
**	Change vtimeout vrawtimeout
**	----------------------------
**	revision 1.5
**	date: 1996-07-25 19:48:29-04;  author: gsl;  state: Exp;  lines: +2 -6
**	NT
**	----------------------------
**	revision 1.4
**	date: 1995-04-25 06:00:34-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
**	drcs state V3_3_15
**	----------------------------
**	revision 1.3
**	date: 1995-04-17 07:52:48-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
**	drcs state V3_3_14
**	----------------------------
**	revision 1.2
**	date: 1995-01-27 18:33:35-05;  author: gsl;  state: V3_3x12;  lines: +4 -0
**	drcs load
**	----------------------------
**	revision 1.1
**	date: 1995-01-27 16:51:34-05;  author: gsl;  state: V3_3c;
**	drcs load
**	=============================================================================
*/
