// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : video.h
// Author : Janice Kenyon
// Date   : 27 May 1992

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

