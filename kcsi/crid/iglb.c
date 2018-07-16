static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "kcsifunc.h"

static char sccsid[]="@(#)iglb.c	1.2 1/27/93";

char inq_tokens[81];
int inq_token;
int inq_token_idx;
int dline_idx,sline_idx,didx,sidx;

char *inq_inqs,*inq_inqd,*inq_msg,*inq_rfl,*inq_dl,*next_inq_rfl,*next_inq_dl;

int inq_rfl_idx,inq_dl_idx,inq_dlo_idx;


void vax_inq_globals()
{
	/* Dummy routine for VAX linker */
}


/*
**	History:
**	$Log: iglb.c,v $
**	Revision 1.3  1996-09-17 19:45:37-04  gsl
**	drcs update
**
**
**
*/
