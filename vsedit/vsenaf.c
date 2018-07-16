static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include <string.h>

#include "vwang.h"

#include "vseglb.h"
#include "vsescr.h"

static char naf_scr[1924];
static char naf_pfs[]="00X";
static char naf_func[]={03};
static char naf_lines[]={24};
static char naf_pfcode[3];
static char naf_status[3];

static VSESCR_FLDS(naf_flds) = {
{LEN(0)	ROW(9)	COL(18)	VALUE("Requested function is not currently available")},
{LEN(0)	ROW(11)	COL(28)	VALUE("Press (ENTER) to continue")},
{LASTITEM}
};

static void init_anaf(VSEFLD *flds);
static void vse_anaf(VSEFLD *flds);

void vse_naf(void)
{
	vse_anaf(naf_flds);
}

static void vse_anaf(VSEFLD *flds)
{
	memcpy(naf_scr,vse_default_oa,sizeof(vse_default_oa));
	vsescr_init(naf_scr);
	init_anaf(flds);
	vwang(naf_func,naf_scr,naf_lines,naf_pfs,naf_pfcode,naf_status);
}

static void init_anaf(VSEFLD *flds)
{
	vsescr(flds,naf_scr);
}

/*
**	History:
**	$Log: vsenaf.c,v $
**	Revision 1.10  1996/09/03 22:24:10  gsl
**	drcs update
**	
**
**
*/
