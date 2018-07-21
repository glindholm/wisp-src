/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


#include <stdio.h>
#include <string.h>

#include "vwang.h"

#include "vseglb.h"
#include "vsescr.h"

static unsigned char naf_scr[1924];
static char naf_pfs[]="00X";
static unsigned char naf_func[]={03};
static unsigned char naf_lines[]={24};
static char naf_pfcode[3];
static unsigned char naf_status[3];

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
**	Revision 1.12  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.11  2002/08/01 15:31:11  gsl
**	type warnings
**	
**	Revision 1.10  1996/09/03 22:24:10  gsl
**	drcs update
**	
**
**
*/
