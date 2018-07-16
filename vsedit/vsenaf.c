#include <stdio.h>
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

static VSESCR_FLDS(cnaf_flds) = {
{LEN(0)	ROW(9)	COL(16)	VALUE("Create not available when editing existing files")},
{LEN(0)	ROW(11)	COL(28)	VALUE("Press (ENTER) to continue")},
{LASTITEM}
};
static VSESCR_FLDS(snaf_flds) = {
{LEN(0)	ROW(9)	COL(16)	VALUE("Save not available when editing new files")},
{LEN(0)	ROW(11)	COL(28)	VALUE("Press (ENTER) to continue")},
{LASTITEM}
};
static VSESCR_FLDS(enaf_flds) = {
{LEN(0)	ROW(9)	COL(22)	VALUE("Requested function is only available")},
{LEN(0) ROW(11) COL(25)	VALUE("from the EDIT-DISPLAY screen.")},
{LEN(0)	ROW(13)	COL(25)	VALUE("Press (ENTER) to continue. . .")},
{LASTITEM}
};

vse_naf()
{
	vse_anaf(naf_flds);
}
	
vse_enaf()
{
	vse_anaf(enaf_flds);
}
	
vse_cnaf()
{
	vse_anaf(cnaf_flds);
}
	
vse_snaf()
{
	vse_anaf(snaf_flds);
}

vse_anaf(flds)
VSEFLD *flds;
{
	memcpy(naf_scr,vse_default_oa,sizeof(vse_default_oa));
	vsescr_init(naf_scr);
	init_anaf(flds);
	vwang(naf_func,naf_scr,naf_lines,naf_pfs,naf_pfcode,naf_status);
}

init_anaf(flds)
VSEFLD *flds;
{
	vsescr(flds,naf_scr);
}

