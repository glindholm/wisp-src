/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

#include "dtyp.h"

#define	DTYPE 	DTYP
#define	IS_NUM	D_NUMERIC
#define	ACHR	D_CHARACTER
#define	NLIT	D_NUM_DELIMITED
#define	BLIT	D_NUM_DELIMITED
#define	BIDX	D_INDEX_TYPE
#define	BZON	D_UNZONED_SL
#define CINT	D_INT_SIGNED
#define	APCK	D_PACKED
#define	BTRN	D_TRUNC_STRING
#define	CSTR	D_C_STRING
#define AUNS	D_ASCII_UNSIGN
#define	BUNS	D_ASCII_UNSIGN
#define	AZON	D_UNZONED_SL
#define	ABIN	D_WANG_BINARY
#define	TZON	D_EDITED_ST
#define	DUNS	D_UNSIGNED_DISPLAY
#define	DHEX	D_HEX

#define	BUNS_LEN	D_UNSIGN_LEN
#define	BZON_LEN	D_UNZONED_LEN

#define	D_YES	0x00000001
#define	BYES	D_SPECIAL|D_YES

#define	D_BLANK	0x00000002
#define	BLNK	D_SPECIAL|D_BLANK

#define	D_ONE	0x00000003
#define	BONE	D_SPECIAL|D_ONE

#define	D_POS	0x00000004
#define	BPOS	D_SPECIAL|D_POS

#define	D_CCHR	0x00000005
#define	CCHR	D_SPECIAL|D_CCHR

#define	D_BCHR	0x00000006
#define	BCHR	D_SPECIAL|D_BCHR

#define	D_CONTROL_TYPE	0x00000007
#define	BCTP	D_SPECIAL|D_CONTROL_TYPE

#define	D_DTYPE_TYPE	0x00000008
#define	BDTP	D_SPECIAL|D_DTYPE_TYPE



/*
**	History:
**	$Log: dtype2.h,v $
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.3  1996/09/17 23:34:07  gsl
**	drcs update
**	
**
**
*/
