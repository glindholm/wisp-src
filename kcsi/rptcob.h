/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
The block passed in RPT-OPTIONS (WSRPTOPT)
------*/
#define	RPT_ID		NULL,0,8,ACHR,0
#define	RPT_DATE	NULL,8,8,ACHR,0
#define	RPT_DEVICE	NULL,16,8,ACHR,0
#define	RPT_FILES	NULL,24,3,BYES,0
#define	RPT_OPTION	NULL,27,3,BYES,0
#define	RPT_ONLY	NULL,30,3,BYES,0
#define	RPT_PAGE	NULL,33,2,ACHR,0
#define	RPT_LINE_1	NULL,35,1,ACHR,0
#define	RPT_LINE_2	NULL,36,1,ACHR,0
#define	RPT_LINE_3	NULL,37,1,ACHR,0
#define	RPT_SPACING_1	NULL,38,1,ACHR,0
#define	RPT_SPACING_2	NULL,39,1,ACHR,0
#define	RPT_SPACING_3	NULL,40,1,ACHR,0
#define	RPT_EXT_SORT	NULL,41,3,BYES,0

/*----
1 byte flags from the rhd-record
------*/
#define	RHD_SEC_FILE		NULL,13,1,ACHR,0
#define	RHD_SORT		NULL,14,1,ACHR,0
#define	RHD_DATA_LIMIT		NULL,15,1,ACHR,0
#define	RHD_CONTROL_FIELDS	NULL,16,1,ACHR,0
#define	RHD_COLUMN_SUBS		NULL,17,1,ACHR,0

/*----
From the RDF (DATA FILES RECORD) we only need the key to the secondary
file.
------*/
#define	RDF_KEY_TO_SEC		NULL,57,8,ACHR,0
#define	RDF_KEY_OCCURRENCE	NULL,65,2,ACHR,0

/*----
These fields occur as sub fields within NF DATA
------*/
#define	NF_LIT_20		NULL,0,20,BLIT,0
#define	NF_OP_NAME		NULL,0,8,ACHR,0
#define	NF_OP_OCCURRENCE	NULL,8,4,BIDX,0
#define	NF_OP_ORIGIN		NULL,12,1,ACHR,0
#define	NF_OP_FILLER		NULL,13,6,ACHR,0
#define	NF_NOT_LIT_CODE		NULL,19,1,ACHR,0
#define	NF_OP_CODE		NULL,20,1,ACHR,0

#define	NF_OP_ENTRY_LEN		21

#define	NF_NAME			NULL,0,8,ACHR,0
#define	NFO_ENTRIES		NULL,8,210,ACHR,0
#define	NF_TYPE			NULL,218,1,BCTP,0
#ifdef	NOPACK
#define	NF_LEN			NULL,219,2,ACHR,0
#else
#define	NF_LEN			NULL,219,2,APCK,0
#endif
#define	NF_DEC			NULL,221,1,ACHR,0

#define	NF_ENTRY_LEN		222
#define	NFO_ENTRY_POS		8

/*---
From the two control break records RCD and RCB
------*/
#define	RTT_TITLE		NULL,13,60,ACHR,0

#define	RTT_ENTRY_LEN		73

/*---
From the two control break records RCD and RCB
------*/
#define	RCB_NAME		NULL,0,8,ACHR,0
#define	RCB_OCCURRENCE		NULL,8,2,ACHR,0
#define	RCB_ACTION_CODE		NULL,10,2,BYES,0
#define	RCB_ORIGIN		NULL,12,1,ACHR,0
#define	RCB_REPEAT_CODE		NULL,13,1,ACHR,0

#define	RCB_ENTRY_LEN		14

#define	RCD_BREAK		NULL,13,40,ACHR,0
#define	RCD_COL			NULL,53,3,ACHR,0

#define	RCD_ENTRY_LEN		56

/*----
For the report fields
------*/
#define	RFL_LINE		NULL,0,1,ACHR,0		/* X = 3 Y = 2 Z = 1*/
#define	RFL_SEQ			NULL,1,2,ACHR,0
#define RFL_NAME_POS		3
#define	RFL_NAME		NULL,RFL_NAME_POS,8,ACHR,0
#define	RFL_OCCURRENCE_POS	11
#define	RFL_OCCURRENCE		NULL,RFL_OCCURRENCE_POS,2,ACHR,0
#define RFL_COL_HEAD_POS	13
#define	RFL_COL_HEAD		NULL,RFL_COL_HEAD_POS,25,ACHR,0
#define	RFL_COL_SUB_HEAD	NULL,38,25,ACHR,0
#ifdef	NOPACK
#define	RFL_SPACES		NULL,63,2,ACHR,0	/*99 COMP*/
#define	RFL_EXT_SIZE		NULL,65,2,ACHR,0	/*999 COMP*/
#else
#define	RFL_SPACES		NULL,63,2,APCK,0	/*99 COMP*/
#define	RFL_EXT_SIZE		NULL,65,2,APCK,0	/*999 COMP*/
#endif
#define	RFL_ZERO_CHAR		NULL,67,1,ACHR,0	
#define	RFL_ZERO_COUNT		NULL,68,1,ACHR,0
#define	RFL_SIGN_CONTROL	NULL,69,2,ACHR,0	/* XX SP 9 DB CR*/
#define	RFL_DEC_CARRY		NULL,71,1,ACHR,0	/* X 0 - 9 or SPACE */
#define	RFL_INSERT_1_COUNT	NULL,72,1,ACHR,0
#define	RFL_INSERT_1_CHAR	NULL,73,1,ACHR,0	/* 3 x XX */
#define	RFL_INSERT_2_COUNT	NULL,74,1,ACHR,0
#define	RFL_INSERT_2_CHAR	NULL,75,1,ACHR,0
#define	RFL_INSERT_3_COUNT	NULL,76,1,ACHR,0
#define	RFL_INSERT_3_CHAR	NULL,77,1,ACHR,0
#define	RFL_DOLLAR_SIGN		NULL,78,1,ACHR,0	/* 0=none 1=fix 2=flt*/
#define	RFL_COMMA		NULL,79,1,ACHR,0
#define	RFL_TOTAL		NULL,80,1,BLNK,0	/* X = yes */
#define	RFL_MAX			NULL,81,1,BLNK,0
#define	RFL_MIN			NULL,82,1,BLNK,0
#define	RFL_AVG			NULL,83,1,BLNK,0
#define	RFL_ORIGIN_POS		84
#define	RFL_ORIGIN		NULL,RFL_ORIGIN_POS,1,ACHR,0

#define	RFL_ENTRY_LEN		85

/*----
Record layout for the control records.
------*/
#define	CRF_NAME		NULL,2,8,ACHR,0
#define	CRF_TYPE		NULL,10,1,BCTP,0
#define	CRF_LEN			NULL,11,3,ACHR,0
#define	CRF_POS			NULL,14,4,ACHR,0
#define	CRF_OCCURRENCES		NULL,18,2,ACHR,0
#define	CRF_DEC			NULL,29,1,ACHR,0

#define	CRF_ENTRY_LEN		130
/*----
These fields aoccur as sub fields within DL DATA
------*/
#define	DLO_OP_CODE_POS		0
#define	DL_OP_CODE		NULL,DLO_OP_CODE_POS,2,ACHR,0
#define	DLO_LIT_POS		2
#define	DL_LIT_20		NULL,DLO_LIT_POS,20,BLIT,0
#define	DLO_NAME_POS		2
#define	DL_OP_NAME		NULL,DLO_NAME_POS,8,ACHR,0
#define DLO_OCCURRENCE_POS	10
#define	DL_OP_OCCURRENCE	NULL,DLO_OCCURRENCE_POS,4,BIDX,0
#define	DLO_ORIGIN_POS		14
#define	DL_OP_ORIGIN		NULL,DLO_ORIGIN_POS,1,ACHR,0
#define	DL_OP_FILLER		NULL,15,6,ACHR,0
#define	DLO_NOT_LIT_CODE_POS	21
#define	DL_NOT_LIT_CODE		NULL,DLO_NOT_LIT_CODE_POS,1,ACHR,0
#define	DLO_CONNECTOR_POS	22
#define	DL_OP_CONNECTOR		NULL,DLO_CONNECTOR_POS,1,ACHR,0

#define	DL_OP_ENTRY_LEN		23
#define	DLO_ENTRY_LEN		DL_OP_ENTRY_LEN

#define	DL_NAME_POS		0
#define	DL_NAME			NULL,DL_NAME_POS,8,ACHR,0
#define	DL_OCCURRENCE_POS	8
#define	DL_OCCURRENCE		NULL,DL_OCCURRENCE_POS,2,ACHR,0
#define	DL_ORIGIN_POS		10
#define	DL_ORIGIN		NULL,DL_ORIGIN_POS,1,ACHR,0
#define	DLO_ENTRIES		NULL,11,230,ACHR,0
#define DL_SET_CONNECTOR_POS	241
#define	DL_SET_CONNECTOR	NULL,DL_SET_CONNECTOR_POS,1,ACHR,0

#define	DL_ENTRY_LEN		242
#define	DLO_ENTRY_POS		11

/*---
Sorts
------*/
#define	SRT_NAME		NULL,0,8,ACHR,0
#define	SRT_OCCURRENCE		NULL,8,2,ACHR,0
#define	SRT_ORDER		NULL,10,1,ACHR,0

#define	SRT_ENTRY_LEN		11


/*
**	History:
**	$Log: rptcob.h,v $
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.3  1996/09/17 23:34:17  gsl
**	drcs update
**	
**
**
*/
