/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef DATCOBH
#define	DATCOBH
/*----
Breakout of the passed COBOL fields.
------*/
#include "dtype.h"

#define	T1_ENTRY_LEN		64
#define	T1_INT_LEN		NULL,0,3,ACHR,0
#define	T1_EXT_LEN		NULL,3,3,ACHR,0
#define	T1_START_POS		NULL,9,4,ACHR,0
#define	T1_OCCURRENCES		NULL,13,2,ACHR,0
#define	T1_ZERO_SUPPRESS	NULL,15,1,ACHR,0
#define	T1_SIGN			NULL,17,1,ACHR,0
#define	T1_DOLLAR_COMMA		NULL,18,1,ACHR,0
#define	T1_UPDATE		NULL,20,1,ACHR,0
#define	T1_DEC			NULL,21,1,ACHR,0
#define	T1_BIN			NULL,22,1,ACHR,0
#define	T1_SEQ			NULL,23,2,ACHR,0
#define	T1_DISPLAY		NULL,32,1,ACHR,0

#define	T2_ENTRY_LEN		96
#define	T2_NAME			NULL,0,8,ACHR,0
#define	T2_TYPE			NULL,8,1,ACHR,0
#define	T2_VALIDATION		NULL,9,2,ACHR,0
#define	T2_TABLE_NAME		NULL,11,6,ACHR,0
#define	T2_LO_RANGE		NULL,17,16,ACHR,0
#define	T2_HI_RANGE		NULL,33,16,ACHR,0
#define	T2_CUMMULATIVE_NAME	NULL,49,8,ACHR,0
#define	T2_DEFAULT_FAC		NULL,88,1,ACHR,0

#define	CH_PRIMARY_KEY_POS	11
#define	CH_PRIMARY_KEY_LEN	8
#define	CA_TABLE_OFF		82
#define	CA_TABLE_LEN		82
#define	CA_ENTRY_OFF		0
#define	CA_ENTRY_LEN		9
#define	CA_KEY_NAME_OFF		0
#define	CA_KEY_NAME_LEN		8
#define	CA_DUPS_OFF		8
#define CA_DUPS_LEN		1
#define CH_SPACING_POS		47
#define CH_SPACING_LEN		1

#define CH_SPACING		NULL,CH_SPACING_POS,CH_SPACING_LEN,ACHR,0


#endif
/*
**	History:
**	$Log: datcob.h,v $
**	Revision 1.4  1996-09-17 19:34:04-04  gsl
**	drcs update
**
**
**
*/
