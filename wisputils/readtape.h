/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
**
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
**
******************************************************************************
*/

struct vol_rec {
	char header[4];
	char label[6];
	char access;
	char _dummy1[20];
	char _dummy2[6];
	char owner[14];
	char _dummy3[28];
	char ansi_level;
};
struct header1 {
	char header[4];
	char name[17];
	char fileset[6];
	char volume_num[4];
	char file_num[4];
	char gen[4];
	char genver[2];
	char created[6];	
	char expires[6];	
	char access;
	char blockcount[6];
	char tapesys[13];
	char _dummy[7];
};
struct header2 {
	char header[4];
	char recfmt;
	char blocklen[5];
	char reclen[5];
	char density;	
	char vol_switch;	
	char job[17];
	char recording[2];
	char carriage_control;	
	char alignment;
	char blocked_records;
	char _dummy1[11];
	char block_offset[2];
	char _dummy2[28];
};
#define VOLHEAD "VOL1"
#define F1HEAD "HDR1"
#define F2HEAD "HDR2"
#define F1EOF "EOF1"
#define F2EOF "EOF2"

struct vol_rec svol;
struct header1 shead1;
struct header2 shead2;
/*
**	History:
**	$Log: readtape.h,v $
**	Revision 1.8  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.7  1996/07/23 18:12:58  gsl
**	drcs update
**	
**
**
*/
