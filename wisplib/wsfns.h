/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		wsfns.h
**
**	Purpose:	To define global stuff for WSFNS routine.
**
**
**	History:
**	12/13/94	Written by SMC
**
*/

#ifndef WSFNS_H
#define WSFNS_H

int WSFNM(char* z1_fn, ...);
int WSFNS(char* z1_fn, ...);

void WLNC_check_first_time_netroncap();
void WLNC_set_toggle_value();
void WLNC_hfio();
int  WLNC_use_netroncap(void);

#endif /* WSFNS_H */
/*
**	History:
**	$Log: wsfns.h,v $
**	Revision 1.10  2003/02/24 19:59:54  gsl
**	fix protos
**	
**	Revision 1.9  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.8  2002/07/10 21:05:37  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.7  1996/08/19 22:33:24  gsl
**	drcs update
**	
**
**
*/
