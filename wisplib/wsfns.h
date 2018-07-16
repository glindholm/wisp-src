/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
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

int WSFNM();
int WSFNS();

void check_first_time_netroncap();
void set_toggle_value();
extern void hfio();
int use_netroncap(void);

#endif /* WSFNS_H */
/*
**	History:
**	$Log: wsfns.h,v $
**	Revision 1.7  1996/08/19 22:33:24  gsl
**	drcs update
**	
**
**
*/
