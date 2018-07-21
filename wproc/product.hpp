//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : product.hpp
// Author : George Soules
// Date   : 12 February 1991

#ifndef PRODUCT__HPP
#define PRODUCT__HPP

// Classes
// (none)

// Definitions and subprograms
#include "environ.hpp"


char *product_copyright();

usign_16 product_version();
char *product_version_str(void);

char *product_name();

#if DEMO
char *demo_notice();
#endif

#endif

/*
**	History:
**	$Log: product.hpp,v $
**	Revision 1.7  1998/08/31 19:50:36  gsl
**	drcs update
**	
**	Revision 1.6  1998-08-31 15:14:09-04  gsl
**	drcs update
**
**	Revision 1.5  1995-10-18 13:19:52-04  gsl
**	add product_version_str()
**
**
*/

//
//	History:
//	$Log: product.hpp,v $
//	Revision 1.7  1998/08/31 19:50:36  gsl
//	drcs update
//	
//	Revision 1.6  1998-08-31 15:14:09-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/product.hpp,v
//	Working file: product.hpp
//	head: 1.5
//	branch:
//	locks: strict
//	access list:
//		gsl
//		scass
//		ljn
//		jockc
//		jlima
//	symbolic names:
//	keyword substitution: kv
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1995-10-18 13:19:52-04;  author: gsl;  state: V4_3_00;  lines: +11 -0
//	add product_version_str()
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:17-04;  author: gsl;  state: V3_3_18;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:33-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:16-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:22-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
