/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

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
**	Revision 1.5  1995-10-18 13:19:52-04  gsl
**	add product_version_str()
**
**
*/
