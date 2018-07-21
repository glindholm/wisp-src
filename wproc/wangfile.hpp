//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	
//
#ifndef WANGFILE__HPP
#define WANGFILE__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"

#if WANG
class wang_filename : public object {
   public:
      wang_filename(const char *s);
      char filename[WANG_FILENAME_SIZE + 1];
      char libname [WANG_LIBNAME_SIZE  + 1];
      char volname [WANG_VOLNAME_SIZE  + 1];
};
#endif

#endif

//
//	History:
//	$Log: wangfile.hpp,v $
//	Revision 1.4  1998/08/31 19:14:29  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/wangfile.hpp,v
//	Working file: wangfile.hpp
//	head: 1.3
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
//	total revisions: 3;	selected revisions: 3
//	description:
//	----------------------------
//	revision 1.3
//	date: 1995-04-25 06:00:36-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.2
//	date: 1995-04-17 07:52:50-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 18:33:37-05;  author: gsl;  state: V3_3x12;
//	drcs load
//	=============================================================================
