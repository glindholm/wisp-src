//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : ext.hpp
// Author : George Soules
// Date   : 4 March 1991

#ifndef EXT__HPP
#define EXT__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


// This is the only place that file extensions should be explicitly coded

#if DOS
#define PCODE_FILE_EXT  ".RUN"
#define SRC_FILE_EXT    ".SRC"
#else
#if WANG
#define PCODE_FILE_EXT  ".wpr"
#define SRC_FILE_EXT    ".wps"
#endif
#endif
#define BAT_FILE_EXT    ".BAT"
#define COM_FILE_EXT    ".COM"
#define EXE_FILE_EXT    ".EXE"

#endif


//
//	History:
//	$Log: ext.hpp,v $
//	Revision 1.6  1998/08/31 19:13:48  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/ext.hpp,v
//	Working file: ext.hpp
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
//	date: 1996-07-25 19:46:08-04;  author: gsl;  state: V4_3_00;  lines: +1 -6
//	NT
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:57-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:15-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:53-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:08-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
