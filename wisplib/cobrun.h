/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	cobrun.h	Defines conditional on what COBOL this wisplib will link with.
*/

#ifndef COBRUN_H
#define COBRUN_H

#ifdef  EXT_COBRUN
#define EXT_COBRUN_DEF
#define INIT_ZERO   = 0
#else
#define EXT_COBRUN_DEF extern
#define INIT_ZERO
#endif

#define COBOL_OTHER	0
#define COBOL_VAX	1
#define COBOL_LPI	2
#define COBOL_ACU	3
#define COBOL_AIX	4
#define COBOL_MF	5

EXT_COBRUN_DEF int	vax_cobol 	INIT_ZERO;
EXT_COBRUN_DEF int	lpi_cobol 	INIT_ZERO;
EXT_COBRUN_DEF int	acu_cobol	INIT_ZERO;
EXT_COBRUN_DEF int	aix_cobol 	INIT_ZERO;
EXT_COBRUN_DEF int	mf_cobol 	INIT_ZERO;
EXT_COBRUN_DEF int	run_cobol	INIT_ZERO;

#undef EXT_COBRUN_DEF
#undef INIT_ZERO

#endif /* COBRUN_H */
/*
**	History:
**	$Log: cobrun.h,v $
**	Revision 1.8  1996-08-19 18:32:14-04  gsl
**	drcs update
**
**
**
*/
