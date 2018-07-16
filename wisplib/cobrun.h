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

#ifndef COBRUN_DEF
#define COBRUN_DEF

#ifdef  EXT_COBRUN
#define EXT_COBRUN_DEF
#define INIT_COBRUN   =0
#else
#define EXT_COBRUN_DEF extern
#define INIT_COBRUN
#endif

#define COBOL_OTHER	0
#define COBOL_VAX	1
#define COBOL_LPI	2
#define COBOL_ACU	3
#define COBOL_AIX	4
#define COBOL_MF	5

EXT_COBRUN_DEF int	vax_cobol 	INIT_COBRUN;
EXT_COBRUN_DEF int	lpi_cobol 	INIT_COBRUN;
EXT_COBRUN_DEF int	acu_cobol	INIT_COBRUN;
EXT_COBRUN_DEF int	aix_cobol 	INIT_COBRUN;
EXT_COBRUN_DEF int	mf_cobol 	INIT_COBRUN;
EXT_COBRUN_DEF int	run_cobol	INIT_COBRUN;

#define FILES_OTHER	0
#define FILES_CISAM	1
#define	FILES_VISION	2
#define FILES_RMS	3
#define FILES_AIX	4

EXT_COBRUN_DEF int	cisam_files	INIT_COBRUN;
EXT_COBRUN_DEF int	vision_files	INIT_COBRUN;
EXT_COBRUN_DEF int	rms_files	INIT_COBRUN;
EXT_COBRUN_DEF int	aix_files	INIT_COBRUN;
EXT_COBRUN_DEF int	run_files	INIT_COBRUN;


#undef EXT_COBRUN_DEF
#undef INIT_COBRUN

#endif
