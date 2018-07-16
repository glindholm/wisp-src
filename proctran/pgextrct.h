/* Copyright (c) 1988-1997 NeoMedia Technologies Inc., All rights reserved. */
/* $Id:$ */
/* PGEXTRCT.H															*/
/*		 Initialize the Extract specific keyword arrays									*/

#define	FORM	1
#define JOBLMT	5
#define LINES	7
#define PRNTER	11
#define SPLSRR	22
#define WS	30

#ifdef INIT_COMMON

EXT char *search_set_extract[] = {	"FILECLAS",
					"FORM#",
					"INLIB",
					"INVOL",
					"JOBCLASS",
					"JOBLIMIT",
					"JOBQUEUE",
					"LINES",
					"OPERMSGS",
					"OUTLIB",
					"OUTVOL",
					"PRINTER",
					"PRNTMODE",
					"PROGLIB",
					"PROGVOL",
					"PRTCLASS",
					"PRTFCLAS",
					"RUNLIB",
					"RUNVOL",
					"SPOOLLIB",
					"SPOOLIB",
					"SPOOLSYS",
					"SPOOLSYSRC",
					"SPOOLVOL",
					"SYSLIB",
					"SYSVOL",
					"TASKTYPE",
					"USERID",
					"USERNAME",
					"WORKVOL",
					"WS",
					"RECORDS",
					""
				};

EXT char *keyword_set_extract[] = {	"FC",
					"FN",
					"IL",
					"IV",
					"JC",
					"JL",
					"JS",
					"LI",
					"OM",
					"OL",
					"OV",
					"PN",
					"PM",
					"PL",
					"PV",
					"PC",
					"PK",
					"RL",
					"RV",
					"SL",
					"SL",
					"RS",
					"RR",
					"SV",
					"XL",
					"XV",
					"TT",
					"ID",
					"NA",
					"WV",
					"WN",
					"RC",
					""
				};
#else

EXT char *search_set_extract[];
EXT char *keyword_set_extract[];

#endif

/*
**	History:
**	$Log: pgextrct.h,v $
**	Revision 1.8  1997/04/21 14:54:10  scass
**	Corrected copyright.
**	
**	Revision 1.7  1996-09-12 19:22:22-04  gsl
**	Add drcs headers
**
**
**
*/
