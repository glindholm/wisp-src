/* PGEXTRCT.H															*/
/*		 Initialize the Extract specific keyword arrays									*/

#define	FORM	1
#define JOBLMT	5
#define LINES	7
#define PRNTER	11
#define SPLSRR	21
#define WS	27

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
					"SPOOLSYS",
					"SPOOLSYSRC",
					"SPOOLVOL",
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
					"P#",
					"PM",
					"PL",
					"PV",
					"PC",
					"PF",
					"RL",
					"RV",
					"SL",
					"RS",
					"RR",
					"SV",
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

