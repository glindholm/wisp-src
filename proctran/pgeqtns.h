/* PGEQTNS.H															*/
/*		 Initialization of the equation specific arrays									*/

#ifdef INIT_COMMON

EXT char *search_equations[] = {	"0",
					"1",
					"2",
					"3",
					"4",
					"5",
					"6",
					"7",
					"8",
					"9",
					"'",
					"(",
					")",
					"!",
					",",
					"+",
					"-",
					"*",
					"/",
					"&",
					"=",
					">",
					"<",
					""
				};

EXT char *search_run_equations[] = {	"=",
					"(",
					")",
					"!",
					",",
					"+",
					"-",
					"*",
					"/",
					">",
					"<",
					""
				 };

EXT char *search_numbers[] = {	"0",
				"1",
				"2",
				"3",
				"4",
				"5",
				"6",
				"7",
				"8",
				"9",
				""
			};
#else

EXT char *search_equations[];
EXT char *search_run_equations[];
EXT char *search_numbers[];

#endif
