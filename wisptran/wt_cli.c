/*
** WISP - Wang Interchange Source Processor
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
*/

/*
**	File:		wt_cli.c
**
**	Project:	WISP
**
**	Purpose:	Command Language Interface (Handle command line args)
**
**	Routines:	
*/

#include <string.h>

#define EXT extern
#include "wisp.h"
#include "wcommon.h"
#include "wmalloc.h"
#include "wispfile.h"
#include "getopt.h"
#include "keywords.h"
#include "ring.h"
#include "keylist.h"

#define WISP_OPTION_FILE	"wisp.opt"
#define WISP_KEY_FILE		"wisp.key"


static int opt_show_flags = 0;
static char opt_wordfile_fname[MAX_FNAME];						/* The name of the WORD file used.		*/
static int opt_echocommand = 0;
static int opt_include_wisp = 0;
static int opt_acu50 = 0;
static int opt_acu51 = 0;
static int opt_acu52 = 0;

static void whodunit(void);
static void printusage(void);
static void showflags(void);
static void use_option(const char* option_arg);

ring_struct	*using_ufb_ring = 0;
struct	using_ufb_struct
{
	char	name[40];
} using_ufb_item;

static int using_ufb_cmp(struct using_ufb_struct *p1, struct using_ufb_struct *p2);

static int option_using_ufb(char* args[]);
static int option_dbfile(char* args[]);


static struct {
	char*	option;				/* The option name */
	int*	flag;				/* Pointer to flag to turn on if option found (or NULL) */
	char**	array;				/* Pointer to array to add option value to (or NULL) */
	int*	cnt;				/*  -- Array count */
	int	max;				/*  -- Array max */
	int	(*routine)(char*args[]);	/* Callback routine to call if option found (or NULL) */
} options_list[] = {
{"#COPY_DECLARATIVE",	NULL, opt_copy_declarative_para,	&opt_copy_declarative_para_cnt, MAX_PARAGRAPHS, NULL},
{"#BLANK_WHEN_ZERO",	NULL, opt_blank_when_zero_item,		&opt_blank_when_zero_cnt,	MAX_BLANK_ITEMS, NULL},
{"#NO_FILE_STATUS",	NULL, opt_no_file_status_item,		&opt_no_file_status_cnt,	MAX_STATUS_ITEMS, NULL},
{"#SORT_FILE",		NULL, opt_sort_file_item,		&opt_sort_file_cnt,		MAX_SF_ITEMS, NULL},
{"#USING_UFB", NULL, NULL, NULL, 0, option_using_ufb},
{"#DBFILE",    NULL, NULL, NULL, 0, option_dbfile}, 
{"#NO_WS_BLANK_WHEN_ZERO",	&opt_no_ws_blank_when_zero, NULL, NULL, 0, NULL},
{"#KEEP_STOP_RUN",		&opt_keep_stop_run, NULL, NULL, 0, NULL},	/* Don't change STOP RUN statements.		*/
{"#CHANGE_STOP_RUN",		&opt_change_stop_run, NULL, NULL, 0, NULL},	/* Change STOP RUN statements to EXIT PROGRAM	*/
{"#DELETE_RECORD_CONTAINS",	&opt_delete_record_contains, NULL, NULL, 0, NULL}, /* Delete the RECORD CONTAINS clause.	*/
{"#DELETE_DATA_RECORDS",	&opt_deldatarecords, NULL, NULL, 0, NULL},	/* Delete the FD DATA RECORDS ARE clause.	*/
{"#NO_WORD_SWAP",		&opt_no_word_swap, NULL, NULL, 0, NULL},	/* Disable WSWAP function.			*/
{"#NO_SEQ_SEQ_OPTIONAL",	&opt_no_seq_seq_optional, NULL, NULL, 0, NULL}, /* Disable OPTIONAL in SELECT statements.	*/
{"#NO_DISPLAY_PROCESSING",	&opt_nodisplay_processing, NULL, NULL, 0, NULL}, /* Disable processing of DISPLAY verb.	*/
{"#KEEP_CONFIG_COMPUTER",	&opt_keep_config_computer, NULL, NULL, 0, NULL},
{"#TRANSLATE_CONFIG_COMPUTER",	&opt_translate_config_computer, NULL, NULL, 0, NULL},
{"#CHANGE_LEVEL77",		&opt_change_level77, NULL, NULL, 0, NULL},
{"#DONT_FORCE_AREA_A_LEVELS",	&opt_dont_force_area_a_levels, NULL, NULL, 0, NULL},
{"#COPYBOOK_EXT_CPY",		&opt_copybook_ext_cpy, NULL, NULL, 0, NULL},
{"#COPYBOOK_EXT_LIB",		&opt_copybook_ext_lib, NULL, NULL, 0, NULL},
{"#NATIVE",			&opt_native, NULL, NULL, 0, NULL},
{"#NOGETLASTFILEOP",		&opt_nogetlastfileop, NULL, NULL, 0, NULL},
{"#NOSLEEP",			&opt_nosleep, NULL, NULL, 0, NULL},
{"#FORCEGENWISPCPY",		&opt_forcegenwispcpy, NULL, NULL, 0, NULL},
{"#SIGN_TRAILING_SEPARATE",	&opt_sign_trailing_separate, NULL, NULL, 0, NULL},
{"#INCLUDE_WISP",		&opt_include_wisp, NULL, NULL, 0, NULL},
{"#ACU50",			&opt_acu50, NULL, NULL, 0, NULL},
{"#ACU51",			&opt_acu51, NULL, NULL, 0, NULL},
{"#ACU52",			&opt_acu52, NULL, NULL, 0, NULL},
{"#NATIVE_SCREENS",		&opt_native_screens, NULL, NULL, 0, NULL},
{"#NOFAC",			&opt_nofac, NULL, NULL, 0, NULL},
{NULL, NULL, NULL, NULL, 0, NULL}
};


int get_cli(int argc, char *argv[])
{
	int	c;
 	int i;
	extern	char	*optarg;
	extern	int	optind;
	/* extern	int	opterr; */
	const char* option_flags; 

 
 	/* Initialize all the flags and counts */
 	for(i=0; options_list[i].option != NULL; i++)
 	{
 		if (options_list[i].flag != NULL)
 		{
 			*options_list[i].flag = 0;
 		}
 		if (options_list[i].cnt != NULL)
 		{
 			*options_list[i].cnt = 0;
 		}
 	}
 

	/*
		Set Defaults
	*/
	opt_log_stats = 0;								/* No logging				*/
	opt_compressed_screens = 1;							/* Compressed screens			*/
	opt_gen_copylib = 0;								/* No copylibs generated		*/
	opt_xref = 0;									/* No cross reference			*/
	opt_xtab = 0;									/* No cross reference tab file		*/
	opt_nowarnings = 0;								/* Show warnings.			*/
	opt_init_data = 0;								/* Init data areas			*/
	opt_init_move = 0;								/* Move spaces as is			*/
	opt_fdfiller = 1;								/* FD fillers are allowed		*/
	opt_wsfiller = 1;								/* WS fillers are allowed		*/
	opt_gen_dms_locking = 1;							/* Do read locking			*/
	acu_cobol = 0;									/* Not ACUCOBOL				*/
 	mf_cobol = 0;									/* Not MF				*/
 	mfoc_cobol = 0;									/* Not MF Object Cobol			*/
 	mfse_cobol = 0;									/* Not MF Server Express		*/
 	unix_cobol = 0;									/* Not UNIX				*/

	opt_x4dbfile = 0;

#ifdef unix
	unix_cobol = 1;
#endif

	acu_cobol = 1;									/* Default is ACUCOBOL			*/

	opt_keyfile_flag = 0;
	opt_optfile_flag = 0;
	opt_show_flags = 0;
	opt_symbzero = 0;
	opt_echocommand = 0;

	opt_wordfile_fname[0] = '\0';

	if (argc == 1)
	{
		printusage();
		exit_wisp(EXIT_FAST);
	}

/*	option_flags = "LlzZcsCxXeETRS1dDFmMfwI:P:G:qkK:oO:V:W:Uu:4?"; */
	option_flags = "14cCdDeEfFG:I:kK:lLmMoO:P:qRsSTu:UV:wW:xXzZ?";

	while ( (c = getopt( argc, argv, option_flags)) != -1 )
	{
		switch( c )
		{
			case '1':							/* /CONVERT				*/
				opt_noprocess = 0;
				opt_gen_copylib = 1;
				break;
			case '4':							/* /X4DBFILE				*/
				opt_x4dbfile = 1;
				break;
			case 'c':							/* /COMMENTS				*/
				/* Obsolete - Comments are always included */
				break;
			case 'C':							/* /COPY_LIB				*/
				opt_gen_copylib = 1;
				break;
			case 'd':							/* /INIT=NODATA				*/
				/* Obsolete */
				break;
			case 'D':							/* /INIT=DATA				*/
				opt_init_data = 1;
				break;
			case 'e':							/* /NOWARNINGS				*/
				opt_nowarnings = 1;
				break;
			case 'E':							/* /ECHOCMD				*/
				opt_echocommand = 1;
				break;
			case 'f':							/* /INIT=NOFD_FILLER			*/
				opt_fdfiller = 0;
				break;
			case 'F':							/* /FLAGS - SHOW ALL FLAGS IN USE		*/
				opt_show_flags = 1;
				break;
			case 'G':							/* /DEBUG= 0 1 2 3			*/
				debug_set_level(optarg);
				break;
			case 'I':							/* /INLIB=				*/
				strcpy(opt_cli_ildir,optarg);
				sp_trunc(opt_cli_ildir);				/* truncate at first space		*/
				break;
			case 'k':
				opt_keyfile_flag = USE_GENERAL_FILE;
				break;
			case 'K':							/* /KEY_FILE=				*/
				strcpy(opt_keyfile_fname,optarg);
				if (opt_keyfile_fname[0])
				{
					if (strpos(opt_keyfile_fname,".") == -1)
						strcat(opt_keyfile_fname,".key");
					write_log("WISP",'I',"KEYFILVAL","KEY file is <%s>.",opt_keyfile_fname);
					opt_keyfile_flag = USE_SPECIFIC_FILE;
				}
				else
				{
					opt_keyfile_flag = USE_GENERAL_FILE;
				}
				break;
			case 'l':							/* /LOG					*/
				opt_log_stats = 1;					/* Write log summary			*/
				break;
			case 'L':							/* /LIST				*/
				opt_logging = 1;					/* enable logging			*/
				break;
			case 'm':							/* /INIT=NOMOVE_SPACEA			*/
				opt_init_move = 1;
				break;
			case 'M':							/* /MANLOCK - manual locking */
				opt_manual_locking = 1;
				break;
			case 'o':
				opt_optfile_flag = USE_GENERAL_FILE;
				break;
			case 'O':							/* /OPTION_FILE=			*/
				strcpy(opt_optfile_fname,optarg);
				if (opt_optfile_fname[0])
				{
					if (strpos(opt_optfile_fname,".") == -1)
						strcat(opt_optfile_fname,".opt");
					write_log("WISP",'I',"OPTFILVAL","OPT file is <%s>.",opt_optfile_fname);
					opt_optfile_flag = USE_SPECIFIC_FILE;
				}
				else
				{
					opt_optfile_flag = USE_GENERAL_FILE;
				}
				break;
			case 'P':							/* PREFIXPATH */
				/*
				**	Special enhancement for use with "COPY file IN lib", gives
				**	the locations to look for "lib/file"
				*/
				strcpy(opt_cli_prefixpath,optarg);
				sp_trunc(opt_cli_prefixpath);
				break;
			case 'q':							/* DATACONV */
				opt_data_conv = 1;
				break;
			case 'R':							/* /OVERRIDE=KEYWORD			*/
				/* Obsolete */
				break;
			case 's':							/* /NOCONCATNATE			*/
				/* Obsolete */
				break;
			case 'S':							/* /NODMS				*/
				opt_gen_dms_locking = 0;
				break;
			case 'T':							/* /NOPROCESS				*/
				opt_noprocess = 1;
				opt_gen_copylib = 0;
				opt_xref = 0;
				break;
			case 'u': /* -u option[=value[,value]] */			/* /USE */
				use_option(optarg);
				break;
			case 'U':							/* /USAGE */
				printusage();
				exit_wisp(EXIT_FAST);
				break;
			case 'V':							/* /LANGUAGE= LPI ACUCOBOL		*/
			{
				acu_cobol = 0;
			 	mf_cobol = 0;
			 	mfoc_cobol = 0;
			 	mfse_cobol = 0;

				if ( 0 == strcmp( optarg, "ACU") )
				{
					acu_cobol = 1;
				}
				else if ( 0 == strcmp( optarg, "ACN") )	/* Acucobol with native screens */
				{
					acu_cobol = 1;
					opt_native_screens = 1;
				}
				else if ( 0 == strcmp( optarg, "MF0") )
				{
					mf_cobol = 1; 
					opt_symbzero = 1;
				}
				else if ( 0 == strcmp( optarg, "MF") )		/* Micro Focus Object Cobol */
				{
					mf_cobol = 1; 
					mfoc_cobol = 1;		
				}
				else if ( 0 == strcmp( optarg, "MFSE") )	/* Micro Focus Server Express */
				{
					mf_cobol = 1; 
					mfse_cobol = 1;
				}
				else
				{
					printf("%%WISP-F-BADCOMPILER Invalid compiler flag (-V %s).\n",optarg);
					perror(0);
					exit_wisp(EXIT_FAST);
				}						
				break;
			}
			case 'w':							/* /INIT=NOWS_FILLER			*/
				opt_wsfiller = 0;
				break;
			case 'W':							/* /WORD_FILE=				*/
				strcpy(opt_wordfile_fname,optarg);
				write_log("WISP",'I',"WORDFILEVAL","WORD file is <%s>.",opt_wordfile_fname);
				break;
			case 'x':							/* /CROSS_REFERENCE			*/
				opt_xref = 1;
				break;
			case 'X':							/* /CROSSTAB - Cross Ref tab file			*/
				opt_xtab = 1;
				break;
			case 'z':							/* /COMPRESS				*/
				/* Obsolete */						/* Compress screens			*/
				break;
			case 'Z':							/* /NOCOMPRESS				*/
				opt_compressed_screens = 0;				/* Don't Compress screens		*/
				break;
			case '?':
			default:
				printusage();
				exit_wisp(EXIT_FAST);
				break;
		}
	}

	if (opt_echocommand)
	{
		int i;
		for(i=0;i<argc; i++)
		{
			printf("%s ",argv[i]);
		}
		printf("\n");
	}

	if (opt_show_flags)
	{
		showflags();
	}

	if (optind >= argc)
	{
		printf("%%WISP-F-NOFILE No filename.\n");
		exit_wisp(EXIT_FAST);
	}

	if (optind < argc-1)
	{
		printf("%%WISP-F-BADARG Invalid command line argument %s \n",argv[optind]);
		exit_wisp(EXIT_FAST);
	}

	/* Get the filename to convert.		*/
	strcpy( in_fname, argv[argc-1] );

	if (!strncmp(in_fname,"WHODUNIT",8) || !strncmp(in_fname,"whodunit",8))	/* Tell them who!			*/
	{
		whodunit();
		exit_wisp(EXIT_FAST);						/* Generate error exit.				*/
	}

	load_change_words(opt_wordfile_fname);

	return 0;
}

void validate_options(void)
{
	/*
	**	Check for option conficts etc.
	*/

	if (mf_cobol)
	{
		opt_nogetlastfileop = 1;	/* MF Cobol does not have a getlastfileop */
		opt_nosleep = 1;		/* MF Cobol does not have a C$SLEEP */
	}

	if (acu_cobol && (opt_acu50 || opt_acu51))
	{
		opt_nogetlastfileop = 1;	/* Older Acucobol does not have getlastfileop */
	}

	if (opt_native)
	{
		if (opt_noprocess)
		{
			write_log("WISP",'F',"OPTIONS","#NATIVE and NOPROCESS (-T) can not both be specified.");
			exit_with_err();
		}
		if (opt_manual_locking)
		{
			write_log("WISP",'F',"OPTIONS","#NATIVE and MANLOCK (-M) can not both be specified.");
			exit_with_err();
		}

		opt_gen_dms_locking = 0;	/* Do not generate DMS locking logic */
		opt_gen_copylib = 1;		/* Generate Copy files */
	}

	if (opt_noprocess && opt_data_conv)
	{
		write_log("WISP",'F',"OPTIONS","NOPROCESS (-T) and DATACONV (-q) can not both be specified.");
		exit_with_err();
	}

	if (opt_manual_locking && !opt_gen_dms_locking)
	{
		write_log("WISP",'F',"OPTIONS","NODMS (-S) and MANLOCK (-M) can not both be specified.");
		exit_with_err();
	}
}

static const char* echo_option(const char* text)
{
	static char *echo_option_str = NULL;
	static size_t echo_option_str_size = 0;

	if (!opt_echocommand)
	{
		return "";
	}

	if (NULL == text)
	{
		if (echo_option_str != NULL)
		{
			echo_option_str[0] = '\0'; /* clear */
		}
		return echo_option_str;
	}

	if (NULL == echo_option_str)
	{
		echo_option_str_size = 1000;
		echo_option_str = wmalloc(echo_option_str_size);
		*echo_option_str = '\0';
	}
	else if (echo_option_str_size < strlen(echo_option_str)+strlen(text)+1)
	{
		echo_option_str_size += 1000;
		echo_option_str = wrealloc(echo_option_str, echo_option_str_size);
	}

	strcat(echo_option_str, text);

	return echo_option_str;
}

/*
**	#DBFILE select-name [table-name]
*/
static int option_dbfile(char* args[])
{
	char buff[100];

	if (args[1] == NULL)
	{
		/* Missing arguments */
		return -1;
	}

	if (args[2] != NULL)
	{
		dbfile_add_item(args[1], args[2]);
		sprintf(buff, " %s=%s,%s", args[0], args[1], args[2]);
	}
	else
	{
		dbfile_add_item(args[1], "");
		sprintf(buff," %s=%s", args[0], args[1]);
	}
	echo_option(buff);
	return 0;
}



#define MAX_OPTION_TOKENS 36

/*
**	ROUTINE:	use_option()
**
**	FUNCTION:	Load one option from command line
**
**	DESCRIPTION:	The option arg can have the format "option[=value[,value]]".
**			If the option has a value it will be immediately followed
**			by a "=value" with NO-WHITE-SPACE. Multiple values can
**			be specified as a coma separated list.
**
**			The leading '#' on the option is optional.
**
**	ARGUMENTS:	
**	option_arg	The text following the "-u" command line flag
**
**	GLOBALS:
**	options_list	The array of options plus actions	
**	opt_echocommand	Are options being echoed flag
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void use_option(const char* option_arg)
{
	char* opt_tokens[MAX_OPTION_TOKENS];
	int opt_token_cnt = 0;
	char* ptr;
	char option_line[256];
	char the_option[256];
	int found_option = 0;
	int i;

	/* Split the option_arg into tokens */
	strcpy(option_line, option_arg);

	opt_tokens[opt_token_cnt++] = option_line;

	if ((ptr = strchr(option_line,'=')) != NULL)
	{
		/* Found '='. split line into tokens at this location */
		*ptr = '\0'; 
		opt_tokens[opt_token_cnt++] = ++ptr;

		while ((ptr = strchr(ptr,',')) != NULL)
		{
			if (opt_token_cnt < MAX_OPTION_TOKENS)
			{
				/* Found ','. split line into tokens at this location */
				*ptr = '\0'; 
				opt_tokens[opt_token_cnt++] = ++ptr;
			}
			else
			{
				write_log("WISP",'E',"MAXTOKENS",
					"Maximum number of OPTION tokens exceeded [%s]", option_arg);
				break;
			}
		}
	}

	opt_tokens[opt_token_cnt] = NULL;

	/*
	**	Load the option token into the_option.
	**	Add leading '#' if not supplied.
	*/
	the_option[0] = '\0';
	if (opt_tokens[0][0] != '#')
	{
		strcat(the_option, "#");  
	}
	strcat(the_option, opt_tokens[0]);  


	/* Possible option: first token starts with # */
	for(i=0; options_list[i].option != NULL; i++)
	{
		if (0==strcmp(options_list[i].option, the_option))
		{
			/* Found the option */
			found_option = 1;

			if (options_list[i].flag != NULL)
			{
				/* Set the flag if present */
				*options_list[i].flag = 1;
			}

			if (options_list[i].array != NULL)
			{
				if (opt_token_cnt < 2)
				{
					write_log("WISP",'E',"OPTIONVALUE",
						"Option [%s] is missing required value argument",
						opt_tokens[0]);
					break;
				}

				/* Add the next value to the array list */
				if (*options_list[i].cnt < options_list[i].max)
				{
					options_list[i].array[*options_list[i].cnt++] =
						(char *)wdupstr(opt_tokens[1]);

				}
				else
				{
					write_log("WISP",'E',"MAXITEMS",
						"Maximum number of %s items exceeded.",
						opt_tokens[0]);
				}
			}

			if (options_list[i].routine != NULL)
			{
				/* Call the call back routine */
				if (0 != (*options_list[i].routine)(opt_tokens) )
				{
					write_log("WISP",'E',"OPTIONVALUE",
						"Option [%s] is missing required value argument",
						opt_tokens[0]);
				}
			}

			break;
		}
	}

	if (!found_option)
	{
		write_log("WISP",'E',"OPTION","Unrecognized option [%s]", option_arg);
	}

}

/*
**	ROUTINE:	load_options_from_file()
**
**	FUNCTION:	Load options from a supplied file
**
**	DESCRIPTION:	Options in an option file are: 
**			- one option per line
**			- options start with '#' character
**			- comments start with a ';' in the first column
**			- values for options follow the option on the same line
**			- values can be separated by whitespace, commas, or "="
**
**	ARGUMENTS:	
**	filename	The option file filename
**
**	GLOBALS:
**	options_list	The array of options plus actions	
**	opt_echocommand	Are options being echoed flag
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void load_options_from_file(const char* filename)
{
	char* opt_tokens[MAX_OPTION_TOKENS];
	int opt_token_cnt = 0;
	FILE *option_file;
	char option_line[256];
	int i;
	int lineno = 0;

	option_file = fopen(filename,"r");

	if (!option_file)
	{
		write_log("WISP",'E',"CANTFINDOPT","OPTION_FILE [%s] not found.",filename);
		return;
	}

	while(wfgets(option_line, sizeof(option_line)-1, option_file))
	{
		char original_line[256];
		char* ptr;
		int found_option;

		lineno++;
		ptr = strchr(option_line,'\n');
		if (ptr != NULL)
		{
			*ptr = '\0';
		}
		strcpy(original_line, option_line);

		/* Free tokens from last loop */
		for(i=0; i<opt_token_cnt; i++) {wfree(opt_tokens[i]);}
		opt_token_cnt = 0;

		if (option_line[0] == ';')
		{
			continue; /* comment */
		}

		/* Split the line into whitespace separated tokens */
		ptr = strtok(option_line," ,\n\t=");
		while(ptr != NULL)
		{
			if (opt_token_cnt < MAX_OPTION_TOKENS)
			{
				opt_tokens[opt_token_cnt++] = wdupstr(ptr);
			}
			else
			{
				write_log("WISP",'E',"MAXTOKENS",
					"Maximum number of OPTION_FILE line tokens exceeded.");

				/* Note: Don't stop looping until strtok() returns NULL */
			}

			ptr = strtok(NULL," ,\n\t=");
		}
		opt_tokens[opt_token_cnt] = NULL;

		if (opt_token_cnt == 0)
		{
			continue; /* empty line */
		}

		if (opt_tokens[0][0] != '#')
		{
			write_log("WISP",'E',"OPTION_FILE","Error parsing OPTION_FILE [%s:%d] line [%s]",
				filename, lineno, original_line);
			continue;
		}

		/* Possible option: first token starts with # */
		found_option = 0;
		for(i=0; options_list[i].option != NULL; i++)
		{
			if (0==strcmp(options_list[i].option, opt_tokens[0]))
			{
				/* Found the option */
				found_option = 1;

				if (options_list[i].flag != NULL)
				{
					/* Set the flag if present */
					*options_list[i].flag = 1;

					if (opt_echocommand)
					{
						char buff[1000];
						sprintf(buff," %s", options_list[i].option);
						echo_option(buff);
					}
				}


				if (options_list[i].array != NULL)
				{
					if (opt_token_cnt < 2)
					{
						write_log("WISP",'E',"OPTIONVALUE",
							"Option [%s] is missing required value argument in file [%s:%d]",
							options_list[i].option, filename, lineno);
						break;
					}

					/* Add the next value to the array list */
					if (*options_list[i].cnt < options_list[i].max)
					{
						options_list[i].array[*options_list[i].cnt++] =
							(char *)wdupstr(opt_tokens[1]);

					}
					else
					{
						write_log("WISP",'E',"MAXITEMS",
							"Maximum number of %s items exceeded.",
							options_list[i].option);
					}

					if (opt_echocommand)
					{
						char buff[1000];
						sprintf(buff, " %s=%s", options_list[i].option, opt_tokens[1]);
						echo_option(buff);
					}
				}

				if (options_list[i].routine != NULL)
				{
					/* Call the call back routine */
					if (0 != (*options_list[i].routine)(opt_tokens))
					{
						write_log("WISP",'E',"OPTIONVALUE",
							"OPTION_FILE Command [%s] is missing required value argument in file [%s:%d]",
							options_list[i].option, filename, lineno);
					}
				}

				break;
			}
		}

		if (!found_option)
		{
			write_log("WISP",'E',"OPTION_FILE","Unrecognized command [%s] in file [%s:%d]",
				opt_tokens[0], filename, lineno);
		}


	}

	/* Free tokens */
	for(i=0; i<opt_token_cnt; i++) {wfree(opt_tokens[i]);}

	fclose(option_file);
}

void load_option_file(void)
{
	if (!opt_optfile_flag) return;

	echo_option(NULL);

	load_options_from_file(opt_optfile_fname);
	if (opt_include_wisp)
	{
		load_options_from_file(WISP_OPTION_FILE);
	}

	if (opt_echocommand)
	{
		printf("Using OPTION_FILE Commands (%s )\n", echo_option(""));
	}
}

/*
**	#USING_UFB routine-name
*/
static int option_using_ufb(char* args[])
{
	int	rc;

	if (args[1] == NULL)
	{
		/* Missing Arguments */
		return -1;
	}

	if (DOUBLE_QUOTE == args[1][0])
	{
		strcpy(using_ufb_item.name,args[1]);
	}
	else
	{
		sprintf(using_ufb_item.name,"\"%s\"",args[1]);
	}

	if (opt_echocommand)
	{
		char buff[100];
		sprintf(buff, " %s=%s", args[0], using_ufb_item.name);
		echo_option(buff);
	}

	if (!using_ufb_ring)
	{
		if((rc=ring_open((char **)&using_ufb_ring,sizeof(struct using_ufb_struct),5,5,
				using_ufb_cmp,1)))
		{
			write_log("WISP",'F',"RINGOPEN",
				"Unable to open ring [using_ufb_ring] rc=%d [%s]",rc,ring_error(rc));
			exit_wisp(EXIT_WITH_ERR);
		}
	}
	
	if ((rc = ring_add(using_ufb_ring,0,&using_ufb_item)))	/* Store the USING_UFB into the ring	*/
	{
		write_log("WISP",'F',"RINGADD",
			"Unable to add to ring [using_ufb_ring] rc=%d [%s]",rc,ring_error(rc));
		exit_wisp(EXIT_WITH_ERR);
	}

	return 0;
}

static int using_ufb_cmp(struct using_ufb_struct *p1, struct using_ufb_struct *p2)
{
	return(strcmp(p1->name, p2->name));
}

int using_ufb_test(const char * item)
{
	int	rc;
	if (!using_ufb_ring) return(0);

	strcpy(using_ufb_item.name,item);
	rc = ring_find(using_ufb_ring,&using_ufb_item,0,0);
	return(!rc);
}


void load_key_file(void)							/* Read the key file.				*/
{
/*
	The keyfile now has several different enteries.

	#LEADING	keyname		- Correct keys with leading separate signs
	#COMPUTATIONAL	keyname		- Change COMP keys to PIC X
	#INCLUDE_WISP			- Include the global WISP key file
*/


	int i;
	char tstr[133],*scan_ptr,*prm_ptr;
	FILE *key_file;
	int use_wisp;

	if (!opt_keyfile_flag) return;						/* Skip it.					*/

	key_file = fopen(opt_keyfile_fname,"r");					/* Try the local key file.			*/

	if (!key_file && (opt_keyfile_flag == USE_SPECIFIC_FILE))
	{
		write_log("WISP",'F',"CANTFINDKEY","Key file %s not found.",opt_keyfile_fname);
		exit_wisp(EXIT_WITH_ERR);
	}

scan_key:

	use_wisp = 0;

	if (!key_file) return;							/* No key file.					*/

	while (wfgets(tstr,132,key_file))
	{									/* Read the names of the fields.		*/
		scan_ptr = tstr;
		uppercase(tstr);						/* Shift whole string to uppercase		*/

		i = 0;
		do								/* scan the line and extract the parms		*/
		{								/* skip over spaces, commas, newlines , tabs	*/
			if ((*scan_ptr == ' ') || (*scan_ptr == ',') || (*scan_ptr == '\n') || (*scan_ptr == '\t'))
			{
				scan_ptr++;
			}
			else							/* copy this parm				*/
			{
				short tflag;
				tflag = 0;

				if ( *scan_ptr == '#' )
				{
					if      (  0==memcmp(scan_ptr,"#LEAD",5) )
					{
						key_list[kl_count].type = KL_LEAD;
						tflag = 1;
					}
					else if (  0==memcmp(scan_ptr,"#COMP",5) )
					{
						key_list[kl_count].type = KL_COMP;
						tflag = 1;
					}
					else if (  0==memcmp(scan_ptr,"#INCL",5) )
					{
						use_wisp = 1;			/* Set flag to INCLUDE wisp global keys file	*/
					}
					else
					{
						write_log("WISP",'F',"INVALIDKEYFILE",
								"Unknown Control in Key file %5.5s.",scan_ptr);
						exit_wisp(EXIT_WITH_ERR);
					}
				
					if ( tflag )
					{
						for(scan_ptr +=5; 			/* Scan until white space		*/
						    	((*scan_ptr != ' ' ) && 
							 (*scan_ptr != ',' ) && 
							 (*scan_ptr != '\n') && 
							 (*scan_ptr != '\t'));  scan_ptr++);

						for(scan_ptr++; 				/* Scan thru white space		*/
						    	((*scan_ptr == ' ' ) || 
							 (*scan_ptr == ',' ) || 
							 (*scan_ptr == '\n') || 
							 (*scan_ptr == '\t'));  scan_ptr++);

					}
				}
				else						/* Assume old format & LEADING			*/
				{
					key_list[kl_count].type = KL_LEAD;
					tflag = 1;
				}

				if (tflag)					/* If we have a key				*/
				{
					prm_ptr = key_list[kl_count].name;	/* Point to current parameter			*/
					do
					{					/* copy till next whitespace			*/
						*prm_ptr++ = *scan_ptr++;
					} while ((*scan_ptr) && (*scan_ptr != ' ') && (*scan_ptr != '\n') && (*scan_ptr != '\t'));
					*prm_ptr = '\0';			/* null terminate				*/

					key_list[kl_count].qual[0] = '\0';	/* Get the OF qual if any.			*/
					sscanf(scan_ptr," OF %s",key_list[kl_count].qual);
					*scan_ptr = '\0';
					kl_count +=1;
				}

			}
		} while (*scan_ptr);						/* till a null 					*/
	}
	fclose(key_file);

	if (use_wisp)
	{
		key_file = fopen(WISP_KEY_FILE,"r");				/* Try the global key file.			*/
		goto scan_key;
	}
}

static char *copyright_display_str = 
"Copyright (c) 1989-" WISP_COPYRIGHT_YEAR_STR " Shell Stream Software LLC";

static void whodunit(void)
{
              /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	printf("\n\n\n");
	printf("WISP: Version=[%s]\n\n\n",WISP_VERSION);
	printf("  Translator written by Greg Lindholm.\n");
	printf("  Library and utilities written by Greg Lindholm, Frank Dziuba, David Young,\n");
	printf("  Jock Cooper, Suzette Cass, and Gregory Adams.\n");
	printf("  Screen management and Wang workstation emulation written by Gregory Adams.\n");
	printf("  Unix implementation written by Greg Lindholm and Jock Cooper.\n");
	printf("  MSDOS implementation written by Greg Lindholm.\n");
	printf("  WIN32 implementation written by Greg Lindholm and Jock Cooper.\n");
	printf("\n");
	printf("  Many thanks to our clients for their assistance and patience, without which\n");
	printf("  this product would not exist.\n");
	printf("\n");
	printf("  %s\n\n\n",copyright_display_str);
}

static void printusage(void)
{
	printf("%s\n",copyright_display_str);
	printf("\n");
	printf("WISP: Version=[%s]\n",WISP_VERSION);
	printf("\n");
	printf("USAGE: wisp [-flags] filename\n\n");
        printf("       FLAG          DESCRIPTION\n");
        printf("        -Vxxx        COBOL: ACU,ACN,MF,MFSE\n");
        printf("        -Ipath       Input copybook directory paths.\n");
        printf("        -Ppath       Prefix path for COPY libraries.\n");
        printf("        -T           No Process. Create only a .txt file.\n");
        printf("        -x           Produce a cross reference.\n");
        printf("        -X           Produce a cross reference tab file.\n");
        printf("        -L           Enable logging.\n");
        printf("        -l           Write a log summary.\n");
        printf("        -Z           Don't compress screen output.\n");
        printf("        -e           Don't show warning messages.\n");
        printf("        -k           Use standard Key file.\n");
        printf("        -Kfile       Use specified Key file.\n");
        printf("        -o           Use standard Option file.\n");
        printf("        -Ofile       Use specified Option file.\n");
        printf("        -Wfile       Use specified Word file.\n");
        printf("        -S           Do not add DMS locking.\n");
        printf("        -D           Do init data areas.\n");
        printf("        -m           Change MOVE SPACES to INITIALIZE.\n");
        printf("        -f           Change FD FILLERs to fields.\n");
        printf("        -w           Change WS FILLERS to fields.\n");
        printf("        -q           Generate data conversion program.\n");
        printf("        -4           Add check for database files.\n");
        printf("        -M           Use manual record locking.\n");
        printf("        -1           Use CONVERSION method (gen COPY files).\n");
        printf("        -Gxxx        Debug trace levels (1-4).\n");
        printf("        -uopt[=val[,val]] Use OPTION_FILE options.\n");
        printf("        -E           Echo command line.\n");
        printf("        -U           SHOW ALL USAGE FLAGS.\n");
        printf("        -F           SHOW FLAGS IN USE.\n");
}

static void showflags(void)
{
	printf("\n");
	printf("The Following WISP Option are in use;\n\n");

        printf(        "       FLAG          DESCRIPTION\n");
        if ( opt_log_stats )                                             /* No logging                           */
                printf("        -l           Write a log summary.\n");
        else				
                printf("     no -l           No log summary.\n");
        if ( opt_compressed_screens )                                /* Not compressed                       */
                printf("     no -Z           Compress screen output.\n");
        else				
                printf("        -Z           Don't compressed screen output.\n");
        if ( opt_gen_copylib )
                printf("        -1           CONVERSION method (Generate copy files).\n");
        else				
                printf("     no -1           PREPROCESSOR method (Concatenate copy files).\n");
        if ( opt_nowarnings )                                            /* No WARNING messages                  */
                printf("        -e           Don't show warning messages.\n");
        else				
                printf("     no -e           Show warning messages.\n");
        if ( opt_xref )                                               /* No cross reference                   */
                printf("        -x           Produce a cross reference.\n");
        else				
                printf("     no -x           No cross reference.\n");
        if ( opt_xtab )                                               /* No cross reference                   */
                printf("        -X           Produce a cross reference tab file.\n");
        else				
                printf("     no -X           No cross reference tab file.\n");
        if ( opt_data_conv )		
                printf("        -q           Generate data conversion program.\n");
        if ( opt_x4dbfile )			
                printf("        -4           Add check for database files.\n");
        if ( opt_init_data )                                             /* Init data areas                      */
                printf("        -D           Initialize data areas.\n");
        else				
                printf("     no -D           Don't initialize data areas.\n");
        if ( opt_init_move )                                             /* Move spaces as is                    */
                printf("        -m           Change MOVE SPACES to INITIALIZE.\n");
        else				
                printf("     no -m           Leave MOVE SPACES alone.\n");
        if ( opt_fdfiller )                                              /* FD fillers are allowed               */
                printf("     no -f           Leave FD FILLERs alone.\n");
        else				
                printf("        -f           Change FD FILLERs to fields.\n");
        if ( opt_wsfiller )                                              /* WS fillers are allowed               */
                printf("     no -w           Leave WS FILLERS alone.\n");
        else				
                printf("        -w           Change WS FILLERS to fields.\n");
        if ( acu_cobol && opt_native_screens)                                             
                printf("        -VACN        Generate ACUCOBOL (NATIVE SCREEN) COBOL.\n");
        if ( acu_cobol && !opt_native_screens)	
                printf("        -VACU        Generate ACUCOBOL COBOL.\n");
        if ( mfoc_cobol )
                printf("        -VMF         Generate MICRO FOCUS OBJECT COBOL.\n");
        if ( mfse_cobol)                                         
                printf("        -VMFSE       Generate MICRO FOCUS Server Express COBOL.\n");
        if ( mf_cobol && opt_symbzero)                                           
                printf("        -VMF0        Generate MICRO FOCUS COBOL. (SYMBOLIC 0=0)\n");
        if ( opt_keyfile_flag )		
                printf("        -Kfile       Use specified Key file.\n");
        else				
                printf("     no -Kfile       Don't use Key file.\n");
        if ( opt_optfile_flag )		
                printf("        -Ofile       Use specified Option file.\n");
        else				
                printf("     no -Ofile       Don't use Option file.\n");
        if ( opt_gen_dms_locking )		
                printf("     no -S           Add DMS locking logic.\n");
        else				
                printf("        -S           Do not add DMS locking logic.\n");
        if ( opt_manual_locking )		
                printf("        -M           Use manual record locking.\n");
        else				
                printf("     no -M           Use automatic record locking.\n");
}

/*
**	History:
**	$Log: wt_cli.c,v $
**	Revision 1.53  2009/10/18 20:19:39  gsl
**	Copyright
**	
**	Revision 1.52  2003/08/13 20:57:06  gsl
**	#NOFAC option
**	
**	Revision 1.51  2003/08/08 19:52:47  gsl
**	Add native screens comments
**	
**	Revision 1.50  2003/08/06 18:12:10  gsl
**	
**	Revision 1.49  2003/06/18 18:17:46  gsl
**	Use -1 instead of -C
**	-C is still supported but removed from the displayed list of flags
**	
**	Revision 1.48  2003/04/11 14:33:17  gsl
**	Add WISP translator options #ACU50 and #ACU51
**	
**	Revision 1.47  2003/03/20 15:01:04  gsl
**	Fix -Wall warnings
**	
**	Revision 1.46  2003/03/11 19:26:33  gsl
**	re-org the option flags alphabetically for sanity
**	Add validate_options() routine which checks for option conficts.
**	
**	Revision 1.45  2003/03/10 18:55:45  gsl
**	Added nosleep option and for ACU default to using C$SLEEP instead
**	of WFWAIT on a READ with HOLD
**	
**	Revision 1.44  2003/03/10 17:36:49  gsl
**	Fix so opt_nogetlastfileop flag is always set for mf_cobol
**	
**	Revision 1.43  2003/03/07 21:44:20  gsl
**	rremove unused opt_native_acu flag
**	
**	Revision 1.42  2003/03/07 17:00:07  gsl
**	For ACU default to using "C$GETLASTFILEOP" to retrieve the last file op.
**	Add option #NOGETLASTFILEOP to use if not C$GETLASTFILEOP is
**	not available.
**	
**	Revision 1.41  2003/03/06 21:36:07  gsl
**	Check #GETLASTFILEOP  is only supported with ACU
**	
**	Revision 1.40  2003/03/04 21:11:25  gsl
**	fix up the option intialization logic
**	
**	Revision 1.39  2003/03/04 19:59:51  gsl
**	remove old option processing
**	
**	Revision 1.38  2003/03/04 18:49:59  gsl
**	Implement '-u option[=value[,value]]' processing
**	
**	Revision 1.37  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.36  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.35  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.34  2003/01/21 16:25:57  gsl
**	remove VMS /DLINK option
**	
**	Revision 1.33  2002/10/14 19:08:01  gsl
**	Remove the -c options as obsolete since comments are now always
**	included in the output.
**	
**	Revision 1.32  2002/07/24 21:41:02  gsl
**	Change directory to the location of the source file if a path is given.
**	
**	Revision 1.31  2002/07/18 21:04:26  gsl
**	Remove MSDOS code
**	
**	Revision 1.30  2002/07/08 21:06:43  gsl
**	Fix %% printf error
**	
**	Revision 1.29  2002/06/21 20:49:32  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.28  2002/06/21 03:48:11  gsl
**	Remove lpi_cobol and vax_cobol
**	
**	Revision 1.27  2002/06/21 03:13:54  gsl
**	Remove VAX & LPI
**	
**	Revision 1.26  2002/06/20 22:55:17  gsl
**	remove obsolete code
**	
**	Revision 1.25  2002/03/28 15:04:27  gsl
**	Use define for copyright year
**	
**	Revision 1.24  2002-03-26 16:13:15-05  gsl
**	Stop displaying Library and Screen versions
**
**	Revision 1.23  2002-03-21 17:12:01-05  gsl
**	Copyright 2002
**
**	Revision 1.22  2002-03-21 17:03:58-05  gsl
**	Add -V MFSE as a langauge option
**	remove AIX and DMF
**
**	Revision 1.21  2001-09-13 10:03:53-04  gsl
**	Remove VMS ifdef's
**	Add -X xtab flag
**
**	Revision 1.19  2000-03-16 10:27:40-05  gsl
**	2000
**
**	Revision 1.18  1999-09-13 15:57:06-04  gsl
**	update copyrights
**
**	Revision 1.17  1998-06-09 13:02:31-04  gsl
**	Add option -M for manual record locking
**
**	Revision 1.16  1998-03-27 10:38:03-05  gsl
**	change_words
**
**	Revision 1.15  1998-03-04 16:13:19-05  gsl
**	Update copyright
**
**	Revision 1.14  1997-08-28 17:12:05-04  gsl
**	Add -VACN for Native Acucobol
**
**	Revision 1.13  1997-07-29 10:31:21-04  gsl
**	Change copyright 
**
**	Revision 1.12  1996-06-24 14:12:14-04  gsl
**	add NT defaults and add missing return codes
**
**	Revision 1.11  1996-01-08 02:24:09-08  gsl
**	Changed Copyright to 1996
**
**
**
*/
