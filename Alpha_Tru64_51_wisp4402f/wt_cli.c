static char copyright[]="Copyright (c) 1989-2002 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wt_cli.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Command Language Interface (Handle command line args)
**
**	Routines:	
*/

#include <string.h>

#define EXT extern
#include "wisp.h"
#include "wcommon.h"
#include "wispfile.h"
#include "getopt.h"
#include "keywords.h"


int	symbzero = 0;

static int show_flags;
static char word_fname[MAX_FNAME];						/* The name of the WORD file used.		*/

static char cli_infile[MAX_FNAME];						/* The initial input file name			*/
static char cli_listfile[MAX_FNAME];						/* The file for conversion information listing	*/

static int concat;

static void whodunit(void);
static void printusage(void);
static void fullusage(void);
static void showflags(void);




int get_cli(int argc, char *argv[])
{
	int	c;
	extern	char	*optarg;
	extern	int	optind,opterr;
	/*
		Set Defaults
	*/
	log_stats = 0;									/* No logging				*/
	compress = 1;									/* Compressed screens			*/
	comments = 1;									/* Comments				*/
	blanklines = 1;									/* Blanklines				*/
	concat = 1;									/* Concatenate files			*/
	copylib = 0;									/* No copylibs generated		*/
	do_xref = 0;									/* No cross reference			*/
	do_xtab = 0;									/* No cross reference tab file		*/
	do_dlink = 0;									/* No VMS dynamic link			*/
	nowarnings = 0;									/* Show warnings.			*/
	init_data = 0;									/* Init data areas			*/
	init_move = 0;									/* Move spaces as is			*/
	fdfiller = 1;									/* FD fillers are allowed		*/
	wsfiller = 1;									/* WS fillers are allowed		*/
	do_locking = 1;									/* Do read locking			*/
	lpi_cobol = 0;									/* Not LPI				*/
	acu_cobol = 0;									/* Not ACUCOBOL				*/
 	vax_cobol = 0;									/* Not VAX				*/
 	mf_cobol = 0;									/* Not MF				*/
 	mfoc_cobol = 0;									/* Not MF Object Cobol			*/
 	mfse_cobol = 0;									/* Not MF Server Express		*/
 	unix_cobol = 0;									/* Not UNIX				*/
 	dos_cobol = 0;									/* Not MS-DOS				*/

 	aix_cobol = 0;									/* Obsolete				*/
 	dmf_cobol = 0;									/* Obsolete				*/

	x4dbfile = 0;

#ifdef unix
	unix_cobol = 1;
	acu_cobol = 1;									/* All others then default is ACUCOBOL	*/
#endif

#ifdef WINNT
	nt_cobol = 1;
	acu_cobol = 1;
#endif

	do_keyfile = 0;
	do_optfile = 0;
	show_flags = 0;

	word_fname[0] = '\0';

	if (argc == 1)
	{
		printusage();
		exit_wisp(EXIT_FAST);
	}

	while ( (c = getopt( argc, argv, "LlzZcsCxXneTRS1dDFmMfwI:P:G:qkK:oO:V:W:U4?")) != -1 )
	{
		switch( c )
		{
			case 'L':							/* /LIST				*/
				logging = 1;						/* enable logging			*/
				break;
			case 'l':							/* /LOG					*/
				log_stats = 1;						/* Write log summary			*/
				break;
			case 'z':							/* /COMPRESS				*/
				compress = 1;						/* Compress screens			*/
				break;
			case 'Z':							/* /NOCOMPRESS				*/
				compress = 0;						/* Don't Compress screens		*/
				break;
			case 'c':							/* /COMMENTS				*/
				comments = 1;
				blanklines = 1;
				break;
			case 's':							/* /NOCONCATNATE			*/
				concat = 0;
				break;
			case 'C':							/* /COPY_LIB				*/
				copylib = 1;
				concat = 0;						/* (implies a noconcatnate)		*/
				break;
			case 'x':							/* /CROSS_REFERENCE			*/
				do_xref = 1;
				break;
			case 'X':							/* Cross Ref tab file			*/
				do_xtab = 1;
				break;
			case 'n':							/* /DLINK				*/
				do_dlink = 1;
				break;
			case 'e':							/* /NOWARNINGS				*/
				nowarnings = 1;
				break;
			case 'T':							/* /NOPROCESS				*/
				copy_only = 1;
				comments = 1;
				blanklines = 1;
				concat = 1;
				copylib = 0;
				do_xref = 0;
				break;
			case 'I':							/* /INLIB=				*/
				strcpy(cli_ildir,optarg);
				sp_trunc(cli_ildir);					/* truncate at first space		*/
				break;
			case 'P':
				/*
				**	Special enhancement for use with "COPY file IN lib", gives
				**	the locations to look for "lib/file"
				*/
				strcpy(cli_prefixpath,optarg);
				sp_trunc(cli_prefixpath);
				break;
			case 'G':							/* /DEBUG= 0 1 2 3			*/
				debug_set_level(optarg);
				break;
			case 'k':
				do_keyfile = USE_GENERAL_FILE;
				break;
			case 'K':							/* /KEY_FILE=				*/
				strcpy(key_fname,optarg);
				if (key_fname[0])
				{
					if (strpos(key_fname,".") == -1)
						strcat(key_fname,".key");
					write_log("WISP",'I',"KEYFILVAL","KEY file is <%s>.",key_fname);
					do_keyfile = USE_SPECIFIC_FILE;
				}
				else
				{
					do_keyfile = USE_GENERAL_FILE;
				}
				break;
			case 'o':
				do_optfile = USE_GENERAL_FILE;
				break;
			case 'O':							/* /OPTION_FILE=			*/
				strcpy(opt_fname,optarg);
				if (opt_fname[0])
				{
					if (strpos(opt_fname,".") == -1)
						strcat(opt_fname,".opt");
					write_log("WISP",'I',"OPTFILVAL","OPT file is <%s>.",opt_fname);
					do_optfile = USE_SPECIFIC_FILE;
				}
				else
				{
					do_optfile = USE_GENERAL_FILE;
				}
				break;
			case 'W':							/* /WORD_FILE=				*/
				strcpy(word_fname,optarg);
				write_log("WISP",'I',"WORDFILEVAL","WORD file is <%s>.",word_fname);
				break;
			case 'd':							/* /INIT=NODATA				*/
				init_data = 0;
				break;
			case 'D':							/* /INIT=DATA				*/
				init_data = 1;
				break;
			case 'm':							/* /INIT=NOMOVE_SPACEA			*/
				init_move = 1;
				break;
			case 'M':
				manual_locking = 1;
				break;
			case 'F':							/* SHOW ALL FLAGS IN USE		*/
				show_flags = 1;
				break;
			case 'f':							/* /INIT=NOFD_FILLER			*/
				fdfiller = 0;
				break;
			case 'w':							/* /INIT=NOWS_FILLER			*/
				wsfiller = 0;
				break;
			case 'R':							/* /OVERRIDE=KEYWORD			*/
				kwproc = 0;
				break;
			case 'S':							/* /NODMS				*/
				do_locking = 0;
				break;
			case '1':							/* /CONVERT				*/
				copy_only = 0;
				comments = 1;
				blanklines = 1;
				concat = 0;
				copylib = 1;
				break;
			case 'V':							/* /LANGUAGE= LPI ACUCOBOL		*/
			{
				lpi_cobol = 0;						/* First Clear the default		*/
				acu_cobol = 0;
			 	vax_cobol = 0;
			 	mf_cobol = 0;
			 	mfoc_cobol = 0;
			 	mfse_cobol = 0;
			 	dos_cobol = 0;

				if ( 0 == strcmp( optarg, "LPI") )
				{
					lpi_cobol = 1;
				}
				else if ( 0 == strcmp( optarg, "ACU") )
				{
					acu_cobol = 1;
				}
				else if ( 0 == strcmp( optarg, "ACN") )	/* Acucobol with native screens */
				{
					acu_cobol = 1;
					acn_cobol = 1;
				}
				else if ( 0 == strcmp( optarg, "VAX") )
				{
					vax_cobol = 1;
				}
				else if ( 0 == strcmp( optarg, "MF0") )
				{
					mf_cobol = 1; 
					symbzero = 1;
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
			case 'q':
				data_conv = 1;
				break;
			case '4':							/* /X4DBFILE				*/
				x4dbfile = 1;
				break;
			case 'U':
				fullusage();
				exit_wisp(EXIT_FAST);
				break;
			case '?':
			default:
				printusage();
				exit_wisp(EXIT_FAST);
				break;
		}
	}

	if (show_flags)
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

	strcpy( in_fname, argv[argc-1] );						/* Get the filename to convert.		*/
	strcpy( cli_infile, in_fname );


	if (!strncmp(cli_infile,"WHODUNIT",8) || !strncmp(cli_infile,"whodunit",8))	/* Tell them who!			*/
	{
		whodunit();
		exit_wisp(EXIT_FAST);						/* Generate error exit.				*/
	}

	/*
	**	The concat switch is being eliminated, there is now only 2 modes:
	**		1) Concatinate and don't generate copybooks	(!copylib) Default
	**		2) Don't concat and generate copybooks		(copylib)
	*/

	if (copylib && concat)
	{
		write_log("WISP",'F',"OPTIONS","Error can not generate copybooks AND concatenate source.");
		exit_with_err();
	}

	if (!copylib && !concat)
	{
		write_log("WISP",'F',"OPTIONS",
			"Unsupported combination of options, MUST generate copybooks OR concatenate source.");
		exit_with_err();
	}

	if (copylib || !concat) { copylib = 1; concat = 0; }
	else			{ copylib = 0; concat = 1; }

	if (manual_locking && !do_locking)
	{
		write_log("WISP",'F',"OPTIONS","NODMS (-S) and MANLOCK (-M) can not both be specified.");
		exit_with_err();
	}
	
	write_log("WISP",'I',"FLAGS","Comments  = %d\nCopy Only = %d\nConcat    = %d\nCopy Lib  = %d",
		comments,copy_only,concat,copylib);

	write_log("WISP",'I',"FLAGS","Data = %d, Move = %d.\n",init_data,init_move);

	load_change_words(word_fname);

	return 0;
}

static char *copyright_display_str = 
"Copyright (c) 1989-" WISP_COPYRIGHT_YEAR_STR " NeoMedia Technologies Inc., All rights reserved.";

static void whodunit(void)
{
              /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	printf("\n\n\n");
/*	printf("WISP: Version=[%s] Library=[%d] Screen=[%d]\n\n\n",WISP_VERSION,LIBRARY_VERSION,SCREEN_VERSION); */
	printf("WISP: Version=[%s]\n\n\n",WISP_VERSION);
	printf("  Translator written by Greg Lindholm.\n");
	printf("  Library and utilities written by Greg Lindholm, Frank Dziuba, David Young,\n");
	printf("  Jock Cooper, Suzette Cass, and Gregory Adams.\n");
	printf("  Screen management and Wang workstation emulation written by Gregory Adams.\n");
	printf("  Unix implementation written by Greg Lindholm and Jock Cooper.\n");
	printf("  MS-DOS implementation written by Greg Lindholm.\n");
	printf("  Windows NT/95 implementation written by Greg Lindholm and Jock Cooper.\n");
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
/*	printf("WISP: Version=[%s] Library=[%d] Screen=[%d]\n",WISP_VERSION,LIBRARY_VERSION,SCREEN_VERSION); */
	printf("WISP: Version=[%s]\n",WISP_VERSION);
	printf("\n");
	printf("USAGE: wisp [-flags] filename\n\n");
        printf("       FLAG      DESCRIPTION\n");
        printf("        -Vxxx    COBOL: ACU,ACN,MF,MFSE\n");
        printf("        -Idir    Input copy directory.\n");
        printf("        -T       No Process. Create only a .txt file.\n");
        printf("        -x       Produce a cross reference.\n");
        printf("        -X       Produce a cross reference tab file.\n");
        printf("        -k       Use standard Key file.\n");
        printf("        -o       Use standard Option file.\n");
        printf("        -e       Don't show warning messages.\n");
        printf("        -Wfile   Use specified Word file.\n");
        printf("        -U       SHOW ALL USAGE FLAGS.\n");
        printf("        -F       SHOW FLAGS IN USE.\n");
	printf("\n");
}
static void fullusage(void)
{
	printf("%s\n",copyright_display_str);
	printf("\n");
/*	printf("WISP: Version=[%s] Library=[%d] Screen=[%d]\n",WISP_VERSION,LIBRARY_VERSION,SCREEN_VERSION); */
	printf("WISP: Version=[%s]\n",WISP_VERSION);
	printf("\n");
	printf("USAGE: wisp [-flags] filename\n\n");
        printf("       FLAG          DESCRIPTION\n");
        printf("        -L           Enable logging.\n");
        printf("        -l           Write a log summary.\n");
        printf("        -Z           Don't compress screen output.\n");
        printf("        -c           Include comments.\n");
        printf("        -s           Do not concatenate copy files.\n");
        printf("        -C           Generate copy files.\n");
        printf("        -e           Don't show warning messages.\n");
        printf("        -x           Produce a cross reference.\n");
        printf("        -X           Produce a cross reference tab file.\n");
        printf("        -T           No Process. Create only a .txt file.\n");
        printf("        -Ipath       Input copy directory paths.\n");
        printf("        -k           Use standard Key file.\n");
        printf("        -Kfile       Use specified Key file.\n");
        printf("        -o           Use standard Option file.\n");
        printf("        -Ofile       Use specified Option file.\n");
        printf("        -Wfile       Use specified Word file.\n");
        printf("        -R           Do not do keyword processing.\n");
        printf("        -S           Do not add DMS locking.\n");
        printf("        -1           One way conversion (-csC).\n");
        printf("        -Vxxx        COBOL: ACU,ACN,MF,MFSE\n");
        printf("        -n           Use VMS dynamic LINK.\n");
        printf("        -d           Don't init data areas.\n");
        printf("        -D           Do init data areas.\n");
        printf("        -m           Change MOVE SPACES to INITIALIZE.\n");
        printf("        -f           Change FD FILLERs to fields.\n");
        printf("        -w           Change WS FILLERS to fields.\n");
        printf("        -q           Generate data conversion program.\n");
        printf("        -4           Add check for database files.\n");
        printf("        -M           Use manual record locking.\n");
}
static void showflags(void)
{
	printf("\n");
	printf("The Following WISP Option are in use;\n\n");

        printf(        "       FLAG          DESCRIPTION\n");
        if ( log_stats )                                             /* No logging                           */
                printf("        -l           Write a log summary.\n");
        else				
                printf("     no -l           No log summary.\n");
        if ( compress )                                              /* Not compressed                       */
                printf("        -z           Compress screen output.\n");
        else				
                printf("        -Z           Don't compressed screen output.\n");
        if ( comments )                                              /* No comments                          */
                printf("        -c           Include comments.\n");
        else				
                printf("     no -c           NO Included comments.\n");
        if ( blanklines )                                            /* No blonklines                        */
                printf("                     Blank lines in Output.\n");
        else				
                printf("                     NO Blank lines in Output.\n");
        if ( concat )                                                /* Concatenate files                    */
                printf("     no -s           Ccncatenate copy files.\n");
        else				
                printf("        -s           Do not concatenate copy files.\n");
        if ( copylib )                                               /* No copylibs generated                */
                printf("        -C           Generate copy files.\n");
        else				
                printf("     no -C           NO Generated copy files.\n");
        if ( nowarnings )                                            /* No WARNING messages                  */
                printf("        -e           Don't show warning messages.\n");
        else				
                printf("     no -e           Show warning messages.\n");
        if ( do_xref )                                               /* No cross reference                   */
                printf("        -x           Produce a cross reference.\n");
        else				
                printf("     no -x           No cross reference.\n");
        if ( do_xtab )                                               /* No cross reference                   */
                printf("        -X           Produce a cross reference tab file.\n");
        else				
                printf("     no -X           No cross reference tab file.\n");
        if ( do_dlink )                                              /* VMS dynamic link                     */
                printf("        -n           Use VMS dynamic LINK.\n");
        else				
                printf("     no -n           Use VMS pseudo-LINK not dynamic LINK.\n");
        if ( data_conv )		
                printf("        -q           Generate data conversion program.\n");
        if ( x4dbfile )			
                printf("        -4           Add check for database files.\n");
        if ( init_data )                                             /* Init data areas                      */
                printf("        -D           Do init data areas.\n");
        else				
                printf("        -d           Don't init data areas.\n");
        if ( init_move )                                             /* Move spaces as is                    */
                printf("        -m           Change MOVE SPACES to INITIALIZE.\n");
        else				
                printf("     no -m           Leave MOVE SPACES alone.\n");
        if ( fdfiller )                                              /* FD fillers are allowed               */
                printf("     no -f           Leave FD FILLERs alone.\n");
        else				
                printf("        -f           Change FD FILLERs to fields.\n");
        if ( wsfiller )                                              /* WS fillers are allowed               */
                printf("     no -w           Leave WS FILLERS alone.\n");
        else				
                printf("        -w           Change WS FILLERS to fields.\n");
        if ( lpi_cobol )                                             
                printf("        -VLPI        Generate LPI COBOL.\n");
        if ( acu_cobol && acn_cobol)                                             
                printf("        -VACN        Generate ACUCOBOL (NATIVE) COBOL.\n");
        if ( acu_cobol && !acn_cobol)	
                printf("        -VACU        Generate ACUCOBOL COBOL.\n");
        if ( vax_cobol )                                             
                printf("        -VVAX        Generate VAX COBOL.\n");
        if ( mfoc_cobol )
                printf("        -VMF         Generate MICRO FOCUS OBJECT COBOL.\n");
        if ( mfse_cobol)                                         
                printf("        -VMFSE       Generate MICRO FOCUS Server Express COBOL.\n");
        if ( mf_cobol && symbzero)                                           
                printf("        -VMF0        Generate MICRO FOCUS COBOL. (SYMBOLIC 0=0)\n");
        if ( do_keyfile )		
                printf("        -Kfile       Use specified Key file.\n");
        else				
                printf("     no -Kfile       Don't use Key file.\n");
        if ( do_optfile )		
                printf("        -Ofile       Use specified Option file.\n");
        else				
                printf("     no -Ofile       Don't use Option file.\n");
        if ( do_locking )		
                printf("     no -S           Add DMS locking logic.\n");
        else				
                printf("        -S           Do not add DMS locking logic.\n");
        if ( manual_locking )		
                printf("        -M           Use manual record locking.\n");
        else				
                printf("     no -M           Use automatic record locking.\n");
}

/*
**	History:
**	$Log: wt_cli.c,v $
**	Revision 1.25  2002-03-28 10:04:27-05  gsl
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
**	Change copyright to NeoMedia
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
