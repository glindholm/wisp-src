			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#define EXT extern
#include "wisp.h"

int	symbzero = 0;

static int show_flags;

#ifdef VMS
#include <descrip.h>
#include <climsgdef.h>
#include <ssdef.h>

/* These are the descriptors for the parameters which could be returned by switches from DCL					*/
/* the cli_xxfile variables are defined in WISP.H										*/

$DESCRIPTOR(cli_name,cli_infile);
$DESCRIPTOR(cli_listname,cli_listfile);
$DESCRIPTOR(cli_ilname,cli_ildir);
$DESCRIPTOR(cli_kname,key_fname);
$DESCRIPTOR(cli_wname,word_fname);
$DESCRIPTOR(cli_oname,opt_fname);

char cli_tline[80];
$DESCRIPTOR(cli_tmp,cli_tline);

/* These are the descriptors for the various DCL switches allowed in the WISP command						*/

$DESCRIPTOR(cli_com,"COMMENTS");
$DESCRIPTOR(cli_cat,"CONCATENATE");
$DESCRIPTOR(cli_conv,"CONVERT");
$DESCRIPTOR(cli_copy,"COPY_LIB");
$DESCRIPTOR(cli_inlib,"INLIB");
$DESCRIPTOR(cli_key,"KEY_FILE");
$DESCRIPTOR(cli_word,"WORD_FILE");
$DESCRIPTOR(cli_list,"LIST");
$DESCRIPTOR(cli_log,"LOG");
$DESCRIPTOR(cli_lang,"LANGUAGE");
$DESCRIPTOR(cli_opt,"OPTION_FILE");
$DESCRIPTOR(cli_cmp,"COMPRESS");
$DESCRIPTOR(cli_proc,"PROCESS");
$DESCRIPTOR(cli_file,"FILENAME");
$DESCRIPTOR(cli_initd,"INITIALIZE.DATA");
$DESCRIPTOR(cli_initm,"INITIALIZE.MOVE_SPACES");
$DESCRIPTOR(cli_initfd,"INITIALIZE.FD_FILLER");
$DESCRIPTOR(cli_initws,"INITIALIZE.WS_FILLER");
$DESCRIPTOR(cli_okw,"OVERRIDE.KEYWORD");
$DESCRIPTOR(cli_oll,"OVERRIDE.LIBRARY_LOGICALS");
$DESCRIPTOR(cli_dms,"DMS");
$DESCRIPTOR(cli_xref,"CROSS_REFERENCE");
$DESCRIPTOR(cli_nowarn,"WARNINGS");
$DESCRIPTOR(cli_dlink,"DLINK");
#endif	/* VMS */

get_cli(argc,argv)
int	argc;
char	*argv[];
{
#ifdef VMS
	int status,i;

	vax_cobol = 1;									/* default to VAX cobol			*/
	copy_only = 0;

	init_data = 1;									/* init data areas (default)		*/

	status = cli$present(&cli_list);
	if (status == CLI$_PRESENT)							/* The /LIST switch is present		*/
	{
		logging = 1;								/* enable logging			*/

		status = cli$get_value(&cli_list,&cli_listname);
		if ((cli_listfile[0] == '\0') || (cli_listfile[0] == ' '))		/* No file provided though, use crt..	*/
		{
			logfile = stdout;
		}
		else									/* try to open the file			*/
		{
			cli_listfile[strpos(cli_listfile," ")] = '\0';			/* null terminate.			*/

			if (strpos(cli_listfile,".") == -1)				/* needs a .type			*/
			{
				strcat(cli_listfile,".LOG");
			}
			logfile = fopen(cli_listfile,"w");				/* open it now				*/
			if (!logfile)							/* Some error.				*/
			{
				printf("%WISP-F-NOLOGFILE Unable to open log file.\n");
				perror(0);
				exit(1);
			}
		}
	}

	status = cli$present(&cli_log);
	if ((status != CLI$_NEGATED) && (status == CLI$_PRESENT))			/*  /LOG switch is set			*/
	{										/* so write a log summary at the end	*/
		log_stats = 1;
	}
	else
	{
		log_stats = 0;
	}

	status = cli$present(&cli_cmp);
	if ((status == CLI$_NEGATED) && (status == CLI$_PRESENT))			/*  /COMPRESS switch is set		*/
	{										/* so set the flag.			*/
		compress = 0;
	}
	else
	{
		compress = 1;
	}


	status = cli$present(&cli_com);
	if ((status != CLI$_NEGATED) && (status == CLI$_PRESENT))			/*  /COMMENTS switch is set		*/
	{										/* so keep comments...			*/
		comments = 1;
		blanklines = 1;
	}
	else
	{
		comments = 0;
		blanklines = 0;
	}


	status = cli$present(&cli_cat);
	if (status == CLI$_NEGATED)							/* No Concatenation desired		*/
	{
		concat = 0;								/* No Concat is set now			*/
	}
	else
	{
		concat = 1;								/* Concat is the default		*/
	}

	status = cli$present(&cli_copy);
	if ((status == CLI$_NEGATED) || (status != CLI$_PRESENT))			/* No copy lib generation		*/
	{
		copylib = 0;
	}
	else
	{
		copylib = 1;
	}


	if (copylib && concat) concat = 0;						/* /COPY_LIB implies NOCONCAT		*/

	status = cli$present(&cli_xref);
	if ((status == CLI$_NEGATED) || (status != CLI$_PRESENT))			/* No cross reference generation	*/
	{
		do_xref = 0;
	}
	else
	{
		do_xref = 1;
	}

	status = cli$present(&cli_nowarn);
	if (status == CLI$_NEGATED)							/* /NOWARNINGS switch is set		*/
	{
		nowarnings = 1;
	}
	else
	{
		nowarnings = 0;
	}

	status = cli$present(&cli_dlink);
	if ((status == CLI$_NEGATED) || (status != CLI$_PRESENT))			/* Dynamic link				*/
	{
		do_dlink = 0;
	}
	else
	{
		do_dlink = 1;
	}

	status = cli$present(&cli_proc);
	if (status == CLI$_NEGATED)
	{										/*  /NOPROCESSING switch is set		*/
		copy_only = 1;								/* so just copy the files		*/
		comments = 1;								/* and the comments			*/
		blanklines = 1;
		concat = 1;								/* Concat the files.			*/
		copylib = 0;								/* no copy lib generation		*/
		do_xref = 0;
	}

	cli_ildir[0] = '\0';								/* clear the inlib directory name	*/

	status = cli$present(&cli_inlib);
	if (status == CLI$_PRESENT)							/* The /INLIB switch is present		*/
	{
		status = cli$get_value(&cli_inlib,&cli_ilname);				/* Get the directory name		*/
		cli_ildir[strpos(cli_ildir," ")] = '\0';				/* Null terminate.			*/
		write_log("WISP",'I',"INLIBVAL","INLIB is <%s>.",cli_ildir);
	}

	status = cli$present(&cli_key);
	if (status == CLI$_PRESENT)							/* The /KEY switch is present		*/
	{
		do_keyfile = USE_SPECIFIC_FILE;
		status = cli$get_value(&cli_key,&cli_kname);				/* Get the file name			*/
		if (status == SS$_NORMAL)
		{
			key_fname[strpos(key_fname," ")+1] = '\0';			/* Null terminate.			*/
			if (strpos(key_fname,".") == -1)
				strcat(key_fname,".KEY");
			write_log("WISP",'I',"KEYFILVAL","KEY file is <%s>.",key_fname);
		}
		else
		{
			do_keyfile = USE_GENERAL_FILE;
			key_fname[0] = '\0';
		}
	}
	else
	{
		do_keyfile = 0;
	}

	status = cli$present(&cli_word);
	if (status == CLI$_PRESENT)							/* The /WORD_FILE switch is present	*/
	{
		status = cli$get_value(&cli_word,&cli_wname);				/* Get the file name			*/
		if (status == SS$_NORMAL)
		{
			word_fname[strpos(word_fname," ")+1] = '\0';			/* Null terminate.			*/
			write_log("WISP",'I',"WORDFILEVAL","WORD file is <%s>.",word_fname);
		}
		else
		{
			printf("Error in word file specification,/WORD_FILE=\n");
			exit_wisp(-1);
		}
	}
	else
	{
		word_fname[0] = '\0';
	}

	status = cli$present(&cli_opt);
	if (status == CLI$_PRESENT)							/* The /OPTION_FILE switch is present	*/
	{
		do_optfile = USE_SPECIFIC_FILE;
		status = cli$get_value(&cli_opt,&cli_oname);				/* Get the file name			*/
		if (status == SS$_NORMAL)
		{
			opt_fname[strpos(opt_fname," ")+1] = '\0';			/* Null terminate.			*/
			if (strpos(opt_fname,".") == -1)
				strcat(opt_fname,".OPT");
			write_log("WISP",'I',"OPTFILVAL","OPTION file is <%s>.",opt_fname);
		}
		else
		{
			do_optfile = USE_GENERAL_FILE;
			opt_fname[0] = '\0';
		}
	}
	else
	{
		do_optfile = 0;
	}

	status = cli$present(&cli_initd);
	if (status == CLI$_NEGATED)						/* The /INITIALIZE=(NODATA) switch is present	*/
	{
		init_data = 0;							/* clear flag, don't init data areas		*/
	}

	status = cli$present(&cli_initm);
	if (status == CLI$_NEGATED)						/* The /INITIALIZE=(NOMOVE_SPACES) is present	*/
	{
		init_move = 1;							/* set flag, change MOVE SPACES to INITIALIZE	*/
	}
	else
	{
		init_move = 0;							/* clear flag, leave move spaces alone		*/
	}

	status = cli$present(&cli_initfd);
	if (status == CLI$_NEGATED)						/* The /INITIALIZE=(NOFD_FILLER) is present	*/
	{
		fdfiller = 0;							/* clear flag, change FILLERs in FD's to fields.*/
	}
	else
	{
		fdfiller = 1;							/* Set flag, allow fillers in fd's.		*/
	}

	status = cli$present(&cli_initws);
	if (status == CLI$_NEGATED)						/* The /INITIALIZE=(NOWS_FILLER) is present	*/
	{
		wsfiller = 0;							/* clear flag, change FILLERs in FD's to fields.*/
	}
	else
	{
		wsfiller = 1;							/* Set flag, allow fillers in fd's.		*/
	}

	status = cli$present(&cli_okw);
	if (status == CLI$_PRESENT)						/* The /OVERRIDE=(KEYWORD) is present		*/
	{
		kwproc = 0;							/* clear flag, don't check for VAX keywords.	*/
	}

	status = cli$present(&cli_oll);
	if (status == CLI$_PRESENT)						/* The /OVERRIDE=(LIBRARY_LOGICALS) is present 	*/
	{
		trans_lib = 1;							/* Set flag to translate library logicals when	*/
	}									/* createing the output .LIB file.		*/

	do_locking = 1;
	status = cli$present(&cli_dms);
	if (status == CLI$_NEGATED)						/* The /NODMS switch is present			*/
	{
		do_locking = 0;							/* don't do READ lock processing		*/
	}
	else
	{
		do_locking = 1;							/* Do READ lock processing			*/
	}

	status = cli$present(&cli_conv);
	if ((status != CLI$_NEGATED) && (status == CLI$_PRESENT))			/*  /CONVERT switch is set		*/
	{
		copy_only = 0;								/* Don't just copy the files.		*/
		comments = 1;								/* Keep the comments.			*/
		blanklines = 1;
		concat = 0;								/* Don't concat the files.		*/
		copylib = 1;								/* Generate the copy libs.		*/
	}

	status = cli$present(&cli_lang);
	if (status == CLI$_PRESENT)							/*  /LANGUAGE switch is set		*/
	{
		vax_cobol = 0;								/* Clear the VAX flag			*/
		init_data = 0;								/* Clear init data areas		*/

		memset(cli_tline,' ',sizeof(cli_tline));
		status = cli$get_value(&cli_lang,&cli_tmp);				/* now get the compiler name		*/
		if (status != SS$_NORMAL)
		{
			printf("Error in compiler specification, %s\n",cli_tline);
			exit_wisp(-1);
		}
		i = strpos(cli_tline," ");
		cli_tline[i] = '\0';

#define LPI 		0
#define ACUCOBOL	4
#define VAX_COBOL	13
#define AIX_COBOL	17
#define DMF_COBOL	21
#define MF_COBOL	25
#define MF0_COBOL	28

/*			    012345678901234567890123456789		*/
		i = strpos("LPI ACUCOBOL VAX AIX DMF MF MF0 ",cli_tline);
		switch(i)
		{
			case LPI:
			{
				lpi_cobol = 1;
				unix_cobol = 1;
				break;
			}

			case ACUCOBOL:
			{
				acu_cobol = 1;
				unix_cobol = 1;
				break;
			}

			case VAX_COBOL:
			{
				vax_cobol = 1;
				init_data = 1;						/* Init data areas with VAX cobol	*/
				break;
			}

			case AIX_COBOL:
			{
				aix_cobol = 1;
				mf_aix = 1;
				unix_cobol = 1;
				symbzero = 1;
				break;
			}

			case MF_COBOL:
			case MF0_COBOL:
			{
				mf_cobol = 1;
				mf_aix = 1;
				unix_cobol = 1;
				if (MF0_COBOL == i)
				{
					symbzero = 1;
				}
				break;
			}

			case DMF_COBOL:
			{
				dmf_cobol = 1;
				mf_aix = 1;
				dos_cobol = 1;
				break;
			}

			default:
			{
				printf("Error in compiler specification, %s\n",cli_tline);
				exit_wisp(-1);
			}
		}
		printf("Compiler selected is %s\n",cli_tline);
	}

	status = cli$get_value(&cli_file,&cli_name);					/* now get the input file name		*/

	strcpy(fname,cli_infile);

	i = strpos(cli_infile," ");
	if (i != -1) cli_infile[i] = '\0';						/* NUll terminate			*/
#else
	int	i,c;
	extern	char	*optarg;
	extern	int	optind,opterr;
	/*
		Set Defaults
	*/
	log_stats = 0;									/* No logging				*/
	compress = 1;									/* Compressed screens			*/
	comments = 0;									/* No comments				*/
	blanklines = 0;									/* No blanklines			*/
	concat = 1;									/* Concatenate files			*/
	copylib = 0;									/* No copylibs generated		*/
	do_xref = 0;									/* No cross reference			*/
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
 	aix_cobol = 0;									/* Not AIX				*/
 	mf_cobol = 0;									/* Not MF				*/
 	dmf_cobol = 0;									/* Not MF for MS-DOS			*/
 	unix_cobol = 0;									/* Not UNIX				*/
 	dos_cobol = 0;									/* Not MS-DOS				*/
	mf_aix = 0;									/* Not MF or AIX			*/

#ifdef unix
	unix_cobol = 1;
#ifdef u3b2
	lpi_cobol = 1;									/* If 3b2 then default is LPI		*/
#else
	acu_cobol = 1;									/* All others then default is ACUCOBOL	*/
#endif
#endif

#ifdef MSDOS
	dos_cobol = 1;
	dmf_cobol = 1;									/* MS-DOS default to Micro Focus	*/
	mf_aix = 1;
#endif

	do_keyfile = 0;
	do_optfile = 0;
	show_flags = 0;

	word_fname[0] = '\0';

	if (argc == 1)
	{
		printusage();
		exit(1);
	}

	while ( (c = getopt( argc, argv, "LlzZcsCxneTRS1dDFmfwI:kK:oO:V:W:U?")) != -1 )
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
			 	aix_cobol = 0;
			 	mf_cobol = 0;
			 	dmf_cobol = 0;
			 	dos_cobol = 0;
				unix_cobol = 0;
				mf_aix = 0;
				if ( 0 == memcmp( optarg, "LPI", 3 ) )
				{
					lpi_cobol = 1;
					unix_cobol = 1;
				}
				else if ( 0 == memcmp( optarg, "ACU", 3 ) )
				{
					acu_cobol = 1;
					unix_cobol = 1;
				}
				else if ( 0 == memcmp( optarg, "VAX", 3 ) )
				{
					vax_cobol = 1;
				}
				else if ( 0 == memcmp( optarg, "AIX", 3 ) )
				{
					aix_cobol = 1; mf_aix = 1;
					unix_cobol = 1;
					symbzero = 1;
				}
				else if ( 0 == memcmp( optarg, "DMF", 3 ) )
				{
					dmf_cobol = 1; mf_aix = 1; 
					dos_cobol = 1;
				}
				else if ( 0 == memcmp( optarg, "MF0", 3 ) )
				{
					mf_cobol = 1; mf_aix = 1;
					unix_cobol = 1;
					symbzero = 1;
				}
				else if ( 0 == memcmp( optarg, "MF", 2 ) )
				{
					mf_cobol = 1; mf_aix = 1;
					unix_cobol = 1;
				}
				else
				{
					printf("%WISP-F-BADCOMPILER Invalid compiler flag (-V %s).\n",optarg);
					perror(0);
					exit(1);
				}						
				break;
			}
			case 'U':
				fullusage();
				exit(1);
				break;
			case '?':
				printusage();
				exit(1);
				break;
			default:
				printf("%WISP-F-BADFLAG Invalid command line flag (-%c).\n",c);
				perror(0);
				exit(1);
				break;
		}
	}

	strcpy( fname, argv[argc-1] );							/* Get the filename to convert.		*/
	strcpy( cli_infile, fname );

	if (show_flags)
	{
		showflags();
	}

	if ( fname[0] == '-' )
	{
		printf("%WISP-F-NOFILE No filename.\n");
		perror(0);
		exit(1);
	}
#endif

	if (!strncmp(cli_infile,"WHODUNIT",8) || !strncmp(cli_infile,"whodunit",8))	/* Tell them who!			*/
	{
		whodunit();
		exit_wisp(-1);							/* Generate error exit.				*/
	}

#ifdef VMS
	write_log("WISP",'I',"DCLSWITCHES","Comments  = %d\nCopy Only = %d\nConcat    = %d\nCopy Lib  = %d",
		comments,copy_only,concat,copylib);

	write_log("WISP",'I',"DCLSWITCHES","Data = %d, Move = %d.\n",init_data,init_move);
#else
	write_log("WISP",'I',"UNIXFLAGS","Comments  = %d\nCopy Only = %d\nConcat    = %d\nCopy Lib  = %d",
		comments,copy_only,concat,copylib);

	write_log("WISP",'I',"UNIXFLAGS","Data = %d, Move = %d.\n",init_data,init_move);
#endif

	load_reswords(word_fname);

}

#define MAX_RESWORDS 100

static load_reswords(filename)
char	*filename;
{
	FILE	*fd;
	char	*ptr;
	char	buf[256];
	int	rc,i,j;

	if ( !filename || !filename[0] )
	{
		res_keywords = (char **)res_defaults;
		return(0);
	}

	fd = fopen(filename, "r");		
	if (!fd)
	{
		printf("Unable to open WORD FILE %s\n",filename);
		exit_wisp(-1);		
	}

	res_keywords = (char **)calloc(MAX_RESWORDS,sizeof(char *));
	if (!res_keywords)
	{
		printf("Unable to alloc Reserved WORD list\n");
		exit_wisp(-1);
	}

	for(i=0;i<MAX_RESWORDS;i++)
	{
		rc = fscanf(fd,"%s",buf);
		if ( rc != 1 ) break;
		for(j=0;buf[j];j++) buf[j] = toupper(buf[j]);
		res_keywords[i] = malloc(strlen(buf)+1);
		strcpy(res_keywords[i],buf);
	}
	fclose(fd);
	res_keywords[i] = "";	
}

whodunit()
{
              /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	printf("\n\n\n");
	printf("WISP: Version=[%s] Library=[%d] Screen=[%d]\n\n\n",WISP_VERSION,LIBRARY_VERSION,SCREEN_VERSION);
	printf("  Translator written by Greg Lindholm and Frank Dziuba.\n");
	printf("  Library and utilities written by Greg Lindholm, Frank Dziuba, David Young,\n");
	printf("  Jock Cooper, Suzette Boron, and Gregory Adams.\n");
	printf("  Screen management and Wang workstation emulation written by Gregory Adams.\n");
	printf("  Unix implementation written by Greg Lindholm and Jock Cooper.\n");
	printf("  MS-DOS implementation written by Dev Bradley and Greg Lindholm.\n");
	printf("\n");
	printf("  Many thanks to our clients for their assistance and patience, without which\n");
	printf("  this product would not exist.\n");
	printf("\n");
	printf("  Copyright (c) 1987, 1988, 1989, 1990, 1991 an unpublished work by\n");
	printf("  International Digital Scientific Incorporated of Valencia California.\n");
	printf("  All rights reserved.\n\n\n\n\n");
}

#ifndef VMS
printusage()
{
	printf("\n");
	printf("WISP: Version=[%s] Library=[%d] Screen=[%d]\n",WISP_VERSION,LIBRARY_VERSION,SCREEN_VERSION);
	printf("\n");
	printf("USAGE: wisp [-flags] filename\n\n");
	printf("       FLAG     VMS SWITCH		DESCRIPTION\n");
	printf("	-Vxxx	/LANGUAGE=xxx		COBOL: LPI,ACU,VAX,AIX,DMF,MF,MF0\n");
	printf("	-Idir	/INLIB=			Input copy directory.\n");
	printf("	-T	/NOPROCESS		Create only a .txt file.\n");
	printf("	-x	/CROSS_REFERENCE	Produce a cross reference.\n");
	printf("	-k	/KEY_FILE		Use standard Key file.\n");
	printf("	-o 	/OPTION_FILE		Use standard Option file.\n");
	printf("	-e 	/NOWARNINGS		Don't show warning messages.\n");
	printf("	-U				SHOW ALL USAGE FLAGS.\n");
	printf("	-F				SHOW FLAGS IN USE.\n");
	printf("\n");
}
fullusage()
{
	printf("\n");
	printf("WISP: Version=[%s] Library=[%d] Screen=[%d]\n",WISP_VERSION,LIBRARY_VERSION,SCREEN_VERSION);
	printf("\n");
	printf("USAGE: wisp [-flags] filename\n\n");
	printf("       FLAG     VMS SWITCH		DESCRIPTION\n");
	printf("	-L	/LIST			Enable logging.\n");
	printf("	-l	/LOG			Write a log summary.\n");
	printf("	-Z	/NOCOMPRESS		Don't compress screen output.\n");
	printf("	-c	/COMMENTS		Include comments.\n");
	printf("	-s	/NOCONCATENATE		Do not concatenate copy files.\n");
	printf("	-C	/COPY_LIB		Generate copy files.\n");
	printf("	-e 	/NOWARNINGS		Don't show warning messages.\n");
	printf("	-x	/CROSS_REFERENCE	Produce a cross reference.\n");
	printf("	-T	/NOPROCESS		Create only a .txt file.\n");
	printf("	-Ipath	/INLIB=			Input copy directory.\n");
	printf("	-k	/KEY_FILE		Use standard Key file.\n");
	printf("	-Kfile	/KEY_FILE=file		Use specified Key file.\n");
	printf("	-o 	/OPTION_FILE		Use standard Option file.\n");
	printf("	-Ofile	/OPTION_FILE=file	Use specified Option file.\n");
	printf("	-Wfile	/WORD_FILE=file		Use specified Word file.\n");
	printf("	-R	/OVERRIDE=(KEYWORD)	Do not do keyword processing.\n");
	printf("	-S	/NODMS     		Do not add DMS locking.\n");
	printf("	-1	/CONVERT		One way conversion (-csC).\n");
	printf("	-Vxxx	/LANGUAGE=xxx		COBOL: LPI,ACU,VAX,AIX,DMF,MF,MF0\n");
	printf("	-n	/DLINK			Use VMS dynamic LINK.\n");
	printf("	-d	/INIT=(NODATA)		Don't init data areas.\n");
	printf("	-D	/INIT=(DATA)		Do init data areas.\n");
	printf("	-m	/INIT=(NOMOVE)		Change MOVE SPACES to INITIALIZE.\n");
	printf("	-f	/INIT=(NOFD_FILLER)	Change FD FILLERs to fields.\n");
	printf("	-w	/INIT=(NOWS_FILLER)	Change WS FILLERS to fields.\n");
}
showflags()
{
	printf("\n");
	printf("The Following WISP Option are in use;\n\n");

	printf("       FLAG     VMS SWITCH		DESCRIPTION\n");
	if ( log_stats )								/* No logging				*/
		printf("        -l	/LOG			Write a log summary.\n");
	else
		printf("     no -l	/NOLOG			No log summary.\n");
	if ( compress )									/* Not compressed			*/
		printf("	-z	/COMPRESS		Compress screen output.\n");
	else
		printf("        -Z	/NOCOMPRESS		Don't compressed screen output.\n");
	if ( comments )									/* No comments				*/
		printf("	-c	/COMMENTS		Include comments.\n");
	else
		printf("     no -c	/NOCOMMENTS		NO Included comments.\n");
	if ( blanklines )								/* No blonklines			*/
		printf("					Blank lines in Output.\n");
	else
		printf("					NO Blank lines in Output.\n");
	if ( concat )		  							/* Concatenate files			*/
		printf("     no -s	/CONCATENATE		Ccncatenate copy files.\n");
	else
		printf("	-s	/NOCONCATENATE		Do not concatenate copy files.\n");
	if ( copylib )									/* No copylibs generated		*/
		printf("	-C	/COPY_LIB		Generate copy files.\n");
	else
		printf("     no -C	/NOCOPY_LIB		NO Generated copy files.\n");
	if ( nowarnings )								/* No WARNING messages			*/
		printf("	-e	/NOWARNINGS		Don't show warning messages.\n");
	else
		printf("     no -e	/WARNINGS		Show warning messages.\n");
	if ( do_xref )									/* No cross reference			*/
		printf("	-x	/CROSS_REFERENCE	Produce a cross reference.\n");
	else
		printf("     no -x	/NOCROSS_REFERENCE	No cross reference table.\n");
	if ( do_dlink )									/* VMS dynamic link			*/
		printf("	-n	/DLINK			Use VMS dynamic LINK.\n");
	else
		printf("     no -n	/NODLINK		Use VMS pseudo-LINK not dynamic LINK.\n");
	if ( init_data )								/* Init data areas			*/
		printf("	-D	/INIT=(DATA)		Do init data areas.\n");
	else
		printf("	-d	/INIT=(NODATA)		Don't init data areas.\n");
	if ( init_move )								/* Move spaces as is			*/
		printf("	-m	/INIT=(NOMOVE)		Change MOVE SPACES to INITIALIZE.\n");
	else
		printf("     no -m	/INIT=(MOVE)		Leave MOVE SPACES alone.\n");
	if ( fdfiller )									/* FD fillers are allowed		*/
		printf("     no -f	/INIT=(FD_FILLER)	Leave FD FILLERs alone.\n");
	else
		printf("	-f	/INIT=(NOFD_FILLER)	Change FD FILLERs to fields.\n");
	if ( wsfiller )									/* WS fillers are allowed		*/
		printf("     no -w	/INIT=(WS_FILLER)	Leave WS FILLERS alone.\n");
	else
		printf("	-w	/INIT=(NOWS_FILLER)	Change WS FILLERS to fields.\n");
	if ( lpi_cobol )								
		printf("	-VLPI	/LANGUAGE=LPI		Generate LPI COBOL.\n");
	if ( acu_cobol )								
		printf("	-VACU	/LANGUAGE=ACU		Generate ACUCOBOL COBOL.\n");
 	if ( vax_cobol )								
		printf("	-VVAX	/LANGUAGE=VAX		Generate VAX COBOL.\n");
 	if ( aix_cobol )								
		printf("	-VAIX	/LANGUAGE=AIX		Generate AIX VS COBOL.\n");
 	if ( mf_cobol && !symbzero )								
		printf("	-VMF	/LANGUAGE=MF		Generate MICRO FOCUS COBOL.\n");
 	if ( dmf_cobol )								
		printf("	-VDMF	/LANGUAGE=DMF		Generate MICRO FOCUS MS-DOS COBOL.\n");
 	if ( mf_cobol && symbzero)								
		printf("	-VMF0	/LANGUAGE=MF0		Generate MICRO FOCUS COBOL. (SYMBOLIC 0=0)\n");
	if ( do_keyfile )
		printf("	-Kfile	/KEY_FILE=file		Use specified Key file.\n");
	else
		printf("     no -Kfile	/NOKEY_FILE=file	Don't use Key file.\n");
	if ( do_optfile )
		printf("	-Ofile	/OPTION_FILE=file	Use specified Option file.\n");
	else
		printf("     no -Ofile	/NOOPTION_FILE=file	Don't use Option file.\n");
	if ( do_locking )
		printf("     no -S	/DMS			Add DMS locking logic.\n");
	else
		printf("        -S	/NODMS			Do not add DMS locking logic.\n");
}
#endif
