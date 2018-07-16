			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT
#define INIT_COMMON

#ifdef VMS
#include <descrip.h>
#endif

#ifdef MSDOS
#include <time.h>
#endif

#include "wisp.h"

static char *copyright = "(c) 1988,89,90,91 International Digital Scientific Inc.";
static struct tbuffer { int cpu; int x,y,z; } time_now;					/* Time reference structure.		*/
static int cpu_time;									/* Initial CPU time.			*/
static char xobuff[OUTPUT_BUFFER_SIZE+1];						/* the output buffer			*/
static char *sargv[50];									/* Save the argv			*/

main(argc, argv)
int	argc;
char	*argv[];
{
	int i;
	char tstr[80];

#ifdef DEMO
	if (!demovalidate()) demoexit();
#endif

	initialize1(argc,argv);

	get_cli(argc,sargv);								/* parse the CLI switches		*/

	initialize2();

	open_io();									/* open the WCB and COB files		*/

#ifdef DEMO
	demo_message();
#endif

	for(;;)
	{
		get_line();								/* while there are lines to get...	*/
		if (copy_only)								/* copy only flag			*/
		{
			sprintf(tstr,"%06d",num_inlines);
			if (strlen(inline) > 6) memcpy(inline,tstr,6);			/* Put in line numbers if possible.	*/
			put_line(inline);
		}
		else	
		{
			wisp();								/* WISP it, eh?				*/
		}
	}

}

wisp()
{

/*	Now this section of the program takes actions based on what part of the program we are in. For example, it is 		*/
/*	not necessary to scan for the SOURCE-COMPUTER declarative when we are not in the environment division, because it	*/
/* 	only occurs there. Also, parsing a screen record could accidently take place in the procedure division if the 		*/
/*	programmers choice of variable names was not a "good" one.								*/

	check_div();								/* parse area A and see if we have 	*/
											/* entered into a new division		*/
	switch(division)
	{
	case IDENTIFICATION_DIVISION:
		p_ident();						/* process IDENTIFICATION DIVISION	*/
		break;

	case ENVIRONMENT_DIVISION:
		p_environ();
		break;

	case INPUT_OUTPUT_SECTION:					/* we are in the input-output section	*/
		p_input();
		break;

	case DATA_DIVISION:						/* we are in the data-division section	*/
		p_data();
		break;

	case WORKING_STORAGE_SECTION:					/* we are in the working stor section	*/
		p_wstor();
		break;

	case PROCEDURE_DIVISION:					/* we are in the procedure  section	*/
		p_proc();
		break;
	} 									/* end of switch 			*/
}



exit_wisp(err)									/* exit WISP, clean up 				*/
int err;
{
	int i,j,k,a,b,rc;
	c_list *tptr;									/* if not an error-exit			*/
	char tstr[256];

	if (!err && !copy_only) scrn_para();						/* write all the screen paragraphs	*/

	if (prog_cnt || lock_clear_para) gen_unlocks();					/* Since there were files, write UNLOCKs*/

	if (read_name[0])								/* was a file for read's created?	*/
	{
		fclose(read_temp_file);							/* close the file with READ's in it	*/
		if (!err)								/* if no error exit then		*/
		{
			copy_file(read_name);						/* copy it into the WISP SECTION	*/
		}
		delete(read_name);							/* and delete it...			*/
	}

	if (par_file) fclose(par_file);							/* close the paragraph file.		*/
	if (decl_file)
	{
		fclose(decl_file);							/* close the paragraph file.		*/
		if (!err)
		{
			copy_file(decl_fname);						/* Copy it into the wisp section.	*/
		}
		delete(decl_fname);							/* And delete it.			*/
	}

	if ( !err && !copy_only) wisp_exit_para();					/* write WISP-EXIT-PROGRAM.		*/

	if (fo_cnt) wflush();								/* if there is output, flush it		*/
	close(outfile);									/* and close the outfile		*/

	if (!err && do_xref)								/* Generate cross reference file.	*/
	{
		xref_file = fopen(xref_fname,"w");
		fprintf(xref_file,"Cross reference listing for file %s.\n",fname);
		fprintf(xref_file,"Files referenced in SELECT statements.\n\n ");

		for (i=0,j=0; i<prog_cnt; i++,j++)
		{
			if (prog_ref[i]) fprintf(xref_file," ");			/* Put an * if the file is never opened.*/
			else		 fprintf(xref_file,"*");

			fprintf(xref_file,"%-32s ",prog_files[i]);
			if (j == 1)
			{
				j = -1;
				fprintf(xref_file,"\n ");
			}
		}

		tptr = xref_ptr;

		fprintf(xref_file,"\n\nSubroutines called.\n\n");

		do									/* Now print the subroutine xref.	*/
		{
			for (i=0, j=0; i<tptr->call_count; i++, j++)
			{
				fprintf(xref_file,"%-3d%-9s ",tptr->ref_count[i],tptr->call_list[i]);
				if (j == 5)
				{
					j = -1;
					fprintf(xref_file,"\n");
				}
			}
			tptr = tptr->next_list;
		} while (tptr);

		fprintf(xref_file,"\n\nCopy files used.\n\n");

		if (rcpy_list_ring)
		{
			fprintf(xref_file,"FILE       LIBRARY    NATIVE FILE PATH\n");
			fprintf(xref_file,"========   ========   ================\n");
			while(0 == (rc = ring_pop(rcpy_list_ring,&rcpy_element)))
			{
				fprintf(xref_file,"%-8.8s   %-8.8s   %s\n",
					rcpy_element.file, rcpy_element.lib, rcpy_element.native);
			}
		}
		fprintf(xref_file,"\n");
		fclose(xref_file);
	}

#ifdef MSDOS
	cpu_time = clock() - cpu_time;							/* Get the number of CPU "ticks".	*/
	j = cpu_time / CLK_TCK;								/* Convert "ticks" to integer seconds.	*/
	k = ((( cpu_time * 100 )) / CLK_TCK ) - ( j * 100 );				/* Get remainder 100ths of seconds.	*/
#else		/* unix and VMS */
	times(&time_now);								/* Get the current CPU value.		*/
	cpu_time = time_now.cpu - cpu_time;						/* Save it.				*/

	j = cpu_time / 100;								/* Convert to seconds.			*/
	k = cpu_time - (j * 100);							/* Convert fraction of seconds.		*/
#endif
	write_log(" ",' '," ","\nEXECUTION STATISTICS WISP %s\n",WISP_VERSION);
	write_log(" ",' '," ","Elapsed CPU                   %d\056%ds.",j,k);		/* Display the time report item.	*/
	write_log(" ",' '," ","Number of screens processed   %d.",num_screens);
	write_log(" ",' '," ","Number of lines written       %d.",out_number);
	write_log(" ",' '," ","Number of lines read          %d.",num_inlines);
	write_log(" ",' '," ","Number of Comment lines       %d.",num_comments);
	write_log(" ",' '," ","Number of SELECT statements   %d.",prog_cnt);
	a = num_inlines-num_comments;
	write_log(" ",' '," ","Total lines processed         %d.",a);

	if (log_stats && !logging)							/* /LOG switch for stats is on		*/
	{										/* and not writing a log file		*/
		printf("\nEXECUTION STATISTICS, WISP %s\n\n",WISP_VERSION);
		printf("Elapsed CPU                   %d\056%ds.\n",j,k);		/* Display the time report item.	*/
		printf("Number of screens processed   %d.\n",num_screens);
		printf("Number of lines written       %d.\n",out_number);
		printf("Number of lines read          %d.\n",num_inlines);
		printf("Number of Comment lines       %d.\n",num_comments);
		printf("Number of SELECT statements   %d.\n",prog_cnt);
		a = num_inlines-num_comments;
		printf("Total lines processed         %d.\n",a);
	}

	if (err == EXIT_AND_RUN)							/* If they want, re run wisp.		*/
	{
		delete(lib_file);							/* delete the library file		*/
		delete(out_fname);							/* Delete the .COB file			*/
		run_wisp();
	}
	else if (used_wrk)								/* Normal exit, used workfile.		*/
	{
		delete(work_fname);							/* So delete it.			*/
	}


#ifdef VMS
	exit(1);
#else
        exit(0);
#endif
}

wisp_exit_code(prefix)
char *prefix;
{
	put_line			("\n");
	put_line			("      ********* WISP EXIT CODE\n");
	put_line			("\n");
	write_line			("       %sWISP-EXIT-PROGRAM.\n",prefix);
	if (!vax_cobol)
	{
		put_line		("           CALL \"setretcode\" USING WISPRETURNCODE.\n");
		put_line		("           IF WISP-APPLICATION-NAME = WISPRUNNAME\n");
		put_line		("               CALL \"WISPEXIT\".\n");
#ifdef OLD
		if (acu_cobol) put_line	("               CALL \"ACUPARGS\"\n");
		put_line		("               CALL \"wexith\"\n");
		put_line		("               CALL \"vexit\".\n");
#endif
	}
	put_line			("           EXIT PROGRAM.\n");
	write_line			("       %sWISP-STOP-RUN.\n",prefix);
	if (!vax_cobol)
	{
		if ( dmf_cobol && ! keepstop )
		{
			write_line	("           PERFORM %sWISP-EXIT-PROGRAM.\n",prefix);
		}
		put_line		("           CALL \"setretcode\" USING WISPRETURNCODE.\n");
		put_line		("           CALL \"WISPEXIT\".\n");
#ifdef OLD
		if (acu_cobol) put_line	("           CALL \"ACUPARGS\".\n");
		put_line		("           CALL \"wexith\".\n");
		put_line		("           CALL \"vexit\".\n");
#endif
        	put_line		("           STOP RUN.\n");
	}
	else if (do_dlink || keepstop)
	{
        	put_line		("           STOP RUN.\n");
	}
	else
	{
		put_line		("           EXIT PROGRAM.\n");
	}
	put_line			("\n");
}

wisp_exit_para()
{
	wisp_exit_code("");

	if (linkmain)
	{
		write_line		("       END PROGRAM %s.\n\n",prog_id);
		put_line		("       END PROGRAM WISPLINK.\n");
	}
}

d_wisp_exit_para()
{
	if (!decl_stop_exit) return(0);

	wisp_exit_code("D-");
}

static char the_com[256];
#ifdef VMS
static $DESCRIPTOR(com_desc,the_com);
#endif

static run_wisp()									/* Execute the WISP command again	*/
{
	int i;

#ifdef VMS
	memcpy(the_com,"WISP ",5);							/* Start with the command		*/

	com_desc.dsc$a_pointer += 5;							/* Set pointer 5 chars over.		*/
	com_desc.dsc$w_length -= 5;							/* Decrease length by 5.		*/

	i = lib$get_foreign(&com_desc);							/* Now get the rest of the command line	*/

	com_desc.dsc$a_pointer -= 5;							/* Set pointer back 5 chars.		*/
	com_desc.dsc$w_length += 5;							/* Increase length by 5.		*/
#else
	the_com[0] = 0;
	for( i=0; sargv[i]; i++ )
	{
		if ( i > 0 )
			strcat( the_com, " " );
		strcat( the_com, sargv[i] );
	}
#endif
	printf("Beginning New execution of %s\n",the_com);

#ifdef VMS
	lib$do_command(&com_desc);
#else
	execvp( sargv[0], sargv );
#endif
}

#ifndef VMS
delete( path )
char *path;
{
	return( unlink( path ) );
}
#endif

static initialize1(argc,argv)								/* Perform all generic initialization 	*/
int	argc;
char	*argv[];
{
	int	i;

#ifdef MSDOS
	cpu_time = clock();								/* Get the number of CPU "ticks".	*/
#else		/* unix and VMS */
	times(&time_now);								/* Get the current CPU value.		*/
	cpu_time = time_now.cpu;							/* Save it.				*/
#endif

	for (i=0; i<argc; i++ ) sargv[i] = argv[i];					/* Save the argv			*/
	sargv[argc] = 0;								/* Null terminate sargv			*/

	fo_buff = &xobuff[0];								/* point to the output buffer		*/

	if (fo_buff == 0)
	{
		printf("%WISP-F-ERRORALLBUFF  Error allocating %d bytes for output buffer.\n",OUTPUT_BUFFER_SIZE);
		exit(-1);
	}
	fo_cnt = 0;									/* nothing in it yet			*/

	logfile = stdout;								/* Define a file for error logs		*/

	crt_relative[0][0] = 0;								/* no relative key yet			*/
	crt_file[0][0] = 0;
	brkline[0] = '\0';								/* no break line either.		*/

	for (i=0; i<MAX_SCREENS; i++) scrn_flags[i] = 0;				/* Init screen flags now.		*/

	com_ptr = com_buf;								/* Set comment ptr to comment buffer.	*/

}

static initialize2()									/* Perform switch specific initialize	*/
{
	if (vax_cobol)
	{
		strcpy(bin2_type,"COMP");
		strcpy(bin4_type,"COMP");
		strcpy(packdec,"COMP-3");
		strcpy(hard_lock,"92");
		strcpy(soft_lock,"90");
		strcpy(cobol_type,"VAX");
	}
	if (lpi_cobol)
	{
		strcpy(bin2_type,"COMP");
		strcpy(bin4_type,"COMP");
		strcpy(packdec,"COMP-3");
		strcpy(hard_lock,"99");
		*soft_lock='\0';
		strcpy(cobol_type,"LPI");
	}
	if (acu_cobol)
	{
		strcpy(bin2_type,"COMP-1");
		strcpy(bin4_type,"COMP-4");
		strcpy(packdec,"COMP-3");
		strcpy(hard_lock,"99");
		*soft_lock='\0';
		strcpy(cobol_type,"ACU");
	}
	if (aix_cobol)
	{
		strcpy(bin2_type,"COMP-5");
		strcpy(bin4_type,"COMP");
		strcpy(packdec,"COMP-3");
		strcpy(hard_lock,"9D");
		*soft_lock='\0';
		strcpy(cobol_type,"AIX");
	}
	if (mf_cobol)
	{
		strcpy(bin2_type,"COMP-5");
		strcpy(bin4_type,"COMP-5");
		strcpy(packdec,"COMP-3");
		strcpy(hard_lock,"9D");
		*soft_lock='\0';
		strcpy(cobol_type,"MF ");
	}
	if (dmf_cobol)
	{
		strcpy(bin2_type,"COMP-5");
		strcpy(bin4_type,"COMP-5");				/* MF always uses Big-Endian except COMP-5		*/
		strcpy(packdec,"COMP-3");
		strcpy(hard_lock,"9D");
		*soft_lock='\0';
		strcpy(cobol_type,"DMF");
	}

	if (do_xref)									/* Init xref structure.			*/
	{
		xref_ptr = (struct c_list *)malloc(sizeof(c_list));  			/* Get some memory.			*/
		xref_ptr->call_count = 0;
		xref_ptr->next_list = 0;
	}
}

#ifdef DEMO
static demo_message()
{
		/*      123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 */
		printf("**** WISP Version %s  EVALUATION COPY ****\n\n",WISP_VERSION);
		printf("The software converted by this copy of WISP is for evaluation purposes only\n");
		printf("and must be destroyed upon the end of the evaluation period.\n\n");
		printf("This program contains proprietary information that is the legal property of\n");
		printf("International Digital Scientific Inc.\n\n");
		printf("WISP is a trademark of International Digital Scientific Incorporated,\n");
		printf("Valencia California.\n\n");
		printf("For assistance contact:\n\n");
		printf("                 International Digital Scientific Incorporated\n");
		printf("                        28460 Avenue Stanford, Suite 100,\n");
		printf("                              Valencia CA 91355\n");
		printf("                            Phone: (805) 295-1155\n");
		printf("                            Fax:   (805) 295-8755\n\n");


		write_line("      *  *****  WISP Version %s  EVALUATION COPY ***** \n",WISP_VERSION);
		put_line("      * \n");
		put_line("      *  This program contains proprietary information that is the\n");
		put_line("      *  legal property of International Digital Scientific Inc.\n");
		put_line("      *\n");
		put_line("      *  This program was translated by an evaluation copy of WISP.\n");
		put_line("      *  The purpose of this evaluation software is to allow the\n");
		put_line("      *  prospective purchaser of WISP to preview the product so as to\n");
		put_line("      *  evaluate its capabilities.  This is the only permitted use of\n");
		put_line("      *  this software.  This converted software may not be used in a \n");
		put_line("      *  production environment, it may not be sold or distributed in\n");
		put_line("      *  any manner, it may not be included or embedded within other\n");
		put_line("      *  software.  The possessor of this software has no rights to this\n");
		put_line("      *  software.\n");
		put_line("      *  \n");
		put_line("      *  This converted software must be DESTROYED upon the end of\n");
		put_line("      *  the evaluation period.\n");
		put_line("      *\n");
		put_line("      *  WISP is a trademark of International Digital Scientific Inc.,\n");
		put_line("      *  Valencia California.\n");
		put_line("      *\n");
		put_line("      *  COPYRIGHT (C) 1989,1990,1991 by International Digital \n");
		put_line("      *  Scientific Incorporated. All rights reserved.\n");
		put_line("      *\n");
		put_line("      *  For assistance contact:\n");
		put_line("      *\n");
		put_line("      *           International Digital Scientific Incorporated\n");
		put_line("      *                  28460 Avenue Stanford, Suite 100,\n");
		put_line("      *                        Valencia CA 91355\n");
		put_line("      *                      Phone: (805) 295-1155\n");
		put_line("      *                      Fax:   (805) 295-8755\n");
		put_line("      *\n");

}

#include "demovali.h"
#endif



