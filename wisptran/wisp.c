			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef VMS
#include <descrip.h>
#endif

#ifdef MSDOS
#include <time.h>
#endif

#include <string.h>

#define EXT
#define INIT_COMMON

#include "wisp.h"
#include "scrn.h"
#include "wispfile.h"
#include "crt.h"
#include "cobfiles.h"
#include "keylist.h"
#include "directiv.h"
#include "wmalloc.h"

#include "wcommon.h"

static char *copyright = "(c) 1988-1993 International Digital Scientific Inc.";
static struct tbuffer { int cpu; int x,y,z; } time_now;					/* Time reference structure.		*/
static int cpu_time;									/* Initial CPU time.			*/
static char *sargv[50];									/* Save the argv			*/
static char *targv[50];									/* argv after response file processing	*/
static int targc;									/* argc to match targv			*/
static int run_wisp();
static int initialize1();
static int initialize2();
static int response_file();

main(argc, argv)
int	argc;
char	*argv[];
{

#ifdef DEMO
	if (!demovalidate()) demoexit();
#endif

	initialize1(argc,argv);

	get_cli(targc,targv);								/* parse the CLI switches		*/

	initialize2();

	open_io();									/* open the WCB and COB files		*/

#ifdef DEMO
	demo_message();
#endif

	if (copy_only)
	{
		gen_txt_file();
	}
	else if (data_conv)
	{
		gen_data_conv();
	}
	else
	{
		wisp();
	}

}

gen_txt_file()
{
	int	lstat;
	char	tstr[10];
	int	cnt;
	char	*ptr;

	cnt = 0;
	while(EOF != (lstat=get_cobol_inline()))
	{
		if (ptr = strchr(inline,'\n')) *ptr = (char)0;
		sprintf(tstr,"%06d",++cnt);
		if (strlen(inline) >= 6) 
			memcpy(inline,tstr,6);
		else
			strcpy(inline,tstr);

		tput_noprocess(inline);
	}
	exit_wisp(EXIT_OK);
}

wisp()
{
	NODE	the_statement;

	the_statement = (NODE)get_statement();

	/*
	**	IDENTIFICATION DIVISION
	*/
	the_statement = (NODE)identification_division(the_statement);

	/*
	**	ENVIRONMENT DIVISION
	*/
	the_statement = (NODE)environment_division(the_statement);

	/*
	**	DATA DIVISION
	*/
	the_statement = (NODE)data_division(the_statement);

	/*
	**	PROCEDURE DIVISION
	*/
	procedure_division(the_statement);

	exit_wisp(EXIT_OK);
}

static int need_to_rerun_flag = 0;
static char *need_to_rerun_reason;

need_to_rerun(reason)
char	*reason;
{
	if (!need_to_rerun_flag)
	{
		need_to_rerun_reason = (char *)wdupstr(reason);
	}
	else
	{
		char	*ptr;
		ptr = wmalloc(strlen(need_to_rerun_reason)+strlen(reason)+1);
		strcpy(ptr,need_to_rerun_reason);
		strcat(ptr,reason);
		wfree(need_to_rerun_reason);
		need_to_rerun_reason = ptr;
	}
	need_to_rerun_flag = 1;
}

static int delayed_error = 0;

delayed_exit_with_err()
{
	delayed_error = 1;
}

exit_with_err()
{
	exit_wisp(EXIT_WITH_ERR);
}

exit_wisp(err)										/* exit WISP, clean up 			*/
int err;
{
	int i,j,k,a,b,rc;
	c_list *tptr;									/* if not an error-exit			*/
	char tstr[256];

	if (err == EXIT_FAST) goto exit_fast;

	if (delayed_error) 
	{
		err = EXIT_WITH_ERR;
	}

	if (!err && !copy_only && !data_conv) 
	{
		scrn_para();								/* write all the screen paragraphs	*/
	}

	if (!err && !copy_only && !data_conv && (prog_cnt || lock_clear_para)) 
	{
		gen_unlocks();								/* Since there were files, write UNLOCKs*/
	}

	if (read_file_ptr)								/* was a file for read's created?	*/
	{
		close_cob_file(read_file_ptr);						/* close the file with READ's in it	*/
		if (!err)								/* if no error exit then		*/
		{
			copy_file(read_fname);						/* copy it into the WISP SECTION	*/
		}
		delete(read_fname);							/* and delete it...			*/
	}

	if (dcl_file_ptr) close_cob_file(dcl_file_ptr);					/* close the paragraph file.		*/
	if (dtp_file_ptr)
	{
		close_cob_file(dtp_file_ptr);						/* close the paragraph file.		*/
		if (!err)
		{
			copy_file(dtp_fname);						/* Copy it into the wisp section.	*/
		}
		delete(dtp_fname);							/* And delete it.			*/
	}

	if ( !err && !copy_only && !data_conv) wisp_exit_para();			/* write WISP-EXIT-PROGRAM.		*/

	if ( !err )
	{
		tput_flush();								/* Flush the put_token() buffers	*/
	}
	close_all_cob_files();								/* Close all the cob files		*/


	if (EXIT_WITH_ERR != err)
	{
		if (need_to_rerun_flag || err == EXIT_AND_RUN)				/* If they want, re run wisp.		*/
		{
			delete(dcl_fname);						/* delete the library file		*/
			delete(out_fname);						/* Delete the .COB file			*/
			run_wisp();
		}
	}


	if (!err && do_xref)								/* Generate cross reference file.	*/
	{
		FILE 	*xref_file;							/* The cross ref file.			*/
		xref_file = fopen(xref_fname,"w");
		fprintf(xref_file,"Cross reference listing for file %s.\n",in_fname);
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
	write_log(" ",' '," ","Number of lines written       %d.",out_line_count());
	write_log(" ",' '," ","Number of lines read          %d.",in_line_count());
	write_log(" ",' '," ","Number of Comment lines       %d.",comment_line_count());
	write_log(" ",' '," ","Number of SELECT statements   %d.",prog_cnt);
	a = in_line_count() - comment_line_count();
	write_log(" ",' '," ","Total lines processed         %d.",a);

	if (log_stats && !logging)							/* /LOG switch for stats is on		*/
	{										/* and not writing a log file		*/
		printf("\nEXECUTION STATISTICS, WISP %s\n\n",WISP_VERSION);
		printf("Elapsed CPU                   %d\056%ds.\n",j,k);		/* Display the time report item.	*/
		printf("Number of screens processed   %d.\n",num_screens);
		printf("Number of lines written       %d.\n",out_line_count());
		printf("Number of lines read          %d.\n",in_line_count());
		printf("Number of Comment lines       %d.\n",comment_line_count());
		printf("Number of SELECT statements   %d.\n",prog_cnt);
		a = in_line_count() - comment_line_count();
		printf("Total lines processed         %d.\n",a);
	}

	if (used_wrk)									/* Normal exit, used workfile.		*/
	{
		delete(work_fname);							/* So delete it.			*/
	}

	delete(par_fname);

exit_fast:

#ifdef VMS
	if (err == EXIT_OK) exit(1);
	exit(0);
#else
	if (err == EXIT_OK) exit(0);
        exit(1);
#endif
}

wisp_exit_code(prefix)
char *prefix;
{
	int	stoprun;

	tput_blank();
	tput_scomment			("****** WISP EXIT CODE ******");
	tput_blank();

	tput_line			("       %sWISP-EXIT-PROGRAM.",prefix);
	if (!vax_cobol)
	{
		tput_line		("           CALL \"setretcode\" USING WISPRETURNCODE.");
		tput_line		("           IF WISP-APPLICATION-NAME = WISPRUNNAME");
		tput_line		("               CALL \"WISPEXIT\".");
	}
	tput_line			("           EXIT PROGRAM.");
	tput_line			("       %sWISP-STOP-RUN.",prefix);
	if (!vax_cobol)
	{
		tput_line		("           CALL \"setretcode\" USING WISPRETURNCODE.");
		tput_line		("           CALL \"WISPEXIT\".");
	}

	stoprun = 1;
	if (vax_cobol && !do_dlink)
	{
		stoprun = 0;
	}
	if (dmf_cobol) 	stoprun = 0;
	if (keepstop) 	stoprun = 1;
	if (changestop) stoprun = 0;

	if (stoprun)
	{
        	tput_line		("           STOP RUN.");
	}
	else
	{
		tput_line		("           EXIT PROGRAM.");
	}
	tput_blank();
}

wisp_exit_para()
{
	wisp_exit_code("");

	if (linkmain)
	{
		tput_line		("       END PROGRAM %s.",prog_id);
		tput_blank();
		tput_line		("       END PROGRAM WISPLINK.");
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
	printf("\nBeginning New execution of [%s]\n",the_com);
	if (need_to_rerun_reason)
	{
		printf("REASON:\n%s\n",need_to_rerun_reason);
	}
	fflush(stdout);
	fflush(stderr);

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

	response_file(argc, sargv);							/* Process any response file.		*/

	logfile = stdout;								/* Define a file for error logs		*/

	crt_relative[0][0] = 0;								/* no relative key yet			*/
	crt_file[0][0] = 0;

	for (i=0; i<MAX_SCREENS; i++) scrn_flags[i] = 0;				/* Init screen flags now.		*/
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
		xref_ptr = (struct c_list *)wmalloc(sizeof(c_list));  			/* Get some memory.			*/
		xref_ptr->call_count = 0;
		xref_ptr->next_list = 0;
	}
}

char *packed_decimal()
{
	return(packdec);
}

/*
**	Routine:	response_file()
**
**	Function:	To handle a response file in the arg list.
**
**	Description:	This routine loads globals targc and targv from the arg list plus any response file.
**			If an argument has the form "@filename" then the file is read for additional arguments.
**			This are inserted into targv in the order found.
**
**			This was done to handle MSDOS limit of 128 characters on a command line. 
**
**	Arguments:
**	argc		The original arg count
**	argv		The original arg list.
**
**	Globals:
**	targc		The expanded arg count.
**	targv		The expanded arg list.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	01/22/93	Written by GSL
**
*/
static response_file(argc,argv)
int	argc;
char	*argv[];
{
	int	i;
	int	show_command;

	show_command = 0;
	targc = 0;
	for(i=0; i<argc; i++)								/* Loop thru each arg in list		*/
	{
		if (argv[i] && argv[i][0] == '@')					/* If a response file then process	*/
		{
			FILE	*fh;
			if (fh = fopen(&argv[i][1],"r"))				/* Open the response file.		*/
			{
				char	buff[256];
				show_command = 1;
				while(1 == fscanf(fh,"%s",buff) )			/* Read next token			*/
				{
					targv[targc] = wmalloc(strlen(buff)+1);		/* Malloc space for token		*/
					strcpy(targv[targc],buff);			/* Copy token to arg list.		*/
					targc++;
				}
				fclose(fh);
			}
			else								/* Unable to open response file		*/
			{
				write_log("WISP",'E',"CANTOPEN","Unable to open response file %s.",argv[i]);
				exit_wisp(EXIT_WITH_ERR);
			}
		}
		else									/* A regular argument.			*/
		{
			targv[targc++] = argv[i];					/* Regular arg so just assign		*/
		}
	}
	targv[targc] = NULL;								/* Add a null to the end of the list	*/

	if (show_command)
	{
		for(i=0; i<targc; i++)
		{
			printf("%s ",targv[i]);
		}
		printf("\n");
	}
#ifdef DEBUG
	for(i=0; i<targc; i++)
	{
		printf("targv[%d] = [%s]\n",i,targv[i]);
	}
#endif /* DEBUG */
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


		tput_scomment("*  *****  WISP Version %s  EVALUATION COPY ***** \n",WISP_VERSION);
		tput_scomment("* \n");
		tput_scomment("*  This program contains proprietary information that is the\n");
		tput_scomment("*  legal property of International Digital Scientific Inc.\n");
		tput_scomment("*\n");
		tput_scomment("*  This program was translated by an evaluation copy of WISP.\n");
		tput_scomment("*  The purpose of this evaluation software is to allow the\n");
		tput_scomment("*  prospective purchaser of WISP to preview the product so as to\n");
		tput_scomment("*  evaluate its capabilities.  This is the only permitted use of\n");
		tput_scomment("*  this software.  This converted software may not be used in a \n");
		tput_scomment("*  production environment, it may not be sold or distributed in\n");
		tput_scomment("*  any manner, it may not be included or embedded within other\n");
		tput_scomment("*  software.  The possessor of this software has no rights to this\n");
		tput_scomment("*  software.\n");
		tput_scomment("*  \n");
		tput_scomment("*  This converted software must be DESTROYED upon the end of\n");
		tput_scomment("*  the evaluation period.\n");
		tput_scomment("*\n");
		tput_scomment("*  WISP is a trademark of International Digital Scientific Inc.,\n");
		tput_scomment("*  Valencia California.\n");
		tput_scomment("*\n");
		tput_scomment("*  COPYRIGHT (C) 1989,1990,1991,1992,1993 by International Digital \n");
		tput_scomment("*  Scientific Incorporated. All rights reserved.\n");
		tput_scomment("*\n");
		tput_scomment("*  For assistance contact:\n");
		tput_scomment("*\n");
		tput_scomment("*           International Digital Scientific Incorporated\n");
		tput_scomment("*                  28460 Avenue Stanford, Suite 100,\n");
		tput_scomment("*                        Valencia CA 91355\n");
		tput_scomment("*                      Phone: (805) 295-1155\n");
		tput_scomment("*                      Fax:   (805) 295-8755\n");
		tput_scomment("*\n");

}

#include "demovali.h"
#endif



