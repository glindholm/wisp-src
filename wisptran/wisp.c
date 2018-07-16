/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/*
**	File:		wisp.c
**
**	Project:	WISP/TRAN
**
**	RCS:		$Source:$
**
**	Purpose:	Main
**
*/

/*
**	Includes
*/


#include <stdio.h>
#include <string.h>
#include <limits.h>

#ifdef unix
#include <sys/times.h>
#endif

#ifdef WIN32
#include <time.h>
#include <process.h>
#include <direct.h>
#endif

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
#include "input.h"
#include "ring.h"
#include "statment.h"
#include "wt_procd.h"
#include "wt_datad.h"
#include "wt_locks.h"

#include "wcommon.h"
#include "wispcfg.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

#ifdef unix
static struct tms time_now;								/* Time reference structure.		*/
#endif
static int cpu_time;									/* Initial CPU time.			*/
static char *sargv[50];									/* Save the argv			*/
static char *targv[50];									/* argv after response file processing	*/
static int targc;									/* argc to match targv			*/
static FILE *xtab_file = NULL;									/* The cross ref tab file.		*/

/*
**	Static Function Prototypes
*/
static int run_wisp(void);
static int initialize1(int argc, char *argv[]);
static int initialize2(void);
static int response_file(int argc, char* argv[]);

static void wisp(void);
static void gen_txt_file(void);
static void wisp_exit_code(const char *prefix);

int main(int argc, char *argv[])
{
	initialize1(argc,argv);
	get_cli(targc,targv);
	initialize2();

	open_io();								/* open the WCB and COB files		*/

	/* Can only load option & key files after open_io */
	load_option_file();							/* Process the options file.			*/
	load_key_file();							/* Process the keylist file.			*/
	validate_options();

	if (opt_xtab)								/* Generate cross reference file.	*/
	{
		xtab_file = fopen(xtab_fname,"w");
	}

	if (opt_noprocess)
	{
		gen_txt_file();
	}
	else if (opt_data_conv)
	{
		gen_data_conv();
	}
	else
	{
		wisp();
	}
	return 0;
}

static void gen_txt_file(void)
{
	int	lstat;
	char	tstr[10];
	int	cnt;
	char	*ptr;

	cnt = 0;
	while(EOF != (lstat=get_cobol_inline()))
	{
		if ((ptr = strchr(linein,'\n'))) *ptr = (char)0;
		sprintf(tstr,"%06d",++cnt);
		if (strlen(linein) >= 6) 
			memcpy(linein,tstr,6);
		else
			strcpy(linein,tstr);

		tput_noprocess(linein);
	}
	exit_wisp(EXIT_OK);
}

static void wisp(void)
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

void need_to_rerun(const char* reason)
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

void delayed_exit_with_err(void)
{
	delayed_error = 1;
}

void exit_with_err(void)
{
	exit_wisp(EXIT_WITH_ERR);
}

int exit_wisp(int err)									/* exit WISP, clean up 			*/
{
	int i,j,k,a,rc;
	c_list *tptr;									/* if not an error-exit			*/
	char	*exit_mess;
	

	switch(err)
	{
	case EXIT_OK: 		exit_mess = "OK";	break;
	case EXIT_WITH_ERR: 	exit_mess = "ERROR";	break;
	case EXIT_FAST: 	exit_mess = "FAST";	break;
	case EXIT_AND_RUN: 	exit_mess = "RERUN";	break;
	default: 		exit_mess = "UNKNOWN";	break;
	}

	if (delayed_error && err != EXIT_FAST)
	{
		err = EXIT_WITH_ERR;
		exit_mess = "DELAYED ERROR";
	}

	if (EXIT_OK == err)
	{
		/*
		*	Catch fatal errors that didn't force an exit with error
		*/
		if (get_highest_serverity() > SEVER_ERROR)
		{
			err = EXIT_WITH_ERR;
			exit_mess = "ERROR";
		}
		else if (get_highest_serverity() > SEVER_SUCCESS)
		{
			/*
			* TODO - add option for non-zero return code on WARNING or ERROR
			*/
		}
	}
		
	write_log("WISP",'I',"EXIT","Exiting with code %s [%d]", exit_mess, err);

	if (err == EXIT_FAST) goto exit_fast;

	if (!err && !opt_noprocess && !opt_data_conv) 
	{
		gen_endstuff();								/* write all the screen paragraphs	*/
	}

	if (!err && !opt_noprocess && !opt_data_conv && (prog_cnt || lock_clear_para)) 
	{
		gen_unlocks();								/* Since there were files, write UNLOCKs*/
	}

	if (dcl_file_ptr) 
	{
		close_cob_file(dcl_file_ptr);						/* close the paragraph file.		*/
	}
	
	if (dtp_file_ptr)
	{
		close_cob_file(dtp_file_ptr);						/* close the paragraph file.		*/
		if (!err)
		{
			copy_file(dtp_fname);						/* Copy it into the wisp section.	*/
		}
		delete(dtp_fname);							/* And delete it.			*/
	}

	if ( !err && !opt_noprocess && !opt_data_conv) wisp_exit_para();			/* write WISP-EXIT-PROGRAM.		*/

	if ( !err )
	{
		tput_flush();								/* Flush the put_token() buffers	*/
	}
	close_all_cob_files();								/* Close all the cob files		*/

	if (xtab_file != NULL)
	{
		fclose(xtab_file);
		xtab_file = NULL;
	}


	if (EXIT_WITH_ERR != err)
	{
		if (need_to_rerun_flag || err == EXIT_AND_RUN)				/* If they want, re run wisp.		*/
		{
			delete(dcl_fname);						/* delete the library file		*/
			delete(out_fname);						/* Delete the .COB file			*/
			run_wisp();
		}
	}


	if (!err && opt_xref)								/* Generate cross reference file.	*/
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
			while(0 == (rc = ring_pop((ring_struct*)rcpy_list_ring,(char*)&rcpy_element)))
			{
				fprintf(xref_file,"%-8.8s   %-8.8s   %s\n",
					rcpy_element.file, rcpy_element.lib, rcpy_element.native);
			}
		}
		fprintf(xref_file,"\n");
		fclose(xref_file);
	}

#ifdef WIN32
	cpu_time = clock() - cpu_time;							/* Get the number of CPU "ticks".	*/
	j = cpu_time / CLK_TCK;								/* Convert "ticks" to integer seconds.	*/
	k = ((( cpu_time * 100 )) / CLK_TCK ) - ( j * 100 );				/* Get remainder 100ths of seconds.	*/
#else		/* unix  */
	times(&time_now);								/* Get the current CPU value.		*/
	cpu_time = time_now.tms_utime - cpu_time;						/* Save it.				*/

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

	if (opt_log_stats && !opt_logging)							/* /LOG switch for stats is on		*/
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

	if (err == EXIT_OK) exit(0);
        exit(1);
	return 0;
}

static void wisp_exit_code(const char* prefix)
{
	int	stoprun;

	tput_blank();
	tput_scomment			("*****************************************************************");
	tput_scomment			("**** WISP EXIT CODE ");
	tput_scomment			("*****************************************************************");
	tput_blank();

	tput_line			("       %sWISP-EXIT-PROGRAM.",prefix);
	if (!opt_native)
	{
		tput_line		("           CALL \"SETRETCODE\" USING WISP-RETURN-CODE.");
		tput_line		("           IF WISP-APPLICATION-NAME = WISP-RUN-NAME");
		tput_line		("               CALL \"WISPEXIT\".");
	}
	tput_line			("           EXIT PROGRAM.");
	tput_line			("       %sWISP-STOP-RUN.",prefix);
	if (!opt_native)
	{
		tput_line		("           CALL \"SETRETCODE\" USING WISP-RETURN-CODE.");
		tput_line		("           CALL \"WISPEXIT\".");
	}

	stoprun = 1;
	if (opt_keep_stop_run) 	stoprun = 1;
	if (opt_change_stop_run) stoprun = 0;

	if (stoprun)
	{
        	tput_line		("           STOP RUN.");
	}
	else
	{
		tput_line		("           EXIT PROGRAM.");
        	tput_line		("           STOP RUN.");
	}
	tput_blank();
}

int wisp_exit_para(void)
{
	wisp_exit_code("");

	return 0;
}

void d_wisp_exit_para(void)
{
	if (!decl_stop_exit) return;

	wisp_exit_code("D-");
}

static char the_com[256];

static int run_wisp(void)								/* Execute the WISP command again	*/
{
	int i;

	the_com[0] = 0;
	for( i=0; sargv[i]; i++ )
	{
		if ( i > 0 )
			strcat( the_com, " " );
		strcat( the_com, sargv[i] );
	}
	printf("\nBeginning New execution of [%s]\n",the_com);
	if (need_to_rerun_reason)
	{
		printf("REASON:\n%s\n",need_to_rerun_reason);
	}

	if (0 != strcmp(original_cwd, new_cwd))
	{
		if (0 != chdir(original_cwd))
		{
			printf("%%WISP-F-CHDIR Unable to change to original directory chdir(\"%s\").\n", original_cwd);
			exit_wisp(EXIT_FAST);
		}
	}

	fflush(stdout);
	fflush(stderr);

#ifdef WATCOM
#define NOEXEC
#endif

#ifdef NOEXEC
	special_noexec_exec(the_com);
#else
	execvp( sargv[0], sargv );
#endif

	printf("\n%%WISP-F-NOEXEC Fatal error; EXEC failed.\n");
	exit(-1); /* rerun failed */
	return 0;
}

/*
**	Routine:	special_noexec_exec()
**
**	Function:	Replacement for the exec() routine for machines that don't support exec.
**
**	Description:	WISP on occasion must rerun itself.  The Watcom compiler does not support
**			the exec() routine.
**			This is what we do...
**
**			The first instance of WISP processes normally up to the point where it
**			must rerun itself.  It then acts as a stub which controls the repeated
**			rerunning of itself.
**			The first time thru it sets an environment variable to lets later instances
**			of WISP know that a controlling stub is in place.  It then uses system()
**			to run a second instance of WISP.  If the second instance of WISP has to
**			rerun then it checks the env variable and sees that a stub is in place, it
**			then exits with an exit code which tells the stub to rerun.
**
**	Arguments:
**	command		The command string to pass to system().
**
**	Globals:	The NOEXEC_STUB environment variable.
**
**	Return:		It will only return if an error occurs.
**
**	Warnings:	None
**
**	History:	
**	05/13/94	Written by GSL
**
*/
#ifdef NOEXEC
int special_noexec_exec(char* command)
{
#define NOEXEC_STUB	"NOEXEC_STUB"
#define NOEXEC_RERUN	127
	static	char	envstr[40];
	char	*ptr;

	/*
	**	If stub already in place then exit with rerun code.
	*/
	if (getenv(NOEXEC_STUB)) exit(NOEXEC_RERUN);

	/*
	**	Set self up as the noexec stub
	*/
	sprintf(envstr,"%s=1",NOEXEC_STUB);
	if (putenv(envstr)) 
	{
		printf("\n%%WISP-F-PUTENV putenv() failed.\n");
		return(1);
	}

	/*
	**	Keep rerunning WISP while recieving a RERUN code.
	*/
	for(;;)
	{
		int	rc;
		_flushall();
		rc = spawnvp( P_WAIT, sargv[0], sargv );

		if (NOEXEC_RERUN != rc) exit(rc);
	}

	return(1);
}
#endif

int delete(const char* path )
{
	return( unlink( path ) );
}

static int initialize1(int argc, char* argv[])						/* Perform all generic initialization 	*/
{
	int	i;

#ifdef WIN32
	cpu_time = clock();								/* Get the number of CPU "ticks".	*/
#else		/* unix */
	times(&time_now);								/* Get the current CPU value.		*/
	cpu_time = time_now.tms_utime;							/* Save it.				*/
#endif

	for (i=0; i<argc; i++ ) sargv[i] = argv[i];					/* Save the argv			*/
	sargv[argc] = 0;								/* Null terminate sargv			*/

	response_file(argc, sargv);							/* Process any response file.		*/

	logfile = stdout;								/* Define a file for error logs		*/

	crt_relative[0][0] = 0;								/* no relative key yet			*/
	crt_file[0][0] = 0;

	for (i=0; i<MAX_SCREENS; i++) scrn_flags[i] = 0;				/* Init screen flags now.		*/
	return 0;
}

static int initialize2(void)								/* Perform switch specific initialize	*/
{
	if (acu_cobol)
	{
		strcpy(bin2_type,"COMP-1");
		strcpy(bin4_type,"COMP-4");
		strcpy(packdec,"COMP-3");
		strcpy(hard_lock,"99");
		*soft_lock='\0';
		strcpy(cobol_type,"ACU");
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

	if (opt_xref)									/* Init xref structure.			*/
	{
		xref_ptr = (struct c_list *)wmalloc(sizeof(c_list));  			/* Get some memory.			*/
		xref_ptr->call_count = 0;
		xref_ptr->next_list = 0;
	}
	return 0;
}

char *packed_decimal(void)
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
static int response_file(int argc, char* argv[])
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
			if ((fh = fopen(&argv[i][1],"r")))				/* Open the response file.		*/
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
#ifdef TEST
	for(i=0; i<targc; i++)
	{
		printf("targv[%d] = [%s]\n",i,targv[i]);
	}
#endif /* TEST */

	return 0;
}

/*
**	Routine:	xtab_log()
**
**	Function:	Log a cross reference item in a tab file
**
**	Arguments:
**	fileName	The file name
**	lineNum		The line number with in the file
**	xtype		The cross reference type ("CALL", "COPY", "OPEN", etc)
**	tabData		Data string with individual elements tab separated.
**
**	Return:		None
**
*/
void xtab_log(const char* fileName, int lineNum, const char* xtype, const char *tabData)
{
	if (opt_xtab && xtab_file != NULL)
	{
		/* PROGRAM, FILE, LINE, XTYPE, Data... */
		fprintf(xtab_file, "%s\t%s\t%d\t%s\t%s\n", 
			prog_id, fileName, lineNum, xtype, tabData);
	}
}


/*
**	History:
**	$Log: wisp.c,v $
**	Revision 1.36  2005/12/02 15:22:47  gsl
**	Keep track of the highest severity level reported.
**	Ensure an non-zero exit status if severity is fatal or higher.
**	
**	Revision 1.35  2003/09/03 17:32:05  gsl
**	With #NATIVE option don't gen SETRETCODE/WISPEXIT calls
**	
**	Revision 1.34  2003/03/11 19:26:46  gsl
**	Add validate_options() routine which checks for option conficts.
**	
**	Revision 1.33  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.32  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.31  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.30  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.29  2003/02/04 17:27:24  gsl
**	Fix -Wall warnings
**	
**	Revision 1.28  2002/07/31 20:24:26  gsl
**	globals
**	
**	Revision 1.27  2002/07/30 19:12:39  gsl
**	SETRETCODE
**	
**	Revision 1.26  2002/07/29 21:13:25  gsl
**	setretcode -> SETRETCODE
**	
**	Revision 1.25  2002/07/24 21:41:02  gsl
**	Change directory to the location of the source file if a path is given.
**	
**	Revision 1.24  2002/07/18 21:04:26  gsl
**	Remove MSDOS code
**	
**	Revision 1.23  2002/06/20 22:54:35  gsl
**	remove obsolete code
**	add opt_native stuff
**	
**	Revision 1.22  2002/06/19 22:48:16  gsl
**	Remove VAX code & cleanup
**	
**	Revision 1.21  2001/09/13 14:07:24  gsl
**	Remove VMS and DEMO ifdefs
**	Add xtab_log() support
**	
**	Revision 1.20  1998-06-09 10:09:10-04  gsl
**	fixed prototypes and added new header
**
**	Revision 1.19  1998-03-04 13:45:13-05  gsl
**	Add logging
**
**	Revision 1.18  1997-09-09 17:55:48-04  gsl
**	Change scrn_para() to gen_endstuff() as part of ACN code
**
**	Revision 1.17  1997-02-17 16:46:22-05  gsl
**	Change address
**
**	Revision 1.16  1996-12-12 12:56:23-05  gsl
**
**	Revision 1.15  1996-10-09 09:30:49-07  gsl
**	add include wispcfg.h
**
**	Revision 1.14  1996-08-30 18:56:12-07  gsl
**	drcs update
**
**
**
*/
