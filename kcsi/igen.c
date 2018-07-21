/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
In the option to generate a duplicate inquiry, the following
information is requested from the user

FILE, LIB and VOLUME for the procedure (shell script).

DISPLAY the query (default YES) allows user to inspect the query
before it is executed.

ENDRUN YES to exit at the end of the query otherwise the user sees
the EOJ screen.

The generated procedure must answer the following parameters

1.	PRNAME = INPUT
	FILE, LIBRARY and VOLUME of the data file.
	Control Files CHANGE = NO or YES
	OPTION		DISPLAY/EXTRACT

2.	IF CHANGE = YES
	FILE LIBRARY AND VOLUME OF THE CONTROL FILE.
	PRNAME unknown.

3.	The query and title


Inquiry GETPARMS

1.	INPUT
	FILE LIBRARY VOLUME
	MODE CHANGE OPTION

2.	CONTROL
	FILE LIBRARY VOLUME

3.	QUERY
	DISPLAY		YES/NO
	TITLE		40
	LINE1A		40
	LINE1B		39
	LINE2A		40
	LINE2B		39
	......
	LINE7A		40
	LINE7B		39

4.	OUTPUT
	FILE LIBRARY VOLUME		(for EXTRACT)
	RECORDS				(not needed for Unix)

5.	EOJ
	1.  New query
	2.  Save query
	3.  Create RDF
	16. Exit

The EOJ screen will be modified to accept ENTER as PF 16.

------*/

/*----
Mods:
July 21, 91 mjb
	Modified to handle diferences in LPI and ACU-COBOL runtimes,
	apparent differences in the name of the putparm routine, and
	added conversion of " to ' for the LINE inputs.
------*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#include "shrthand.h"
#include "cobioblk.h"
#include "kwisp.h"
#include "intdef.h"
#include "kcsifunc.h"


#define COMMAND     ""				/* Obsolete remnant */
#define COMMENT	   ( (gen_wproc) ? "* ":"# " ) 	/* PROC vs Shell comment chars */

static char inq_sys_name[81];

char inq_progname[81]
#ifdef KCSI_ACU
="wrun INQUIRY"
#endif
#ifdef KCSI_MFX
="inquiry"
#endif
;

static int gen_wproc = 1;	/* Generate WPROC procedures */

static void prologue(FILE *qf,char *fname);
static void input_putparm(FILE *qf,char *dio,char *change,char *option);
static void control_putparm(FILE *qf,char *cio);
static void query_putparm(FILE *qf,char *disp,char *title,char *lines);
static void output_putparm(FILE *qf,char *eio);
static void eoj_putparm(FILE *qf);
static void runinquiry(FILE *qf);
static void enter_file(FILE *qf,char *io);
static void enter_putparm(FILE *qf,char *prname);
static void enter_keyword(FILE *qf,char *keyword,char *value);
static void continue_line(FILE *qf);
static void end_line(FILE *qf);
static void enter_quoted_keyword(FILE *qf,char *keyword,char *value);

void GENINQ(char *gio,char *display,char *endrun,char *dio,char *change,
	    char *option,char *cio,char *qtitle,char *qlines,char *eio)
{
	int4 mode;
	FILE *qfile;
	char	*ptr;

	if ((ptr = getenv("KCSIGENWPROC")) && 0==strcmp(ptr,"NO"))
	{
		gen_wproc = 0;
	}
	
	/* Create the file for the inquiry */

	if (gen_wproc)
        {
		WSETFILEXT("wps");
        }
	else
        {
#ifdef KCSI_UNIX
		WSETFILEXT("sh");
#endif
        }
	
	mode = IS_OUTPUT;
	WFOPEN2(&mode,&gio[VOLUME_POS],&gio[LIBRARY_POS],&gio[NAME_POS],
			inq_sys_name,"INQUIRY ","INQUIRY ");
	KCSI_strunc(inq_sys_name);
	qfile = fopen(inq_sys_name,"w");

	/* Introductory stuff and comments */
	prologue(qfile,&gio[NAME_POS]);

	if (gen_wproc)
	{
		runinquiry(qfile);
	}

	/* Generate input putparm */
	input_putparm(qfile,dio,change,option);

	/* Generate control putparm if change CONTROL = YES */
	/* Generate it anyway */
/*
	if(Memeq(change,"YES",3))
*/
	control_putparm(qfile,cio);
	/* Generate the query itself */
	query_putparm(qfile,display,qtitle,qlines);

	/* If the option was to extract then generate the output file putparm*/
	if(Memeq(option,"EXTRACT",7))
		output_putparm(qfile,eio);

	/* If endrun is yes then generate the end of job putparm */
	if(Memeq(endrun,"YES",3))
		eoj_putparm(qfile);

	/* And the closing coup is to actually run the program */
	if (!gen_wproc)
	{
		runinquiry(qfile);
	}
	

	/* Closeup and make executable */
	fclose(qfile);
	/*make executable */
#ifdef KCSI_UNIX
	chmod(inq_sys_name,00775);
#endif
}

/*----
Procedure ID, date stamp user_id
------*/
static void prologue(FILE *qf,char *fname)
{
	time_t a_time;
	char wrk_buf[9];
	char *comment;

	wrk_buf[0] = 0;
	sscanf(fname,"%8s",wrk_buf);

	if (gen_wproc)
	{
		/*
		**	If a wproc then the PROCEDURE line is not a comment
		*/
		comment = "";
	}
	else
	{
		comment = COMMENT;
	}

	fprintf(qf,"%s%sPROCEDURE %s to run INQUIRY\n",
		COMMAND,comment,wrk_buf);

	time(&a_time);
	fprintf(qf,"%s%sCreated %s",
		COMMAND,COMMENT,ctime(&a_time));
}

/*----
Generate the INPUT putparm
FILE LIBRARY VOLUME CHANGE OPTION.
------*/

static void input_putparm(FILE *qf,char *dio,char *change,char *option)
{
	char wrk_buf[11];

	fprintf(qf,
		"%s%sDATA File, CHANGE control file, and DISPLAY/EXTRACT results\n",
		COMMAND,COMMENT);
	enter_putparm(qf,"INPUT");		/*23 bytes */
	enter_file(qf,dio);			/*44 bytes */
	continue_line(qf);

	wrk_buf[0] = 0;
	sscanf(change,"%3s",wrk_buf);
	enter_keyword(qf,"CHANGE",wrk_buf);	/*11 bytes */

	wrk_buf[0] = 0;
	sscanf(option,"%7s",wrk_buf);
	enter_keyword(qf,"OPTION",wrk_buf);	/*14 bytes*/
	end_line(qf);
	end_line(qf);
}

/*----
Generate the CONTROL putparm
FILE LIBRARY VOLUME
------*/

static void control_putparm(FILE *qf,char *cio)
{

	fprintf(qf,"%s%sSpecification for the new control file\n",
		COMMAND,COMMENT);
	enter_putparm(qf,"CONTROL");		/*23 bytes */
	enter_file(qf,cio);			/*44 bytes */
	end_line(qf);
	end_line(qf);
}

/*----
Putparm for the actual query.
------*/
static void query_putparm(FILE *qf,char *disp,char *title,char *lines)
{
	char wrk_buf[81];
	char *wrk_ptr;
	char qkeyword[9];
	int qnum;

	fprintf(qf,
		"%s%sCan the user DISPLAY plus the title and the actual query.\n",
		COMMAND,COMMENT);

	enter_putparm(qf,"QUERY");
	wrk_buf[0] = 0;
	sscanf(disp,"%3s",wrk_buf);
	enter_keyword(qf,"DISPLAY",wrk_buf);
	continue_line(qf);

	/* Extract the title ignoring spaces */
	memset(wrk_buf,0,81);
	wrk_buf[0] = 0;
	sscanf(title,"%40c",wrk_buf);
	enter_quoted_keyword(qf,"TITLE",wrk_buf);

	/* Extract all of the query fields ignoring spaces */
	for(qnum = 1; qnum < 8; ++qnum)
	{
		memset(wrk_buf,0,81);
		sscanf(lines,"%40c",wrk_buf);

		/* Convert all double quotes to single quotes in the output */
		wrk_ptr = wrk_buf;
		while(*wrk_ptr)
		{
			if(*wrk_ptr == '\"')
				*wrk_ptr = '\'';
			++wrk_ptr;
		}
		sprintf(qkeyword,"LINE%dA",qnum);
		if(!KCSI_isnblank(wrk_buf,40))
		{
			continue_line(qf);
			enter_quoted_keyword(qf,qkeyword,wrk_buf);
		}
		lines += 40;
		memset(wrk_buf,0,81);
		sscanf(lines,"%39c",wrk_buf);

		/* double quotes to single quotes in the other half */
		wrk_ptr = wrk_buf;
		while(*wrk_ptr)
		{
			if(*wrk_ptr == '\"')
				*wrk_ptr = '\'';
			++wrk_ptr;
		}
		sprintf(qkeyword,"LINE%dB",qnum);
		if(!KCSI_isnblank(wrk_buf,39))
		{
			continue_line(qf);
			enter_quoted_keyword(qf,qkeyword,wrk_buf);
		}
		lines += 39;
	}
	end_line(qf);
	end_line(qf);
}

/*----
Generate the OUTPUT putparm
FILE LIBRARY VOLUME.
------*/

static void output_putparm(FILE *qf,char *eio)
{
	int torg;

	fprintf(qf,"%s%sOutput File for the EXTRACT\n",
		COMMAND,COMMENT);
	enter_putparm(qf,"OUTPUT");		/*23 bytes */
	enter_file(qf,eio);			/*44 bytes */
	torg = eio[ORG_POS];
	switch(torg)
	{
	case 'C':
		enter_keyword(qf,"CONSEC","YES");
		break;
	case 'R':
		enter_keyword(qf,"CONSEC","REL");
		break;
	default:
		enter_keyword(qf,"CONSEC","NO");
		break;
	}
	end_line(qf);
	end_line(qf);
}

/*----
Generate an end of job putparm
------*/
static void eoj_putparm(FILE *qf)
{

	fprintf(qf,"%s%sAnd finally set up to exit INQUIRY\n",COMMAND,COMMENT);
	enter_putparm(qf,"EOJ");		/*23 bytes */
	end_line(qf);
	end_line(qf);
}

/*----
With all the putparms created, all that is left is to run
the INQUIRY program.
------*/
static void runinquiry(FILE *qf)
{
	fprintf(qf,"%s%sRun INQUIRY\n",COMMAND,COMMENT);

	if (gen_wproc)
	{
		fprintf(qf,"RUN INQUIRY\n");
	}
	else
	{
		fprintf(qf,"%s%s\n",COMMAND,inq_progname);
	}
}


/*----
Standard keywords for a file extracted from and io_block
------*/
static void enter_file(FILE *qf,char *io)
{
	char wrk_buf[9];

	wrk_buf[0] = 0;
	sscanf(&io[NAME_POS],"%8s",wrk_buf);
	enter_keyword(qf,"FILE",wrk_buf);		/*14*/
	wrk_buf[0] = 0;
	sscanf(&io[LIBRARY_POS],"%8s",wrk_buf);
	enter_keyword(qf,"LIBRARY",wrk_buf);		/*17*/
	wrk_buf[0] = 0;
	sscanf(&io[VOLUME_POS],"%6s",wrk_buf);
	enter_keyword(qf,"VOLUME",wrk_buf);		/*13*/
}

static void enter_putparm(FILE *qf,char *prname)
{
	if (gen_wproc)
	{
		fprintf(qf,"    ENTER %s DUMMY=FIRST\n",prname);
	}
	else
	{
		fprintf(qf,"%swputparm ENTER %s ",COMMAND,prname);
	}
}
static void enter_keyword(FILE *qf,char *keyword,char *value)
{
	if (gen_wproc)
	{
		fprintf(qf,"    ,%s=%s \n",keyword,value);
	}
	else
	{
		fprintf(qf," %s=%s ",keyword,value);
	}
	
}
static void enter_quoted_keyword(FILE *qf,char *keyword,char *value)
{
	if (gen_wproc)
	{
		fprintf(qf,"    ,%s=\"%s\" \n",keyword,value);
	}
	else
	{
		fprintf(qf,"%s=\"%s\" ",keyword,value);
	}
}

static void continue_line(FILE *qf)
{
	if (!gen_wproc)
	{
		fprintf(qf," \\\n");	/* Shell continue line */
	}
}

static void end_line(FILE *qf)
{
	fprintf(qf,"\n");
}

int KCSI_isnblank(char *mem,int len)
{
	while(len--)
	{
		if(*mem != ' ')
			return(0);
		++mem;
	}
	return(1);
}

/*
**	History:
**	$Log: igen.c,v $
**	Revision 1.18  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.17  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.16  2002/10/17 21:22:41  gsl
**	cleanup
**	
**	Revision 1.15  2002/10/17 17:17:18  gsl
**	Removed VAX VMS code
**	
**	Revision 1.14  2002/07/29 15:46:54  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.13  2002/07/29 14:47:21  gsl
**	wfopen2 ->WFOPEN2
**	wfopen3 ->WFOPEN3
**	
**	Revision 1.12  2002/07/25 15:20:28  gsl
**	Globals
**	
**	Revision 1.11  2002/07/23 20:49:51  gsl
**	globals
**	
**	Revision 1.10  2002/07/12 17:17:01  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  2002/06/21 20:48:16  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.8  2001/11/02 23:13:33  gsl
**	Fixed WPROC comment characters
**	Removed VMS
**	
**	Revision 1.7  2001-09-04 16:55:49-04  gsl
**	Change default to save INQUIRY's as wproc .wps files instead of shell scripts.
**
**	Revision 1.6  1998-01-20 11:54:59-05  gsl
**	Fix time references to be type time_t
**
**	Revision 1.5  1996-09-24 17:56:29-04  gsl
**	Rewrote the GENINQ() routine to generate a WPROC procedure for
**	WIN32 instead of a shell script.
**	To force a WPROC procedure to be generated in other environments (unix)
**	set the environment variable  KCSIGENWPROC=YES
**	For shell scripts now set extension to ".sh"
**	For VAX command scripts set extension to ".com"
**
**	Revision 1.4  1996-09-17 16:45:37-07  gsl
**	drcs update
**
**
**
*/
