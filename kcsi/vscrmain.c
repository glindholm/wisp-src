static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


/*----
Modules

vscrglb.c	globals for create

vscrmain.c	main() module

vscrhelp.c	help screens called from valrious places in the 
		running program.

vscrout.c	Screens, entry and validation for the output file to
		be created, including the file, org, record len,
		primary key and alternate key screens.

vscrblk.c	Accepts user input for multiple blocks and
		fields until exhausted. Includes logic for 
		allocating, linking and freeing blocks
		and fields.

vscrffld.c	Data entry for defining a field from a file
		to be included in the output.

vscrbchk.c	Checks after each record processed from the
		input specifications, whether the block
		has reached its logical end.

vscrsqz.c	Array squeezing routine.

vscrspky.c	Routine for setting the global split key
		variable.

vscrdbg.c	Routines to format and print a file containing
		create debug information.

vscreoj.c	End of Job Routine.

vscrfile.c	Logic to create, and format the output records.

------*/

/*----
RNotes

		Release Notes - 3/26/94

v. 1.7 3/26/94
-----
Problem:	Binary Sequential File type Requested by several
		users.

Resolution:	A Binary sequential file type has been added as
		a file type 'B'. This file type is fixed length
		records, the file contains only the data
		in the record.
+++++

v. 1.6 3/17/94	Not Released


v. 1.5 3/3/94

Problem:	INPOS not being used correctly as the starting
		point for an inputfield in a file.

Resolution:	INPOS logic corrected to start input from
		the correct position in a record.

-----
+++++

v. 1.4 3/3/94
	Internal version control only. No changes in the utility.

v. 1.3 10/31/93

Problem:	Relative files being created that consume large
		disk space. 

Resolution:	Open output logic for relative files corrected
		to create corrrect size files.

v. 1.2 08/15/93

	Initial Release for AIX, using AcuCOBOL Vision 3.0 files.

EndRNotes
------*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "create.h"
#include "kcsifunc.h"
#include "vscrglb.h"
#include "kwisp.h"

/*
**	Structures and Defines
*/

/*
**	Static data
*/

static char applname[]="CREATE  ";

char WISPFILEXT[39];
char WISPRETURNCODE[3];


extern char *create_version(void);
extern char *create_platform(void);

extern void vwang_title(const char *title);

static void create_init(void);
static void vscreate(void);
static void create_extract_usage(void);


main(int argc, char **argv)
{
	int ch;
	char the_title[80];

	sprintf(the_title,"CREATE %s.%s", create_version(), create_platform());
	vwang_title(the_title);

	while( (ch = getopt(argc,argv,"d")) != -1)
		{
		switch(ch)
			{
			case 'd':
				cr_debug = 1;
				break;
			default:
				break;
			}
		}

	initglbs(applname);
	create_init();
	vscreate();
	cr_exit(0);
	return 0;
}

void cr_exit(int code)
{
	vwang_shut();
	exit(code);
}

static void create_init(void)
{
	init_gpint();
	create_extract_usage();
}
static void init_cr_out(void)
{
	memset(&cr_out,0,sizeof(cr_out));
	memset(cr_out.ofile._name,' ',8);
	memcpy(cr_out.ofile._library,create_outlib,8);
	memcpy(cr_out.ofile._volume,create_outvol,6);
	strcpy(cr_out.ofile._prname,"NEWFILE ");
}

static void create_extract_usage(void)
{
	wargs(2L);
	EXTRACT("IL",create_inlib);
	EXTRACT("IV",create_invol);
	EXTRACT("OL",create_outlib);
	EXTRACT("OV",create_outvol);
}

static void vscreate(void)
{
	int rc;

	for(;;)
		{
		init_cr_out();
		rc = cr_get_output_spec(create_version(),create_platform());
		if (rc == 1)
			continue;
		if(rc == 16)
			break;
		cr_get_blocks();
		cr_create_file();
		rc = cr_eoj();
		if(rc == 16)
			break;
		}
}

/*----
A quick io routine to the cr_out.ofile.
------*/
void cr_io(char *io)
{
	cr_out.ofile._record = cr_out_rec;
	strcpy(cr_out.ofile._io,io);
	gpwcsio(&cr_out.ofile,cr_out.ofile._record);
}

/*----
A quick io routine to any file 
------*/
void cr_fileio(KCSIO_BLOCK *kfb, char *io)
{
	long mode;

	mode = 0;
	if(kfb->_org[0] == 'I')
		mode += WISP_INDEXED;

	strcpy(kfb->_io,io);
	if(*io == 'O')
		{
		if(!(strcmp(io,OPEN_OUTPUT))) 
			mode += WISP_OUTPUT;
		kcsio_wfopen(mode,kfb);
		}
		
	ccsio(kfb, kfb->_record);
}


/*
**	History:
**	$Log: vscrmain.c,v $
**	Revision 1.12  1997/12/19 22:10:13  gsl
**	fix warnings
**	
**	Revision 1.11  1997-12-19 17:01:17-05  gsl
**	add include
**
**	Revision 1.10  1997-08-15 10:36:37-04  scass
**	Moved create_version() to version.c so builds properly and
**	still maintains the information.
**
**	Revision 1.9  1997-04-21 15:19:26-04  gsl
**	Add windows title
**
**	Revision 1.8  1997-03-26 09:12:12-05  gsl
**	Add create_version() which forms a version number from the define CREATE_VERSION
**
**	Revision 1.7  1997-03-26 08:41:19-05  gsl
**	Change version to 2.0
**
**	Revision 1.6  1996-10-02 20:12:12-04  gsl
**	added a KCSI_WIN32 platform code
**
**	Revision 1.5  1996-10-02 15:13:33-07  gsl
**	Remove WISPLIB include files
**
**	Revision 1.4  1996-10-02 09:10:55-07  gsl
**	Add standard headers
**	Fix prototypes and warnings
**	Changed vexit to vwang_shut()
**
**
**
*/
