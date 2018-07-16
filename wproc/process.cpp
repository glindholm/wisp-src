/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : process.cpp
// Author : George Soules
// Date   : 24 February 1991

// Specification
#include "process.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "colors.hpp"
#include "debugaid.hpp"
#include "memory.hpp"
#include "fileinfo.hpp"

#include <string.h>

#include "envs.h"

process *the_process;


process::process() {
   // This method is only called once for the first process in a run-unit

   base_level           = 0;		// Pass by environment variable across a LINK. (load_from_env())
   nesting_level        = 1;		// The nesting level within the current run-unit
   trace_level          = 0;  		// The level at which a trace is started and thus can be stopped
   trace_active         = false;
   the_top_filename     = NULL;
   the_parent_pathname  = NULL;
   the_input_pathname   = NULL;
   the_source_pathname  = NULL;
   the_source_timestamp = 0;
   global_symbol_table  = NULL;
   cursor_off           = false;

   load_from_env();			// Loads process variables from higher link levels

   trace_end(object);
}


process::process(process &a_process) {
   base_level           = a_process.base_level;
   nesting_level        = a_process.nesting_level + 1;
   trace_level          = a_process.trace_level;
   trace_active         = a_process.trace_active;
   the_parent_pathname  = a_process.the_input_pathname;
   the_top_filename     = a_process.the_top_filename;
   the_input_pathname   = NULL;
   the_source_pathname  = NULL;
   the_source_timestamp = 0;
   global_symbol_table  = a_process.global_symbol_table;
   cursor_off           = a_process.cursor_off;
   trace_end(object);
}


process::~process() {
   if (nesting_level == 1)
      colors_cleanup();
   if (the_input_pathname)
      delete the_input_pathname;
   if (the_source_pathname)
      delete the_source_pathname;
   trace(object, "~process");
}

void process::set_top_filename(const char *path)
{
	char	*ptr;
	char	temp[256];

	if (the_top_filename) return;

	if ((ptr = getenv(WPROC_TOPFILE_ENV)) && *ptr)
	{
		//	Were at a lower link-level so get the top filename
		//	from the environment that was set earlier

		the_top_filename = dup_string(ptr);
		return;
	}

	if (!path)
	{
		strcpy(temp,"UNKNOWN");
	}
	else if ((ptr = strrchr(path,SLASH_CHAR)))
	{
		strcpy(temp,&ptr[1]);
	}
	else
	{
		strcpy(temp,path);
	}

	if ((ptr = strchr(temp,'.')))
	{
		*ptr = (char)0;
	}

	temp[8] = (char)0;

	if ((char)0 == temp[0])
	{
		strcpy(temp,"UNKNOWN");
	}

	the_top_filename = dup_string(temp);

	sprintf(temp,"%s=%s",WPROC_TOPFILE_ENV,the_top_filename);

	putenv(dup_string(temp));
}

void process::prepare_for_link()
{
	char	temp[265];

	/*
	**	The new base_level will be the current plus the nesting_level
	**	less one because the nesting_level has already been incremented.
	*/
	sprintf(temp,"%s=%d",WPROC_BASELEVEL_ENV,nesting_level+base_level-1);
	putenv(dup_string(temp));

	sprintf(temp,"%s=%s",WPROC_TRACEACTIVE_ENV,(trace_active)?"true":"false");
	putenv(dup_string(temp));

	sprintf(temp,"%s=%d",WPROC_TRACELEVEL_ENV,trace_level);
	putenv(dup_string(temp));

	if (trace_active)
	{
		void tracer_prepare_for_link();
		tracer_prepare_for_link();
	}
}

extern "C" int wgetpgrp(void);

void process::load_from_env()
{
	char	*ptr;
	char	temp[265];
	int	rc;
	int	gid = -1;

	if ((ptr = getenv(WPROC_TOPGID_ENV)))
	{
		rc = sscanf(ptr,"%d",&gid);
		if (rc != 1 || gid < 1 ) gid = -1;

		if (gid != wgetpgrp())
		{
			/*
			**	RESET == true
			**
			**	This is likely the first proc in a submitted process chain.
			**	All the WPROC environment variables are from the parent process
			**	chain and likely do not apply to this new process chain so
			**	reset all of them including RESET.
			*/

#define RESET_ENV(XXX) 	{ \
				  char *ptr; \
				  if ((ptr = getenv(XXX)) && *ptr) \
				  { \
					    char temp[256]; \
					    sprintf(temp,"%s=",XXX); \
					    putenv(dup_string(temp)); \
				  } \
			}

			RESET_ENV(WPROC_TOPFILE_ENV);
			RESET_ENV(WPROC_BASELEVEL_ENV);
			RESET_ENV(WPROC_TRACELEVEL_ENV);
			RESET_ENV(WPROC_TRACEACTIVE_ENV);
			RESET_ENV(WPROC_TRACEFILE_ENV);
			RESET_ENV(WPROC_TRACEFLAGS_ENV);

			gid = -1;
		}
	}

	if (-1 == gid)
	{
		sprintf(temp,"%s=%d",WPROC_TOPGID_ENV,wgetpgrp());
		putenv(dup_string(temp));
	}

	if ((ptr = getenv(WPROC_BASELEVEL_ENV)) && *ptr)
	{
		sscanf(ptr,"%d",&base_level);
	}

	if ((ptr = getenv(WPROC_TRACEACTIVE_ENV)) && *ptr)
	{
		if (0==strcmp(ptr,"true"))
		{
			trace_active = true;
		}
		else if (0==strcmp(ptr,"false"))
		{
			trace_active = false;
		}
	}

	if ((ptr = getenv(WPROC_TRACELEVEL_ENV)) && *ptr)
	{
		sscanf(ptr,"%d",&trace_level);
	}
}

/*
**	History:
**	$Log: process.cpp,v $
**	Revision 1.11  1998-01-16 09:31:57-05  gsl
**	Fix set_top_filename to use SLASH_CHAR so it correctly parses
**	filepaths on windows
**
**	Revision 1.10  1997-10-01 09:28:22-04  gsl
**	warnings
**
**	Revision 1.9  1996-07-25 14:16:01-04  gsl
**	Renamed from process.cc to process.cpp
**
**	Revision 1.8  1995-10-17 02:52:07-07  gsl
**	add declare of wgetpgrp()
**
// Revision 1.7  1995/10/16  13:57:55  gsl
// Added logic to do a "RESET" if the GID is different then the TOPGID
// environment variable.  This should only occur if this is the top
// proc in a SUBMIT'ed chain.  This also sets TOPGID if needed.
//
// Revision 1.6  1995/10/12  16:54:12  gsl
// Add logic to detect and handle RESET.
// If RESET=true then clear all the WPROC environment variables.
// This fixes the BUG of wproc grabbing the wrong globals file
// when it has been submitted.
//
// Revision 1.5  1995/10/12  12:18:49  gsl
// moved the env defines to envs.h
//
**
**
*/
