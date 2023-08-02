//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
//
// Module : wang_os.hpp
// Author : George Soules
// Date   : 17 March 1992

#if WANG

#ifndef WANG_OS__HPP
#define WANG_OS__HPP

// Classes
#include "args.hpp"
#include "data.hpp"
#include "machine.hpp"

// Definitions and subprograms
#include "environ.hpp"
#include "wangfile.hpp"


char *wang_os_backref_full(const char *label);

char *wang_os_backref_partial(const char *label, const char *keyword);

Boolean wang_os_exists(wang_filename a_name);

void wang_os_extract_alpha(char *keyword, char *value);

void wang_os_extract_int(char *keyword, int_32 &receiver);

void wang_os_readfdr_int(
	const char *filename,
	const char *libname,
	const char *volname,
	const char *field,
	int_32 &receiver);

arguments *wang_os_fetched_args_for(char *this_procedure);

void wang_os_update_fetched_args();

void wang_os_init();

void wang_os_increment_link_level();
void wang_os_decrement_link_level();

int_32 wang_os_link(
   char      *a_name,
   arguments *args,
   Boolean    handle_cancel,
   Boolean    transparent,
   int_32    &status);

int_32 wang_os_print(
   char   *filename,
   char   *libname,
   char   *volname,
   char   *fclass,
   char   *status,
   int_32  form,
   int_32  copies,
   char   *disp);

int_32 wang_os_putparm(
   Boolean            is_enter,
   const char        *put_label,
   const char        *ref_label,
   const char        *prname,
   int                pfkey,
   string_array_data &keylist);

int_32 wang_os_rename(
   Boolean is_library,
   char   *file,
   char   *library,
   char   *volume,
   char   *new_file,
   char   *new_library);

int_32 wang_os_scratch(
   Boolean is_library,
   char   *file,
   char   *library,
   char   *volume);

void wang_os_set_alpha(char *keyword, char *value);

void wang_os_set_int(char *keyword, int_32 value);

void write_string_literal(FILE *fh, char *str, int_32 size);

struct submit_parameters
{
#define MAX_SUBMIT_PARAMETERS 32 	/* This is arbitrary - can be increased if needed (was 8) */
	int_32	count;
	int_32	integer[MAX_SUBMIT_PARAMETERS];
	char	*string[MAX_SUBMIT_PARAMETERS];
};

int_32 wang_os_submit(
   char    *filename,
   char    *libname,
   char    *volname,
   char    *proc_id,
   struct submit_parameters *parameters,
   Boolean  globals,
   Boolean  environment,
   char    *jclass,
   char    *status,
   char    *dump,
   int_32   cpulimit,
   char    *action,
   Boolean  requeue);

char *wang_to_native_file_name(const char *a_name, int &file_kind);

void wang_os_access_to_machine(machine *a_machine);

void wang_os_first_procedure_name(char *a_name);

#endif
#endif

/*
**	History:
**	$Log: wang_os.hpp,v $
**	Revision 1.8  1998/08/31 19:50:41  gsl
**	drcs update
**	
**	Revision 1.7  1998-08-31 15:14:28-04  gsl
**	drcs update
**
**	Revision 1.6  1996-08-12 19:42:23-04  gsl
**	Increased the MAX_SUBMIT_PARAMETERS to 32 from 8 which was needed when
**	we used SETSUBMIT() which is no longer used.
**
**	Revision 1.5  1995-10-18 06:42:55-07  gsl
**	updated prototypes
**
**
**
*/

//
//	History:
//	$Log: wang_os.hpp,v $
//	Revision 1.8  1998/08/31 19:50:41  gsl
//	drcs update
//	
//	Revision 1.7  1998-08-31 15:14:28-04  gsl
//	drcs update
//
//

//	
//
//	Working file: wang_os.hpp
//	head: 1.6
//	branch:
//	locks: strict
//	access list:
//		gsl
//		scass
//		ljn
//		jockc
//		jlima
//	symbolic names:
//	keyword substitution: kv
//	total revisions: 6;	selected revisions: 6
//	description:
//	----------------------------
//	revision 1.6
//	date: 1996-08-12 19:42:23-04;  author: gsl;  state: V4_3_00;  lines: +6 -3
//	Increased the MAX_SUBMIT_PARAMETERS to 32 from 8 which was needed when
//	we used SETSUBMIT() which is no longer used.
//	----------------------------
//	revision 1.5
//	date: 1995-10-18 09:42:55-04;  author: gsl;  state: V3_3_19;  lines: +13 -0
//	updated prototypes
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:35-04;  author: gsl;  state: V3_3_18;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:49-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:36-05;  author: gsl;  state: V3_3x12;  lines: +26 -11
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:34-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
