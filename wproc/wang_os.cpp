//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//
// Copyright (c) Lexical Copyright, 1992.  All rights reserved.
//
// Module : wang_os.cpp
// Author : George Soules
// Date   : 17 March 1992

#if WANG

#include <time.h>
#include <sys/types.h>

// Specification
#include "wang_os.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "debugaid.hpp"
#include "memory.hpp"
#include "runtype.h"
#include "utility.hpp"
#include "product.hpp"
#include "wisp_rts.h"
#include "miscsubs.h"

const int max_keyvalue = 80;


#define argcount_is(i)  \
   {                    \
   int_32 args = i;     \
   wvaset(&args);       \
   }


// See wang_os_access_to_machine() for an explanation of this variable
machine *parent_machine = NULL;

char *wang_os_backref_full(const char *label) {
   char mrecv_file[8 + 1] = "        ";
   char mrecv_lib[8 + 1]  = "        ";;
   char mrecv_vol[6 + 1]  = "      ";
   char putlabel[WANG_LABELNAME_SIZE + 1];
   init_string_with_blank_pad(putlabel, WANG_LABELNAME_SIZE, label);
   upper_case(putlabel);
   int_32 keycnt = 3;
   int_32 mrecvln_file = 8;
   int_32 mrecvln_lib  = 8;
   int_32 mrecvln_vol  = 6;
   int_32 retcode = 0;

   wswap(&keycnt);
   
   argcount_is(13);
   PUTPARM("M", putlabel, &keycnt,
      "FILE    ", mrecv_file, &mrecvln_file,
      "LIBRARY ", mrecv_lib,  &mrecvln_lib,
      "VOLUME  ", mrecv_vol, &mrecvln_vol,
      &retcode);

   wswap(&retcode);

   mrecv_file[8] = '\0';
   mrecv_lib[8]  = '\0';
   mrecv_vol[6]  = '\0';

   trace_si(general, "Full backward reference rc = ", retcode);
   trace_ss(general, "FILE    = ", mrecv_file);
   trace_ss(general, "LIBRARY = ", mrecv_lib);
   trace_ss(general, "VOLUME  = ", mrecv_vol);

   const int size = 8 + 1 + 8 + 1 + 6;
   char *flv = new_string(size);
   char *temp;
   temp = trim(mrecv_file);
   strcpy(flv, temp);
   delete_string(temp);
   strcat(flv, "\x01");
   temp = trim(mrecv_lib);
   strcat(flv, temp);
   delete_string(temp);
   strcat(flv, "\x02");
   temp = trim(mrecv_vol);
   strcat(flv, temp);
   delete_string(temp);
   return flv;
}


char *wang_os_backref_partial(const char *label, const char *keyword) {
   char mrecv[max_keyvalue + 1];
   char putlabel[WANG_LABELNAME_SIZE + 1];
   char kword[8 + 1];
   init_string_with_blank_pad(putlabel, WANG_LABELNAME_SIZE, label);
   upper_case(putlabel);
   init_string_with_blank_pad(kword, 8, keyword);
   upper_case(kword);
   init_string_with_blank_pad(mrecv, max_keyvalue);
   int_32 keycnt = 1;
   int_32 mrecvln = max_keyvalue;
   char pfkeyrecv[1];
   int_32 retcode = 0;

   wswap(&keycnt);
   wswap(&mrecvln);

   argcount_is(9);
   PUTPARM("M", putlabel, &keycnt, kword, mrecv, &mrecvln, pfkeyrecv, " ", &retcode);

   wswap(&retcode);
   
   mrecv[max_keyvalue] = '\0';

   trace_si(general, "Partial backward reference rc = ", retcode);
   trace_ss(general, "Label   = ", putlabel);
   trace_ss(general, "Keyword = ", kword);
   trace_ss(general, "Value   = ", mrecv);

   return trim(mrecv);
}


void wang_os_extract_alpha(char *keyword, char *value) {
   argcount_is(2);
   EXTRACT(keyword, (void *) value);
}


Boolean wang_os_exists(wang_filename a_name) {
   int_32 start_count = 1;
   int_32 count       = 0;
   int_32 f_count     = 0;
   argcount_is(7);
   wswap(&start_count);
   wswap(&count);
   wswap(&f_count);
   FIND(a_name.filename, a_name.libname, a_name.volname, &start_count,
      &count, NULL, &f_count);
   wswap(&f_count);
   return BOOLEAN(f_count > 0);
}


void wang_os_extract_int(char *keyword, int_32 &receiver) {
   int_32 temp;
   argcount_is(2);
   wswap(&temp);
   EXTRACT(keyword, (void *) &temp);
   wswap(&temp);
   receiver = temp;
}

extern "C" void READFDR(
	const char *file, 
	const char *lib, 
	const char *vol, 
	const int_32 *mode, 
	const char *field, 
	void *value, 
	int_32 *rc);

void wang_os_readfdr_int(
	const char *filename,
	const char *libname,
	const char *volname,
	const char *field,
	int_32 &receiver)
{
	int_32	mode = 0;
	int_32	temp = 0;
	int_32	rc = 0;

	argcount_is(7);
	READFDR(filename, libname, volname, &mode, field, &temp, &rc);

	receiver = (0 == rc) ? temp : -1;
}

arguments *wang_os_fetched_args_for(char *this_procedure) {
   const int max_fetched_args = 32;
   char      pname[80 + 1];
   int_16    pcount = 0;
   char    **parm = new char*[max_fetched_args];
   int       i;
   
   init_string_with_blank_pad(pname, 80, this_procedure);

   for (i = 0; i < max_fetched_args; i++) {
      parm[i] = new char[FETCHED_ARG_SIZE + 1];
      init_string_with_blank_pad(parm[i], FETCHED_ARG_SIZE);
   }

   LINKGARG(pname, &pcount,
      parm[0],  parm[1],  parm[2],  parm[3],  parm[4],  parm[5],  parm[6],  parm[7],
      parm[8],  parm[9],  parm[10], parm[11], parm[12], parm[13], parm[14], parm[15],
      parm[16], parm[17], parm[18], parm[19], parm[20], parm[21], parm[22], parm[23],
      parm[24], parm[25], parm[26], parm[27], parm[28], parm[29], parm[30], parm[31]);

   arguments *args = new arguments((int)pcount);

   assert(pcount <= max_fetched_args);
   for (i = 0; i < max_fetched_args; i++)
      if (i < pcount)
         // Assign these to args (args' destructor will free them)
         args->raw_set(i + 1, parm[i]);
      else
         // Free unused args
         delete parm[i];

   delete parm;

   return args;
}


void wang_os_update_fetched_args() {
   LINKPARG();
}


void wang_os_init() {
   initglbs("WPROC   ");
}


void wang_os_increment_link_level() {
   newlevel();
}


void wang_os_decrement_link_level() {
   oldlevel();
}


int_32 wang_os_link(
   char      *a_name,
   arguments *args,
   Boolean    handle_cancel,
   Boolean    transparent,
   int_32    &status)
{
   char linktype[1];
   char path[80];
   int i;
   
   assert( sizeof(int_32) == 4 && sizeof(usign_32) == 4);
   assert( sizeof(int_16) == 2 && sizeof(usign_16) == 2);

   wang_filename name(a_name);
   char *filename = name.filename;
   char *libname = name.libname;
   char *volname = name.volname;

   int_32 userargs = args->count();
   int_32 userargs_swapped;
   
   userargs_swapped = userargs;
   wswap(&userargs_swapped);

   trace(general, "WANG_OS_LINK");
   trace_ss(general, "filename      : ", filename);
   trace_ss(general, "libname       : ", libname);
   trace_ss(general, "volname       : ", volname);
   trace_si(general, "userargs      : ", userargs);

   findrun(filename, libname, volname, path, linktype);

   for (i = 1; i <= userargs; i ++) {
      trace_ss(general, "arg           : ",  args->value(i));
   }
   trace_si(general, "handle_cancel : ", handle_cancel);
   trace_si(general, "transparent   : ", transparent);
   status = 0;

   int_32 retcode = 0;

   const int fixed_args   = 8;
   const int varying_args = 32;

   void *ptrarg[fixed_args + varying_args];
   int_32 lenarg[fixed_args + varying_args];

   ptrarg[0] = (void *) filename;
   lenarg[0] = 8;

   ptrarg[1] = (void *) linktype;
   lenarg[1] = 1;

   ptrarg[2] = (void *) libname;
   lenarg[2] = 8;

   ptrarg[3] = (void *) volname;
   lenarg[3] = 6;

   ptrarg[4] = (void *) &userargs_swapped;
   lenarg[4] = 4;

   const int args_offset = 5;
   int offset = args_offset;
   int_32 *integer_value;
   char   *string_value;

   // Make a copy of the value of each argument and use a pointer to the
   // copy as the parameter to LINK2.  LINK2 will pass these copies "by
   // reference" to the linked-to program (if the program modifies the
   // parameters, LINK2 will in turn modify the values of these copies
   // to achieve the effect of pass by reference).

   for (i = 1; i <= userargs; i++) {
      expression *the_exp = args->exp(i);
      if (the_exp) {
         // Arg is an lvalue (non-lvalue args have a null expression pointer)
         // Make a copy from the expression object.
         if (the_exp->kind() == expression::integer_kind) {
            integer_value = new int_32;
            *integer_value = the_exp->integer();
	    wswap(integer_value);
            ptrarg[offset] = (void *) integer_value;
            lenarg[offset++] = sizeof(int_32);
         }
         else {
            string_value = dup_string(the_exp->string());
            ptrarg[offset] = (void *) string_value;
            lenarg[offset++] = strlen(string_value);
         }
      }
      else {
         // Arg is a constant--make a copy from the arg value.
         if (args->value_kind(i) == expression::integer_kind) {
            integer_value = new int_32;
            string_to_int_32(args->value(i), *integer_value);
	    wswap(integer_value);
            ptrarg[offset] = (void *) integer_value;
            lenarg[offset++] = sizeof(int_32);
         }
         else {
            string_value = dup_string(args->value(i));
            ptrarg[offset] = (void *) string_value;
            lenarg[offset++] = strlen(string_value);
         }
      }
   }

   offset = args_offset + userargs;

   ptrarg[offset + 0] = (void *) (handle_cancel ? "C" : " ");
   lenarg[offset + 0] = 1;
   ptrarg[offset + 1] = (void *) &status; // compcode
   lenarg[offset + 1] = 4;
   ptrarg[offset + 2] = (void *) &retcode;
   lenarg[offset + 2] = 4;

   // Dump all the global symbols to a temp file.
   // (This used to be done in machine::exec_run() in e_run.cc)
   parent_machine->symbol_table_ptr()->dump_globals(true);
   
   status = 0;
   argcount_is(fixed_args + userargs);
   LINK2(
      	ptrarg[0],lenarg[0],
	ptrarg[1],lenarg[1],
	ptrarg[2],lenarg[2],
      	ptrarg[3],lenarg[3],
	ptrarg[4],lenarg[4],
	ptrarg[5],lenarg[5],
      	ptrarg[6],lenarg[6],
      	ptrarg[7],lenarg[7],
	ptrarg[8],lenarg[8],
	ptrarg[9],lenarg[9],
      	ptrarg[10],lenarg[10],
	ptrarg[11],lenarg[11],
	ptrarg[12],lenarg[12],
      	ptrarg[13],lenarg[13],
	ptrarg[14],lenarg[14],
	ptrarg[15],lenarg[15],
      	ptrarg[16],lenarg[16],
	ptrarg[17],lenarg[17],
	ptrarg[18],lenarg[18],
      	ptrarg[19],lenarg[19],
	ptrarg[20],lenarg[20],
	ptrarg[21],lenarg[21],
      	ptrarg[22],lenarg[22],
	ptrarg[23],lenarg[23],
	ptrarg[24],lenarg[24],
      	ptrarg[25],lenarg[25],
	ptrarg[26],lenarg[26],
	ptrarg[27],lenarg[27],
      	ptrarg[28],lenarg[28],
	ptrarg[29],lenarg[29],
	ptrarg[30],lenarg[30],
      	ptrarg[31],lenarg[31],
	ptrarg[32],lenarg[32],
	ptrarg[33],lenarg[33],
      	ptrarg[34],lenarg[34],
	ptrarg[35],lenarg[35],
	ptrarg[36],lenarg[36],
      	ptrarg[37],lenarg[37],
	ptrarg[38],lenarg[38],
	ptrarg[39],lenarg[39]
      );

   wswap(&retcode);
   wswap(&status);

   // The link has completed.  The copies we made of the arguments must now
   // all be deleted; however, before deleting them, see which ones are
   // associated with procedure variables (lvalues) and assign to those
   // variables the values of any copies that were changed by the program
   // run by LINK2.

   // FIRST - update the globals so if any of the args are global they won't be overridden.
   // (This used to be done in machine::exec_run() in e_run.cc)
   parent_machine->symbol_table_ptr()->update_globals(true);

   offset = args_offset;
   for (i = 1; i <= userargs; i++) {
      // Examine each argument
      expression *the_exp = args->exp(i);
      if (the_exp) {
         // Arg is an lvalue--assign copied value if different from original
         if (the_exp->kind() == expression::integer_kind) {
            integer_value = (int_32 *) ptrarg[offset];
	    wswap(integer_value);
            if (*integer_value != the_exp->integer())
               parent_machine->assign(new expression(*the_exp), *integer_value);
            delete integer_value;
         }
         else {
            string_value = (char *) ptrarg[offset];
            if (strcmp(string_value, the_exp->string()) != 0)
               parent_machine->assign(new expression(*the_exp), string_value);
            delete string_value;
         }
      }
      else {
         // Arg is a constant--just delete the copied value
         if (args->value_kind(i) == expression::integer_kind) {
            integer_value = (int_32 *) ptrarg[offset];
            delete integer_value;
         }
         else {
            string_value = (char *) ptrarg[offset];
            delete string_value;
         }
      }
      offset++;
   }

   return retcode;
}


int_32 wang_os_print(
   char   *filename,
   char   *libname,
   char   *volname,
   char   *fclass,
   char   *status,
   int_32  form,
   int_32  copies,
   char   *disp)
{
   int_32 retcode = 0;

   if (form < 0)
      wang_os_extract_int("FN", form);

   if (fclass[0] == '?')
      wang_os_extract_alpha("PC", fclass);

   wswap(&copies);
   wswap(&form);
   
   argcount_is(9);
   PRINT(filename, libname, volname, status, disp, &copies, fclass, &form,
      &retcode);

   wswap(&retcode);
   
   return retcode;
}


int_32 wang_os_putparm(
   Boolean            is_enter,
   const char        *put_label,
   const char        *ref_label,
   const char        *prname,
   int                pfkey,
   string_array_data &keylist)
{
   int_32 usagecnt = 1;
   int_32 keycnt = keylist.dimension() / 2;
   int_32 retcode = 0;
   char   aid[2] = " ";
   int    i;

   aid[0] = pfkey + (pfkey <= 16 ? 64 : 80);

   trace(general, "WANG_OS_PUTPARM");
   trace_si(general, "is_enter : ", is_enter);
   trace_ss(general, "put label: ", put_label);
   if (ref_label)
      trace_ss(general, "ref label: ", ref_label);
   trace_ss(general, "prname   : ", prname);
   trace_ss(general, "pfkey    : ", aid);
   for (i = 0; i < keylist.dimension(); i += 2) {
      trace_ss(general, "keyword  : ", keylist[i].contents_address());
      const char *kval = keylist[i+1].contents_address();
      if (kval[0] == 0x01)
         trace_ss(general, "keyvalue (pbr) : ", kval + 1);
      else
         trace_ss(general, "keyvalue : ", kval);
   }

   const int fixed_args   = 9;
   const int triplets     = 80;		/* <<<<<<<<< HARDCODED LIMIT OF 80 KEYWORDS <<<<<<<<< */
   const int varying_args = triplets * 3;

   void *arg[fixed_args + varying_args];

   assert(keycnt<=triplets);

   wswap(&usagecnt);
   int_32 keycnt_swapped = keycnt;
   wswap(&keycnt_swapped);

   arg[0] = (void *) is_enter ? (char*)"E" : (char*)"D";
   arg[1] = (void *) &usagecnt;
   arg[2] = (void *) prname;
   arg[3] = (void *) &keycnt_swapped;

   int         offset = 4;
   int         keyargs = keycnt * 3;
   const char *keyval;
   int_32      keylen[triplets];
   int         k = 0; // keylist index
   int         l = 0; // length index
   Boolean     pbr;

   for (i = 0; i < keyargs;) {
      arg[offset + i++] = (void *) keylist[k++].contents_address();
      keyval = keylist[k++].contents_address();
      // If 1st byte is 1, next 16 bytes are label/keyword of partial back ref
      pbr = BOOLEAN(keyval[0] == 0x01);
      if (pbr)
      {
	   keylen[l] = -1; // -1 tells WISP this is pbr
      }
      else
      {
	   keylen[l] = strlen(keyval); 
      }
      
      arg[offset + i++] = (void *) (keyval + (pbr ? 1 : 0));
      trace_ss(general, "Passed keyvalue : ", (char*) (arg[offset + i - 1]));
      wswap(&keylen[l]);
      arg[offset + i++] = (void *) &keylen[l++];
   }

   offset += keyargs;

   arg[offset + 0] = (void *) aid;
   arg[offset + 1] = (void *) put_label;
   arg[offset + 2] = (void *) ref_label;
   arg[offset + 3] = (void *) " ";
   arg[offset + 4] = (void *) &retcode;

   

   argcount_is(fixed_args + keycnt*3);
   PUTPARM(
      	arg[0],arg[1],arg[2],arg[3],arg[4],arg[5],arg[6],arg[7],arg[8],arg[9],
      	arg[10],arg[11],arg[12],arg[13],arg[14],arg[15],arg[16],arg[17],arg[18],arg[19],
      	arg[20],arg[21],arg[22],arg[23],arg[24],arg[25],arg[26],arg[27],arg[28],arg[29],
      	arg[30],arg[31],arg[32],arg[33],arg[34],arg[35],arg[36],arg[37],arg[38],arg[39],
      	arg[40],arg[41],arg[42],arg[43],arg[44],arg[45],arg[46],arg[47],arg[48],arg[49],
      	arg[50],arg[51],arg[52],arg[53],arg[54],arg[55],arg[56],arg[57],arg[58],arg[59],
      	arg[60],arg[61],arg[62],arg[63],arg[64],arg[65],arg[66],arg[67],arg[68],arg[69],
	arg[70],arg[71],arg[72],arg[73],arg[74],arg[75],arg[76],arg[77],arg[78],arg[79],
	arg[80],arg[81],arg[82],arg[83],arg[84],arg[85],arg[86],arg[87],arg[88],arg[89],
	arg[90],arg[91],arg[92],arg[93],arg[94],arg[95],arg[96],arg[97],arg[98],arg[99],
	arg[100],arg[101],arg[102],arg[103],arg[104],arg[105],arg[106],arg[107],arg[108],arg[109],
	arg[110],arg[111],arg[112],arg[113],arg[114],arg[115],arg[116],arg[117],arg[118],arg[119],
	arg[120],arg[121],arg[122],arg[123],arg[124],arg[125],arg[126],arg[127],arg[128],arg[129],
	arg[130],arg[131],arg[132],arg[133],arg[134],arg[135],arg[136],arg[137],arg[138],arg[139],
      	arg[140],arg[141],arg[142],arg[143],arg[144],arg[145],arg[146],arg[147],arg[148],arg[149],
      	arg[150],arg[151],arg[152],arg[153],arg[154],arg[155],arg[156],arg[157],arg[158],arg[159],
      	arg[160],arg[161],arg[162],arg[163],arg[164],arg[165],arg[166],arg[167],arg[168],arg[169],
	arg[170],arg[171],arg[172],arg[173],arg[174],arg[175],arg[176],arg[177],arg[178],arg[179],
	arg[180],arg[181],arg[182],arg[183],arg[184],arg[185],arg[186],arg[187],arg[188],arg[189],
	arg[190],arg[191],arg[192],arg[193],arg[194],arg[195],arg[196],arg[197],arg[198],arg[199],
	arg[200],arg[201],arg[202],arg[203],arg[204],arg[205],arg[206],arg[207],arg[208],arg[209],
      	arg[210],arg[211],arg[212],arg[213],arg[214],arg[215],arg[216],arg[217],arg[218],arg[219],
      	arg[220],arg[221],arg[222],arg[223],arg[224],arg[225],arg[226],arg[227],arg[228],arg[229],
      	arg[230],arg[231],arg[232],arg[233],arg[234],arg[235],arg[236],arg[237],arg[238],arg[239],
      	arg[240],arg[241],arg[242],arg[243],arg[244],arg[245],arg[246],arg[247],arg[248],arg[249]
      );

   wswap(&retcode);
   
   return retcode;
}


int_32 wang_os_rename(
   Boolean is_library,
   char   *file,
   char   *library,
   char   *volume,
   char   *new_file,
   char   *new_library)
{
   int_32 retcode = 0;

   argcount_is(7);
   wrename(is_library ? "L" : "G", file, library, volume,
      new_file, new_library, &retcode);

   wswap(&retcode);
   
   return retcode;
}


int_32 wang_os_scratch(
   Boolean is_library,
   char   *file,
   char   *library,
   char   *volume)
{
   int_32 retcode = 0;
   char   l_library[8], l_volume[6];

   /* The SCRATCH statement defaults to OUTLIB and OUTVOL if not specified */

   if (!is_library && 0==memcmp(library,"        ",8))
   {
	wang_os_extract_alpha("OL",l_library);
	library = l_library;
   }
   if (0==memcmp(volume,"      ",6))
   {
	wang_os_extract_alpha("OV",l_volume);
	volume = l_volume;
   }

   argcount_is(5);
   SCRATCH(is_library ? "L" : "F", file, library, volume, &retcode);

   wswap(&retcode);
   
   return retcode;
}


void wang_os_set_alpha(char *keyword, char *value) {
   argcount_is(2);
   SET(keyword, (void *) value);
}


void wang_os_set_int(char *keyword, int_32 value) {
   argcount_is(2);
   wswap(&value);
   SET(keyword, (void *) &value);
}

/*
**	ROUTINE:	write_string_literal()
**
**	FUNCTION:	Write out a proc style string literal.
**			This is used to generate procs to be submitted.
**
**	DESCRIPTION:	This routine will write out a proc style string literal.
**			It will enclose it in quotes and handle spanning multiple
**			lines and translate internal quote characters into syntactically
**			correct two quote chars.
**
**	ARGUMENTS:
**	fh		The open file handle positioned to the beginning of a line.
**	str		The string to write out.
**	size		The size of the string. (If size is < 0 then use strlen(str) to get size.)
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/

void write_string_literal(FILE *fh, char *str, int_32 size)
{
#define LAST_COL 71
	int str_idx, col_idx;
	
	if (size < 0) size = strlen(str);
	
	fputc('\"',fh);
	col_idx = 1;
	for (str_idx=0; str_idx<size; str_idx++)
	{
		if ('\"' == str[str_idx])
		{
			/*
			** Change quote (") into two quotes ("")
			*/

			fputc(str[str_idx],fh);
			col_idx++;
			if (LAST_COL==col_idx)
			{
				fputc('\n',fh);
				col_idx = 0;
			}
		}

		fputc(str[str_idx],fh);
		col_idx++;
		if (LAST_COL==col_idx)
		{
			fputc('\n',fh);
			col_idx = 0;
		}
	}

	fputc('\"',fh);
	col_idx++;
	if (col_idx+1 >=LAST_COL)
	{
		/*
		**  Ensure there is room for at least one more character on this line.
		**  A comma ',' is often written following a strig literal.
		*/
		fputc('\n',fh);
	}
}

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
   Boolean  requeue)
{
	trace(general, "SUBMIT");
	for (int i = 0; i < parameters->count; i++)
	{
		if (parameters->string[i])
			trace_ss(general, "Parameters  : ", parameters->string[i]);
		else
			trace_si(general, "Parameters  : ", parameters->integer[i]);
	}
	trace_si(general,    "Globals     : ", globals);
	trace_si(general,    "Environment : ", environment);
	trace_ss(general,    "Dump        : ", dump);
	trace_ss(general,    "Action      : ", action);
	trace_si(general,    "Requeue     : ", requeue);


	int_32 retcode;

	if (jclass[0] == '?')
		wang_os_extract_alpha("JC", jclass);

	char *disposition;	
	disposition = (requeue) ? (char*)"R" : (char*)"D";
		
	char *abort_action = "R";
	if      ('Y' == *dump) abort_action = "D";
	else if ('N' == *dump) abort_action = "N";
	else if ('P' == *dump) abort_action = "D";

	char *limit_flag = "C";
	if      ('C' == *action) limit_flag = "C";
	else if ('P' == *action) limit_flag = "P";
	else if ('W' == *action) limit_flag = "W";

	wswap(&cpulimit);

	if (globals || environment || parameters->count > 0)
	{
		/*
		**	If option GLOBALS=YES or ENVIRONMENT=YES or USING was requested then
		**	write out a new procedure with the globals declared then submit it instead.
		*/

		char	file[8] , lib[8], vol[6], filepath[80];
		FILE *fh;
		time_t the_time = time(NULL);
	   
		tempproc(file, lib, vol, filepath);
		fh = fopen(filepath,"w");

		fprintf(fh,"Procedure \"%8.8s\" created by %s %s at %s",
			file, product_name(), product_version_str(), ctime(&the_time));

		if (globals)
		{
			fprintf(fh,"\n");
			fprintf(fh,"*\n");
			fprintf(fh,"*  GLOBALS=YES\n");
			fprintf(fh,"*\n");

			parent_machine->symbol_table_ptr()->write_declare_globals(fh);
		}

		if (environment)
		{
			char	t_str[80];
			int_32	t_int;
			
			fprintf(fh,"\n");
			fprintf(fh,"*\n");
			fprintf(fh,"*  ENVIRONMENT=YES\n");
			fprintf(fh,"*\n");
			
			wang_os_extract_alpha("IL",t_str);
			fprintf(fh,"SET INLIB = \"%8.8s\"\n",t_str);
			wang_os_extract_alpha("IV",t_str);
			fprintf(fh,"SET INVOL = \"%6.6s\"\n",t_str);

			wang_os_extract_alpha("OL",t_str);
			fprintf(fh,"SET OUTLIB = \"%8.8s\"\n",t_str);
			wang_os_extract_alpha("OV",t_str);
			fprintf(fh,"SET OUTVOL = \"%6.6s\"\n",t_str);

			wang_os_extract_alpha("PL",t_str);
			fprintf(fh,"SET PROGLIB = \"%8.8s\"\n",t_str);
			wang_os_extract_alpha("PV",t_str);
			fprintf(fh,"SET PROGVOL = \"%6.6s\"\n",t_str);

			wang_os_extract_alpha("RL",t_str);
			fprintf(fh,"SET RUNLIB = \"%8.8s\"\n",t_str);
			wang_os_extract_alpha("RV",t_str);
			fprintf(fh,"SET RUNVOL = \"%6.6s\"\n",t_str);

			wang_os_extract_alpha("SL",t_str);
			fprintf(fh,"SET SPOOLLIB = \"%8.8s\"\n",t_str);
			wang_os_extract_alpha("SV",t_str);
			fprintf(fh,"SET SPOOLVOL = \"%6.6s\"\n",t_str);

			wang_os_extract_alpha("WV",t_str);
			fprintf(fh,"SET WORKVOL = \"%6.6s\"\n",t_str);

			wang_os_extract_alpha("JC",t_str);
			fprintf(fh,"*SET JOBCLASS = \"%1.1s\"\n",t_str);
			wang_os_extract_alpha("JS",t_str);
			fprintf(fh,"*SET JOBQUEUE = \"%1.1s\"\n",t_str);
			wang_os_extract_alpha("PM",t_str);
			fprintf(fh,"SET PRNTMODE = \"%1.1s\"\n",t_str);
			wang_os_extract_alpha("PC",t_str);
			fprintf(fh,"SET PRTCLASS = \"%1.1s\"\n",t_str);

			wang_os_extract_int("FN",t_int);
			fprintf(fh,"SET FORM# = %ld\n",(long)(t_int));
			wang_os_extract_int("LI",t_int);
			fprintf(fh,"SET LINES = %ld\n",(long)(t_int));
			wang_os_extract_int("P#",t_int);
			fprintf(fh,"SET PRINTER = %ld\n",(long)(t_int));
		}

		fprintf(fh,"\n");
		fprintf(fh,"*\n");
		fprintf(fh,"* SUBMIT -> RUN\n");
		fprintf(fh,"*\n");
		fprintf(fh,"RUN \"%8.8s\"",filename);
		if (*libname && ' ' != *libname) fprintf(fh," IN \"%8.8s\"",libname);
		if (*volname && ' ' != *volname) fprintf(fh," ON \"%6.6s\"",volname);
		fprintf(fh,"\n");

		if (parameters->count > 0)
		{
			fprintf(fh,"USING\n");

			for(int i = 0; i < parameters->count; i++)
			{
				if (i > 0) fprintf(fh,",\n");

				if (parameters->string[i])
				{
					write_string_literal(fh,parameters->string[i],-1);
				}
				else
				{
					fprintf(fh,"%ld",(long)(parameters->integer[i]));
				}
			}
			fprintf(fh,"\n");
			
		}
		fprintf(fh,"ERROR EXIT IS DP  CANCEL EXIT IS DP\n");

		if ('R' == *disposition)
		{
			fprintf(fh,"\n");
			fprintf(fh,"*\n");
			fprintf(fh,"* DISPOSITION=REQUEUE\n");
			fprintf(fh,"*\n");
			fprintf(fh,"RETURN\n");
		}
		
		fprintf(fh,"\n");
		fprintf(fh,"DP: DESTROY PROCEDURE\n");

		fprintf(fh,"\n");
		fprintf(fh,"* --- END ---\n");
		
		fclose(fh);

		argcount_is(11);
		SUBMIT(file, lib, vol, proc_id, status, disposition, jclass, abort_action,
		       &cpulimit, limit_flag, &retcode);
	}
	else
	{
		argcount_is(11);
		SUBMIT(filename, libname, volname, proc_id, status, disposition, jclass, abort_action,
		       &cpulimit, limit_flag, &retcode);
	}

	wswap(&retcode);

	return retcode;
}


char *wang_to_native_file_name(const char *a_name, int &file_kind) {
   // File kinds: 0 = Not found; 1 = procedure; 2 = not procedure

   char          linktype[1];
   char          path[80];
   wang_filename name(a_name);

   if (findrun(name.filename, name.libname, name.volname, path, linktype) == 0) {
      trace_ss(general, "findrun() path = ", path);
      trace_sc(general, "findrun() linktype = ", linktype[0]);
      file_kind = runtype(path);
      trace_si(general, "runtype() type = ", file_kind);
      file_kind = (file_kind == RUN_PROC || file_kind == RUN_PROCOBJ) ? 1 : 2;
   }
   else
      file_kind = 0;

   return dup_string(path);
}


void wang_os_access_to_machine(machine *a_machine) {
   // This routine is here to facilitate pass by reference to COBOL programs
   // (or any other program run via LINK2 that supports pass by reference).
   // Normally, code in wang_os.cpp could not access the machine
   // of its parent procedure.  To allow this, e_run.cpp calls this routine
   // just before calling the_input->run().  It is assumed that variable
   // parent_machine will be assigned here each time before a call
   // to wang_os_link().  Actually, the variable will be set each time
   // the_input->run() is called, but the value is only used by wang_os_link().

   // Also used by SUBMIT.

   parent_machine = a_machine;
}


void wang_os_first_procedure_name(char *a_name) {
   firstproc(a_name);
}

#endif

/*
**	History:
**	$Log: wang_os.cpp,v $
**	Revision 1.27.2.2  2003/02/11 19:12:59  gsl
**	fix duplicate history
**	
**	Revision 1.27.2.1  2003/02/11 18:52:00  gsl
**	Removed unneeded #ifdef code for AIX and DEBUG
**	
**	Revision 1.27  2001/08/22 20:42:14  gsl
**	fix gnu errors
**	
**	Revision 1.26  1999-08-29 13:21:10-04  gsl
**	Move the vwang_title() call to driver.cpp so it doesn't get called
**	if just checking the version.
**
**	Revision 1.25  1999-01-19 11:16:31-05  gsl
**	fix typo in last fix
**
**	Revision 1.24  1999-01-19 11:08:41-05  gsl
**	Fix warning
**
**	Revision 1.23  1999-01-04 15:27:10-05  gsl
**	Fix SCRATCH to default to OUTLIB and OUTVOL if not specfied
**
**	Revision 1.22  1998-08-31 15:50:40-04  gsl
**	drcs update
**
**	Revision 1.21  1998-08-31 15:14:27-04  gsl
**	drcs update
**
**	Revision 1.20  1998-02-11 14:21:44-05  gsl
**	Fix bug is LINK, args 16-18 were repeated.
**
**	Revision 1.19  1997-10-01 18:15:48-04  gsl
**	fix warnings
**
**	Revision 1.18  1997-06-10 10:45:10-04  scass
**	Final for portability.
**
**	Revision 1.17  1997-06-10 01:09:13-04  scass
**	change int_32 to long
**
**	Revision 1.16  1997-06-09 15:49:41-04  scass
**	Changed type of length parameter from void * to
**	int_32 because more correct and was causing problems on Alpha
**	port.  Now has two arrays for ptr and len pair instead
**	of one large array.
**
**	Revision 1.15  1997-01-14 20:12:03-05  gsl
**	Add a call to vwang_title()
**
**	Revision 1.14  1996-07-25 16:48:35-07  gsl
**	NT
**
**	Revision 1.13  1996-07-25 11:16:39-07  gsl
**	Renamed from wang_os.cc to wang_os.cpp
**
**	Revision 1.12  1996-04-18 10:01:11-07  jockc
**	moved declaration of for loop index from for loop to function
**	auto decl area
**
**
**
*/

// Revision 1.11  1995/10/19  10:47:33  gsl
// In a SUBMIT, if DISP=REQUEUE then don't destroy the generated procedure
//
// Revision 1.10  1995/10/18  17:16:05  gsl
// Extensive mods to the submit logic to add support for GLOBALS=YES
// and ENVIRONMENT=YES.
// If these are specified or args are passed with the USING clause
// then wproc will generate a new temp proc which will setup the
// globals or environment and then run the program.  This generated
// proc is then submitted.
//
// Revision 1.9  1995/10/16  14:06:55  gsl
// On a SUBMIT, pass thru the Disposition, Abort_action, and Limit_flag
// to the VSSUB SUBMIT for handling
//
//	----------------------------
//	revision 1.8
//	date: 1995-07-17 10:02:46-04;  author: gsl;  state: V3_3_18;  lines: +1 -1
//	fix sprintf() arg type mismatch warning
//	----------------------------
//	revision 1.7
//	date: 1995-07-17 09:50:51-04;  author: gsl;  state: Exp;  lines: +6 -0
//	On a SUBMIT USING change integer parameters into strings that
//	can be passed on the command line.
//	----------------------------
//	revision 1.6
//	date: 1995-06-10 13:48:17-04;  author: gsl;  state: V3_3_17;  lines: +8 -0
//	Moved the dump_globals()/update_globals() to here from exec_run().
//	----------------------------
//	revision 1.5
//	date: 1995-05-12 10:55:22-04;  author: gsl;  state: V3_3_16;  lines: +1 -1
//	On the RENAME verb changed the type from "F" to "G" to accomadate
//	renaming a file into a different directory.
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:34-04;  author: gsl;  state: V3_3_15;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:49-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:35-05;  author: gsl;  state: V3_3x12;  lines: +178 -65
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:34-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
