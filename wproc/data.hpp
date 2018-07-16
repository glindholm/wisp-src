//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : data.hpp
// Author : George Soules
// Date   : 16 April 1991

#ifndef DATA__HPP
#define DATA__HPP

// Classes
#include "pcode.hpp"
#include "scope.hpp"
#include "stateobj.hpp"

// Definitions and subprograms
#include "assert.hpp"
#include "environ.hpp"


typedef int_16 array_index;
typedef int_16 string_index;


class data : public object {
   public:
#if WANG
      data() {the_ok = true; the_raw_data = NULL;}
#else
      data() {the_ok = true;}
#endif
      virtual ~data() {}
      Boolean ok() {return the_ok;}
#if WANG
      usign_8 *raw_data()      {return the_raw_data;}
      int      raw_data_size() {return the_raw_data_size;}
#endif
   protected:
      Boolean the_ok;
#if WANG
      usign_8 *the_raw_data;
      int      the_raw_data_size;
#endif
};


class array_data : public data {
   public:
      array_index dimension() {return the_dimension;}
      virtual void sort() {};
   protected:
      array_index the_dimension;
};


class integer_data : public data {
   public:
      integer_data(int_32 an_integer);
      ~integer_data() {}
      int_32 contents() {return the_contents;}
      void   set_contents(int_32 an_integer);
   private:
      int_32 the_contents;
};


class integer_array_data : public array_data {
   public:
      integer_array_data(array_index a_dimension, int_32 an_integer);
      ~integer_array_data();
      int_32 &operator[] (array_index an_index) {
         assert(an_index >= 0 && an_index < the_dimension);
         return the_contents[an_index];
      }
      void sort();
   private:
      int_32 *the_contents;
};


class string_data : public data {
   public:
      ~string_data();
      string_index size() {return the_size;}
      char        *contents();
      const char  *contents_address() {return the_contents;}
      virtual void set_contents(const char *a_string) = 0;
      char        *substring(string_index start, string_index length);

      void set_substring(
         string_index start,
         string_index length,
         const char  *a_string);

   protected:
      char *the_contents;
      string_index the_size;
      string_index last_non_blank(const char *a_string);
};


class fixed_string_data : public string_data {
   public:
      fixed_string_data(string_index a_size, const char *a_string);
      void set_contents(const char *a_string);
};


class dynamic_string_data : public string_data {
   public:
      dynamic_string_data(const char *a_string);
      void set_contents(const char *a_string);
   private:
      string_index the_max_size;
};


class string_array_data : public array_data {
   public:
      string_array_data(
         array_index  a_dimension,
         string_index a_size,
         const char  *initial_string);

      ~string_array_data();
      string_data &operator[] (array_index an_index) {
         assert(an_index >= 0 && an_index < the_dimension);
         return *the_contents[an_index];
      }
      void sort();
   private:
      string_data **the_contents;
      void delete_array_and_elements();
};


class builtin_data : public data {
   public:
      builtin_data(int a_kind, int min_args, int max_args) {
         the_kind     = a_kind;
         the_min_args = min_args;
         the_max_args = max_args;
      }
      int kind()     {return the_kind;}
      int min_args() {return the_min_args;}
      int max_args() {return the_max_args;}
   private:
      int the_kind;
      int the_min_args;
      int the_max_args;
};


class label_data : public data {
   public:
      label_data(offset a_location = 0) {
         the_location = a_location;
         the_value    = 0;
      }
      offset location()                {return the_location;}
      int_32 value()                   {return the_value;}
      void   set_value(int_32 a_value) {the_value = a_value;}
   private:
      offset the_location;
      int_32 the_value;
};

#endif


//
//	History:
//	$Log: data.hpp,v $
//	Revision 1.5  1998/08/31 19:13:39  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/data.hpp,v
//	Working file: data.hpp
//	head: 1.4
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
//	total revisions: 4;	selected revisions: 4
//	description:
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:46-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:03-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:42-05;  author: gsl;  state: V3_3x12;  lines: +2 -2
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:01-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
