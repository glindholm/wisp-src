// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : assert.cpp
// Author : George Soules
// Date   : 8 October 1991

// Specification
#include "crt_io.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "fileinfo.hpp"
#include <stdlib.h>
#include "utility.hpp"


void assert_error(char *source_file, int line_number) {
   char num[INT_32_STRING_SIZE];
   int_32_to_ascii(line_number, num);
   use_standard_io();
   write_stdout("\nERROR CODE = ");
   write_stdout(upper_case(file_name(source_file)));
   write_stdout(num);
   write_stdout("\n");
   write_stdout
      ("An unexpected error in WPROC has occurred.  Please report the code above\n");
   write_stdout
      ("to NeoMedia or your dealer.  Thank you for your assistance.\n\n");
   abort();
}

