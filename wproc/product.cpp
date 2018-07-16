// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : product.cpp
// Author : George Soules
// Date   : 12 February 1991

// Specification
#include "product.hpp"

// Classes
// (none)

// Definitions and subprograms
#include <stdio.h>
#include <string.h>

extern "C" char *wisp_version(void);

char *product_copyright() {
   static char notice[256];

#if WANG
#if RUNTIME
   strcpy(notice, "WPROCRT");
#else
   strcpy(notice, "WPROC");
#endif
#else
#if RUNTIME
   strcpy(notice, "RunRT");
#else
   strcpy(notice, "Run");
#endif
#endif

#if DEMO
   strcat(notice, " DEMO");
#endif

#if DOS
   strcat(notice, " v2.0x Copyright (c) 1991-1992 Lexical Software. All Rights Reserved.");
#if NETWORK
   strcat(notice, "\nPortions Copyright (c) 1983-1990 Novel, Inc. All Rights Reserved.");
#endif
#else
   char wproc_version[80];
   sprintf(wproc_version," version %s    (WL=%s)\n", product_version_str(), wisp_version());
   strcat(notice, wproc_version);
   strcat(notice, "Copyright (c) 1991-1993 Lexical Software. All rights reserved.\n");
   strcat(notice, "Portions Copyright (c) 1994-1997 NeoMedia Technologies, Inc.\nAll Rights Reserved.\n");
#endif
   return notice;
}

usign_16 product_version() {
#if DOS
   return 0x0200;
#else
   return WPROC_VERSION;
#endif
}

char *product_version_str(void)
{
	static char version[10];
	static int first = 1;
	
	if (first)
	{
		first = 0;
	
		sprintf(version,"v%d.%02d",(int)(WPROC_VERSION / 100), (int)(WPROC_VERSION % 100));
	}
	
	return version;
}



char *product_name() {
#if WANG
   return "WPROC";
#else
   return "Run";
#endif
}

#if DEMO
char *demo_notice() {
   return "DEMONSTRATION SOFTWARE -- NOT FOR SALE";
}
#endif
