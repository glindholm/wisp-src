#ifdef MSDOS
/*
**	sortx.c		This routine exists only to change the module name
**			to make Intel Code Builder not complain about 
**			duplicate module names.  This is caused because
**			ACUCOBOL run386.lib contains a module named "sort".
*/

#define MSDOS_SORTX
#include "sort.c"

#else /* MSDOS */
static int dummy_sortx;
#endif

