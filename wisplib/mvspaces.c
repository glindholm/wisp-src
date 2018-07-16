#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif

movespaces( start, len )
char *start;
int  *len;
{
	memset( start, ' ', *len );
}
