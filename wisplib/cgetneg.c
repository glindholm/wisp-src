/* IDSI proprietary stuff										*/

#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif

#include "idsistd.h"

cgetneg(value)
int4 *value;
{
	int4 tmp1,tmp2;

	memcpy(&tmp1,value,4);
	tmp2 = -tmp1;
	memcpy(value,&tmp2,4);
}
