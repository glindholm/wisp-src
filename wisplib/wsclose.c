/*  Terminates screen 'file'	*/

#define CLOSE_WORK_STATION	4

#include "idsistd.h"
#ifndef NULL
#define NULL 0
#endif

WSCLOSE()
{
	unsigned char function;
	
	function = CLOSE_WORK_STATION;							/* Set up for call to vwang.		*/
	vwang(&function,NULL,NULL,NULL,NULL,NULL);
}
