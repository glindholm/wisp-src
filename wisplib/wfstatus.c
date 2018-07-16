/* WFSTATUS.C ... Converts the VAX/VMS status code value into the VS equivalent.						*/

#include <ctype.h>
#include "idsistd.h"

wfstatus(status_val)
char *status_val;					                                /* Pointer to the arg.			*/
{
	char l_status_val[2];								/* Storage for the arg.			*/
	int i;

 	memcpy(l_status_val, status_val, 2);						/* Get the value for speed purposes.	*/

	switch(l_status_val[0])
	{                
											
		case '0':                                                               /* The following first value causes the	*/
		case '2':								/* status value to be unconverted.	*/
		case '3':
		case '9':								/* Note that codes starting with a '9'	*/
		{									/* will have to be handled later.	*/
			break;								/* Value stays the same.		*/
		}
		case '1':								/* Any status starting with a '1' is	*/
	 	{                             						/* translated to a '10'.		*/
			l_status_val[1] = '0';
			break;
		}
	}                                                                                                                         
 	memcpy(status_val, l_status_val, 2);
}
                                                                                                                         