#include "idsistd.h"
WSPAWN(action,progname,name_len,msg,msg_len)
short *action;
char *progname;
short *name_len;
char *msg;
short *msg_len;
{
	char l_msg[200],l_name[200];
	int l_act;
	uint4 vms_status;

	l_act = *action;								/* Copy action code.			*/

	if (*name_len)									/* Copy name (if any)			*/
	{
		memcpy(l_name,progname,*name_len);
	}
	l_name[*name_len] = 0;								/* Put null in.				*/

	if (*msg_len)									/* Copy name (if any)			*/
	{
		memcpy(l_msg,msg,*msg_len);
	}
	l_msg[*msg_len] = 0;								/* Put null in.				*/

	spawn2 (l_act,l_name,l_msg,&vms_status);					/* Do the spawn				*/
}