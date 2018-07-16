/*----
General utilties and routines for miscellaneous field
and file manipulation.
------*/

#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

/*----
Truncate to last non-space
----*/
trunc(str)
char *str;
{
	int len;
	while(len = strlen(str))
		{
		if(isspace(str[--len]))
			str[len] = '\0';
		else
			break;
		}
}

/*----
Add spaces to the end of a field to len
------*/
untrunc(s,len)
char *s,len;
{
	while(strlen(s) < len)
		strcat(s," ");
}

/*----
Returns true if str is spaces for len
------*/
isblank(str,len)
char *str;
int len;
{
	while((*str) &&
	      (len)   )
		{
		if(*str != ' ')
			return(0);
		++str;
		--len;
		}
	return(1);
}

/*----
Returns true is a system named file exists.
------*/
exists(name)
char *name;
{
	struct stat stat_buf;
	int rc;

	rc = stat(name,&stat_buf);
	if(rc == 0)
		return(1);
	return(0);
}

