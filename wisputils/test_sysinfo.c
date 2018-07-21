/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
**
**
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
**
**
**
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>

#include <sys/systeminfo.h>

static int call_sysinfo(int command, const char *command_name)
{
	char buff[200];
	
	if ( -1 == sysinfo(command, buff, sizeof(buff)))
	{
		printf("sysinfo(%s) failed errno=%d\n", command_name, errno);
		
		return(1);
	}

	printf("sysinfo(%s)=[%s]\n", command_name, buff);
	
	return(0);
	
}

int main(int argc, char* argv[])
{
	
	call_sysinfo(SI_ARCHITECTURE,"SI_ARCHITECTURE");
	call_sysinfo(SI_HOSTNAME,"SI_HOSTNAME");
	call_sysinfo(SI_HW_PROVIDER,"SI_HW_PROVIDER");
	call_sysinfo(SI_HW_SERIAL,"SI_HW_SERIAL");
	call_sysinfo(SI_MACHINE,"SI_MACHINE");
	call_sysinfo(SI_RELEASE,"SI_RELEASE");
	
	return(0);
}

/*
**	History:
**	$Log: test_sysinfo.c,v $
**	Revision 1.1  2003/02/12 19:13:49  gsl
**	Test calls to sysinfo()
**	
**
**
**
*/
