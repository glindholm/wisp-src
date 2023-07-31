/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


#define IVS__CHINESE__
#include <stdio.h>
#include <string.h>
#include "vchinese.h"

/* 
 * module: vchinese stubs
 *
 */
void IVS_xlat_stream(unsigned char *instr, int insz, unsigned char *out, int* outsz, struct xlcontext **ctx)
{
	memcpy(out,instr,insz);
	*outsz = insz;
	out[insz]=(char)0;
}
int IVS_vlanguage(const char* path)
{
	return 0;
}
/*
**	History:
**	$Log: vchstubs.c,v $
**	Revision 1.10  2003/01/31 19:38:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.9  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.8  2002/07/15 13:29:02  gsl
**	IVS_ globals
**	
**	Revision 1.7  2002/07/12 20:40:43  gsl
**	Global unique WL_ changes
**	
**	Revision 1.6  1996/10/11 22:16:01  gsl
**	drcs update
**	
**
**
*/
