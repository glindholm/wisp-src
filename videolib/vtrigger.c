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


/*			This is a stubb to replace the actual trigger routine. THIS MUST NEVER BE				*/
/*			REPLACED. To have an actual trigger, this routine is replaced by an					*/
/*			alternate routine of the same name.									*/

#include "video.h"

int VL_vtrigger()
{
	return(FALSE);									/* This does not trigger.		*/
}
/*
**	History:
**	$Log: vtrigger.c,v $
**	Revision 1.11  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/15 20:16:15  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.9  1996/10/11 22:16:22  gsl
**	drcs update
**	
**
**
*/
