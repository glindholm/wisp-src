/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

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
