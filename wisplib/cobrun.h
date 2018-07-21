/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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

/*
	cobrun.h	Defines conditional on what COBOL this wisplib will link with.
*/

#ifndef COBRUN_H
#define COBRUN_H

int wisp_acu_cobol();
int wisp_mf_cobol();

#endif /* COBRUN_H */
/*
**	History:
**	$Log: cobrun.h,v $
**	Revision 1.12  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.11  2002/10/18 19:14:11  gsl
**	Cleanup
**	
**	Revision 1.10  2002/07/12 19:10:11  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  2002/07/02 04:01:30  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.8  1996/08/19 22:32:14  gsl
**	drcs update
**	
**
**
*/
