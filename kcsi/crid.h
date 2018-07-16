/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

/*----
Function declarations for the support routines for CONTROL
REPORT INQUIRY and DATENTRY
For Acu-COBOL 2.00 using Vision version 3 files.
------*/

int	call_kcsio();
int	call_kdisp();
int	call_ctrlvers();
int	call_rptvers();
int	call_rptcall();
int	call_kformat();
int	call_inqvers();
int	call_parseinq();
int	call_geninq();
int	call_datvers();
int	call_dbsc();
int	call_inidio();
int	call_dmnt();
int	call_kexists();
int	call_valnam();
int	call_valvol();
int	call_valfnm();
int	call_rptwop();
int	call_kmatch();
int	call_kfiltyp();
int	call_ktrace();

void KCSIO();
void INIDIO();
void KDISP();
void KFORMAT();
void CTRLVERS();
void RPTVERS();
void INQVERS();
void DATVERS();
void RPTCALL();
void PARSEINQ();
void GENINQ();
void DBSC();
void DMNT();
void VALNAM();
void VALVOL();
void VALFNM();
void KEXISTS();
void KMATCH();
void KFILTYP();
void KTRACE();
void RPTWOP();

/*
**	History:
**	$Log: crid.h,v $
**	Revision 1.5  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.4  1996/09/18 18:33:54  gsl
**	Fix warnings
**	
**	Revision 1.3  1996-09-17 16:34:03-07  gsl
**	drcs update
**
**
**
*/
