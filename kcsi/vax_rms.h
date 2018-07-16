/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
/* VAX_RMS.H
Define FABs, RABs, NAMs and XABs as typedefs to make them easier to use
and not mess with the Digital standard .h files.
*/

typedef struct FAB      KFAB;
typedef struct RAB      KRAB;
typedef struct XABALL   KXABALL;       /* Allocation */
typedef struct XABDAT   KXABDAT;       /* Date/Time */
typedef struct XABFHC   KXABFHC;       /* File Header Characteristics */
typedef struct XABKEY   KXABKEY;       /* Key Definition */
typedef struct XABPRO   KXABPRO;       /* File Protection */
typedef struct XABRDT   KXABRDT;       /* Revision Date and Time */
typedef struct XABSUM   KXABSUM;       /* Summary */
typedef struct XABTRM   KXABTRM;       /* Terminal Control */
/*
**	History:
**	$Log: vax_rms.h,v $
**	Revision 1.3  1996/09/17 23:34:20  gsl
**	drcs update
**	
**
**
*/
