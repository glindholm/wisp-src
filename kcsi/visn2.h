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
visn2.h
Created from notes faxed by Greg Lindholm at IDSI. This is supposed
to suffice as a header for VISION file access.
------*/

#define MAGIC		0x10121416
#define LOG_NAME_SIZE	12
#define	MAX_KEYS	15

/* open modes */
#define Finput		0
#define	Foutput		1
#define	Fio		2
#define	Fextend		3
#define Fopen_mask	3
#define	Fread_lock	0x100
#define	Fwrite_lock	0x200
#define	Fbuffered	0x400
#define	Fmass_update	0x600
#define	Flock_mask	0x700
#define Fis_device	0x800

/* start modes */
#define	V_EQUALS	0
#define	V_NOT_LESS	1
#define	V_GREATER	2
#define	V_LESS		3
#define	V_NOT_GREATER	4

/* file types ? */
#define	V_FIXED		'F'
#define	V_VARIABLE	'V'
#define	V_PRINT		'P'

/* the V_FILE struct */

typedef struct _V_FILE{
	int	dummy;
}V_FILE;

/* Error codes */

#define	V_SYS_ERR		1
#define	V_NOT_VISION		2
#define	V_PARAM_ERR		3
#define	V_TOO_MANY_FILES	4
#define	V_NOT_OPEN		5
#define	V_MODE_CLASH		6
#define	V_VERSION_ERR		7
#define V_REC_LOCKED		8
#define	V_BROKEN		9
#define	V_DUPLICATE		10
#define	V_NOT_FOUND		11
#define	V_UNDEF_RECORD		12
#define	V_ILL_KEY		13
#define	V_DISK_FULL		14
#define	V_FILE_LOCKED		15
#define	V_REC_CHANGED		16
#define	V_MISMATCH		17
#define	V_NO_MEMORY 		18
#define	V_ILL_RECSIZE		19
#define	V_MISSING_FILE		20
#define	V_PERMISSION		21
#define	V_NO_PREVIOUS		22

extern int v_errno;

#define	v2_start(f,rec,key,mode)	v3_start(f,rec,key,0,mode)

/*
**	History:
**	$Log: visn2.h,v $
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.3  1996/09/17 23:34:22  gsl
**	drcs update
**	
**
**
*/
