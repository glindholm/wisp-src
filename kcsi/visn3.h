/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
/*----
visn2.h
Created from notes faxed by Greg Lindholm at IDSI. This is supposed
to suffice as a header for VISION file access.
------*/

#ifndef visn3_H
#define visn3_H

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
#define	F_EQUALS	0
#define	F_NOT_LESS	1
#define	F_GREATER	2
#define	F_LESS		3
#define	F_NOT_GREATER	4

/* file types ? */
/*
#define	V_FIXED		'F'
#define	V_VARIABLE	'V'
#define	V_PRINT		'P'
*/

/* the I_FILE struct */

/*
typedef struct _i_file{
	int	dummy;
}I_FILE;
*/
#define	I_FILE	long

/* Error codes */

#define	E_SYS_ERR		1
#define	E_PARAM_ERR		2
#define	E_TOO_MANY_FILES	3
#define	E_MODE_CLASH		4
#define E_REC_LOCKED		5
#define	E_BROKEN		6
#define	E_DUPLICATE		7
#define	E_NOT_FOUND		8
#define	E_UNDEF_RECORD		9
#define	E_DISK_FULL		10
#define	E_FILE_LOCKED		11
#define	E_REC_CHANGED		12
#define	E_MISMATCH		13
#define	E_NO_MEMORY 		14
#define	E_MISSING_FILE		15
#define	E_PERMISSION		16
#define	E_NO_SUPPORT		17
#define	E_NO_LOCKS		18

#define	W_NO_SUPPORT		100
#define	W_DUP_OK		101

/*
#define	E_NOT_OPEN		5
#define	E_VERSION_ERR		7
#define	E_ILL_KEY		13
#define	E_ILL_RECSIZE		19
#define	E_NO_PREVIOUS		22
#define	E_NOT_VISION		2
*/

extern short f_errno;


extern int i_init();
extern char* i_open();
extern int i_make();
extern int i_close();
extern int i_next();
extern int i_previous();
extern int i_read();
extern int i_start();
extern int i_write();
extern int i_rewrite();
extern int i_delete();
extern int i_unlock();
extern int i_info();

#endif /* visn3_H */


/*
**	History:
**	$Log: visn3.h,v $
**	Revision 1.4  1996/10/02 22:12:15  gsl
**	define the acucobol i_xxx() funtions
**	
**	Revision 1.3  1996-09-17 16:34:22-07  gsl
**	drcs update
**
**
**
*/
