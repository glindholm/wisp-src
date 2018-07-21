/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

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


extern int i_init(void);
extern int i_make(char* name, char* comment, void* p_parms, void* l_parms, void* keys, void* trans);
extern char* i_open(char* name, int mode, void* l_parms);
extern int i_close(void* f);
extern unsigned i_read(void* f, char* record, int keynum);
extern unsigned i_next(void* f, char* record);
extern unsigned i_previous(void* f, char* record);
extern int i_start(void* f, char* record, int keynum, int keysize, int mode);
extern int i_write(void* f, char* record, unsigned size);
extern int i_rewrite(void* f, char* record, unsigned size);
extern int i_delete(void* f, char* record);
extern int i_unlock(void* f);
extern int i_info(void* f, int mode, char* result);

#endif /* visn3_H */


/*
**	History:
**	$Log: visn3.h,v $
**	Revision 1.6  2003/06/11 19:22:10  gsl
**	add 4.0.01 updates to fix signal problem with acu
**	
**	Revision 1.5  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.4  1996/10/02 22:12:15  gsl
**	define the acucobol i_xxx() funtions
**	
**	Revision 1.3  1996-09-17 16:34:22-07  gsl
**	drcs update
**
**
**
*/
