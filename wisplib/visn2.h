/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/* vision2 - header information for version 2 vision */

#ifdef	O_VMS
#include <rms.h>
#endif


#define	MAGIC			0x10121416
#define	LOG_NAME_SIZE		12
#define	MAX_KEYS		15

#define	Finput	       		0   		/* open modes */
#define Foutput			1		
#define Fio			2
#define	Fextend       		3
#define	Fopen_mask		3
#define	Fread_lock		0x100
#define	Fwrite_lock		0x200
#define	Fbuffered		0x400
#define	Fmass_update		0x600
#define	Flock_mask		0x700
#define	Fis_device		0x800		/* internal use only */

#define	V_EQUALS		0		/* start modes */
#define	V_NOT_LESS		1
#define	V_GREATER		2
#define	V_LESS			3
#define	V_NOT_GREATER		4

#define	V_FIXED			'F'
#define	V_VARIABLE		'V'
#define	V_PRINT			'P'


			/* VAX/VMS interface to RMS */

#ifdef	O_VMS
          
extern	char		*v_name_default;

typedef	struct FAB	Fab;
typedef struct RAB	Rab;
typedef	struct XABSUM	Xabsum;
typedef	struct XABKEY	Xabkey;

typedef struct _v_file {
	Fab		fab;
	Rab		rab;
	int		current_key;
	int		keys;
	int		open_mode;
	int		variable;
	unsigned	rec_size;
} V_FILE;

#define	R_FILE		V_FILE
#define	S_FILE		V_FILE

#else
			/* Standard Vision Definitions */

#ifndef	VISION_INTERNALS

typedef	struct _v_file {
	int		dummy;
} V_FILE;

#endif

typedef	struct _r_file {
	int		fileds;
	long		currec, filepos;
	long		lockaddr;
	int		open_mode;
	unsigned	rec_size;
	char		use_on_next;
} R_FILE;

typedef	struct _s_file {
	int		fileds;
	int		open_mode;
	unsigned	rec_size, block_size, max_block_size;
	int		variable;
	char		*block, *blkptr;
	FILE		*pfile;
	long		currec;
	long		nxtrec, nxtblock;
	unsigned	last_recsize;
	short		is_device;
} S_FILE;

#endif

			/* Error codes */

#define V_SYS_ERR		1 
#define V_NOT_VISION		2
#define V_PARAM_ERR		3
#define V_TOO_MANY_FILES	4
#define V_NOT_OPEN		5
#define	V_MODE_CLASH		6
#define V_VERSION_ERR		7
#define V_REC_LOCKED		8
#define	V_BROKEN		9
#define	V_DUPLICATE		10
#define	V_NOT_FOUND		11
#define V_UNDEF_RECORD		12
#define V_ILL_KEY		13
#define V_DISK_FULL		14
#define	V_FILE_LOCKED		15
#define	V_REC_CHANGED		16
#define	V_MISMATCH		17
#define	V_NO_MEMORY		18
#define	V_ILL_RECSIZE		19
#define	V_MISSING_FILE		20
#define	V_PERMISSION		21
#define	V_NO_PREVIOUS		22

#ifndef	VISION_INTERNALS

#define	v_start(f,rec,key,mode)		v3_start(f,rec,key,0,mode)
#define	v2_start(f,rec,key,mode)	v3_start(f,rec,key,0,mode)
#define	v_open(name,mode)		v2_open(name,mode)
#define	v_close(f)			v2_close(f)
#define	v_read(f,rec,key)		( v2_read(f,rec,key) != 0 )
#define	v_next(f,rec)			( v2_next(f,rec) != 0 )
#define	v_write(f,rec)			v2_write(f,rec,0)
#define	v_delete(f,rec)			v2_delete(f,rec)
#define	v_rewrite(f,rec)		v2_rewrite(f,rec,0)

extern	int		v_errno, v_supl_err, key_trace, v_no_lock;
extern	short		v_single_tasking;
extern	char		*v_errlist[];

extern	V_FILE		*v2_open();
extern	int		v_make(), v2_make(), v2_close();
extern	int		v_error(), v2_write();
extern	unsigned	v2_read(), v2_next(), v2_previous();
extern	int		v2_delete(), v2_rewrite();
extern	int		v3_start();
extern	void		v_info(), v2_info();

extern	R_FILE		*r_open();
extern	long		r_next(), r_write();
extern	int		r_close(), r_unlock(), r_read(), r_start();
extern	int		r_rewrite(), r_delete();
extern	S_FILE		*s_open();
extern	int		s_close(), s_read(), s_write(), s_rewrite();

#endif

/* */


/*
**	History:
**	$Log: visn2.h,v $
**	Revision 1.8  1996/08/19 22:33:04  gsl
**	drcs update
**	
**
**
*/
