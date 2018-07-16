/* vision.h - simulate old vision v2 interface using replacable file system */
/*  $Id:$  */

#ifndef	VISION_H
#define	VISION_H

#ifndef	F_EQUALS
#include <fsi/filesys.h>
#endif 	/* F_EQUALS */

#define	V_EQUALS		F_EQUALS
#define	V_NOT_LESS		F_NOT_LESS
#define	V_GREATER		F_GREATER
#define	V_LESS			F_LESS
#define	V_NOT_GREATER		F_NOT_GREATER

#define	V_FIXED			S_FIXED
#define	V_VARIABLE		S_VARIABLE
#define	V_PRINT			S_PRINT

#define V_SYS_ERR		E_SYS_ERR 
#define V_NOT_VISION		9999		/* not used! */
#define V_PARAM_ERR		E_PARAM_ERR
#define V_TOO_MANY_FILES	E_TOO_MANY_FILES
#define V_NOT_OPEN		9999		/* not used! */
#define	V_MODE_CLASH		E_MODE_CLASH
#define V_VERSION_ERR		9999		/* not used! */
#define V_REC_LOCKED		E_REC_LOCKED
#define	V_BROKEN		E_BROKEN
#define	V_DUPLICATE		E_DUPLICATE
#define	V_NOT_FOUND		E_NOT_FOUND
#define V_UNDEF_RECORD		E_UNDEF_RECORD
#define V_ILL_KEY		9999		/* not used! */
#define V_DISK_FULL		E_DISK_FULL
#define	V_FILE_LOCKED		E_FILE_LOCKED
#define	V_REC_CHANGED		E_REC_CHANGED
#define	V_MISMATCH		E_MISMATCH
#define	V_NO_MEMORY		E_NO_MEMORY
#define	V_ILL_RECSIZE		9999		/* not used! */
#define	V_MISSING_FILE		E_MISSING_FILE
#define	V_PERMISSION		E_PERMISSION
#define	V_NO_PREVIOUS		E_NO_SUPPORT
#define	V_NO_LOCKS		E_NO_LOCKS

extern	char			* NEAR v_errlist[];
extern	short			NEAR v_make_vers, NEAR v_vers_snapshot;
extern	short			NEAR v_buf_data;
extern	long			NEAR v_seg_size;
extern	short			NEAR v_opn_err_ok;
extern	short			NEAR v_opn_strict;
extern	short			NEAR v_opn_no_index;
#if V_ALT_LOCK
extern	short			NEAR v_lock_method;
#endif 	/* V_ALT_LOCK */

/*  Print message procedure  */
/*  The user of the vision library needs to set v_msg_proc in order to  */
/*  get translated messages.  Otherwise, they get english.  */
/*  Pass 1) V_MSG_Code, 2) number of values, 3) values  */
/*  Return a char * suitable for copying.  */
typedef	char	*(*VPrintMsgProc) P_((int, int, char **));
extern	VPrintMsgProc		NEAR v_msg_proc;
#define	V_MSG_OtherSource	0
#define	V_MSG_LockingTimeout	1
#define	V_MSG_HeaderAccess	2
#define	V_MSG_ReadsWaited	3

/* translate v3_ calls into v4_ calls without change */
#define	v3_close			v4_close
#define	v3_delete			v4_delete
#define	v3_exit				v4_exit
#define	v3_init				v4_init
#define	v3_next				v4_next
#define	v3_open				v4_open
#define	v3_read				v4_read
#define	v3_remove			v4_remove
#define	v3_rename			v4_rename
#define	v3_rewrite			v4_rewrite
#define	v3_start			v4_start
#define	v3_sync				v4_sync
#define	v3_unlock			v4_unlock
#define	v3_version			v4_version
#define	v3_write			v4_write

/* translate v2_ calls into v4_ calls */
#define	v2_close			v4_close
#define	v2_delete			v4_delete
#define	v2_next				v4_next
#define	v2_previous			v4_previous
#define	v2_read				v4_read
#define	v2_rewrite			v4_rewrite
#define	v2_start(f,rec,key,mode)	v4_start(f,rec,key,0,mode)
#define	v2_unlock			v4_unlock
#define	v2_write			v4_write

#define	v_close(f)			v4_close(f)
#define	v_delete(f,rec)			v4_delete(f,rec)
#define	v_exit				v4_exit
#define	v_init				v4_init
#define	v_next(f,rec)			( v4_next(f,rec) != 0 )
#define	v_open(name,mode)		v2_open(name,mode)
#define	v_read(f,rec,key)		( v4_read(f,rec,key) != 0 )
#define	v_remove			v4_remove
#define	v_rename			v4_rename
#define	v_rewrite(f,rec)		v4_rewrite(f,rec,0)
#define	v_start(f,rec,key,mode)		v4_start(f,rec,key,0,mode)
#define	v_sync				v4_sync
#define	v_unlock(f)			v4_unlock(f)
#define	v_version			v4_version
#define	v_write(f,rec)			v4_write(f,rec,0)

#define	v_buffers			i_buffers
#define	v_errno				f_errno
#define	v_missing_file			f_missing_file
#define	v_no_lock			f_no_lock
#define	v_single_tasking		f_single_tasking

extern	Void		v4_abilities P_((I_ABILITIES *));
extern	int		v4_begin P_((void));
extern	int		v4_close P_((IFilePtr));
extern	int		v4_commit P_((int));
extern	int		v4_copy P_((char *, char *));
extern	int		v4_delete P_((IFilePtr, char *));
extern	void		v4_error P_((char *));
extern	int		v4_execute P_((char *, char *));
extern	Void		v4_exit P_((void));

extern	void		v_info P_((IFilePtr, char *, char *));
extern	void		v2_info P_((IFilePtr, char *, char *));
extern	int		v4_info P_((IFilePtr, int, char *));

extern	int		v4_init P_((void));
extern	int		v4_init_vers P_((int, int *));

extern	int		v_make P_((char *, char *, char *));
extern	int		v2_make P_((char *, char *, char *));
extern	int		v4_make P_((char *, char *, char *, char *, char *,
					char *));

extern	unsigned	v4_next P_((IFilePtr, char *));

extern	IFilePtr	v2_open P_((char *, int ));
extern	IFilePtr	v4_open P_((char *, int, char *));

extern	unsigned	v4_previous P_((IFilePtr, char *));
extern	unsigned	v4_read P_((IFilePtr, char *, int));
extern	int		v4_recover P_((void));
extern	int		v4_remove P_((char *));
extern	int		v4_rename P_((char *, char *));
extern	int		v4_rewrite P_((IFilePtr, char *, unsigned));
extern	int		v4_rollback P_((int));
extern	int		v4_start P_((IFilePtr, char *, int, int, int));
extern	Void		v4_sync P_((int));
extern	int		v4_unlock P_((IFilePtr));
extern	int		v4_force_unlock P_((IFilePtr));
extern	int		v4_version P_((char *, char *, char *, int, long *));
extern	int		v4_write P_((IFilePtr, char *, unsigned));

#endif	/* VISION_H  */

/* */
