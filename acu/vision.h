
/* Copyright notice: Copyright (c) 1985-2000, */
/* an unpublished work by Acucorp, Inc. */




#ifndef	ACU_VISION_VISION_H
#define	ACU_VISION_VISION_H

#ifndef	F_EQUALS
#include <fsi/filesys.h>
#endif	

#ifndef	Byte
#define	Byte		unsigned char
#endif	

#define	V_EQUALS		F_EQUALS
#define	V_NOT_LESS		F_NOT_LESS
#define	V_GREATER		F_GREATER
#define	V_LESS			F_LESS
#define	V_NOT_GREATER		F_NOT_GREATER

#define	V_FIXED			S_FIXED
#define	V_VARIABLE		S_VARIABLE
#define	V_PRINT			S_PRINT

#define V_SYS_ERR		E_SYS_ERR 
#define V_NOT_VISION		9999		
#define V_PARAM_ERR		E_PARAM_ERR
#define V_TOO_MANY_FILES	E_TOO_MANY_FILES
#define V_NOT_OPEN		9999		
#define	V_MODE_CLASH		E_MODE_CLASH
#define V_VERSION_ERR		9999		
#define V_REC_LOCKED		E_REC_LOCKED
#define	V_BROKEN		E_BROKEN
#define	V_DUPLICATE		E_DUPLICATE
#define	V_NOT_FOUND		E_NOT_FOUND
#define V_UNDEF_RECORD		E_UNDEF_RECORD
#define V_ILL_KEY		9999		
#define V_DISK_FULL		E_DISK_FULL
#define	V_FILE_LOCKED		E_FILE_LOCKED
#define	V_REC_CHANGED		E_REC_CHANGED
#define	V_MISMATCH		E_MISMATCH
#define	V_NO_MEMORY		E_NO_MEMORY
#define	V_ILL_RECSIZE		9999		
#define	V_MISSING_FILE		E_MISSING_FILE
#define	V_PERMISSION		E_PERMISSION
#define	V_NO_PREVIOUS		E_NO_SUPPORT
#define	V_NO_LOCKS		E_NO_LOCKS

extern	char			*v_errlist[];
extern	short			v_vers_snapshot;
extern	short			v_opn_strict;
extern	short			v_opn_no_index;
extern	char			v_mark_read_corrupt;






typedef	char	*(*VPrintMsgProc) P_((int, int, char **));
extern	VPrintMsgProc		v_msg_proc;
#define	V_MSG_OtherSource	0
#define	V_MSG_LockingTimeout	1
#define	V_MSG_HeaderAccess	2
#define	V_MSG_ReadsWaited	3


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
#define	v_write(f,rec)		v4_write(f,rec,0)

#if	USE_THREADS



extern short* __cdecl  _f_errno(void);
#define v_errno				(*_f_errno())

#else	
#define	v_errno				f_errno
#endif	

#define	v_no_lock			f_no_lock
#define	v_missing_file		f_missing_file
#define	v_single_tasking	f_single_tasking

typedef	int		(*ANetGetHostnameProc) P_((char *, int));
typedef	void		(*ANetVExitProc) P_((void));
typedef	int		(*ANetVMakeProc) P_((char *, char *, char *, char *, char *, char *));
typedef	IFilePtr	(*ANetVOpenProc) P_((char *, int, char *));
typedef	int		(*ANetVCloseProc) P_((IFilePtr));
typedef	unsigned	(*ANetVReadProc) P_((IFilePtr, char *, int));
typedef	unsigned	(*ANetVNextProc) P_((IFilePtr, char *));
typedef	unsigned	(*ANetVPreviousProc) P_((IFilePtr, char *));
typedef	int		(*ANetVStartProc) P_((IFilePtr, char *, int, int, int));
typedef	int		(*ANetVWriteProc) P_((IFilePtr, char *, unsigned, short, short));
typedef	int		(*ANetVDeleteProc) P_((IFilePtr, char *, short, short, char *,
					       unsigned *, char *));
typedef	int		(*ANetVRewriteProc) P_((IFilePtr, char *, unsigned, short,
						short, char *, unsigned *));
typedef	int		(*ANetVUnlockProc) P_((IFilePtr));
typedef	int		(*ANetVInfoProc) P_((IFilePtr, int, char *));
typedef	Void		(*ANetVSyncProc) P_((int));
typedef	int		(*ANetVRemoveProc) P_((char *));
typedef	int		(*ANetVRenameProc) P_((char *, char *));
typedef	int		(*ANetVCopyProc) P_((char *, char *));
typedef	int		(*ANetRInitProc) P_((void));
typedef	void		(*ANetRExitProc) P_((void));
typedef	int		(*ANetRMakeProc) P_((char *, char *));
typedef	RFilePtr	(*ANetROpenProc) P_((char *, int, unsigned, unsigned));
typedef	int		(*ANetRUnlockProc) P_((RFilePtr));
typedef	int		(*ANetRCloseProc) P_((RFilePtr));
typedef	unsigned	(*ANetRReadProc) P_((RFilePtr, char *, long));
typedef	unsigned	(*ANetRNextProc) P_((RFilePtr, char *));
typedef	unsigned	(*ANetRPreviosProc) P_((RFilePtr, char *));
typedef	int		(*ANetRStartProc) P_((RFilePtr, long, int));
typedef	int		(*ANetRWriteProc) P_((RFilePtr, char *, unsigned, long,
					      short, short));
typedef	int		(*ANetRDeleteProc) P_((RFilePtr, long, short, short, char *));
typedef	int		(*ANetRRewriteProc) P_((RFilePtr, char *, unsigned, long,
						short, short, char *));
typedef	int		(*ANetRRemoveProc) P_((char *));
typedef	int		(*ANetRRenameProc) P_((char *, char *));
typedef	int		(*ANetRCopyProc) P_((char *, char *));
typedef	int		(*ANetRInfoProc) P_((RFilePtr, int, char *));
typedef	int		(*ANetSExitProc) P_((SFilePtr, int));
typedef	int		(*ANetSMakeProc) P_((char *, char *));
typedef	SFilePtr	(*ANetSOpenProc) P_((char *, int, unsigned, int, unsigned,
					     int, char *, int));
typedef	int		(*ANetSCloseProc) P_((SFilePtr, int));
typedef	int		(*ANetSReadProc) P_((SFilePtr, char *));
typedef	int		(*ANetSWriteProc) P_((SFilePtr, char *, unsigned, int));
typedef	int		(*ANetSRewriteProc) P_((SFilePtr, char *, unsigned));
typedef	int		(*ANetSRemoveProc) P_((char *));
typedef	int		(*ANetSRenameProc) P_((char *, char *));
typedef	int		(*ANetSCopyProc) P_((char *, char *));
typedef	int		(*ANetPingProc) P_((char *, long *, char *, int));

typedef	struct		remote_file_functions {
	unsigned		inited:1;	
	unsigned		acunet:1;	
	ANetGetHostnameProc	gethostname;
	ANetVExitProc		rv_exit;
	ANetVMakeProc		rv_make;
	ANetVOpenProc		rv_open;
	ANetVCloseProc		rv_close;
	ANetVReadProc		rv_read;
	ANetVNextProc		rv_next;
	ANetVPreviousProc	rv_previous;
	ANetVStartProc		rv_start;
	ANetVWriteProc		rv_write;
	ANetVDeleteProc		rv_delete;
	ANetVRewriteProc	rv_rewrite;
	ANetVUnlockProc		rv_unlock;
	ANetVInfoProc		rv_info;
	ANetVSyncProc		rv_sync;
	ANetVRemoveProc		rv_remove;
	ANetVRenameProc		rv_rename;
	ANetVCopyProc		rv_copy;
	ANetRInitProc		r_init;
	ANetRExitProc		r_exit;
	ANetRMakeProc		r_make;
	ANetROpenProc		r_open;
	ANetRUnlockProc		r_unlock;
	ANetRCloseProc		r_close;
	ANetRReadProc		r_read;
	ANetRNextProc		r_next;
	ANetRPreviosProc	r_previous;
	ANetRStartProc		r_start;
	ANetRWriteProc		r_write;
	ANetRDeleteProc		r_delete;
	ANetRRewriteProc	r_rewrite;
	ANetRRemoveProc		r_remove;
	ANetRRenameProc		r_rename;
	ANetRCopyProc		r_copy;
	ANetRInfoProc		r_info;
	ANetSExitProc		s_exit;
	ANetSMakeProc		s_make;
	ANetSOpenProc		s_open;
	ANetSCloseProc		s_close;
	ANetSReadProc		s_read;
	ANetSWriteProc		s_write;
	ANetSRewriteProc	s_rewrite;
	ANetSRemoveProc		s_remove;
	ANetSRenameProc		s_rename;
	ANetSCopyProc		s_copy;
	ANetPingProc		st_ping;
} COBOLFileOperations;

extern	COBOLFileOperations	cbl_fileops;

extern	COBOLFileOperations	*GetVisionFileOpsPtr P_((void));
extern	void			VRegisterRemoteFunctions P_((COBOLFileOperations *));
extern	void			VDeregisterRemoteFunctions P_((COBOLFileOperations *));
extern	int			IsVisionAcunetEnabled P_((void));
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
extern	void		v4_init_cfg P_((void));
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
extern	int		Afileinfo P_((char *, Byte *));
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

#if	BULK_ADD



typedef	struct tag_progress_struct {
	IFilePtr	f;		
	int		curKey;		
	int		numKeys;	
	long		curRec;		
	long		numRecs;	
} V_PROGRESSSTRUCT;

typedef	void		(*V_PROGRESSFUNC) P_((V_PROGRESSSTRUCT *progress));

extern	V_PROGRESSFUNC	v4_register_progress P_((V_PROGRESSFUNC pFunc));



typedef	struct tag_duplicate_struct {
	IFilePtr	f;		
	char		*record;	
	unsigned	size;		
} V_DUPLICATESTRUCT;

typedef	void		(*V_DUPLICATEFUNC) P_((V_DUPLICATESTRUCT *dup));

extern	V_DUPLICATEFUNC	v4_register_duplicate P_((V_DUPLICATEFUNC pFunc));

#endif	

#endif	


