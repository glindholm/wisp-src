
/* Copyright notice: Copyright (c) 1985-2000, */
/* an unpublished work by Acucorp, Inc. */




#ifndef	ACU_FSI_FILESYS_H
#define	ACU_FSI_FILESYS_H

#include <stdlib/machine.h>





#ifndef	Void
#define	Void	void
#endif	





#ifndef	P_
#ifdef	_MSC_VER
#define	PROTOTYPE
#endif	
#ifdef	PROTOTYPE
#define	P_(s)	s
#else	
#define	P_(s)	()
#endif	
#endif	







#define	WANT_REGION_LOCKS	0

#if	WANT_REGION_LOCKS
#ifdef	IS_UNIX
#ifndef	ENFORCED_LOCKS
#define	REGION_LOCKS_OK
#endif	
#endif	
#endif	

typedef	struct _i_abilities {
	unsigned	has_previous : 1;
	unsigned	alternate_collate : 1;
} I_ABILITIES;

#ifdef	INTERNAL_USE

typedef	struct _i_file		*IFilePtr;
typedef	struct _r_file		*RFilePtr;
typedef	struct _s_file		*SFilePtr;


typedef	int		(*GenericProc)();

typedef	int		(*FSIInitProc) P_((int, int *));
typedef	Void		(*FSIExitProc) P_((void));
typedef	int		(*FSIMakeProc) P_((char *, char *, char *, char *, char *, char *));
typedef	IFilePtr	(*FSIOpenProc) P_((char *, int, char *));
typedef	int		(*FSICloseProc) P_((IFilePtr));
typedef	unsigned	(*FSIReadProc) P_((IFilePtr, char *, int));
typedef	unsigned	(*FSINextProc) P_((IFilePtr, char *));
typedef	unsigned	(*FSIPreviousProc) P_((IFilePtr, char *));
typedef	int		(*FSIStartProc) P_((IFilePtr, char *, int, int, int));
typedef	int		(*FSIWriteProc) P_((IFilePtr, char *, unsigned));
typedef	int		(*FSIRewriteProc) P_((IFilePtr, char *, unsigned));
typedef	int		(*FSIDeleteProc) P_((IFilePtr, char *));
typedef	int		(*FSIUnlockProc) P_((IFilePtr));
typedef	int		(*FSIInfoProc) P_((IFilePtr, int, char *));
typedef	Void		(*FSISyncProc) P_((int));
typedef	int		(*FSIRemoveProc) P_((char *));
typedef	int		(*FSIRenameProc) P_((char *, char *));
typedef	int		(*FSIVersionProc) P_((char *, char *, char *, int, long *));
typedef	Void		(*FSIAbilitiesProc) P_((I_ABILITIES *));
typedef	int		(*FSIExecuteProc) P_((char *, char *));
typedef	int		(*FSICopyProc) P_((char *, char *));
typedef	int		(*FSIBeginProc) P_((void));
typedef	int		(*FSICommitProc) P_((int));
typedef	int		(*FSIRollbackProc) P_((int));
typedef	int		(*FSIRecoverProc) P_((void));
typedef	GenericProc	(*FSIRegisterProc) P_((char *, GenericProc));
typedef	void		(*FSIInitConfigProc) P_((void));

typedef	struct _i_dispatch {
	FSIInitProc		p_init;
	FSIExitProc		p_exit;
	FSIMakeProc		p_make;
	FSIOpenProc		p_open;
	FSICloseProc		p_close;
	FSIReadProc		p_read;
	FSINextProc		p_next;
	FSIPreviousProc		p_previous;
	FSIStartProc		p_start;
	FSIWriteProc		p_write;
	FSIRewriteProc		p_rewrite;
	FSIDeleteProc		p_delete;
	FSIUnlockProc		p_unlock;
	FSIInfoProc		p_info;
	FSISyncProc		p_sync;
	FSIRemoveProc		p_remove;
	FSIRenameProc		p_rename;
	FSIVersionProc		p_version;
	FSIAbilitiesProc	p_abilities;
	FSIExecuteProc		p_execute;
	FSICopyProc		p_copy;
	FSIBeginProc		p_begin;
	FSICommitProc		p_commit;
	FSIRollbackProc		p_rollback;
	FSIRecoverProc		p_recover;
	FSIRegisterProc		p_register;
	FSIInitConfigProc	p_init_cfg;
} I_DISPATCH;

typedef	int		(*FSIGetDispatchTableProc) P_((I_DISPATCH **));


#else	

struct _AcuFile_ {
	long	notused;
};

typedef	struct _AcuFile_	*IFilePtr;
typedef	struct _AcuFile_	*RFilePtr;
typedef	struct _AcuFile_	*SFilePtr;

#endif	


#define	CURRENT_INTERFACE_VERSION	2
#define	TRANSACTION_VERSION		1
#define	SIXTEEN_SEGMENTS_VERSION	2
#define	VERS_WITH_EXPDATE_VERSION	2	

#define	MAX_KEYS		120
#define	MAX_KEY_SIZE		250
#define	V3_MAX_SEGS		6
#define	MAX_SEGS		16
#define	DFLT_MAX_FILES		32
#define	DFLT_LOCKS_PER_FILE	10
#define	DFLT_COMP_FACTOR	70
#define	DFLT_PLUGIN_MAX_FILES		255
#define	DFLT_PLUGIN_LOCKS_PER_FILE	256
#define	DFLT_PLUGIN_MAX_LOCKS		512


#define CLIENT_SIZE	20
#define USER_SIZE	16
#define UMASK_SIZE	3
#define LOCAL_USER_SIZE	16
#define PASSWORD_SIZE	12
#define FILLER_SIZE	61



#define	V3_KEY_DESC_SIZE	3
#define	KEY_DESC_SIZE		4
#define	SEG_DESC_SIZE		10
#define	WHOLE_KEY_DESC_SIZE	(KEY_DESC_SIZE + MAX_SEGS * SEG_DESC_SIZE)
#define	V3_WHOLE_KEY_DESC_SIZE	(V3_KEY_DESC_SIZE + V3_MAX_SEGS * SEG_DESC_SIZE)
#define	v3_key_desc_size(kd)	(((kd)[0]-'0') * SEG_DESC_SIZE + \
					V3_KEY_DESC_SIZE)
#define	key_desc_size(kd)	((((kd)[0]-'0') * 10 + ((kd)[1]-'0')) * \
					SEG_DESC_SIZE + KEY_DESC_SIZE)




#define	COMMIT_CONTEXT_PROGRAMMED	0	
#define	COMMIT_CONTEXT_UNKNOWN		1	
#define	COMMIT_CONTEXT_NO_TRX		2	



typedef struct _i_info_seg_name {
	char name[128];
	char size[11];	
	short type;	
	unsigned short seg;
} I_INFO_SEG_NAME;

#define	V4_DATA		255
#define	V4_INDEX	254



#define	FA_NONE		0
#define	FA_MASS_UPDATE	(1<<0)
#define	FA_REMOTE	(1<<1)
#define	FA_ALL		-1

#define	Finput	       		0   		
#define Foutput			1		
#define Fio			2
#define	Fextend       		3
#define	Fopen_mask		3
#define	Ftext			4		
#define	Fbinary			0		
#define	Flocal_only		8
#define	Fmulti_lock		0x0010
#define	Fmulti_records		0x0020
#define	Fread_lock		0x0100
#define	Fwrite_lock		0x0200
#define	Fbuffered		0x0400
#define	Fmass_update		0x0600
#define	Flock_mask		0x0700
#define	Fis_device		0x0800		
#define	Fopt_lock		0x1000		
#define	Fappend			0x2000		
#define	Ftrans			0x4000
#if	BULK_ADD
#define	Fbulk_add		0x8600
#endif	

#define	F_EQUALS		0		
#define	F_NOT_LESS		1
#define	F_GREATER		2
#define	F_LESS			3
#define	F_NOT_GREATER		4

#define	S_FIXED			-1		
#define	S_VAR_COUNT		-2
#define	S_LINE			-3
#define	S_PRINT			-4
#define	is_line(x)		(x <= S_LINE)

			

#define E_SYS_ERR		1 
#define E_PARAM_ERR		2
#define E_TOO_MANY_FILES	3
#define	E_MODE_CLASH		4
#define E_REC_LOCKED		5
#define	E_BROKEN		6
#define	E_DUPLICATE		7
#define	E_NOT_FOUND		8
#define E_UNDEF_RECORD		9
#define E_DISK_FULL		10
#define	E_FILE_LOCKED		11
#define	E_REC_CHANGED		12
#define	E_MISMATCH		13
#define	E_NO_MEMORY		14
#define	E_MISSING_FILE		15
#define	E_PERMISSION		16
#define	E_NO_SUPPORT		17
#define	E_NO_LOCKS		18
#define	E_INTERFACE		19
#define	E_LICENSE_ERR		20
#define	E_UNKNOWN_ERR		21
#define	E_TRANSACTION		22
#define	W_NO_SUPPORT		100
#define	W_DUP_OK		101



#define E_LOG_EXTERNAL		1
#define E_LOG_TOO_MANY 		2
#define E_LOG_MISSING 		3
#define E_LOG_PERMISSION 	4
#define E_LOG_SYS_ERR 		5
#define E_LOG_CORRUPT 		6
#define E_LOG_LOCKED 		7
#define E_LOG_NO_MEMORY 	8
#define E_LOG_DISK_FULL 	9
#define	E_NO_LOG 		10
#define E_RB_LOG_CORRUPT	11
#define	E_LOG_INCOMPLETE	12
#define	E_OPEN_NOT_LOGGED	13
#define E_LOG_INTERFACE		14
#define	E_LOG_REMOTE		15
#define E_LOG_NESTED_START	16
#define E_LOG_TEMP		17
#define W_LOG_NO_SUPPORT	99

extern	char		* f_log_file;
extern	short		f_logging;
extern	short		f_log_encrypt;
extern	short		f_log_device;
extern	char		* f_logdir;

#if	USE_THREADS
extern short* __cdecl  _f_errno(void);
#define f_errno        (*_f_errno())
extern short* __cdecl  _f_log_errno(void);
#define f_log_errno    (*_f_log_errno())


#else	
extern  short       f_errno;
extern	short		f_log_errno;
#endif	

extern	short		f_no_lock;
extern	short		f_trace;
extern	char		f_missing_file[];
extern	long		f_int_errno, f_int2_errno;
extern	short		f_ext_errs;
extern	short		f_locks_per_file, f_maxfiles;
extern	short		f_maxlocks, i_comp_factor;
extern	short		f_single_tasking;
extern	long		i_buffers;
extern	short		f_upgrade_rlock;
extern	char		* f_errmsg;
extern	char		f_clntpassword[];
extern	char		i_in_commit;
extern	short		i_in_transaction;
extern	short		i_implied_transaction;
extern	char		* i_username;
extern	char		* i_termname;
extern	char		i_hostname[];
extern	short		i_ttyline;
extern	short		client_transaction;
extern	short		client_rollback;
extern	char		* client_bimage;
extern	unsigned	client_bsize;
extern	char		* client_fullkey;






typedef	int		(*F_QUERYRETRY) P_((char *));

extern	F_QUERYRETRY	f_query_retry;
					  
extern	int	i_make P_((char *, char *, char *, char *, char *, char *));
extern IFilePtr	i_open P_((char *, int, char *));
extern	int	i_close P_((IFilePtr));
extern unsigned	i_read P_((IFilePtr, char *, int));
extern unsigned	i_next P_((IFilePtr, char *));
extern unsigned	i_previous P_((IFilePtr, char *));
extern	int	i_write P_((IFilePtr, char *, unsigned));
extern	int	i_rewrite P_((IFilePtr, char *, unsigned));
extern	int	i_delete P_((IFilePtr, char *));
extern	int	i_start P_((IFilePtr, char *, int, int, int));
extern	int	i_info P_((IFilePtr, int, char *));
extern	int	i_version P_((char *, char *, char *, int, long *));
extern	int	i_remove P_((char *));
extern	Void	i_sync P_((int));
extern	int	i_rename P_((char *, char *));
extern	int	i_copy P_((char *, char *));
extern	int	i_unlock P_((IFilePtr));
extern	void	i_init_cfg P_((void));
extern	int	i_init P_((void));
extern	Void	i_exit P_((void));
extern	Void	i_abilities P_((I_ABILITIES *));
extern	int	i_execute P_((char *, char *));
extern	int	i_begin P_((int));
extern	int	i_commit P_((int));
extern	int	i_rollback P_((int));
extern	int	i_recover P_((void));



#define MULTILOG

typedef	struct {
	int		*dispatcher;
	long		remote;	
	int		fileds;
	char		rollback;
	char		var_size;
#ifdef	MULTILOG
	int		log;
#endif	
	unsigned short	min_rec_size;
	unsigned short	max_rec_size;
	short		o_mode;
} COMMON_FILE;

typedef	COMMON_FILE	*CFilePtr;

extern	long		r_recno;
extern	short		r_has_previous;

extern	int	f_exists P_((char *));
extern	int	f_rename P_((char *, char *));

extern	int	Ar_init P_((void));
extern	void	Ar_exit P_((void));
extern	int	Ar_make P_((char *, char *, long));
extern RFilePtr	Ar_open P_((char *, int, unsigned, unsigned, long));
extern	int	Ar_unlock P_((RFilePtr));
extern	int	Ar_close P_((RFilePtr));
extern unsigned	Ar_read P_((RFilePtr, char *, long));
extern unsigned	Ar_next P_((RFilePtr, char *));
extern unsigned	Ar_previous P_((RFilePtr, char *));
extern	int	Ar_start P_((RFilePtr, long, int));
extern	int	Ar_write P_((RFilePtr, char *, unsigned, long));
extern	int	Ar_delete P_((RFilePtr, long));
extern	int	Ar_rewrite P_((RFilePtr, char *, unsigned, long));
extern	int	Ar_remove P_((char *));
extern	int	Ar_rename P_((char *, char *));
extern	int	Ar_copy P_((char *, char *));
extern	int	Ar_info P_((RFilePtr, int, char *));
extern	int	As_exit P_((SFilePtr, int));
extern	int	As_make P_((char *, char *));
extern SFilePtr	As_open P_((char *, int, unsigned, int, unsigned,
			     int, char *, int));
extern	int	As_close P_((SFilePtr, int));
extern	int	As_read P_((SFilePtr, char *));
extern	int	As_write P_((SFilePtr, char *, unsigned, int));
extern	int	As_rewrite P_((SFilePtr, char *, unsigned));
extern	int	As_remove P_((char *));
extern	int	As_rename P_((char *, char *));
extern	int	As_copy P_((char *, char *));
extern	LargeFileOff_t	As_seek P_((SFilePtr, LargeFileOff_t, int));

#endif	


