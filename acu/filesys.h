/* filesys - header information file system interface */
/*  $Id:$  */

#ifndef	_FILESYS_H

#define	_FILESYS_H

/* Some 'C' compilers cannot take the address of a void function.  For	*/
/* these machines, define "Void" to be "int".  For the normal case, we	*/
/* define "Void" to be "void".  */

#ifndef	Void
#define	Void	void
#endif 	/* Void */

/*  Some 'C' compilers understand prototypes better than others.  If */
/*  'C' compiler understands full prototyping, define PROTOTYPE in the */
/*  cc command line, and you will get full prototype checking.  */

#ifndef	P_
#ifdef	_MSC_VER
#define	PROTOTYPE
#endif	/* _MSC_VER */
#ifdef	PROTOTYPE
#define	P_(s)	s
#else 	/* PROTOTYPE */
#define	P_(s)	()
#endif 	/* PROTOTYPE */
#endif 	/* P_ */

/* Allow "region locks" (ie, record locks that are applied to the 	*/
/* entire record) by setting WANT_REGION_LOCKS to "1".  Region locks	*/
/* improve performance in cases where very many locks are applied in a	*/
/* file at once (because adjacent record locks will collapse into a 	*/
/* single lock region in the Unix lock manager).  */

#define	WANT_REGION_LOCKS	0

#if 	WANT_REGION_LOCKS
#ifdef	IS_UNIX
#ifndef	O_AMOS
#ifndef	ENFORCED_LOCKS
#define	REGION_LOCKS_OK
#endif
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

typedef	struct _i_dispatch {
	int		(*p_init) P_((int, int *));
	Void		(*p_exit) P_((void));
	int		(*p_make) P_((char *, char *, char *, char *,
				      char *, char *));
	IFilePtr	(*p_open) P_((char *, int, char *));
	int		(*p_close) P_((IFilePtr));
	unsigned	(*p_read) P_((IFilePtr, char *, int));
	unsigned	(*p_next) P_((IFilePtr, char *));
	unsigned	(*p_previous) P_((IFilePtr, char *));
	int		(*p_start) P_((IFilePtr, char *, int, int, int));
	int		(*p_write) P_((IFilePtr, char *, unsigned));
	int		(*p_rewrite) P_((IFilePtr, char *, unsigned));
	int		(*p_delete) P_((IFilePtr, char *));
	int		(*p_unlock) P_((IFilePtr));
	int		(*p_info) P_((IFilePtr, int, char *));
	Void		(*p_sync) P_((int));
	int		(*p_remove) P_((char *));
	int		(*p_rename) P_((char *, char *));
	int		(*p_version) P_((char *, char *, char *, int, long *));
	Void		(*p_abilities) P_((I_ABILITIES *));
	int		(*p_execute) P_((char *, char *));
	int		(*p_copy) P_((char *, char *));
	int		(*p_begin) P_((void));
	int		(*p_commit) P_((int));
	int		(*p_rollback) P_((int));
	int		(*p_recover) P_((void));
} I_DISPATCH;


#else 	/* INTERNAL_USE */

struct _AcuFile_ {
	long	notused;
};

typedef	struct _AcuFile_	*IFilePtr;
typedef	struct _AcuFile_	*RFilePtr;
typedef	struct _AcuFile_	*SFilePtr;

#endif 	/* INTERNAL_USE */

#ifndef	NEAR
#ifdef	_MSC_VER
#define	NEAR		_near
#else 	/* _MSC_VER */
#define	NEAR
#endif 	/* _MSC_VER */
#endif 	/* NEAR */

/*  Interface version - Current isam.c style is 2  */
#define	CURRENT_INTERFACE_VERSION	2
#define	TRANSACTION_VERSION		1
#define	SIXTEEN_SEGMENTS_VERSION	2
#define	VERS_WITH_EXPDATE_VERSION	2	/*  checked in isam.c  */

#define	MAX_KEYS		120
#define	MAX_KEY_SIZE		250
#define	V3_MAX_SEGS		6
#define	MAX_SEGS		16
#define	DFLT_MAX_FILES		32
#define	DFLT_LOCKS_PER_FILE	10
#define	DFLT_COMP_FACTOR	70


/* Define size of a key description returned by "i_info" */

#define	V3_KEY_DESC_SIZE	3
#define	KEY_DESC_SIZE		4
#define	SEG_DESC_SIZE		10
#define	WHOLE_KEY_DESC_SIZE	(KEY_DESC_SIZE + MAX_SEGS * SEG_DESC_SIZE)
#define	V3_WHOLE_KEY_DESC_SIZE	(V3_KEY_DESC_SIZE + V3_MAX_SEGS * SEG_DESC_SIZE)
#define	v3_key_desc_size(kd)	(((kd)[0]-'0') * SEG_DESC_SIZE + \
					V3_KEY_DESC_SIZE)
#define	key_desc_size(kd)	((((kd)[0]-'0') * 10 + ((kd)[1]-'0')) * \
					SEG_DESC_SIZE + KEY_DESC_SIZE)


/* Context values for i_begin(), i_commit() and i_rollback() functions */

#define	COMMIT_CONTEXT_PROGRAMMED	0	/* COMMIT/ROLLBACK verb */
#define	COMMIT_CONTEXT_UNKNOWN		1	/* various implied operations */
#define	COMMIT_CONTEXT_NO_TRX		2	/* verb coded, no open trx */

/* structure for i_info(-8) */

typedef struct _i_info_seg_name {
	char name[128];
	char size[11];	/* 2^31 + '\0' */
	short type;
	unsigned short seg;
} I_INFO_SEG_NAME;


#define	Finput	       		0   		/* open modes */
#define Foutput			1		
#define Fio			2
#define	Fextend       		3
#define	Fopen_mask		3
#define	Fmulti_lock		0x10
#define	Fread_lock		0x100
#define	Fwrite_lock		0x200
#define	Fbuffered		0x400
#define	Fmass_update		0x600
#define	Flock_mask		0x700
#define	Fis_device		0x800		/* internal use only */
#define	Fopt_lock		0x1000		/* internal use only */
#define	Fappend			0x2000		/* internal use only */
#define	Ftrans			0x4000

#define	F_EQUALS		0		/* start modes */
#define	F_NOT_LESS		1
#define	F_GREATER		2
#define	F_LESS			3
#define	F_NOT_GREATER		4

#define	S_FIXED			-1		/* sequential file types */
#define	S_VAR_COUNT		-2
#define	S_LINE			-3
#define	S_PRINT			-4
#define	is_line(x)		(x <= S_LINE)

			/* Error codes */

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

/* Transaction Errors (in f_log_errno) */

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

extern	char		* NEAR f_lockdir;
extern	char		* NEAR f_log_file;
extern	short		NEAR f_log_buffer_size;
extern	short		NEAR f_logging;
extern	short		NEAR f_log_encrypt;
extern	short		NEAR f_log_device;
extern	char		* NEAR f_logdir;
extern	short		NEAR f_log_errno;
extern	short		NEAR f_errno, NEAR f_trace, NEAR f_no_lock;
extern	char		NEAR f_missing_file[];
extern	short		NEAR f_int_errno, NEAR f_int2_errno;
extern	short		NEAR f_ext_errs;
extern	short		NEAR f_locks_per_file, NEAR f_maxfiles;
extern	short		NEAR f_maxlocks, NEAR i_comp_factor;
extern	short		NEAR f_trx_holds_locks;
extern	short		NEAR f_single_tasking, NEAR i_buffers;
extern	short		NEAR f_upgrade_rlock;
extern	char		* NEAR f_errmsg;
extern	char		NEAR f_clntpassword[];
extern	char		NEAR i_in_commit;
extern	short		NEAR i_in_transaction;
extern	short		NEAR i_implied_transaction;
extern	char		* NEAR i_username;
extern	char		* NEAR i_termname;
extern	char		NEAR i_hostname[];
extern	short		NEAR i_ttyline;
extern	short		NEAR client_transaction;
extern	short		NEAR client_rollback;
extern	char		* NEAR client_bimage;
extern	unsigned	NEAR client_bsize;
extern	char		* NEAR client_fullkey;

/* "f_query_retry" is set by the ACUCOBOL-GT runtime to point to an	*/
/* internal function that is used when the Windows version of Vision is	*/
/* locked out of a file due to heavy usage.  This puts up a message box	*/
/* that asks if the user wishes to continue waiting for the file.  */

typedef	int		(*F_QUERYRETRY) P_((char *));

extern	F_QUERYRETRY	NEAR f_query_retry;
					  
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
extern	int	i_init P_((void));
extern	Void	i_exit P_((void));
extern	Void	i_abilities P_((I_ABILITIES *));
extern	int	i_execute P_((char *, char *));
extern	int	i_begin P_((int));
extern	int	i_commit P_((int));
extern	int	i_rollback P_((int));
extern	int	i_recover P_((void));

/* The remainder of this file is for internal use */

#define MULTILOG

typedef	struct {
	int		*dispatcher;
	long		remote;	
	int		fileds;
	char		rollback;
#ifdef	MULTILOG
	int		log;
#endif
	unsigned short	max_rec_size;
} COMMON_FILE;

typedef	COMMON_FILE	*CFilePtr;

extern	long		NEAR r_recno;
extern	short		NEAR r_has_previous;
extern	short		NEAR r_delete_value;

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

#endif 	/* _FILESYS_H */

/* */
