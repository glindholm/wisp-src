/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
#ifndef __QUIDEF_LOADED
#define __QUIDEF_LOADED	1

/*** MODULE $quidef ***/
/*                                                                          */
/* Get Queue Information Service ($GETQUI) definitions.                     */
/*                                                                          */
/* NOTE:  New items must always be added at the end so users will not have to relink. */
/*                                                                          */
/* NOTE:  Update [VMSLIB.SRC]QUITABLE.MAR to reflect changes in $QUIDEF.    */
/*                                                                          */
/*                                                                          */
/* Function codes                                                           */
/*                                                                          */
#define QUI$_CANCEL_OPERATION 1         /* Cancel a wildcard operation      */
#define QUI$_DISPLAY_CHARACTERISTIC 2   /* Return characteristic attributes */
#define QUI$_DISPLAY_FILE 3             /* Return file attributes           */
#define QUI$_DISPLAY_FORM 4             /* Return form attributes           */
#define QUI$_DISPLAY_JOB 5              /* Return job attributes            */
#define QUI$_DISPLAY_QUEUE 6            /* Return queue attributes          */
#define QUI$_TRANSLATE_QUEUE 7          /* Validate and translate queue name */
#define QUI$_DISPLAY_ENTRY 8            /* Return entry (job) attributes (without first having to establish queue context) */
#define QUI$_RESERVED_FUNC_9 9          /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_FUNC_10 10        /* Reserved for Digital use (name may change) */
/*                                                                          */
/* Item codes                                                               */
/*                                                                          */
#define QUI$_ACCOUNT_NAME 1             /* Job: Submitter's account name    */
#define QUI$_AFTER_TIME 2               /* Job: /AFTER=time                 */
#define QUI$_ASSIGNED_QUEUE_NAME 3      /* Queue: ASSIGN/QUEUE target       */
#define QUI$_BASE_PRIORITY 4            /* Queue: /BASE_PRIORITY=n          */
#define QUI$_CHARACTERISTIC_NAME 5      /* Characteristic: Name             */
#define QUI$_CHARACTERISTIC_NUMBER 6    /* Characteristic: Number           */
#define QUI$_CHARACTERISTICS 7          /* Job, queue: /CHARACTERISTICS=(c,...) */
#define QUI$_CHECKPOINT_DATA 8          /* Job: Checkpoint data             */
#define QUI$_CLI 9                      /* Job: /CLI=filename               */
#define QUI$_COMPLETED_BLOCKS 10        /* Job: Completed blocks including checkpoint */
#define QUI$_CONDITION_VECTOR 11        /* Job: Completion status           */
#define QUI$_CPU_DEFAULT 12             /* Queue: /CPUDEFAULT=t             */
#define QUI$_CPU_LIMIT 13               /* Job, queue: /CPUMAXIMUM=t        */
#define QUI$_DEVICE_NAME 14             /* Queue: /ON=device                */
#define QUI$_ENTRY_NUMBER 15            /* Job: Entry number                */
#define QUI$_FILE_COPIES 16             /* File: /COPIES=n                  */
#define QUI$_FILE_COPIES_CHKPT 17       /* File: File copies checkpoint (for internal use only) */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_FILE_COPIES_DONE 18        /* File: File copies completed      */
#define QUI$_FILE_FLAGS 19              /* File: Boolean information        */
#define QUI$_FILE_SETUP_MODULES 20      /* File: /SETUP=(module,...)        */
#define QUI$_FILE_SPECIFICATION 21      /* File: Full file specification    */
#define QUI$_FILE_STATUS 22             /* File: Status information         */
#define QUI$_FIRST_PAGE 23              /* File: /PAGES=(n,"")              */
#define QUI$_FORM_DESCRIPTION 24        /* Form: /DESCRIPTION=string        */
#define QUI$_FORM_FLAGS 25              /* Form: Boolean information        */
#define QUI$_FORM_LENGTH 26             /* Form: /LENGTH=n                  */
#define QUI$_FORM_MARGIN_BOTTOM 27      /* Form: /MARGIN=BOTTOM=n           */
#define QUI$_FORM_MARGIN_LEFT 28        /* Form: /MARGIN=LEFT=n             */
#define QUI$_FORM_MARGIN_RIGHT 29       /* Form: /MARGIN=RIGHT=n            */
#define QUI$_FORM_MARGIN_TOP 30         /* Form: /MARGIN=TOP=n              */
#define QUI$_FORM_NAME 31               /* Form, job, queue: Form name      */
#define QUI$_FORM_NUMBER 32             /* Form: Number                     */
#define QUI$_FORM_SETUP_MODULES 33      /* Form: /SETUP=(module,...)        */
#define QUI$_FORM_STOCK 34              /* Form: /STOCK=stock-name          */
#define QUI$_FORM_WIDTH 35              /* Form: /WIDTH=n                   */
#define QUI$_GENERIC_TARGET 36          /* Queue: /GENERIC=(queue-name,...) */
#define QUI$_INTERVENING_BLOCKS 37      /* Job: Intervening pending blocks  */
#define QUI$_INTERVENING_JOBS 38        /* Job: Intervening pending jobs    */
#define QUI$_JOB_COPIES 39              /* Job: /JOB_COUNT=n                */
#define QUI$_JOB_COPIES_CHKPT 40        /* Job: Job copies checkpoint (for internal use only) */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_JOB_COPIES_DONE 41         /* Job: Job copies completed        */
#define QUI$_JOB_FLAGS 42               /* Job: Boolean information         */
#define QUI$_JOB_LIMIT 43               /* Queue: /JOB_LIMIT=n              */
#define QUI$_JOB_NAME 44                /* Job: Name                        */
#define QUI$_JOB_RESET_MODULES 45       /* Queue: /SEPARATE=RESET=(module,...) */
#define QUI$_JOB_SIZE 46                /* Job: Total blocks in job         */
#define QUI$_JOB_SIZE_MAXIMUM 47        /* Queue: /BLOCK_LIMIT=n            */
#define QUI$_JOB_SIZE_MINIMUM 48        /* Queue: /BLOCK_LIMIT=(n,"")       */
#define QUI$_JOB_STATUS 49              /* Job: Status information          */
#define QUI$_LAST_PAGE 50               /* File: /PAGES=n                   */
#define QUI$_LIBRARY_SPECIFICATION 51   /* Queue: /LIBRARY=file-specification */
#define QUI$_LOG_QUEUE 52               /* Job: /PRINTER=queue-name         */
#define QUI$_LOG_SPECIFICATION 53       /* Job: /LOG_FILE=file-specification */
#define QUI$_NOTE 54                    /* Job: /NOTE=string                */
#define QUI$_OPERATOR_REQUEST 55        /* Job: /OPERATOR=string            */
#define QUI$_OWNER_UIC 56               /* Queue: /OWNER=uic                */
#define QUI$_PAGE_SETUP_MODULES 57      /* Form: /PAGE_SETUP=(module,...)   */
#define QUI$_PARAMETER_1 58             /* Job: /PARAMETER=string           */
#define QUI$_PARAMETER_2 59
#define QUI$_PARAMETER_3 60
#define QUI$_PARAMETER_4 61
#define QUI$_PARAMETER_5 62
#define QUI$_PARAMETER_6 63
#define QUI$_PARAMETER_7 64
#define QUI$_PARAMETER_8 65
#define QUI$_PRIORITY 66                /* Job: /PRIORITY=n                 */
#define QUI$_PROCESSOR 67               /* Queue: /PROCESSOR=filename       */
#define QUI$_PROTECTION 68              /* Queue: /PROTECTION=mask          */
#define QUI$_QUEUE_FLAGS 69             /* Queue: Boolean information       */
#define QUI$_QUEUE_NAME 70              /* Job, queue: Queue name           */
#define QUI$_QUEUE_STATUS 71            /* Queue: Status information        */
#define QUI$_REFUSAL_REASON 72          /* Job: Reason symbiont refused job (for internal use only) */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_REQUEUE_PRIORITY 73        /* Job: Priority after requeue (for internal use only) */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_REQUEUE_QUEUE_NAME 74      /* Job: Queue after requeue         */
#define QUI$_SCSNODE_NAME 75            /* Queue: /ON=node::                */
#define QUI$_SEARCH_FLAGS 76            /* Flags to control search          */
#define QUI$_SEARCH_NAME 77             /* Object name to search for	    */
#define QUI$_SEARCH_NUMBER 78           /* Object number to search for      */
#define QUI$_SUBMISSION_TIME 79         /* Job: Submission time             */
#define QUI$_UIC 80                     /* Job: Submitter's UIC             */
#define QUI$_USERNAME 81                /* Job: Submitter's username        */
#define QUI$_WSDEFAULT 82               /* Job, queue: /WSDEFAULT=n         */
#define QUI$_WSEXTENT 83                /* Job, queue: /WSEXTENT=n          */
#define QUI$_WSQUOTA 84                 /* Job, queue: /WSQUOTA=n           */
#define QUI$_RESERVED_BOOLEAN_85 85     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_86 86     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_INPUT_87 87       /* Reserved for Digital use (name may change) */
#define QUI$_SEARCH_USERNAME 88         /* Username of owner of job to modify search */
#define QUI$_DEFAULT_FORM_NAME 89       /* Default form name on queue       */
#define QUI$_DEFAULT_FORM_NUMBER 90     /* Default form number on queue     */
#define QUI$_DEFAULT_FORM_STOCK 91      /* Stock name for default form      */
#define QUI$_JOB_PID 92                 /* Pid of batch job                 */
#define QUI$_FILE_IDENTIFICATION 93     /* File identification (From RMS NAM block) */
#define QUI$_PENDING_JOB_BLOCK_COUNT 94 /* Total number of blocks for all pending jobs */
#define QUI$_RESERVED_BOOLEAN_95 95     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_96 96     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_97 97     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_98 98     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_99 99     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_100 100   /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_101 101   /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_BOOLEAN_102 102   /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_INPUT_103 103     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_INPUT_104 104     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_INPUT_105 105     /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_INPUT_106 106     /* Reserved for Digital use (name may change) */
#define QUI$_EXECUTING_JOB_COUNT 107    /* Number of jobs that are executing */
#define QUI$_HOLDING_JOB_COUNT 108      /* Number of holding jobs           */
#define QUI$_TIMED_RELEASE_JOB_COUNT 109 /* Number of jobs specified with /AFTER_TIME */
#define QUI$_PENDING_JOB_REASON 110     /* Secondary status describing why job is pending */
#define QUI$_ORB_ADDRESS 111            /* Address of in-memory ORB data structure (for internal use only) */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_QUEUE_DESCRIPTION 112      /* Queue: /DESCRIPTION              */
#define QUI$_SYMBIONT_FLAGS 113         /* Symbiont options (requests) passed to job controller */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_JOB_ACCESS_CLASS 114       /* Job access class information (for internal use only) */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_ORB_LOCK_KEY 115           /* Key value for locking of in-memory ORB data structure (for internal use only) */
/* (note this item code is intentionally not documented and reserved for Digital use) */
#define QUI$_RESERVED_OUTPUT_116 116    /* Reserved for Digital use (name may change) */
#define QUI$_PENDING_JOB_COUNT 117      /* Number of pending jobs           */
#define QUI$_RETAINED_JOB_COUNT 118     /* Number of retained jobs          */
#define QUI$_RESTART_QUEUE_NAME 119     /* Name of queue into which a job would be restarted, if appropriate */
#define QUI$_RESERVED_OUTPUT_120 120    /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_OUTPUT_121 121    /* Reserved for Digital use (name may change) */
#define QUI$_RESERVED_OUTPUT_122 122    /* Reserved for Digital use (name may change) */
/*                                                                          */
/* Subfields of FILE_FLAGS item code.                                       */
/*                                                                          */
#define QUI$M_FILE_BURST 1
#define QUI$M_FILE_BURST_EXP 2
#define QUI$M_FILE_DELETE 4
#define QUI$M_FILE_DOUBLE_SPACE 8
#define QUI$M_FILE_FLAG 16
#define QUI$M_FILE_FLAG_EXP 32
#define QUI$M_FILE_TRAILER 64
#define QUI$M_FILE_TRAILER_EXP 128
#define QUI$M_FILE_PAGE_HEADER 256
#define QUI$M_FILE_PAGINATE 512
#define QUI$M_FILE_PASSALL 1024
#define QUI$M_FILE_PAGINATE_EXP 2048
struct file_flags {
    unsigned qui$v_file_burst : 1;      /* /BURST                           */
    unsigned qui$v_file_burst_exp : 1;  /* /[NO]BURST explicit (intentionally not documented) */
    unsigned qui$v_file_delete : 1;     /* /DELETE                          */
    unsigned qui$v_file_double_space : 1; /* /SPACE                         */
    unsigned qui$v_file_flag : 1;       /* /FLAG                            */
    unsigned qui$v_file_flag_exp : 1;   /* /[NO]FLAG explicit (intentionally not documented) */
    unsigned qui$v_file_trailer : 1;    /* /TRAILER                         */
    unsigned qui$v_file_trailer_exp : 1; /* /[NO]TRAILER explicit (intentionally not documented) */
    unsigned qui$v_file_page_header : 1; /* /HEADER                         */
    unsigned qui$v_file_paginate : 1;   /* /FEED                            */
    unsigned qui$v_file_passall : 1;    /* /PASSALL                         */
    unsigned qui$v_file_paginate_exp : 1; /* /[NO]FEED explicit (intentionally not documented) */
    unsigned qui$v_filler : 20;
    } ;
/*                                                                          */
/* Subfields of FILE_STATUS item code.                                      */
/*                                                                          */
#define QUI$M_FILE_CHECKPOINTED 1
#define QUI$M_FILE_EXECUTING 2
struct file_status {
    unsigned qui$v_file_checkpointed : 1; /* File checkpointed              */
    unsigned qui$v_file_executing : 1;  /* File currently executing         */
    unsigned qui$v_filler : 30;
    } ;
/*                                                                          */
/* Subfields of FORM_FLAGS item code.                                       */
/*                                                                          */
#define QUI$M_FORM_SHEET_FEED 1
#define QUI$M_FORM_TRUNCATE 2
#define QUI$M_FORM_WRAP 4
struct form_flags {
    unsigned qui$v_form_sheet_feed : 1; /* /SHEET_FEED                      */
    unsigned qui$v_form_truncate : 1;   /* /TRUNCATE                        */
    unsigned qui$v_form_wrap : 1;       /* /WRAP                            */
    unsigned qui$v_filler : 29;
    } ;
/*                                                                          */
/* Subfields of JOB_FLAGS item code.                                        */
/*                                                                          */
#define QUI$M_JOB_CPU_LIMIT 1
#define QUI$M_JOB_FILE_BURST 2
#define QUI$M_JOB_FILE_BURST_ONE 4
#define QUI$M_JOB_FILE_BURST_EXP 8
#define QUI$M_JOB_FILE_FLAG 16
#define QUI$M_JOB_FILE_FLAG_ONE 32
#define QUI$M_JOB_FILE_FLAG_EXP 64
#define QUI$M_JOB_FILE_TRAILER 128
#define QUI$M_JOB_FILE_TRAILER_ONE 256
#define QUI$M_JOB_FILE_TRAILER_EXP 512
#define QUI$M_JOB_LOG_DELETE 1024
#define QUI$M_JOB_LOG_NULL 2048
#define QUI$M_JOB_LOG_SPOOL 4096
#define QUI$M_JOB_LOWERCASE 8192
#define QUI$M_JOB_NOTIFY 16384
#define QUI$M_JOB_RESTART 32768
#define QUI$M_JOB_WSDEFAULT 65536
#define QUI$M_JOB_WSEXTENT 131072
#define QUI$M_JOB_WSQUOTA 262144
#define QUI$M_JOB_FILE_PAGINATE 524288
#define QUI$M_JOB_FILE_PAGINATE_EXP 1048576
struct job_flags {
    unsigned qui$v_job_cpu_limit : 1;   /* /CPUTIME explicit                */
    unsigned qui$v_job_file_burst : 1;  /* /BURST=ALL                       */
    unsigned qui$v_job_file_burst_one : 1; /* /BURST=ONE                    */
    unsigned qui$v_job_file_burst_exp : 1; /* /[NO]BURST explicit (intentionally not documented) */
    unsigned qui$v_job_file_flag : 1;   /* /FLAG=ALL                        */
    unsigned qui$v_job_file_flag_one : 1; /* /FLAG=ONE                      */
    unsigned qui$v_job_file_flag_exp : 1; /* /[NO]FLAG explicit (intentionally not documented) */
    unsigned qui$v_job_file_trailer : 1; /* /TRAILER=ALL                    */
    unsigned qui$v_job_file_trailer_one : 1; /* /TRAILER=ONE                */
    unsigned qui$v_job_file_trailer_exp : 1; /* /[NO]TRAILER explicit (intentionally not documented) */
    unsigned qui$v_job_log_delete : 1;  /* /NOKEEP                          */
    unsigned qui$v_job_log_null : 1;    /* /NOLOG_FILE                      */
    unsigned qui$v_job_log_spool : 1;   /* /PRINTER                         */
    unsigned qui$v_job_lowercase : 1;   /* /LOWERCASE                       */
    unsigned qui$v_job_notify : 1;      /* /NOTIFY                          */
    unsigned qui$v_job_restart : 1;     /* /RESTART                         */
    unsigned qui$v_job_wsdefault : 1;   /* /WSDEFAULT explicit              */
    unsigned qui$v_job_wsextent : 1;    /* /WSEXTENT explicit               */
    unsigned qui$v_job_wsquota : 1;     /* /WSQUOTA explicit                */
    unsigned qui$v_job_file_paginate : 1; /* /FEED                          */
    unsigned qui$v_job_file_paginate_exp : 1; /* /[NO]FEED explicit (intentionally not documented) */
    unsigned qui$v_filler : 11;
    } ;
/*                                                                          */
/* Subfields of JOB_STATUS item code.                                       */
/*                                                                          */
#define QUI$M_JOB_ABORTING 1
#define QUI$M_JOB_EXECUTING 2
#define QUI$M_JOB_HOLDING 4
#define QUI$M_JOB_INACCESSIBLE 8
#define QUI$M_JOB_REFUSED 16
#define QUI$M_JOB_REQUEUE 32
#define QUI$M_JOB_RESTARTING 64
#define QUI$M_JOB_RETAINED 128
#define QUI$M_JOB_STARTING 256
#define QUI$M_JOB_TIMED_RELEASE 512
#define QUI$M_JOB_SUSPENDED 1024
#define QUI$M_JOB_PENDING 2048
struct job_status {
    unsigned qui$v_job_aborting : 1;    /* Job is aborting                  */
    unsigned qui$v_job_executing : 1;   /* Job is executing                 */
    unsigned qui$v_job_holding : 1;     /* Job is holding for /HOLD         */
    unsigned qui$v_job_inaccessible : 1; /* Job is inaccessible             */
    unsigned qui$v_job_refused : 1;     /* Job was refused by symbiont      */
    unsigned qui$v_job_requeue : 1;     /* Job will requeue after abort (intentionally not documented) */
    unsigned qui$v_job_restarting : 1;  /* Job started execution at least once (intentionally not documented) */
    unsigned qui$v_job_retained : 1;    /* Job was retained by /RETAIN      */
    unsigned qui$v_job_starting : 1;    /* Job is starting                  */
    unsigned qui$v_job_timed_release : 1; /* Job is holding for /AFTER      */
    unsigned qui$v_job_suspended : 1;   /* Job is suspended by STOP/QUEUE command */
    unsigned qui$v_job_pending : 1;     /* Job is pending (see QUI$_JOB_PENDING_REASON for more information) */
    unsigned qui$v_filler : 20;
    } ;
#define QUI$V_JOB_TIMED 9               /* Synonym for QUI$V_JOB_TIMED_RELEASE for V4 compatibility */
#define QUI$M_JOB_TIMED 512             /* Synonym for QUI$M_JOB_TIMED_RELEASE for V4 compatibility */
/*                                                                          */
/* Subfields of PENDING_JOB_REASON.                                         */
/*                                                                          */
/* Note that in the descriptions below "queue" refers to the execution queue in which a job resides, or to the set of execution */
/* queues that is the target of the generic queue in which a job resides.   */
/*                                                                          */
#define QUI$M_PEND_CHAR_MISMATCH 1
#define QUI$M_PEND_JOB_SIZE_MAX 2
#define QUI$M_PEND_JOB_SIZE_MIN 4
#define QUI$M_PEND_LOWERCASE_MISMATCH 8
#define QUI$M_PEND_NO_ACCESS 16
#define QUI$M_PEND_QUEUE_BUSY 32
#define QUI$M_PEND_QUEUE_STATE 64
#define QUI$M_PEND_STOCK_MISMATCH 128
struct pending_job_reason {
    unsigned qui$v_pend_char_mismatch : 1; /* Queue's characteristics do not match job's requirements */
    unsigned qui$v_pend_job_size_max : 1; /* Print job's block size is too large to execute on queue (print job only) */
    unsigned qui$v_pend_job_size_min : 1; /* Print job's block size is too small to execute on queue (print job only) */
    unsigned qui$v_pend_lowercase_mismatch : 1; /* Queue's lowercase attribute does not match job's requirements (print job only) */
    unsigned qui$v_pend_no_access : 1;  /* User does not have access to queue */
    unsigned qui$v_pend_queue_busy : 1; /* Job is ready to execute but must wait until other jobs ahead of it in the queue */
/* complete execution (this is a normal state that requires no user intervention) */
    unsigned qui$v_pend_queue_state : 1; /* Queue state prevents job from executing (see QUI$_QUEUE_STATUS for more info) */
    unsigned qui$v_pend_stock_mismatch : 1; /* Stock type of mounted form on output execution queue does not match */
/* stock stock type required by form associated with job (print job only)   */
    unsigned qui$v_filler : 24;
    } ;
/*                                                                          */
/* Subfields of QUEUE_FLAGS item code.                                      */
/*                                                                          */
#define QUI$M_QUEUE_BATCH 1
#define QUI$M_QUEUE_CPU_DEFAULT 2
#define QUI$M_QUEUE_CPU_LIMIT 4
#define QUI$M_QUEUE_FILE_BURST 8
#define QUI$M_QUEUE_FILE_BURST_ONE 16
#define QUI$M_QUEUE_FILE_FLAG 32
#define QUI$M_QUEUE_FILE_FLAG_ONE 64
#define QUI$M_QUEUE_FILE_TRAILER 128
#define QUI$M_QUEUE_FILE_TRAILER_ONE 256
#define QUI$M_QUEUE_GENERIC 512
#define QUI$M_QUEUE_GENERIC_SELECTION 1024
#define QUI$M_QUEUE_JOB_BURST 2048
#define QUI$M_QUEUE_JOB_FLAG 4096
#define QUI$M_QUEUE_JOB_SIZE_SCHED 8192
#define QUI$M_QUEUE_JOB_TRAILER 16384
#define QUI$M_QUEUE_RETAIN_ALL 32768
#define QUI$M_QUEUE_RETAIN_ERROR 65536
#define QUI$M_QUEUE_SWAP 131072
#define QUI$M_QUEUE_TERMINAL 262144
#define QUI$M_QUEUE_WSDEFAULT 524288
#define QUI$M_QUEUE_WSEXTENT 1048576
#define QUI$M_QUEUE_WSQUOTA 2097152
#define QUI$M_QUEUE_FILE_PAGINATE 4194304
#define QUI$M_QUEUE_RECORD_BLOCKING 8388608
#define QUI$M_QUEUE_PRINTER 16777216
#define QUI$M_QUEUE_ACL_SPECIFIED 33554432
struct queue_flags {
    unsigned qui$v_queue_batch : 1;     /* /BATCH                           */
    unsigned qui$v_queue_cpu_default : 1; /* /CPUDEFAULT specified          */
    unsigned qui$v_queue_cpu_limit : 1; /* /CPUMAXIMUM specified            */
    unsigned qui$v_queue_file_burst : 1; /* /DEFAULT=BURST=ALL              */
    unsigned qui$v_queue_file_burst_one : 1; /* /DEFAULT=BURST=ONE          */
    unsigned qui$v_queue_file_flag : 1; /* /DEFAULT=FLAG=ALL                */
    unsigned qui$v_queue_file_flag_one : 1; /* /DEFAULT=FLAG=ONE            */
    unsigned qui$v_queue_file_trailer : 1; /* /DEFAULT=TRAILER=ALL          */
    unsigned qui$v_queue_file_trailer_one : 1; /* /DEFAULT=TRAILER=ONE      */
    unsigned qui$v_queue_generic : 1;   /* /GENERIC                         */
    unsigned qui$v_queue_generic_selection : 1; /* /ENABLE_GENERIC          */
    unsigned qui$v_queue_job_burst : 1; /* /SEPARATE=BURST                  */
    unsigned qui$v_queue_job_flag : 1;  /* /SEPARATE=FLAG	            */
    unsigned qui$v_queue_job_size_sched : 1; /* /SCHEDULE=SIZE              */
    unsigned qui$v_queue_job_trailer : 1; /* /SEPARATE=TRAILER              */
    unsigned qui$v_queue_retain_all : 1; /* /RETAIN=ALL                     */
    unsigned qui$v_queue_retain_error : 1; /* /RETAIN=ERROR                 */
    unsigned qui$v_queue_swap : 1;      /* /NODISABLE_SWAPPING              */
    unsigned qui$v_queue_terminal : 1;  /* /DEVICE=TERMINAL or terminal device type reported by symbiont */
    unsigned qui$v_queue_wsdefault : 1; /* /WSDEFAULT specified             */
    unsigned qui$v_queue_wsextent : 1;  /* /WSEXTENT specified              */
    unsigned qui$v_queue_wsquota : 1;   /* /WSQUOTA specified               */
    unsigned qui$v_queue_file_paginate : 1; /* /DEFAULT=FEED                */
    unsigned qui$v_queue_record_blocking : 1; /* /RECORD_BLOCKING           */
    unsigned qui$v_queue_printer : 1;   /* /DEVICE=PRINTER or printer device type reported by symbiont */
    unsigned qui$v_queue_acl_specified : 1; /* ACL has been specified for queue */
    unsigned qui$v_filler : 6;
    } ;
/*                                                                          */
/* Subfields of QUEUE_STATUS item code.                                     */
/*                                                                          */
#define QUI$M_QUEUE_ALIGNING 1
#define QUI$M_QUEUE_IDLE 2
#define QUI$M_QUEUE_LOWERCASE 4
#define QUI$M_QUEUE_OPERATOR_REQUEST 8
#define QUI$M_QUEUE_PAUSED 16
#define QUI$M_QUEUE_PAUSING 32
#define QUI$M_QUEUE_REMOTE 64
#define QUI$M_QUEUE_RESETTING 128
#define QUI$M_QUEUE_RESUMING 256
#define QUI$M_QUEUE_SERVER 512
#define QUI$M_QUEUE_STALLED 1024
#define QUI$M_QUEUE_STARTING 2048
#define QUI$M_QUEUE_STOPPED 4096
#define QUI$M_QUEUE_STOPPING 8192
#define QUI$M_QUEUE_UNAVAILABLE 16384
#define QUI$M_QUEUE_CLOSED 32768
struct queue_status {
    unsigned qui$v_queue_aligning : 1;  /* Queue is aligning                */
    unsigned qui$v_queue_idle : 1;      /* Queue is idle                    */
    unsigned qui$v_queue_lowercase : 1; /* Lowercase device                 */
    unsigned qui$v_queue_operator_request : 1; /* Queue is doing /OPERATOR (intentionally not documented) */
    unsigned qui$v_queue_paused : 1;    /* Queue is paused                  */
    unsigned qui$v_queue_pausing : 1;   /* Queue is pausing                 */
    unsigned qui$v_queue_remote : 1;    /* Remote device                    */
    unsigned qui$v_queue_resetting : 1; /* Incomplete remote request to reset */
    unsigned qui$v_queue_resuming : 1;  /* Queue is resuming                */
    unsigned qui$v_queue_server : 1;    /* /DEVICE=SERVER or reported to be a served queue by the symbiont */
    unsigned qui$v_queue_stalled : 1;   /* Device is stalled                */
    unsigned qui$v_queue_starting : 1;  /* Queue is starting                */
    unsigned qui$v_queue_stopped : 1;   /* Queue is stopped                 */
    unsigned qui$v_queue_stopping : 1;  /* Queue is stopping                */
    unsigned qui$v_queue_unavailable : 1; /* Device is unavailable          */
    unsigned qui$v_queue_closed : 1;    /* Queue is closed                  */
    unsigned qui$v_filler : 16;
    } ;
/*                                                                          */
/* Subfields of SEARCH_FLAGS item code.                                     */
/*                                                                          */
#define QUI$M_SEARCH_ALL_JOBS 1
#define QUI$M_SEARCH_WILDCARD 2
#define QUI$M_SEARCH_BATCH 4
#define QUI$M_SEARCH_SYMBIONT 8
#define QUI$M_SEARCH_THIS_JOB 16
#define QUI$M_SEARCH_PRINTER 32
#define QUI$M_SEARCH_SERVER 64
#define QUI$M_SEARCH_TERMINAL 128
#define QUI$M_SEARCH_GENERIC 256
#define QUI$M_SEARCH_GENERIC_TARGET 512
#define QUI$M_SEARCH_PENDING_JOBS 1024
#define QUI$M_SEARCH_EXECUTING_JOBS 2048
#define QUI$M_SEARCH_TIMED_RELEASE_JOBS 4096
#define QUI$M_SEARCH_HOLDING_JOBS 8192
#define QUI$M_SEARCH_RETAINED_JOBS 16384
#define QUI$M_SEARCH_FREEZE_CONTEXT 32768
struct search_flags {
    unsigned qui$v_search_all_jobs : 1; /* Select all jobs (else only those with same username as caller) */
    unsigned qui$v_search_wildcard : 1; /* Force a wildcard operation       */
    unsigned qui$v_search_batch : 1;    /* Select batch queues              */
    unsigned qui$v_search_symbiont : 1; /* Select symbiont (output) queues (note that QUI$V_SEARCH_SYMBIONT is equivalent */
/* to setting QUI$V_SEARCH_PRINTER, QUI$V_SEARCH_SERVER, and QUI$V_SEARCH_TERMINAL) */
    unsigned qui$v_search_this_job : 1; /* Select only caller's batch job (forcing new context) */
    unsigned qui$v_search_printer : 1;  /* Select printer queues            */
    unsigned qui$v_search_server : 1;   /* Select server queues             */
    unsigned qui$v_search_terminal : 1; /* Select terminal queues           */
    unsigned qui$v_search_generic : 1;  /* Select generic queues            */
    unsigned qui$v_search_generic_target : 1; /* Select generic target queues after selecting generic queue (for internal use) */
/* (note this option is NOT supported; it is intentionally not documented and */
/* reserved for Digital use)                                                */
    unsigned qui$v_search_pending_jobs : 1; /* Select only pending jobs     */
    unsigned qui$v_search_executing_jobs : 1; /* Select only executing jobs */
    unsigned qui$v_search_timed_release_jobs : 1; /* Select only timed release jobs */
    unsigned qui$v_search_holding_jobs : 1; /* Select only holding jobs     */
    unsigned qui$v_search_retained_jobs : 1; /* Select only retained jobs   */
    unsigned qui$v_search_freeze_context : 1; /* Do not advance wildcard context to next object in list at end of operation */
    unsigned qui$v_filler : 16;
    } ;
/*                                                                          */
/* Subfields of SYMBIONT_FLAGS item code.                                   */
/*                                                                          */
/* Note: this item code is intentionally not documented and reserved for Digital use. */
/*                                                                          */
#define QUI$M_SYM_NOTIFIES 1
#define QUI$M_SYM_REQUESTS_OPER 2
#define QUI$M_SYM_COPIES_FILE 4
#define QUI$M_SYM_COPIES_JOB 8
#define QUI$M_SYM_ACCEPTS_ALL_FORMS 16
#define QUI$M_SYM_NO_JOB_CHECKPOINT 32
struct symbiont_flags {
    unsigned qui$v_sym_notifies : 1;    /* Symbiont notifies for job completions */
    unsigned qui$v_sym_requests_oper : 1; /* Symbiont generates operator messages */
    unsigned qui$v_sym_copies_file : 1; /* Symbiont generates multiple file copies */
    unsigned qui$v_sym_copies_job : 1;  /* Symbiont generates multiple job copies */
    unsigned qui$v_sym_accepts_all_forms : 1; /* Symbiont processes all form types */
    unsigned qui$v_sym_no_job_checkpoint : 1; /* Always reprocess full job on a restart */
    unsigned qui$v_filler : 26;
    } ;
 

#endif					/* __QUIDEF_LOADED */
/*
**	History:
**	$Log: quidef.h,v $
**	Revision 1.5  1996-08-19 18:32:45-04  gsl
**	drcs update
**
**
**
*/
