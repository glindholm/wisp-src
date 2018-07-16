/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef _DISAM_H
#define _DISAM_H
/*----
disam.h
------*/
#ifdef comout

VERSION 1.2

COPYRIGHT - BYTE DESIGNS LTD. (c) 1988

this header file is normally included in any module which uses D-ISAM

#endif

/****************************************************************************
    T H E   F O L L O W I N G    S E C T I O N  I S    S E T    A T

                     P O R T I N G    T I M E
   if anything is changed, the entire isam library must be recompiled!
****************************************************************************/

                               /******/

/***** SELECT YOUR OPERATING SYSTEM,
       ASSURE ONE AND ONLY ONE OF THE FOLLOWING DEFINES IS UNCOMMENTED */

/* #define ISDOS             /* MS or PC DOS operating system */
/* #define ISDOSLOCK      /* NETWORKED or otherwise shared DOS */
#define ISUNIX         /* UNIX or XENIX, not BERKLEY */
/* #define ISBERKLEY      /* A BERKLEY VERSION OF UNIX */


                               /******/


/***** SELECT YOUR PROCESSOR LOGIC
       ASSURE ONE AND ONLY ONE OF THE FOLLOWING DEFINES IS UNCOMMENTED */
/* mjb I have modified isxtra.c which uses this define so that it makes
   its own internal determination of byte order, and does not need the
   defines.
*/

/* #define ISINTEL     /* Intel order processor, ie: 8088, 80x86
                          and other processors who
                          do integer storage similarly */
/* #define ISMOTOROLA  /* Motorola order processor, ie: 680x0
                          and other processors who
                          do integer storage similarly */

/*#define MLARGE          /* uncomment if you are using the INTEL LARGE model,
                           or 386 MODE */


                               /******/


/***** UNCOMMENT THE LINE WHICH DEFINES YOUR LOCKING FUNCTION
       ASSURE ONE AND ONLY ONE OF THE FOLLOWING DEFINES IS UNCOMMENTED */

/* #define LOCKFCTN none        /* if there is no locking function */
/* #define LOCKFCTN doslock  /* for a shared (networked) DOS environment */
#define LOCKFCTN lockf    /* if you have a lockf() ( unix 5.2 etc.) */
/* #define LOCKFCTN locking  /* if you have a locking() function, XENIX etc.*/
/* #define flock             /* if you have a berkley system which has no */
                             /* lockf(), or you need compatibility with
                                C-ISAM locking where a .lok file is created */


/***** UNCOMMENT THE FOLLOWING LINE IF YOUR LOCKING FUNCTION'S "LENGTH"
       IS OF TYPE long, (AS WITH XENIX'S LOCKING), or if your integers
       are four byte */

#define LONGLOCK            /* commented out only w/ 80x86 processors
                                  where "locking" does not exist */


/***** DOS ONLY, select which order your rename is -- it seems
       to be inconsistantly implemented */

#define rname( x,y ) rename( x,y )      /* THE NORMAL METHOD */
/* #define rname( x,y ) rename( y,x )   /* XENIX X-COMPILE METHOD */

                               /******/


/***** DEFINE THE INDEX BLOCKING SIZE.  D-ISAM WILL BUILD FILES WITH THE
       BLOCK SIZE SET HERE, AND WILL READ FILES WITH THIS BLOCK SIZE OR
       SMALLER, BUT NOT LARGER.  A LARGE BLOCK SIZE PRODUCES FASTER RESULTS,
       BUT COSTS MEMORY, AND PRODUCES A LARGER INITIAL .IDX SIZE */

#define IsMAXBLK 512             /* normally 512 or 1024 */


/***** DEFINE THE NUMBER OF BUFFERS WHICH ARE AVAILABLE TO THE AUTOMATIC
       BUFFERING SYSTEM.  THE MORE BUFFERS AVAILABLE, THE FASTER D-ISAM
       WILL WORK.  EACH BUFFER COSTS JUST OVER ONE IsBLOCK SIZE OF MEMORY. */

#define IsNBUFS 20               /* SETTINGS LOWER THAN 20 ARE UNADVISED */




                               /******/

/***** SELECT WHETHER AN ISBUILD call will create over the old file
       or will error out if the file already exists */

#ifndef ISDOS
/* #define BU_PARAM O_RDWR | O_CREAT | O_EXCL        /* ERROR OUT IF EXISTS */
#define BU_PARAM O_RDWR | O_CREAT | O_TRUNC    /* FORCE BUILD */

#else
/* #define BU_PARAM O_RDWR | O_CREAT | O_EXCL | O_BINARY  /* ERROR OUT */
#define BU_PARAM O_RDWR | O_CREAT | O_TRUNC | O_BINARY /* FORCE */

#endif


                               /******/


/***** UNCOMMENT THE FOLLOWING LINE IF YOU WANT ACCESS TO OUR DEBUGGING AIDS */
/* #define ISDEBUG             /* see PORTING(I) for usage details */

                               /******/

/***** UNCOMMENT THE FOLLOWING LINE IF YOU WANT EXTENDED ERROR CODES */
#define EXTENDED            /* if so, you will get further information on
                                  EBADFILE errors, useful to help us debug */


                               /******/


/***** SET THE FOLLOWING VALUES AS REQUIRED, THE SUPPLIED SETTINGS
                     ARE NORMALLY ACCEPTABLE                               */

#define MAXFDS 10              /* max. number of open files,
                                  on UNIX (v5.2 or less) systems set to 10 */
#define MAXSUBS 17
                               
#define NPARTS 8               /* number of key parts per key,
                                  8 is C-ISAM compatable.
                               */
#define MAXLOCKS 20            /* maximum number of manual locked records
                                  per opened file.  No C-ISAM equivelant
                               */

/****************************************************************************
              E N D    P O R T I N G    S E C T I O N
****************************************************************************/

extern int iserrno;            /* on err, value of error is returned here */
extern int iserrio;            /* on return, contains value of most recent
                                  isam function called, see isfctns.h */
extern long isrecnum;

#ifndef isammain
extern int issingleuser;       /* if set true, then all locking is abandoned */
extern char *iscopyright;      /* our copyright notice */
extern char *isserial;         /* the program's serial number */
extern char *isversnumber;     /* the program's version number */
extern int is_nerr;            /* the highest isam error code */
extern char *is_errlist[];     /* names of the errors */
extern int IsBLOCK;            /* dynamically set to present block size */

#else
int issingleuser = 0;          /* default to multiuser */
char *iscopyright = "COPYRIGHT BYTE DESIGNS LTD. 1988";
char *isserial = "";
char *isversnumber = "2.0";
int is_nerr = 17;
int IsBLOCK = IsMAXBLK;
char *is_errlist[] =     /* less terse descriptions can be inserted here */
       { "EDUPL",      /* error 100 */
         "ENOTOPEN",
         "EBADARG",
         "EBADKEY",
         "ETOOMANY",
         "EBADFILE",
         "ENOTEXCL",
         "ELOCKED",
         "EKEXISTS",
         "EPRIMKEY",
         "EENDFILE",
         "ENOREC",
         "ENOCURR",
         "EFLOCKED",
         "EFNAME",
         "ENOLOK",
         "EBADMEM" };
#endif

struct keypart
       { short kp_start;       /* offset to key part */
         short kp_leng;        /* # of bytes in keypart */
         short kp_type;        /* processing directions for keypart */
       };

#define k_start k_part[0].kp_start
#define k_leng  k_part[0].kp_leng
#define k_type  k_part[0].kp_type


typedef struct keydesc
       { short k_flags;                        /* key characteristics */
         short k_nparts;                       /* number of parts in key */
         struct keypart k_part[NPARTS];        /* discription of each part */
               /* the following needn't be set by user */
         short k_len;                          /* length of key */
         long  k_rootnode;                     /* where root is for this key */
       }KEYDESC;

extern struct dictinfo
       { short di_nkeys;       /* # of keys */
         short di_recsize;     /* # of bytes in a datarecord,
                                  ( not including delete flag ) */
         short di_idxsize;     /* # of bytes in an index block */
         long  di_nrecords;    /* number of records
                                  ( 0 IF EMPTY, 1 IF NOT EMPTY ) */
      /* only from here up valid on isindexinfo() call */

         int   di_datfd;       /* open() file descriptor of the .dat file */
         int   di_idxfd;       /* open() file descriptor of the .key file */
         long  di_datrecd;     /* which data record is "CURRENT" 0 if none */
         short di_actvkey;     /* which key is the active key */
         short di_openmode;    /* the type of open which was used */
         char *di_map;         /* map of autoconversions (allocated) */
         char *di_name;        /* the name given to the file at open time */
         char *di_pad;         /* allocated key description */
         char  di_disjoint;    /* if set, then CURR & NEXT give same result */
         long  di_locrec;      /* relocation record */
         char *di_lockey;      /* the associated key */
         unsigned int  di_locdup;      /* relocation duplicate # */
         int   di_locidx;      /* the associated index */
         int   di_noprim;      /* if( di_noprim ) then has null primary key */
         struct keydesc *di_desc[MAXSUBS]; /* pointer to key descr. info */
       } *isfdmap[ MAXFDS ];   /* info can be accessed as per:
                                  isfdmap[ isfd ]->xxx,
                                  if file not open, isfdmap[ isfd ] = (NULL)
                               */

/******************************************
             USER CONSTANTS
*******************************************/

#define SUCCESS 0
#define WHOLE 0

/** OPENING **/
#define ISINPUT        0               /* open read only */
#define ISOUTPUT       1               /* open write only */
#define ISINOUT        2               /* open with read & write permission */

/** LOCKING **/
#define ISAUTOLOCK     0x200           /* record locking automatic */
#define ISMANULOCK     0x400           /* manual locking */
#define ISEXCLLOCK     0x800           /* full file lock */
#define ISLOCK         0x100           /* lock on read request */
#define ISWAIT         0x400           /* lock on read, wait til can */

/** KEY DEFINING OPTIONS **/
#define ISNODUPS       0               /* duplicates not allowed */
#define ISDUPS         1               /* duplicate keys permitted */
#define DCOMPRESS      2               /* compress duplicates */
#define LCOMPRESS      4               /* leading redundancy compress */
#define TCOMPRESS      8               /* trailing constant compress */
#define TNULL          0x10            /* use null as trailing constant */
#define COMPRESS ( LCOMPRESS + DCOMPRESS + TCOMPRESS )

/** KEY TYPES **/

#define ISDESC         0x80            /* use descending order, flag */

#define CHARTYPE       0
#define CHARSIZE       1

#define INTTYPE        1
#define INTSIZE        2

#define LONGTYPE       2
#define LONGSIZE       4

#define DOUBLETYPE     3
#define DOUBLESIZE     sizeof( double )

#define FLOATTYPE      4
#define FLOATSIZE      sizeof( float )

#define MINTTYPE       5
#define MINTSIZE       sizeof( int )

#define MLONGTYPE      6
#define MLONGSIZE      sizeof( long )

#define STRINGTYPE     7
#define STRINGSIZE     1

/** READING OPTIONS **/
#define ISFIRST        0               /* find logical first record */
#define ISLAST         1               /* find logical last record */
#define ISNEXT         2               /* find logical next record */
#define ISPREV         3               /* find logical previous record */
#define ISCURR         4               /* find "current" record */
#define ISEQUAL        5               /* find exact key match */
#define ISGREAT        6               /* find nearest greater than key */
#define ISGTEQ         7               /* find exact or nearest key */

/** AUDIT TRAIL INFO **/

#define AUDSETNAME     0               /* DEFINES for isaudit() */
#define AUDGETNAME     1
#define AUDSTART       2
#define AUDSTOP        3
#define AUDINFO        4

/* functions returning non ints defined here for convenience */
long ldlong();
long ldmlong();
float ldfloat();
double lddbl();
char *isseekey();
long isseecurr();


/****************************************************************************
                         E R R O R    C O D E S
*****************************************************************************/

/** ERROR MNEUMONICS **/
#define EDUPL          100             /* duplicate record */
#define ENOTOPEN       101             /* file not open */
#define EBADARG        102             /* invalid argument */
#define EBADKEY        103             /* invalid key description */
#define ETOOMANY       104             /* out of file descriptors */
#define EBADFILE       105             /* invalid isam file format */
#define ENOTEXCL       106             /* exclusive lock required */
#define ELOCKED        107             /* record claimed by another */
#define EKEXISTS       108             /* key already exists */
#define EPRIMKEY       109             /* primary key may not be used */
#define EENDFILE       110             /* beginning or end of file reached */
#define ENOREC         111             /* no match was found */
#define ENOCURR        112             /* there is no "current" established */
#define EFLOCKED       113             /* entire file locked by another */
#define EFNAME         114             /* file name too long */
#define ENOLOK         115             /* cannot create lock file */
#define EBADMEM        116             /* memory allocation request failed */
#define EBADCOLL       117             /* bad custom collating */

/***************************************************************
POTENTIAL BUG ERRORS, SET TO EBADFILE UNLESS #EXTENDED SET
****************************************************************/

#ifdef EXTENDED
#define BAD_REQ 201
#define TOO_FAR 202
#define NO_INFO 203
#define NOT_SET 204
#define BAD_INFO 205

#else
#define BAD_REQ 105
#define TOO_FAR 105
#define NO_INFO 105
#define NOT_SET 105
#define BAD_INFO 105
#endif

/******************************************************************************
                  F U N C T I O N   M N E U M O N I C S
******************************************************************************/

/* these values are found in iserrio after the return of the associated
   function call */

#define FC_DELCURR     1               /* dellcurr() typ. */
#define FC_DELETE      2
#define FC_READ        3
#define FC_REWCURR     4
#define FC_REWRITE     5
#define FC_WRCURR      6
#define FC_WRITE       7
#define FC_START       8
#define FC_OPEN        9
#define FC_DELREC      11
#define FC_REWREC      12
#define FC_ADDIND      13
#define FC_LOCK        14
#define FC_DELIND      15
#define FC_REL         16

#endif /*_DISAM_H */
/*
**	History:
**	$Log: disam.h,v $
**	Revision 1.2  1996-10-02 12:06:13-04  gsl
**	Add standard headers
**
**
**
*/
