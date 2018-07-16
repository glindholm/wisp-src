static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----
kvxi.c

IO for VAX ISAM files.

----*/

#include <stdio.h>
#include <ctype.h>

#include rms
#include "vax_rms.h"
#include "kcsio.h"
#include "vaxiocod.h"

static char sccsid[]="@(#)kvxi.c	2.75 1/23/95";

KFAB   fab_ent[MAXFILES];               /* Array of 10 FABs for files */
KRAB   rab_ent[MAXFILES];               /* Record area for each file */
KXABKEY   xab_key_ent[MAXFILES][MAXKEYS];
                                       /* Set of 17 XABKEYs for each file */
/* If the number of MAXFILES is ever changed, then this has to be changed
   also so that these 2 arrays are initialized to zero.  We do this to
   avoid system dependent initialization in higher level routines.
*/
int   in_use[MAXFILES] = {0,0,0,0,0,0,0,0,0,0};
int   locked[MAXFILES] = {0,0,0,0,0,0,0,0,0,0};

KFAB   *fab_ptr;
KRAB   *rab_ptr;
KXABKEY *xab_ptr;

int   f_errno = 0;

static char hd_rec[4096];

/*----
            <<<<<<<<<  OPEN AND CLOSE ROUTINES   >>>>>>>>>>>
------*/
/*---
Shared is the same as IO for C-ISAM. All C-ISAM opens to be compatible
with COBOL require that the file be set up at the first record.
A COBOL open sets things up for the first record. A c-isam open
does not.
------*/
ksam_open_shared(kfb)
KCSIO_BLOCK *kfb;
{
   ksam_open_io(kfb);
}

ksam_open_input(kfb)
KCSIO_BLOCK *kfb;
{
   do_open(kfb,Finput);
}

ksam_open_io(kfb)
KCSIO_BLOCK *kfb;
{
   do_open(kfb,Fio);
}
/*----
Doopen should not be used for open output
------*/
static do_open(kfb,mode)
KCSIO_BLOCK *kfb;
int mode;
{
   register int idx;
   f_errno = 0;
   f_errno = vax_open_idx(kfb, mode);
   kfb->_status = vax_trans(f_errno);
   f_errno = kfb->_status;
   kfb->_last_io_key = 0;
   kfb->_io_key = 0;
   kfb->_open_status = 1;
   ksam_file_info(kfb);

/* Connect the record stream to the file and set next record pointer to
   first record
*/
   if(f_errno == 0)
      {
      idx = kfb->_io_channel;
      sys$connect(&rab_ent[idx]);
      }
}

/*----
KSAM_OPEN_OUTPUT
Set evrything up for an open output with FAB, RAB and XABKEYs.
----*/
ksam_open_output(kfb)
   KCSIO_BLOCK *kfb;
{
vax_open_output(kfb);
}

/*----
VAX_OPEN Routine
----*/
int vax_open_idx(kfb,mode)
   KCSIO_BLOCK *kfb;
   int mode;
{
register int han;
int err_code;

        han=find_fab();                /* Get a an unused FAB */
if(han < 0)
   {
   return(EBADFILE);                   /* Used all FAB's.  Shouldn't happen */
   }
kfb->_io_channel = han;

init_fab(han);                         /* Init the FAB/RAB/XAB areas */

/*----

For ISAM files there is 1 XABKEY for each key.  There are a maximun of
MAXKEYS for any file.  Here we link all the XABKEYs for this FAB together
so that the open can fill in the information.
----*/
link_xab_keys(han);

/*----
Set up the FAB for the actual OPEN
----*/
if(mode == Fio)                        /* If IO mode */
   fab_ent[han].fab$b_fac = (FAB$M_UPD+FAB$M_DEL+FAB$M_PUT+FAB$M_GET);
                                       /* Allows Read, Write, Del & Upd */
else if(mode == Finput)
   fab_ent[han].fab$b_fac = FAB$M_GET;  /* Allows Read only */

fab_ent[han].fab$l_fna = kfb->_sys_name;  /* Point to file spec/name */
fab_ent[han].fab$b_fns = strlen(kfb->_sys_name); /* File Spec size */
/* fab_ent[han].fab$b_shr = FAB$M_SHRUPD;    /* Allowing All by other users */
fab_ent[han].fab$b_shr = 
	(FAB$M_SHRPUT+FAB$M_SHRGET+FAB$M_SHRDEL+FAB$M_SHRUPD); /* True All */
err_code = sys$open(&fab_ent[han]);    /* Open the file */
unlink_xab_keys(han);               /* unlink XABKEYS not needed */

/* Return code from sys$open has invalid key of reference code in it and
   the file isn't really open.  Try again again if we have an error like
   a bad file.
*/
err_code = vax_trans(err_code);
if(err_code = EBADFILE)                /* If it looks like a bad file */
   err_code = sys$open(&fab_ent[han]); /* Open it again */

return(err_code);
}


/*----
VAX_OPEN_OUTPUT
Set up FAB, RAB and XABKEYS for open output
----*/

vax_open_output(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;                   /* File handle or index into FAB's */
register int idx;                      /* Index into XABKEYs for this FAB */
register int ak;
int err_code;

han=find_fab();                        /* Get a an unused FAB */
if(han < 0)
   {
   return(EBADFILE);                   /* Used all FABs.  Shouldn't happen */
   }

kfb->_io_channel = han;
init_fab(han);                         /* Set up data structures */

/*----
For ISAM files there is 1 XABKEY for each key.  There are a maximun of
MAXKEYS for any file.  Here we link all the XABKEYs for this FAB together
so that the open can fill in the information.
----*/
link_xab_keys(han);

/*----
Set up the FAB for the actual OPEN
----*/
fab_ent[han].fab$b_fac = FAB$M_PUT;    /* Allows Read, Write, Del & Upd */
fab_ent[han].fab$l_fna = kfb->_sys_name;  /* Point to file spec/name */
fab_ent[han].fab$b_fns = strlen(kfb->_sys_name); /* File Spec size */
fab_ent[han].fab$b_shr = FAB$M_NIL;    /* No other access to file */
fab_ent[han].fab$w_mrs = kfb->_record_len;   /* Rec size */
fab_ent[han].fab$b_org = FAB$C_IDX;    /* File organization */

fab_ent[han].fab$b_rat = FAB$M_CR;     /* Carriage control */

/* Record attributes */
fab_ent[han].fab$b_rfm = FAB$C_FIX;    /* Record format */
fab_ent[han].fab$b_bks = 0;            /* Let RMS set default bucket size */

/* Set up Primary key */
xab_key_ent[han][0].xab$b_siz0 = kfb->_key[0].k_part[0].kp_leng; /* Size */
xab_key_ent[han][0].xab$w_pos0 = kfb->_key[0].k_part[0].kp_start; /* Pos */

/* Set up alt keys */
for(ak = 0, idx = 1; ak < kfb->_altkey_count; ++ak, idx++)
   {
   xab_key_ent[han][idx].xab$b_siz0 = kfb->_key[ak+1].k_part[0].kp_leng;
   xab_key_ent[han][idx].xab$w_pos0 = kfb->_key[ak+1].k_part[0].kp_start;

   if(kfb->_key[ak+1].k_flags == ISDUPS)
      xab_key_ent[han][idx].xab$b_flg = (XAB$M_DUP + XAB$M_CHG);
   else
      xab_key_ent[han][idx].xab$b_flg = 0;
   }

/* Unlink unnecessary XABKEYs */
idx = kfb->_altkey_count;              /* Number of alt keys */
xab_key_ent[han][idx].xab$l_nxt = 0;   /* No next XABKEY */

f_errno = sys$create(&fab_ent[han]);   /* Do the create */
kfb->_status = vax_trans(f_errno);
f_errno = kfb->_status;
kfb->_last_io_key = 0;
kfb->_open_status = 1;
ksam_file_info(kfb);

/* If no error in the file, then establish a record stream for writing */
if(f_errno == 0)
   {
   idx = kfb->_io_channel;
   sys$connect(&rab_ent[idx]);
   }
}

/*----
KSAM_CLOSE()
Routine to close and clean up
----*/
ksam_close(kfb)
KCSIO_BLOCK *kfb;
{

kfb->_status = f_errno = 0;
if(kfb->_open_status == 0)
   return(0);
vax_close(kfb->_io_channel);
kfb->_status = vax_trans(f_errno);
kfb->_open_status = 0;

}

/*----
VAX_CLOSE()
Just close the file and flush the buffers
----*/
vax_close(channel)
   int   channel;
{
f_errno = sys$close(&fab_ent[channel]);   /* Close the file */
in_use[channel] = 0;                      /* Show FAB as not in use */
}

/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
ksam_read(kfb)
KCSIO_BLOCK *kfb;
{
   kfb->_io_key = 0;
   ksam_read_keyed(kfb);
}

ksam_hold(kfb)
KCSIO_BLOCK *kfb;
{
   kfb->_io_key = 0;
   ksam_hold_keyed(kfb);
}

ksam_read_next(kfb)
KCSIO_BLOCK *kfb;
{
f_errno = 0;
f_errno = vax_read_next(kfb, RAB$M_NLK);     /* Read Next No Lock */
kfb->_status = vax_trans2(f_errno);
}

ksam_hold_next(kfb)
KCSIO_BLOCK *kfb;
{
f_errno = 0;
f_errno = vax_read_next(kfb, 0);       /* Lock is the default */
kfb->_status = vax_trans2(f_errno);
}
ksam_read_keyed(kfb)
KCSIO_BLOCK *kfb;
{
f_errno = 0;
f_errno = vax_read_keyed(kfb, RAB$M_NLK); /* No lock option */
kfb->_status = vax_trans(f_errno);
}

ksam_hold_keyed(kfb)
KCSIO_BLOCK *kfb;
{
f_errno = 0;
f_errno = vax_read_keyed(kfb, 0);      /* Lock by default */
kfb->_status = vax_trans(f_errno);
}

/*----
         <<<< STARTS >>>>
------*/
ksam_start_eq(kfb)
KCSIO_BLOCK *kfb;
{
   kfb->_io_key = 0;
   do_start(kfb,F_EQUALS);
}

ksam_start_nlt(kfb)
KCSIO_BLOCK *kfb;
{
   kfb->_io_key = 0;
   do_start(kfb,F_NOT_LESS);
}

ksam_start_gt(kfb)
KCSIO_BLOCK *kfb;
{
   kfb->_io_key = 0;
   do_start(kfb,F_GREATER);
}

ksam_start_eq_keyed(kfb)
KCSIO_BLOCK *kfb;
{
   do_start(kfb,F_EQUALS);
}

ksam_start_nlt_keyed(kfb)
KCSIO_BLOCK *kfb;
{
   do_start(kfb,F_NOT_LESS);
}

ksam_start_gt_keyed(kfb)
KCSIO_BLOCK *kfb;
{
   do_start(kfb,F_GREATER);
}

ksam_start_last(kfb)
KCSIO_BLOCK *kfb;
{
   kfb->_status = EBADARG;
}

/*----
Executes a start and sets up a new _last_io_key.
------*/
static do_start(kfb,mode)
KCSIO_BLOCK *kfb;
int mode;
{
f_errno = 0;
f_errno = vax_start(kfb, mode);
kfb->_status = vax_trans(f_errno);
kfb->_last_io_key = kfb->_io_key;
}

/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete are vanilla except that a hold should have
been issued before the rewrite or delete so these are released.
For VAX version, in order to emulate the WANG record locking, a check
has to be done to see if any operation with a hold has been done, and
if so, to unlock any records on the record stream before subsequent
IO is done to the file on that record stream.
------*/
ksam_write(kfb)
KCSIO_BLOCK *kfb;
{
f_errno = 0;
f_errno = vax_write(kfb);
kfb->_status = vax_trans(f_errno);
}

ksam_rewrite(kfb)
KCSIO_BLOCK *kfb;
{
	memcpy(hd_rec,kfb->_record,kfb->_record_len);
	ksam_hold(kfb);
	memcpy(kfb->_record,hd_rec,kfb->_record_len);
	if(kfb->_status)
		return;
	
	f_errno = 0;
	f_errno = vax_rewrite(kfb);
	kfb->_status = vax_trans(f_errno);
}

ksam_delete(kfb)
KCSIO_BLOCK *kfb;
{
	memcpy(hd_rec,kfb->_record,kfb->_record_len);
	ksam_hold(kfb);
	memcpy(kfb->_record,hd_rec,kfb->_record_len);
	if(kfb->_status)
		return;
	
	f_errno = 0;
	f_errno = vax_delete(kfb);
	kfb->_status = vax_trans(f_errno);
}

ksam_unlock(kfb)
KCSIO_BLOCK *kfb;
{
sys$free(&rab_ent[kfb->_io_channel]);  /* Free all records */
locked[kfb->_io_channel] = 0;          /* Clear flag */
}

/*----
VAX_READ_KEYED
Routine to just read on the key of reference
----*/
vax_read_keyed(kfb, lock)
   KCSIO_BLOCK *kfb;
   int   lock;
{
register int   han;                    /* File handle or index into FABs */
register int   idx;                    /* XABKEY index */
register char  *buf;                   /* Record buffer */
int   err_code;

han = kfb->_io_channel;                /* Get FAB index */
idx = kfb->_io_key;                    /* Key of Ref */

if(locked[han] != 0)                   /* If last operation locked it */
   {
   sys$free(&rab_ent[han]);            /* Free last record locked */
   locked[han] = 0;                    /* Clear flag */
   }

rab_ent[han].rab$b_krf = idx;          /* Set in RAB */
buf = kfb->_record;                    /* Point to record buffer */
buf += xab_key_ent[han][idx].xab$w_pos0;  /* Offset of key */
rab_ent[han].rab$l_kbf = buf;          /* Set RAB key buffer */
rab_ent[han].rab$b_ksz = xab_key_ent[han][idx].xab$b_siz0;
                                       /* Size of key */
rab_ent[han].rab$b_rac = RAB$C_KEY;    /* Keyed access */
rab_ent[han].rab$l_rop = 0;            /* Key equal only */
rab_ent[han].rab$l_rop |= RAB$M_WAT;   /* Wait if record is locked */

rab_ent[han].rab$l_rop |= lock;        /* Lock option */
if(lock == 0)
   locked[han] = 1;                    /* Set lock flag */
rab_ent[han].rab$l_rbf = kfb->_record; /* Record buffer (output */
rab_ent[han].rab$w_rsz = kfb->_record_len; /* Output buffer len */
rab_ent[han].rab$l_ubf = kfb->_record;    /* Input Record Buf */
rab_ent[han].rab$w_usz = kfb->_record_len; /* Input Buf Size */

sys$get(&rab_ent[han]);                /* Get the record */
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
return(rab_ent[han].rab$l_sts);
}

/*----
VAX_READ_NEXT()
Routine to read the next record.  Input is KFB and lock bit mask
*/
vax_read_next(kfb, lock)
   KCSIO_BLOCK *kfb;
   int   lock;
{
register int   han;                    /* File handle or index into FABs */
register int   idx;                    /* XABKEY index */
register char  *buf;                   /* Record buffer */
int   err_code;

han = kfb->_io_channel;                /* Get FAB index */

if(locked[han] != 0)                   /* If last operation locked something */
   {
   sys$free(&rab_ent[han]);            /* Free it up */
   locked[han] = 0;                    /* Clear Flag */
   }

/*----
These next few lines setting up the key of reference and so forth may
not be necessary when the Record Access Mode rab$b_rac is set to
sequential.
----*/
/*----
idx = kfb->_io_key;
rab_ent[han].rab$b_krf = idx;
buf = kfb->_record;
buf += xab_key_ent[han][idx].xab$w_pos0;
rab_ent[han].rab$l_kbf = buf;
rab_ent[han].rab$b_ksz = xab_key_ent[han][idx].xab$b_siz0;
----*/

rab_ent[han].rab$b_rac = RAB$C_SEQ;    /* Sequential Access */
rab_ent[han].rab$l_rop = 0;
rab_ent[han].rab$l_rop |= lock;        /* Lock option */
if(lock != 0)                          /* If we locked it */
   locked[han] = 1;                    /* Set the flag */
rab_ent[han].rab$l_rop |= RAB$M_WAT;   /* Wait if record is locked */

rab_ent[han].rab$l_rbf = kfb->_record;       /* Record buffer (output) */
rab_ent[han].rab$w_rsz = kfb->_record_len;   /* Output buffer len */
rab_ent[han].rab$l_ubf = kfb->_record;       /* Input Record Buf */
rab_ent[han].rab$w_usz = kfb->_record_len;   /* Input Buf Size */

sys$get(&rab_ent[han]);                /* Get the record */
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
return(rab_ent[han].rab$l_sts);
}

/*----
VAX_START()
Main start routine.  Input is kfb and what type of start (mode0
----*/
vax_start(kfb, mode)
   KCSIO_BLOCK *kfb;
   int   mode;
{
register int   han;                    /* File handle or index into FABs */
register int   idx;                    /* XABKEY index */
register char  *buf;                   /* Record buffer */
int   err_code;

han = kfb->_io_channel;                /* Get FAB index */
idx = kfb->_io_key;                    /* Key of Ref */

if(locked[han] != 0)                   /* If last operation locked it */
   {
   sys$free(&rab_ent[han]);            /* Free last record locked */
   locked[han] = 0;                    /* Clear flag */
   }

rab_ent[han].rab$b_krf = idx;          /* Set in RAB */
buf = kfb->_record;                    /* Point to record buffer */
buf += xab_key_ent[han][idx].xab$w_pos0;  /* Offset of key */
rab_ent[han].rab$l_kbf = buf;          /* Set RAB key buffer */
rab_ent[han].rab$b_ksz = xab_key_ent[han][idx].xab$b_siz0;
                                       /* Size of key */
rab_ent[han].rab$b_rac = RAB$C_KEY;    /* Keyed access */
rab_ent[han].rab$l_rop = 0;            /* Clear previous options */

               /* Set the start mode. Zero shouel be key equals */
if(mode == F_NOT_LESS)
   rab_ent[han].rab$l_rop |= RAB$M_KGE;   /* NLT bits */
else if(mode == F_GREATER)
   rab_ent[han].rab$l_rop |= RAB$M_KGT;   /* NLT bits */

rab_ent[han].rab$l_rop |= RAB$M_NLK;   /* Don't lock any rec found */

sys$find(&rab_ent[han]);               /* Find the record */
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
return(rab_ent[han].rab$l_sts);
}

/*---
VAX_WRITE()
Function to write a record to an indexed file.
----*/
vax_write(kfb)
   KCSIO_BLOCK *kfb;
{
register int   han;                    /* File handle or index into FABs */
register int   idx;                    /* XABKEY index */
register char  *buf;                   /* Record buffer */
int   err_code;

han = kfb->_io_channel;                /* Get FAB index */
idx = kfb->_io_key;                    /* Key of Ref */

if(locked[han] != 0)                   /* If last operation locked it */
   {
   sys$free(&rab_ent[han]);            /* Free last record locked */
   locked[han] = 0;                    /* Clear flag */
   }

/*----
NOTE: The RAB Key of Ref and Key Size fields are only used on a write
for a Relative in the KEY access mode.
----*/
rab_ent[han].rab$b_rac = RAB$C_KEY;    /* Keyed access */
rab_ent[han].rab$l_rop = 0;            /* Key equal only */
rab_ent[han].rab$l_rbf = kfb->_record; /* Record buffer (output */
rab_ent[han].rab$w_rsz = kfb->_record_len; /* Output buffer len */
rab_ent[han].rab$l_ubf = kfb->_record;    /* Input Record Buf */
rab_ent[han].rab$w_usz = kfb->_record_len; /* Input Buf Size */

sys$put(&rab_ent[han]);
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
return(rab_ent[han].rab$l_sts);
}

/*----
VAX_REWRITE()
Function to Update a record.
----*/
vax_rewrite(kfb)
   KCSIO_BLOCK *kfb;
{
register int   han;                    /* File handle or index into FABs */
int   err_code;

han = kfb->_io_channel;                /* Get FAB index */

rab_ent[han].rab$l_rop = 0;            /* Key equal only */
rab_ent[han].rab$l_rbf = kfb->_record; /* Record buffer (output */
rab_ent[han].rab$w_rsz = kfb->_record_len; /* Output buffer len */

sys$update(&rab_ent[han]);
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */

/* The UPDATE service may not unlock the held record even in the event
   that there is an error.  We already got the return code, so now we
   unlock the record just to be safe.
*/
sys$free(&rab_ent[han]);            /* Free last record locked */
locked[han] = 0;                    /* Clear flag */

return(err_code);
}

/*----
VAX_DELELTE()
----*/
vax_delete(kfb)
   KCSIO_BLOCK *kfb;
{
register int   han;                    /* File handle or index into FABs */
int   err_code;

han = kfb->_io_channel;                /* Get FAB index */

rab_ent[han].rab$l_rop = 0;            /* Key equal only */

sys$delete(&rab_ent[han]);
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
return(rab_ent[han].rab$l_sts);
}



/*----
                                UTILITY ROUTINES
----*/
/*----
FIND_FAB - return an index into FAB array of unused FAB
*/
int find_fab()
{
register int idx;

for(idx = 0; idx < MAXFILES; idx++)
   {
   if(in_use[idx] == 0)
      break;
   }

if(idx >= MAXFILES)
   return(-1);
in_use[idx] = 1;
return(idx);
}

/*----
INIT_FAB()
Init the FAB, RAB and all XABKEYs for this handle
----*/
init_fab(handle)
   int   handle;
{
register int han;                      /* Register for speed */
register int idx;                      /* Index to XABKEYs */

han = handle;

memset(&fab_ent[han], 0, sizeof(KFAB)); /* Clear out data structures */
memset(&rab_ent[han], 0, sizeof(KRAB));
for(idx = 0; idx < MAXKEYS; idx++)
   memset(&xab_key_ent[han][idx], 0, sizeof(KXABKEY));
locked[han] = 0;                       /* No records locked yet */

                          /* Init memory to FAB type */

fab_ent[han].fab$b_bid = FAB$C_BID;    /* Identify block as FAB type */
fab_ent[han].fab$b_bln = FAB$C_BLN;    /* Defines length of a FAB */
fab_ent[han].fab$l_xab = &xab_key_ent[han];   /* Point to this FAB's 1st XAB */

                            /* Do the same for RAB */

rab_ent[han].rab$b_bid = RAB$C_BID; /* Identify block as a RAB */
rab_ent[han].rab$b_bln = RAB$C_BLN; /* Defines length of a RAB */
rab_ent[han].rab$l_fab = &fab_ent[han]; /* Address of corresponding FAB */

                  /* Define all of the XAB's as XABKEY types */
for(idx = 0; idx < MAXKEYS; idx++)
   {
   xab_key_ent[han][idx].xab$b_bln = XAB$C_KEYLEN;
   xab_key_ent[han][idx].xab$b_cod = XAB$C_KEY;
   }
}

/*----
LINK_XAB_KEYS()
Funciont to link all of the XABKEYs to this FAB.
The First XABKEY should already have been linked to the FAB by the init
routine.
----*/
static link_xab_keys(han)
   int   han;
{
register int idx;                      /* Use registers for speed */
register int idx1;                     /* XABKEYS are 2 dim array */
register KXABKEY *xp;

idx = han;                          /* File handle or index into FABs */
xp = &xab_key_ent[idx][0];

fab_ent[idx].fab$l_xab = xp;  /* Set XABKEY address in FAB again to be sure */
xp++;                                  /* Set to address of next XAB */
for(idx1 = 0; idx1 < MAXKEYS; idx1++)
   {
   xab_key_ent[idx][idx1].xab$b_ref = idx1;  /* Key of reference Number */
   xab_key_ent[idx][idx1].xab$l_nxt = xp; /* Link to next XABKEY */
   xp++;                               /* Next forward XABKEY */
   }
xab_key_ent[idx][MAXKEYS-1].xab$l_nxt = 0;
}

/*----
UNLINK_XAB_KEYS()
This function unlinks unnecessary XABKEYS from the chain.  The first XABKEY
for this FAB that has no key size is unnecessary as there is no key for it
----*/
unlink_xab_keys(han)
   int han;
{
	int idx;

	for(idx = 0;
		(idx < MAXKEYS) && (xab_key_ent[han][idx].xab$b_siz0 != 0);
		++idx)
		;
	if(idx > 0)               /* If not the last possible key */
		{
		--idx;
   		xab_key_ent[han][idx].xab$l_nxt = 0;  /* Break the chain */
		}
}


#ifdef XXXX
/*----
MEMSET()
Function to set memory to specific byte value
----*/
memset(ptr,fill,len)
   char *ptr;
   char fill;
   int len;
{
register char *p;                      /* Use registers for speed */
register char f;
register int  l;

p = ptr;
f = fill;
l = len;

while(l)
   {
   *p++ = f;
   l--;
   }
}
#endif

/*----
VAX_TRANS()
Translate error status from RMS to something KCSI routines understand
----*/

vax_trans(code)
   int code;
{
switch(code)
   {
   case 0:
      return(0);
   case RMS$_SUC:
      return(0);
   case RMS$_OK_DUP:
      return(0);
   case RMS$_RLK:
      return(ELOCKED);
   case RMS$_OK_ALK:
   case RMS$_OK_RLK:
      return(0);
   case RMS$_FUL:
   case RMS$_SYS:
   case RMS$_RER:
      return(f_errno);
   case RMS$_ACT:                      /* File activity precludes operation */
   case RMS$_FAC:                      /* Op not allowed by FAB$B_FAC */
   case RMS$_FOP:                      /* Invalid File Proc Options */
   case RMS$_SHR:                      /* Attempt to share Seq file */
   case RMS$_SQO:                      /* Attempted Random access of seq file */
      return(ENOTEXCL);
   case RMS$_TRE:                      /* Index tree error */
   case RMS$_ATR:                      /* File Header Read error */
   case RMS$_ATW:                      /* Write error on file header */
   case RMS$_CHK:                      /* Bucket corrupted */
   case RMS$_IRC:                      /* Invalid Record Encountered */
   case RMS$_ORG:                      /* Unknown File Org */
   case RMS$_PLG:                      /* Prolog error.  File is corrupt */
   case RMS$_PLV:                      /* Prolog version unsupported */
   case RMS$_WPL:                      /* Prolog write error */
      return(EBADFILE);
   case RMS$_DUP:                      /* Dup key not allowed */
   case RMS$_REX:                      /* Record Exists */
      return(EDUPL);
   case RMS$_RNF:                      /* Rec Not Found */
      return(ENOREC);
   case RMS$_EOF:
      return(EENDFILE);                /* End Of File */
   case RMS$_DME:                      /* Dynamic Memory Exhausted */
      return(EBADMEM);
   default:
      return(EBADFILE);
   }
}

vax_trans2(code)
   int   code;
{
if(code == RMS$_RNF)
   return(EENDFILE);
else
   return(vax_trans(code));
}


/*----
table_info is same as file_info below
------*/
ksam_table_info(kfb)
KCSIO_BLOCK *kfb;
{
	ksam_file_info(kfb);
}


/*----
KSAM_FILE_INFO()
Get the data from RMS open service and fill in the KCSIO_BLOCK
----*/
ksam_file_info(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;
register int idx;
int   ak;

han = kfb->_io_channel;
kfb->_record_len = fab_ent[han].fab$w_mrs;   /* Max Rec Size */
kfb->_altkey_count = vax_count_alts(han);
clear_keys(kfb);

/* Get the primary key structure */
kfb->_key[0].k_part[0].kp_leng = xab_key_ent[han][0].xab$b_siz0;
kfb->_key[0].k_part[0].kp_start = xab_key_ent[han][0].xab$w_pos0;
kfb->_key[0].k_nparts = 1;

for(ak = 0, idx = 1; ak < kfb->_altkey_count; ++ak, idx++)
   {
   if(xab_key_ent[han][idx].xab$b_flg | XAB$M_DUP)  /* Dups allowed */
      kfb->_key[ak + 1].k_flags = ISDUPS;
   else
      kfb->_key[ak + 1].k_flags = ISNODUPS;
   kfb->_key[ak + 1].k_part[0].kp_leng = xab_key_ent[han][idx].xab$b_siz0;
   kfb->_key[ak + 1].k_part[0].kp_start = xab_key_ent[han][idx].xab$w_pos0;
   kfb->_key[0].k_nparts = 1;
   }
}



/*----
VAX_COUNT_ALTS
Routine to cound the alt keys when given an index into FAB array
----*/

vax_count_alts(han)
   int   han;
{
register int idx;
register int count;

idx = han;

for(count = 0; count < MAXKEYS; count++)
   {
   if(xab_key_ent[idx][count].xab$l_nxt == 0)
      break;
   }
return(count);
}

/*----
KSAM_FILE_SPACE()
Just a dummy to allow a linke to vcsio
----*/
ksam_file_space(kfb)
   KCSIO_BLOCK *kfb;
{
kfb->_space = 1;
}

ksam_read_previous(kfb)
KCSIO_BLOCK *kfb;
{
	kfb->_status = EBADARG;
}

/*
**	History:
**	$Log: kvxi.c,v $
**	Revision 1.3  1996-09-17 19:45:42-04  gsl
**	drcs update
**
**
**
*/
