static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*---
vxrelio.c

IO for relative files. Relative files are binary files processed
by open, read and write logic.

------*/
#include <stdio.h>
#include <ctype.h>
/*----
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
----*/
#include rms
#include "vax_rms.h"
#include "vaxiocod.h"
#include "kcsio.h"

static char sccsid[]="@(#)vxrelio.c	1.3 1/31/93";

extern KFAB   fab_ent[MAXFILES];       /* Array of 10 FABs for files */
extern KRAB   rab_ent[MAXFILES];       /* Record area for each file */
extern KXABKEY   xab_key_ent[MAXFILES][MAXKEYS];
                                       /* Set of 17 XABKEYs for each file */
/* If the number of MAXFILES is ever changed, then this has to be changed
   also so that these 2 arrays are initialized to zero.  We do this to
   avoid system dependent initialization in higher level routines.
*/
extern int   in_use[MAXFILES];
extern int   locked[MAXFILES];

extern KFAB   *fab_ptr;
extern KRAB   *rab_ptr;
extern KXABKEY *xab_ptr;

extern int   f_errno;

/*----
            <<<<<<<<<  OPEN AND CLOSE ROUTINES   >>>>>>>>>>>
------*/
/*---
Shared is the same as IO for C-ISAM. All C-ISAM opens to be compatible
with COBOL require that the file be set up at the first record.
A COBOL open sets things up for the first record. A c-isam open
does not.
------*/
rel_open_shared(kfb)
KCSIO_BLOCK *kfb;
{
	rel_open_io(kfb);
}

rel_open_input(kfb)
KCSIO_BLOCK *kfb;
{
	do_open(kfb,Finput);
}

rel_open_io(kfb)
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
register int han;

f_errno = vax_open_rel(kfb, mode);
kfb->_status = vax_trans(f_errno);
f_errno = kfb->_status;
han = kfb->_io_channel;

/* Connect RAB to FAB. Note that this positions to EOF */
if(f_errno == 0)
   sys$connect(&rab_ent[han]);

kfb->_last_io_key = 0;
kfb->_io_key = 0;
kfb->_open_status = 1;
kfb->_rel_key = 0;
rel_file_info(kfb);

/* The $CONNECT service positions to EOF on Rel files. */
if(f_errno == 0)
   {
   f_errno = sys$rewind(&rab_ent[han]);
   kfb->_status = vax_trans(f_errno);
   }
}

/*----
A relative open output.
------*/
rel_open_output(kfb)
   KCSIO_BLOCK *kfb;
{

vax_open_output_rel(kfb);

kfb->_status = vax_trans(f_errno);

if(f_errno)
   return;

kfb->_last_io_key = 0;
kfb->_open_status = 1;
f_errno = sys$rewind(&rab_ent[kfb->_io_channel]);
kfb->_status = vax_trans(f_errno);
kfb->_rel_key = 0;
rel_file_info(kfb);
}

rel_close(kfb)
KCSIO_BLOCK *kfb;
{

kfb->_status = f_errno = 0;
if(kfb->_open_status == 0)
   return;
vax_close(kfb->_io_channel);
kfb->_status = vax_trans(f_errno);
kfb->_open_status = 0;
}
/*----
               <<<<<<<<  READ HOLD  >>>>>>>>>>>
------*/
/*----
A read record request assumes the primary key.
------*/
rel_read(kfb)
KCSIO_BLOCK *kfb;
{
kfb->_io_key = 0;
rel_read_keyed(kfb);
}

rel_hold(kfb)
KCSIO_BLOCK *kfb;
{
kfb->_io_key = 0;
rel_hold_keyed(kfb);
}

/*----
Skips null records. null record is all null for record length
------*/
rel_read_next(kfb)
   KCSIO_BLOCK *kfb;
{

f_errno = vax_rel_read_next(kfb);
f_errno = vax_trans2(f_errno);
kfb->_status = f_errno;

}


rel_read_previous(kfb)
KCSIO_BLOCK *kfb;
{

if(kfb->_rel_key < 2)
   kfb->_status = EENDFILE;
else
   {
   --kfb->_rel_key;
   rel_read_keyed(kfb);
   }
}


/*----
No holding logic for relative files.
------*/
rel_hold_next(kfb)
KCSIO_BLOCK *kfb;
{
	rel_read_next(kfb);
}


rel_read_keyed(kfb)
KCSIO_BLOCK *kfb;
{

f_errno = vax_rel_read_keyed(kfb);
kfb->_status = vax_trans(f_errno);
}

/*----
Seek to the specified relative key.
------*/
static rel_key_seek(kfb)
KCSIO_BLOCK *kfb;
{

vax_rel_key_seek(kfb);

}


/*----
No holding logic for relative files.
------*/
rel_hold_keyed(kfb)
   KCSIO_BLOCK *kfb;
{

rel_read_keyed(kfb);

}

/*----
			<<<< STARTS >>>>
------*/
/*----
Starts are illegal on relative files.
------*/
rel_start_eq(kfb)
   KCSIO_BLOCK *kfb;
{

kfb->_status = EBADARG;

}

rel_start_nlt(kfb)
   KCSIO_BLOCK *kfb;
{

kfb->_status = EBADARG;

}

rel_start_gt(kfb)
   KCSIO_BLOCK *kfb;
{

kfb->_status = EBADARG;

}

rel_start_eq_keyed(kfb)
   KCSIO_BLOCK *kfb;
{

kfb->_status = EBADARG;

}

rel_start_nlt_keyed(kfb)
   KCSIO_BLOCK *kfb;
{

kfb->_status = EBADARG;

}

rel_start_gt_keyed(kfb)
   KCSIO_BLOCK *kfb;
{

kfb->_status = EBADARG;

}

rel_start_last(kfb)
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

kfb->_status = EBADARG;

}

/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete.
1.	Write extends the file the required amount if needed and
	writes. If write is on an existing record, it is overwritten.
2.	Rewrites only work on existing records.
3.	Deletes only work on existing records.
------*/

rel_write(kfb)
   KCSIO_BLOCK *kfb;
{

f_errno = vax_rel_write(kfb);
kfb->_status = vax_trans(f_errno);

}

rel_rewrite(kfb)
KCSIO_BLOCK *kfb;
{

f_errno = vax_rel_rewrite(kfb);
kfb->_status = vax_trans(f_errno);

}

rel_delete(kfb)
KCSIO_BLOCK *kfb;
{

rel_read(kfb);                         /* Read the record first */
if(kfb->_status)
   return;

f_errno = vax_rel_delete(kfb);
kfb->_status = vax_trans(f_errno);

}

/*----
No unlocking needed.
------*/
rel_unlock(kfb)
KCSIO_BLOCK *kfb;
{
/*	kfb->_status = 0; */
}

/*----
The rel_file_info() routine pulls the data on an open file and
sets up the c structure.
------*/
rel_file_info(kfb)
KCSIO_BLOCK *kfb;
{

kfb->_altkey_count = 0;

clear_keys(kfb);

kfb->_record_len = fab_ent[kfb->_io_channel].fab$w_mrs;  /* Record size */
if(!(kfb->_record_len))
   kfb->_record_len = 1;

}

/*----
VAX_OPEN_REL()
Similar to the Indexed open, but for relative files
----*/
int vax_open_rel(kfb,mode)
   KCSIO_BLOCK *kfb;
   int mode;
{
register int han;
int err_code;

han=find_fab();                        /* Get a an unused FAB */
if(han < 0)
   {
   return(EBADFILE);                   /* Used all FAB's.  Shouldn't happen */
   }

kfb->_io_channel = han;
init_fab(han);                         /* Init the FAB/RAB/XAB areas */

fab_ent[han].fab$l_xab = 0;            /* XABKEY chain is not needed */

/*----
Set up the FAB for the actual OPEN
----*/
if(mode == Fio)                        /* If IO mode */
   fab_ent[han].fab$b_fac = (FAB$M_UPD|FAB$M_GET|FAB$M_PUT|FAB$M_DEL);
                                       /* Allows Read, Write, Del & Upd */
else if(mode == Finput)
   fab_ent[han].fab$b_fac = FAB$M_GET;  /* Allows Read only */

fab_ent[han].fab$l_fna = kfb->_sys_name;  /* Point to file spec/name */
fab_ent[han].fab$b_fns = strlen(kfb->_sys_name); /* File Spec size */
/* fab_ent[han].fab$b_shr = FAB$M_SHRUPD;*/   /* Allowing All by other users */
fab_ent[han].fab$b_shr = FAB$M_NIL;    /* No sharing for Rel Files */
err_code = sys$open(&fab_ent[han]);    /* Open the file */


/* Connect the RAB to the FAB and positino to EOF */
return(err_code);
}

/*----
VAX_OPEN_OUTPUT_REL()
----*/
vax_open_output_rel(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;                   /* File handle or index into FAB's */
int err_code;

han = find_fab();                        /* Get a an unused FAB */
if(han < 0)
   {
   return(EBADFILE);                   /* Used all FABs.  Shouldn't happen */
   }

kfb->_io_channel = han;
init_fab(han);                         /* Set up data structures */

fab_ent[han].fab$l_xab = 0;            /* XABKEY chain is not needed */

/*----
Set up the FAB for the actual OPEN
----*/
fab_ent[han].fab$b_fac = FAB$M_PUT;    /* Allows Read, Write, Del & Upd */
fab_ent[han].fab$l_fna = kfb->_sys_name;  /* Point to file spec/name */
fab_ent[han].fab$b_fns = strlen(kfb->_sys_name); /* File Spec size */
fab_ent[han].fab$b_shr = FAB$M_NIL;    /* No other access to file */
fab_ent[han].fab$w_mrs = kfb->_record_len;   /* Rec size */
fab_ent[han].fab$l_mrn = 0;            /* No Max Rec # */
fab_ent[han].fab$b_org = FAB$C_REL;    /* File organization */

fab_ent[han].fab$b_rat = FAB$M_CR;     /* Carriage control */

/* Record attributes */
fab_ent[han].fab$b_rfm = FAB$C_FIX;    /* Record format */
fab_ent[han].fab$b_bks = 0;            /* Let RMS set default bucket size */

f_errno = sys$create(&fab_ent[han]);   /* Do the create */
kfb->_status = vax_trans(f_errno);
f_errno = kfb->_status;
kfb->_last_io_key = 0;
kfb->_open_status = 1;

/* If no error in the file, then establish a record stream for writing */
if(f_errno == 0)
   sys$connect(&rab_ent[han]);
}

/*----
VAX_REL_READ_KEYED()
----*/
vax_rel_read_keyed(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;
int   err_code;

han = kfb->_io_channel;

rab_ent[han].rab$l_kbf = &kfb->_rel_key;  /* Key of Ref is Rel Key # */
rab_ent[han].rab$b_ksz = 4;            /* Rel key is a long word */
rab_ent[han].rab$b_rac = RAB$C_KEY;    /* Keyed access */
rab_ent[han].rab$l_rop = 0;            /* Key equal only */
/* rab_ent[han].rab$l_rop |= RAB$M_NLK;*/   /* No lock on relative files */
rab_ent[han].rab$l_rbf = kfb->_record; /* Record buffer (output */
rab_ent[han].rab$w_rsz = kfb->_record_len; /* Output buffer len */
rab_ent[han].rab$l_ubf = kfb->_record;    /* Input Record Buf */
rab_ent[han].rab$w_usz = kfb->_record_len; /* Input Buf Size */

sys$get(&rab_ent[han]);                /* Get the record */
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
if(err_code == RMS$_SUC)
	kfb->_rel_key = *rab_ent[han].rab$l_kbf;
return(rab_ent[han].rab$l_sts);
}

/*----
VAX_REL_READ_NEXT()
----*/
vax_rel_read_next(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;
int   err_code;

han = kfb->_io_channel;

rab_ent[han].rab$l_kbf = &kfb->_rel_key; /* Key of Ref is Rel Key # */
rab_ent[han].rab$b_ksz = 4;            /* A long word */
rab_ent[han].rab$b_rac = RAB$C_SEQ;    /* Sequential access */
rab_ent[han].rab$l_rop = 0;            /* Clear options */
/* rab_ent[han].rab$l_rop |= RAB$M_NLK;*/  /* No lock on relative files */
rab_ent[han].rab$l_rbf = kfb->_record; /* Record buffer (output) */
rab_ent[han].rab$w_rsz = kfb->_record_len; /* Output buffer len */
rab_ent[han].rab$l_ubf = kfb->_record;    /* Input Record Buf */
rab_ent[han].rab$w_usz = kfb->_record_len; /* Input Buf Size */

sys$get(&rab_ent[han]);                /* Get the record */
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
/* Bucket code set to rel rec # */
kfb->_rel_key = rab_ent[han].rab$l_bkt;
/*
if(err_code == RMS$_SUC)
	++kfb->_rel_key;
*/
return(rab_ent[han].rab$l_sts);
}

/*----
VAX_REL_KEY_SEEK()
This one may not work.  The manual says that the Key of Ref field in the
RAB is only or indexed operations for $FIND operations.  The $GET operation
status is is the Rel Key #.  It may just have been omitted from the manual.
----*/
vax_rel_key_seek(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;
int   err_code;

han = kfb->_io_channel;

rab_ent[han].rab$l_kbf = &kfb->_rel_key;   /* Key of Ref is Rel Key # */
rab_ent[han].rab$b_ksz = 4;            /* A long word */
rab_ent[han].rab$b_rac = RAB$C_KEY;    /* Keyed access */
rab_ent[han].rab$l_rop = 0;            /* Key equal only */
/* rab_ent[han].rab$l_rop |= RAB$M_NLK;*/   /* No lock on relative files */

sys$find(&rab_ent[han]);              /* Locate the record */
err_code = rab_ent[han].rab$l_sts;     /* Get the real error code */
return(rab_ent[han].rab$l_sts);
}

/*----
VAX_REL_WRITE()
----*/
vax_rel_write(kfb)
   KCSIO_BLOCK *kfb;
{
register int   han;
int err_code;

han = kfb->_io_channel;

rab_ent[han].rab$l_kbf = &kfb->_rel_key;  /* Address of the rel key */
rab_ent[han].rab$b_ksz = 4;            /* Size always a long */
rab_ent[han].rab$b_rac = RAB$C_KEY;    /* Just write it anywhere */
rab_ent[han].rab$l_rbf = kfb->_record; /* Point to record */
rab_ent[han].rab$w_rsz = kfb->_record_len;   /* Size of record */

sys$put(&rab_ent[han]);

return(rab_ent[han].rab$l_sts);
}

/*----
VAX_REL_REWRITE()
Do the UPDATE call
----*/
vax_rel_rewrite(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;
int err_code;

han = kfb->_io_channel;

rab_ent[han].rab$l_rbf = kfb->_record; /* Point to record */
rab_ent[han].rab$w_rsz = kfb->_record_len;   /* Size of record */

sys$update(&rab_ent[han]);
return(rab_ent[han].rab$l_sts);
}

/*----
VAX_REL_DELETE()
----*/
vax_rel_delete(kfb)
   KCSIO_BLOCK *kfb;
{
register int han;

han = kfb->_io_channel;

sys$delete(&rab_ent[han]);

return(rab_ent[han].rab$l_sts);
}

/*----
REL_FILE_SPACE()
Dummy to allow a link to vcsio
----*/
rel_file_space(kfb)
   KCSIO_BLOCK *kfb;
{
kfb->_space = 1;
}

/*----
E_TRANS
This is kept here since it was initially defined in this module.
It is only called from VXSEQIO at this time.
----*/
/*
Commented out by ZR 03/14/95 as per MJB.  This code moved into the
vscio.c module.

e_trans(code)
int code;
{
	if(code == 0)
		return(0);
	return(EBADFILE);
}
*/
/*
**	History:
**	$Log: vxrelio.c,v $
**	Revision 1.3  1996-09-17 19:45:54-04  gsl
**	drcs update
**
**
**
*/
