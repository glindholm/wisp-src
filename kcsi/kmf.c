/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
**
**
** 
** CVS
**
**
**
**
******************************************************************************
*/


#ifdef KCSI_MFX

/*---
	kmf.c

	IO for Micro Focus COBOL isam files using ExtFH.

------*/

/*
**	Includes
*/
#include <stdio.h>
#include <ctype.h>

#include "kcsifunc.h"
#include "kcsit.h"
#include "kcsio.h"
#include "mffcd.h"
#include "kcsimem.h"
#include "assert.h"

/*
**      Structures and Defines
*/
#define UNKNOWN_FT	0xFF
#define NORMAL_COBOL_IO	0xFA

#define FCDMAGIC	-111111

#define ASSERTVALIDFCD( fcd )		ASSERT( fcd && fcd->magic == FCDMAGIC )

/*
**      Globals and Externals
*/
extern int cobinit (void);
extern int KCSEXTFH();

/*
**      Static data
*/

/*
**      Function Prototypes
*/
static FCD* set_mfio(KFB* kfb);
static void call_ixfile(int io_op_type, int io, KFB* kfb);
static void do_open(KFB* kfb, int mode);
static FCD* fcd_new();
static char* mfkblock_new();
static void init_for_open(KFB* kfb);
static void init_for_open_output(KFB* kfb);
static void release_fcd(KFB* kfb);
static void store_number(char* dest, void* src, int len);
static void retrieve_number(char* src, void* dest, int len);
static void local_file_info(KFB* kfb);
static void mf_x_status(KFB* kfb, FCD* user_fcd);
static int2 mf_trans(FCD* user_fcd);
static void set_local_key_info(KFB* kfb);
static void do_start(KFB* kfb, int mode);

void ksam_init()
{
	static int first = 1;

	if (first)
	{
		/* No init code needed */
		first = 0;
	}
}

/*
**      Routine:        call_ixfile()
**
**      Function:	Perform the ExtFH call using the given paramters
**
**      Description:
**
**      Arguments:
**      io_op_type      Indicates the IO type - normal or special
**      io		Operation code to direct ExtFH to perform
**	kfb		Current file KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
static void call_ixfile(int io_op_type, int io, KFB* kfb)
{
	FCD* user_fcd;
	char op_code[2];
	static int first = 1;
	
	if (first)
	{
		cobinit();
		first = 0;
	}
	
	user_fcd = set_mfio( kfb );
	op_code[0] = io_op_type;
	op_code[1] = io;
	kcsitrace(2, "kmf:call_ixfile()", "info", "Call ExtFH: op code = [%x], filename = [%s]", io,kfb->_sys_name);
	KCSEXTFH( op_code, user_fcd, kfb->_record, kfb->_sys_name, kfb->_mfkblock );

	kfb->_status = mf_trans( user_fcd );
	mf_x_status(kfb, user_fcd);
}

/*
**      Routine:        ksam_file_space()
**
**      Function:	Extract the root file data and move in the space.
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/07/97        Written by SMC
**
*/
void ksam_file_space(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_file_space()", "enter", "%s", kfb->_sys_name);

	kfb->_space = 1;
}

/*----
            <<<<<<<<<  OPEN AND CLOSE ROUTINES   >>>>>>>>>>>
------*/
/*---
Shared is the same as IO for C-ISAM. All C-ISAM opens to be compatible
with COBOL require that the file be set up at the first record.
A COBOL open sets things up for the first record. A c-isam open
does not.
------*/

/*
**      Routine:        ksam_open_shared()
**
**      Function:	Open a file shared
**
**      Description:	Open a file with Operation Code of MFIO_OPEN_IO
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_open_shared(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_open_shared()", "enter", "%s", kfb->_sys_name);
	ksam_open_io( kfb );
}

/*
**      Routine:        ksam_open_input()
**
**      Function:	Open a file as INPUT only
**
**      Description:	Open a file with Operation Code of MFIO_OPEN_INPUT
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_open_input(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_open_input()", "enter", "%s", kfb->_sys_name);
	do_open(kfb, MFIO_OPEN_INPUT);
}

/*
**      Routine:        ksam_open_io()
**
**	Function:	Open a file as INPUT/OUTPUT
**
**      Description:	Open a file with Operation Code of MFIO_OPEN_IO
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_open_io(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_open_io()", "enter", "%s", kfb->_sys_name);
	do_open(kfb, MFIO_OPEN_IO);
}

/*
**      Routine:        fcd_new()
**
**      Function:	Will allocate and initialize a File Control Description (FCD) area.
**
**      Description:	Get the memory required for the structues and then initialize
**			the required pieces as documented to binary zeroes (NULL).
**
**      Arguments:	None
**
**      Globals:        None
**
**      Return:		Pointer to the FCD
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
static FCD* fcd_new()
{
	FCD* l_new;

	l_new = (FCD*) MALLOCT( FCD );
	memset(l_new, 0x00, sizeof(FCD));

#ifdef DEBUG
	l_new->magic = FCDMAGIC;
	ASSERT( (char *) &l_new->magic - (char *) l_new == 100 );
#endif

	return( l_new );
}

/*
**      Routine:        mfkblock_new()
**
**      Function:	Will allocate and initialize a Key Definition Block.
**
**      Description:	Get the memory required for the structues and then initialize
**			the required pieces as documented to binary zeroes (NULL).
**
**      Arguments:
**
**      Globals:        None
**
**      Return:		Pointer to the array of chars (char*)
**
**      Warnings:       None
**
**      History:
**      08/04/97        Written by SMC
**
*/
static char* mfkblock_new()
{
	char* l_new;
	int2 size;

/*
		Commpute the largest possible length of the Key Definition Block:
		Add  the length of the GIA, then add the Length of the KDA multiplied
		by the max number of keys, then add the length of the CDA multiplied
		by the max number of components for a key, multiplied by the max numbr
		of keys.
*/
	size = LENGIA + (LENKDA * WANG_MAX_KEYS) + (LENCDA * NPARTS * WANG_MAX_KEYS );
	l_new = (char*) MALLOCA( char, size );
	memset(l_new, 0x00, size);

	return( l_new );
}

/*
**      Routine:        release_fcd()
**
**      Function:	Free up the memory that was allocated for the FCD
**
**      Description:	Free up all pieces of memory associated with the FCD
**
**      Arguments:
**      kfb		current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/03/97        Written by SMC
**
*/
static void release_fcd(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:release_fcd()", "enter", "%s", kfb->_sys_name);

	FREE( kfb->_mfkblock );
	kfb->_mfkblock = NULL;
	user_fcd = (FCD*) kfb->_fcd;
	memset(user_fcd->filename,'\0',4);
	memset(user_fcd->mfkblock,'\0',4);
	FREE( user_fcd );
	user_fcd = NULL;
}

/*
**      Routine:        do_open()
**
**      Function:	Perform the open on of a file
**
**      Description:	Has defined in mode the open mode, opens a file
**
**      Arguments:
**      mode		operation code defining I/O operation on file
**	kfb		current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       do_open() Should NOT be used for OPEN OUTPUT
**
**      History:
**      08/01/97        Written by MoB
**
*/
static void do_open(KFB* kfb, int mode)
{
	FCD* user_fcd;
	
	kcsitrace(1, "kmf:do_open()", "enter", "mode=%d %s", mode, kfb->_sys_name);

	if ( mode == MF_OPEN_INPUT ||		
	     mode == MF_OPEN_OUTPUT ||
	     mode == MF_OPEN_IO ||
	     mode == MF_OPEN_EXTEND )
	{
		/* OK */
	}
	else
	{
		kcsitrace(4, "kmf:do_open()", "error", "Invalid OPEN mode [%d]", mode);
	}

	/* Init the FCD */
	user_fcd = fcd_new();
	user_fcd->org = UNKNOWN_FT;
	kfb->_fcd = (char*) user_fcd;
	init_for_open( kfb );

	/* Get the file info */
	local_file_info( kfb );

	user_fcd->access_mode |= MF_DYNAMIC_ACCESS;

	if (mode == MF_OPEN_INPUT)
	{
		/* If INPUT then don't care about locked records. */
		user_fcd->flags |= MF_NODETECTLOCK;		
	}	

	call_ixfile(NORMAL_COBOL_IO, mode, kfb);
	kfb->_open_status = 1;
}

/*
**      Routine:        init_for_open_output()
**
**      Function:
**
**      Description:
**
**      Arguments:
**      kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
static void init_for_open_output(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:init_for_open_output()", "enter", "%s", kfb->_sys_name);

	user_fcd = fcd_new();

	if ( kfb->_org[0] == 'I' )
	{
		user_fcd->org = MF_INDEXED;
	}
	else if ( kfb->_org[0] == 'R' )
	{
		user_fcd->org = MF_RELATIVE;
	}
	else if ( kfb->_org[0] == 'C' )
	{
		user_fcd->org = MF_SEQUENTIAL;
	}
	else
	{
		kcsitrace(3, "kmf:init_for_open_output()", "error", "Invalid file organization");
	}

	user_fcd->format = kfb->_format;

	kfb->_fcd = (char*) user_fcd;
	init_for_open( kfb );
}

/*
**      Routine:        init_for_open()
**
**      Function:
**
**      Description:
**
**      Arguments:
**      kfb
**	user_fcd
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
static void init_for_open(KFB* kfb)
{
	FCD* user_fcd;
	int2 len;

	kcsitrace(1, "kmf:init_for_open()", "enter", "%s", kfb->_sys_name);

	user_fcd = (FCD*) kfb->_fcd;
	user_fcd->access_mode = MF_USER_STATUS;
	user_fcd->open_mode = MF_CLOSED;
	len = strlen( kfb->_sys_name );
	store_number(user_fcd->namelen, &len, 2);
	user_fcd->lockmode = MF_LOCK_MANUAL;
	user_fcd->flags = MF_NOT_OPTIONAL;
	user_fcd->ans_flag = MF_ANS85;
	store_number(user_fcd->maxreclen, &kfb->_record_len, 2);
	user_fcd->recording_mode = MF_FIXED_LENGTH;
	store_number(user_fcd->curreclen, &kfb->_record_len, 2);
	store_number(user_fcd->minreclen, &kfb->_record_len, 2);
	kfb->_mfkblock = mfkblock_new( kfb );
}

/*
**      Routine:        ksam_unlock()
**
**      Function:
**
**      Description:
**
**      Arguments:
**	kfb
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_unlock(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_unlock()", "enter", "%s", kfb->_sys_name);

	/* nothing to do */
}
	
/*
**      Routine:        ksam_open_output()
**
**      Function:	Set the appropriate fields for doing an OPEN of a file
**			as OUTPUT
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_open_output(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_open_output()", "enter", "%s", kfb->_sys_name);

	init_for_open_output( kfb );
	set_local_key_info( kfb );
	call_ixfile(NORMAL_COBOL_IO, MFIO_OPEN_OUTPUT, kfb);
	kfb->_open_status = 1;
}

/*
**      Routine:        ksam_close()
**
**      Function:	Close an open file
**
**      Description:	Close a file and then release any memory associated with
**			data structures defined for the file.
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_close(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_close()", "enter", "%s", kfb->_sys_name);

	/*
	 *  If the file is already closed, then just return.
	 */
	if( kfb->_open_status == 0 )
	{
		return;
	}

	call_ixfile(NORMAL_COBOL_IO, MFIO_CLOSE, kfb);

	if (kfb->_status == ENOTOPEN)
	{
		/*
		 *	File was already closed so just reset the status code to 0.
		 *
		 *	MFOC 4.1 will close a file when you do a ksam_file_info().
		 */
		kcsitrace(3, "kmf:ksam_close()", "NOTOPEN", "File not open %s", kfb->_sys_name);
		kfb->_status = 0;
	}
	
	kfb->_open_status = 0;
	release_fcd( kfb );
}

/*
**      Routine:        ksam_read()
**
**      Function:	Read a record from a file
**
**      Description:	Set the io_key to indicate the key of reference, used 
**			for random READ operations.
**
**			0 = specifies the primary key
**			1 = specifies the first alternate key
**			2 = specifies the second alternate key
**			etc ...
**
**      		NOTE: A read record request assumes the primary key
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_read(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_read()", "enter", "%s", kfb->_sys_name);

	kfb->_io_key = 0;			/* Set to read along the primary key path */
	ksam_read_keyed( kfb );
}

/*
**      Routine:        ksam_hold()
**
**      Function:	Read a record from a file and lock the record
**
**      Description:	Set the io_key to indicate the key of reference, used 
**			for random READ operations.
**
**			0 = specifies the primary key
**			1 = specifies the first alternate key
**			2 = specifies the second alternate key
**			etc ...
**
**      		NOTE: A read record request assumes the primary key
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_hold(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_hold()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;		/* Set to read along the primary key path */
	ksam_hold_keyed( kfb );
}

/*
**      Routine:        set_mfio()
**
**      Function:	Will assign the current FCD for the file so
**			will be using the same FCD for every I/O operation.
**
**      Description:
**
**      Arguments:
**	kfb
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
static FCD* set_mfio(KFB* kfb)
{
	FCD* user_fcd;

	user_fcd = (FCD*) kfb->_fcd;
#ifdef DEBUG
	ASSERTVALIDFCD( user_fcd );
#endif
	return( user_fcd );
}

/*
**      Routine:        ksam_read_next()
**
**      Function:	Read the next record in a file
**
**      Description:	Read the file sequentially so retrieves the next record
**			in the file
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_read_next(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_read_next()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	call_ixfile(NORMAL_COBOL_IO, MFIO_SEQ_READ_NO_LOCK, kfb); 

}

/*
**      Routine:        ksam_read_previous()
**
**      Function:	Read the previous record in the file
**
**      Description:	Reads the file sequentially but backward so gets the
**			previous record in the file
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_read_previous(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_read_previous()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	call_ixfile(NORMAL_COBOL_IO, MFIO_READ_PREV_NO_LOCK, kfb);
}

/*
**      Routine:        ksam_hold_next()
**
**      Function:	Reads the next record in the file and locks it
**
**      Description:
**
**      Arguments:	Current KCSIO_BLOCK
**	kfb
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_hold_next(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_hold_next()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	call_ixfile(NORMAL_COBOL_IO, MFIO_SEQ_READ_LOCK, kfb);
}

/*
**      Routine:        ksam_read_keyed()
**
**      Function:	Reads the record in the file based on the key set
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_read_keyed(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_read_keyed()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	store_number(user_fcd->keynum, &kfb->_io_key, 2);	/* Set so specifies prime key */
	call_ixfile(NORMAL_COBOL_IO, MFIO_RNDM_READ, kfb);
}

/*
**      Routine:        ksam_hold_keyed()
**
**      Function:	Read the record in the file based on the key set and
**			locks the record
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_hold_keyed(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_hold_kyed()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	store_number(user_fcd->keynum, &kfb->_io_key, 2);
	call_ixfile(NORMAL_COBOL_IO, MFIO_RNDM_READ_LOCK, kfb);
}

/*
**      Routine:        ksam_start_eq()
**
**      Function:	Issues a START on a file with key equal to full length
**			primary key
**
**      Description:	Positions the record pointer based on the key
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_start_eq(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_start_eq()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	do_start(kfb, MFIO_START_EQ);
}

/*
**      Routine:        ksam_start_nlt()
**
**      Function:	Issues a START on a file with key not less than key set
**
**      Description:	Positions the record pointer based on the key
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_start_nlt(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_start_nlt()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	do_start(kfb, MFIO_START_NLT);
}

/*
**      Routine:        ksam_start_gt()
**
**      Function:	Issues a START on a file with key greater than key set
**
**      Description:	Positions the record pointer based on the key
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_start_gt(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_start_gt()", "enter", "%s", kfb->_sys_name);
	kfb->_io_key = 0;
	do_start(kfb, MFIO_START_GT);
}

/*
**      Routine:        ksam_start_eq_keyed()
**
**      Function:	Issues a START on a file with key equal to any key/record number
**
**      Description:	Positions the record pointer based on the key
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_start_eq_keyed(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_start_eq_keyed()", "enter", "%s", kfb->_sys_name);
	do_start(kfb, MFIO_START_EQ);
}

/*
**      Routine:        ksam_start_nlt_keyed()
**
**      Function:	Issues a START on a file with key not less than key/record number
**
**      Description:	Positions the record pointer based on the key
**
**      Arguments:
**	kfb		Current KCSI_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_start_nlt_keyed(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_start_nlt_keyed()", "enter", "%s", kfb->_sys_name);
	do_start(kfb, MFIO_START_NLT);
}

/*
**      Routine:        ksam_start_gt_keyed()
**
**      Function:	Issues a START on a file with key greater than key/record number
**
**      Description:	Positions the record pointer based on the key
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_start_gt_keyed(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_start_gt_keyed()", "enter", "%s", kfb->_sys_name);
	do_start(kfb, MFIO_START_NLT);
}

/*
**      Routine:        ksam_start_last()
**
**      Function:	Returns an error so not sure why being called ...
**
**      Description:	
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_start_last(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_start_last()", "enter", "%s", kfb->_sys_name);
	kfb->_status = EBADARG;
}

/*
**      Routine:        do_start()
**
**      Function:	Executes a start and sets up a new _last_io_key
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**	mode		operation code for call to ExtFH
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
static void do_start(KFB* kfb, int mode)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:do_start()", "enter", "%s mode=%d", kfb->_sys_name, mode);
	user_fcd = set_mfio( kfb );
	call_ixfile(NORMAL_COBOL_IO, mode, kfb);
}

/*----
               <<<<<<<< WRITE REWRITE DELETE  >>>>>>>>>>>>
------*/
/*----
Write, rewrite and delete are vanilla except that a hold should have
been issued before the rewrite or delete so these are released.
------*/

/*
**      Routine:        ksam_write()
**
**      Function:	Write a record to a file
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_write(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_write()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	call_ixfile(NORMAL_COBOL_IO, MFIO_WRITE, kfb);
}

/*
**      Routine:        ksam_rewrite()
**
**      Function:	Logically replaces a record existing in a disk file
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_rewrite(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_rewrite()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	call_ixfile(NORMAL_COBOL_IO, MFIO_REWRITE, kfb);
}

/*
**      Routine:        ksam_delete()
**
**      Function:	Deletes a record from a file
**
**      Description:
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_delete(KFB* kfb)
{
	FCD* user_fcd;

	kcsitrace(1, "kmf:ksam_delete()", "enter", "%s", kfb->_sys_name);
	user_fcd = set_mfio( kfb );
	call_ixfile(NORMAL_COBOL_IO, MFIO_DELETE, kfb);
}

/*
**      Routine:        ksam_table_info()
**
**      Function:
**
**      Description:
**			Note: This routine assumes that key length = record_length
**			which is the case for table files.
**
**      Arguments:
**	kfb
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_table_info(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_table_info()", "enter", "%s", kfb->_sys_name);
	ksam_file_info( kfb );
}

/*
**      Routine:        ksam_file_info()
**
**      Function:
**
**      Description:
**			The ksam_file_info() routine pulls the data on an open file and
**			sets up the c structure. Under microfocus the key data is invalid.
**
**      Arguments:
**	kfb		current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
void ksam_file_info(KFB* kfb)
{
	kcsitrace(1, "kmf:ksam_file_info()", "enter", "%s", kfb->_sys_name);
	local_file_info( kfb );
}

/*
**      Routine:        local_file_info()
**
**      Function:	Get the information for the specified file
**
**      Description:	Returns information on keys for the specified file
**			in the format of the Key Definition Block for index
**			files and general information for all file types
**			supported.
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/05/97        Written by SMC
**
*/
static void local_file_info(KFB* kfb)
{
	register int	idx;
	static char info_code[2]= {0x00,0x06};	/* Special opcode Returns info on file and keys in KDB */
	MFGIA*	mgia;
	MFCDA*	mcda;
	MFKDA*	mkey;
	FCD*	user_fcd;
	int2	l_nkeys;
	unsigned char hold_open_mode;

	kcsitrace(1, "kmf:local_file_info()", "enter", "%s", kfb->_sys_name);

	user_fcd = set_mfio( kfb );

	/*
         *  Change in functionality from MF COBOL 3.2 to 4.1
	 *  The user_fcd->open_mode gets changed to MF_CLOSED when the
	 *  information on the file is retrieved.  This is not a documented
	 *  action.  To work around this change in behavior, I save the mode
	 *  prior to the call and then set it back to what it was.
         */
	hold_open_mode = user_fcd->open_mode;

	call_ixfile(info_code[0], info_code[1], kfb);
	user_fcd->open_mode = hold_open_mode;
	
	if ( user_fcd->status[0] == 9 && (unsigned char)(user_fcd->status[1]) == (unsigned char)161 )
	{
		kcsitrace(2, "kmf:local_file_info()", "info", "Found a fixed-length sequential file");
		user_fcd->org = MF_SEQUENTIAL;
		call_ixfile(info_code[0], info_code[1], kfb);
      	}
#ifdef DEBUG
	ASSERTVALIDFCD( user_fcd );
#endif

	switch( user_fcd->org )
	{
		default:
		case MF_INDEXED:
		{
			kfb->_org[0] = 'I';
			break;
		}
		case MF_RELATIVE:
		{
			kfb->_org[0] = 'R';
			break;
		}
		case MF_SEQUENTIAL:
		case MF_LINE_SEQUENTIAL:
		{
			kfb->_org[0] = 'C';
			break;
		}
	}	
	
	retrieve_number(user_fcd->maxreclen, &kfb->_record_len, 2);
	KCSI_clear_keys( kfb );
	mgia = (MFGIA*) kfb->_mfkblock;
	retrieve_number(mgia->nkeys, &l_nkeys, 2);
	if ( l_nkeys <= 0 )
	{
		kfb->_altkey_count = 0;
	}
	else
	{
		kfb->_altkey_count = l_nkeys - 1;
	}

	for( idx = 0; idx < l_nkeys; ++idx )
	{
		int2 l_offset;
		int4 rcvr;
/*
			Set pointer to the current key by starting with the pointer to the KDA and
			adding the length of the GIA, then adding the length of the KDA multiplied
			by the current index of the key.
*/
		mkey = (MFKDA*) (((char*)kfb->_mfkblock) +  LENGIA + (LENKDA * idx));
		retrieve_number(mkey->comp_cnt, &kfb->_key[idx].k_nparts, 2);
		retrieve_number(mkey->offset, &l_offset, 2);
/*
			Set pointer to the CDA by starting with the pointer to the KDA and adding
			the length of the GIA, then adding the offset defined in the current key.
*/
		mcda = (MFCDA*) (((char*)kfb->_mfkblock) + l_offset);

		retrieve_number(mcda->len, &rcvr, 4);
		kfb->_key[idx].k_part[0].kp_leng = rcvr;
		retrieve_number(mcda->pos, &rcvr, 4);
		kfb->_key[idx].k_part[0].kp_start = rcvr;
		kfb->_key[idx].k_part[0].kp_type = 0;

		if(	(mkey->dup_flags & MF_IS_PRIMARY) ||
			!(mkey->dup_flags & MF_DUPS_ALLOWED) )
		{
			kfb->_key[idx].k_flags = ISNODUPS;
		}
		else
		{
			kfb->_key[idx].k_flags = ISDUPS;
		}
	}
}

/*
**      Routine:        set_local_key_info()
**
**      Function:	Set the information in the FCD Key Definition Block
**			for the specified file
**
**      Description:	
**
**      Arguments:
**	kfb		Current KCSIO_BLOCK
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/09/97        Written by SMC
**
*/
static void set_local_key_info(KFB* kfb)
{
	register int	idx, jdx;
	FCD*	user_fcd;
	MFGIA*	mgia;
	MFCDA*	mcda;
	MFKDA*	mkey;
	int2	l_nkeys, l_kdb_len, l_offset;

	kcsitrace(1, "kmf:set_local_key_info()", "enter", "%s", kfb->_sys_name);

	if ( kfb->_org[0] != 'I' )
	{
		return;
	} 

	user_fcd = set_mfio( kfb );
#ifdef DEBUG
	ASSERTVALIDFCD( user_fcd );
#endif
	l_nkeys = 0;
/*
		Count the actual number of keys for the file
*/
	for( idx = 0; idx < WANG_MAX_KEYS; ++idx )
	{
		if ( kfb->_key[idx].k_nparts > 0 ) l_nkeys++;
	}

	mgia = (MFGIA*) kfb->_mfkblock;
	store_number(mgia->nkeys, &l_nkeys, 2);
	l_kdb_len = l_offset = LENGIA + (LENKDA * l_nkeys);
 
	for ( idx = 0; idx < l_nkeys; idx++)
	{
		int2 l_numparts;
/*
			Set pointer to the current key by starting with the ptr to the GIA and
			adding the length of the GIA, then adding the length of the KDA multiplied
			by the current index of the key.
*/
		mkey = (MFKDA*) (((char*)mgia) + LENGIA + (idx * LENKDA));

		l_numparts = kfb->_key[idx].k_nparts;
		store_number(mkey->comp_cnt, &l_numparts, 2);
		store_number(mkey->offset, &l_offset, 2);
		if ( kfb->_key[idx].k_flags == ISDUPS )
		{
			mkey->dup_flags |= MF_DUPS_ALLOWED;
		}

		if ( idx == 0 )
		{
			mkey->dup_flags |= MF_IS_PRIMARY;
		}

		for ( jdx = 0; jdx < l_numparts; jdx++ )
		{
			int4 l_len, l_start;
/*
			Set pointer to the CDA by starting with the ptr to the GIA and adding
			the length of the GIA, then adding the offset defined in the key from
			above, then adding the length of the CDA multiplied
			by the current index of the component.
*/
			mcda = (MFCDA*)(((char*)mgia) + l_offset + (jdx * LENCDA));

			l_start = kfb->_key[idx].k_part[jdx].kp_start;
			store_number( mcda->pos, &l_start, 4);
			l_len = kfb->_key[idx].k_part[jdx].kp_leng;
			store_number( mcda->len, &l_len, 4);
		}
		l_offset += (l_numparts * LENCDA);
		l_kdb_len += (l_numparts * LENCDA); 
	}
	store_number(mgia->kdb_len, &l_kdb_len, 2);
}

/*
**      Routine:        mf_trans()
**
**      Function:	Check the status key and convert to a defined error
**			and return the defined error to the kcsi block data item.
**
**      Description:
**
**      Arguments:
**	user_fcd	current FCD for the file
**
**      Globals:        None
**
**      Return:		defined error
**
**      Warnings:       None
**
**      History:
**      08/05/97        Written by SMC
**
*/
static int2 mf_trans(FCD* user_fcd)
{
	kcsitrace(1, "kmf:mf_trans()", "enter", "File status = [%c%c] (hex=%x,%x)", 
		  user_fcd->status[0], user_fcd->status[1], 
		  user_fcd->status[0], user_fcd->status[1]);

	switch( user_fcd->status[0] )
	{
		case '0':
			return(0);
		case '1':
			return(EENDFILE);
		case '2':
		{
			switch( user_fcd->status[1] )
			{
				default:
				case '4':
				case '1':
					return(EBADKEY);
				case '2':
					return(EDUPL);
				case '3':
					return(ENOREC);
			}
		}
		case '3':
		{
			switch( user_fcd->status[1] )
			{
				default:
				case '0':
				case '4':
				case '5':
					return(EBADFILE);
				case '7':
					return(EBADARG);
				case '8':
					return(EFLOCKED);
			}
		}
		case '4':
		{
			switch( user_fcd->status[1] )
			{
				default:
					return(EBADARG);
				case '2':
					return(ENOTOPEN);
				case '4':
					return(EBADFILE);		
			}
		}
		case '9':
		{
			switch( user_fcd->status[1] )
			{
			case 67: /* 9/067 - 9C */
				return(ENOTOPEN);
			case 68: /* 9/068 - 9D */
				return(ELOCKED);
			default:
				return(EBADFILE);
			}
		}
		default:
		{
			return(EBADFILE);
		}
	}
}

/*
**      Routine:        mf_x_status()
**
**      Function:	Copy the actual status into the kfb extended status
**
**      Description:
**
**      Arguments:
**      kfb		Current KCSIO_BLOCK
**	user_fcd	current FCD for the file
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/01/97        Written by MoB
**
*/
static void mf_x_status(KFB* kfb, FCD* user_fcd)
{
	if(user_fcd->status[0] != '9')
	{
		sprintf(kfb->_x_status,"%c%c",
			user_fcd->status[0],
			user_fcd->status[1]);
	}
	else
	{
		sprintf(kfb->_x_status,"%c/%03d",
			user_fcd->status[0],
			user_fcd->status[1]);
	}

	kcsitrace(2, "kmf:mf_x_status()", "info", "Set KFB->_x_status to [%s]", kfb->_x_status);
}

/*
**      Routine:        store_number()
**
**      Function:	Will do any byte manipulation for Big-endian or 
**			Little-endian storage architecture and store into
**			a character string of specified length.
**
**      Description:    This will store the number in Big-endian format.
**
**      Arguments:
**      src		The integer data item to copy from
**	dest		The character data item to copy to
**	len		The number of characters to copy
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/05/97        Written by SMC
**
*/
static void store_number(char* dest, void* src, int len)
{
	if ( len == 2 || len == 4)
	{
		memcpy(dest, src, len);
	}
	else
	{
		kcsitrace(4,"kmf:store_number()", "error", "Invalid length (%d)",len);
		return;
	}

	if ( !WL_bytenormal() )
	{
		WL_reversebytes(dest, len);
	}
}

/*
**      Routine:        rtrieve_number()
**
**      Function:	Will do any byte manipulation for Big-endian or 
**			Little-endian storage architecture and retrieve a
**			character string into a number data item.
**
**      Description:
**
**      Arguments:
**      src		The character data item to copy from
**	dest		The integer data item to copy to
**	len		The number of characters to copy
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/05/97        Written by MoB
**
*/
static void retrieve_number(char* src, void* dest, int len)
{
	if ( len == 2 || len == 4)
	{
		memcpy(dest, src, len);
		if ( !WL_bytenormal() )
		{
			WL_reversebytes(dest, len);
		}
	}
	else
	{
		kcsitrace(4,"kmf:retrieve_number()", "error", "Invalid length (%d)",len);
	}
}

#endif /* KCSI_MFX */

/*
**	History:
**	$Log: kmf.c,v $
**	Revision 1.34  2003/05/07 17:57:37  gsl
**	-Wall
**	
**	Revision 1.33  2003/05/07 17:52:53  gsl
**	-Wall
**	
**	Revision 1.32  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.31  2002/10/24 14:20:37  gsl
**	Make globals unique
**	
**	Revision 1.30  2002/10/21 16:07:05  gsl
**	Add ksam_init
**	
**	Revision 1.29  2002/10/17 17:56:17  gsl
**	Rename variables new to l_new
**	
**	Revision 1.28  2002/07/25 15:20:26  gsl
**	Globals
**	
**	Revision 1.27  2002/07/10 21:06:25  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.26  2002/04/24 15:35:04  gsl
**	Tracing
**	
**	Revision 1.25  2002-04-23 16:48:26-04  gsl
**	Return 9/067 means file not open (MF4.1)
**	In ksam_close() if get a ENOTOPEN just log message and set status to OK
**
**	Revision 1.24  2002-04-23 13:31:17-04  gsl
**	In do_open() if open for INPUT then set the MF_NODETECTLOCK bit
**	so we will ingore locked records.
**
**	Revision 1.23  2002-04-22 16:57:58-04  gsl
**	Fix status checking for locked record
**	Fixed kcsitrace
**	NOT COMPLETE
**	MF LOCKED RECORD
**
**	Revision 1.22  2002-03-18 17:56:54-05  gsl
**	Fix the logging of status codes to make easier to read.
**
**	Revision 1.21  1999-03-04 19:17:53-05  gsl
**	Fix to call cobinit() before first call to a cobol module.
**	Micro Focus requires this (get an error on AIX but not on HP)
**
**	Revision 1.20  1998-10-13 15:10:00-04  gsl
**	fix signed to unsigned comparison
**
**	Revision 1.19  1998-07-29 18:13:30-04  scass
**	Corrected bub where was not setting the duplicates
**	flag correctly.
**	for DANZAS - Unit4
**
**	Revision 1.18  1998-07-29 11:03:07-04  scass
**	Added logic to test if file is already closed
**	If is then returns otherwise does the close.
**	Was causing a seg. viol. on Digital Unix
**
**	Revision 1.15  1997-10-30 14:37:09-05  scass
**	Made changes to accomodate a 64-bit machine.
**
**	Revision 1.12  1997-10-28 10:37:12-05  scass
**	Fixed some ASSERT stuff which was wrong.
**
**	Revision 1.11  1997-10-02 15:42:06-04  gsl
**	fix warnings
**
**	Revision 1.10  1997-08-13 10:32:29-04  scass
**	Corrected handling of key length and starting position.
**	Removed unneeded code.
**
**	Revision 1.9  1997-08-12 13:40:33-04  scass
**	Added setting of key information for creating a file
**
**	Revision 1.8  1997-08-07 17:45:55-04  scass
**	changed some login on error notification
**	and where to init the FCD
**
**	Revision 1.7  1997-08-07 15:12:49-04  scass
**	Move from wisp/kcsi/crid to wisp/kcsi/common
**
**	Revision 1.6  1997-08-07 14:55:17-04  scass
**	Added comments and some ASSERTS
**
**	Revision 1.5  1997-08-06 18:15:44-04  scass
**	Removed unneeded code.  Logging in for historical purposes
**	Can read a file.  Need to check all actions.
**
**	Revision 1.4  1997-08-06 15:24:22-04  scass
**	Added code to support EXTFH() from Micro Focus COBOL.
**	Will allow use of all supported file formats:
**	IDXFORMAT"3" and IDXFORMAT"4" in particular.
**
**	Revision 1.3  1997-08-01 14:20:10-04  scass
**	changed to use new structures defined in mffcd.h
**
**	Revision 1.2  1996-09-17 19:45:41-04  gsl
**	drcs update
**
*/
