static char copyright[]="Copyright (c) 1995-1998 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wt_locks.c
**
**	Project:	WISP/TRAN
**
**	RCS:		$Source:$
**
**	Purpose:	Record locking
**
*/


#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "wt_locks.h"

/*
**	AUTOMATIC vs MANUAL Record Locking
**
**	The ANSI COBOL's that WISP goes to has two types of record locking,
**	these are AUTOMATIC and MANUAL (ON MULTIPLE RECORDS). 
**	AUTOMATIC mode behaves slightly different then Wang VS, it will unlock a record
**	on any I/O statement for that file including a READ (without HOLD) or a WRITE.
**	MANUAL ON MULTIPLE RECORDS mode will only unlock a record when a CLOSE
**	or an UNLOCK statement is executed.
**	WISP defaults to using automatic for ACUCOBOL and MF (VMS only uses MANUAL).
**	On the Wang a locked record is only released on a DELETE, REWRITE, CLOSE, 
**	FREE ALL, or another READ WITH HOLD.
**	If -M (manual_locking) is specified then we need to do an explicit UNLOCK
**	after a DELETE or a REWRITE in order to properly emulate the Wang.
**	The READ WITH HOLD alway does an UNLOCK and CLOSE and FREE ALL also unlock.
**	For manual_locking we us the WITH LOCK ON MULTIPLE RECORDS clause 
**	even though only one record is ever held, this is done to prevent the
**	READ and WRITE from automaticaly unlocking the record.
*/

static int gen_nolocking_free_all(void);

int gen_unlocks(void)									/* Generate UNLOCK logic for reads.	*/
{
	int i;

	if (!do_locking)
	{
		/* ACUCOBOL does an UNLOCK ALL instead of a PERFORM WISP-FREE-ALL so doesn't need the paragraph */
		if (!acu_cobol)	
		{
			gen_nolocking_free_all();
		}
		return 0;
	}

	tput_blank();
	tput_scomment				("****** LOCK PROCESSING PARAGRAPHS ******");
	tput_line				("       WISP-FREE-ALL.");
	tput_line				("           MOVE SPACES TO WISP-NEW-LOCK-ID.");
	tput_line				("           PERFORM WISP-LOCK-START THRU WISP-LOCK-END.");
	tput_blank();
	
	tput_line				("       WISP-LOCK-START.");
	
	if (acu_cobol)
	{
		if (x4dbfile)
		{
			tput_line		("           COMMIT."); 		/* Unlock all locks.			*/
		}
		else
		{
			tput_line		("           UNLOCK ALL RECORDS."); 	/* Unlock all locks.			*/
		}
	}
	else
	{
		tput_line			("           IF WISP-LOCK-ID = SPACES THEN");
		tput_line			("               GO TO WISP-RECORD-LOCK-CLEANUP.");
		tput_blank();
		for (i=0; i<prog_cnt; i++)						/* Generate the GOTO DEPENDING stuff.	*/
		{									/* Skip if It's a print/sort file.	*/
											/* Or an ACUCOBOL SEQ file.		*/
			tput_line		("           IF WISP-LOCK-ID = \"%s\" THEN",prog_files[i]);
			if ((prog_ftypes[i] & (PRINTER_FILE + SORT_FILE)) ||
			    (acu_cobol && (prog_ftypes[i] & SEQ_FILE)))
			{
				tput_line	("               GO TO WISP-LOCK-END."); /* Do nothing.			*/
			}
			else
			{
				tput_line	("               GO TO WISP-RECORD-UNLOCK-%d.",i+1); 
			}
			tput_blank();
		}
	}

	tput_line	  			("       WISP-RECORD-LOCK-CLEANUP.");
	tput_line	  			("           MOVE WISP-NEW-LOCK-ID TO WISP-LOCK-ID.");
	tput_line	  			("       WISP-LOCK-END.");
	tput_line	  			("           EXIT.");

	if (!acu_cobol)
	{
		for (i=0; i<prog_cnt; i++)						/* Generate the UNLOCK paragraphs.	*/
		{
			if ((prog_ftypes[i] & (PRINTER_FILE + SORT_FILE)) ||		/* Skip it if it's printer/sort file.	*/
			    (acu_cobol && (prog_ftypes[i] & SEQ_FILE)))			/* Or if it's ACUCOBOL & seq file.	*/
				continue;
			tput_blank();
			tput_line		("       WISP-RECORD-UNLOCK-%d.\n",i+1);
			if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
			{
				/*
				**	Don't neet to UNLOCK if AUTOLOCK is used and on same file.
				*/
				tput_line	("           IF WISP-LOCK-ID = WISP-NEW-LOCK-ID THEN");
				tput_line	("               GO TO WISP-RECORD-LOCK-CLEANUP.\n");
			}
			tput_line		("           MOVE %s TO WISP-SAVE-FILE-STATUS.\n",prog_fstats[i]);
			if (vax_cobol)	
				tput_line	("           UNLOCK %s ALL.\n",prog_files[i]);
			else	tput_line	("           UNLOCK %s.\n",prog_files[i]);
			tput_line		("           MOVE WISP-SAVE-FILE-STATUS TO %s.\n",prog_fstats[i]);
			tput_line		("           GO TO WISP-RECORD-LOCK-CLEANUP.\n");
		}
	}

	return 0;
}

static int gen_nolocking_free_all(void)
{
	int	i;

	tput_blank();
	tput_scomment			("****** LOCK PROCESSING ******");
	tput_line			("       WISP-FREE-ALL.");

	for (i=0; i<prog_cnt; i++)						/* Generate the UNLOCK paragraphs.	*/
	{
		if (prog_ftypes[i] & (PRINTER_FILE + SORT_FILE))		/* Skip it if it's printer/sort file.	*/
		{
			continue;
		}
		tput_line		("           MOVE %s TO WISP-SAVE-FILE-STATUS.\n",prog_fstats[i]);
		if (vax_cobol)	
			tput_line	("           UNLOCK %s ALL.\n",prog_files[i]);
		else	tput_line	("           UNLOCK %s.\n",prog_files[i]);
		tput_line		("           MOVE WISP-SAVE-FILE-STATUS TO %s.\n",prog_fstats[i]);
		tput_blank();
	}
	return 0;
}

void set_lock_holder_id(int fnum, int col)
{
	if (!do_locking) return;

	if ( fnum >= 0 )
	{
		tput_line_at(col, "MOVE \"%s\"",prog_files[fnum]);
	}
	else
	{
		tput_line_at(col, "MOVE SPACES");
	}

	tput_clause(col+4, "TO WISP-NEW-LOCK-ID");
											
	/* Store the ID of the file who will do	*/
	/* The current lock, then check to see	*/
	/* That whoever has the last lock is	*/
	/* Told to release it.			*/


	tput_line_at(col, "PERFORM WISP-LOCK-START");	
	tput_clause(col+4,    "THRU WISP-LOCK-END");	
											
	lock_clear_para = 1;
}

/*
	if_file_clear_lock_holder_id	Add logic which checks if this file has the lock and if it does it is cleared.
*/
void if_file_clear_lock_holder_id(int fnum, int col)
{
	if (!do_locking) return;

	if (fnum < 0) return;

	if ( !(prog_ftypes[fnum] & (PRINTER_FILE + SORT_FILE)))				/* If not a printer file clear locks 	*/
	{
		tput_line_at(col, "IF \"%s\" = WISP-LOCK-ID THEN",prog_files[fnum]);
		tput_line_at(col+4, "MOVE SPACES TO WISP-LOCK-ID");			/* The current lock is on. If so, clear	*/
		tput_line_at(col, "END-IF");
	}
}

/*
	clear_lock_holder_id		Add logic to clear the lock holder id.

	With automatic record locking any IO statement will release the previous lock
	so we call the before the statement to clear our holder id.
*/
void clear_lock_holder_id(int col)
{
	if (!do_locking) return;

	tput_line_at(col, "MOVE SPACES TO WISP-LOCK-ID");
}

/*
       unlock_record			Unlock the currently locked record.

       This is used with manual record locking to release the current lock.
*/
void unlock_record(int col)
{
	if (!do_locking) return;

	tput_line_at(col,"PERFORM WISP-FREE-ALL");
	lock_clear_para = 1;
}

/*
**	History:
**	$Log: wt_locks.c,v $
**	Revision 1.15  1998/12/03 20:32:41  gsl
**	Change the WISP-LOCK-START logic to use COMMIT instead of UNLOCK ALL if
**	translating for ACU4GL.
**	
**	Revision 1.14  1998-06-09 13:13:31-04  gsl
**	Rewrote most of this.
**	Fixed headers
**	Added support for manual record locking
**
**	Revision 1.13  1998-03-03 15:47:05-05  gsl
**	update for cobol-85 changes
**
**	Revision 1.12  1998-02-11 17:20:34-05  gsl
**	Add column parameter to predelete_locking() routine
**
**	Revision 1.11  1998-02-10 15:10:58-05  gsl
**	add column position to preclose_locking()
**
**	Revision 1.10  1996-08-30 21:56:22-04  gsl
**	drcs update
**
**
**
*/
