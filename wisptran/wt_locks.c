static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#define EXT extern
#include "wisp.h"
#include "cobfiles.h"

gen_unlocks()										/* Generate UNLOCK logic for reads.	*/
{
	int i;

	if (!do_locking)
	{
		if (!acu_cobol)
		{
			gen_wisp_free_all();
		}
		return 0;
	}

	tput_blank();
	tput_scomment				("****** LOCK PROCESSING PARAGRAPHS ******");
	tput_line				("       WISP-LOCK-START.");
	
	if (acu_cobol)
	{
		tput_line			("           UNLOCK ALL RECORDS."); 	/* Unlock all locks.			*/
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

}

int gen_wisp_free_all()
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

set_lock(fnum,indent)
int fnum;
int indent;
{
	if (!do_locking) return 0;

	if (in_decl || copy_to_dcl_file) 						/* Don't check if in DECLARATIVES!!	*/
	{
		tput_line_at(indent,"CONTINUE");
	}

	if ( fnum >= 0 )
		tput_line_at(indent, "MOVE \"%s\" TO WISP-NEW-LOCK-ID",prog_files[fnum]);
	else
		tput_line_at(indent, "MOVE SPACES TO WISP-NEW-LOCK-ID",prog_files[fnum]);
											/* Store the ID of the file who will do	*/
	tput_line_at	    (indent, "PERFORM WISP-LOCK-START THRU WISP-LOCK-END");	/* The current lock, then check to see	*/
											/* That whoever has the last lock is	*/
											/* Told to release it.			*/
	lock_clear_para = 1;
}

/*
	preclose_locking	Add pre-close DMS locking logic
*/
preclose_locking(filenum)
int filenum;
{
	if (!do_locking) return 0;

	if ( !(prog_ftypes[filenum] & (PRINTER_FILE + SORT_FILE)))			/* If not a printer file clear locks 	*/
	{
		tput_line("           IF \"%s\" = WISP-LOCK-ID THEN",prog_files[filenum]);
		tput_line("               MOVE SPACES TO WISP-LOCK-ID");		/* The current lock is on. If so, clear	*/
		tput_line("           END-IF");
	}
}

/*
	predelete_locking	Add pre-delete DMS locking logic
*/
predelete_locking()
{
	if (!do_locking) return 0;

	tput_line_at(12, "MOVE SPACES TO WISP-LOCK-ID");				/* Clear the lock.			*/
}

/*
	prerewrite_locking	Add pre-rewrite DMS locking logic
*/
prerewrite_locking()
{
	if (!do_locking) return 0;
	tput_line_at(12,"MOVE SPACES TO WISP-LOCK-ID");					/* Clear the lock.			*/
}

/*
	clear_locking		Add logic to clear the lock holder id
*/
clear_locking()
{
	if (!do_locking) return 0;
	tput_line_at(16, "MOVE SPACES TO WISP-LOCK-ID");
}

/*
**	History:
**	$Log: wt_locks.c,v $
**	Revision 1.10  1996-08-30 21:56:22-04  gsl
**	drcs update
**
**
**
*/
