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

gen_unlocks()										/* Generate UNLOCK logic for reads.	*/
{
	int i;

	if (!do_locking) return;

	put_line				("\n      ***** LOCK PROCESSING PARAGRAPHS\n\n");
	put_line				("       WISP-LOCK-START.\n");
	
	if (acu_cobol)
	{
		put_line			("           UNLOCK ALL RECORDS.\n"); 	/* Unlock all locks.			*/
	}
	else
	{
		put_line			("           IF WISP-LOCK-ID = SPACES THEN\n");
		put_line			("               GO TO WISP-RECORD-LOCK-CLEANUP.\n\n");
		for (i=0; i<prog_cnt; i++)						/* Generate the GOTO DEPENDING stuff.	*/
		{									/* Skip if It's a print/sort file.	*/
											/* Or an ACUCOBOL SEQ file.		*/
			write_line		("           IF WISP-LOCK-ID = \"%s\" THEN\n",prog_files[i]);
			if ((prog_ftypes[i] & (PRINTER_FILE + SORT_FILE)) ||
			    (acu_cobol && (prog_ftypes[i] & SEQ_FILE)))
			{
				write_line	("               GO TO WISP-LOCK-END.\n\n"); /* Do nothing.			*/
			}
			else
			{
				write_line	("               GO TO WISP-RECORD-UNLOCK-%d.\n\n",i+1); 
			}
		}
	}

	put_line	  			("       WISP-RECORD-LOCK-CLEANUP.\n");
	put_line	  			("           MOVE WISP-NEW-LOCK-ID TO WISP-LOCK-ID.\n");
	put_line	  			("       WISP-LOCK-END.\n");
	put_line	  			("           EXIT.\n\n");

	if (!acu_cobol)
	{
		for (i=0; i<prog_cnt; i++)						/* Generate the UNLOCK paragraphs.	*/
		{
			if ((prog_ftypes[i] & (PRINTER_FILE + SORT_FILE)) ||		/* Skip it if it's printer/sort file.	*/
			    (acu_cobol && (prog_ftypes[i] & SEQ_FILE)))			/* Or if it's ACUCOBOL & seq file.	*/
				continue;
			write_line		("       WISP-RECORD-UNLOCK-%d.\n",i+1);
			write_line		("           MOVE %s TO WISP-SAVE-FILE-STATUS.\n",prog_fstats[i]);
			if (vax_cobol)	
				write_line	("           UNLOCK %s ALL.\n",prog_files[i]);
			else	write_line	("           UNLOCK %s.\n",prog_files[i]);
			write_line		("           MOVE WISP-SAVE-FILE-STATUS TO %s.\n",prog_fstats[i]);
			put_line		("           GO TO WISP-RECORD-LOCK-CLEANUP.\n");
		}
	}

}

set_lock(fnum,indent)
int fnum;
int indent;
{
	char	col[80];

	if (!do_locking) return;

	memset(col,' ',80);
	col[indent] = 0;

	if (in_decl || copy_para) 							/* Don't check if in DECLARATIVES!!	*/
	{
		write_line("%sCONTINUE\n",col);
	}

	if ( fnum >= 0 )
		write_line("%sMOVE \"%s\" TO WISP-NEW-LOCK-ID\n",col,prog_files[fnum]);
	else
		write_line("%sMOVE SPACES TO WISP-NEW-LOCK-ID\n",col,prog_files[fnum]);
											/* Store the ID of the file who will do	*/
	write_line	  ("%sPERFORM WISP-LOCK-START THRU WISP-LOCK-END\n",col);	/* The current lock, then check to see	*/
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
	if (!do_locking) return;

	if ( !(prog_ftypes[filenum] & (PRINTER_FILE + SORT_FILE)))			/* If not a printer file clear locks 	*/
	{
		write_line("           IF \"%s\" = WISP-LOCK-ID THEN\n",prog_files[filenum]);
											/* The current lock is on. If so, clear	*/
		put_line  ("               MOVE SPACES TO WISP-LOCK-ID\n");
		put_line  ("           END-IF\n");
	}
}

/*
	predelete_locking	Add pre-delete DMS locking logic
*/
predelete_locking()
{
	if (!do_locking) return;

	put_line("           MOVE SPACES TO WISP-LOCK-ID\n");				/* Clear the lock.			*/
}

/*
	prerewrite_locking	Add pre-rewrite DMS locking logic
*/
prerewrite_locking()
{
	if (!do_locking) return;
	put_line("           MOVE SPACES TO WISP-LOCK-ID\n");				/* Clear the lock.			*/
}

/*
	clear_locking		Add logic to clear the lock holder id
*/
clear_locking()
{
	if (!do_locking) return;
	put_line  		("                MOVE SPACES TO WISP-LOCK-ID\n");
}

