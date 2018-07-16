 			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Definitions									*/

#define	MAX_BUFFS 64									/* Define maximum number of messages.	*/
#define MAX_MSG   80									/* Define maximum message size.		*/

struct tbuffer { int cpu; int x,y,z; } time_now;					/* Time reference structure.		*/
static int time_buf[MAX_BUFFS];								/* Array of times.			*/
static char labels[MAX_BUFFS][MAX_MSG];							/* Array of labels.			*/
static int tcount = 0;									/* Count of number of buffers.		*/
static int initial;									/* Initial CPU time.			*/


/*						Subroutine Entry Point								*/

vtrace(info) char *info;								/* Set up to trace.			*/
{
	register char *linfo;								/* Working pointer.			*/
	register int i, j, j1, k;							/* Working registers.			*/

	linfo = info;									/* Set working pointer to buffer start.	*/

	if (*linfo == '!')								/* Flush the buffer?			*/
	{
		if (tcount)								/* Is there anything in it?		*/
		{
			vmove(0,0);							/* Move to a convenient place.		*/
			vprint("----- CPU Usage Report -----     \n");			/* Make sure we are on the screen.	*/

			for (i = 0; i < tcount; i++)					/* Loop through all items.		*/
			{
				j = (time_buf[i] - initial);				/* Get delta amount.			*/
				j1 = j / 100;						/* Convert to seconds.			*/
				k = j - (j1 * 100);					/* Convert fraction of seconds.		*/

				vprint("%-60s %d\056%d seconds.   \n",labels[i],j1,k);	/* Display the time report item.	*/
				initial = time_buf[i];					/* Get new initial reference.		*/
			}

			vprint("Depress any key to continue...    ");			/* Wait for user to acknowledge.	*/
			vgetc();
		}

		times(&time_now);							/* Get the current CPU value.		*/
		initial = time_now.cpu;							/* Record the current CPU time.		*/
		tcount = 0;								/* Nothing left to log.			*/
	}

	else										/* Store log request into the tables.	*/
	{
		if (tcount < MAX_BUFFS)							/* Is there room?			*/
		{
			i = 0; 								/* Initialize the count.		*/
			while ((*linfo != '.') && (*linfo != 0) && (i < MAX_MSG-1))	/* At end of description string?	*/
			{
				labels[tcount][i++] = *linfo++;				/* No so store the data.		*/
			}

			labels[tcount][i] = '\0';					/* Null terminate.			*/
			times(&time_now);						/* Get the current CPU value.		*/
			time_buf[tcount++] = time_now.cpu;				/* Save it.				*/
		}
	}
}
