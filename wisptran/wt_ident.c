			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/
#ifdef MSDOS
#include <string.h>
#endif

#define EXT extern
#include "wisp.h"

extern int 	symbzero;
extern char	decimal_is;

char	*figcon1_ring = 0;
struct	figcon1_struct
{
	long	value;
	char	name[40];
} figcon1_item;
int figcon1_compare();


p_ident()
{
	long j,k,l,rc;
	char templine[128];
	char	token[80];
	int	progid;

	progid = 0;									/* not in program-id.			*/

	for(;;)
	{
		while(0==strcmp(area_a,"    "))
		{
			if (progid && strlen(inline) > 6) inline[6] = '*';		/* Force to a comment. This is to 	*/
											/* handle comments following program-id	*/
											/* on same line.			*/
			put_line(inline);
			get_line();							/* Skip over any comments.		*/
		}
		nopunct(token,parms[0]);

		progid = 0;								/* not in program-id.			*/
		if (!strcmp(token,"PROGRAM-ID"))					/* fix up program id			*/
		{									/* make sure it's on one line		*/
			get_param(templine);						/* get PROGRAM-ID			*/
			get_param(templine);						/* get program name			*/
			if (strlen(templine) > 8) templine[8] = '\0';			/* cut off progid at 8 chars.		*/
			strcpy(prog_id,templine);					/* and save it				*/

			if (linkmain)
			{
				write_line("       COPY \"wisplink.cpy\" REPLACING \"PROGNAME\" BY \"%s\".\n",prog_id);
				put_line  ("       IDENTIFICATION DIVISION.\n");
			}

			if (init_data || linkmain)
			{
				strcpy(templine," IS");
				if (linkmain)	strcat(templine," COMMON");
				if (init_data)	strcat(templine," INITIAL");
				strcat(templine," PROGRAM");
			}
			else
			{
				templine[0] = '\0';
			}

			write_line("       PROGRAM-ID. %s%s.\n",prog_id,templine);
			progid = 1;							/* doing program-id.			*/
		}

		else if (!strcmp(token,"AUTHOR"))					/* fix up AUTHOR.			*/
		{
			put_line(inline);						/* Just output it.			*/
		}

		else if (!strcmp(token,"INSTALLATION"))					/* fix up INSTALLATION.			*/
		{
			put_line(inline);						/* Just output it.			*/
		}

		else if (!strcmp(token,"DATE-WRITTEN"))					/* fix up DATE-WRITTEN.			*/
		{
			put_line(inline);						/* Just output it.			*/
		}

		else if (!strcmp(token,"DATE-COMPILED"))				/* fix up DATE-COMPILED.		*/
		{
			put_line(inline);						/* Just output it.			*/
		}

		else if (!strcmp(token,"DATE-COMPILEDDATE"))				/* Special fix for USAF			*/
		{									/* Change DATE-COMPILED.DATE.		*/
			stredt(inline,".",". ");					/* into   DATE-COMPILED. DATE.		*/
			put_line(inline);
		}
			 
		else if (!strcmp(token,"SECURITY"))					/* fix up SECURITY.			*/
		{
			put_line(inline);						/* Just output it.			*/
		}
		else
		{
			hold_line();
			break;
		}
		get_line();
	}
}

p_environ()
{
	char	token[80];
	int	rc;

	nopunct(token,parms[0]);
	if (!strcmp(token,"SOURCE-COMPUTER"))					/* Look for source computer		*/
	{									/* And fix it up			*/
		check_config();
		if (vax_cobol)
			put_line("       SOURCE-COMPUTER. VAX.\n");
		else if (dos_cobol)
			put_line("       SOURCE-COMPUTER. MSDOS.\n");
		else
			put_line("       SOURCE-COMPUTER. UNIX.\n");

		do
		{
			get_line();
		} while (!strcmp(area_a,"    "));
		hold_line();
	}
	else if (!strcmp(token,"OBJECT-COMPUTER"))				/* Same with object computer		*/
	{
		check_config();
		if (vax_cobol)
			put_line("       OBJECT-COMPUTER. VAX.\n");
		else if (dos_cobol)
			put_line("       OBJECT-COMPUTER. MSDOS.\n");
		else
			put_line("       OBJECT-COMPUTER. UNIX.\n");

		do
		{
			get_line();
		} while (!strcmp(area_a,"    "));
		hold_line();
	}
	else if (!strcmp(token,"SPECIAL-NAMES"))					/* fix up SPECIAL-NAMES.		*/
	{
		wrote_special_names = 1;
		put_line(inline);							/* Just output it.			*/
		get_line();
		while (!strcmp(area_a,"    "))
		{
			char *p;

			if (p=(char *)strchr(inline,'.')) *p = ' ';

			if (-1 != strpos(inline,"DECIMAL-POINT") ) 
			{
				decimal_is = ',';
			}
			else							/* Don't write the DECIMAL IS COMMA HERE	*/
			{
				put_line(inline);
			}

			get_line();
		} 
		hold_line();
	}
	else if ( (!strcmp(token,"FIGURATIVE-CONSTANTS")) || (pmode == FIG_CONS))       /* Found or currently in FIG CONS	*/
	{
		int	figvalue;

		if (!pmode)
		{
			write_log("WISP",'I',"BEGINFIGCON","Begin FIGURATIVE-CONSTANTS Analysis.");
			pmode = FIG_CONS;						/* Set mode so we can be called again	*/ 
			check_config();
			init_figcons();
		}
		else do									/* Scan for names and hex values	*/
		{
			ptype = get_param(o_parms[0]);					/* look for a name			*/

			if ((ptype == -1) && (o_parms[0][0] == '\0')) goto fig_cons_done;	/* Fig cons ended abruptly.	*/

			ptype = get_param(templine);					/* Get the value it represents		*/
			if (!strcmp(templine,"IS"))					/* If it is the keyword IS, skip it	*/
			{
				ptype = get_param(templine);
			}
			sscanf(&templine[1],"%x",&figvalue);				/* Scan for the hex value		*/

			if (figvalue > 255)						/* bad news				*/
			{
				strcpy(fig_cons[fig_count],o_parms[0]);			/* save the name			*/
											/* Scan for the hex values		*/
				templine[5] = templine[4];
				templine[4] = templine[3];
				templine[3] = '\0';
				templine[6] = '\0';
				sscanf(&templine[1],"%x",&fig_val[fig_count][0]);
				sscanf(&templine[4],"%x",&fig_val[fig_count][1]);

				write_log("WISP",'I',"2BYTEFIGCON","2 byte Figurative constant, values are %d, %d.",
										fig_val[fig_count][0],fig_val[fig_count][1]);
				fig_count++;						/* count it				*/
			}
			else
			{
		
				if (!figcon1_ring)
				{
					if( rc = ring_open(&figcon1_ring,sizeof(struct figcon1_struct),25,25,figcon1_compare,1) )
					{
						write_log("WISP",'F',"RINGOPEN",
							"Unable to open ring [figcon1_ring] rc=%d [%s]",rc,ring_error(rc));
						exit_wisp(-1);
					}
				}
				strcpy(figcon1_item.name,o_parms[0]);
				figcon1_item.value = figvalue;
				if (rc = ring_add(figcon1_ring,0,&figcon1_item))	/* Store the figcon into the ring	*/
				{
					write_log("WISP",'F',"RINGADD",
						"Unable to add to ring [figcon1_ring] rc=%d [%s]",rc,ring_error(rc));
					exit_wisp(-1);
				}
				
				if (!symbzero) figvalue++;				/* Add 1 to correct collating sequence	*/
				write_line("           SYMBOLIC %s IS %d\n",o_parms[0],figvalue);/* Write var name 		*/
			}
fig_cons_done:
			if (ptype == -1)  						/* Last one !				*/
			{
				finish_figcons();
				write_log("WISP",'I',"FINFIGCON","Finish FIGURATIVE-CONSTANTS Analysis.");
				pmode = 0;						/* clear re-call flag			*/
			}
		} while (pmode);							/* End of do				*/
	}
	else
	{
		hold_line();							/* It was a line needing other processing.	*/
	}
}


int figcon1_compare(p1,p2)
struct figcon1_struct *p1, *p2;
{
	int	cmp;

	cmp = strcmp(p1->name, p2->name);
	return(cmp);
}

int isafigcon1(item)
char	*item;
{
	int	rc;
	if (!figcon1_ring) return(0);
	if (strlen(item) > 39) return(0);					/* Item is too big to be a figcon		*/

	strcpy(figcon1_item.name,item);
	rc = ring_find(figcon1_ring,&figcon1_item,0,0);
	return(!rc);
}
