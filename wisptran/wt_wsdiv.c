			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wisp_wsdiv.c
*/

#define EXT extern
#include "wisp.h"

p_wstor()
{
	int i,j,k,done;
	char tstr[200],tstr1[80],namestr[80],rstr[40];
	char	bin_temp[8];

	if ( 	(pmode == WS_SCREEN) ||
		(!strcmp(parms[2],"USAGE") && !strcmp(parms[3],"DISPLAY-WS.")) ||
		(!strcmp(parms[2],"USAGE") && !strcmp(parms[3],"IS") && !strcmp(parms[4],"DISPLAY-WS.")) ||
		(!strcmp(parms[2],"DISPLAY-WS.")))
											/* Found or currently in a WS screen!!	*/
	{										/* Begin processing the WS screen	*/
		p_item();								/* continue processing the item		*/
	}										/* Fix BINARY variables			*/
	else if (!has_lit && chk_binary())
	{										/* and no literal and has BINARY.	*/
		stredt(inline," USAGE "," ");						/* Try to remove USAGE IS		*/
		stredt(inline," IS "," ");
		if  ((stredt(inline," BINARY."," $COMP.")) == -1) 			/* Replace the BINARY string		*/
		{
			if  ((stredt(inline," BINARY "," $COMP ")) == -1)
			{
				if  ((stredt(inline," BINARY\n"," $COMP\n")) == -1)
				{
					write_log("WISP",'I',"BINARYERR","Error in BINARY statement. input line is\n<%s>",inline);
				}
			}
		}

		if (strpos(inline," PIC") != -1)					/* It already has a PIC.		*/
		{
			strcpy(bin_temp,bin4_type);
			stredt(inline,"$COMP",bin_temp);				/* Just keep the COMP.			*/
		}
		else
		{									/* Generate the PIC also.		*/
			strcpy(bin_temp,bin2_type);
			strcpy(tstr,bin_temp);
			strcat(tstr," PIC S9(4)");
			stredt(inline,"$COMP",tstr);
		}

		write_log("WISP",'I',"REPLBINARY","Replaced BINARY with %s PIC S9(4) in WORKING STORAGE.",bin2_type);

		if (wsqueeze(inline,72) == -1)						/* first try to squeeze it.		*/
		{
			sprintf(tstr," %s ",bin_temp);
			sprintf(tstr1,"\n               %s ",bin_temp);
			stredt(inline,tstr,tstr1);					/* If it doesn't work, break the line	*/
		}

		strcpy(tstr,inline);							/* Save it for a moment.		*/

		i = 0;
		sscanf(parms[0],"%d",&i);						/* Get the value of the level number.	*/

		done = 0;

		j = 0;

		/*
			In COBOL you can specifiy a USAGE clause at the group level and have it apply to all elementary
			items in that group. This does NOT work for the PIC clause. We translate BINARY, which is a USAGE, into
			a USAGE plus a PIC.  If this was at the group level we need to fix it up.
			NOTE: "USAGE IS" was stripped earlier.

			WANG:	01  A-ITEM  USAGE IS BINARY.
				    05  B-ITEM.
				    05  C-ITEM.
				    05  D-ITEM.

			WISP:	01  A-ITEM  COMPx PIC S9(4).		<---- This is WRONG!!  Needs to be fixed up.
				    05  B-ITEM.
				    05  C-ITEM.
				    05  D-ITEM.

			FIXED:	01  A-ITEM.
				    05  B-ITEM  COMPx PIC S9(4).	<---- Move the USAGE and PIC to the elementary level.
				    05  C-ITEM  COMPx PIC S9(4).
				    05  D-ITEM  COMPx PIC S9(4).

		*/

		if (i)
		{
			do								/* Now we need to check each of the next*/
			{								/* Lines to see if they are sub fields.	*/
				if (check_line()) goto finish_up;			/* If opening a copy lib, exit now.	*/

				j = 0;
				sscanf(parms[0],"%d",&j);				/* Get the value of the level number.	*/

				if ((j > i) && (j < 50))				/* We are down a level, needs a pic.	*/
				{
					if (tstr[0])
					{						/* This will work cause the last line	*/
											/* Didn't have a pic originally.	*/
						sprintf(tstr1,"%s PIC S9(4)",bin2_type);
						stredt(tstr,tstr1,"");			/* Undo the mods to the first line.	*/
						put_line(tstr);				/* Output the first line now.		*/
						tstr[0] = '\0';
					}
					strcpy(rstr,parms[1]);
					stredt(rstr,".","");				/* remove period if any.		*/
					strcpy(namestr,rstr);				/* Get the item name.			*/
					strcat(namestr," ");
					strcat(namestr,bin2_type);
					strcat(namestr," PIC S9(4)");			/* Add the COMP stuff.			*/
					stredt(inline,rstr,namestr);			/* Fix it!				*/
					stredt(inline," BINARY "," ");			/* Check for redundant.			*/
					stredt(inline," BINARY.",".");			/* Check for redundant.			*/
					if (wsqueeze(inline,72) == -1)			/* first try to squeeze it.		*/
					{
						char   t1[20], t2[20];
						sprintf(t1," %s ", bin2_type);
						sprintf(t2,"\n               %s ", bin2_type);
						stredt(inline,t1,t2);
					}
					put_line(inline);
				}
				else
				{
					done = 1;					/* all done.				*/
				}
			} while (!done);
		}
		else
		{
			inline[0]='\0';
			inline[1]='\n';
		}
		hold_line();								/* Hold the last line			*/
finish_up:
		if (!wsfiller) fix_filler(tstr);
		put_line(tstr);								/* Output the first line in case.	*/
	}
	else if ( !has_lit &&
		  ( (strpos(inline," COMP ")  != -1) || 
		    (strpos(inline," COMP,")  != -1) || 
		    (strpos(inline," COMP.")  != -1) || 
		    (strpos(inline," COMP\n") != -1) ||
		    (strpos(inline," COMPUTATIONAL ")  != -1) || 
		    (strpos(inline," COMPUTATIONAL,")  != -1) || 
		    (strpos(inline," COMPUTATIONAL.")  != -1) || 
		    (strpos(inline," COMPUTATIONAL\n") != -1)    
		  ) 
		)
	{
		editcomp();								/* Edit COMP to packdec in inline.	*/

		if (!wsfiller) fix_filler(inline);
		put_line(inline);
	}
	else if ( !has_lit && !ws_blank && (strpos(inline," BLANK ") != -1))		/* Get rid of BLANK WHEN ZERO		*/
	{
		stredt(inline," BLANK "," ");						/* Remove BLANK				*/
		stredt(inline," WHEN "," ");						/* Remove WHEN				*/
		if (stredt(inline," ZEROES"," ") == -1)					/* Remove ZEROES			*/
			if (stredt(inline," ZEROS"," ") == -1)				/* Or ZEROS.				*/
				stredt(inline," ZERO"," ");				/* Or ZERO.				*/

		write_log("WISP",'I',"REPLBLANK","Removed BLANK WHEN ZERO in WORKING STORAGE.");
		if (!wsfiller) fix_filler(inline);
		put_line(inline);
	}
	else if (kl_count && isdigit(parms[0][0]))					/* Better see if it's a field to fix.	*/
	{
		if (!fix_key())								/* Try to fix a key field.		*/
		{
			if (!wsfiller) fix_filler(inline);				/* Not a key.				*/
			put_line(inline);						/* For now, just copy it.		*/
		}
	}
	else
	{
		if (!wsfiller) fix_filler(inline);
		put_line(inline);							/* For now, just copy it.		*/
	}
}


chk_binary()
{
	if ((strpos(inline," BINARY ") == -1) && (strpos(inline," BINARY.") == -1) && (strpos(inline," BINARY\n") == -1))
		return(0);								/* No BINARY found			*/
	else
		return(1);								/* One of them is true.			*/
}
