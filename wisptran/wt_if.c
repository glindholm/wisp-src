			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern

#include "wisp.h"
/*
	IF condition [THEN] { statmemt-1    }
			    { NEXT SENTENCE }
                     [ELSE  { statment-2    } ]
		     [      { NEXT SENTENCE } ]


	condition 	( condition )

			F-data-name ALTERED						* "FAC OF" has been changed to "F-"

			figcon  {IN} {identifier}     [IS] [NOT] {ON}
				{OF} {F-display-item}		 {OFF}




	(BEFORE)
           IF FIGCON-1  IN ITEM-1 IS ON OR
              FIGCON-2  IN FAC OF ITEM-2 OFF THEN
              PERFORM XXX-PARA.

	(AFTER)
           MOVE FIGCON-1 TO WISP-TEST-BYTE,
           CALL "bit_test" USING
                    WISP-TEST-BYTE,
                    ITEM-1,
                    WISP-SCRATCH-BYTE-1,

           MOVE FIGCON-2 TO WISP-TEST-BYTE,
           CALL "bit_test" USING
                    WISP-TEST-BYTE,
                    ITEM-2,
                    WISP-SCRATCH-BYTE-2,

           IF WISP-SCRATCH-BYTE-1-TRUE
              OR
              NOT WISP-SCRATCH-BYTE-2-TRUE
           THEN
              PERFORM XXX-PARA.

*/

p_if()											/* Process IF statements.		*/
{
	int i,bytenum,negate;
	char tstr[132],*tptr,*oldstr,*lastr,*curstr,str1[80],str2[80],str3[80],pstr[80];
	int *tnl,*oldnl,*lastnl,*curnl,nl1,nl2,nl3;
	char ifbuff[4096];

	bytenum  = 1;
	oldstr = str1;									/* assign pointers and initialize.	*/
	oldstr[0] = '\0';
	oldnl = &nl1;
	nl1 = 0;

	lastr = str2;
	lastr[0] = '\0';
	lastnl = &nl2;
	nl2 = 0;

	curstr = str3;
	curnl = &nl3;
	nl3 = 0;

	strcpy(ifbuff,inline);								/* Copy the current input line.		*/
	i = strpos(ifbuff,"IF") + 2;
	ifbuff[i] = '\0';								/* put a null after the IF.		*/
	stredt(inline," IF","");							/* remove the if.			*/
	ptype = get_param(curstr);							/* Fetch it.				*/

	for(;;)
	{
		ptype = get_param(curstr);						/* Get the next word.			*/
		*curnl = 0;

		if (curstr[0] == '"')							/* It is a literal			*/
		{
			if (strpos(&curstr[1],"\"") == -1)				/* not closed.				*/
			{
				ptype = 0;
				i = strpos(inline,curstr) + 1;				/* look for param in inline		*/
				memset(curstr,' ',i);					/* set up spaces			*/
				curstr[0] = '\n';
				curstr[i] = '\0';
				i--;
				strcat(curstr,&inline[i]);				/* get the rest of the line.		*/
				get_line();
				ptype = 1;
			}
		}

		if (ptype == 1)
		{									/* New line needed, flag it.		*/
			*curnl = 1;
		}
		else
		{
			*curnl = 0;
		}

		if (!strcmp(curstr,"IS"))
		{
			stredt(inline," IS","");
			i = ptype;
			ptype = get_param(curstr);					/* Skip over IS.			*/
			if (i) ptype = i;						/* Remember if it was first.		*/
		}

		if (ptype == 1)
		{									/* New line needed, finish old line.	*/
			*curnl = 1;
		}
		else
		{
			*curnl = 0;
		}
											/* All done.				*/
		if (!strcmp(curstr,"THEN") || keyword(curstr,proc_keywords) || strcmp(area_a,"    "))
		{
			if (!strcmp(curstr,"THEN"))
			{
				i = 1;
				stredt(inline," THEN","");
			}
			else
			{
				i = 0;
			}
			if (*lastnl) strcat(ifbuff,"\n           ");
			else	     strcat(ifbuff," ");
			strcat(ifbuff,lastr);						/* output the final few words.		*/
			if (*oldnl) strcat(ifbuff,"\n           ");
			else	    strcat(ifbuff," ");
			strcat(ifbuff,oldstr);
			if (i) strcat(ifbuff,"\n            THEN\n");			/* put the THEN in.			*/
			else   strcat(ifbuff,"\n");
			dump_ifbuff = 1;						/* Flag we are dumping the if buffer.	*/
			put_line(ifbuff);
			dump_ifbuff = 0;
			if (!strcmp(curstr,"ELSE"))					/* Special case, IF test ELSE		*/
			{
				write_log("WISP",'E',"IFNOIMPER","IF statement with no imperative, CONTINUE inserted.");
				put_line("               CONTINUE\n");
			}
			hold_line();							/* allow the remainder to parse		*/
			return(0);
		}
		else if (!strcmp(curstr,"ALTERED") || !strcmp(curstr,"ALTERED)") || !strcmp(curstr,"ALTERED))"))
		{
			strcpy(tstr," ");						/* precede it with a space.		*/
			strcat(tstr,curstr);
			stredt(inline,curstr,"");					/* remove it.				*/

			if ((i = strpos(curstr,")")) != -1)				/* If there is a closing paren.		*/
			{
				strcpy(pstr,&curstr[i]);				/* Copy the parens and stuff.		*/
				curstr[i] = '\0';
			}
			else
			{
				pstr[0] = '\0';						/* Blank it out.			*/
			}

			if (!strcmp(oldstr,"NOT"))					/* preceded by a NOT?			*/
			{
				tptr = lastr;						/* Use the LAST string.			*/
											/* add the the TRUE value.		*/
				if (tptr[0] == '(')					/* Has an open paren.			*/
				{
					stredt(tptr,"(","");				/* Remove it				*/
					sprintf(tstr,"\n            (NOT WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
				else
				{
					sprintf(tstr,"\n            NOT WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
			}
			else
			{
				tptr = oldstr;						/* use the OLD string.			*/
				if (*lastnl) strcat(ifbuff,"\n           ");		/* and the newline info.		*/
				else	     strcat(ifbuff," ");
				strcat(ifbuff,lastr);					/* remember to output the LAST string.	*/
				if (tptr[0] == '(')					/* Has it got an open paren?		*/
				{
					stredt(tptr,"(","");				/* Remove it				*/
					sprintf(tstr,"\n            (WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
				else
				{
					sprintf(tstr,"\n            WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
				}
			}

			strcat(ifbuff,tstr);

			put_line  ("               CALL \"bit_test\" USING\n");		/* see if the bit is set		*/
			put_line  ("                    WISP-ALTERED-BIT,\n");
											/* Did they leave off FAC OF?		*/
			if (tptr[0] == 'F' && tptr[1] == '-')				/* Does it have "F-"?			*/
			{
				write_line("                    %s,\n",tptr);		/* Yes, it's ok.			*/
			}
			else
			{
				make_fac(tstr,tptr);					/* Make it a FAC field.			*/
				write_line("                    %s,\n",tstr);		/* Add it on.				*/
			}

			write_line("                    WISP-SCRATCH-BYTE-%d,\n\n",bytenum);

			*curstr = '\0';							/* Clear all the strings.		*/
			*oldstr = '\0';
			*lastr = '\0';
			bytenum++;
		}
		else if (isafigcon1(oldstr) && (!strcmp(curstr,"IN") || !strcmp(curstr,"OF")))
		{
			if (!strcmp(curstr,"IN"))
			{
				stredt(inline," IN","");				/* remove it.				*/
			}
			else
			{
				stredt(inline," OF","");				/* remove it.				*/
			}

			if (*lastnl) strcat(ifbuff,"\n           ");
			else	     strcat(ifbuff," ");
			strcat(ifbuff,lastr);						/* output the LAST string.		*/
			ptype = get_param(curstr);					/* Get the tested string.		*/
			stredt(inline,curstr,"");

			if (ptype == 1)
			{								/* New line needed, finish old line.	*/
				*curnl = 1;
			}
			else
			{
				*curnl = 0;
			}

			ptype = get_param(o_parms[1]);					/* Get the tested string.		*/
			stredt(inline,o_parms[1],"");
											/* next parm has open,it is subscripted	*/
			if (((strpos(curstr,"(") != -1) && (strpos(curstr,")") == -1)) || (o_parms[1][0] == '('))
			{								/* it has a subscript			*/
				strcat(curstr," ");					/* add a space				*/
				strcat(curstr,o_parms[1]);				/* add it on to the end of the name	*/
				while (strpos(o_parms[1],")") == -1)			/* no closing parentheses		*/
				{
					get_param(o_parms[1]);				/* get the next part of the sub script	*/
					if (o_parms[1][0] != ')')			/* a field is next, add a space		*/
					{
						strcat(curstr," ");
					}
					strcat(curstr,o_parms[1]);			/* add it on to the end of the name	*/
					stredt(inline,o_parms[1],"");			/* Remove from input line.		*/
				}
				write_log("WISP",'I',"SUBSCRMOVE",
							"Handle subscripted source field %s in MOVE WITH CONVERSION.",o_parms[0]);
				ptype = get_param(o_parms[1]);				/* get the tested string		*/
			}

			write_line("               MOVE %s TO WISP-TEST-BYTE,\n",oldstr);	/* Output the test value.	*/
			put_line  ("               CALL \"bit_test\" USING\n");
			put_line  ("                    WISP-TEST-BYTE,\n");
			write_line("                    %s,\n",curstr);
			write_line("                    WISP-SCRATCH-BYTE-%d,\n\n",bytenum);


			strcpy(curstr,o_parms[1]);

			if (ptype == 1)
			{								/* New line needed, finish old line.	*/
				*curnl = 1;
			}
			else
			{
				*curnl = 0;
			}

			if (!strcmp(curstr,"IS"))
			{
				i = ptype;
				ptype = get_param(curstr);				/* Skip over IS.			*/
				if (i) ptype = i;

				stredt(inline,curstr,"");

				if (ptype == 1)
				{							/* New line needed, finish old line.	*/
					*curnl = 1;
				}
				else
				{
					*curnl = 0;
				}
			}

			negate = 0;

			if (!strcmp(curstr,"NOT"))					/* preceded by a NOT?			*/
			{
				ptype = get_param(curstr);				/* Skip over it.			*/
				stredt(inline,curstr,"");

				if (ptype == 1)
				{							/* New line needed, finish old line.	*/
					*curnl = 1;
				}
				else
				{
					*curnl = 0;
				}
				negate = 1;						/* add the the TRUE value.		*/
			}

			if ((i = strpos(curstr,")")) != -1)				/* If there is a closing paren.		*/
			{
				strcpy(pstr,&curstr[i]);				/* Copy the parens and stuff.		*/
				curstr[i] = '\0';
			}
			else
			{
				pstr[0] = '\0';						/* Blank it out.			*/
			}
											/* Check OFF or NOT ON or NOT OFF or ON	*/
			if ((!strcmp(curstr,"OFF") && !negate ) || (!strcmp(curstr,"ON") && negate ))
			{
				sprintf(tstr,"\n            NOT WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
			}
			else								/* Assume it is ON.			*/
			{
				sprintf(tstr,"\n            WISP-SCRATCH-BYTE-%d-TRUE%s\n            ",bytenum,pstr);
			}

			strcat(ifbuff,tstr);

			*curstr = '\0';							/* Clear all the strings.		*/
			*oldstr = '\0';
			*lastr = '\0';
			bytenum++;
		}
		else
		{
			stredt(&inline[7],curstr,"");					/* remove them from the input		*/
			if (*lastnl) 	    strcat(ifbuff,"\n           ");		/* and the newline info.		*/
			else if (lastr[0]) strcat(ifbuff," ");
			strcat(ifbuff,lastr);						/* output the last string		*/
			tptr = lastr;
			tnl = lastnl;
			lastr = oldstr;							/* advance all the pointers.		*/
			lastnl = oldnl;
			oldstr = curstr;
			oldnl = curnl;
			curstr = tptr;
			curnl = tnl;
			*curstr = '\0';							/* blank out the current one.		*/
		}

	}
}


