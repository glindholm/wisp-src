/********************************************************************************************************************************
*	filename:	acpmap.c												*
********************************************************************************************************************************/
/********************************************************************************************************************************
*																*
*	bldacp.c	Subroutine to build linked list from contents of file acpmap.dat.  Load information about device for	*
*			serial communication.											*
*																*
********************************************************************************************************************************/
#include <stdio.h>
#include "acp.h"
#include "werrlog.h"
#ifdef VMS

bldacp()
{

#define 	ROUTINE		4500

	FILE 		*acp_file;						/* A file pointer.				*/
	acp_term_id 	*term_ptr;						/* Local pointer for linked list.		*/
	char 		inline[132];						/* Input buffer.				*/
	char		hex_string[6];						/* For hex conversion of EOR sequences.		*/
	char 		*scn_ptr;						/* Pointer for scanning across buffer.		*/
	char 		*prm_ptr;						/* Parameter pointer.				*/
	char 		tempc;							/* Temporary save location.			*/
	char		eor_seqlen;						/* Counter for EOR characters.			*/
	int		i, eor_index;						/* Indeces.					*/
	int		n;							/* Temporary storage of EOR hex value.		*/
	int 		flag;							/* Flag to indicate parameter being processed.	*/
	char		*malloc();
	
	if (acp_term_list) return;						/* List already constructed.			*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0);					/* Generate an entry log.			*/

	acp_file = fopen(ACPMAP_FILE,"r");					/* ACPMAP.DAT contains terminal definitions.	*/

	if (!acp_file)								/* Error opening file.				*/
	{
		werrlog(ERRORCODE(10),0,0,0,0,0,0,0,0);
		exit();
	}

	if (fgets(inline,132,acp_file))   do					/* If there's data, then start processing it.	*/
	{

		/********************************************************
		*	First allocate an element in linked list	*
		********************************************************/
		if (!acp_term_list)						/* First element in linked list?		*/
		{								/* Allocate memory for 1st element in list.	*/
			acp_term_list = (acp_term_id *)malloc(sizeof(acp_term_id));
			acp_term_list->next = 0;				/* Initialize  next pointer to zero.		*/
			acp_term_list->acp_termname[0] = '\0';			/* Initialize as null string.			*/
			acp_term_list->acp_termnum[0] = '\0';			/* Initialize as null string.			*/
			acp_term_list->acp_weorseq[0] = '\0';			/* Initialize write EOR sequence null string.	*/
			for (i=0;i<3;acp_term_list->acp_reorseq[i++][0]='\0');	/* Initalize read EOR sequences null string.	*/
			term_ptr = acp_term_list;				/* Initialize local pointer to head of list.	*/
		}
		else								/* Generate subsequent element in linked list.	*/
		{								/* Allocate a next element, and point to it.	*/
			term_ptr->next = (struct acp_term_id *)malloc(sizeof(acp_term_id));
			term_ptr = (acp_term_id *)term_ptr->next;		/* Increment local pointer to next.		*/
			term_ptr->next = 0;					/* Initialize new next pointer to zero.		*/
			term_ptr->acp_termname[0] = '\0';			/* Initialize as null string.			*/
			term_ptr->acp_termnum[0] = '\0';			/* Initialize as null string.			*/
			term_ptr->acp_weorseq[0] = '\0';			/* Initialize write EOR sequence null string.	*/
			for (i=0;i<3;term_ptr->acp_reorseq[i++][0]='\0');	/* Initalize read EOR sequences null string.	*/
		}

		/********************************************************
		*	Second, scan input line and load parameters.	*
		********************************************************/
		scn_ptr = inline;						/* Initialize pointer to scan input line.	*/

		flag = 1;							/* Switch to parse term name, term number.	*/
		eor_index = 0;							/* Initialize index to acp_eor_seq.		*/

		do								/* Scan the line and extract the parameters.	*/
		{								/* Skip over spaces, commas, newlines , tabs.	*/
			if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t')) scn_ptr++;
			else							/* Copy this parameter.				*/
			{
				if (flag == 1)					/* flag == 1 indicates copy of terminal name.	*/
				{
					flag = 2;				/* Set the flag for later copying of term num.	*/
					prm_ptr = term_ptr->acp_termname;	/* Point to terminal name area.			*/
					do
					{					/* Copy till next whitespace.			*/
						*prm_ptr++ = *scn_ptr++;
					} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n') && (*scn_ptr != '\t'));
					*prm_ptr = '\0';			/* Null terminate.				*/
				}
				else if (flag == 2)				/* flag == 2 indicates copy of terminal number.	*/
				{
					flag = 3;				/* Set the flag for later copying of weor seq	*/
					prm_ptr = term_ptr->acp_termnum;	/* Point to terminal number area.		*/
					*prm_ptr = *scn_ptr++;			/* Get first character.				*/
					if (*prm_ptr != '_')			/* Insert leading underscore: '_'.		*/
					{
						tempc = *prm_ptr;		/* Save char.					*/
						*prm_ptr++ = '_';		/* Put in '_'.					*/
						*prm_ptr = tempc;		/* Restore char.				*/
					}
					prm_ptr++;				/* Next char position.				*/
					do
					{					/* Copy till next whitespace.			*/
						*prm_ptr++ = *scn_ptr++;
					} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n') && (*scn_ptr != '\t'));
					prm_ptr--;				/* Need to look at last character.		*/
					if (*prm_ptr != ':')			/* Requires colon on end.			*/
					{
						prm_ptr++;			/* add one on...				*/
						*prm_ptr = ':';
					}
					prm_ptr++;				/* Point to end.				*/
					*prm_ptr = '\0';			/* Null terminate.				*/
					if (*scn_ptr == '\n') flag = 0;		/* If no more data, clear the flag.		*/
					
				}
				else if (flag == 3)				/* flag == 3 indicates copy of WEOR sequences.	*/
				{
					flag = 4;				/* Set the flag for later copying of reor seq	*/
					eor_seqlen = 0;				/* Initialize counter of WEOR characters.	*/
					for (i=0;i<6;i++) hex_string[i]=0;	/* Clear out hex character buffer.		*/
					prm_ptr = hex_string;			/* Point to hex conversion buffer.		*/
					do
					{					/* Copy till next whitespace.			*/
						*prm_ptr++ = *scn_ptr++;
						eor_seqlen++;			/* Increment counter of WEOR characters.	*/
					} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n') && (*scn_ptr != '\t'));

					if (hex_string[0] == '+')		/* '+' indicates no WEOR sequence given.	*/
						eor_seqlen = 0;			/* Set length of WEOR sequence to zero.		*/
					else
						eor_seqlen = eor_seqlen / 2;	/* Calculate number of chars in WEOR sequence.	*/

					/********************************************************
					*	Note here that the above calculations of	*
					*	eor_seqlen result in a value of 0 if any	*
					*	of the following conditions is true:		*
					*	1.)  The first character of WEOR is '+'		*
					*	2.)  WEOR contains only one character		*
					********************************************************/

					term_ptr->acp_weorseq[0] = eor_seqlen;	/* Assign length to WEOR string.		*/

					prm_ptr = hex_string;			/* Point to hex conversion buffer.		*/
					for (i=1;i<(eor_seqlen+1);i++)		/* Extract hex values from hex_string.		*/
					{
						n=0;				/* Initialize with NULL.			*/
						if (sscanf(prm_ptr,"%2x",&n))	/* Scan for 2 digit hex value.			*/
						{
							term_ptr->acp_weorseq[i] = n;	/* Assign char value to EOR.		*/
							prm_ptr += 2;			/* Increment to next 2 digit value.	*/
						}
					}
					if (*scn_ptr == '\n') flag = 0;		/* If no more data, clear the flag.		*/
				}
				else if (flag == 4)				/* flag == 4 indicates copy of REOR sequences.	*/
				{
					eor_seqlen = 0;				/* Initialize counter of EOR characters.	*/
					for (i=0;i<6;i++) hex_string[i]=0;	/* Clear out hex character buffer.		*/
					prm_ptr = hex_string;			/* Point to hex conversion buffer.		*/
					do
					{					/* Copy till next whitespace.			*/
						*prm_ptr++ = *scn_ptr++;
						eor_seqlen++;			/* Increment counter of EOR characters.		*/
					} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n') && (*scn_ptr != '\t'));

					eor_seqlen = eor_seqlen / 2;		/* Calculate number of chars after conversion.	*/
					term_ptr->acp_reorseq[eor_index][0] = eor_seqlen;	/* Assign length to EOR string.	*/

					prm_ptr = hex_string;			/* Point to hex conversion buffer.		*/
					for (i=1;i<(eor_seqlen+1);i++)		/* Extract hex values from hex_string.		*/
					{
						n=0;				/* Initialize with NULL.			*/
						if (sscanf(prm_ptr,"%2x",&n))	/* Scan for 2 digit hex value.			*/
						{
							term_ptr->acp_reorseq[eor_index][i] = n;  /* Assign char value to EOR.	*/
							prm_ptr += 2;		/* Increment to next 2 digit value.		*/
						}
					}
					eor_index++;
					if ((*scn_ptr == '\n') || (eor_index >= 3)) flag = 0;	/* Clear the flag.		*/
				}
			}
		} while (*scn_ptr && flag);					/* Till a null or flag is clear.		*/
	}while (fgets(inline,132,acp_file));					/* While there are still lines to process.	*/

	fclose(acp_file);							/* Close the file.				*/

}
/********************************************************************************************************************************
*																*
*	getterm.c	Subroutine to search linked list created by bldacp, and return pointer to element with given		*
*			terminal name.												*
*			input:	txname	terminal name, up to 16 chars								*
*																*
*			output: 	return pointer to appropriate linked list element.					*
*																*
********************************************************************************************************************************/
acp_term_id *getterm(txname)

char	txname[16];
{

#define		ROUTINE		22500

	acp_term_id	*term_ptr;						/* Local pointer for linked list.		*/
	int		i,j;							/* To assist with string comparison.		*/

	if (!acp_term_list) return(0);						/* Empty list.					*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0);					/* Generate an entry log.			*/

	term_ptr = acp_term_list;						/* Start at head of list.			*/

	for (j=0;j<16;j++)							/* Set j to number of characters in txname.	*/
		if (txname[j] == ' ') break;					/* Find first nonblank char in txname.		*/

	while ((i=strncmp(txname,term_ptr->acp_termname,j)) && (term_ptr->next))	/* Search list for matching termname.	*/
		term_ptr = (acp_term_id *)term_ptr->next;

	if (i==0) return term_ptr;						/* If strcmp result was 0, we've found it.	*/
	else return(0);								/* Otherwise, it must not be in the list.	*/
}
#endif
#ifdef unix
bldacp()
{
	return;
}
acp_term_id *getterm(name)
char *name;
{
	return &acp_term_struct;
}
#endif
