			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	wisp_input.c		Handle the INPUT-OUTPUT SECTION.

				This is mainly the SELECT statement, all else is just passed thru unchanged.

*/

#define EXT extern
#include "wisp.h"

extern	int	ioc_found;								/* Was I-O-CONTROL found		*/
extern	char	selecttext[400];							/* Additional user inserted text	*/

static	int	mf_compress = 0;

#define MAX_ALT_KEYS	64

#define RELATIVE	0x0001
#define	INDEX		0x0002
#define SEQUENTIAL	0x0004
#define RANDOM		0x0008
#define DYNAMIC		0x0010

p_input()
{
	int i,first;
	char	reckey[40],								/* The RECORD KEY.			*/
		reckeyq[40],								/* The reckey qualification		*/
		areas[8],								/* RESERVE AREAS.			*/
		relkey[40],								/* The RELATIVE KEY.			*/
		relkeyq[40];								/* The relkey qual			*/
	char	altkey[MAX_ALT_KEYS][40];						/* Table of alternate record keys.	*/
	char	altkeyq[MAX_ALT_KEYS][40];						/* Table of altkey qual.		*/
	char	*splitkey, *altsplitkey[MAX_ALT_KEYS];					/* Splitkey ptrs.			*/
	int	duplic[MAX_ALT_KEYS];							/* Table of duplicates.			*/
	int	org;									/* Organization.			*/
	int	accmod;									/* Access Mode.				*/
	int	num_alt;
	int	printer;
	int	this_file;
	int	seq_org_flag, seq_acc_flag;						/* Did we really see the SEQ clause ?	*/
	char	seqtype[10];
	int	optional_found;
	int	was_seq_dyn;

	if (!strcmp(parms[0],"SELECT"))							/* Is a SELECT statement, find out if	*/
	{										/* it is the CRT file.			*/
		if (prog_cnt > MAX_FILES)
		{
			write_log("WISP",'F',"FILEBUFEXC","Maximum number of files exceeded in SELECT statement.");
			exit_wisp(-1);
		}

		this_file = prog_cnt;							/* Remember this file.			*/

		prog_ftypes[this_file] = DISK_FILE;					/* assume it is a disk file		*/
		prog_keys[this_file].count = 0;						/* CLear the keys pointer.		*/
		prog_recs[this_file] = (char *) 0;					/* Clear the pointer.			*/

		file_master[this_file].key_cnt = 0;					/* Init number of keys			*/
		file_master[this_file].key_start = '\0';				/* Init start ptr			*/

		get_param(o_parms[0]);							/* skip the select			*/
		get_param(o_parms[0]);							/* this is the symbolic-name		*/
		if ( strcmp(o_parms[0],"OPTIONAL") == 0 )
		{
			optional_found = 1;
			get_param(o_parms[0]);						/* this is the symbolic-name		*/
		}
		else
		{
			optional_found = 0;
		}

		if (sortfile)
		{
			if (sf_count == MAX_SF_ITEMS)
			{
				write_log("WISP",'E',"MAXSFITEMS","Maximum number of SORT_FILE items exceeded.");
			}
			else
			{
				strcpy(sf_item[sf_count++],o_parms[0]);			/* Save the name of the file.		*/
			}

			sortfile = 0;
		}

		get_param(o_parms[1]);							/* look for the ASSIGN TO		*/
		if (strcmp(o_parms[1],"ASSIGN"))					/* See if it is ASSIGN			*/
		{
			write_log("WISP",'F',"ERRINASSIGN","Error in ASSIGN Statement, Input line is;");
			write_log(" ",' '," ","%s",inline);
			write_log(" ",' '," ","parm is ->%s\n",o_parms[1]);
			exit(0);
		}

		ptype = get_param(o_parms[1]);						/* if this is TO, keep going		*/
		if (!strcmp(o_parms[1],"TO"))						/* if Not a TO, it is the next parm	*/
		{
			ptype = get_param(o_parms[1]);					/*  else get the next parm directly	*/
		}									/* this is the parameter ref name	*/

		if (o_parms[1][0] == '\042')						/* if file name is a literal		*/
		{
			i = strlen(o_parms[1]) - 1;					/* see if properly terminated		*/
			if (o_parms[1][i] != '\042')
			{
				ptype = get_param(o_parms[2]);				/* not terminated, get the dangled "	*/
				strcat(o_parms[1],"\042");				/* append one on			*/
			}
		}

		printer = 0;

		if (ptype != -1)
		{
			ptype = get_param(o_parms[2]);					/* This may be the file type		*/

			if (!strcmp(o_parms[2],"\"DISK\""))				/* It's a disk file.			*/
			{
				ck_nodis();						/* Check for NODISPLAY.			*/
			}
			else if (!strcmp(o_parms[2],"\"DISPLAY\""))			/* It's a CRT file, Flag it.		*/
			{
				prog_ftypes[this_file] = -1;
			}
			else if (!strcmp(o_parms[2],"\"PRINTER\""))
			{
				FSET(prog_ftypes[this_file],PRINTER_FILE);		/* it is a print file			*/
				printer = 1;
				if (autolockprint) prog_ftypes[this_file] |= AUTOLOCK;	/* Turn on automatic locking flag	*/
				ck_nodis();						/* Check for NODISPLAY.			*/
			}
			else if (!strcmp(o_parms[2],"\"TAPE\""))			/* It's a tape file.			*/
			{
				prog_ftypes[this_file] = TAPE_FILE;
				ck_nodis();						/* Check for NODISPLAY.			*/
			}
		}

		*seqtype = '\0';
		if (!vax_cobol)
		{
			if ( printer || seqline) 
			{
				strcpy(seqtype,"LINE ");
			}
			if ( seqbinary )
			{
				if (mf_aix) 	strcpy(seqtype,"RECORD ");
				else 		strcpy(seqtype,"BINARY ");
			}
			seqline = 0;
			seqbinary = 0;
		}

		if (prog_ftypes[this_file] == -1)					/* is it a DISPLAY?			*/
		{
			cur_crt = crt_fcount;
			crt_cursor[crt_fcount][0] = '\0';				/* may not be a cursor field		*/
			crt_pfkey[crt_fcount][0] ='\0';					/* Or a pfkey field.			*/
			crt_status[crt_fcount][0] = '\0';				/* Or a file status field.		*/
			crt_relative[crt_fcount][0] = '\0';				/* Or a relative key field.		*/

			write_log("WISP",'I',"SELDISPLAY","SELECT for the display, name is %s",o_parms[0]);
			strcpy(crt_file[crt_fcount],o_parms[0]);			/* copy the screen file name		*/
			write_line("           SELECT %s\n",crt_file[crt_fcount]);	/* include SELECT for dummy CRT file	*/
			put_line  ("               ASSIGN TO \042WISP-CRTFILE\042.\n");

			do
			{
				ptype = get_param(templine);				/* get next parameter			*/
				if (!strcmp(templine,"PFKEY"))				/* it's the PFKEY keyword		*/
				{
					ptype = get_param(crt_pfkey[crt_fcount]);	/* next word is either IS or is		*/
					if (!strcmp(crt_pfkey[crt_fcount],"IS"))	/* if IS then skip it			*/
					{
						ptype = get_param(crt_pfkey[crt_fcount]);/* get pfkey name			*/
					}
					write_log("WISP",'I',"PFKEYVAL","  PFKEY is %s",crt_pfkey[crt_fcount]);
				}
				else if (!strcmp(templine,"CURSOR"))			/* it's the CURSOR keyword		*/
				{							/* find out what the field name is	*/
					ptype = get_param(crt_cursor[crt_fcount]);
					if (!strcmp(crt_cursor[crt_fcount],"POSITION"))	/* if POSITION then skip it		*/
					{
						ptype = get_param(crt_cursor[crt_fcount]);
					}
					if (!strcmp(crt_cursor[crt_fcount],"IS"))	/* if IS then skip it			*/
					{
						ptype = get_param(crt_cursor[crt_fcount]);
					}
					write_log("WISP",'I',"CURSORVAL","  CURSOR POSITION is %s",crt_cursor[crt_fcount]);
				}

				else if (!strcmp(templine,"RELATIVE"))		/* it's the RELATIVE keyword		*/
				{						/* find out what the field name is	*/
					ptype = get_param(crt_relative[crt_fcount]);
					if (!strcmp(crt_relative[crt_fcount],"KEY"))	/* if KEY then skip it			*/
					{
						ptype = get_param(crt_relative[crt_fcount]);
					}
					if (!strcmp(crt_relative[crt_fcount],"IS"))	/* if IS then skip it			*/
					{
						ptype = get_param(crt_relative[crt_fcount]);
					}
					write_log("WISP",'I',"RELKEYVAL","  RELATIVE KEY is %s",crt_relative[crt_fcount]);
				}

				else if (!strcmp(templine,"STATUS"))		/* it's the STATUS keyword		*/
				{
					ptype = get_param(crt_status[crt_fcount]);	/* next word is either IS or is		*/
					if (!strcmp(crt_status[crt_fcount],"IS"))	/* if IS then skip it			*/
					{
						ptype = get_param(crt_status[crt_fcount]);/* get pfkey name			*/
					}
					write_log("WISP",'I',"STATVAL","  STATUS is %s",crt_status[crt_fcount]);
				}
			} while (ptype != -1);						/* do it till end of statement		*/
											/* Make sure we have a pfkey value	*/
			if (!crt_pfkey[crt_fcount][0]) strcpy(crt_pfkey[crt_fcount],"WISP-PFKEY-VALUE");
											/* And a file status.			*/
			if (!crt_status[crt_fcount][0]) strcpy(crt_status[crt_fcount],"WISP-FILE-STATUS");
			crt_fcount++;							/* Now count up one.			*/
		}
		else									/* was not a display, fix it up		*/
		{									/* It is a real file of some type.	*/
			if (vax_cobol && (strpos(o_parms[1],"#") != -1))		/* found a # symbol in the name		*/
			{
				write_log("WISP",'I',"NAMECHANGE","Changed %s to ",o_parms[1]);
				do {} while(stredt(o_parms[1],"#","$") != -1);		/* change # to $ in SELECT 		*/
				write_log(" ",' '," ","%s.",o_parms[1]);
			}

			strcpy(prog_files[this_file],o_parms[0]);			/* save it in the table			*/
			if (autolockfile)						/* if autolockflag is set then...	*/
			{
				prog_ftypes[this_file] |= AUTOLOCK;			/* mark this file for automatic locking */
				autolockfile = 0;					/* Clear the flag.			*/
			}
			prog_vnames[this_file][0] = 0;					/* clear volume name			*/
			prog_lnames[this_file][0] = 0;					/* clear library name			*/
			prog_fnames[this_file][0] = 0;					/* clear file name			*/
			prog_fstats[this_file][0] = 0;					/* clear file status field		*/
			prog_ref[this_file] = 0;					/* not opened yet.			*/

			strcpy(prog_prname[this_file],o_parms[1]);			/* Save the prname.			*/
			stredt(prog_prname[this_file],"\"","");				/* Remove any quotes.			*/
			stredt(prog_prname[this_file],"\"","");
			prog_cnt++;


			if (ptype == -1)						/* end it now				*/
			{
				org = SEQUENTIAL;
				accmod = SEQUENTIAL;
				prog_ftypes[this_file] |= SEQ_FILE;
				prog_ftypes[this_file] |= SEQ_SEQ;

				if (del_sel()) return(0);				/* Quit if this file should be deleted.	*/

				write_sel(printer,this_file,org,accmod);		/* Normal SELECT gen for SEQ-SEQ.	*/

				if (*seqtype)			
					write_line("\n             ORGANIZATION IS %sSEQUENTIAL",seqtype);


				if (chk_sort(prog_files[this_file]))			/* if a SORT file were done		*/
				{
					put_line(".\n\n");				/* End it all				*/
					return(0);
				}

				i = 0;
				if (nfs_count)						/* If there are files not needing stat	*/
				{
					do
					{						/* Look for it in the list.		*/
						if (!strcmp(prog_files[this_file],nfs_item[i])) break;
					} while (++i < nfs_count);
				}

				if (i < nfs_count)					/* It was there.			*/
				{
					put_line(".\n");				/* Just end the line.			*/
				}
				else
				{
					write_log("WISP",'I',"NOSTATUS",
							"No file status for file %s, WISP-FILE-STATUS used.",
						prog_files[this_file]);

					put_line("\n             FILE STATUS IS WISP-FILE-STATUS.\n");
					strcpy(prog_fstats[this_file],"WISP-FILE-STATUS");	/* save the name		*/
				}

				if ( mf_compress )					/* If MF file compression is on		*/
				{
					put_line("      $SET DATACOMPRESS\"0\"\n");	/* Turn it off				*/
					mf_compress = 0;
				}
			}
			else								/* Find out more about it.		*/
			{
				reckey[0] = '\0';					/* Doesn't have a record key.		*/
				reckeyq[0] = '\0';					/* Doesn't have a record key.		*/
				splitkey = NULL;					/* No splitkey 				*/
				areas[0] = '\0';					/* No areas reserved.			*/
				relkey[0] = '\0';					/* No relative key defined.		*/
				relkeyq[0] = '\0';					/* No relative key defined.		*/
				org = 0;						/* Organization is not known yet.	*/
				accmod = 0;						/* Access mode is not known yet.	*/
				seq_org_flag = 0;					/* Have not seen SEQ clause		*/
				seq_acc_flag = 0;					/* Have not seen SEQ clause		*/
				num_alt = 0;						/* There are no alternate record keys.	*/
				first = 1;						/* Flag first time.			*/

				do
				{

					if (!first)					/* Don't do this the first time.	*/
					{
						ptype = get_param(o_parms[0]);		/* get a new parm....			*/
					}
					else
					{
						strcpy(o_parms[0],o_parms[2]);
						first = 0;				/* No longer the first time.		*/
					}

					if (!strcmp(o_parms[0],"ALTERNATE"))		/* alternate record key			*/
					{
						write_log("WISP",'I',"BEGALTRECKEY","Begin ALTERNATE RECORD KEY");
						ptype = get_param(o_parms[0]);		/* should be RECORD			*/
						ptype = get_param(o_parms[0]);		/* is it KEY?				*/
						if (!strcmp(o_parms[0],"KEY"))
						{
							ptype = get_param(o_parms[0]);	/* Skip it also.			*/
						}
						write_log("WISP",'I',"COMALTRECKEY","Completed ALTERNATE RECORD KEY");
					}
					if (isdigit(o_parms[0][0]))			/* a second alternate record key	*/
					{
						write_log("WISP",'I',"BEGSECALTKEY","Begin SECONDARY ALTERNATE RECORD KEY");
						if (num_alt == MAX_ALT_KEYS)
						{
							write_log("WISP",'F',"ALTKEYEXC","Number of alternate keys exceeded.");
							exit_wisp(1);
						}

											/* Get the record key			*/
						ptype = get_record_key(altkey[num_alt],altkeyq[num_alt],&altsplitkey[num_alt]);

						key_name(altkey[num_alt],altkeyq[num_alt]); /* Weve got a key, test for edits	*/

						duplic[num_alt] = 0;			/* No duplicates.			*/
						num_alt++;
						write_log("WISP",'I',"COMSECALTKEY","Completed SECONDARY ALTERNATE RECORD KEY");
						o_parms[0][0] = 0;
						continue;
					}

					if (!strcmp(o_parms[0],"DUPLICATES"))		/* Last key allowed duplicates.		*/
						duplic[num_alt-1] = 1;

					if (!strcmp(o_parms[0],"ORGANIZATION"))		/* Process organization.		*/
					{
						ptype = get_kw(o_parms[0]);		/* Get next word.			*/
						if (!strcmp(o_parms[0],"INDEXED"))	/* FILE is indexed.			*/
						{
							write_log("WISP",'I',"ISINDEX","INDEXED file detected.");
							FSET(prog_ftypes[this_file],INDEXED_FILE);    /* It is an indexed file.	*/
							org = INDEX;			/* Set the mode.			*/
						}
						else if (!strcmp(o_parms[0],"RELATIVE"))
							org = RELATIVE;
						else if (!strcmp(o_parms[0],"SEQUENTIAL"))
						{
							seq_org_flag = 1;		/* We really found the SEQ keyword	*/
							org = SEQUENTIAL;
						}
						o_parms[0][0] = 0;
						continue;
					}
					if (!strcmp(o_parms[0],"ACCESS"))		/* Process access mode.			*/
					{
						ptype = get_param(o_parms[0]);		/* Get access mode.			*/
						if (!strcmp(o_parms[0],"MODE"))
							ptype = get_param(o_parms[0]);
						if (!strcmp(o_parms[0],"IS"))
							ptype = get_param(o_parms[0]);
						if (!strcmp(o_parms[0],"SEQUENTIAL"))
						{
							seq_acc_flag = 1;		/* We really found the SEQ keyword	*/
							accmod = SEQUENTIAL;		/* Set the mode.			*/
						}
						else if (!strcmp(o_parms[0],"RANDOM"))
							accmod = RANDOM;
						else if (!strcmp(o_parms[0],"DYNAMIC"))
							accmod = DYNAMIC;
						o_parms[0][0] = 0;
						continue;
					}
					if (!strcmp(o_parms[0],"STATUS"))		/* FILE STATUS				*/
					{
						write_log("WISP",'I',"BEGFILESTATUS","Begin FILE STATUS");
						ptype = get_kw(o_parms[0]);
						strcpy(prog_fstats[this_file],o_parms[0]);	/* save it in the table		*/
						write_log("WISP",'I',"COMFILESTATUS","Completed FILE STATUS.");
						o_parms[0][0] = 0;
						continue;
					}

					if (!strcmp(o_parms[0],"BUFFER"))		/* BUFFER SIZE IS NN BLOCKS phrase	*/
					{
						write_log("WISP",'I',"REMBUFSIZ","Remove BUFFER SIZE phrase.");
						ptype = get_param(o_parms[0]);
						if (!strcmp(o_parms[0],"SIZE"))		/* skip over SIZE			*/
						{
							ptype = get_param(o_parms[0]);
						}
						if (!strcmp(o_parms[0],"IS"))		/* skip over IS				*/
						{
							ptype = get_param(o_parms[0]);
						}
						o_parms[0][0] = '\0';			/* clear last word (size value)		*/
						continue;
					}

					if (!strcmp(o_parms[0],"RECORD"))		/* Get the record key.			*/
					{
						next_param(o_parms[0]);
						if (!strcmp(o_parms[0],"KEY"))		/* if KEY then skip it			*/
						{
							ptype = get_param(o_parms[0]);
						}

						ptype = get_record_key(reckey,reckeyq,&splitkey);
						key_name(reckey,reckeyq);

						write_log("WISP",'I',"RECKEYVAL","  RECORD KEY is %s",reckey);
						o_parms[0][0] = 0;
						continue;
					}

					if (!strcmp(o_parms[0],"RELATIVE"))		/* Get the relative key.		*/
					{
						ptype = get_param(relkey);
						if (!strcmp(relkey,"KEY"))		/* if KEY then skip it			*/
						{
							ptype = get_param(relkey);
						}
						if (!strcmp(relkey,"IS"))		/* if IS then skip it			*/
						{
							ptype = get_param(relkey);
						}
						if (ptype != -1)
						{
							next_param(relkeyq);
							if (!strcmp(relkeyq,"OF"))
							{
								ptype = get_param(relkeyq);	/* Get the "OF"			*/
								ptype = get_param(relkeyq);	/* Get the record		*/
							}
							else
							{
								relkeyq[0] = '\0';
							}
						}
						key_name(relkey,relkeyq);
						write_log("WISP",'I',"RELKEYVAL","  RELATIVE KEY is %s",relkey);
						o_parms[0][0] = 0;
						continue;
					}
											/* RESPECIFY phrase			*/
					if (!strcmp(o_parms[0],"RESPECIFY"))
					{
						write_log("WISP",'I',"REMBUFSIZ","Remove RESPECIFY phrase.");
						o_parms[0][0] = '\0';				/* clear the word 		*/
						continue;
					}

					if (!strcmp(o_parms[0],"NORESPECIFY") || !strcmp(o_parms[0],"OPTIONAL"))
					{
						write_log("WISP",'I',"RESPEC","Flagged NORESPECIFY/OPTIONAL phrase.");
						o_parms[0][0] = '\0';				/* clear the word 		*/
						prog_ftypes[this_file] |= NORESPECIFY;	/* set the flag bit			*/
						continue;
					}

					if (!strcmp(o_parms[0],"RESERVE"))		/* RESERVE NN AREAS phrase		*/
					{
						write_log("WISP",'I',"RESERVENN","Copy RESERVE NN AREAS phrase.");
						ptype = get_param(areas);
						if (ptype != -1)
						{
							ptype = next_param(o_parms[1]);	/* is next word AREAS?			*/
							if (!strcmp(o_parms[1],"AREAS") || !strcmp(o_parms[1],"AREA"))
							{
								ptype = get_param(o_parms[0]);	/* get the word AREAS		*/
							}
						}
					}
				}
				while (ptype != -1);					/* do it till we hit a LAST parm	*/

				if (del_sel()) return(0);				/* Quit if this file should be deleted.	*/

				if (!prog_fstats[this_file][0])				/* was there no STATUS?			*/
				{

					i = 0;
					if (nfs_count)					/* If there are files not needing stat	*/
					{
						do
						{					/* Look for it in the list.		*/
							if (!strcmp(prog_files[this_file],nfs_item[i])) break;
						} while (++i < nfs_count);
					}

					if (i >= nfs_count)				/* It was not there.			*/
					{
						write_log("WISP",'I',"NOSTATUS",
							"No file status for file %s, WISP-FILE-STATUS used.",
								prog_files[this_file]);
						strcpy(prog_fstats[this_file],"WISP-FILE-STATUS");	/* save the name	*/
					}
				}

				if (!org) org = SEQUENTIAL;
				if (!accmod) accmod = SEQUENTIAL;

				if (org == SEQUENTIAL && accmod == SEQUENTIAL) 
				{
					prog_ftypes[this_file] |= SEQ_FILE;
					prog_ftypes[this_file] |= SEQ_SEQ;
				}

				was_seq_dyn = 0;
				if (org == SEQUENTIAL && accmod == DYNAMIC && !relkey[0]) /* Is it SEQUENTIAL/DYNAMIC/no key?	*/
				{
					write_log("WISP",'W',"CHANGTOSEQ",
						"Changed SEQUENTIAL-DYNAMIC with no relative key to SEQUENTIAL-SEQUENTIAL for %s."
													,prog_files[this_file]);
					put_line("\n      *%WISP-I-CHANGED SEQUENTIAL-DYNAMIC TO SEQUENTIAL-SEQUENTIAL");
					accmod = SEQUENTIAL;				/* Change to SEQUENTIAL.		*/
					prog_ftypes[this_file] |= SEQ_FILE;
					prog_ftypes[this_file] |= SEQ_DYN;		/* set the flag bit			*/
					was_seq_dyn = 1;
				}


				write_sel(printer,this_file,org,accmod);		/* Write the SELECT statement		*/
											/* Now generated the statements.	*/

				if (chk_sort(prog_files[this_file]))			/* if a SORT file were done		*/
				{
					put_line(".\n\n");				/* End it all				*/
					return(0);
				}

				if (areas[0]) write_line	("\n             RESERVE      %s AREAS",areas);

											/* Write ORGANIZATION.			*/
				if (org == SEQUENTIAL && (seq_org_flag || *seqtype))			
					write_line("\n             ORGANIZATION IS %sSEQUENTIAL",seqtype);
				else if (org == INDEX)
					put_line  ("\n             ORGANIZATION IS INDEXED");
				else if (org == RELATIVE)
					put_line  ("\n             ORGANIZATION IS RELATIVE");

											/* Write ACCESS.			*/
				if (accmod == SEQUENTIAL && seq_acc_flag)
					put_line("\n             ACCESS MODE  IS SEQUENTIAL");
				else if (accmod == RANDOM)
					put_line("\n             ACCESS MODE  IS RANDOM");
				else if (accmod == DYNAMIC)
					put_line("\n             ACCESS MODE  IS DYNAMIC");

				if (multiplelock || (mf_aix && (org == INDEX || was_seq_dyn)))
					put_line("\n             LOCK MODE IS AUTOMATIC");
				if (multiplelock)
					put_line("\n             WITH LOCK ON MULTIPLE RECORDS");
				multiplelock = 0;

				if (reckey[0]) write_line	("\n             RECORD KEY   IS %s",reckey);
				if (reckeyq[0]) write_line	("\n                          OF %s",reckeyq);

				if (relkey[0]) write_line	("\n             RELATIVE KEY IS %s",relkey);
				if (relkeyq[0]) write_line	("\n                          OF %s",relkeyq);

				if (splitkey) 
				{
					write_line("\n%s",splitkey);
					free(splitkey);
					splitkey = NULL;
				}

				if (num_alt)
				{
					for (i=0; i<num_alt; i++)
					{
						put_line		("\n             ALTERNATE RECORD KEY IS");
						write_line		("\n               %s",altkey[i]);
						if (altkeyq[i][0])
							write_line	("\n               OF %s",altkeyq[i]);
						if (altsplitkey[i])
						{
							write_line("\n%s",altsplitkey[i]);
							free(altsplitkey[i]);
							altsplitkey[i] = NULL;
						}
						if (duplic[i]) 
							put_line	("\n               WITH DUPLICATES");
					}
				}


				if (reckey[0])						/* Put it into the altkey list		*/
				{
					strcpy(altkey[num_alt++],reckey);
					prog_keys[this_file].list = malloc(num_alt*40);/* Get a ptr to some mem.		*/
					memcpy(prog_keys[this_file].list,altkey,num_alt*40);	/* Save the list.		*/
					prog_keys[this_file].count = num_alt;		/* And the length.			*/
				}

				if (prog_fstats[this_file][0])				/* was there STATUS?			*/
				{
					write_line("\n             FILE STATUS  IS %s",prog_fstats[this_file]);
				}

				put_line(".\n\n");					/* End it all				*/

				if ( mf_compress )					/* If MF file compression is on		*/
				{
					put_line("      $SET DATACOMPRESS\"0\"\n");	/* Turn it off				*/
					mf_compress = 0;
				}
			}
		}
	}
	else
	{
		put_line(inline);							/* For now, just copy it.		*/
	}

}


int key_name( the_name, the_qual )							/* Change the name if on key_list	*/
char	*the_name;
char	*the_qual;
{
	char tstr[40];
	int i;

	for( i=0; i < kl_count; i++ )							/* Search the key_list			*/
 	{
		if ( !strcmp(key_list[i].name,the_name) )				/* Is it in the list?			*/
		{
			if ( !the_qual || (the_qual && !strcmp(key_list[i].qual,the_qual) ) )
			{
				make_fld(tstr,the_name,"K-");				/* prefix it with a K-			*/
				strcpy(the_name,tstr);					/* Now put it back.			*/
				return(i);						/* Say it's so.				*/
			}
		}
	}
	return(-1);
}

static ck_nodis()									/* See if next word is NODISPLAY	*/
{
	if (ptype != -1)								/* Not the end yet			*/
	{
		ptype = get_param(o_parms[2]);						/* get next parameter			*/
		if (!strcmp(o_parms[2],"NODISPLAY"))					/* remove NODISPLAY parms		*/
		{
			if (ptype != -1)						/* it was not the end			*/
			{
				o_parms[2][0] = '\0';					/* Zero it out.				*/
			}
		}
	}
}

static write_sel(printer,this_file,org,accmod)						/* Write initial part of SELECT.	*/
int printer,this_file,org,accmod;
{
	if (prog_ftypes[this_file] & AUTOLOCK)
	{
		if ( !(prog_ftypes[this_file] & SEQ_SEQ) )				/* If not SEQ/SEQ then			*/
		{
			prog_ftypes[this_file] ^= AUTOLOCK;				/* XOR the AUTOLOCK bit OFF		*/
			write_log("WISP",'E',"AUTOLOCK","File %s NOT SEQ/SEQ.",prog_files[this_file]);
		}

	}

	mf_compress = 0;
	if ( compressfile && mf_aix )							/* Turn on MF file compression		*/
	{
		put_line("      $SET DATACOMPRESS\"1\"\n");
		mf_compress = 1;							/* We are in a MF compress		*/
	}
											/* SEQ-SEQ are OPTIONAL.		*/
	if (	!nooptional &&
		!chk_sort(prog_files[this_file]) && 
		use_optional && 
		!printer && 
		(org == SEQUENTIAL) && 
		(accmod == SEQUENTIAL)			)
		write_line("\n           SELECT OPTIONAL %s",prog_files[this_file]);	/* select file				*/
	else
		write_line("\n           SELECT %s",prog_files[this_file]);		/* select file				*/

	nooptional = 0;									/* Reset the nooptional flag		*/

	if (acu_cobol || mf_aix)
	{										/* If it's acucobol, write the gen name.*/
		char	fnbuff[80];
		make_fld(fnbuff,prog_files[this_file],"N-");
		write_line      ("\n           ASSIGN TO %s",fnbuff);
		if ( compressfile && acu_cobol )
		{
			put_line("\n           WITH COMPRESSION");
		}
	}
	else if (lpi_cobol)								/* If lpi cobol.			*/
	{										/* Only write for printers.		*/
		if (printer) write_line("\n           ASSIGN TO PRINTER");
	}
	else
	{										/* write the name			*/
		write_line("\n           ASSIGN TO \"%s\"",prog_prname[this_file]);
	}
	compressfile = 0;								/* Reset the flag.			*/

	if (*selecttext)
	{
		put_line(selecttext);
		selecttext[0] = '\0';
	}
}

static int del_sel()									/* Determine if delete current SELECT.	*/
{
	int i,j;

	if (!prog_dscnt) return(0);							/* none to delete.			*/

	j = prog_cnt - 1;								/* Get which one.			*/

	for (i=0; i<prog_dscnt; i++)							/* Scan the list.			*/
	{
		if (!strcmp(prog_files[j],prog_dsel[i])) break;				/* Compare this file with the list.	*/
	}

	if (i == prog_dscnt) return(0);							/* Didn't find it.			*/

	write_log("WISP",'W',"DELSEL","Delete SELECT for file %s because no FD.",prog_dsel[i]);
	prog_cnt--;									/* Found it, Delete it.			*/
	put_line("\n");									/* Finish previous line.		*/
	return(1);
}

chk_sort(fname)										/* Check list of SORT files.		*/
char *fname;
{
	int i;

	if (sf_count)
	{
		for (i=0; i<sf_count; i++)
		{
			if (!strcmp(fname,sf_item[i])) return(1);			/* Found it.				*/
		}
	}

	return(0);									/* Not found.				*/
}


io_control()										/* Add an I-O-CONTROL section and insert*/
											/* APPLY LOCK-HOLDING statements to do	*/
											/* manual record locking.		*/
{
	int	i;
	int	wrote_io_control;
	int	need_period;

	wrote_io_control = 0;								/* haven't wrote paragraph name		*/
	need_period = (ioc_found == 2);							/* Do we need a period			*/

	for(i=0; i<prog_cnt; i++)							/* loop thru all files			*/
	{
		if ( !(prog_ftypes[i] & AUTOLOCK) )					/* if not auto-lock files then		*/
		{
			if (!ioc_found && !wrote_io_control)				/* If not yet written then		*/
			{
				put_line("       I-O-CONTROL.\n");			/* write the paragraph header		*/
				wrote_io_control = 1;
			}
											/* write the lock-holding clause	*/
			write_line	("           APPLY LOCK-HOLDING ON %s\n", prog_files[i]);
			need_period = 1;
		}
	}

	if (need_period)								/* if period is needed			*/
	{
		put_line("           .\n");
	}
}

/*
**	Routine:	get_record_key
**
**	Function:	To extract the record key from the input stream and return it as individual components.
**			It can handle qualified keys and split keys.
**
**	Input:		get_param()	- get the next param off the input stream 
**			next_param()	- look at the next param
**			get_kw()	- gets the next significant param
**
**	Output:		reckey		- the record key (unqualified)
**			reckeyqual	- the qualification for reckey or NULL
**			splitkey	- pointer to malloc memory that contains a printable split key string or NULL
**
**	Return:		ptype from get_param()
**
**	Warnings:	If splitkey is not NULL then the calling program is responsible for freeing the memory.
**
**	History:	5/15/92		Written by GSL
**
*/
static int get_record_key(reckey,reckeyqual,splitkey)
char	*reckey;
char	*reckeyqual;
char	**splitkey;
{
	int	ptype, o_ptype;
	char	buff[80];

	reckeyqual[0] = '\0';							/* Initialize these variables			*/
	*splitkey = NULL;

	ptype = get_kw(reckey);							/* Get the record key (skipping opt_IS)		*/

	if (ptype != -1)
	{
		next_param(buff);
		if (0==strcmp(buff,"OF"))					/* If next token is "OF" then is qualified	*/
		{
			ptype = get_param(buff);				/* Get the "OF"					*/
			ptype = get_param(reckeyqual);				/* Get the qualification			*/
		}
		else if (buff[0] == '=')					/* Split key   SK = K1 K2 K3 ...		*/
		{
			ptype = get_param(buff);				/* Get the "="					*/
			if (!(*splitkey = malloc(80*6)))			/* Malloc some space for split key		*/
			{
				write_log("WISP",'F',"MALLOC","Malloc failed in get_record_key().");
				exit_wisp(-1);
			}
			**splitkey = '\0';
			add2buff(*splitkey,"=",16,0);

			for(;;)
			{
				o_ptype = next_param(buff);			/* See if next token terminates the splitkey	*/
				if (	0==strcmp(buff,"ALTERNATE") 	||
					0==strcmp(buff,"WITH") 		||
					0==strcmp(buff,"DUPLICATES") 	||
					0==strcmp(buff,"FILE") 		||
					0==strcmp(buff,"STATUS") 	||
					isdigit(buff[0])		  )	/* a second alternate record key		*/
				{
					break;					/* end of split-key segment list		*/
				}
				ptype = get_param(buff);			/* Get the split-key segment			*/
				add2buff(*splitkey,buff,16,0);			/* Add it to the print buffer			*/
				if (ptype == -1)
				{
					break;
				}
			}
		}
	}

	return(ptype);
}

