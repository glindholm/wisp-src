static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/* This routine is the MAIN routine for subroutines which are RUN with parameters.						*/
/* It accepts parameters from the command line and creates storage for them, then calls the subroutine.				*/
/* It can handle up to 32 parameters.												*/


#include <descrip.h>
#include <ssdef.h>

#define LIB$K_CLI_GLOBAL_SYM	2
#define MAX_PARMS	32
#define MAX_PLUS_SYMBOLS	16

extern char *malloc();

static struct	{
		char *parm[MAX_PARMS];							/* The parms to pass.			*/
	} the_parms;

static long tabtyp,len;
static char restr[256];									/* Descriptor to hold results.		*/
static $DESCRIPTOR(res,restr);
static char symstr[256];								/* Descriptor to hold current symbol.	*/
static $DESCRIPTOR(sym,symstr);

main(argc,argv)
int argc;										/* The number of arguments.		*/
char *argv[];										/* The argument pointers.		*/
{
	struct dsc$descriptor_s symdat[MAX_PLUS_SYMBOLS];				/* Array of ptrs to symbols data.	*/
	char argstr[256],pname[40];
	int i,j,offset,is_sub,pcount;
	int pend,pstart,tlen;
	long status;

	/*
	**	Patch for VMS/Alpha.
	**	If program run with no arguments then argc is being set to 2 instead of 1,
	**	and the argv[1] is null.
	**	Check if argc is 2 and argv[1] in null then change argc to 1.
	*/
	if (2==argc && !argv[1])
	{
		argc = 1;
	}

	for (i=0; i<MAX_PARMS; i++) the_parms.parm[i] = 0;


	is_sub = 0;									/* Assume no SUBROUTINE request.	*/
	offset = 1;									/* And parms start in position 1.	*/

	if (argc > 2)									/* Could be a SUBROUTINE request	*/
	{
		if (!strcmp(argv[1],"subroutine"))					/* Are we supposed to call a sub?	*/
		{
			strcpy(pname,argv[2]);						/* Yes, get the name.			*/
			strcat(pname,"       ");
			offset = 3;							/* offset to the first parm.		*/
			is_sub = 1;							/* set the flag.			*/
		}
	}

	if (!is_sub)									/* Was not a subroutine, use file name.	*/
	{
		if (!(i = strpos(argv[0],"]") + 1)) i = strpos(argv[0],":") + 1;	/* Look for closing bracket or colon.	*/
		else while(j=strpos(&argv[0][i],"]")+1) i+=j;				/* Set i to last of the "]"s.		*/

		j = strpos(&argv[0][i],".");						/* Find period.				*/
		if (j != -1) argv[0][i+j] = '\0';					/* Set to null.				*/
		strcpy(pname,&argv[0][i]);
		strcat(pname,"       ");
	}

	if ((argc-offset) > MAX_PARMS)
	{
		printf("Error, too many parms in command (%d), max is %d.\n",argc-offset,MAX_PARMS);
		exit();
	}

	if (argc > offset)								/* Any arguments to pass?		*/
	{
		for (i=offset; i<argc; i++)						/* Get them all.			*/
		{
			strcpy(argstr,argv[i]);						/* Copy the line data.			*/
			strcat(argstr,"+");						/* Add a plus.				*/

			pstart = 0;							/* First char in position 0.		*/
			pcount = 0;							/* Count how many plus parms there are.	*/
			tlen = 0;							/* Count total length of parm.		*/

			do
			{
				if (pcount == MAX_PLUS_SYMBOLS)
				{
					printf("Error, too many concat operators in command, max is %d.\n",MAX_PLUS_SYMBOLS);
					exit();
				}

				pend = strpos(&argstr[pstart],"+");			/* Get next plus.			*/
				memcpy(symstr,&argstr[pstart],pend);			/* Copy the current name.		*/
				sym.dsc$w_length = pend;				/* Count length from here.		*/

				symdat[pcount].dsc$b_dtype = DSC$K_DTYPE_T;		/* Set the descriptor type.		*/
				symdat[pcount].dsc$b_class = DSC$K_CLASS_S;
				symdat[pcount].dsc$w_length = 256;
				if ( !(symdat[pcount].dsc$a_pointer = malloc(256)))	/* Get mem for symbol data.		*/
				{
					printf("Error allocating memory for symbol data\n");
					exit();
				}

				status = lib$get_symbol(&sym,&symdat[pcount],&len,&tabtyp);	/* Get it's contents.		*/

				if (status != SS$_NORMAL)
				{
					printf("%WISP-F-ERRINSYM Error accessing symbol \"%s\", status is %d.\n",argv[i],status);
					exit();
				}

				symdat[pcount++].dsc$w_length = len;			/* Store actual length.			*/
				tlen = tlen + len;					/* Keep a total.			*/
				pstart = pstart + pend + 1;				/* Point beyond the '+'.		*/
			} while (argstr[pstart]);					/* Till no more chars.			*/

			if ( !(the_parms.parm[i-offset] = malloc(tlen)))		/* Get memory to put it in.		*/
			{
				printf("Error allocating memory for symbol data\n");
				exit();
			}

			pstart = 0;

			for (j=0; j<pcount; j++)					/* Append each portion.			*/
			{
				memcpy(&the_parms.parm[i-offset][pstart],symdat[j].dsc$a_pointer,symdat[j].dsc$w_length);
				pstart = pstart + symdat[j].dsc$w_length;		/* Point to next location to append.	*/
				free(symdat[j].dsc$a_pointer);				/* Free each temp ptr.			*/
			}
		}
	}

	wispsub(pname,the_parms);							/* Call the WISP routine.		*/


	if (argc > offset)								/* Any arguments to return?		*/
	{
		for (i=offset; i<argc; i++)						/* Get them all.			*/
		{
			strcpy(argstr,argv[i]);						/* Copy the line data.			*/
			strcat(argstr,"+");						/* Add a plus.				*/
			pstart = 0;							/* First char in position 0.		*/
			j = 0;

			do
			{
				pend = strpos(&argstr[pstart],"+");			/* Get next plus.			*/
				memcpy(symstr,&argstr[pstart],pend);			/* Copy the current name.		*/
				sym.dsc$w_length = pend;				/* Count length from here.		*/

				res.dsc$w_length = 255;
				status = lib$get_symbol(&sym,&res,&len,&tabtyp);	/* Get it's contents, to find its size.	*/

				if (status != SS$_NORMAL)
				{
					printf("%WISP-F-ERRINSYM Error accessing symbol \"%s\", status is %d.\n",argv[i],status);
					exit();
				}

				res.dsc$w_length = len;					/* Set the length.			*/
				memcpy(restr,&the_parms.parm[i-offset][j],len);		/* copy the new data.			*/

				tabtyp = LIB$K_CLI_GLOBAL_SYM;
				status = lib$set_symbol(&sym,&res,&tabtyp);		/* Now set it.				*/

				j = j + len;						/* Point to next bit of data.		*/
				pstart = pstart + pend + 1;				/* Point beyond the '+'.		*/
			} while (argstr[pstart]);					/* Till no more chars.			*/
			free(the_parms.parm[i-offset]);					/* Free the buffers.			*/
		}
	}
}
/*
**	History:
**	$Log: wisp_using_main.c,v $
**	Revision 1.5  1996-07-23 14:13:09-04  gsl
**	drcs update
**
**
**
*/
