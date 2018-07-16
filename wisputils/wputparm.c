static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/* program wputparm.c														*/
/* shell/DCL callable putparm													*/

#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <varargs.h>

#include "idsistd.h"
#define EXT_FILEXT
#include "filext.h"
#include "wdefines.h"

#include "putparm.h"
#include "wisplib.h"
#include "wexit.h"
#include "level.h"

static char func;									/* Function E, D, or C.			*/
static char prname[9];									/* putparm parameter referance name	*/
static char plabel[9];									/* putparm label  			*/
static char rlabel[9];									/* putparm reference label  		*/
static char aidchar;									/* aid char to end getparm 		*/
static int keyw_cnt;									/* number of keywords specified		*/
static int usage_cnt;									/* usage count for PUTPARN.		*/

#ifdef SCO				/* This must be defined in the .umf file on the SCO/unix machine with CFLAGS = -DSCO	*/
#define PARMCNT 130
#endif
#ifdef MSDOS
#define PARMCNT 130
#endif
#ifndef PARMCNT
#define PARMCNT 250
#define PARMCNT250
#endif

/*
	NOTE:	On VMS there is a limit of 253 parameters.
		On SCO there is a limit of 131 parameters.
*/

#define MAXARGS 256

struct ps { char *pargv[MAXARGS]; };
struct ps  pstruct;									/* used to simulate varargs		*/

struct aidstruct 
{
	char pfstr[6];
	char pfbyte;
};

struct aidstruct aidlist[] =
{
	{ "HELP", '0' },
	{ "ENTER", '@' },
	{ "PF1", 'A' },
	{ "PF2", 'B' },
	{ "PF3", 'C' },
	{ "PF4", 'D' },
	{ "PF5", 'E' },
	{ "PF6", 'F' },
	{ "PF7", 'G' },
	{ "PF8", 'H' },
	{ "PF9", 'I' },
	{ "PF10", 'J' },
	{ "PF11", 'K' },
	{ "PF12", 'L' },
	{ "PF13", 'M' },
	{ "PF14", 'N' },
	{ "PF15", 'O' },
	{ "PF16", 'P' },
	{ "PF17", 'a' },
	{ "PF18", 'b' },
	{ "PF19", 'c' },
	{ "PF20", 'd' },
	{ "PF21", 'e' },
	{ "PF22", 'f' },
	{ "PF23", 'g' },
	{ "PF24", 'h' },
	{ "PF25", 'i' },
	{ "PF26", 'j' },
	{ "PF27", 'k' },
	{ "PF28", 'l' },
	{ "PF29", 'm' },
	{ "PF30", 'n' },
	{ "PF31", 'o' },
	{ "PF32", 'p' },
	{ "", (char)0 }
};      

static int usage();
static int capitalize();
static int getopts();
static int response_file();
static int cutarg();
static int getaid();

#ifdef unix
static int gid;
#endif



main(o_argc,o_argv)
int 	o_argc;										/* Original argc & argv			*/
char 	*o_argv[];
{
	int	argc;									/* Adjusted argc & argv			*/
	char	*argv[256];

	int4 *keyw_len;
	char *p;
	int pargc, retcode, i, j, kw_cnt_pos, pid;
	int level;

        /************* PATCH FOR ALPHA/OPENVMS **************/
        /* alpha returns argc of 2 when no parms are passed */
        /* so added a check to see if argc=2 and argv[1] is */
        /* NULL, then set argc to 1.                        */
        if (2 == o_argc && !o_argv[1])
        {
           o_argc = 1;
        }
        /****************************************************/

	if (o_argc < 2) usage();							/* Need 2 args for this stuff.		*/

	response_file(o_argc, o_argv, &argc, argv);					/* Process any response file.		*/

	/*
	**	The link-level a putparm is created at is very important as
	**	it determines when it is cleaned-up in an "unlink".
	**	The initglbs() routine will increment the link-level which we 
	**	don't want.  So we save the linklevel then restore it after the
	**	call to initglbs().
	**
	**	A putparm created from a startup script should have a link-level
	**	of 0 because the cobol program which follows it will have a 
	**	link-level of 1.
	**	
	*/
	level = linklevel();
	initglbs("WPUTPARM");								/* Init WISP parameters because not	*/
	setlevel(level);

	aidchar='@';									/* called from COBOL.			*/
	usage_cnt= -1;									/* Mark as NOT-SET			*/
	strcpy(plabel,"        ");							/* Set the PUTPARM label to blanks.	*/
	strcpy(rlabel,"        ");							/* Set the reference label to blanks.	*/
	getopts(&argc,argv);								/* See if any options specified.	*/
	if (argc < 2) usage();								/* Need 2 args for this stuff.		*/
	capitalize(argv[1]);

#ifdef unix
	gid=wgetpgrp();
	pid = getpid(0);
	if (pid == gid)
	{
		fprintf(stderr,"%%WPUTPARM-F-SHELL Group ID = Process ID; Use Bourne Shell or set %s=$$\n",WISP_GID_ENV);
		wexit(1);
	}
#endif

	func = toupper(argv[1][0]);							/* Test first character of command.	*/
	if (!strchr("EDCSIDG",func))
	{
		fprintf(stderr,"*** invalid command [%s]\n",argv[1]);
		usage();								/* Display how to use.			*/
	}

	if (!strcmp(argv[1],"CLEAR"))							/* A CLEAR command.			*/
	{
		if (argc > 2)								/* Note: plabel gotten with option flag	*/
		{
			fprintf(stderr,"*** use options when specifying label [%s]\n",argv[2]);
			usage();
		}
		j = 0;
		pstruct.pargv[j++] = "C";						/* start setting up structure		*/
		pstruct.pargv[j++] = plabel;
		pstruct.pargv[j++] = (char*)&retcode;
		wvaset(&j);
		PUTPARM(pstruct.pargv[0],pstruct.pargv[1],pstruct.pargv[2]);
		wswap(&retcode);
		if (retcode) fprintf(stderr,"*** PUTPARM return code = %d ***\n",retcode);
		wexit(retcode);
	}
	else if (!strcmp(argv[1],"SHOW"))						/* A SHOW command.			*/
	{
		show_parm_area();
		wexit(0);
	}
	else if (!strcmp(argv[1],"ISHOW"))						/* A SHOW command.			*/
	{
		ishow_parm_area();
		wexit(0);
	}
	else if (!strcmp(argv[1],"DUMP"))						/* A SHOW command.			*/
	{
		if ( argc > 2 )
		{
			dump_parm_area(argv[2]);
		}
		else
		{
			fprintf(stderr,"You must supply a filename with DUMP\nwputparm DUMP filename\n");
		}
		wexit(0);
	}
	else if (!strcmp(argv[1],"GET"))		/* unix: symbol=`wputparm -l label GET keyword`				*/
	{						/* VMS:  wputparm -l label GET &symbol=keyword [&symbol=keyword ...]	*/
		SHMH	*parm_area;
		char	buff[100];
		int	pos, i, len;

		if ( strcmp(plabel,"        ") == 0 )
		{
			fprintf(stderr,"Label is required with GET option\n");
			wexit(1);
		}

		if ( argc < 3 )
		{
			fprintf(stderr,"Keyword is required with GET option\n");
			wexit(1);
		}

#ifdef unix
		if ( argc > 3 )
		{
			fprintf(stderr,"Too many arguments\n");
			wexit(1);
		}
#endif

		parm_area = get_prb_area(NULL,plabel,1);				/* Find PRB for label			*/
		if (!parm_area)
		{
			fprintf(stderr,"Label not found [%s]\n",plabel);
			wexit(1);
		}

		for (pos = 2; pos < argc; ++pos) 
		{
			char	keyword[8+1];
			char	symbol[80];
			char	*ptr;
			int4	status;

			capitalize(argv[pos]);						/* Make args uppercase			*/

			ptr = strchr(argv[pos],'=');
			if (ptr)
			{
#ifdef VMS
				*ptr = '\0';
				ptr++;
				strcpy(symbol,argv[pos]);
				strncpy(keyword,ptr,8);
				keyword[8] = '\0';
#else
				fprintf(stderr,"Invalid syntax: \"=\" not valid in keyword\n");
				wexit(1);
#endif
			}
			else
			{
#ifdef VMS
				fprintf(stderr,"Invalid syntax: keyword=symbol required\n");
				wexit(1);
#else
				strncpy(keyword,argv[pos],8);
				keyword[8] = '\0';
#endif
			}


			if ( !search_parm_area(buff,keyword,sizeof(buff),parm_area))
			{
				fprintf(stderr,"Keyword not found [%s]\n",keyword);
				wexit(1);
			}
			for(i=sizeof(buff)-1;i>=0 && buff[i]==' ';i--);				/* find the length		*/
			len=i+1;
			buff[len]='\0';

#ifdef VMS
			status = setsymb(symbol,buff,len,0 );
			if ( status )
			{
				fprintf(stderr,"Unable to set the symbol [%s] to value [%s] status=%d\n",symbol,buff,status);
				wexit(1);
			}
#else
			for(i=0; i<len; i++) putchar( buff[i] );
#endif
		}
		wexit(0);
	}
	else if (func != 'E' && func != 'D')
	{
		fprintf(stderr,"Unknown command\n");
		wexit(1);
	}

	if (argc < 3) usage();								/* Need 3 args for this stuff.		*/

/*	for (i = 1; i < argc; ++i) capitalize(argv[i]); */

	strcpy(prname,argv[2]);								/* third parm is prname.		*/
	capitalize(prname);
	if (strchr(prname,'=') != '\0')							/* Is missing the prname parameter.	*/
	{
		fprintf(stderr,"*** missing prname\n");
		usage();
	}
	i = strlen(prname);
	if (i < 8) memset(&prname[i],' ',8-i);						/* Pad to 8 chars.			*/
	keyw_cnt = 0;            							/* and the number of keywords		*/
	j=0;
	
	pstruct.pargv[j++] = &func;							/* start setting up structure		*/
	if (usage_cnt>=0)
	{
		wswap(&usage_cnt);
		pstruct.pargv[j++] = (char *)&usage_cnt;
	}
	pstruct.pargv[j++] = prname;
	kw_cnt_pos = j++;
	for (i=3; i<argc; ++i, j+=3)							/* now loop and copy in keyword info	*/
	{
		if ((p = strchr(argv[i],'='))==NULL)					/* must be something=something		*/
		{
			fprintf(stderr,"*** invalid keyword pair [%s]\n",argv[i]);
			usage();
		}
		*p++ = (char)0;						    		/* replace the = with a null		*/
		keyw_len = (int4 *)calloc(1,sizeof(int4));				/* make a 4 byte int			*/ 
		*keyw_len = strlen(p);							/* store the keyword len there		*/
		wswap(keyw_len);
		pstruct.pargv[j] = (char *)malloc(9);					/* malloc space for keyword		*/
		capitalize(argv[i]);
		strncpy(pstruct.pargv[j],argv[i],8);					/* copy in keyword			*/
		pstruct.pargv[j+1] = p;							/* value pointer			*/
		pstruct.pargv[j+2] = (char *)keyw_len;					/* length				*/
		++keyw_cnt;

		
		if ( j >= PARMCNT - 3 )
		{
			fprintf(stderr,"Too many arguments\n");
			wexit(1);
		}
	}
	pstruct.pargv[j++] = &aidchar;
	pstruct.pargv[j++] = plabel;
	pstruct.pargv[j++] = rlabel;
	pstruct.pargv[j++] = (char *)&retcode;						/* space for retcode			*/
	wswap(&keyw_cnt);
	pstruct.pargv[kw_cnt_pos] = (char *)&keyw_cnt;					/* how many keywords?                   */
	wvaset(&j);									/* tell how many args			*/

#ifdef OLD
	This is not done because not all O/S allow it.
	PUTPARM(pstruct); 
#endif

	PUTPARM(pstruct.pargv[0],pstruct.pargv[1],pstruct.pargv[2],pstruct.pargv[3],pstruct.pargv[4],
		pstruct.pargv[5],pstruct.pargv[6],pstruct.pargv[7],pstruct.pargv[8],pstruct.pargv[9],
		pstruct.pargv[10],pstruct.pargv[11],pstruct.pargv[12],pstruct.pargv[13],pstruct.pargv[14],
		pstruct.pargv[15],pstruct.pargv[16],pstruct.pargv[17],pstruct.pargv[18],pstruct.pargv[19],
		pstruct.pargv[20],pstruct.pargv[21],pstruct.pargv[22],pstruct.pargv[23],pstruct.pargv[24],
		pstruct.pargv[25],pstruct.pargv[26],pstruct.pargv[27],pstruct.pargv[28],pstruct.pargv[29],
		pstruct.pargv[30],pstruct.pargv[31],pstruct.pargv[32],pstruct.pargv[33],pstruct.pargv[34],
		pstruct.pargv[35],pstruct.pargv[36],pstruct.pargv[37],pstruct.pargv[38],pstruct.pargv[39],
		pstruct.pargv[40],pstruct.pargv[41],pstruct.pargv[42],pstruct.pargv[43],pstruct.pargv[44],
		pstruct.pargv[45],pstruct.pargv[46],pstruct.pargv[47],pstruct.pargv[48],pstruct.pargv[49],
		pstruct.pargv[50],pstruct.pargv[51],pstruct.pargv[52],pstruct.pargv[53],pstruct.pargv[54],
		pstruct.pargv[55],pstruct.pargv[56],pstruct.pargv[57],pstruct.pargv[58],pstruct.pargv[59],
		pstruct.pargv[60],pstruct.pargv[61],pstruct.pargv[62],pstruct.pargv[63],pstruct.pargv[64],
		pstruct.pargv[65],pstruct.pargv[66],pstruct.pargv[67],pstruct.pargv[68],pstruct.pargv[69],
		pstruct.pargv[70],pstruct.pargv[71],pstruct.pargv[72],pstruct.pargv[73],pstruct.pargv[74],
		pstruct.pargv[75],pstruct.pargv[76],pstruct.pargv[77],pstruct.pargv[78],pstruct.pargv[79],
		pstruct.pargv[80],pstruct.pargv[81],pstruct.pargv[82],pstruct.pargv[83],pstruct.pargv[84],
		pstruct.pargv[85],pstruct.pargv[86],pstruct.pargv[87],pstruct.pargv[88],pstruct.pargv[89],
		pstruct.pargv[90],pstruct.pargv[91],pstruct.pargv[92],pstruct.pargv[93],pstruct.pargv[94],
		pstruct.pargv[95],pstruct.pargv[96],pstruct.pargv[97],pstruct.pargv[98],pstruct.pargv[99],
		pstruct.pargv[100],pstruct.pargv[101],pstruct.pargv[102],pstruct.pargv[103],pstruct.pargv[104],
		pstruct.pargv[105],pstruct.pargv[106],pstruct.pargv[107],pstruct.pargv[108],pstruct.pargv[109],
		pstruct.pargv[110],pstruct.pargv[111],pstruct.pargv[112],pstruct.pargv[113],pstruct.pargv[114],
		pstruct.pargv[115],pstruct.pargv[116],pstruct.pargv[117],pstruct.pargv[118],pstruct.pargv[119],
		pstruct.pargv[120],pstruct.pargv[121],pstruct.pargv[122],pstruct.pargv[123],pstruct.pargv[124],
		pstruct.pargv[125],pstruct.pargv[126],pstruct.pargv[127],pstruct.pargv[128],pstruct.pargv[129]
#ifdef PARMCNT250
		,pstruct.pargv[130],pstruct.pargv[131],pstruct.pargv[132],pstruct.pargv[133],pstruct.pargv[134],
		pstruct.pargv[135],pstruct.pargv[136],pstruct.pargv[137],pstruct.pargv[138],pstruct.pargv[139],
		pstruct.pargv[140],pstruct.pargv[141],pstruct.pargv[142],pstruct.pargv[143],pstruct.pargv[144],
		pstruct.pargv[145],pstruct.pargv[146],pstruct.pargv[147],pstruct.pargv[148],pstruct.pargv[149],
		pstruct.pargv[150],pstruct.pargv[151],pstruct.pargv[152],pstruct.pargv[153],pstruct.pargv[154],
		pstruct.pargv[155],pstruct.pargv[156],pstruct.pargv[157],pstruct.pargv[158],pstruct.pargv[159],
		pstruct.pargv[160],pstruct.pargv[161],pstruct.pargv[162],pstruct.pargv[163],pstruct.pargv[164],
		pstruct.pargv[165],pstruct.pargv[166],pstruct.pargv[167],pstruct.pargv[168],pstruct.pargv[169],
		pstruct.pargv[170],pstruct.pargv[171],pstruct.pargv[172],pstruct.pargv[173],pstruct.pargv[174],
		pstruct.pargv[175],pstruct.pargv[176],pstruct.pargv[177],pstruct.pargv[178],pstruct.pargv[179],
		pstruct.pargv[180],pstruct.pargv[181],pstruct.pargv[182],pstruct.pargv[183],pstruct.pargv[184],
		pstruct.pargv[185],pstruct.pargv[186],pstruct.pargv[187],pstruct.pargv[188],pstruct.pargv[189],
		pstruct.pargv[190],pstruct.pargv[191],pstruct.pargv[192],pstruct.pargv[193],pstruct.pargv[194],
		pstruct.pargv[195],pstruct.pargv[196],pstruct.pargv[197],pstruct.pargv[198],pstruct.pargv[199],
		pstruct.pargv[200],pstruct.pargv[201],pstruct.pargv[202],pstruct.pargv[203],pstruct.pargv[204],
		pstruct.pargv[205],pstruct.pargv[206],pstruct.pargv[207],pstruct.pargv[208],pstruct.pargv[209],
		pstruct.pargv[210],pstruct.pargv[211],pstruct.pargv[212],pstruct.pargv[213],pstruct.pargv[214],
		pstruct.pargv[215],pstruct.pargv[216],pstruct.pargv[217],pstruct.pargv[218],pstruct.pargv[219],
		pstruct.pargv[220],pstruct.pargv[221],pstruct.pargv[222],pstruct.pargv[223],pstruct.pargv[224],
		pstruct.pargv[225],pstruct.pargv[226],pstruct.pargv[227],pstruct.pargv[228],pstruct.pargv[229],
		pstruct.pargv[230],pstruct.pargv[231],pstruct.pargv[232],pstruct.pargv[233],pstruct.pargv[234],
		pstruct.pargv[235],pstruct.pargv[236],pstruct.pargv[237],pstruct.pargv[238],pstruct.pargv[239],
		pstruct.pargv[240],pstruct.pargv[241],pstruct.pargv[242],pstruct.pargv[243],pstruct.pargv[244],
		pstruct.pargv[245],pstruct.pargv[246],pstruct.pargv[247],pstruct.pargv[248],pstruct.pargv[249]
#endif /* PARMCNT250 */
		);  /* NOTE  on VMS there is a 253 parameter limit */

	if (keyw_cnt) free(keyw_len);							/* Free up memory allocated.		*/
	wswap(&retcode);
	if (retcode) fprintf(stderr,"*** PUTPARM return code = %d ***\n",retcode);
	wexit(retcode);
}

static usage()										/* self explanitory			*/
{
	fprintf(stderr,"Usage: wputparm command   [options   ] parameters\n");
	fprintf(stderr,"                ENTER    Create a putparm.\n");
	fprintf(stderr,"                DISPLAY  Create a putparm (force GETPARM screen to display).\n");
	fprintf(stderr,"                CLEAR    Delete a labeled putparm or delete all putparms.\n");
	fprintf(stderr,"                SHOW     Show existing putparms.\n");
	fprintf(stderr,"                ISHOW    Show internals.\n");
	fprintf(stderr,"                GET      Extract the value of a keyword from a labeled putparm.\n");
	fprintf(stderr,"                DUMP     Dump the putparm shared memory area to a file.\n");

	fprintf(stderr,"\n");
	fprintf(stderr,"       wputparm {ENTER  } [-a aidchar ] prname keyword=value [keyword=value ...]\n");
	fprintf(stderr,"                {DISPLAY} [-l label   ]\n");
	fprintf(stderr,"                          [-c count   ]\n");
	fprintf(stderr,"                          [-r reflabel]\n");

	fprintf(stderr,"\n");
	fprintf(stderr,"       wputparm  CLEAR    [-l label   ]\n");

	fprintf(stderr,"\n");
	fprintf(stderr,"       wputparm {SHOW }\n");
	fprintf(stderr,"                {ISHOW}\n");

	fprintf(stderr,"\n");
#ifdef VMS
	fprintf(stderr,"       wputparm  GET       -l label    &symbol=keyword [&symbol=keyword ...]\n");
#else
	fprintf(stderr,"       wputparm  GET       -l label    keyword\n");
#endif

	fprintf(stderr,"\n");
	fprintf(stderr,"       wputparm  DUMP                  filename\n");



	wexit(1);
}

static capitalize(p)
register char *p;
{
	while (*p)
	{
		*p = toupper(*p);
		++p;
	}
}

static getopts(cnt,vals)								/* If options specified then parse.	*/
int *cnt;
char *vals[];
{
	int i;

	for (i = 0; i < *cnt; )
	{
		if (vals[i][0]=='-')
	  	{
			switch (vals[i][1])
			{
				case 'a': case 'A':					/* Specified aid character.		*/
				{
					if (strlen(vals[i])==2)				/* is it -apfxx or -a pfxx 		*/
			  		{
						getaid(vals[i+1]);			/* it's -a pfxxx, get aid from next 	*/
				  		cutarg(i+1,cnt,vals);			/* arg, and cut it  			*/
				  	}
				  	else getaid(&vals[i][2]);			/* it's -apfxx 				*/
					cutarg(i,cnt,vals);				/* Remove arg from struct.		*/
					break;
				}
				case 'l': case 'L':					/* Specified PUTPARM label.		*/
				{
					if (strlen(vals[i])==2)
			  		{
						strncpy(plabel,vals[i+1],strlen(vals[i+1]));
						cutarg(i+1,cnt,vals);			/* Remove arg from struct.		*/
					}
					else strncpy(plabel,&vals[i][2],strlen(&vals[i][2]));
					capitalize(plabel);
					cutarg(i,cnt,vals);				/* Remove arg from struct.		*/
					break;
				}
				case 'r': case 'R':					/* Specified reference label.		*/
				{
					if (strlen(vals[i])==2)
			  		{
						strncpy(rlabel,vals[i+1],strlen(vals[i+1]));
						cutarg(i+1,cnt,vals);			/* Remove arg from struct.		*/
					}
					else strncpy(rlabel,&vals[i][2],strlen(&vals[i][2]));
					capitalize(rlabel);
					cutarg(i,cnt,vals);				/* Remove arg from struct.		*/
					break;
				}
				case 'c': case 'C':					/* Specified usage count.		*/
				{
					if (strlen(vals[i])==2)
					{
						usage_cnt = atoi(vals[i+1]);
						cutarg(i+1,cnt,vals);			/* remove arg form struct.		*/
					}
					else usage_cnt = atoi(&vals[i][2]);
					cutarg(i,cnt,vals);
					break;
				}
				default:						/* Display how to use command.		*/
				{
					usage();
					break;
				}
			}
		}
		else i++;
	}
}

static cutarg(pos,cnt,vals)								/* Remove the option from the arguments.*/
int pos, *cnt;
char *vals[];
{
	register int i;
	
	for (i=pos+1; i< *cnt; ++i) vals[i-1]=vals[i];
	vals[(*cnt)--] = NULL;
}

static getaid(str)										/* Get the aid character.	*/
char *str;
{
	register int i;
	char ustr[20];
	
	strcpy(ustr,str);
	capitalize(ustr);
	for (i = 0; aidlist[i].pfstr[0]; ++i)
	{
		if (!strcmp(ustr,aidlist[i].pfstr))
	  	{
			aidchar = aidlist[i].pfbyte;
			return;
	  	}
	}	
	aidchar='@';
}

/*
**	Routine:	response_file()
**
**	Function:	To handle a response file in the arg list.
**
**	Description:	This routine loads n_argc and n_argv from the original arg list plus any response file.
**			If an argument has the form "@filename" then the file is read for additional arguments.
**			This are inserted into targv in the order found.
**
**			This was done to handle MSDOS limit of 128 characters on a command line. 
**
**	Arguments:
**	o_argc		The original arg count
**	o_argv		The original arg list.
**	n_argc_ptr	The adjusted arg count. (pointer to it)
**	n_argv		The adjusted arg list.
**
**	Globals:
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	02/03/93	Written by GSL
**
*/
static response_file(o_argc,o_argv,n_argc_ptr,n_argv)
int	o_argc;
char	*o_argv[];
int	*n_argc_ptr;
char	*n_argv[];
{
	int	i;

	*n_argc_ptr = 0;
	for(i=0; i<o_argc; i++)								/* Loop thru each arg in list		*/
	{
		if (o_argv[i] && o_argv[i][0] == '@')					/* If a response file then process	*/
		{
			FILE	*fh;
			if (fh = fopen(&o_argv[i][1],"r"))				/* Open the response file.		*/
			{
				char	buff[256];
				while(1 == fscanf(fh,"%s",buff) )			/* Read next token			*/
				{
					n_argv[*n_argc_ptr] = (char *)malloc(strlen(buff)+1);	/* Malloc space for token	*/
					strcpy(n_argv[*n_argc_ptr],buff);			/* Copy token to arg list.	*/
					(*n_argc_ptr)++;
				}
				fclose(fh);
			}
			else								/* Unable to open response file		*/
			{
				fprintf(stderr,"%%WPUTPARM-F-RESPONSE Unable to open response file %s.\n",o_argv[i]);
				wexit(1);
			}
		}
		else									/* A regular argument.			*/
		{
			n_argv[(*n_argc_ptr)++] = o_argv[i];				/* Regular arg so just assign		*/
		}
	}
	n_argv[*n_argc_ptr] = NULL;							/* Add a null to the end of the list	*/

#ifdef TESTING
	for(i=0; i<*n_argc_ptr; i++)
	{
		printf("n_argv[%d] = [%s]\n",i,n_argv[i]);
	}
#endif /* TESTING */
}

#ifndef VMS
#include "wutils.h"
#endif /* !VMS */

/*
**	History:
**	$Log: wputparm.c,v $
**	Revision 1.13  1997/07/29 15:03:17  gsl
**	Fix problem with link-level in wputparm()
**	It was incorrectly incrementing the link-level in initglbs()
**	which caused the putparm to be marked with the wrong link-link
**	which caused the putparm to be deleted at the wrong "unlink".
**	
**	Revision 1.12  1997-06-10 14:57:28-04  scass
**	?Changed long to int4 for portability.
**
**	Revision 1.11  1996-08-22 20:34:53-04  gsl
**	Removed use of PGRPID.
**
**	Revision 1.10  1996-07-23 11:22:41-07  gsl
**	add headers
**
**	Revision 1.9  1996-07-23 11:13:11-07  gsl
**	drcs update
**
**
**
*/
