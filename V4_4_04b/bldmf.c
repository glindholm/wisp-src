static char copyright[]="Copyright (c) 1992-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**      File:           bldmf.c
**
**      Purpose:        To determine dependencies in COBOL systems
**                      and write makefiles
**
**      Routines:       main()           main routine
**                      parse_args()     parse args
**                      bld_prog_list()  build list of progs
**                      bld_deps()       build deps for all prog
**                      is_cobol_prog()  determine if file is prog
**                      makedepend()     build deps for a singal prog
**                      search_line()    search a line for something
**                      make_runname()   create a target name 
**                      matchopt()       match a command line opt
**                      write_makefile() produce makefile output
**                      build_inc_dir_list()   build LIST of include dirs
**                      usage()          print usage info
**                      upstring()       convert a string to upper case
**                      lowstring()      convert a string to lower case
**                      init_globals()   initialize global flags
**                      process_globals() "fill in" globals after reading options
**                      my_strdup()	 dup a string
**                      srch_fixed()     search a buffer
**                      gmem()           alloc memory
**                      add_node()       add a node to a linked list
**                      sort_list()      sort a linked list
**                      list_size()      return count of items on a list
**                      compare_strs()   compare two strings
**
**      History:
**                      06/17/92        Written by JEC
**                      06/30/92        added -?, fixed loop on bad opt,
**                                      added usage on bad opt, fixed errlog behavior
**
**
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#define EXT
#define MAIN

#include "bldmf.h"

static char rcs_date[]		="$Date:$";
static char rcs_revision[]	="$Revision:$";
static char rcs_state[]		="$State: Exp $";
static char bldmf_version[80];

/**************************************************************
**  FUNCTION DECLARATIONS                                     
**/
#ifdef __STDC__

/* local routines */
int main( int argc, char* argv[] );
void parse_args( int argc, char* argv[] );
void bld_prog_list( LIST** plist );
void bld_deps( LIST** plist );
bool is_cobol_prog( char* path );
LIST* makedepend( char* path );
int search_line( char* buf, char* pat );
char* make_runname( char* name );
void matchopt( char* opt, int* num, int* type, char** destval, int* val );
void write_makefile( LIST** plist );
void build_inc_dir_list();
void usage();
void upstring(char* str);
void lowstring(char* str);
void init_globals();
void process_globals();
char *my_strdup(char* str );
int srch_fixed(char* string, char* substr );
char *gmem(int size, int cnt);
void add_node( LIST** listp, void* node );
void sort_list( LIST* listp, int (*sortroutine)( const void* s1, const void* s2 ) );
int compare_strs( const void *s1, const void *s2 );
int list_size( LIST* listp );

extern char *nextfile( char* path, char** context );
extern char *re_compile_pattern( char *regex, int regex_size, regexp_t compiled);
extern int re_search(regexp_t compiled, char *string, int size, int startpos, 
                     int range, regexp_registers_t regs);
extern void re_compile_fastmap(regexp_t compiled);


/* yylex from bldmf_l.l*/
extern int yylex();
#else

/* local routines */
int main();
void parse_args();
void bld_prog_list();
void bld_deps();
bool is_cobol_prog();
LIST* makedepend();
int search_line();
char* make_runname();
void matchopt();
void write_makefile();
void build_inc_dir_list();

void usage();
void upstring();
void lowstring();
void init_globals();
void process_globals();
char *my_strdup();
int srch_fixed();
char *gmem();
void add_node();
void sort_list();
int compare_strs();
int list_size();

extern char *nextfile();
extern char *re_compile_pattern();
extern int re_search();
extern void re_compile_fastmap();

/* yylex from bldmf_l.l*/
extern int yylex();

#endif

#ifndef LINUX
extern char *sys_errlist[];
extern int errno;
#endif

/*
** END OF FUNCTION DECLARATIONS                               
***************************************************************/

/*
**      Routine:        main()
**      Function:       main routine
**
**      Input:          argc, argv
**                      
**
**      Output:         COBOL makefile
**                      
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        06/17/92     written by JEC
**
*/
 main(argc,argv)
int argc;
char* argv[];
{
        LIST *proglist = NULL;

	make_version();
        parse_args(argc, argv);
        
        if (show_help)
          usage();

        bld_prog_list(&proglist);
        if (proglist == NULL)
        {
                fprintf(stderr,"No COBOL programs found.\n");
                exit(0);
        }
        bld_deps(&proglist);
        write_makefile(&proglist);
        return 0;
}

/*
**      Routine:        bld_prog_list()
**
**      Function:       Make a list of COBOL programs
**
**      Description:    Scans the source dir for COBOL programs
**
**      Input:          Pointer to List
**                      
**
**      Output:         List of COBOL programs
**                      
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        6/17/92         Written by JEC
**
*/
void bld_prog_list(plist)
LIST** plist;
{
        char *path=".";
        char *context=NULL;
        char *file;
        PROGNODE *prog;
        void *calloc();
        char *my_strdup();
        int ret;

        /* print status */
        if (!quiet_mode)
          fprintf(stderr,"building program list...\n");
        
        column=0;
        /* read files from dir */
        while (file = nextfile(path,&context))
        {
                /* check to see if it is COBOL program */
                ret=is_cobol_prog(file);
                
                /* non zero means it is */
                if (ret)
                {
                        prog = calloc(1,sizeof(PROGNODE));    /* get space for a PROGNODE */
                        prog->filename = my_strdup(file);     /* attach the program name to the PROGNODE */
                        if (!quiet_mode)                      /* print it on CRT for user to see */
                        {
                                fprintf(stderr,"%12s  ",prog->filename);
                                fflush(stderr);
                                column += 14;
                                if (column>60)
                                {
                                        column=0;
                                        fprintf(stderr,"\n");
                                }
                        }
                        add_node(plist,(void*)prog);          /* add it to our list */
                }
        }
}
/*
**      Routine:        is_cobol_prog()
**
**      Function:       determine if a file is a COBOL program
**
**      Description:    this function first looks at the filename.
**                      it must end with .wcb.  Next, it opens the
**                      file and searches HEADCNT lines into the
**                      file for IDENTIFICATION DIVISION. 
**
**      Input:          name - file name
**                      
**
**      Output:         None
**                      
**
**      Return:         1 (is program) or 0 (not program)
**
**      Warnings:       None
**
**      History:        06/17/92       Written by JEC
**
*/
bool is_cobol_prog(name)
char *name;
{
        FILE *f;
        int lcnt, nlen;
        char buffer[100];
        int found;
        char *re_stat, *re_compile_pattern();
        struct re_pattern_buffer the_pat;
        char fastmap[256];
        struct re_registers regs;

        /* 
           now do some checking to insure it's a cobol program name 
        */
        nlen=strlen(name);
        if (nlen<5)          /* x.wcb = 5 chars : shortest possible name */
          return FALSE;
        if (strncmp(name + nlen - 4, ".wcb", 4)) /* examine last 4 chars */
          return FALSE;
        
        f=fopen(name,"r");
        if ( NULL==f ) 
          return FALSE;
        
        /* init the pat struct a bit */
        the_pat.allocated = 0;
        the_pat.buffer = 0;
        the_pat.translate = NULL;
        the_pat.fastmap = fastmap;
        the_pat.translate = NULL;

        /* compile the pattern */
        re_stat = re_compile_pattern(COB_IDENT_PAT, strlen(COB_IDENT_PAT),&the_pat);

        /* 
          re_stat is NULL if success, otherwise is a pointer to an error message.
          re_compile_pattern should never fail here.
         */
        if (re_stat)
        {
                fprintf(stderr,"bldmf: PANIC: %s\n",re_stat);
                fprintf(stderr,"       Contact NeoMedia Customer Support.\n");
                exit(1);
        }

        /* presumably this speeds the search */
        re_compile_fastmap(&the_pat);

        /* now look HEADCNT lines into the file */
        for (found=FALSE, lcnt=0; lcnt < lines; ++lcnt)
        {
                int re_found;
                
                if (fgets(buffer, COBLEN, f)==0)
                  break;
                /* fold to upper case */
                upstring(buffer);

                /* do the search */
                re_found = re_search(&the_pat, buffer, strlen(buffer), 0, strlen(buffer), &regs);
                if (re_found >= 0)
                {
                        
                        found=TRUE;
                        break;
                }
        }               
        fclose(f);
        return found;
}

/*
**      Routine:        bld_deps()
**
**      Function:       To determine dependencies
**
**      Description:    Traverse the list of programs, get deps for each
**                      program.  A program's dependencies are its COPY 
**                      members
**
**      Input:          List of programs
**                      
**
**      Output:         Attaches dependency list to each node in program list
**                      
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        06/17/92        Written by JEC
**
*/
void bld_deps(plist)
LIST** plist;
{
        LIST *progs;
        
        if (!quiet_mode)
          fprintf(stderr,"\nbuilding dependency table...\n");

        /* travel down the list of programs */
        for (column=0, progs = (*plist)->next; progs; progs = progs->next)
        {
                if (!quiet_mode) /* print prog name */
                {
                        fprintf(stderr,"%12s  ",(char*)((PROGNODE*)progs->data)->filename);
                        fflush(stderr);
                        column += 14;
                        if (column>60)
                        {
                                column=0;
                                fprintf(stderr,"\n");
                        }
                }
                /* now get a deps list */
                ((PROGNODE*)progs->data)->deps = makedepend(((PROGNODE*)progs->data)->filename);
                /* and a runname */
                ((PROGNODE*)progs->data)->runname = make_runname(((PROGNODE*)progs->data)->filename);
        }
}

/*
**      Routine:        makedepend()
**
**      Function:       To make a dependency list
**
**      Description:    parses the program, looking for COPY statements.
**                      build a linked list of COPY members.  Uses a 
**                      state machine (table) defined in bldmf.h
**
**      Input:          program name
**                      
**
**      Output:         linked list of COPY members
**                      
**
**      Return:         linked list of COPY members
**
**      Warnings:       None
**
**      History:        06/17/92        Written by JEC
**
*/
LIST* makedepend(path)
char* path;
{
        LIST *deps=NULL;
        extern FILE *yyin;
        int copystate;

	/*
	 * the following line was changed from 
	 * extern char *yytext;
	 * changing it to the following fixed the 33x8 core dump 
 	 */

	extern char yytext[];
        char name[9];
        char lib[9];
        char *genpath(), *dpath;
        int eof;
        int token;

        line_number=0;
        current_prog = path;
        yyin = fopen(path,"r");
	nextwcbfile(); /* in bldmf_l.l file */
        /* first dep is the program's source */
        add_node(&deps,(void*)path);

        name[0]=0;
        lib[0]=0;
        
        dpath=NULL;

        /* start the state machine at NORMAL */
        copystate=S_NORMAL;
        for (eof=FALSE; eof==FALSE; )
        {
		extern int cont_str;
                token=yylex();  /* get a token from the cobol prog */
		if (cont_str)
		{
			continue;
		}

              reset:
                copystate = stab[token][copystate];   /* determine state */
                
                switch(copystate)      /* now do the action if any for current state */
                {
                      case S_COPYSTMT:
                        break;
                        
                      case S_COPYNAME:
			if (token == T_QUOTED)
			{
				char *p;
				
				strcpy(name,yytext+1);
				p=strchr(name,'"');
				if (p) 
				{
					*p=(char)0;
				}
			}
			else
			  strcpy(name,yytext);
                        break;

                      case S_INOF:
                        break;

                      case S_COPYLIB:
                        strcpy(lib,yytext);
                        break;                  
                        
                      case S_NORMAL:
                        break;
                        
                      case S_DONE:               /* done is a special state. */
                        dpath=genpath(name,lib); /* entering the done state is dependent on the current token */
                        copystate=S_NORMAL;      /* but done does not consume the token */
                        goto reset;              /* so we must reuse the current token */

                      case S_EXIT:
                        eof=TRUE;
                        break;

                }
                /* a valid dpath is a sign that a state or two ago we finished parsing 
                   a good copy statement */
                if (dpath)
                {
                        add_node(&deps,(void*)dpath); /* so add the dpath to our list */
                        dpath=0;   /* clear dpath */
                }
        }
        fclose(yyin);
        sort_list(deps,compare_strs);
        return deps; /* give back the list */
}

/*
**      Routine:        genpath()
**
**      Function:       To generate a copy member filename
**
**      Description:    build path and check its access.  If LIB
**                      is not NULL, use it to generate the path.
**                      Otherwise, if include_dirs was given 
**                      use it to search for file.  Also search '.'.
**
**      Input:          NAME - copy member  name 
**                      LIB  - copy member library (optional)
**
**      Output:         path of copy member
**                      
**
**      Return:         path of copy member
**
**      Warnings:       None
**
**      History:        06/17/92        Written by JEC
**
*/
char *genpath(name,lib)
char *name, *lib;
{
        char buf[200], *good;
        char *my_strdup();
        LIST *copydirs;
        extern LIST *inc_dir_list;

        good=NULL;
        lowstring(name);
        if (strlen(lib))  /* if a lib was given, use it */
        {
                lowstring(lib);
                sprintf(buf,"../%s/%s.wcb",lib,name);
                if (access(buf,0)==0)  
                  good = buf;
        }
        else
        {
                if (inc_dir_list)  /* go down the inc_dir_list */
                {
                        for (copydirs = inc_dir_list->next; copydirs; copydirs=copydirs->next)
                        {
                                sprintf(buf,"%s/%s.wcb",(char*)copydirs->data, name);
                                if (access(buf,0)==0)  /* if we find it, break */
                                {
                                        good=buf;
                                        break;
                                }
                        }
                }
                else
                {
                        sprintf(buf,"./%s.wcb", name);
                        if (access(buf,0)==0)
                          good=buf;
                }
        }
        if (!good) /* good means that the path was found */
        {
                if (column)
                {
                        fprintf(stderr,"\n");
                }
                fprintf(stderr,"ERROR: %11s:%-5d    Missing copy member: %s\n",current_prog,line_number,
                        buf);
                column=0;
        }
        
        return my_strdup(good); /* good really just points to buf[], so return a dup */
}

/*
**      Routine:        make_runname()
**
**      Function:       generate the target runname
**
**      Description:    generate the target runname
**
**      Input:          name of sourcefile
**                      
**
**      Output:         name of target
**                      
**
**      Return:         name of target
**
**      Warnings:       None
**
**      History:        06/17/92        Written by JEC
**
*/
char* make_runname(name)
char* name;
{
        char buf[200];
        char newname[9];
        char *p;

        memset(newname,0,sizeof(newname));
        p=strchr(name,'.');
        strncpy(newname,name,p-name); /* copy everything up to the period */
        upstring(newname);            /* and convert to uppercase */
        sprintf(buf,"$(RUNDIR)/%s%s",newname,runext);  /* add $(RUNDIR) and the .ext */
        return my_strdup(buf); /* return a dup */
}

/*
**      Routine:        write_makefile()
**
**      Function:       To write makefile
**
**      Description:    based on global flags and dep info,
**                      write makefile
**
**      Input:          Prog/dependency list, global flags (set by
**                      cmd line opts)
**
**      Output:         the Makefile
**                      
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        06/17/92        Written by JEC
**
*/
void write_makefile(plist)
LIST **plist;
{
        LIST *pp, *dp;
        FILE *output;
        char basename[32];
        char *p;
        char logerrstr[64];
        char output_name_old[256];
	int saved;
	
        /* if user wants an error log */
        strcpy(logerrstr,log_errors?" >>error.log 2>&1":"");

	if (access(output_name,0)==0)
	{
		sprintf(output_name_old,"%s.old",output_name);
		if (!quiet_mode) 
		{
			fprintf(stderr,"\nSaving %s ==> %s ",output_name,output_name_old);
			fflush(stderr);
		}
		if ((access(output_name_old,0)==0) && (access(output_name_old,02)!=0))
		{
			fprintf(stderr,"(failed due to no write permission)");
			fflush(stderr);
		}
		else
		{
			unlink(output_name_old);
			link(output_name,output_name_old);
			unlink(output_name);
		}
		
	}

        if (!quiet_mode)
          fprintf(stderr,"\nwriting %s...",output_name);

        output=fopen(output_name,"w");
        if (!output)
        {
                fprintf(stderr,"\nError opening %s for output\n",output_name);
                exit(2);
        }

        /* write preliminary info */
        fprintf(output,"#\n# Makefile generated by bldmf.\n#\n");
        fprintf(output,"SHELL = /bin/sh\n");
        fprintf(output,"COBOLTYPE = %s\n",cobol_type);
        fprintf(output,"WISPTRAN  = %s\n",wisp_path);
        if (strlen(include_dirs))
          fprintf(output,"WISPFLAGS = %s -V$(COBOLTYPE) -I%s\n",wisp_switches,include_dirs);
        else
          fprintf(output,"WISPFLAGS = %s -V$(COBOLTYPE)\n",wisp_switches);
        fprintf(output,"COBOL     = %s\n",cobol_command);
        fprintf(output,"COBFLAGS  = %s\n",cobol_switches);
        fprintf(output,"RUNDIR    = %s\n",run_dir);
        fprintf(output,"\n");
        
        /* now write all target */
        fprintf(output,"all:");
        
        for (pp = (*plist)->next; pp; pp=pp->next)
        {
                fprintf(output,"\t%s",((PROGNODE*)pp->data)->runname);
                if (pp->next)
                  fprintf(output,"\\\n");
                else 
                  fprintf(output,"\n\n");
        }
        /* now write individual targets for each program */
        for (pp = (*plist)->next; pp; pp=pp->next)
        {
                char *last_dep; /* use this to prevent repeating the deps if the prog */
                                /* includes the same member more than once */
               
                strcpy(basename,((PROGNODE*)pp->data)->filename);
                p = strchr(basename,'.');
                *p = (char)0; /* no error check because outname is checked for .wcb already */

                fprintf(output,"%s:",((PROGNODE*)pp->data)->runname); /* target */
                column = strlen(((PROGNODE*)pp->data)->runname) +1; /* len + ':' */
                
                /* next write deps for that target; traverse the deps list */
                for (last_dep=NULL, dp = ((PROGNODE*)pp->data)->deps; dp; dp=dp->next) 
                {
                        if (last_dep==NULL || strcmp(last_dep,(char*)dp->data))
                        {
                                fprintf(output,"  %s",(char*)dp->data);
                                column += strlen((char*)dp->data) +2;   /* len + "  " */
                                
                                if (column > MAX_DEP_WIDTH) /* write about four or five columns */
                                {
                                        if (dp->next)
                                          fprintf(output,"\\\n");
                                        else 
                                          fprintf(output,"\n");
                                        column = 0;
                                }
                        }
                        last_dep = (char*)dp->data;
                }
                if (column) 
                  fprintf(output,"\n");
                /* now write actions, starting with WISP */
		if (log_errors)
		  fprintf(output,"\t@echo Building %s >> error.log 2>&1\n",
			  ((PROGNODE*)pp->data)->runname);
                fprintf(output,"\t$(WISPTRAN) $(WISPFLAGS) %s %s\n",((PROGNODE*)pp->data)->filename,logerrstr);
                if (cob_type == TYPE_ACU)
                {
                        /* ACU cobol allows specification of the dest */
                        fprintf(output,"\t$(COBOL) $(COBFLAGS) -o %s %s.cob %s\n",
                                ((PROGNODE*)pp->data)->runname,
                                basename, logerrstr);
                }
                else
                {
                        /* MF doesn't allow spec of the output */
                        fprintf(output,"\t$(COBOL) $(COBFLAGS) %s.cob %s\n",basename,logerrstr);
                        /* so also write a move action to move to the runname */
                        fprintf(output,"\tmv %s%s %s\n",
                                basename,runext,((PROGNODE*)pp->data)->runname);
                }
                /* now add delete or compress action */
                if (delete_cobs)
                {
                        fprintf(output,"\trm %s.cob\n",basename);
                }
                if (compress_cobs)
                {
                        fprintf(output,"\tcompress %s.cob\n",basename);
                }
                fprintf(output,"\n");
        }
        fclose(output);
	fprintf(stderr,"\n");
}

/*
**      Routine:        parse_args()
**
**      Function:       To parse the args from the command line
**
**      Description:    To parse the args from the command line
**
**      Input:          ARGC  - number of args
**                      ARGV  - array of pointers to args
**
**      Output:         sets some global flags
**                      
**
**      Return:         None
**
**      Warnings:       None
**
**      History:        06/11/92   written by JEC
**
*/
void parse_args(argc,argv)
int argc;
char* argv[];
{
        int i,num,type,val;
        char *destval;

        init_globals();
        
        for (i=1;i<argc;)
        {
                /* see if current arg matches any legal opt */
                matchopt(argv[i],&num,&type,&destval,&val);

                /* if not, print err msg */
                if (num == -1)
                {
                        fprintf(stderr,"bad option \"%s\"\n",argv[i]);
			usage();
                }
                /* if type was int, copy value to receiver */
                if (type==OPT_INT) 
                  memcpy(destval,&val,sizeof(val));
                /* else get the string from the command line */
                else if (type==OPT_STRING) 
                {
                        /* handle both  "-oxxx" and  "-o xxx" */
                        if ( strlen(argv[i]) != strlen(optlist[num].opt) ) 
                          strcpy(destval,argv[i]+strlen(optlist[num].opt));
                        else 
                          strcpy(destval,argv[++i]);
                }
                ++i;
        }

        /* using include_dirs, build a linked list of include dirs */
        build_inc_dir_list();

        /* also, some additional processing of the globals depending on user's options */
        process_globals();

        if (delete_cobs && compress_cobs)
        {
                fprintf(stderr,"Warning: Options \"-dc\" (delete .cob files) and \"-cc\" (compress .cob files)\n");
                fprintf(stderr,"         are mutually exclusive.  Using \"-dc\"...\n\n");
                compress_cobs = 0;
        }

	lines = atoi(lines_str);
	if (!lines) 
	  lines=HEADCNT;
}
/*
**      Routine:        matchopt()
**
**      Function:       match a given option
**
**      Description:    To match an option, fill in
**                      the passed arguments from the opt struct
**
**      Input:          OPT   - pointer to the option from cmd line
**                      NUM   - index into option table
**                      TYPE  - data of option's value
**                      DESTVAL - pointer to option's receiver
**                      VAL   - value for this option (if any)
**
**      Output:         set input pointers as appropriate
**                      
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void matchopt(opt,num,type,destval,val)
char *opt,**destval;
int *num,*type,*val;
{
        struct optstruct *p;
        int i;

        /* go down opt list, looking for match of opt string */
        for(i=0,p=optlist;p->opt;++p,++i)
        {
                if (!strncmp(opt,p->opt,strlen(p->opt)))
                {
                        *num=i;
                        *type=p->type;
                        *destval=p->optdest;
                        *val=p->optval;
                        return;
                        
                }
        }
        *num= -1;
        *type = OPT_BAD;
}
/*
**      Routine:        build_inc_dir_list()
**
**      Function:       build linked list of include dirs
**
**      Description:    based on include_dirs, set on cmd line
**                      build linked list of include dirs
**
**      Input:          None
**
**      Output:         build linked list called inc_dir_list (defined
**                      globally)
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void build_inc_dir_list()
{
        char *p, *strtok();
        char *tmp_inc_dirs, *my_strdup();
        
        inc_dir_list=NULL;
        if (strlen(include_dirs)) /* if user gave a list of inc dirs */
        {
                tmp_inc_dirs = my_strdup(include_dirs); /* get a temp copy, because we still */
                                                     /* need the original for the WISPFLAGS later. */
                p = strtok(tmp_inc_dirs,":");        /* now break the list apart at the :'s */
                do
                {
                        add_node(&inc_dir_list,(void*)p);
                        
                } while (p = strtok(NULL,":"));
                add_node(&inc_dir_list,(void*)".");   /* add '.' last of all */
        }
}
/*
**      Routine:        gmem()
**
**      Function:       get memory
**
**      Description:    get memory
**
**      Input:          SIZE - size of object 
**                      CNT  - number of objects
**
**      Output:         pointer to mem area
**                      
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
char *gmem(size,cnt)
int size,cnt;
{
        void *calloc();
        
        return (char*)calloc(size,cnt);
}

/*
**      Routine:        upstring()
**
**      Function:       convert a string to upper case
**
**      Description:    convert a string to upper case
**
**      Input:          STR   - pointer to the string
**
**      Output:         string is converted to upper case
**                      
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void upstring(str)
char *str;
{
        while (*str) 
        {
                *str = toupper(*str);
                ++str;
        }
}
/*
**      Routine:        lowstring()
**
**      Function:       convert a string to lower case
**
**      Description:    convert a string to lower case
**
**      Input:          STR   - pointer to the string
**
**      Output:         string is converted to lower case
**                      
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void lowstring(str)
char *str;
{
        while (*str) 
        {
                *str = tolower(*str);
                ++str;
        }
}

/*
**      Routine:        my_strdup()
**
**      Function:       duplicate a string
**
**      Description:    given a null terminated C string
**                      produce a pointer to an identical string
**
**      Input:          STR   - pointer to the string
**
**      Output:         pointer to the new string
**                      
**
**      Return:         pointer to the new string
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
char *my_strdup(str)
char *str;
{
        char *gmem();
        char *tmp;

        if (str==NULL)
          return NULL;
        tmp = gmem(strlen(str)+1,1);
        strcpy(tmp,str);
        return tmp;
}

/*
**      Routine:        init_globals()
**
**      Function:       initialize global variables (mostly program
**                      options)
**
**      Description:    initialize global variables (mostly program
**                      options)
**
**      Input:          None
**
**      Output:         the variables are initialized
**                      
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void init_globals()
{
        inc_dir_list = NULL;
        current_prog = NULL;

        quiet_mode = FALSE;
        show_help  = FALSE;
        log_errors = FALSE;
        compress_cobs = FALSE;
        delete_cobs = FALSE;
        warn_missing = FALSE;

        cob_type = TYPE_ACU;

        strcpy(run_dir,"../run");      
        strcpy(src_dir,".");    
        strcpy(wisp_path,"wisp"); /* use "wisp" if not specified */
        strcpy(cobol_type,"ACU"); /* default to ACU if not specified */
        
        /* now zero these strings */
        memset(runext,0,sizeof(runext));
        memset(include_dirs,0,sizeof(include_dirs));
        memset(wisp_switches, 0, sizeof(wisp_switches)  );
        memset(cobol_switches,0, sizeof(cobol_switches));
        memset(cobol_command, 0, sizeof(cobol_command)  );
        memset(output_name, 0, sizeof(output_name));
        memset(lines_str, 0, sizeof(lines_str));
}
/*
**      Routine:        process_globals()
**
**      Function:       finish setup of global vars
**
**      Description:    after options have been processed,
**                      do remaining setup of global vars
**
**      Input:          none
**
**      Output:         the global vars are made ready for 
**                      the output process
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void process_globals()
{
        if (!quiet_mode)
		fprintf(stderr,"bldmf %s: Build makefiles for COBOL projects\n",bldmf_version);	
        
        upstring(cobol_type);
        
        if (!strcmp(cobol_type,"ACU"))
        {
                /* default here to ccbl */
                if (strlen(cobol_command)==0)
                {
                        strcpy(cobol_command,"ccbl");
                }
                /* and add -da4 if user didn't give it */
                if (srch_fixed(cobol_switches,"-da4") == -1)
                  strcat(cobol_switches," -da4");
        }
        else if (!strcmp(cobol_type,"MF"))
        {
                cob_type = TYPE_MF;
                /* default cmd to cob */
                if (strlen(cobol_command)==0)
                {
                        strcpy(cobol_command,"cob");
                }
                /* setup ext */
                strcpy(runext,".int");
                /* make sure -i -C warning=2 is there */
                if (srch_fixed(cobol_switches,"-i") == -1)
                  strcat(cobol_switches," -i");
                if (srch_fixed(cobol_switches,"-C warning=2") == -1)
                  strcat(cobol_switches," -C warning=2");
        }
        else
        {
                /* warning and exit */
                fprintf(stderr,"Error: Unknown COBOL type: %s.  Use ACU or MF\n",cobol_type);
                exit(2);
        }
        /* chdir to src dir if it was specified */
        if (strlen(src_dir))
        {
                if (chdir(src_dir)<0)
                {
                        fprintf(stderr,"bldmf: cannot change dir to %s\nreason: %s\n",
                                src_dir,
                                sys_errlist[errno]);
                        exit(1);
                }
        }
        /* setup output name if user didn't specify */
        if (strlen(output_name)==0)
        {
                strcpy(output_name,"Makefile");
        }
        if (access(output_name,0)==0)
	{
		if (access(output_name,02)!=0)
		{
			fprintf(stderr,"Error: No write access for output file %s\n",
				output_name);
			exit(2);
		}
	}
}
/*
**      Routine:        srch_fixed()
**
**      Function:       search a string for a substring
**
**      Description:    search a string for a substring
**
**      Input:          STRING   - pointer to the string
**                      SUBSTR   - pointer to the substring to 
**                                 be searched for 
**
**      Output:         index of the string or -1
**
**      Return:         index of the string or -1
**
**      Warnings:       This is very inefficient but is only
**                      being used for very small, one time searches
**
**      History:        06/11/92        Written by JEC
**
*/
int srch_fixed(string,substr) 
char *string, *substr;          
{
        int len,sublen,idx;
        
        len=strlen(string);
        sublen=strlen(substr);

        /* too simplistic , but what the heck. anything more is overkill here */
        for (idx=0; idx <= len-sublen; ++idx)
        {
                if (strncmp(string+idx,substr,sublen)==0)
                  return idx;
        }
        return -1;
}
/*
**      Routine:        usage()
**
**      Function:       print usage info for user
**
**      Description:    show all options and usage info
**
**      Input:          None
**
**      Output:         info on stdou
**                      
**      Return:         none
**
**      Warnings:       exits program
**
**      History:        06/11/92        Written by JEC
**
*/
void usage()
{
        fprintf(stderr,"usage: bldmf [args]\n\n");
        fprintf(stderr,"Possible args:\n");
        fprintf(stderr,"     -t <type>               Specify COBOL type: ACU (default) or MF\n");
        fprintf(stderr,"     -s <srcdir>             Directory containing source files\n");
        fprintf(stderr,"     -r <destdir>            Destination directory for run files\n");
	fprintf(stderr,"                                (default: \"../run\")\n");
        fprintf(stderr,"     -ws <wisp switches>     Additional switches for WISP command\n");
        fprintf(stderr,"     -wp <wisp path>         Path of WISP translator (default: \"wisp\")\n");
        fprintf(stderr,"     -cs <cob switches>      Additional switches for COBOL compile command\n");
        fprintf(stderr,"     -cm <cobol name>        Command to use to compile .cob files\n");
        fprintf(stderr,"     -I <include dir(s)>     Directory(s) containing copy books\n");
        fprintf(stderr,"     -l <line count>         Specify number of lines to search for\n");
	fprintf(stderr,"                                \"IDENTIFICATION DIVISION\"\n");
	fprintf(stderr,"                                (default: 500 lines)\n");
        fprintf(stderr,"     -dc                     Delete .cob files after successful compilation\n");
        fprintf(stderr,"     -cc                     Compress .cob files after compilation\n");
        fprintf(stderr,"     -e                      Log errors to error.log during make\n");
        fprintf(stderr,"     -o                      Output filename (default: \"Makefile\")\n");
        fprintf(stderr,"     -q                      Quiet mode\n");
        fprintf(stderr,"     -h                      This help message\n");
        exit(0);
}
/* 
 * LIST routines follow
 */
/*
**      Routine:        add_node()
**
**      Function:       add a node to a list
**
**      Description:    add data to a linked list
**
**      Input:          LIST  - pointer to pointer to the list
**                              if the pointer to the list is NULL,
**                              add node will create the list, otherwise
**                              add_node assumes the LIST pointer is valid
**                              
**                      NODE  - void pointer to the data
**
**      Output:         creates a new node for the data
**                      
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void add_node(listp,node)
LIST **listp; /* pointer to pointer to list head .. null if empty */
void *node;   /* data for new node */
{
  char *gmem();
  LIST *tmp;

  if (*listp) /* is list pointer already valid? */
    {
      tmp = (LIST*)((*listp)->list_tail);           /* find end of list */
      tmp->next = (LIST*)gmem(1,sizeof(LIST));      /* alloc a new thingy */
      tmp->next->prev = tmp;                        /* new thingy's prev must point to current node */
      tmp= tmp->next;                               /* now ready to work on new item */
      tmp->data = node;                             /* stash data in there */
      tmp->next = NULL;                             /* no next node */
      (*listp)->list_tail = tmp;                    /* update the last pointer */
    }
  else
    {
      *listp=(LIST*)gmem(1,sizeof(LIST));           /* get space for head (head node contains no data)*/
      (*listp)->next=(LIST*)gmem(1,sizeof(LIST));   /* get first item */
      (*listp)->prev= NULL;                         /* no prev node */
      (*listp)->list_tail = (void*)(*listp)->next;  /* init tail pointer to point last node in list */
      tmp = (*listp)->next;                         /* now fill in node */
      tmp->data = node;
      tmp->prev = *listp;
      tmp->next = NULL;
    }
}
/*
**      Routine:        sort_list()
**
**      Function:       sort a linked list
**
**      Description:    sort a linked list
**
**      Input:          
**	listp		pointer to  the list
**      sortroutine	comparison routine
**			receives two void ** pointers (called by qsort)
**			routine must deref twice to get to data item
**
**      Output:         rearranges the list in place
**
**      Return:         none
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
void sort_list( LIST* listp, int (*sortroutine)( const void* s1, const void* s2 ) )
{
  void **ptrs;
  char *gmem();
  int cnt, i;
  LIST *tmp;
  
  cnt=list_size(listp);                                /* find out how many items to sort */
  ptrs=(void**)gmem(cnt,sizeof(void*));                /* alloc array space for sort */
  for (tmp=listp->next, i=0; tmp;  tmp=tmp->next, ++i) /* load array with values */
    *(ptrs+i) = tmp->data;
  qsort((char*)ptrs,cnt,sizeof(tmp->data),sortroutine);/* sort array */
  for (tmp=listp->next, i=0; tmp;  tmp=tmp->next, ++i) /* copy sorted array back into list */
     tmp->data = *(ptrs+i);
  free(ptrs);                                          /* free space we grabbed for array */
}
/*
**      Routine:        list_size()
**
**      Function:       count items on a list
**
**      Description:    traverse list and count items
**
**      Input:          LISTP   pointer to list
**
**      Output:         items in list
**                      
**
**      Return:         items in list
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
int list_size(listp)
LIST *listp;
{
  int tmp;
  if (!listp) return 0;
  for(tmp=0; listp->next; ++tmp, listp=listp->next);
  return tmp;
}
/*
**      Routine:        compare_strs()
**
**      Function:       compare two strings
**
**      Description:    compare two strings with strmcmp
**
**      Input:          receives two pointer to pointer to char
**                      deref and strcmp
**
**      Output:         int return from strcmp
**                      
**
**      Return:         int return from strcmp
**
**      Warnings:       None
**
**      History:        06/11/92        Written by JEC
**
*/
int compare_strs(const void *s1, const void *s2)
{
	/*
	**	s1 and s2 are actually (char **)
	*/
        return strcmp( *((char **)s1), *((char **)s2) );
}

make_version()
{
	char	buff[80];
	
	bldmf_version[0] = (char)0;

	buff[0] = (char)0;
	sscanf(&rcs_state[1],"State: %s ",buff);
	strcat(bldmf_version,buff);
	strcat(bldmf_version," ");
	
	buff[0] = (char)0;
	sscanf(&rcs_revision[1],"Revision: %s ",buff);
	strcat(bldmf_version,buff);
	strcat(bldmf_version," ");
	
	buff[0] = (char)0;
	sscanf(&rcs_date[1],"Date: %s ",buff);
	strcat(bldmf_version,buff);
}
/*
**	History:
**	$Log: bldmf.c,v $
**	Revision 1.15.2.1  2002/09/05 19:22:23  gsl
**	LINUX
**	
**	Revision 1.15  1998/10/13 19:05:08  gsl
**	Fix sub arg to qsort)
**	
**	Revision 1.14  1997-06-02 09:18:27-04  gsl
**	Fix usage message
**
**	Revision 1.13  1997-06-02 09:14:16-04  gsl
**	Correct the usage/help message
**
**	Revision 1.12  1996-12-12 13:12:32-05  gsl
**	DevTech -> NeoMedia
**
**	Revision 1.11  1996-07-23 11:12:48-07  gsl
**	drcs update
**
**
**
*/
