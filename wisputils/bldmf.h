/*
**      File:           bldmf.h
**
**      Purpose:        header stuff for bldfm.c
**
**
**      History:        06/11/92         Written by JEC
**
**      RCS change log:
**
**      $Log: bldmf.h,v $
 * Revision 1.2  1992/12/23  19:49:37  jockc
 * added lines flag for headcnt and new state for state machine to handle
 * quoted copy name
 *
*/

#ifndef BLDMF_H
#define BLDMF_H

#if !defined(FALSE)
#define FALSE 0
#endif
#if !defined(TRUE)
#define TRUE !FALSE
#endif

/*
 * list related defines for add_node
 *
 */
typedef struct linked_list
{
  struct linked_list *next, *prev; /* next and prev pointers for each node */
  void *data;                      /* data for each node, last pointer for head node */
} linked_list;

#define LIST linked_list
#define list_tail data

#ifndef NULL
#define NULL 0
#endif


/********************************************************************
 * from regexpr.h
 */
#define RE_NREGS        10  /* number of registers available */

typedef struct re_pattern_buffer
{
  char *buffer;          /* compiled pattern */
  int allocated;         /* allocated size of compiled pattern */
  int used;              /* actual length of compiled pattern */
  char *fastmap;         /* fastmap[ch] is true if ch can start pattern */
  char *translate;       /* translation to apply during compilation/matching */
  char fastmap_accurate; /* true if fastmap is valid */
  char can_be_null;      /* true if can match empty string */
  char uses_registers;   /* registers are used and need to be initialized */
  char anchor;           /* anchor: 0=none 1=begline 2=begbuf */
} *regexp_t;

typedef struct re_registers
{
  int start[RE_NREGS];  /* start offset of region */
  int end[RE_NREGS];    /* end offset of region */
} *regexp_registers_t;

/********************************************************************/

typedef int bool;

/* max array lengths */
#define MAX_PATH 128
#define MAX_PATHS 256
#define MAX_SWITCHES 128

#define MAX_RUN_DIR             MAX_PATH
#define MAX_SRC_DIR             MAX_PATH
#define MAX_WISP_SWITCHES       MAX_SWITCHES
#define MAX_COBOL_SWITCHES      MAX_SWITCHES
#define MAX_INC_DIRS            MAX_PATHS
#define MAX_COBOL_COMMAND       MAX_PATH
#define MAX_OUTPUT_NAME         MAX_PATH
#define MAX_WISP_PATH           MAX_PATH
#define MAX_COBOL_TYPE          8

EXT char run_dir[MAX_RUN_DIR];
EXT char src_dir[MAX_SRC_DIR];
EXT char wisp_switches[MAX_WISP_SWITCHES];
EXT char cobol_switches[MAX_COBOL_SWITCHES];
EXT char include_dirs[MAX_INC_DIRS];
EXT char cobol_command[MAX_COBOL_COMMAND];
EXT char output_name[MAX_OUTPUT_NAME];
EXT char wisp_path[MAX_WISP_PATH];
EXT char cobol_type[MAX_COBOL_TYPE];

EXT char runext[4];

EXT int cob_type;

EXT int warn_missing ;
EXT int delete_cobs  ;
EXT int compress_cobs;
EXT int log_errors   ;
EXT int show_help    ;
EXT int quiet_mode   ;

EXT LIST *inc_dir_list;
EXT char *current_prog;

EXT int column;

#define MAX_LINES_STR 5
EXT char lines_str[MAX_LINES_STR+1];
EXT int lines;

#define TYPE_ACU 1
#define TYPE_MF 2

struct optstruct
{
        char *opt;
        int type;
        char *optdest;
        int optmax;
        int optval;
};

#define OPT_INT 1
#define OPT_STRING 2
#define OPT_BAD -1

#ifdef MAIN
struct optstruct optlist[]=
{     /* option      type           receiver            length (if str)   value (if int) */
        { "-s",    OPT_STRING,         src_dir,         MAX_SRC_DIR,        0 },
        { "-r",    OPT_STRING,         run_dir,         MAX_RUN_DIR,        0 },
        { "-ws",   OPT_STRING,         wisp_switches,   MAX_WISP_SWITCHES,  0 },
        { "-cs",   OPT_STRING,         cobol_switches,  MAX_COBOL_SWITCHES, 0 },
        { "-cm",   OPT_STRING,         cobol_command,   MAX_COBOL_COMMAND , 0 },
        { "-I",    OPT_STRING,         include_dirs,    MAX_INC_DIRS,       0 },
        { "-o",    OPT_STRING,         output_name,     MAX_OUTPUT_NAME,    0 },
        { "-t",    OPT_STRING,         cobol_type,      MAX_COBOL_TYPE,     0 },
        { "-l",    OPT_STRING,         lines_str,       MAX_LINES_STR,      0 },
        { "-wp",   OPT_STRING,         wisp_path,       MAX_WISP_PATH,      0 },
        { "-warn", OPT_INT,    (char*)&warn_missing,    0,               TRUE },
        { "-dc",   OPT_INT,    (char*)&delete_cobs,     0,               TRUE },
        { "-cc",   OPT_INT,    (char*)&compress_cobs,   0,               TRUE },
        { "-e",    OPT_INT,    (char*)&log_errors,      0,               TRUE },
        { "-q",    OPT_INT,    (char*)&quiet_mode,      0,               TRUE },
        { "-h",    OPT_INT,    (char*)&show_help,       0,               TRUE },       
        { "-?",    OPT_INT,    (char*)&show_help,       0,               TRUE },       
        { NULL,    0, NULL, 0, 0 }
};
#else
EXT struct optstruct optlist[];

#endif

/* max width for dependency lists */
#define MAX_DEP_WIDTH 60

/* lines to search into a .wcb file for IDENTIFICATION... */
#define HEADCNT 500
#define COBLEN 80
#define COB_IDENT_PAT "IDENTIFICATION +DIVISION"


#define PROGNODE struct pnode
PROGNODE 
{
        char *filename;
        char *runname;
        LIST *deps;
};

/* state machine for parsing out copy statements */

#define T_COPY 1
#define T_INOF 2
#define T_VALUE 3
#define T_QUOTED 4

#define S_NORMAL 0
#define S_COPYSTMT 1
#define S_COPYNAME 2
#define S_INOF 3
#define S_COPYLIB 4
#define S_DONE 5
#define S_EXIT 6
#ifdef MAIN
int stab[5][7] =
{         /* S_NORMAL        S_COPYSTMT         S_COPYNAME   S_INOF,    S_COPYLIB       S_DONE */
/*(eof)*/   { S_EXIT,          S_EXIT,           S_DONE,      S_EXIT,      S_DONE,         S_EXIT,     0,   },
/*T_COPY*/  { S_COPYSTMT,     S_COPYNAME,       S_DONE,      S_COPYLIB,   S_DONE,         S_NORMAL,    0,  },
/*T_INOF*/  { S_NORMAL,       S_COPYNAME,       S_INOF,      S_COPYLIB,   S_DONE,         S_NORMAL,    0,  },
/*T_VALUE*/ { S_NORMAL,       S_COPYNAME,       S_DONE,      S_COPYLIB,   S_DONE,         S_NORMAL,    0,  },
/*T_QUOTED*/{ S_NORMAL,       S_COPYNAME,       S_DONE,      S_COPYLIB,   S_DONE,         S_NORMAL,    0,  }
};
#endif

#endif


