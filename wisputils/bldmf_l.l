%{
/*
**      File:           bldmf_l.l
**
**      Purpose:        simple lex analyzer for COPY parse stuff
**      
**      Description:    there are only three tokens returned:
**                      T_COPY  (when COPY detected)
**                      T_INOF  (when IN or OF detected)
**                      T_VALUE (when any identifier is detected)
**                      Anything else is discarded, incl. newline,
**                      whitespace, comments and line nos.
**
**      History:        06/17/92         Written by JEC
**
*/
#define EXT extern 

#include <sys/types.h>
#include <fcntl.h>
#include <ctype.h>
#include "bldmf.h"
#ifdef ECHO
#undef ECHO
#endif
int yywrap(){ return 1; }
%}

ident [A-Za-z0-9]+
stuff \".*\"
copy [^A-Z"-]COPY[^A-Z"-]
in [^A-Z-]IN[^A-Z-]
of [^A-Z-]OF[^A-Z-]

%%
^.{6}[\*\/].*    ;
^.{6}            ;

{copy}  { yyless(yyleng-1); return T_COPY; }
{in} { yyless(yyleng-1); return T_INOF;}
{of} { yyless(yyleng-1); return T_INOF;}
{ident}  return T_VALUE;
{stuff} { yyless(yyleng-1); return T_QUOTED;}
. |
[ \t] |
\n ;
