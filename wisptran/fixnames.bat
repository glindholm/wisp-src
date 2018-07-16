@echo off
call veren wispxpic.c wisp_pic.C  

set names=call cli crtrw datad decl delet disp divs 
for %%N in ( %names% ) do call veren wtx%%N.c wt_%%N.c

set names=files free ident if input io locks opcls procd 
for %%N in ( %names% ) do call veren wtx%%N.c wt_%%N.c

set names=read scrn sort start utils write wsdat wsdiv
for %%N in ( %names% ) do call veren wtx%%N.c wt_%%N.c
