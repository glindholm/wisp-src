page 60,132
;************************************************************************
;*                                                                      *
;* WISPRT - Define user supplied call-by-name routines                  *
;* Copyright (C) Micro Focus 1989                                       *
;*                                                                      *
;* Copyright (C) International Digital Scientific, Inc. 1991		*
;*                                                                      *
;************************************************************************

	name    WISPRT
	title   WISPRT - user calls definition module
.xlist
	include INSTALLF.MAC
.list
;************************************************************************
;*                                                                      *
;* Define here a list of all the subroutines you wish to make           *
;* available to your application. Each line should be of one of         *
;* the forms :                                                          *
;*                                                                      *
;*              ADDCALL assembler-routine-name                          *
;*              ADDCFUN c-function-name                                 *
;*              ADDAPI  os/2-api-routine-name                           *
;*                                                                      *
;* See the definitions below for examples of each                       *
;*                                                                      *
;************************************************************************
;*                                                                      *
;	ADDCALL CALCSUM		; example - define assembler subroutine	*
;	ADDCFUN CALCMEAN	; example - define MS C6.0 function	*
;	ADDAPI  DOSGETVERSION	; example - define OS/2 API routine	*
;*                                                                      *
;************************************************************************

;
;	WISP functions:
;
;	ADDCFUN ACUGARGS	; Only needed for ACU-COBOL version.
;	ADDCFUN ACUNARGS	; Only needed for ACU-COBOL version.
;	ADDCFUN ACUPARGS	; Only needed for ACU-COBOL version.
;	ADDCFUN SYSTEM		; Only needed for ACU-COBOL version.
	ADDCFUN BELL 
	ADDCFUN BITPACK 
	ADDCFUN BITUNPK 
	ADDCFUN COBLINK         
	ADDCFUN CANCEL  
	ADDCFUN CEXIT   
	ADDCFUN DATE 
	ADDCFUN DAY 
	ADDCFUN EXTRACT         
	ADDCFUN FILECOPY 
	ADDCFUN FIND 
	ADDCFUN GETPARM 
	ADDCFUN getparmbuild 
	ADDCFUN PUTPARM 
	ADDCFUN HEXPACK 
	ADDCFUN HEXUNPK 
	ADDCFUN LINK 
	ADDCFUN LINKMF
	ADDCFUN LINKPROC 
	ADDCFUN LOGOFF
;	ADDCFUN MESSAGE 
	ADDCFUN NOHELP 
	ADDCFUN ONHELP 
	ADDCFUN PRINT 
	ADDCFUN RETCODE 
	ADDCFUN READFDR 
;	ADDCFUN READVTOC 
	ADDCFUN SCRATCH 
	ADDCFUN SCREEN 
	ADDCFUN SEARCH 
	ADDCFUN SET 
;	ADDCFUN SETSUBMIT
	ADDCFUN SETTRIGPROG 
	ADDCFUN SORT 
	ADDCFUN SORTCALL 
	ADDCFUN SORTINFO 
	ADDCFUN SORTLINK 
	ADDCFUN STRING 
;	ADDCFUN SUBMIT
	ADDCFUN UPDATFDR 
;	ADDCFUN UPPER 
	ADDCFUN WACCEPT 
	ADDCFUN WCHAIN 
	ADDCFUN WDISPLAY 
	ADDCFUN WISPEXIT 
	ADDCFUN WISPSHUT 
	ADDCFUN WISPSYNC 
	ADDCFUN WISPSORT 
	ADDCFUN WSXIO 
	ADDCFUN menu 
	ADDCFUN wrename 
	ADDCFUN w2rowcol 
	ADDCFUN wscreen 
	ADDCFUN mwconv 
	ADDCFUN initwisp 
	ADDCFUN initwisp2 
	ADDCFUN bit_on 
	ADDCFUN bit_off  
	ADDCFUN lbit_on 
	ADDCFUN lbit_off 
	ADDCFUN bit_test 
	ADDCFUN wfname 
	ADDCFUN wfopen 
	ADDCFUN wfopen2 
	ADDCFUN wfopen3		; New in version 3.0
	ADDCFUN wfclose 
	ADDCFUN wdinit 
	ADDCFUN wdfinish 
	ADDCFUN wfwait 
	ADDCFUN wfswait 
	ADDCFUN wmemcpy 
	ADDCFUN wvaset 
	ADDCFUN setwfilext 
	ADDCFUN getwfilext 
	ADDCFUN setretcode 
	ADDCFUN wexith 
	ADDCFUN vexit 
	ADDCFUN wfilechk 
	ADDCFUN vwang 
	ADDCFUN greclen 
	ADDCFUN setrunname 
	ADDCFUN setwispfilext
	ADDCFUN setprogid 
	ADDCFUN wpause 
	ADDCFUN wsetstat 
	ADDCFUN xx2byte 
	ADDCFUN ws80 
	ADDCFUN ws132 

;
;	NETRON CAP routines:	; NOT INSTALLED.
;
;	ADDCFUN WSCLOSE
;	ADDCFUN WSFNM
;	ADDCFUN WSFNS

;
;	WISPTEST routines:
;
	ADDCFUN MESSUP
	ADDCFUN QASCROLL
	ADDCFUN QATSTLST
	ADDCFUN QAGP255

;
;	ACP routines:		; NOT INSTALLED.
;
;	ADDCFUN OPENACP
;	ADDCFUN CLOSEACP
;	ADDCFUN READACP
;	ADDCFUN WRITEACP
;	ADDCFUN BREAKACP
;	ADDCFUN CHECKACP
;	ADDCFUN GETACP
;	ADDCFUN SETACP

;
;	EDE routines:		; NOT INSTALLED.
;
;	ADDCFUN ws_bar_menu
;	ADDCFUN EDLOAD
;	ADDCFUN EDEXIT
;	ADDCFUN EDCLRSCR
;	ADDCFUN EDDRKSCR
;	ADDCFUN EDLTESCR
;	ADDCFUN EDWIDSCR
;	ADDCFUN EDNARSCR
;	ADDCFUN TRACEGO
;	ADDCFUN TRACEEND
;	ADDCFUN RETRACE
;	ADDCFUN PFKEYSON
;	ADDCFUN NOPFKEYS
;	ADDCFUN MENUSAVE
;	ADDCFUN MENUREST
;	ADDCFUN MENULOAD
;	ADDCFUN MENUITEM
;	ADDCFUN MENUGO
;	ADDCFUN MENUCONT
;	ADDCFUN DYLINK
;	ADDCFUN DYUNLINK
;	ADDCFUN MENUKILL
;	ADDCFUN MENUEXIT
;	ADDCFUN MENUMODE
;	ADDCFUN VIDMOVE
;	ADDCFUN VIDMODE
;	ADDCFUN VIDLINE
;	ADDCFUN VIDTEXT
;	ADDCFUN PUSHSCRN
;	ADDCFUN PUSHAREA
;	ADDCFUN POPAREA
;	ADDCFUN MENUINFO
;	ADDCFUN gcalc
;	ADDCFUN gcalend
;	ADDCFUN gclock
;	ADDCFUN gnotepad
;	ADDCFUN gpuzzle


	end			; end of source module WISPRT

