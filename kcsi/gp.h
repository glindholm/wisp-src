/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef _GP_H
#define	_GP_H
#include "intdef.h"

/*----
PFKEY masks for building valid PFKEYS
------*/

#define	GP_PF_01	0x80000000
#define	GP_PF_02	0x40000000
#define	GP_PF_03	0x20000000
#define	GP_PF_04	0x10000000
#define	GP_PF_05	0x08000000
#define	GP_PF_06	0x04000000
#define	GP_PF_07	0x02000000
#define	GP_PF_08	0x01000000
#define	GP_PF_09	0x00800000
#define	GP_PF_10	0x00400000
#define	GP_PF_11	0x00200000
#define	GP_PF_12	0x00100000
#define	GP_PF_13	0x00080000
#define	GP_PF_14	0x00040000
#define	GP_PF_15	0x00020000
#define	GP_PF_16	0x00010000
#define	GP_PF_17	0x00008000
#define	GP_PF_18	0x00004000
#define	GP_PF_19	0x00002000
#define	GP_PF_20	0x00001000
#define	GP_PF_21	0x00000800
#define	GP_PF_22	0x00000400
#define	GP_PF_23	0x00000200
#define	GP_PF_24	0x00000100
#define	GP_PF_25	0x00000080
#define	GP_PF_26	0x00000040
#define	GP_PF_27	0x00000020
#define	GP_PF_28	0x00000010
#define	GP_PF_29	0x00000008
#define	GP_PF_30	0x00000004
#define	GP_PF_31	0x00000002
#define	GP_PF_32	0x00000001

/*----
A series of MACROs to shorten GETPARM definition
------*/
#define	GP gparg.ptr[gpcnt++] = (char*)

#define	GPNUM(num)			GP &GPINT[num]
#define	GPLEN(len)			GPNUM(len)
#define	GPAT(row,col)			GP "A";GPNUM(row);GP "A";GPNUM(col)
#define	GPFLD(fld,len,row,col)		GP fld;GPLEN(len);GPAT(row,col)
#define	GPTEXT(txt,ln,rw,cl)		GP "T";GPFLD(txt,ln,rw,cl)
#define	GPCTEXT(txt,rw,cl)		GPTEXT(txt,(strlen(txt)),rw,cl)
#define	GPKW(kw,rcv,ln,rw,cl,typ)	GP "K";GP kw;GPFLD(rcv,ln,rw,cl);GP typ
#define	GPID(pn,pf,mn,mi)		GP pn;GP pf;GP mn; GP mi
#define	GPTYP(tp,rq)			GP tp;GP rq
#define	GPTOP(tp,rq,pn,pf,mn,mi)	GPTYP(tp,rq);GPID(pn,pf,mn,mi);GPLEN(0)
#define	GPSETUP()			init_gpint();gpcnt=0
#define	GPSTD(pn,mi)			GPTOP("I ","R",pn,gppfrcvr,"0001",mi)
#define	GPDEF(pn,mi)			GPTOP("ID","R",pn,gppfrcvr,"0001",mi)
#define	GPFILE(fn,fr,fc)		GPFILEC(fn,fr,fc,"C")
#define GPDOC(fn,fr,fc)                 GPDOCC(fn,fr,fc,"C")
#define	GPLIB(ln,lr,lc)			GPLIBC(ln,lr,lc,"C")
#define	GPVOL(vn,vr,vc)			GPVOLC(vn,vr,vc,"C")
#define	GPFILEU(fn,fr,fc)		GPFILEC(fn,fr,fc,"L")
#define	GPLIBU(ln,lr,lc)		GPLIBC(ln,lr,lc,"L")
#define	GPVOLU(vn,vr,vc)		GPVOLC(vn,vr,vc,"L")
#define	GPFILEC(fn,fr,fc,cs)		GPKW("FILE    ",fn,8,fr,fc,cs)
#define	GPDOCC(fn,fr,fc,cs)		GPKW("DOCUMENT",fn,5,fr,fc,cs)
#define	GPLIBC(ln,lr,lc,cs)		GPKW("LIBRARY ",ln,8,lr,lc,cs)
#define	GPVOLC(vn,vr,vc,cs)		GPKW("VOLUME  ",vn,6,vr,vc,cs)
#define	GPXFC(x,fn,fr,fc,cs)		GPKW(x "FILE    ",fn,8,fr,fc,cs)
#define	GPXLC(x,ln,lr,lc,cs)		GPKW(x "LIBRARY ",ln,8,lr,lc,cs)
#define	GPXVC(x,vn,vr,vc,cs)		GPKW(x "VOLUME  ",vn,6,vr,vc,cs)
#define	GPXFLVC(x,fn,ln,vn,fr,fc)	GPXFC(x,fn,fr,fc,"C");GPXLC(x,ln,fr,fc+21,"C");GPXVC(x,vn,fr,fc+42,"C");
#define	GPMFC(fn,fr,fc,cs)		GPKW("MFILE   ",fn,8,fr,fc,cs)
#define	GPMLC(ln,lr,lc,cs)		GPKW("MLIBRARY",ln,8,lr,lc,cs)
#define	GPMVC(vn,vr,vc,cs)		GPKW("MVOLUME ",vn,6,vr,vc,cs)
#define	GPMFLVC(fn,ln,vn,fr,fc)	GPMFC(fn,fr,fc,"C");GPMLC(ln,fr,fc+21,"C");GPMVC(vn,fr,fc+42,"C");
#define	GPEXT(xn,xr,xc)			GPKW("FILEXT  ",xn,3,xr,xc,"C")
#define	GPFLV(fn,ln,vn,fr)		GPFLVC(fn,ln,vn,fr,3)
#define	GPFLVU(fn,ln,vn,fr)		GPFLVUC(fn,ln,vn,fr,3)
#define	GPFLVC(fn,ln,vn,fr,fc)	GPFILE(fn,fr,fc);GPLIB(ln,fr,fc+21);GPVOL(vn,fr,fc+42)
#define	GPDOCIDC(fn,vn,fr,fc)		GPDOC(fn,fr,fc);GPVOL(vn,fr,fc+21)
#define	GPFLVUC(fn,ln,vn,fr,fc)	GPFILEU(fn,fr,fc);GPLIBU(ln,fr,fc+21);GPVOLU(vn,fr,fc+42)
#define	GPSYSNAMEC(sn,sr,sc)		GPKW("SYSNAME ",sn,60,sr,sc,"C")
#define	GPXSYSNAMEC(x,sn,sr,sc)		GPKW(x "SYSNAME ",sn,60,sr,sc,"C")
#define	GPMSYSNAMEC(sn,sr,sc)		GPKW("MSYSNAME",sn,60,sr,sc,"C")
#define	GPSYSNAME(sn,sr)		GPSYSNAMEC(sn,sr,3)
#define	GPPFS(x)			GP "P";GP x
#define	GPENTER()			GP "E"
#define	GPNOENTER()			GP "N"


#define	GP_ROW_01			9


typedef struct _gparg {char *ptr[500];}GPARG;

/*----
The functions
------*/
int4	display_and_read_gp();

#ifdef	EXTERN_DEF
#undef	EXTERN_DEF
#endif

#ifdef	_GP_C
#define	EXTERN_DEF
#else
#define	EXTERN_DEF	extern
#endif

EXTERN_DEF	int4	GPINT[255];
EXTERN_DEF	GPARG	gparg;
EXTERN_DEF	int4	gpcnt;
EXTERN_DEF	int4	gppfkeys;
EXTERN_DEF	int4	gppfkey;
EXTERN_DEF	char	gppfrcvr[1];

#endif	/* _GP_H */

/*
**	History:
**	$Log: gp.h,v $
**	Revision 1.6  1996/09/17 23:34:08  gsl
**	drcs update
**	
**
**
*/
