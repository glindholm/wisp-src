/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

#ifndef _GP_H
#define	_GP_H

#include "idsistd.h"

void init_gpint(void);
int4 display_and_read_gp(void);

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
#define	GP gparg[gpcnt++] = (char*)

#define	GPNUM(num)			GP &GPINT[num]
#define	GPLEN(len)			GPNUM(len)
#define	GPAT(row,col)			GP "A";GPNUM(row);GP "A";GPNUM(col)
#define	GPFLD(fld,len,row,col)		GP fld;GPLEN(len);GPAT(row,col)
#define	GPTEXT(txt,ln,rw,cl)		GP "T";GPFLD(txt,ln,rw,cl)
#define	GPCTEXT(txt,rw,cl)		GPTEXT(txt,(strlen(txt)),rw,cl)
#define	GPTEXTU(txt,ln,rw,cl)		GP "U";GPFLD(txt,ln,rw,cl)
#define	GPCTEXTU(txt,rw,cl)		GPTEXTU(txt,(strlen(txt)),rw,cl)
#define	GPKW(kw,rcv,ln,rw,cl,typ)	GP "K";GP kw;GPFLD(rcv,ln,rw,cl);GP typ
#define	GPRESP(kw,rcv,ln,rw,cl,typ)	GP "R";GP kw;GPFLD(rcv,ln,rw,cl);GP typ
#define	GPID(pn,pf,mn,mi)		GP pn;GP pf;GP mn; GP mi
#define	GPTYP(tp,rq)			GP tp;GP rq
#define	GPTOP(tp,rq,pn,pf,mn,mi,mc)	GPTYP(tp,rq);GPID(pn,pf,mn,mi);GPLEN(mc);
#define	GPSETUP()			init_gpint();gpcnt=0
#define	GPSTD(pn,mi,mc)			GPTOP("I ","R",pn,gppfrcvr,"0001",mi,mc)
#define GPRES(pn,mi,mc)                 GPTOP("R ","R",pn,gppfrcvr,"0001",mi,mc)
#define GPHIDDEN(pn,mi,mc)              GPTOP("ID","R",pn,gppfrcvr,"0001",mi,mc)
#define GPMSG(ln)                       GP ln; GPLEN(strlen(ln))
#define	GPFILE(fn,fr,fc)		GPKW("FILE    ",fn,8,fr,fc,"U")
#define	GPLIB(ln,lr,lc)			GPKW("LIBRARY ",ln,8,lr,lc,"U")
#define	GPVOL(vn,vr,vc)			GPKW("VOLUME  ",vn,6,vr,vc,"U")
#define	GPFLV(fn,ln,vn,fr)	GPFILE(fn,fr,3);GPLIB(ln,fr,24);GPVOL(vn,fr,45)
#define	GPSYSNAME(sn,sr)		GPKW("SYSNAME ",sn,60,sr,2,"C")
#define	GPPFS(x)			GP "P";GP x
#define	GPENTER()			GP "E"
#define	GPNOENTER()			GP "N"


/*----
The functions
------*/

#endif	/* _GP_H */

/*
**	History:
**	$Log: vsegp.h,v $
**	Revision 1.11  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.10  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.9  1996/09/03 22:24:06  gsl
**	drcs update
**	
**
**
*/
