			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	File:		isexec.c
**
**	Purpose:	To check if a file is executable
**
**	Routines:	isexec() <u3b2>		Test if a file is an executable or Acucobol or MicroFocus file
**			isexec() <hpux>
**			isexec() <sun>
**			isexec() <NCR32>
**			isexec() 
**
**
**	History:
**			mm/dd/yy	OLD
**			06/04/92	Standardized with new defines.		GSL
**
*/


#ifdef unix

#include <stdio.h>
#include <errno.h>

#include "wdefines.h"

#define ACUMAGIC_U 0x1012									/* U = Bytes are Unswapped.	*/
#define ACUMAGIC_S 0x1210									/* S = Bytes are Swapped.	*/

#define MFINT	0x4D69										/* The chars "Mi" (defunct).	*/
#define MFINT_STR	"Micro Focus COBOL"							/* String to verify.		*/
#define MFINT_SIZE	4									/* 4 to 17 byte verify.		*/

#ifdef ULTRIX
#include <filehdr.h>
#define EXECMAGIC MIPSELMAGIC
#endif /* ULTRIX */

#ifdef MIPS
#include <filehdr.h>
#define EXECMAGIC MIPSEBMAGIC
#endif	/* MIPS */

#ifdef AIX
#include <filehdr.h>
#define EXECMAGIC 0x01df
#endif

#ifdef SCO
#include <filehdr.h>
#define EXECMAGIC I386MAGIC
#endif

#ifdef NCR486
#include <filehdr.h>
#define EXECMAGIC 0x7f45
#endif

#ifdef UNISYS
#include <filehdr.h>
#define EXECMAGIC 0x457f
#endif

#ifdef DGUX
#include <filehdr.h>
#define EXECMAGIC MC88DGMAGIC
#endif

#ifdef BULL
#include <filehdr.h>
#define EXECMAGIC MC68KBCSMAGIC
#endif

#ifdef MOTOROLA
#include <filehdr.h>
#define EXECMAGIC MC88MAGIC
#endif

#ifdef u3b2
#define EXECMAGIC 0413
#include <a.out.h>
#define ISEXEC_DEFINED
int isexec(name)
char *name;
{
	FILE *f;
	struct filehdr fhdr;		
	extern int retstat;
	struct aouthdr ahdr;

	f = fopen(name,"rb");
	if (	!f )
	{
		retstat = fixerr(errno);							/* save errno for parent */
		return ACCERR;
	}
	
	fread(&fhdr, sizeof(struct filehdr), 1, f);
	fread(&ahdr, sizeof(struct aouthdr), 1, f);
        fclose(f);
	if (ahdr.magic == EXECMAGIC) return ISEXEC;
	else if (fhdr.f_magic == ACUMAGIC_U) return ISACU;
	else if (fhdr.f_magic == ACUMAGIC_S) return ISACU;
	else if ( 0 == memcmp( &fhdr, MFINT_STR, MFINT_SIZE ) ) return ISMFINT;
	else return NOTEXEC;
}
#endif	/* u3b2 */

#ifdef hpux
#include <magic.h>

#define ISEXEC_DEFINED
int isexec(name)
char *name;
{
	FILE *f;
	MAGIC m;
	extern int retstat;
	
	f=fopen(name,"r");
	if (	!f )
	{
		retstat = fixerr(errno);							/* save errno for parent */
		return ACCERR;
	}
	fread(&m,sizeof(MAGIC),1,f);
	fclose(f);
	if (m.file_type==EXEC_MAGIC ||
	    m.file_type==SHARE_MAGIC)     return ISEXEC;
	else if (m.system_id==ACUMAGIC_U) return ISACU;
	else if (m.system_id==ACUMAGIC_S) return ISACU;
	else if ( 0 == memcmp( &m, MFINT_STR, MFINT_SIZE ) ) return ISMFINT;
	else return NOTEXEC;
}
#endif	/* hpux */

#ifdef sun
#include <a.out.h>

#define ISEXEC_DEFINED
int isexec(name)
char *name;
{
  	FILE *f;
  	struct exec estruct;
  	short acumagic;

  	f=fopen(name,"r");
  	if (!f)
    	{
      		return ACCERR;
    	}
  	fread(&estruct,sizeof(struct exec),1,f);
  	fseek(f,0,0);
  	fread(&acumagic,sizeof(short),1,f);
  	fclose(f);
  	if (estruct.a_magic==NMAGIC ||
            estruct.a_magic==ZMAGIC) return ISEXEC;
  	else if (acumagic==ACUMAGIC_U ||
	         acumagic==ACUMAGIC_S) return ISACU;
	else if ( 0 == memcmp( &estruct, MFINT_STR, MFINT_SIZE ) ) return ISMFINT;
  	return NOTEXEC;
}
#endif	/* sun */

#ifdef NCR32
#include <filehdr.h>
#define ISEXEC_DEFINED
int isexec(name)
char *name;
{
	FILE *f;
	struct filehdr fhdr;
	extern int retstat;

	f = fopen(name,"r");
	if (!f)
	{
		retstat = fixerr(errno);
		return ACCERR;
	}
	fread(&fhdr, sizeof(struct filehdr), 1 f);
	fclose(f);
	if      (fhdr.f_magic == NCR32200MAGIC) return ISEXEC;
	else if (fhdr.f_magic == NCR32600MAGIC) return ISEXEC;
	else if (fhdr.f_magic == NCR32800MAGIC) return ISEXEC;
	else if (fhdr.f_magic == ACUMAGIC_U) return ISACU;
	else if (fhdr.f_magic == ACUMAGIC_S) return ISACU;
	else if (fhdr.f_magic == MFINT) return ISMFINT;
	else return NOTEXEC;
}
#endif

#ifndef ISEXEC_DEFINED

#ifndef EXECMAGIC
EXECMAGIC IS NOT DEFINED
#endif

int isexec(name)
char *name;
{
	FILE *f;
	struct filehdr fhdr;		
	extern int retstat;

	f = fopen(name,"r");
	if (	!f )
	{
		retstat = fixerr(errno);							/* save errno for parent */
		return ACCERR;
	}
	
	fread(&fhdr, sizeof(struct filehdr), 1, f);
        fclose(f);
	if (fhdr.f_magic == EXECMAGIC) return ISEXEC;
	else if (fhdr.f_magic == ACUMAGIC_U) return ISACU;
	else if (fhdr.f_magic == ACUMAGIC_S) return ISACU;
	else if ( 0 == memcmp( &fhdr, MFINT_STR, MFINT_SIZE ) ) return ISMFINT;
	else return NOTEXEC;	
} 
#endif	/* not IS_EXEC_DEFINED (NOT: u3b2, hpux, or sun)	*/

#endif	/* unix */
