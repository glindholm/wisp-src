static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


/*
**	File:		isexec.c
**
**	Purpose:	To check if a file is executable
**
**	Routines:	
**	isexec()		Test if an executable by looking at the magic number.
**	is_acuobj_magic()	Test if an Acucobol object file by magic number
**	is_elf_magic()		Test if an ELF executable by looking at the header.
**
**	NOTE:	*** Check the file "/etc/magic" to find out what is an executable. ***
**
*/


#ifdef unix

#include <stdio.h>
#include <errno.h>
#include <fcntl.h>

#include "idsistd.h"
#include "wdefines.h"
#include "runtype.h"

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif

#define MIN_HEADER_SIZE	64

#define MFINT	0x4D69										/* The chars "Mi" (defunct).	*/
#define MFINT_STR	"Micro Focus COBOL"							/* String to verify.		*/
#define MFINT_SIZE	4									/* 4 to 17 byte verify.		*/


/*
**	Standard platforms
*/

#ifdef AIX
/*
**	AIX uses only it's only executable format
*/
#include <filehdr.h>
#define EXECMAGIC 0x01df
#endif

#ifdef SCO
/*
**	SCO uses both iAPX and ELF executables.
*/
#include <sys/elf.h>
#include <filehdr.h>
#define EXECMAGIC I386MAGIC
#endif

#ifdef OSF1_ALPHA
/*
**	Digital Unix uses only COFF executables
*/
#include <filehdr.h>
#define EXECMAGIC ALPHAMAGIC
#endif

#if defined(SOLARIS)
/*
**	Solaris uses only ELF executables
*/
#include <sys/elf.h>
#endif

#ifdef LINUX
/*
**	Note it's <elf.h> not <sys/elf.h>
*/
#include <elf.h>
#endif

/*
**	Non-Standard Platforms
*/

#if defined(DGUX) || defined(DGUX_INTEL)
/*
**	DG/UX Intel uses only ELF
**	DG/UX Motorola uses both ELF and filehdr
**
**	Note: "DGUX" seems to be defined on DGUX_INTEL machines
*/
#include <sys/elf.h>
#ifndef DGUX_INTEL
#include <filehdr.h>
#define EXECMAGIC MC88DGMAGIC
#endif
#endif

#if defined(PYRAMID)
#include <sys/elf.h>
#endif

#ifdef ULTRIX
#include <filehdr.h>
#define EXECMAGIC MIPSELMAGIC
#endif /* ULTRIX */

#ifdef MIPS
#include <filehdr.h>
#define EXECMAGIC MIPSEBMAGIC
#endif	/* MIPS */

#ifdef UNIXWARE
#include <sys/elf.h>
#endif

#ifdef NCR486
#include <sys/elf.h>
#endif

#ifdef ICL
#include <sys/elf.h>
#endif

#ifdef UNISYS
#include <sys/elf.h>
#endif

#ifdef SEQUENT
#include <filehdr.h>
#define EXECMAGIC 0x0154
#endif

#ifdef BULL
#include <filehdr.h>
#define EXECMAGIC MC68KBCSMAGIC
#endif

#ifdef MOTOROLA
#include <filehdr.h>
#define EXECMAGIC MC88MAGIC
#endif

/*
**	ROUTINE:	is_acuobject_magic()
**
**	FUNCTION:	Test if an Acucobol object file by the magic number
**
**	DESCRIPTION:	Compare the 4 byte magic number. If buff less then 4 bytes then check 2 bytes.
**
**	ARGUMENTS:	
**	buff		First bytes of a file that contains the magic number
**	len		The lenght of buff. (Should be at least 4 bytes)
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		Is an acucobol object file
**	0		Not an acucobol object file
**
**	WARNINGS:	none
**
*/
int is_acuobject_magic(const char* buff, int len)
{
#define ACUMAGIC "\x10\x12\x14\x20"
#define ACUMAGIC_LEN	4
#define ACUMAGIC_U 0x1012									/* U = Bytes are Unswapped.	*/
#define ACUMAGIC_S 0x1210									/* S = Bytes are Swapped.	*/

	if (len >= ACUMAGIC_LEN)
	{
		if (0==memcmp(buff,ACUMAGIC,ACUMAGIC_LEN))
		{
			return 1;
		}
	}
	else if (len >= 2)
	{
		int2 *i2p;
		
		i2p = (int2 *)buff;
	
		if (ACUMAGIC_U == *i2p || ACUMAGIC_S == *i2p)
		{
			return 1;
		}
	}
	
	return 0;
}

#ifdef ELFMAG
static int is_elf_magic(const char* header, int sizeof_header)
{
	const Elf32_Ehdr *ehdr;
	
	if (sizeof_header < sizeof(Elf32_Ehdr))
	{
		return 0;
	}
	
	ehdr = (const Elf32_Ehdr *) header;

	if (0 == memcmp(ehdr->e_ident,ELFMAG,4) && ET_EXEC == ehdr->e_type)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

#endif /* ELFMAG */


#ifdef HPUX
/*
**	HPUX  has a two part magic number, the first part defines 
**	the machine type (we don't check this), the second part
**	defines what type of object it is.  If the type is either
**	and excutable (EXEC_MAGIC) or and shared executable (SHARE_MAGIC)
**	then we return ISEXEC.
**
*/
#include <magic.h>

#define ISEXEC_DEFINED
int isexec(const char* name)
{
	FILE *f;
	MAGIC m;
	
	f=fopen(name,"r");
	if (	!f )
	{
		return ACCERR;
	}
	fread(&m,sizeof(MAGIC),1,f);
	fclose(f);
	if (m.file_type==EXEC_MAGIC ||
	    m.file_type==SHARE_MAGIC)     return ISEXEC;
	if (is_acuobject_magic((char*)&m, sizeof(m))) return ISACU;
	return NOTEXEC;
}
#endif	/* hpux */


#ifdef ATT3B2
#define EXECMAGIC 0413
#include <a.out.h>
#define ISEXEC_DEFINED
int isexec(const char* name)
{
	FILE *f;
	struct filehdr fhdr;		
	struct aouthdr ahdr;

	f = fopen(name,"rb");
	if (	!f )
	{
		return ACCERR;
	}
	
	fread(&fhdr, sizeof(struct filehdr), 1, f);
	fread(&ahdr, sizeof(struct aouthdr), 1, f);
        fclose(f);
	if (ahdr.magic == EXECMAGIC) return ISEXEC;
	if (is_acuobject_magic((char*)&fhdr, sizeof(fhdr))) return ISACU;
	return NOTEXEC;
}
#endif	/* ATT3B2 */

#ifdef SUNOS
#include <a.out.h>

#define ISEXEC_DEFINED
int isexec(const char* name)
{
  	FILE *f;
  	struct exec estruct;

  	f=fopen(name,"r");
  	if (!f)
    	{
      		return ACCERR;
    	}
  	fread(&estruct,sizeof(struct exec),1,f);
  	fclose(f);

  	if (estruct.a_magic==NMAGIC ||
            estruct.a_magic==ZMAGIC) return ISEXEC;
	if (is_acuobject_magic((char*)&estruct, sizeof(estruct))) return ISACU;
  	return NOTEXEC;
}
#endif	/* sun */

#ifdef NCR32
#include <filehdr.h>
#define ISEXEC_DEFINED
int isexec(const char* name)
{
	FILE *f;
	struct filehdr fhdr;

	f = fopen(name,"r");
	if (!f)
	{
		return ACCERR;
	}
	fread(&fhdr, sizeof(struct filehdr), 1, f);
	fclose(f);
	if (fhdr.f_magic == NCR32200MAGIC) return ISEXEC;
	if (fhdr.f_magic == NCR32600MAGIC) return ISEXEC;
	if (fhdr.f_magic == NCR32800MAGIC) return ISEXEC;
	if (is_acuobject_magic((char*)&fhdr, sizeof(fhdr))) return ISACU;
	return NOTEXEC;
}
#endif



#ifndef ISEXEC_DEFINED

/*
**	ROUTINE:	isexec()  (Default)
**
**	FUNCTION:	Test filehdr magic number and/of ELF
**
**	DESCRIPTION:	This is the default isexec() it can test either or both
**			1) EXECMAGIC == filehdr->f_magic
**			2) ELF executable
**
**			To test EXECMAGIC you must define it.
**			To test ELF include <sys/elf.h> which defines ELFMAG
**			You must do one of these.
**
**	ARGUMENTS:	
**	name		The file name path.
**
**	GLOBALS:	none
**
**	RETURN:		
**	ACCERR		Can't read magic number	
**	ISEXEC		It is an executable file
**	ISACU		It is an Acucobol object file
**	NOTEXEC		Not an executable
**
**	WARNINGS:	None
**
*/

#ifndef EXECMAGIC
#ifndef ELFMAG
BOTH EXECMAGIC AND ELFMAG IS NOT DEFINED
#endif
#endif

int isexec(const char* name)
{
	FILE *f;
	struct filehdr *fhdrp;		
	char header[64];
	
	f = fopen(name,"r");
	if ( NULL == f )
	{
		return ACCERR;
	}
	
	fread(header, sizeof(header), 1, f);
        fclose(f);

#ifdef ELFMAG
	if (is_elf_magic(header, sizeof(header)))
	{
		return ISEXEC;
	}
#endif

#ifdef EXECMAGIC
	fhdrp = (struct filehdr *)header;
	if (EXECMAGIC == fhdrp->f_magic)
	{
		return ISEXEC;
	}
#endif

	if (is_acuobject_magic(header, sizeof(header)))
	{
		return ISACU;
	}
	
	return NOTEXEC;	
} 
#endif	/* Default isexec() */

#endif	/* unix */
/*
**	History:
**	$Log: isexec.c,v $
**	Revision 1.15.2.1  2002/09/05 19:22:29  gsl
**	LINUX
**	
**	Revision 1.15  1998/10/15 14:05:43  gsl
**	fix const warning
**	
**	Revision 1.14  1998-10-14 17:23:04-04  gsl
**	Consolidated and documented much of this.
**	Fixed SCO to support both ELF and iAPX files.
**
**	Revision 1.13  1998-05-18 10:07:33-04  gsl
**	Fix how acucobol object is checked.
**	Remove test for micro focus as this test is done from in runtype()
**
**	Revision 1.12  1997-09-30 10:07:48-04  scass
**	Added code specific stuff for DGUX_INTEL port
**
**	Revision 1.11  1996-08-19 18:32:24-04  gsl
**	drcs update
**
**
**
*/
