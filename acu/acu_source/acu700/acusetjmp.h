/* acusetjmp.h - declarations for acusavenv and aculongjmp routines  */
/* $Id: acusetjmp.h,v 1.6.6.1 2005/01/03 17:20:37 mark Exp $ */

/* Copyright (c) 2002-2005 by Acucorp, Inc.  All rights reserved.	*/


#ifndef	ACU_LIB_ACUSETJMP_H
#define	ACU_LIB_ACUSETJMP_H

#include <setjmp.h>

typedef struct tag_acujmpbuf {
	jmp_buf	setjmp_mark;
	void	*acu_state;
} acujmp_buf;

extern	jmp_buf	*acusavenv(acujmp_buf *buffer);
extern	void	aculongjmp(acujmp_buf *buffer);

#endif	/* ACU_LIB_ACUSETJMP_H */
