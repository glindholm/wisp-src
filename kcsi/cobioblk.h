/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */

/*----
cobioblk.h
The io block passed by COBOL is a character string organized with the
following structure. The to avoid alignment problems, the passed area
is treated as a string, and pointers are set up into the various
elements.  The FILE-SYS-IO-BLOCK is filled with a copy of the
structure that is being by this program so that on each entry we
have the IO block.

This approximates the Wang UFB which is passed for each IO.
The UFB is not used in this version because we have no data on what
an LPI ufb looks like. Instead we build our own.

The COBOL version of this structure is:
000100*--------------------------------------------------------
000200* DIO BLOCK ALLOWS A FILE TO BE STRUCTURED DYNAMICALLY
000300* BEFORE AN OPEN OR OTHER IO REQUEST.
000400*--------------------------------------------------------
000500     03  COBOL-DATA.
000600     05  FILE-IO.
000700         10  FILE-IO-MAIN            PIC X.
000800         10  FILE-IO-SUB             PIC X.
000900     05  FILE-SPEC.
001000         10  FILE-NAME               PIC X(8).
001100         10  FILE-LIBRARY            PIC X(8).
001200         10  FILE-VOLUME             PIC X(6).
001210     05  FILE-PRNAME                 PIC X(8).
001300     05  FILE-STATUS                 PIC XX.
000000* The extended file status is provided by C-ISAM
000000* and could be useful for additional diagnostics.
000000     05  FILE-STATUS-EXT             PIC XXX.
001400     05  FILE-OPEN-STATUS            PIC 9.
001500     05  FILE-SPACE                  PIC 9(8).
001900     05  FILE-COMPRESSED-FLAG        PIC X.
002000     05  FILE-VARIABLE-FLAG          PIC X.
002100     05  FILE-IO-KEY                 PIC 99.
002200     05  FILE-REL-KEY                PIC 9(8).
002300* THE FILE IO CHANNEL IS ALLOCATED BY C-ISAM AT OPEN
002400* TIME AND SHOULD BE USED FOR ALL IO.
002500     05  FILE-IO-CHANNEL             PIC 9(5).
002505* Everything within the FILE-ID 05 level is used to
002506* to identify a file format key and organization.
000000* If an opened file's data is extracted into this
000000* structure and matched with a similar block
000000* constructed from the CONTROL data. If they match
000000* then the file can be processed by control.
002510     05  FILE-ID-DATA.
000000         10  FILE-ORG                    PIC X.
001600         10  FILE-RECORD-LEN             PIC 9999.
001700         10  FILE-KEY-POS                PIC 9999.
001800         10  FILE-KEY-LEN                PIC 99.
002600         10  FILE-ALTKEY-DATA.
002700             15  FILE-ALTKEY-COUNT       PIC 99.
002800             15  FILE-ALTKEY-BLOCKS.
002900                 20  FILLER   OCCURS 16 TIMES.
003000                     25  FILE-ALTKEY-NUMBER  PIC 99.
003100                     25  FILE-ALTKEY-POS     PIC 9999.
003250                     25  FILE-ALTKEY-LEN     PIC 99.
003300                     25  FILE-ALTKEY-DUP     PIC 9.
003400     03  C-DATA.
000000* The Sys IO block holds the copy of the struct
000000* used for IO by KCSIO.
003500     05  FILE-SYS-IO-BLOCK               PIC X(3000).

------*/
/*----
Definitions for breaking out the passed fields.
------*/

#define   IO_POS               0
#define   IO_LEN               2
#define   NAME_POS             2
#define   NAME_LEN             8
#define   LIBRARY_POS         10
#define   LIBRARY_LEN          8
#define   VOLUME_POS          18
#define   VOLUME_LEN           6
#define   PRNAME_POS          24
#define   PRNAME_LEN           8
#define   STATUS_POS          32
#define   STATUS_LEN           2
#define   STATUS_EXT_POS      34
#define   STATUS_EXT_LEN       3
#define   OPEN_STATUS_POS     37
#define   OPEN_STATUS_LEN      1
#define   SPACE_POS           38
#define   SPACE_LEN            8
#define   COMP_FLAG_POS       46
#define   COMP_FLAG_LEN        1
#define   VAR_FLAG_POS        47
#define   VAR_FLAG_LEN         1
#define   IO_KEY_POS          48
#define   IO_KEY_LEN           2
#define   REL_KEY_POS         50
#define   REL_KEY_LEN          8
#define   IO_CHANNEL_POS      58
#define   IO_CHANNEL_LEN       5
#define   ORG_POS             63
#define   ORG_LEN              1
#define   RECORD_LEN_POS      64
#define   RECORD_LEN_LEN       4
#define   KEY_POS_POS         68
#define   KEY_POS_LEN          4
#define   KEY_LEN_POS         72
#define   KEY_LEN_LEN          2
#define   ALTKEY_COUNT_POS    74
#define   ALTKEY_COUNT_LEN     2
#define   ALTKEY_BLOCKS_POS   76
#define   ALTKEY_BLOCK_LEN     9
#define   ALTKEY_BLOCKS_LEN   (16 * ALTKEY_BLOCK_LEN)
#define   ALTKEY_NUMBER_OFF    0
#define   ALTKEY_NUMBER_LEN    2
#define   ALTKEY_POS_OFF       2
#define   ALTKEY_POS_LEN       4
#define   ALTKEY_LEN_OFF       6
#define   ALTKEY_LEN_LEN       2
#define   ALTKEY_DUP_OFF       8
#define   ALTKEY_DUP_LEN       1
#define   SYS_IO_BLOCK_POS    (ALTKEY_BLOCKS_POS + ALTKEY_BLOCKS_LEN)
#define	  COBOL_BLOCK_LEN	SYS_IO_BLOCK_POS
#define   SYS_IO_BLOCK_LEN  3000
#define	  IO_BLOCK_LEN	(SYS_IO_BLOCK_LEN + COBOL_BLOCK_LEN)

/*
**	History:
**	$Log: cobioblk.h,v $
**	Revision 1.3  1996/09/17 23:34:02  gsl
**	drcs update
**	
**
**
*/
