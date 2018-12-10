/*  rmcbdefs.h: RM/COBOL CodeBridge definitions  */
/*  $Id: rmcbdefs.h 66993 2015-02-03 22:56:44Z mark $  */

/* Copyright (C) 2008 Micro Focus.  All rights reserved. */

#ifndef ACU_LIB_RMCBDEFS_H
#define ACU_LIB_RMCBDEFS_H

/* #define CBDLLAPI	RM_DLLEXPORT RM_CDECL */
/* #define CBDLLEXP	RM_DLLEXPORT */
/* #define CBDLLIMP	RM_DLLIMPORT */
/* #define CBCDECL 	RM_CDECL */
/* #define HANDLEPTR(name) RM_HWND name */

enum ParmFlags
{
    PF_ASSERT_SIGNED            =0x00000008,
    PF_ASSERT_UNSIGNED          =0x00000010,
    PF_IN                       =0x00000020,
    PF_INTEGER_ONLY             =0x00000040,
    PF_LEADING                  =0x00000180, /* Mask for leading VALUE or SPACES */
    PF_LEADING_MINUS            =0x00000001,
    PF_LEADING_SIGN             =0x00000000,
    PF_LEADING_SPACES           =0x00000080,
    PF_LEADING_VALUE            =0x00000100,
    PF_NO_NULL_POINTER          =0x00000200,
    PF_NO_SIZE_ERROR            =0x00000400,
    PF_NUMERIC_STRING_MASK      =0x00000007,
    PF_OCCURS                   =0x00000800,
    PF_OPTIONAL                 =0x00001000,
    PF_OUT                      =0x00002000,
    PF_POINTER_MAX_SIZE         =0x00004000,
    PF_POINTER_RESET_OFFSET     =0x00008000,
    PF_REPEAT                   =0x00010000,
    PF_RETURN_VALUE             =0x00020000,
    PF_ROUNDED                  =0x00040000,
    PF_SCALED                   =0x00080000,
    PF_SILENT                   =0x00100000,
    PF_SIZE                     =0x00200000,
    PF_TRAILING                 =0x00C00000, /* Mask for trailing VALUE or SPACES */
    PF_TRAILING_CREDIT          =0x00000006,
    PF_TRAILING_DEBIT           =0x00000007,
    PF_TRAILING_MINUS           =0x00000005,
    PF_TRAILING_SIGN            =0x00000004,
    PF_TRAILING_SPACES          =0x00400000,
    PF_TRAILING_VALUE           =0x00800000,
    PF_UNSIGNED                 =0x01000000,
    PF_VALUE_IF_OMITTED         =0x02000000,
    PF_C_DATA_IS_ANSI           =0x04000000, /* C data is ANSI (Windows only) */
    PF_C_DATA_IS_OEM            =0x08000000, /* C data is OEM (Windows only) */
    PF_LEAVE_TRAILING_ZEROES    =0x10000000,
    PF_DEBUG                    =0x40000000
};

enum DiagnosticFlags
{
    DF_SILENT  = -1,
    DF_NORMAL  =  0,
    DF_VERBOSE =  1
};

enum LengthEntry
{
    LE_BUFFER_LENGTH    = 0,	/* buffer length requested flag (bit 0) and array index */
    LE_EFFECTIVE_LENGTH = 1,	/* effective length requested flag (bit 1) and array index */
    LE_MAX_VALUE        = 2,	/* max array entries (buffer length & effective length) */
    LE_CONVTABLE_EXT    = 3	/* conversion table extension allocated because of missing optional arguments (bit 3) */
};

typedef struct tag_conv_table
{
    void    *pBlock;
    short    LengthFlags;
    short    EntryCount;
    void    *pLength[LE_MAX_VALUE];
    int      sLength[LE_MAX_VALUE];
    int      Occurs[LE_MAX_VALUE];
    void    *pText[LE_MAX_VALUE];
    int      sText[LE_MAX_VALUE];
} CONV_TABLE;

#endif	/* ACU_LIB_RMCBDEFS_H */
