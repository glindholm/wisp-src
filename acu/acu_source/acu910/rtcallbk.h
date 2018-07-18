/****************************** BEGIN RTCALLBK.H *******************************
 *
 *  Title:  rtcallbk.h
 *	    RM/COBOL Runtime Call Back Table Definition
 *
 *RMFILT%INCLUDE include/copyrit1.h
 *
 *	Version = @(#) $Revision: 59884 $ $Date: 2011-01-11 19:33:21 +0000 (Tue, 11 Jan 2011) $
 *
 *RMFILT%IF ! RELEASE_BUILD
 *
 *  $Id: rtcallbk.h 59884 2011-01-11 19:33:21Z merge $
 *  $URL: svn://sd-dev/acu/tags/v9-1-0-build-0-rc-4/cobolgt/lib/rtcallbk.h $
 *  $Date: 2011-01-11 19:33:21 +0000 (Tue, 11 Jan 2011) $
 *
 *  Implementation File: <../runtime/rmcallbk.c>
 *
 *RMFILT%ENDIF
 ******************************************************************************/

#ifndef ACU_RUNTIME_RTCALLBK_H
#define ACU_RUNTIME_RTCALLBK_H

/*RMFILT%IF ! RELEASE_BUILD #*/

/*----------------------------------------------------------------------------*\
**									      **
**   The RUNTIME_CALLBACKS table contains a list of pointers to functions     **
**   and data that are accessed from DLLs and external access methods.	      **
**									      **
**   TO REMAIN COMPATIBLE WITH CURRENT DLLS, DO NOT CHANGE THE ORDER OF       **
**   THESE VECTORS.  ADD NEW VECTORS TO THE END OF THE TABLE.		      **
**									      **
\*----------------------------------------------------------------------------*/

typedef void (*GUIFE_DLL_CALLBACK)(const char *item, const void *data, int size);

/*RMFILT%ENDIF #*/

#include "sub.h"                    /* prerequisite */

/* Compatibility definitions (subset of <rmstddef.h>) */
#ifndef _RMSTDDEF_H_
typedef unsigned short	BIT16;
typedef unsigned int	BIT32;
typedef unsigned int	UINT;
#if defined(_WINDOWS) || defined(_WIN64)
#define RM_HWND 	HWND
#else
#define RM_HWND 	void *
typedef int BOOLEAN;			/* matches RM <standdef> definition */
#endif
typedef unsigned char	RM_BYTE;
#define RM_DLLEXPORT
#define RM_CDECL
#endif	/* _RMSTDDEF_H_ */

#if !defined(CONV_TABLE) && !defined(ACU_LIB_RMCBDEFS_H)
#define CONV_TABLE void
#endif

typedef struct tagTerminationInfo
{
    /* version 1 and later */
    BIT16	 Version;	    /* structure version; 1 is first version, 2 is second version, ... */
    BIT16	 State; 	    /* runtime state when error occurred; see #define TIS_... below */
    BIT16	 ReturnCode;	    /* runtime return code */
    const char	*ErrorCode;	    /* error code string pointer */
    const char	*TermMsgs;	    /* termination messages string pointer */

}   TERMINATION_INFO;

#define TIS_UNKNOWN		0
#define TIS_INITIALIZING	1
#define TIS_RUNNING		2
#define TIS_TERMINATING 	3

typedef struct tagCallerInfo
{
    /* version 1 and later */
    BIT16	 Version;	    /* structure version; 1 is first version, 2 is second version, ...	*/
    BIT16	 Flags; 	    /* flags; see #define CIF_... below 	*/
    const char	*ProgramLocation;   /* line number of CALL or segment/offset of statement after CALL */
    const char	*ProgramName;	    /* calling program-name (called name, may not match PROGRAM-ID program-name) */
    const char	*ProgramFileName;   /* calling program object file name (including path) */
    const char	*ProgramDateTime;   /* calling program date and time compiled */
    /* version 2 and later */
    const char	*ProgramID;	    /* calling program PROGRAM-ID program-name */

}   CALLER_INFO;

typedef struct tagCallerStackInfo
{
    BIT16    Version;		/* structure version; 1100 is first version  */
    BIT16    Flags;		/* flags; see #define CIF_... below	    */
    /* version 3 and later */
    char    *ChainSeparatelyCompiledName;   /* separately compiled program name in current chain link*/
    char    *ChainCallerProgramName;	    /* caller program name in current chain link*/
    char    *ChainCallerFileName;	    /* caller's object file name in current chain link (to be implemented)*/
    int     ActualLevel;	/* actual calling level (counted from the leaf upward) */

} CALLER_STACK_INFO;

#define CIF_LOCATION_ADDRESS  0x8000	/* indicates ProgramLocation is segment/offset */
#define CIF_NESTED_PROGRAM    0x4000	/* indicates calling program is a nested program */
#define CIF_MAIN_PROGRAM      0x2000	/* indicates calling program is the main  program */

#define RUN_NATIVE_CHARSET_OEM	    2 /* equivalent to current Windows OEM code page */
#define RUN_NATIVE_CHARSET_EBCDIC   4 /* not currently supported; was used on AS/400 */
#define RUN_NATIVE_CHARSET_ANSI     6 /* equivalent to current Windows ANSI code page */
#define RUN_NATIVE_CHARSET_UNKNOWN  8 /* no translations on Windows or UNIX */

typedef struct	tagRuntimeCallsTable
{
    BIT16   table_size; 	    /* sizeof(this) */
    BIT16   table_version;

    void    (*pFmSetTermination)	    ();
    BOOLEAN (*pFmTestTermination)	    ();

/*RMFILT%IF ! RELEASE_BUILD #*/     /* hide in RELEASE_BUILD (alternative names to pad table are below)  */
#if defined(_WINDOWS) || defined(_WIN64)    /* only valid for Windows (alternative names to pad table are below) */
    int     (*pGuiWaitMsgUntil) 	    (HANDLE break_event, int waiting_for);
    int     (*pGuiShowGuife)();

    void    (*pGuiCqShow)		    (int sw_state);
    void    (*pfnReserved06)		    ();
    void    (*pfnReserved07)		    ();
    void    (*pfnReserved08)		    ();
    void    (*pfnReserved09)		    ();

    void    (*pGuifeDllExecAsyncCommand)    (const char *cmd,
					     const char *data,
					     int datasize,
					     GUIFE_DLL_CALLBACK callback);

    LRESULT (*pGuifeDllExecStringCommand)   (const char *cmd, const char *string);
    LRESULT (*pGuifeDllExecDataCommand)     (const char *cmd, const void *data, int extra);
    LRESULT (*pGuifeDllExecCommand)	    (const char *cmd);

    RM_HWND (*pGuifeDllGetWindowHandle)     ();

    LRESULT (*pGuifeDllMessageBox)	    (long style,
					     const char *title,
					     const char *format, ...);

    void    (*pGuiSetRuntimeConfiguration)  (int item, char *lpszType);
    void    (*pGuiSetPrinterDialogAlways)   (int item, char *lpszType);
    void    (*pGuiSetPrinterDialogNever)    (int item, char *lpszType);
#else /* ! defined(O_WINNT) || defined(_WINDOWS) */
/*RMFILT%ENDIF #*/		/* alternative hidden definitions (RELEASE_BUILD or !MS_WINDOWS_WIN32) to pad table */
    void    (*pfnReserved03)();
    void    (*pfnReserved04)();
    void    (*pfnReserved05)();
    void    (*pfnReserved06)();
    void    (*pfnReserved07)();
    void    (*pfnReserved08)();
    void    (*pfnReserved09)();
    void    (*pfnReserved10)();
    void    (*pfnReserved11)();
    void    (*pfnReserved12)();
    void    (*pfnReserved13)();
    void    (*pfnReserved14)();
    void    (*pfnReserved15)();
    void    (*pfnReserved16)();
    void    (*pfnReserved17)();
    void    (*pfnReserved18)();

/*RMFILT%IF ! RELEASE_BUILD #*/
#endif /* ! defined(O_WINNT) || defined(_WINDOWS) */
/*RMFILT%ENDIF #*/

    void    (*pGetDevelopmentMode)	    (int *pDevMode);		  /* available everywhere */

    int     (*pAssertDigits)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     unsigned short MaxValue,
					     unsigned short MinValue);
    int     (*pAssertDigitsLeft)	    (short ArgCount,short ArgNumber, Argument Arguments[], int Flags,
					     unsigned short MaxValue, unsigned short MinValue);
    int     (*pAssertDigitsRight)	    (short ArgCount, short ArgNumber,Argument Arguments[], int Flags,
					     unsigned short MaxValue, unsigned short MinValue);
    int     (*pAssertLength)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     unsigned long MaxValue, unsigned long MinValue);
    int     (*pAssertSigned)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags);
    int     (*pAssertUnsigned)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags);
    int     (*pBufferLength)		    (short ArgCount, short ArgNumber, CONV_TABLE *ConvTable, int Flags,
					     int Occurs, void *Parameter, int Size);
    void    (*pConversionCleanup)	    (short ArgCount, CONV_TABLE *ConvTable);
    int     (*pConversionStartup)	    (short ArgCount, CONV_TABLE **ConvTable,
					     char *Name, short Version);
    int     (*pCobolArgCount)		    (short ArgCount, int Flags, void *Parameter, int Size);
    int     (*pCobolDescriptorAddress)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter);
    int     (*pCobolDescriptorDigits)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void *Parameter, int Size);
    int     (*pCobolDescriptorLength)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void *Parameter, int Size);
    int     (*pCobolDescriptorScale)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void *Parameter, int Size);
    int     (*pCobolDescriptorType)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void *Parameter, int Size);
    int     (*pCobolInitialState)	    (int Flags, void *Parameter, int Size, short State);
    int     (*pCobolToFloat)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags, int Occurs,
					     double Omitted, void **Parameter, int Repeat, int Size);
    int     (*pCobolToGeneralString)	    (short ArgCount, short ArgNumber, Argument Arguments[],
					     CONV_TABLE *ConvTable, int Flags,
					     int Occurs, char *Omitted, void **Parameter, int Repeat, int Size,
					     short Value1, short Value2);
    int     (*pCobolToInteger)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     int Occurs, long Omitted, void **Parameter, int Repeat, short Scale, int Size);
    int     (*pCobolToNumericString)	    (short ArgCount, short ArgNumber, Argument Arguments[],
					     CONV_TABLE *ConvTable, int Flags,
					     int Occurs, char *Omitted, void **Parameter, int Repeat, int Size);
    int     (*pCobolToPointerAddress)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter);
    int     (*pCobolToPointerBase)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter);
    int     (*pCobolToPointerLength)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void *Parameter, int Size);
    int     (*pCobolToPointerOffset)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter, int Size);
    int     (*pCobolToPointerSize)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter, int Size);
    int     (*pCobolToString)		    (short ArgCount, short ArgNumber, Argument Arguments[],
					     CONV_TABLE *ConvTable, int Flags, int Occurs, char *Omitted,
					     void **Parameter, int Repeat, int Size, short Value1, short Value2);
    int     (*pCobolWindowsHandle)	    (int Flags, void *Parameter, int Size, RM_HWND WindowsHandle);
    void    (*pDiagnosticMode)		    (short Flag);
    int     (*pEffectiveLength) 	    (short ArgCount, short ArgNumber, CONV_TABLE *ConvTable,
					     int Flags, int Occurs, void *Parameter, int Size);
    int     (*pFloatToCobol)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     int Occurs, void *Parameter, int Repeat, int Size);
    int     (*pGeneralStringToCobol)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     int Occurs, void *Parameter, int Repeat, int Size, short value1, short Value2);
    int     (*pIntegerToCobol)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     int Occurs, void *Parameter, int Repeat, short Scale, int Size);
    int     (*pNumericStringToCobol)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     int Occurs, void *Parameter, int Repeat, int Size);
    int     (*pPointerBaseToCobol)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter);
    int     (*pPointerOffsetToCobol)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void *Parameter, int Size);
    int     (*pPointerSizeToCobol)	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void *Parameter, int Size);
    int     (*pStringToCobol)		    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     int Occurs, void *Parameter, int Repeat, int Size, short Value1, short Value2);
    CALLER_INFO * (*pGetCallerInfo)	    (void);
    char *  (*pGetEsectBase)		    ();
    char *  (*pGetEsectLimit)		    ();
    int     (*pLoadCobolInt)		    (Argument *pArgEntry);
    void    (*pStoreCobolInt)		    (Argument *pArgEntry, int Value);

    char ** ppVersion;
    char ** ppExeDir;
    char ** ppProductName;
    /*FONT_INFO*/ void *pGuifeDllFontInfo;	/* TODO: FONT_INFO not always available, can't be #defined */

/*RMFILT%IF ! RELEASE_BUILD #*/ 		/* suppress in RELEASE_BUILD (not a customer available call-back */
#if 1
    char ** ppSupportModulesLicenseString;	/* hidden after release filtering */
#else
/*RMFILT%ENDIF #*/
    void *  pReserved;				/* RM/COBOL reserved call-back */
/*RMFILT%IF ! RELEASE_BUILD #*/ 		/* guard end-if for non-customer call-back alternative name */
#endif
/*RMFILT%ENDIF #*/

/* table_version >= 0x0801 */
    int     (*pBISExchange)		    (int Timeout);
    int     (*pBISReadRequest)		    (int Timeout);
    int     (*pBISWriteResponse)	    (int ResponseCode);
    int     (*pBISSetInactivityTimeout)     (int Timeout);
    int     (*pBISSetServiceTimeout)	    (int Timeout);
/* table_version >= 0x0900 */
    BIT16    *pRunNativeCharset;	  /* see RUN_NATIVE_CHARSET_xxx macros for values */
    int     (*pBISExchangeString)	    (const RM_BYTE *pszResponseString, int nResponseLen, const RM_BYTE **ppszRequestString, int *pnRequestLen, int Timeout, int nReserved);
    int     (*pBISReadRequestString)	    (const RM_BYTE **ppszRequestString, int *pnRequestLen, int Timeout, int nReserved);
    int     (*pBISWriteResponseString)	    (const RM_BYTE *pszResponseString, int nResponseLen, int nResponseType, int nReserved);
    int     (*pBISWriteLineToLog)	    (int nReserved, const char *pszFormat, ...);
/* table_version >= 0x1000 */
    TERMINATION_INFO *(*pGetTerminationInfo)(void);
/* table_version >= 0x1100 */
    void    (*pConversionResetLocale)	    (int index, const char* country);
/* table_version >= 0x1200 */
    CALLER_STACK_INFO *(*pGetCallerStackInfo)(int Level);
    BOOLEAN (*pGetBISPresent)		    (void);

    unsigned int(*pLoadCobolUInt)           (Argument *pArgEntry);
    void    (*pStoreCobolUInt)		    (Argument *pArgEntry, unsigned int Value);

/* table version >= 0x1201 */           /* Extend 8.2 port */
    void *  (*pAmalloc)                     (size_t, int, const char *, long, const char *, const char *desc, ...);
    void *  (*pArealloc)                    (void *, size_t, const char *, long, const char *desc, ...);
    void    (*pAfree)                       (void *, const char *, long, const char *desc, ...);
    void    (*pRtCqWsToAsc)                 (Argument *Arg, char *Name, int What);
    int     (*pOsLocateEnvironmentEntry)    (const char *Name, char **Val);
    void    (*pw_set_fgbg)                  (int, int);
    void    (*pw_set_term)                  (void);
    void    (*pw_reset_term)                (void);
    void *  (*pGetTInfo)                    (void);
    void *  (*pGetPTInfo)                   (void);
    void *  (*pGetColorMap)                 (void);
    void    (*pStopRuntime)                 (int exit_code, int mode, char *msg, ... );
    int     (*pAtranslate_address)          (unsigned int pd_addr, char **pFilename, long *pLine, unsigned int forceLoad);
    long    (*pGetAstdlib_f_int_errno)      ();
    long    (*pGetA_cerrno)                 ();
    char *  (*pGetLastErr)                  ();
    short   (*pMapToRMType)		    ();

/* table version >= 0x1300 */		/* extend 9.0 */

    BOOLEAN (*pGetSymtab)		    (char **ppXMLsymtab, int *pXMLsymtabsize, void **phXMLsymtab);
    int     (*pACobolToPointer) 	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter);
    int     (*pPointerToACobol) 	    (short ArgCount, short ArgNumber, Argument Arguments[], int Flags,
					     void **Parameter);
    void    (*pAResolveFileLocation)	    (char *pResultName, char *pFileName, int ResultLength);

}   RUNTIME_CALLS_TABLE;


/*
 * This structure contains all of the call back functions for BIS.
 * The callbacks are individual sets of structures so that passing
 * a pointer to a set of callbacks will be easy.
 */
typedef struct tagBISCallsTable1
{
    int (*biste_read_request)(int nSecs);	       /* BIS_ReadRequest_C_Callback */
    int (*biste_write_response)(int nResponseType);    /* BIS_WriteResponse_C_Callback */
    int (*biste_set_inactivity_timeout)(int nSecs);    /* BIS_SetInactivityTimeout_C_Callback */
    int (*biste_set_service_timeout)(int nSecs);       /* BIS_SetServiceTimeout_C_Callback */
    int (*biste_set_response_status)(int nStatus);     /* BIS_SetResponseStatus_C_Callback */
    int (*biste_trace)(const char *pszFormat, ...);    /* BIS_Trace_C_Callback */
    int (*biste_trace_v)(const char *pszFormat, va_list pArg);   /* BIS_Trace_C_V_Callback */
#if 0
    int (*biste_tracew)(LPCWSTR pszFormat, ...);       /* BIS_Trace_C_CallbackW */
    int (*biste_trace_vw)(LPCWSTR pszFormat, va_list pArg); /* BIS_Trace_C_Callback_VW */
#endif
} BIS_CALLS_TABLE_1;

typedef struct	tagExternalCallsTable
{
    BIT16		table_size; /* sizeof(this) */
    BIS_CALLS_TABLE_1	BIS_1;
} EXTERNAL_CALLS_TABLE;

#ifdef __cplusplus
    #define EXTERN	    extern "C"
#else
    #define EXTERN	    extern
#endif

/*RMFILT%IF ! RELEASE_BUILD #*/
EXTERN RUNTIME_CALLS_TABLE  *pRtCallBackTable;
/*RMFILT%ENDIF #*/

EXTERN RM_DLLEXPORT char *  RM_CDECL RM_AddOnBanner();
EXTERN RM_DLLEXPORT void    RM_CDECL RM_AddOnCancelNonCOBOLProgram(char* ProgramName);
EXTERN RM_DLLEXPORT int     RM_CDECL RM_AddOnInit(int Argc, char** Argv, RUNTIME_CALLS_TABLE *pRtCall);
EXTERN RM_DLLEXPORT char *  RM_CDECL RM_AddOnLoadMessage();
EXTERN RM_DLLEXPORT void    RM_CDECL RM_AddOnTerminate();
EXTERN RM_DLLEXPORT BOOLEAN RM_CDECL RM_AddOnUnload();
EXTERN RM_DLLEXPORT BOOLEAN RM_CDECL RM_AddOnVersionCheck(char* Version,
							  int	MinRuntimeInterfaceVersion,
							  int	MaxRuntimeInterfaceVersion,
							  int*	DesiredInterfaceVersion);

EXTERN RM_DLLEXPORT RMENTRYTABLE* RM_CDECL RM_EnumEntryPoints(int index);

#if defined(_WINDOWS) || defined(_WIN64)
EXTERN RM_DLLEXPORT RMENTRYTABLE RM_EntryPoints[];
#endif	/* defined(O_WINNT) || defined(_WINDOWS) */

/*RMFILT%IF ! RELEASE_BUILD #*/
EXTERN RM_DLLEXPORT int RM_CDECL RM_AddOnFlags();
EXTERN RM_DLLEXPORT char * RM_CDECL RM_AddOnRuntimeVersionCheck();
/*RMFILT%ENDIF #*/

EXTERN void SetBISCallbacks(BIS_CALLS_TABLE_1 *pBISCallbacks);
EXTERN void RemoveBISCallbacks(BIS_CALLS_TABLE_1 *pBISCallbacks);
EXTERN BOOLEAN GetBISAvailability(void);
EXTERN int bis_trace(const char *format, ...);
EXTERN int bis_trace_v(const char *format, va_list varArgs);

#undef EXTERN

/*RMFILT%ENDIF #*/

#endif	/* ACU_RUNTIME_RTCALLBK_H */
