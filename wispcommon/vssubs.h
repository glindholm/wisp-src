/* 
	Copyright (c) 2003 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

/*
**	File:		vssubs.h
**
**	Project:	WISPLIB
**
**
**
**	Purpose:	Function prototypes for VSSUBS subs
**
*/

#ifndef vssubs_H
#define vssubs_H
/*
**	Includes
*/
#include <sys/types.h>
#include "intdef.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/

void BELL(int4 *bell_count);

void BITPACK(unsigned char *in_ptr,unsigned char *out_ptr, int4 *in_len);

void BITUNPK(unsigned char *in_ptr, unsigned char *out_ptr, int4 *in_len);

void COBLINK(const char *progname);

void WISPDATE(const char* func, ...);	/* DATE -> WISPDATE */
void DATE2(const char* func, ...);
void DATE4(const char* func, ...);
void DATE6(const char* func, ...);

#define DATE_RC_0_SUCCESS			0
#define DATE_RC_4_INVALID_YEAR			4
#define DATE_RC_8_INVALID_INPUT			8
#define DATE_RC_16_INVALID_TYPE			16

void DAY(char* date, int4* dow);

void EXTRACT(const char* keyword, ...);
void EXTRACT2(const char* keyword, void* recvr);

void FIND(char* the_file, char* the_lib, char* the_vol, int4 *starter, int4 *counter, char* receiver, ...);

#define GETPARM_MAX_ARGS	500
void GETPARM(char* type, char* form, ...);
void GETPARM2(char* args[], int args_count);

void HEXUNPK( char* source,  char* target, int4 *tlen );

void HEXPACK( char* source,  char* target, int4* tlen );

void LINK2 (const char *progname, int4 progname_len, ... );
#define LINK_CC_0_SUCCESS			0
#define LINK_CC_8_FAILED			8
#define LINK_CC_16_CANCELLED			16

#define LINK_RC_0_SUCCESS			0
#define LINK_RC_4_VOLUME_NOT_FOUND		4
#define LINK_RC_8_VOLUME_BUSY			8
#define LINK_RC_16_LIBRARY_NOT_FOUND		16
#define LINK_RC_20_FILE_NOT_FOUND		20
#define LINK_RC_24_EXCEEDS_LIMITS		24
#define LINK_RC_28_ACCESS_DENIED		28
#define LINK_RC_40_INVALID_INPUT_PARAM		40
#define LINK_RC_44_IO_ERROR			44
#define LINK_RC_52_NOT_A_PROGRAM		52
#define LINK_RC_60_NO_MEMORY			60
#define LINK_RC_99_FORK_FAILED			99

void LOGOFF(void);

void MESSAGE( char* arg1_func, char* arg2_port, ... );

void PAUSE(int4* hsec);

void PRINT(char *l_file, ...);

#define PRINT_RC_0_SUCCESS			0
#define PRINT_RC_4_VOLUME_NOT_FOUND		4
#define PRINT_RC_8_VOLUME_BUSY			8
#define PRINT_RC_16_LIBRARY_NOT_FOUND		16
#define PRINT_RC_20_FILE_NOT_FOUND		20
#define PRINT_RC_24_INVALID_FILETYPE		24
#define PRINT_RC_28_ACCESS_DENIED		28
#define PRINT_RC_40_INVALID_PARAM		40
#define PRINT_RC_60_INVALID_MODE		60
#define PRINT_RC_61_INVALID_DISP		61
#define PRINT_RC_62_UNIQUE_ERROR		62
#define PRINT_RC_63_LP_ERROR			63
#define PRINT_RC_99_QUEUE_NOT_AVAILABLE		99

void PUTPARM(const char* function, ...);

void READFDR(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, ...);
void READFDR4(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, ...);

#define READFDR_RC_0_SUCCESS			0
#define READFDR_RC_4_VOLUME_NOT_FOUND		4
#define READFDR_RC_16_LIBRARY_NOT_FOUND		16
#define READFDR_RC_20_FILE_NOT_FOUND		20
#define READFDR_RC_24_NO_FILE_HEADER		24
#define READFDR_RC_32_STAT_ERROR		32
#define READFDR_RC_40_INVALID_INPUT_PARAM	40
#define READFDR_RC_44_IO_ERROR			44
#define READFDR_RC_68_UNKNOWN_FILE_FORMAT	68

void READVTOC(char *option, char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *rtrn_code, int4 *total);

void RENAME(const char* type, char* file, char* lib, char* vol, ...);

#define RENAME_RC_0_SUCCESS			0
#define RENAME_RC_4_VOLUME_NOT_FOUND		4
#define RENAME_RC_16_LIBRARY_NOT_FOUND		16
#define RENAME_RC_20_FILE_NOT_FOUND		20
#define RENAME_RC_24_ACCESS_DENIED		24
#define RENAME_RC_32_FILE_IN_USE		32
#define RENAME_RC_44_INVALID_PARAMETER		44
#define RENAME_RC_48_IO_ERROR			48
#define RENAME_RC_52_ALREADY_EXISTS		52
#define RENAME_RC_56_NEW_NAME_INVALID		56

void SCRATCH(	char *tflag,		/* The type of scratch, F=file, L=library		*/
		char *fname,		/* alpha(8) name of the file to scratch			*/
		char *lib,		/* library name, alpha(8).				*/
		char *vol,		/* volume, alpha(6)					*/
		...);

#define SCRATCH_RC_0_SUCCESS			0
#define SCRATCH_RC_4_VOLUME_NOT_FOUND		4
#define SCRATCH_RC_12_SYSTEM_CALL_FAILED	12
#define SCRATCH_RC_16_LIBRARY_NOT_FOUND		16
#define SCRATCH_RC_20_FILE_NOT_FOUND		20
#define SCRATCH_RC_24_ACCESS_DENIED		24
#define SCRATCH_RC_32_FILE_IN_USE		32
#define SCRATCH_RC_48_IO_ERROR			48
#define SCRATCH_RC_52_FILE_BYPASSED		52

void SCREEN(const char *status, const void *ufb_addr, const char *func_type, char *output_rcvr, ...);

void SEARCH(char* table, 
	    int4* table_size_arg, 
	    int4* item_length_arg,
	    char* search_item,
	    ...);

void SET(const char *keyword, const char* value, ...);
void SET2(const char *keyword, const char* value);

void SETFILE(char* vol, char* lib, char* file, char* status, ...);
void SETFILE2(char* vol, char* lib, char* file, ...);

void SORT(char* input_array, int4* element_count, int4* element_length, ...);

void SORTCALL(char* sortdata, int4 *retcode);

void SORTINFO(char* filetype, int4* recsize, int4* sortcode);

void SORTLINK(char* function, char* sort_options, ...);

void STRING(char* func_type, char* in_string, ...);

void SUBMIT(char* l_file, ...);

#define SUBMIT_RC_0_SUCCESS		0
#define SUBMIT_RC_4_VOLUME_NOT_FOUND	4
#define SUBMIT_RC_8_VOLUME_BUSY		8
#define SUBMIT_RC_16_LIBRARY_NOT_FOUND	16
#define SUBMIT_RC_20_FILE_NOT_FOUND	20
#define SUBMIT_RC_24_IMPROPER_FILETYPE	24
#define SUBMIT_RC_28_ACCESS_DENIED	28
#define SUBMIT_RC_40_INVALID_FILE_SPEC	40
#define SUBMIT_RC_52_SYSTEM_ERROR	52
#define SUBMIT_RC_56_INVALID_OPTIONS	56
#define SUBMIT_RC_99_FORK_FAILED	99
#define SUBMIT_RC_900_INVALID_STATUS	900
#define SUBMIT_RC_901_INVALID_DISP	901
#define SUBMIT_RC_902_INVALID_JOBCLASS	902
#define SUBMIT_RC_903_INVALID_ABORTACT	903
#define SUBMIT_RC_904_INVALID_TIME	904
#define SUBMIT_RC_905_INVALID_LIMIT	905
#define SUBMIT_RC_1000_ABORTED		1000

void SETSUBMIT(short *arg_count_ptr, ...);

void UPDATFDR();

void WSXIO(const char* function, ...);


#endif /* vssubs_H */

/*
**	History:
**	$Log: vssubs.h,v $
**	Revision 1.19  2003/03/28 20:15:57  gsl
**	Add EXTRACT2
**	
**	Revision 1.18  2003/03/27 21:23:17  gsl
**	DATE6 changes
**	
**	Revision 1.17  2003/03/20 19:05:14  gsl
**	Change references fo DATE to WISPDATE
**	
**	Revision 1.16  2003/03/19 22:26:19  gsl
**	Standardize PRINT RC defines
**	
**	Revision 1.15  2003/03/19 20:03:17  gsl
**	Standardize SUBMIT return code defines
**	
**	Revision 1.14  2003/03/17 17:21:51  gsl
**	Change to use  SETFILE2
**	
**	Revision 1.13  2003/03/06 21:31:57  gsl
**	Add WISP_SETFILE() for changes from MODE to ATTR
**	
**	Revision 1.12  2003/02/21 19:36:11  gsl
**	Switch GETPARM to stdarg.h
**	
**	Revision 1.11  2003/02/21 19:18:32  gsl
**	Switch SORTLINK to stdarg.h
**	
**	Revision 1.10  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.9  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.8  2003/02/17 20:30:36  gsl
**	Define return codes in vssubs.h
**	
**	Revision 1.7  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.6  2003/01/30 21:11:22  gsl
**	Change RENAME to use stdarg.h
**	
**	Revision 1.5  2003/01/30 19:43:24  gsl
**	Change RENAME to use vssubs.h and define all the return codes
**	WL_file_rename() changed to WL_rename()
**	
**	Revision 1.4  2003/01/29 21:50:08  gsl
**	Switch to use vssubs.h
**	
**	Revision 1.3  2003/01/29 21:08:11  gsl
**	Change PUTPARM to use stdarg.h
**	
**	Revision 1.2  2003/01/29 16:19:52  gsl
**	Add LOGOFF()
**	
**	Revision 1.1  2003/01/28 21:20:31  gsl
**	first pass
**	
**
**
*/
