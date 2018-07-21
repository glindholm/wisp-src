/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/

/*
**	File:		win32msg.c
**
**	Project:	WISPLIB
**
**	Purpose:	unix message queue compatibility
**
**	Routines:	
**	
*/
#ifdef WIN32

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <direct.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "win32msg.h"
#include "win32err.h"
#include "werrlog.h"
#include "wispcfg.h"

/*
**	Structures and Defines
*/
#define WIN32MAGIC 0xd4aadeff
#define WMSGMAX 2014
#define WIN32BADFILEPOS 0xFFFFFFFF

typedef struct win32msg_s {
	long mtype;
	char mtext[WMSGMAX];
} win32msg;

#define WIN32MSGERRCHK(cond, txt, arg) if (cond) { \
  char errtxt1[1024], *errtxt2;\
  sprintf(errtxt1,txt,arg);\
  errtxt2=WL_GetWin32Error(errtxt1);\
  WL_werrlog_error(WERRCODE(104),"WIN32MSG","ERRCHK", "%s", errtxt2);\
  errno=EINVAL;\
  return -1;} 

/*
**	Globals and Externals
*/
const char *wispprbdir(char *dir);
int WL_fexists(const char* name);
static long nReadTimeoutMsecs=0;
#define INC_READ_TIMEOUT_MSEC 100

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
static void makemsgdatafile(key_t keyval, char *path);

/*
**	ROUTINE:	WL_win32msgget()
**
**	FUNCTION:	Get message queue identifier based on provided key 
**
**	DESCRIPTION:	emulates the unix msgget() system call
**
**	ARGUMENTS:	
**	nTheKey		The message queue key
**      nFlag		Only IPC_CREAT is significant
**
**	GLOBALS:	
**
**	RETURN:		a message queue identifier
**
**	WARNINGS:	
**
*/
int WL_win32msgget(key_t nTheKey, int nFlag)
{
	HANDLE hTheFile;
	char szFilePath[256];
	long nMagic, nMsgCount, nByteCount;
	DWORD nBytesWritten;
	BOOL bSuccess;
	
	makemsgdatafile(nTheKey, szFilePath);

	if (nFlag & IPC_CREAT)
	{	
		hTheFile = CreateFile(szFilePath, GENERIC_READ|GENERIC_WRITE,0,
				     NULL,CREATE_NEW,FILE_FLAG_RANDOM_ACCESS,NULL);
		if (INVALID_HANDLE_VALUE == hTheFile)
		{
			WIN32MSGERRCHK(TRUE,"WL_win32msgget: CreateFile failed (%s)",szFilePath);
			errno = ENOENT;
			return -1;
		}

		nMagic = WIN32MAGIC;
		bSuccess=WriteFile(hTheFile, (void*)&nMagic, (DWORD)sizeof(nMagic), &nBytesWritten, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgget: WriteFile failed (magic number %08X)",nMagic);
			
		nMsgCount = 0;
		bSuccess=WriteFile(hTheFile, (void*)&nMsgCount, (DWORD)sizeof(nMsgCount), &nBytesWritten, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgget: WriteFile failed (message count %d)",nMsgCount);

		nByteCount = 0;
		bSuccess=WriteFile(hTheFile, (void*)&nByteCount, (DWORD)sizeof(nByteCount), &nBytesWritten, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgget: WriteFile failed (byte count %d)", nByteCount);

		CloseHandle(hTheFile);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgget: CloseHandle failed (%s)",szFilePath);
	}
	else
	{
		/*
		**	The data file should already exist, if it doesn't then it's an error.
		*/
		if (!WL_fexists(szFilePath))
		{			
			WL_werrlog_error(WERRCODE(104),"win32msgget", "FILEMISSING", 
				"Message data file missing (%s)",szFilePath);
			errno = ENOENT;
			return -1;
		}
	}
	return (int)nTheKey;
}

/*
**	ROUTINE:	OpenMsgFile()
**
**	FUNCTION:	Open an existing message data file for exclusive IO.
**
**	DESCRIPTION:	
**			The message data file should already exist.
**			Attempt to open the file exculsively if it fails assume that 
**			someone else has it open and retry every 25 milliseconds until
**			it openes or 1 second (1000 ms) elapses.
**
**	ARGUMENTS:	
**	szFilePath	The filepath to the message data file.
**
**	GLOBALS:	None
**
**	RETURN:		The open file handle or INVALID_HANDLE_VALUE
**
**	WARNINGS:	None
**
*/
static HANDLE OpenMsgFile(char* szFilePath)
{
	HANDLE hTheFile;
	int	millsecs;

	if (!WL_fexists(szFilePath))
	{
		return INVALID_HANDLE_VALUE;
	}

	for(millsecs=0;;)
	{
		hTheFile = CreateFile(szFilePath, GENERIC_READ|GENERIC_WRITE,0,
				      NULL,OPEN_EXISTING,FILE_FLAG_RANDOM_ACCESS,NULL);
		if (hTheFile != INVALID_HANDLE_VALUE)
		{
			return hTheFile;
		}
		if (millsecs > 1000)
		{
			return INVALID_HANDLE_VALUE;
		}
		Sleep(25);
		millsecs += 25;
	}
}

/*
**	ROUTINE:	WL_win32msgsnd()
**
**	FUNCTION:	send a message on the specified message queue
**
**	DESCRIPTION:	emulates the unix msgsnd() system call
**
**	ARGUMENTS:	int     nTheKey         key or id of queue
**                      char *  msg            message data
**                      size_t  nPassedMsgLen  size of message
**                      int     nFlag           (to specify non-blocked read)
**
**	GLOBALS:	
**
**	RETURN:		not implemented yet
**
**	WARNINGS:	
**
*/

int WL_win32msgsnd(int nTheKey, char *pMsg, size_t nPassedMsgLen, int nFlag)
{
	DWORD nPassedType;
	char *pData;
	
	BOOL bSuccess;
	DWORD nWritten, nRead;
	HANDLE hTheFile;
	DWORD nMagic, nMsgCount, nByteCount, nFilePos;
	char szFilePath[256];

	/*
	** open the message file, read the magic, msgcount, and bytecount
	*/
	makemsgdatafile(nTheKey, szFilePath);
	hTheFile = OpenMsgFile(szFilePath);
	if (INVALID_HANDLE_VALUE == hTheFile)
	{
		WIN32MSGERRCHK(1,"WL_win32msgsnd: Unable to open message data file (%s)",szFilePath);
		errno = EINVAL;
		return -1;
	}

	bSuccess=ReadFile(hTheFile, (void*)&nMagic, (DWORD)sizeof(nMagic), &nRead, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: ReadFile failed (magic number)",0);
	WIN32MSGERRCHK(nMagic != WIN32MAGIC, "WL_win32msgsnd: ReadFile read invalid magic number %08X",nMagic);

	bSuccess=ReadFile(hTheFile, (void*)&nMsgCount, (DWORD)sizeof(nMsgCount), &nRead, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: ReadFile failed (message count)",0);
	
	bSuccess=ReadFile(hTheFile, (void*)&nByteCount, (DWORD)sizeof(nByteCount), &nRead, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: ReadFile failed (byte count)",0);

	/*
	** position the pointer at the end of the file.. 
	*/
	nFilePos = SetFilePointer(hTheFile, (long)(sizeof(nMagic)+sizeof(nMsgCount)+sizeof(nByteCount) + nByteCount),
				       NULL, FILE_BEGIN);
	WIN32MSGERRCHK(nFilePos == WIN32BADFILEPOS,"WL_win32msgsnd: SetFilePointer failed (%d)",
		       (long)(sizeof(nMagic)+sizeof(nMsgCount)+sizeof(nByteCount) + nByteCount));

	/*
	** copy the passed message type (in the msg struct) to a local variable
	*/
	memcpy(&nPassedType,pMsg,sizeof(nPassedType));

	/*
	** point past the type to the data
	*/
	pData = pMsg + sizeof(nPassedType);

	/*
	** now write the type, len, and data
	*/
	bSuccess = WriteFile(hTheFile, (void*)&nPassedType, sizeof(nPassedType), &nWritten, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: Writefile failed (message type %d)",nPassedType);
	
	bSuccess = WriteFile(hTheFile, (void*)&nPassedMsgLen, sizeof(nPassedMsgLen), &nWritten, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: Writefile failed (message length %d)",nPassedMsgLen);

	bSuccess = WriteFile(hTheFile, (void*)pData, nPassedMsgLen, &nWritten, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: Writefile failed (message data)",0);

	/*
	** reposition the pointer to right after the magic to update the msgcount and bytecount
	*/
	nFilePos = SetFilePointer(hTheFile, (long)sizeof(nMagic),NULL, FILE_BEGIN);
	WIN32MSGERRCHK(nFilePos == WIN32BADFILEPOS,"WL_win32msgsnd: SetFilePointer failed (offset %d)",(long)sizeof(nMagic));
	
	++nMsgCount;
	nByteCount += sizeof(nPassedType) + sizeof(nPassedMsgLen) + nPassedMsgLen;
	bSuccess = WriteFile(hTheFile,(void*)&nMsgCount, (DWORD)sizeof(nMsgCount), &nWritten, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: Writefile failed (rewrite message count %d)",nMsgCount);

	bSuccess = WriteFile(hTheFile,(void*)&nByteCount, (DWORD)sizeof(nByteCount), &nWritten, NULL);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: Writefile failed (rewrite byte count %d)",nByteCount);

	bSuccess = CloseHandle(hTheFile);
	WIN32MSGERRCHK(!bSuccess,"WL_win32msgsnd: CloseHandle failed (%s)",szFilePath);
	return 0;
}
/*
**	ROUTINE:	WL_win32msgrcv()
**
**	FUNCTION:	receive a message on the specified message queue
**
**	DESCRIPTION:	emulates the unix msgrcv() system call
**
**	ARGUMENTS:	int     nTheKey         key or id of queue
**                      char *  msg            message data receiver area
**                      size_t  msglen         size of message receiver area
**                      int     msgtype        type of message to receive
**                      int     nFlag           (to specify non-blocked read)
**
**	GLOBALS:	
**
**	RETURN:		number of bytes read from queue
**
**	WARNINGS:	
**
*/
int WL_win32msgrcv(int nTheKey, char *pMsg, size_t nPassedMsgLen, int nPassedMsgType, int nFlag)
{
	char *pData, *pDatap, *pSave;

	size_t nFileMsgLen, nActMsgLen;
	int nFileMsgType;
	int nBytes, nRetval=0;
	
	BOOL bSuccess, bGotMsg;
	DWORD nWritten, nRead, nFileSize, nAdjFileSize;
	HANDLE hTheFile;
	DWORD nMagic, nMsgCount, nByteCount, nFilePos;
	char szFilePath[256];
	BOOL bForever;
	
	makemsgdatafile(nTheKey, szFilePath);
	bGotMsg = FALSE;

	/*
	** decide if it's a blocking or nonblocking call
	*/
	if (nReadTimeoutMsecs == 0 && !(nFlag & IPC_NOWAIT))
	{
		bForever = TRUE;
	}
	else
	{
		bForever = FALSE;
	}
	/*
	** this is the main read loop.. basically we open the file, read the magic, msgcount
	** and bytecount. if there are messages we enter a second loop below.  if an appropriate
	** message is found, it is copied out and then deleted.. next the updated msgcount, bytecount
	** and message data is rewritten.. the bGotMsg flag gets us out of the loop..  we can also get
	** out if the nReadTimeoutMsecs goes below zero (bForever must be false for this)
	*/
	while ((bForever || nReadTimeoutMsecs >= 0) && bGotMsg==FALSE)
	{
		hTheFile = OpenMsgFile(szFilePath);
		if (INVALID_HANDLE_VALUE == hTheFile)
		{
			WIN32MSGERRCHK(1,"WL_win32msgrcv: Unable to open message data file (%s)",szFilePath);
			errno = EINVAL;
			return -1;
		}
		
		bSuccess=ReadFile(hTheFile, (void*)&nMagic, (DWORD)sizeof(nMagic), &nRead, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: ReadFile failed (magic number)",0);
		WIN32MSGERRCHK(nMagic != WIN32MAGIC, "WL_win32msgrcv: ReadFile read invalid magic number %08X",nMagic);

		nFileSize=GetFileSize(hTheFile,NULL);
		WIN32MSGERRCHK(nFileSize == WIN32BADFILEPOS, "WL_win32msgrcv: GetFileSize failed",0);

		/*
		** compute the size of the data area we need; it is the actual filesize reported
		** by windows minus the size of the magic, msgcount and bytecount
		*/
		nAdjFileSize =  nFileSize - (sizeof(nMagic)+sizeof(nMsgCount)+sizeof(nByteCount));
		pData = malloc(nAdjFileSize);
		memset(pData,0,nAdjFileSize);
		pDatap = pData;

		/*
		** now read the msgcount, bytecount, and message data
		*/
		bSuccess=ReadFile(hTheFile, (void*)&nMsgCount, (DWORD)sizeof(nMsgCount), &nRead, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: ReadFile failed (message count)",0);

		bSuccess=ReadFile(hTheFile, (void*)&nByteCount, (DWORD)sizeof(nByteCount), &nRead, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: ReadFile failed (byte count)",0);

		bSuccess=ReadFile(hTheFile, (void*)pData, nAdjFileSize, &nRead, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: ReadFile failed (message data %d bytes)",nAdjFileSize);
		bGotMsg = 0;

		if (nMsgCount>0)
		do
		{
			/*
			** save a pointer to the start of current message
			*/
			pSave = pDatap;

			/*
			** copy out the msgtype and msglen for the current message
			*/
			memcpy((void*)&nFileMsgType, pDatap, (DWORD)sizeof(nFileMsgType));
			pDatap += sizeof(nFileMsgType);
			memcpy((void*)&nFileMsgLen, pDatap, (DWORD)sizeof(nFileMsgLen));
			pDatap += sizeof(nFileMsgLen);

			/*
			** check to see if this message type matches the type which was requested
			*/
			if (nFileMsgType == nPassedMsgType)
			{
				/*
				** compute the size of the message we will use.. it is either the
				** actual size, or the passed size, whichever is smaller.. this value
				** is also passed back as return code
				*/
				nRetval = nActMsgLen = __min(nFileMsgLen,nPassedMsgLen);

				/*
				** copy the msg type into the caller's msg struct
				*/
				memcpy(pMsg, &nFileMsgType, sizeof(nFileMsgType));

				/*
				** now copy the data to the area immediately following the type
				** in the caller's area
				*/
				memcpy(pMsg + sizeof(nPassedMsgType), pDatap, nActMsgLen);
				bGotMsg=TRUE;
			}
			/*
			** now point to the next message
			*/
			pDatap += nFileMsgLen;

			/*
			** repeat this loop until we get a message or move past the end of the file data
			*/
		} while (!bGotMsg && ( (unsigned)(pDatap - pData) < (unsigned)nAdjFileSize));

		if (bGotMsg)
		{
			/*
			** if we got a message, reset the file pointer to right after the magic number
			** so we can rewrite everything
			*/
			nFilePos = SetFilePointer(hTheFile, (long)sizeof(nMagic) ,NULL, FILE_BEGIN);
			WIN32MSGERRCHK(nFilePos == WIN32BADFILEPOS,"WL_win32msgrcv: SetFilePointer failed (offset %d)",(long)sizeof(nMagic));

			/*
			** nBytes will be the number of bytes of the message we got
			*/
			nBytes = pDatap - pSave;

			/*
			** now we can copy over the message using our saved pointer to the message we
			** took a copy of (from above), our current pointer (to the message right after),
			** and the size we compute based on the known file size and some pointer arithmetic.
			** this deletes the message from our data area
			**
			** also decrement the msgcount and bytecount values .. then rewrite it all
			*/
			memcpy(pSave, pDatap, (pData + nAdjFileSize) - pDatap);

			--nMsgCount;
			nByteCount -= nBytes;
			
			bSuccess = WriteFile(hTheFile,(void*)&nMsgCount, (DWORD)sizeof(nMsgCount), &nWritten, NULL);
			WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: Writefile failed (rewrite message count %d)",nMsgCount);

			bSuccess = WriteFile(hTheFile,(void*)&nByteCount, (DWORD)sizeof(nByteCount), &nWritten, NULL);
			WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: Writefile failed (rewrite byte count %d)",nByteCount);

			nReadTimeoutMsecs=0;
			bSuccess = WriteFile(hTheFile,(void*)pData, (DWORD)nByteCount, &nWritten, NULL);
			WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: Writefile failed (rewrite data %d bytes)",nByteCount);
		}
		else
		{
			/*
			** -1 return from msgrcv means either system call error or (in this case)
			** that there were no messages found and the read wasn't blocking
			*/
			nRetval = -1;
		}

		/*
		** free the mem and close the file
		*/
		free(pData);
		bSuccess = CloseHandle(hTheFile);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgrcv: CloseHandle failed (%s)",szFilePath);

		/*
		** if nReadTimeoutMsecs is greater than zero, we are in a timed read;  so if we didn't
		** get a message, sleep a fraction of a second before the next time thru the loop
		*/
		if (nReadTimeoutMsecs > 0 && !bGotMsg)
		{
			Sleep(INC_READ_TIMEOUT_MSEC);
		}
		/*
		** decrement the timeout value.. this is done even if the read isn't timed.. its negative
		** value is used to exit the loop 
		*/
		nReadTimeoutMsecs -= INC_READ_TIMEOUT_MSEC;
	}
	/*
	** reset the readtimeout value for the next call
	*/
	nReadTimeoutMsecs=0;
	return nRetval;
}
/*
**	ROUTINE:	WL_win32msgtimeout()
**
**	FUNCTION:	setup timeout value for a subsequent call to WL_win32msgrcv
**
**	ARGUMENTS:	int secs_to_wait   - obvious
**
**	GLOBALS:	nReadTimeoutMsecs
**
**	RETURN:		void
**
**	WARNINGS:	
**
*/
void WL_win32msgtimeout(int secs_to_wait)
{
	nReadTimeoutMsecs = secs_to_wait * 1000;
}
/*
**	ROUTINE:	WL_win32msgctl()
**
**	FUNCTION:	some control routines for the message queue
**
**	DESCRIPTION:	emulates unix msgctl() system call
**
**	ARGUMENTS:	int    nTheKey    key/id of the message queue
**                      int    nFunction  desired function
**                      struct msqid_ds *mctlbuf   receiver area
**
**	GLOBALS:	
**
**	RETURN:		0 - success   or  -1 failure
**
**	WARNINGS:	
**
*/
int WL_win32msgctl(int nTheKey, int nFunction, struct msqid_ds *mctlbuf)
{
	BOOL bSuccess;
	DWORD nMsgCount, nMagic, nRead;
	HANDLE hTheFile;
	
	char szFilePath[256];

	makemsgdatafile(nTheKey, szFilePath);
	
	switch (nFunction)
	{
	case IPC_RMID:
		/*
		** rmid just deletes the message file.. 
		*/ 
		_unlink(szFilePath);
		break;
		
	case IPC_STAT:
		/*
		** STAT currently only retrieves the msgcount.. this can be expanded if other values
		** are needed;  however currently the MESSAGE subroutine only uses the msgcount, and this
		** code is only needed by MESSAGE
		*/
		hTheFile = OpenMsgFile(szFilePath);
		if (INVALID_HANDLE_VALUE == hTheFile)
		{
			WIN32MSGERRCHK(1,"WL_win32msgctl: Unable to open message data file (%s)",szFilePath);
			errno = EINVAL;
			return -1;
		}

		bSuccess=ReadFile(hTheFile, (void*)&nMagic, (DWORD)sizeof(nMagic), &nRead, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgctl: ReadFile failed (magic number)",0);
		WIN32MSGERRCHK(nMagic != WIN32MAGIC, "WL_win32msgctl: ReadFile read invalid magic number %08X",nMagic);

		bSuccess=ReadFile(hTheFile, (void*)&nMsgCount, (DWORD)sizeof(nMsgCount), &nRead, NULL);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgctl: ReadFile failed (message count)",0);

		mctlbuf->msg_qnum = nMsgCount;
		bSuccess=CloseHandle(hTheFile);
		WIN32MSGERRCHK(!bSuccess,"WL_win32msgctl: CloseHandle failed (%s)",szFilePath);
		break;
	}
	return 0;
	
}
/*
**	ROUTINE:	makemsgdatafile()
**
**	FUNCTION:	build path of message file for specified key
**
**	ARGUMENTS:	key_t keyval
**                      char * szPath    receiver for path
**
**	GLOBALS:	
**
**	RETURN:		
**
**	WARNINGS:	
**
*/
static void makemsgdatafile(key_t keyval, char *szPath)
{
	const char *msgdir = wispmsgsharedir(NULL);

	sprintf(szPath,"%s\\M_%08X.mdat",msgdir,keyval);

	if (!WL_fexists(msgdir))
	{
		/* 
		**	If the MESSAGE directory doesn't exist then create it here.
		**	(This is kind of sloppy but any error will be picked up later when we go to use it.)
		*/
		_mkdir(msgdir);
		_chmod(msgdir, _S_IREAD | _S_IWRITE );
	}
}

/*
**	ROUTINE:	WL_win32move()
**
**	FUNCTION:	move a file
**
**	DESCRIPTION:	link not supported, so we use MoveFile.  MoveFile requires windows.h, so
**                      WL_win32move was included in this file instead of message.c
**
**	ARGUMENTS:	char *src, char *dest
**
**	GLOBALS:	?
**
**	RETURN:		0 = success, -1 = failure
**
**	WARNINGS:	?
**
*/
int WL_win32move(char *src, char *dest)
{
	return MoveFile(src,dest) == TRUE ? 0 : -1;
}

#endif /* WIN32 */

/*
**	History:
**	$Log: win32msg.c,v $
**	Revision 1.10  2009/10/18 20:37:48  gsl
**	fix windows warnings
**	
**	Revision 1.9  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.8  2002/12/09 19:15:36  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.7  2002/09/30 21:02:00  gsl
**	update
**	
**	Revision 1.6  2002/07/10 21:05:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.5  2002/07/10 04:27:35  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.4  1997/05/19 18:00:26  gsl
**	Fix the handling of message data files so MESSAGE works for WIN32.
**	Handle problem of exclusive opening of the data file. If file exists but
**	the open fails them assume it is because someone else has the file open.
**	In this case retry the open for a second until we get it.
**	Fix where the data file gets created, it should be created in the
**	same place as the key file which is in the shared message directory.
**	
**	Revision 1.3  1996-12-06 18:36:25-05  jockc
**	include win32err.h for proto for GetWin32Error
**
**	Revision 1.2  1996-11-05 10:24:16-08  gsl
**	Change mkdir() to _mkdir()
**	Add WIN32
**
**	Revision 1.1  1996-10-15 16:02:11-07  jockc
**	Initial revision
**
**
**
*/
