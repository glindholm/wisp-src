/*
 * Copyright (c) 1994-1996 Ataman Software, Inc.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	 This product includes software developed by Ataman Software, Inc.
 * 4. The name of Ataman Software, Inc. may not may be used to endorse or
 *    promote products derived from this software without specific prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY ATAMAN SOFTWARE, INC. ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL ATAMAN SOFTWARE, INC. BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */


static char copyright[] =
	"Copyright (c) 1994-1996 Ataman Software, Inc.  All rights reserved.";

/*
 *  To compile this program for use under Windows NT or Windows 95 using
 *  Microsoft Visual C++, rename the file rexec.c to rexec.cpp and then
 *  use the following command:
 *  cl -DWIN32 -O2 -D_MT /MT rexec.cpp libcmt.lib kernel32.lib advapi32.lib wsock32.lib
 */

#ifdef WIN32
#pragma warning(disable: 4699)
/* Includes for Win32 systems go here. */
#define STRICT
#pragma warning(disable: 4201)
#include <windows.h>
#pragma warning(default: 4201)
#include <winsock.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>
#include <signal.h>
#define RETVAL DWORD
#define IDENT HANDLE
#define STDINPUT hStdIn
#define STDOUTPUT hStdOut
#define STDERROR hStdErr
#define FILECOOKIE HANDLE

static void PassInputThread(void *);
static void PassOutputThread(void *);
static void PassErrorThread(void *);

HANDLE hStdIn, hStdOut, hStdErr;

#else
/* Includes for Unix systems go here. */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sgtty.h>
#define SOCKET int
#define RETVAL int
#define IDENT int
#define INVALID_SOCKET (-1)
#define SOCKET_ERROR (-1)
#define WSAGetLastError() errno
#define GetLastError() ((unsigned long)errno)
#define closesocket(s) close(s)
#define STDINPUT 0
#define STDOUTPUT 1
#define STDERROR 2
#define FILECOOKIE int
#define BOOL int
#define TRUE 1
#define FALSE 0
#endif



/*
Think carefully before enabling the -p option.  While it may be
convenient to have this option, it is for many (if not most) sites a
security hole.  Remember that the '-p password' used on the command
line is visible on most Unix systems to any user that is allow to run
the 'ps' command (normally ALL users can run this command).  While no
utility that comes by default with Windows NT at this time, shows the
same information, it is unclear whether or not the information is
avaiable to all users.  Certainly privileged users would be be able to
see this information on any system.

If the security risk is acceptable at your site, you can enable the -p
option by uncommenting the #define below.
*/

#define ALLOWDASH_P

static void Usage(void);

static int NullStdIn;
static char *GetUsername(void);
static char *GetPassword(void);
static void OpenService(const char *remote_host);
static void Cleanup(void);
static IDENT PassInput(void);
static IDENT PassOutput(void);
static IDENT PassError(void);
static BOOL Close(FILECOOKIE);
static int Read(FILECOOKIE, char *, size_t);
static BOOL Write(FILECOOKIE, const char *, size_t);
static BOOL Send(SOCKET, const char *, size_t);
static BOOL SendZString(const char *);
static BOOL GetErrString(char *, size_t);
static void Wait(IDENT, RETVAL *);

static SOCKET sIO = INVALID_SOCKET;
static SOCKET sErr = INVALID_SOCKET;

IDENT idIn = 0;
IDENT idOut, idErr;

void main(int argc, char *argv[])
{
	char *hostname;
	char *username;
	char *password;
	char command[4096];
	char *pCmd;
	size_t cmdlen;
	int i,pos;
	RETVAL rvIn, rvOut, rvErr;

	if (argc < 3) {
		Usage();
	}

#ifdef WIN32
	hStdIn = GetStdHandle(STD_INPUT_HANDLE);
	hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
	hStdErr = GetStdHandle(STD_ERROR_HANDLE);
#endif

	hostname = argv[1];
	password = NULL;
	username = NULL;

	for (i=2; i<argc; i++) {
		if (*argv[i] != '-') {
			break;
		} else if (strcmp(argv[i], "--") == 0) {
			i++;
			break;
		} else if (strcmp(argv[i], "-d") == 0) {
#if defined(WIN32) && defined(_DEBUG)
			DebugBreak();
#endif
		} else if (strcmp(argv[i], "-n") == 0) {
			NullStdIn = 1;
		} else if (strcmp(argv[i], "-l") == 0) {
			username = argv[++i];
		} else if (strcmp(argv[i], "-p") == 0) {
#ifdef ALLOWDASH_P
			password = argv[++i];
#else
			fprintf(stderr,
				"The -p option is a potential security hole, "
				"please see the comments in the source code\n"
				"before enabling this feature.\n");
			exit(1);
#endif
		} else {
			Usage();
		}
	}

	if (i >= argc) {
		Usage();
	}

	*command = '\0';

	cmdlen = 0;
#ifdef WIN32
	/*
	**	WIN32: Handle the command line differently to preserve
	**	the quotes that argv and argc mung with.
	**	Step thru the args until up to the beginning of the command
	**	then copy to "command".
	*/
	pCmd = GetCommandLine();
	for (pos=0; pos < i; pos++)
	{
		char* ptr;

		/*
		**	Find the next arg in the command line.
		*/
		if(ptr = strstr(pCmd, argv[pos]))
		{
			pCmd = ptr + strlen(argv[pos]);
		}
		else
		{
			/*
			**	The strstr() failed, probably because of
			**	quotes.
			*/
			fprintf(stderr, "Unable to parse command line.\n");
			exit(1);
		}
	}

	/* Skip any white space before the command*/
	while(' '== *pCmd) {pCmd++;}

	if (strlen(pCmd)+1 > sizeof(command))
	{
		fprintf(stderr, "Command too long.\n");
		exit(1);
	}
	strcpy(command, pCmd);
#else
	for (; i<argc; i++) {
		size_t arglen;

		arglen = strlen(argv[i]);
		if (cmdlen+arglen+2 > sizeof command) {
			fprintf(stderr, "Command too long.\n");
			exit(1);
		}
		strcpy(&command[cmdlen], argv[i]);
		strcpy(&command[cmdlen+arglen], " ");
		cmdlen += arglen+1;
	}
#endif

	if (!username) {
		username = GetUsername();
	}

	if (!password) {
		password = GetPassword();
	}

	OpenService(hostname);

	SendZString(username);
	SendZString(password);
	memset (password, '\0', strlen(password));
	SendZString(command);

	if (!GetErrString(command, sizeof command)) {
		fprintf(stderr, "Remote aborted connection without initiating protocol: %d.\n",
			WSAGetLastError());
		exit(1);
	}

	if (*command != '\0') {
		char *p = command;
		if (*p == '\001') {
			p++;
		}
		fprintf(stderr, "Remote aborted connection: %s\n", p);
		exit(1);
	}

	if (!NullStdIn) {
		idIn = PassInput();
	} else {
		if (!Close(STDINPUT)) {
			fprintf(stderr, "Failed to close standard input: error = %lu.\n",
				GetLastError());
			exit(1);
		}

		if (shutdown(sIO, 1) == SOCKET_ERROR) {
			fprintf(stderr, "Failed to shutdown from input socket: error = %d.\n",
				WSAGetLastError());
			exit(1);
		}
	}

	idOut = PassOutput();
	idErr = PassError();

	if (!NullStdIn) {
		Wait(idIn, &rvIn);
	} else {
		rvIn = 0;
	}
	Wait(idOut, &rvOut);
	Wait(idErr, &rvErr);

	exit((int)(rvIn | rvOut | rvErr));
}


static char *GetUsername()
{
	static char username[1024];
#ifdef WIN32
	unsigned char tubuf[1024];
	char chDomain[510];
	TOKEN_USER *ptu = (TOKEN_USER *)tubuf;
	SID_NAME_USE snu;
	DWORD cbUser, cbDomain, cbDummy;
	HANDLE hAccessToken;

	if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &hAccessToken)) {
		fprintf(stderr, "Can't open process token: error = %lu.\n", GetLastError());
		exit(1);
	}

	if (!GetTokenInformation(hAccessToken, TokenUser, ptu, sizeof tubuf, &cbDummy)) {
		fprintf(stderr, "Can't get user sid: error = %lu.\n", GetLastError());
		exit(1);
	}

	cbUser = sizeof username;
	cbDomain = sizeof chDomain;
	if (!LookupAccountSid(NULL, ptu->User.Sid, username, &cbUser, chDomain, &cbDomain, &snu)) {
		fprintf(stderr, "Can't get user name: error = %lu.\n", GetLastError());
		exit(1);
	}

	(void)CloseHandle(hAccessToken);
#else
	struct passwd *pw;
	pw = getpwuid(getuid());
	if (!pw) {
		fprintf(stderr, "Can't get user name.\n");
		exit(1);
	}
	strncpy(username, pw->pw_name, sizeof username);
	username[sizeof username-1] = '\0';
#endif
	return username;
}


static char *GetPassword()
{
	static char password[30];
	char *p;
#ifdef WIN32
	HANDLE hConIn, hConOut;
	DWORD dwMode;
	DWORD cbRead, cbWritten;

	hConIn = CreateFile("CONIN$", GENERIC_READ | GENERIC_WRITE,
		FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, 0);

	if (hConIn == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "Can't open Console for input: %lu\n", GetLastError());
		exit(1);
	}

	hConOut = CreateFile("CONOUT$", GENERIC_READ | GENERIC_WRITE,
		FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, 0);

	if (hConOut == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "Can't open Console for output: %lu\n", GetLastError());
		exit(1);
	}

	(void) WriteFile(hConOut, "Password: ", 10, &cbWritten, 0);

	if (!GetConsoleMode(hConIn, &dwMode)) {
		fprintf(stderr, "Can't Console input mode: %lu\n", GetLastError());
		exit(1);
	}

	dwMode &= ~(ENABLE_ECHO_INPUT);

	(void) signal(SIGINT, SIG_IGN);

	(void)SetConsoleMode(hConIn, dwMode);

	(void)ReadFile(hConIn, password, sizeof password, &cbRead, 0);

	dwMode |= ENABLE_ECHO_INPUT;
	(void)SetConsoleMode(hConIn, dwMode);

	(void) signal(SIGINT, SIG_DFL);
	
	(void) WriteFile(hConOut, "\r\n", 2, &cbWritten, 0);


	(void) CloseHandle(hConIn);
	(void) CloseHandle(hConOut);
#else
	struct sgttyb osb, nsb;
	int fd;

	fd = open("/dev/tty", 2);

	if (fd == -1) {
		fprintf(stderr, "Can't open console: %ld\n", errno);
		exit(1);
	}

	(void) write(fd, "Password: ", 10);

	if (gtty(fd, &osb) == -1) {
		fprintf(stderr, "Can't get console settings: %ld\n", errno);
		exit(1);
	}
	nsb = osb;
	nsb.sg_flags &= ~(ECHO);
	(void) signal(SIGINT, SIG_IGN);

	(void)stty(fd, &nsb);
	(void)read(fd, password, sizeof password);
	(void)stty(fd, &osb);

	(void) write(fd, "\r\n", 2);
	(void)close(fd);

	(void) signal(SIGINT, SIG_DFL);
#endif
	if ((p=strchr(password, '\r')) != NULL) {
		*p = '\0';
	}

	if ((p=strchr(password, '\n')) != NULL) {
		*p = '\0';
	}

	return password;
}


static void Usage(void)
{
#ifdef ALLOWDASH_P
	fprintf(stderr, "rexec host [-n] [-l loginname] [-p password] command [arg ...]\n");
#else
	fprintf(stderr, "rexec host [-n] [-l loginname] command [arg ...]\n");
#endif
	exit(1);
}


static IDENT PassInput()
{
	IDENT id;
#ifdef WIN32
	id = (IDENT)_beginthread(PassInputThread, 4096, NULL);
	if ((long)id == -1) {
		fprintf(stderr, "Could not start input passing thread: error = %lu\n", GetLastError());
		exit(1);
	}
	return id;
}

static void PassInputThread(void *)
{
#else
	id = fork();
	if (id == -1) {
		fprintf(stderr, "Fork for input passing failed: error = %d\n", errno);
		exit(1);
	}
	if (id != 0) {
		return id;
	}
#endif
	{
	RETVAL retval = 0;
	int count;
	char buf[4096];

	while ((count=Read(STDINPUT, buf, sizeof buf)) > 0) {
		if (!Send(sIO, buf, count)) {
			fprintf(stderr, "Error writing input to socket: error = %d.\n",
				WSAGetLastError());
			retval = 1;
			break;
		}
	}

	if (count == -1) {
		fprintf(stderr, "Error reading from standard input: error = %lu.\n",
			GetLastError());
		retval = 1;
	} else {
		if (!Close(STDINPUT)) {
			fprintf(stderr, "Error closing standard input: error = %lu.\n", GetLastError());
			retval = 1;
		}
	}

	if (shutdown(sIO, 1) == SOCKET_ERROR) {
		fprintf(stderr, "Failed to shut from input socket down: error = %d.\n",
			WSAGetLastError());
		retval = 1;
	}

#ifdef WIN32
	ExitThread(retval);
#else
	exit(retval);
#endif
	}
}


static IDENT PassOutput()
{
	IDENT id;
#ifdef WIN32
	id = (IDENT)_beginthread(PassOutputThread, 4096, NULL);
	if ((long)id == -1) {
		fprintf(stderr, "Could not start output passing thread: error = %lu\n", GetLastError());
		exit(1);
	}
	return id;
}

static void PassOutputThread(void *)
{
#else
	id = fork();
	if (id == -1) {
		fprintf(stderr, "Fork for output passing failed: error = %d\n", errno);
		exit(1);
	}
	if (id != 0) {
		return id;
	}
#endif
	{
	RETVAL retval = 0;
	int count;
	char buf[4096];

	while ((count=recv(sIO, buf, sizeof buf, 0)) > 0) {
		if (!Write(STDOUTPUT, buf, count)) {
			fprintf(stderr, "Error writing to standard output: error = %lu.\n", GetLastError());
			retval = 1;
			break;
		}
	}

	if (count == -1) {
		fprintf(stderr, "Error passing standard output from socket: error = %d.\n",
			WSAGetLastError());
		retval = 1;
	}

	if (!Close(STDOUTPUT)) {
		fprintf(stderr, "Error closing standard output: error = %lu.\n", GetLastError());
		retval = 1;
	}

	if (count != -1) {
		if (shutdown(sIO, 0) == SOCKET_ERROR) {
			fprintf(stderr, "Failed to shutdown standard output socket: error = %d.\n",
				WSAGetLastError());
			retval = 1;
		}
	}

#ifdef WIN32
	if (idIn) {
		TerminateThread(idIn, 0);
	}
	ExitThread(retval);
#else
	if (idIn) {
		kill(idIn, SIGTERM);
	}
	exit(retval);
#endif
	}
}


static IDENT PassError()
{
	IDENT id;
#ifdef WIN32
	id = (IDENT)_beginthread(PassErrorThread, 4096, NULL);
	if ((long)id == -1) {
		fprintf(stderr, "Could not start error passing thread: error = %lu\n", GetLastError());
		exit(1);
	}
	return id;
}

static void PassErrorThread(void *)
{
#else
	id = fork();
	if (id == -1) {
		fprintf(stderr, "Fork for error passing failed: error = %d\n", errno);
		exit(1);
	}
	if (id != 0) {
		return id;
	}
#endif
	{
	RETVAL retval = 0;
	int count;
	char buf[4096];

	while ((count=recv(sErr, buf, sizeof buf, 0)) > 0) {
		if (!Write(STDERROR, buf, count)) {
			fprintf(stderr, "Error writing to standard error: error = %lu.\n", GetLastError());
			retval = 1;
			break;
		}
	}

	if (count == -1) {
		fprintf(stderr, "Error passing standard error from socket: error = %d.\n",
			WSAGetLastError());
		retval = 1;
	}

	if (!Close(STDERROR)) {
		fprintf(stderr, "Error closing standard output: error = %lu.\n", GetLastError());
		retval = 1;
	}

	if (count != -1) {
		if (shutdown(sErr, 0) == SOCKET_ERROR) {
			fprintf(stderr, "Failed to shutdown standard error socket: error = %d.\n",
				WSAGetLastError());
			retval = 1;
		}
	}

#ifdef WIN32
	ExitThread(retval);
#else
	exit(retval);
#endif
	}
}


static void OpenService(const char *remote_host)
{
#ifdef WIN32
	WSADATA wsadata;
#endif
	struct sockaddr_in server_addr, my_err_addr, junk_addr;
	struct servent *sv;
	struct hostent *hent;
	static char portbuf[30];
	SOCKET sTmp;
	int addr_len;

#ifdef WIN32
	if (WSAStartup(MAKEWORD(1,1), &wsadata) != 0) {
		fprintf(stderr, "Failed to initialize TCP/IP: error=%d.\n", WSAGetLastError());
		exit(1);
	}

	if (LOBYTE(wsadata.wVersion) != 1 || HIBYTE(wsadata.wVersion) != 1) {
		fprintf(stderr, "Old version of TCP/IP: error=%d.\n", WSAGetLastError());
		exit(1);
	}
#endif
	if (atexit(Cleanup) != 0) {	
#ifdef WIN32
		WSACleanup();
#endif
		fprintf(stderr, "Could not register TCP/IP cleanup function.\n");
		exit(1);
	}

	hent = gethostbyname(remote_host);
	if(!hent) {
		fprintf(stderr, "Lookup of server hostname failed: error=%d.\n",
			WSAGetLastError());
		exit(1);
	}

	sv=getservbyname("exec", "tcp");
	if (!sv) {
		fprintf(stderr, "Lookup of port number for rexec service failed: error=%d.\n",
			WSAGetLastError());
		exit(1);
	}

	memcpy((char *)&server_addr.sin_addr, hent->h_addr, hent->h_length);
	server_addr.sin_family = hent->h_addrtype;
	server_addr.sin_port = sv->s_port;

	if((sIO=socket(PF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
		fprintf(stderr, "I/O socket creation failed: error=%d.\n",
			WSAGetLastError());
		exit(1);
	}

	if(connect(sIO, (struct sockaddr *)&server_addr, sizeof server_addr) == SOCKET_ERROR) {
		fprintf(stderr, "I/O socket connection failed: error=%d.\n",
			WSAGetLastError());
		exit(1);
	}

	memset(&my_err_addr, '\0', sizeof my_err_addr);
	my_err_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	my_err_addr.sin_family = AF_INET;
	my_err_addr.sin_port = 0;

	if ((sTmp=socket(PF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
		fprintf(stderr, "Error socket creation failed: error=%d.\n",
			WSAGetLastError());
		exit(1);
	}

	if (bind(sTmp, (struct sockaddr *)&my_err_addr, sizeof my_err_addr) == SOCKET_ERROR) {
		fprintf(stderr, "Error socket bind failed: error=%d.\n",
			WSAGetLastError());
		(void) closesocket(sTmp);
		exit(1);
	}

	if (listen(sTmp, 1) == SOCKET_ERROR) {
		fprintf(stderr, "Error socket listen failed: error=%d.\n",
			WSAGetLastError());
		(void) closesocket(sTmp);
		exit(1);
	}	

	addr_len = sizeof my_err_addr;
	if (getsockname(sTmp, (struct sockaddr *)&my_err_addr, &addr_len) == SOCKET_ERROR) {
		fprintf(stderr, "Error socket bind failed: error=%d.\n",
			WSAGetLastError());
		(void) closesocket(sTmp);
		exit(1);
	}

	sprintf(portbuf, "%hu", ntohs(my_err_addr.sin_port));

	SendZString(portbuf);

	addr_len = sizeof junk_addr;
	if ((sErr = accept(sTmp, (struct sockaddr *)&junk_addr, &addr_len))
		== INVALID_SOCKET) {
		fprintf(stderr, "Error socket accept failed: error=%d.\n",
			WSAGetLastError());
		(void) closesocket(sTmp);
		exit(1);
	}

	(void) closesocket(sTmp);
}

static void Cleanup(void)
{
	if (sIO != INVALID_SOCKET) {
		(void)closesocket(sIO);
	}

	if (sErr != INVALID_SOCKET) {
		(void)closesocket(sErr);
	}

#ifdef WIN32
		WSACleanup();
#endif
}


static BOOL Close(FILECOOKIE fc)
{
#ifdef WIN32
	return CloseHandle(fc);
#else
	if (close(fc) == -1) {
		return FALSE;
	}
	return TRUE;
#endif
}


static int Read(FILECOOKIE fc, char *buf, size_t nbuf)
{
#ifdef WIN32
	DWORD cbRead;
	if (!ReadFile(fc, buf, nbuf, &cbRead, NULL)) {
		return -1;
	}
	return (int)cbRead;
#else
	return read(fc, buf, nbuf);
#endif
}


static BOOL Write(FILECOOKIE fc, const char *buf, size_t nbuf)
{
#ifdef WIN32
	DWORD cbWritten;

	if (!WriteFile(fc, buf, nbuf, &cbWritten, NULL)) {
		return FALSE;
	}
	if (cbWritten != nbuf) {
		return FALSE;
	}
	return TRUE;
#else
	if (write(fc, buf, nbuf) != nbuf) {
		return FALSE;
	}
	return TRUE;
#endif
}


static BOOL Send(SOCKET s, const char *buf, size_t nbuf)
{
	int cnt;
	size_t sent = 0;

	while (sent < nbuf) {
		cnt = send(s, &buf[sent], nbuf-sent, 0);
		if (cnt == -1) {
			return FALSE;
		}
		sent += cnt;
	}
	return TRUE;
}


static BOOL SendZString(const char *str)
{
	return Send(sIO, str, strlen(str)+1);
}


static BOOL GetErrString(char *str, size_t len)
{
	size_t pos = 0;

	while (pos < len) {
		char ch;
		if (recv(sIO, &ch, 1, 0) != 1) {
			return FALSE;
		}
		str[pos++] = ch;
		if (ch == '\0') {
			return TRUE;
		}
		if (ch == '\n') {
			return TRUE;
		}
	}
	return FALSE;
}


static void
Wait(IDENT id, RETVAL *prv)
{
#ifdef WIN32
	DWORD	dwRc;
	BOOL	bSuccess;

	dwRc = WaitForSingleObject(id, INFINITE);
	if (WAIT_FAILED == dwRc) 
	{
		*prv = 2;
	} 
	else 
	{
		bSuccess = GetExitCodeThread(id, prv);
		if (!bSuccess) 
		{
			*prv = 4;
		}
	}
#else
	if (waitpid(id, prv, 0) == -1) {
		*prv = 2;
	} else {
		if (WIFEXITED(*prv)) {
			*prv = WEXITSTATUS(*prv);
		} else {
			*prv = 3;
		}
	}
#endif
}
