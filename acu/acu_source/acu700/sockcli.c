/*  sockcli.c - A sample client for socksrv1.cbl  */
/*  Copyright (c) 2001-2005 by Acucorp, Inc.  */
/*  Users of ACUCOBOL may freely use this file.  */

/*  This program demonstrates a client for socksrv1.cbl  */
/*  Note that this program is not meant to be a tutorial on  */
/*  sockets in general, but rather to be a demonstration of  */
/*  how to communicate with the COBOL library routine C$SOCKET.  */
/*  As such, it does little error checking in order to concentrate  */
/*  on the essentials of that communication.  */

/*  With minor modifications, this program could be changed to be  */
/*  the server that sockcli.cbl connects to.  Those modifications are  */
/*  left as an exercise for the reader.  */

#include <stdio.h>
#ifdef	WIN32
#include <winsock.h>
#else	/* WIN32 */
#include <sys/socket.h>
#include <netinet/in.h>
#endif	/* WIN32 */

int
main( argc, argv )
int	argc;
char	*argv[];
{
    int			sd, port;
    char		message[50], *server;
    struct sockaddr_in	server_address;

    if (argc != 3) {
	fprintf( stderr, "Usage: sockcli server port\n" );
	exit( 3 );
    }

    server = argv[1];
    port = atoi( argv[2] );

    /*  This is the standard connection logic in its basic form.  */
    sd = socket( AF_INET, SOCK_STREAM, 0 );
    memset( &server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons( port );
    server_address.sin_addr.s_addr = inet_addr( server );

    connect( sd, (struct sockaddr *) &server_address, sizeof(server_address));

    /*  At this point, if you are running socksrv1.cbl in debug mode,  */
    /*  it has just returned from "C$SOCKET" using AGS_ACCEPT ...  */

    /*  In a loop, get a message from the user to send to the server  */
    for (;;) {
	printf( "Enter a message for the server: " );
	memset( message, 0, sizeof(message));
	/*  If the user presses the End-Of-File character  */
	/*  (usually CTRL-D, or CTRL-Z on Windows) then we close  */
	/*  our socket and quit.  */
	if (NULL == fgets( message, sizeof(message), stdin )) {
	    printf( "Shutting down\n" );
	    break;
	}
	/*  Technically, send() may not actually send the entire  */
	/*  message.  The return value tells how many bytes were  */
	/*  actually sent.  This should be detected, and the send  */
	/*  should be repeated as appropriate until the entire  */
	/*  message is sent.  See a good book on socket programming */
	/*  for details.  */
	send( sd, message, sizeof(message), 0 );

	/*  Technically, recv() may not actually read all the  */
	/*  bytes available.  See a good book on socket programming  */
	/*  for details.  */
	recv( sd, message, sizeof(message), 0 );
	printf( "The server responded: %s", message );
    }

#ifdef	WIN32
    /*  Windows has a slightly different method for closing sockets.  */
    closesocket( sd );
#else	/* WIN32 */
    close( sd );
#endif	/* WIN32 */
    return 0;
}

/*  EOF  */
