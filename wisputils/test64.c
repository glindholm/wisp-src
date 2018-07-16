#define _LARGEFILE64_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

int main(int argc, char *argv[])
{
	struct stat64 buf;

	if (argc != 2)
	{
		printf("usage: test64 {file}\n");
		return 0;
	}

	printf("Calling stat64() on [%s]\n", argv[1]);
	
	if (0 != stat64(argv[1], &buf))
	{
		printf("stat64() failed with errno=[%d] msg=[%s]\n",
		       errno, strerror(errno));
		return 0;
		
	}

	if (sizeof(buf.st_ino) == 8)
	{
		printf("st_ino   [%lld]\n", 	buf.st_ino);
	}
	else
	{
		printf("st_ino   [%d]\n", 	buf.st_ino);
	}
	
	printf("st_size  [%lld]\n", 	buf.st_size);
	
	return 0;
}


