#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ONEMEG 1048576

int main(int argc, char* argv[])
{
	FILE* fh;
	char* filename = "testhuge.out";
	char*  buff;	/* 1 meg buffer */
	int i;

	fh = fopen(filename,"w");
	if (fh == NULL)
	{
		perror("fopen() failed");
		return 1;
	}
	printf("Creating huge file [%s]\n", filename);

	buff = malloc(ONEMEG);
	memset(buff,'*', ONEMEG);
	for(i=0; i < (4*1024 + 100); i++)
	{
		int rc;
		
		rc = fwrite(buff,ONEMEG,1,fh); /* write one meg */
		if (rc != 1)
		{
			printf("ERROR writing record %d (meg)\n", i+1);
			perror("fwrite() failed");
			return 1;
		}

		if ((i+1)%100 == 0)
		{
			printf("Wrote record %d (meg)\n",i+1);
		}
	}

	printf("Created file [%s] with [%d] 1-meg records\n", filename, i);
	return 0;
}
