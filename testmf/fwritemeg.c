#include <stdio.h>
#include <string.h>
#include <errno.h>

const char* name="meg.out";

void usage(void)
{
	printf("Usage: fwritemeg {Mb}\n\n"
	       "Mb==number of Megabytes to write, 1024 == 1gig\n"
	       "Output file is %s\n", name);
}

int main(int argc, char* argv[])
{
	int	meg = 0;
	int	mx;
	FILE	*fh;
	
	if (argc != 2)
	{
		usage();
		exit(0);
	}
	
	if (1 != sscanf(argv[1],"%d",&meg))
	{
		printf("Couldn't read number of meg to write.\n");
		usage();
		exit(0);
	}

	printf("Creating %s as %d meg\n", name, meg);
	fh = fopen(name,"w");
	if (NULL == fh)
	{
		printf("unable to create %s\n",name);
		exit(0);
	}
	
	for(mx=0; mx<meg; mx++)
	{
		int	i;
		char	buff[1024];
		int	cnt;

		for(i=0;i<1024;i++)
		{
			cnt = fwrite(buff, 1, sizeof(buff), fh);
			if (cnt != sizeof(buff) || ferror(fh))
			{
				printf("Error on fwrite() errno=%d\n", errno);
				exit(1);
			}
		}
	}
	
	fclose(fh);
	printf("Done\n");
	return 0;
}
