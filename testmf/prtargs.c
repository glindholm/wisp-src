/*
	prtargs - print the args
		- used to test LINKPROC
*/
#include <stdio.h>
main(argc,argv)
int	argc;
char	*argv[];
{
	int	i;

	printf("\n prtargs - Print Arguments\n\n\n");
	printf(" argc = %d\n\n", argc);

	for(i=0; argv[i]; i++)
	{
		printf("arg[%d] = <%s>\n", i, argv[i]);
	}

	printf("\n\n press return to continue\n");
	getchar();
}

