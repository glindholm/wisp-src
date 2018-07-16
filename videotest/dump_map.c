#include <stdio.h>
#include <v/video.h>

dump_map()
{
	extern int vscr_cng, vlin_cng[MAX_LINES_PER_SCREEN];
	extern char vchr_map[24][MAX_COLUMNS_PER_LINE];
	extern char vatr_map[24][MAX_COLUMNS_PER_LINE];
	extern char vmap_cng[24][MAX_COLUMNS_PER_LINE];
	register int i, j, k, p;
	FILE *fopen(), *fid;

	fid = fopen("VIDEO.DMP","w");

	p = 0;
	for (i = 0; i < 24; i++)
	{
		k = vml(i);
		vchr_map[k][90] = NULL;
		fprintf(fid,"\n%2d - %s\n",i,&vchr_map[k][0]);
		for (j = 0; j < 90; j++)
		{
			fprintf(fid,"%3o ",vchr_map[k][j]);
			if ((j == 29) || (j == 59)) fprintf(fid,"\n");
		}
		fprintf(fid,"\n");

		for (j = 0; j < 90; j++)
		{
			fprintf(fid,"%3o ",vatr_map[k][j]);
			if ((j == 29) || (j == 59)) fprintf(fid,"\n");
		}
		fprintf(fid,"\n");

		for (j = 0; j < 90; j++)
		{
			if (vmap_cng[k][j] == 0) fprintf(fid,"--- ");
			else if (vmap_cng[k][j] == 1) fprintf(fid,"NEW ");
			else fprintf(fid,"OLD ");
			if ((j == 29) || (j == 59)) fprintf(fid,"\n");
		}

		fprintf(fid,"vlin_cng[%d] = %d\n",i,vlin_cng[i]);

		if (p++ == 3) {fprintf(fid,"\n\f"); p = 0;}
		else fprintf(fid,"\n");
	}
	fclose(fid);
	return(0);
}
