			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include "video.h"									/* Include the video database.		*/
#include "vform.h"									/* Include the view database.		*/
#include "vintdef.h"
#include "vlocal.h"
#include "vplus.h"
#include "vdata.h"
#include <time.h>

/*						Subroutine entry point.								*/

void VPRINTFORM(comarea,printcnt1,pagecnt1) struct vplus_comarea *comarea; int2 *printcnt1, pagecnt1;
{
	char *ctime();
	time_t time_data;
	int i,j, k;
	char buffer[128], *tp, *dp, tb[10], fn[18];
	FILE *fid, *fopen();
	
	dp = vformcurrent->name;
	for (i = 0; (i < 16) && (*(dp+i) != ' '); i++) fn[i] = *(dp+i);			/* Loop through the file name.		*/
	fn[i] = CHAR_NULL;

	buffer[0] = CHAR_NULL;
	strcat(buffer,fn);
	strcat(buffer,"_");

	time_data = time(NULL);
	tp = ctime(&time_data);
	for (i = 0; i < 8; i++) tb[i] = *(tp+i+11);
	tb[i] = CHAR_NULL;
	strcat(buffer,tb);

	strcat(buffer,".lp");

	vre_window("Printing screen to file '%s'.",buffer);
	fid = fopen(buffer,"w");
	fprintf(fid,"Video View Screen Printout - Form %s printed %s\n\n",fn,tp);

	buffer[80] = CHAR_NULL;
	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)
	{
		k = vml(i);
		for (j = 0; j < 80; j++)
		{
			buffer[j] = vchr_map[k][j];
		}
		fprintf(fid,"%s\n",buffer);
	}
	fclose(fid);
}
