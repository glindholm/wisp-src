			/************************************************************************/
			/*	             VIDEO VIEW Screen Management System		*/
			/*			    Copyright (c) 1993				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*			Include standard header files.										*/

#include <ctype.h>										/* Get character type macros.	*/
#include <stdio.h>										/* Reference standard I/O.	*/
#include <v/video.h>										/* Reference video definitions.	*/

#define DQ '\042'										/* Double quotes.		*/
#define SQ '\047'										/* Single quote.		*/

#define SINGLE_HORIZONTAL 0									/* Define types of lines.	*/
#define DOUBLE_HORIZONTAL 1
#define SINGLE_VERTICAL   2
#define DOUBLE_VERTICAL   3

/*			Function definitions.											*/

static int adjacent();
static int rule();
static int isline();
static int iscont();
static int wascont();
static int obedient();
static int isif();
static int samelevel();
static int detind();
static void adjind();
static int addp();
static int gna();
static int gl();
static void pl();
static void glw();
static void put_after();
static int decode_after();
static int crossing_lines();
static void fapa();
static void process_form();
static int process_field();
static int under_only();
static void strip_header();
static void process_specs();
static int pxs();
static void txs();
static void remove_file();

static char pushed_text[256];
static int pushed_line = FALSE;
static int verbosity = 0;
static int add_underscore = FALSE;
static char tbuf[24][81];
static int tl;
static char ldc[4] = { ',', ';', '.', ':' };
static int rules[4][36] = {
	 1, 2, 3, 4, 0, 0, 0, 0, 9,10,11,12, 0, 0, 0, 0,17,18,19,20, 0, 0, 0, 0,25,26,27,28, 0, 0, 0, 0,33, 0,35, 0,
	 0, 0, 0, 0, 5, 6, 7, 8, 0, 0, 0, 0,13,14,15,16, 0, 0, 0, 0,21,22,23,24, 0, 0, 0, 0,29,30,31,32, 0,34, 0,36,
	 1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0,17,18,19,20,21,22,23,24, 0, 0, 0, 0, 0, 0, 0, 0,33,34, 0, 0,
	 0, 0, 0, 0, 0, 0, 0, 0, 9,10,11,12,13,14,15,16, 0, 0, 0, 0, 0, 0, 0, 0,25,26,27,28,29,30,31,32, 0, 0,35,36};

/*			Entry point.												*/

vfcon(ipf, v0, v1) char *ipf; int v0,v1;
{
	int i,j,nf;
	char c;
	char inbuf[256], outbuf[256], outfile[256], infile[256];
	FILE *in, *out;
	int ftl[MAX_NUMBER_OF_FORMS];

	verbosity = 0;
	if (v0) verbosity = 1;
	if (v1) verbosity = 2;

	strcpy(infile,ipf);
	strcpy(outfile,ipf);

	if (strpos(infile,".hpf") < 0) strcat(infile,".hpf");

	if ((in = fopen(infile,"r")) == NULL)
	{
		printf("Error: Unable to open input file %s\n",infile);
		exit();
	}

	if ((i = strpos(outfile,".hpf")) >= 0) outfile[i] = CHAR_NULL;
	strcat(outfile,".vvf");

#ifndef VMS
	if ((out = fopen(outfile,"r")) != NULL)
	{
		fclose(out);
		printf("The output file %s already exists, overwrite (Y/N)? ",outfile);
		c = getchar();
		if ((c == 'Y') || (c == 'y'));
		else
		{
			printf("Exiting, output file not changed.\n");
			exit();
		}
	}
#endif

	if ((out = fopen("temp.sif","w")) == NULL)
	{
		printf("Error: Unable to open work file 'temp.sif'\n");
		exit();
	}

	strip_header(in,out);
	fclose(out);
	fclose(in);
	in = fopen("temp.sif","r");

	if ((out = fopen(outfile,"w")) == NULL)
	{
		printf("Error: Unable to open output file %s\n",outfile);
		exit();
	}

	fprintf(out,"Video View Forms Control File V4.01\n");
	if (verbosity >= 1) printf("    Video View File Version: V4.01\n");

	i = gna(in,"There are ");
	if ((i < 1) || (i > MAX_NUMBER_OF_FORMS-1))
	{
		printf("Error: %d is an invalid number of forms.\n",i);
		exit();
	}
	fprintf(out,"%d\n",i);
	if (verbosity >= 1)
	{
		printf("               Listing File: %s\n",infile);
		printf("             New Forms File: %s\n",outfile);
		printf("                 Form Count: %d\n",i);
	}
	nf = i;

	glw(in,inbuf,"Next Form");
	gl(in,inbuf);
	for (i = 0; i < nf; i++)
	{
		gl(in,inbuf);
		j = ffn(8,inbuf);
		j--;
		ftl[i] = decode_after(&inbuf[j]," ");
	}

	fclose(in);
	in = fopen("temp.sif","r");

	fapa(in,out,"Head Form: ");
	fapa(in,out,"Default Display Enhancement: ");
	fapa(in,out,"Error Enhancement: ");
	fapa(in,out,"Window Display Line: ");
	fapa(in,out,"Window Enhancement: ");

	for (i = 0; i < nf; i++) process_form(in,out,ftl[i]);

	fclose(in);
	fclose(out);

	if (!v1)
	{
		remove_file("temp.tmp");
		remove_file("temp.fie");
		remove_file("temp.ini");
		remove_file("temp.con");
		remove_file("temp.fin");
		remove_file("temp.sif");
	}

	return(SUCCESS);
}

static void strip_header(in,out) FILE *in,*out;
{
	int i,j;
	char buf1[256], buf2[256];

	while (gl(in,buf1))
	{
		if (buf1[0] == CHAR_NULL)
		{
			if (!gl(in,buf2)) return;
			if (strpos(buf2,"FORMSPEC  VERSION") >= 0)
			{
				gl(in,buf1);
				gl(in,buf1);
				if (strpos(buf1,"FORM NAME:") >= 0);
				else if (buf1[0] == CHAR_NULL)
				{
					gl(in,buf2);
					if (strpos(buf2,"Formsfile directory continued:") >= 0)
					{
						gl(in,buf1);
						gl(in,buf1);
						gl(in,buf1);
						gl(in,buf1);
						gl(in,buf1);
					}
					else
					{
						pl(buf2);
					}
				}
			}
			else
			{
				pl(buf2);
				fprintf(out,"%s\n",buf1);
			}
		}
		else fprintf(out,"%s\n",buf1);

	}
}

static int gna(in,string) FILE *in; char *string;
{
	int i,j;
	char buff[256];
	int scanning;
	int number;

	scanning = TRUE;

	while(scanning)
	{
		if (gl(in,buff))
		{
			i = strpos(buff,string);
			if (i >= 0)
			{
				scanning = FALSE;
				number = decode_after(buff,string);
			}
		}
		else
		{
			scanning = FALSE;
			number = -1;
		}
	}

	return(number);
}

static void fapa(in,out,string) FILE *in,*out; char *string;
{
	int i,j;
	char buff[256];
	int scanning;
	int number;

	scanning = TRUE;

	while(scanning)
	{
		if (gl(in,buff))
		{
			i = strpos(buff,string);
			if (i >= 0)
			{
				put_after(out,buff,string);
				if (verbosity >= 1) printf("%s\n",buff);
				scanning = FALSE;
			}
		}
		else
		{
			printf("Abort: Unable to find %s\n",string);
			exit();
		}
	}
}

static void glw(in,buff,string) FILE *in; char *buff, *string;
{
	int i,j;
	int scanning;

	scanning = TRUE;
	while(scanning)
	{
		if (gl(in,buff))
		{
			i = strpos(buff,string);
			if (i >= 0) scanning = FALSE;
		}
		else
		{
			printf("Abort: Unable to find %s\n",string);
			exit();
		}
	}
}

static void pl(buff) char *buff;
{
	pushed_line = TRUE;
	strcpy(pushed_text,buff);
}

static void put_after(out,buff,string) FILE *out; char *buff,*string;
{
	int i,j;
	char *ptr;

	i = strpos(buff,string);
	j = strlen(string);
	ptr = buff+i+j;

	fprintf(out,"%s\n",ptr);
}

static int decode_after(buff,string) char *buff, *string;
{
	int i,j;
	char *ptr;
	int value;

	value = 0;
	
	i = strpos(buff,string);
	j = strlen(string);
	ptr = buff+i+j;

	while (*ptr == ' ') ptr++;
	while (*ptr != ' ')
	{
		value = (value*10) + (*ptr - '0');
		ptr++;
	}
	return(value);
}

static int ffn(n,buff) int n; char *buff;
{
	int i,j;
	char *p;

	p = buff;										/* Point to the buffer.		*/
	while (*p == ' ') p++;									/* Find the 1st field.		*/
	for (i = 0; i < n-1; i++)
	{
		while (*p != ' ') p++;								/* Find EO last field.		*/
		while (*p == ' ') p++;								/* Find the next field.		*/
	}

	return((int)(p-buff));									/* Return the index.		*/
}

static int under_only(buff) char *buff;
{
	int i,j;
	int only, one;

	only = TRUE;
	one = FALSE;

	for (i = 0; (i < strlen(buff)) & only; i++)
	{
		if ((buff[i] == '_')) one = TRUE;
		if ((buff[i] == ' ') || (buff[i] == '_'));
		else only = FALSE;
	}

	return(only&one);
}

static int addp(outbuf,inbuf,string) char outbuf[],inbuf[]; char *string;
{
	int i,j;
	char temp[256];
	char *ib, *tb;

	ib = inbuf;
	tb = temp;

	i = strpos(inbuf,string);
	i = i + strlen(string);
	ib = ib + i + 1;

	while (*ib == ' ') ib++;
	while ((*ib != ' ') && (*ib)) *tb++ = *ib++;

	*tb = CHAR_NULL;
	strcat(outbuf,temp);
	strcat(outbuf," ");
}

static int process_field(in,out,row,col,len,k)	FILE *in,*out;
						int row[],col[],len[],k;
{
	int i,j;
	int r,c,l;
	char inbuf[256], outbuf[256], inbuf2[256], temp[256];

	r = row[k];
	c = col[k];

	glw(in,inbuf,"Field: ");
	outbuf[0] = CHAR_NULL;
	addp(outbuf,inbuf,"Field:");

	gl(in,inbuf);

	addp(outbuf,inbuf,"Name:");
	addp(outbuf,inbuf,"Num:");

	sprintf(temp,"%d %d ",r+1,c+1);
	strcat(outbuf,temp);

	l = decode_after(inbuf,"Len: ");
	if (l >= len[k]) k++;
	else
	{
		c = c + l;
		len[k] = len[k] - l;
		col[k] = col[k] + l;
	}

	addp(outbuf,inbuf,"Len:");
	addp(outbuf,inbuf,"FType:");

	gl(in,inbuf2);
	i = strpos(inbuf2,"Init Value:") + 11;
	if (strlen(&inbuf2[i])) strcat(outbuf,&inbuf2[i+1]);
	fprintf(out,"%s\n",outbuf);
	if (verbosity >= 2) printf("   Field: %s\n",outbuf);

	strcpy(outbuf,"");
	addp(outbuf,inbuf,"Enh:");
	if (add_underscore) strcat(outbuf,"U");
	fprintf(out,"%s\n",outbuf);
	if (verbosity >= 2) printf("      Enh: %s\n",outbuf);

	strcpy(outbuf,"");
	addp(outbuf,inbuf,"DType:");
	fprintf(out,"%s\n",outbuf);
	if (verbosity >= 2) printf("      DType: %s\n",outbuf);

	gl(in,inbuf);
	if (strpos(inbuf,"*** PROCESSING SPECIFICATIONS ***") >= 0) process_specs(in,out);
	else
	{
		fprintf(out,"0\n");
		fprintf(out,"0\n");
		fprintf(out,"0\n");
	}

	return(k);
}

static void process_specs(in,out) FILE *in, *out;
{
	int i,j;
	char inbuf[256], inbuf2[256];
	int active;
	int inicnt, fiecnt, fincnt, concnt;

	inicnt = 0;									/* Assume there are no edit.		*/
	fiecnt = 0;
	fincnt = 0;
	concnt = 0;

	active = TRUE;
	while (active)
	{
		if (!gl(in,inbuf)) active = FALSE;					/* End of file means we are done.	*/
		if (inbuf[0] == CHAR_NULL) active = FALSE;				/* A blank line means we are done.	*/
		else if (strpos(inbuf,"Field: ") == 0)					/* The tag "Field:" means we are done.	*/
		{
			active = FALSE;
			pl(inbuf);							/* But put it back to avoid overrun.	*/
		}
		else if (strcmp(inbuf,"CONFIG") == 0) concnt = pxs(in,"con");		/* Is this a config edit?		*/
		else if (strcmp(inbuf,"INIT") == 0) inicnt = pxs(in,"ini");		/* Init edits?				*/
		else if (strcmp(inbuf,"FIELD") == 0) fiecnt = pxs(in,"fie");		/* Stated field edits?			*/
		else if (strcmp(inbuf,"FINISH") == 0) fincnt = pxs(in,"fin");		/* Finish edits?			*/
		else fiecnt = pxs(in,"fie");						/* Default to field edits.		*/
	}

	txs(inicnt,out,"temp.ini");
	txs(fiecnt,out,"temp.fie");
	txs(fincnt,out,"temp.fin");
}

static void txs(count,out,file) int count; FILE *out; char *file;
{
	int i,j;
	char buff[256];
	FILE *in;
	
	fprintf(out,"%d\n",count);
	if (count)
	{
		in = fopen(file,"r");
		while (vgetline(in,buff))
			fprintf(out,"%s\n",buff);
		fclose(in);
	}
}

static int isif(buff,ix) char buff[]; int ix;
{
	int i,j;

	for (i = 0; i < ix; i++)
	{
		if (buff[i] != ' ') return(FALSE);
	}
	if (!memcmp("IF ",&buff[ix],3)) return(TRUE);
	if (!memcmp("ELSE",&buff[ix],4)) return(TRUE);
	return(FALSE);
}

static int detind(buff) char buff[];
{
	int i,j;

	i = 0;
	while (buff[i] == ' ') i++;
	return(i);
}

static int samelevel(buff,ix) char buff[]; int ix;
{
	if (detind(buff) == ix) return(TRUE);
	return(FALSE);
}

static void adjind(buff,ix) char buff[]; int ix;
{
	int i,j;
	char temp[256];

	if (detind(buff) == (ix * 3)) return;
	if (verbosity >= 2) printf("      Adjusting: %s\n",buff);

	strcpy(temp,&buff[detind(buff)]);
	for (i = 0; i < 256; i++) buff[i] = ' ';
	strcpy(&buff[ix*3],temp);
}

static int iscont(buff) char buff[];
{
	int i,j;

	i = 0;
	while (buff[i] == ' ') i++;
	if ((buff[i] == DQ) || (buff[i] == SQ))
	{
		strcpy(buff,&buff[i]);
		return(TRUE);
	}
	return(FALSE);
}

static int wascont(buff) char buff[];
{
	int i,j;

	if ((i = strlen(buff)) > 0)
	{
		if (buff[i-1] == ',') return(TRUE);
	}
	return(FALSE);
}

static int pxs(in,ext) FILE *in; char *ext;
{
	int i,j;
	int lines;
	char buff[256], buf2[256];
	FILE *out;
	int scanning, inlevel;
	int iflevel;
	int indent[16];
	iflevel = 0;
	lines = 0;

	for (i = 0; i < 16; i++) indent[i] = 0;
	gl(in,buff);
	pl(buff);
	indent[0] = detind(buff);

	strcpy(buff,"temp.");
	strcat(buff,ext);
	out = fopen(buff,"w");

	scanning = TRUE;
	while (scanning && (gl(in,buff)))
	{
		if (buff[0] == CHAR_NULL)
		{
			while (gl(in,buff) && (buff[0] == 0));
			if (feof(in)) scanning = FALSE;
			else if (strpos(buff,"Field: ") == 0) scanning = FALSE;
		}

		if (strpos(buff,"INIT")   == 0) scanning = FALSE;
		else if (strpos(buff,"FIELD")  == 0) scanning = FALSE;
		else if (strpos(buff,"FINISH") == 0) scanning = FALSE;
		else if (strpos(buff,"LOCALEDITS") == 0) scanning = FALSE;
		else if (scanning)
		{
recat:			gl(in,buf2);
			if (iscont(buf2) || wascont(buff))
			{
				strcat(buff," ");
				strcat(buff,buf2);
				if (verbosity >= 2) printf("      Concat: %s\n",buf2);
				goto recat;
			}
			else pl(buf2);

			inlevel = TRUE;
			while (inlevel)
			{
				if (isif(buff,indent[iflevel]))
				{
					iflevel++;
					gl(in,buf2);
					indent[iflevel] = detind(buf2);
					pl(buf2);
					if (indent[iflevel] == indent[iflevel-1])
					{
						fprintf(out,"%s\n",buff);
						lines++;
						gl(in,buff);
						iflevel--;
					}
					else inlevel = FALSE;
				}

				else if (samelevel(buff,indent[iflevel]))
				{
					adjind(buff,iflevel);
					inlevel = FALSE;
				}

				else if (iflevel) iflevel--;
			}
			fprintf(out,"%s\n",buff);
			lines++;
		}
		if (!scanning) pl(buff);
	}

	fclose(out);
	return(lines);
}

static int gl(in,buff) FILE *in; char *buff;
{
	int i;

	for (i = 0; i < 256; i++) buff[i] = CHAR_NULL;

	i = 0;

	if (pushed_line)
	{
		i = SUCCESS;
		strcpy(buff,pushed_text);
		pushed_line = FALSE;
	}
	else i = vgetline(in,buff);
	return(i);
}

static void process_form(in,out,tli) FILE *in,*out; int tli;
{
	int i,j,k;
	int m,n;
	char buff[256];
	char nbuf[256];
	FILE *temp;
	int scanning;
	int fc;
	int row[MAX_FORM_FIELDS], col[MAX_FORM_FIELDS];
	int len[MAX_FORM_FIELDS];
	int in_param;
	char *p;
	int ngl;										/* Count of graphical lines.	*/
	char ltype[256];									/* Type of line constructed.	*/
	int srow[256], scol[256], llen[256];							/* Line data.			*/

	tl = tli;
	fapa(in,out,"Form: ");
	fapa(in,out,"Repeat Option: ");
	fapa(in,out,"Next Form Option: ");
	fapa(in,out,"Next Form: ");
	fprintf(out,"ON\n");								/* Help (key) display defaults to on.	*/
	fprintf(out,"%d\n",tl);

	glw(in,buff,"********* ********* ********* *********");

	k = 0;											/* Row, col, len index.		*/
	for (i = 0; i < tl; i++)
	{
		gl(in,buff);
		gl(in,nbuf);

		if (under_only(nbuf))								/* Underscores only?		*/
		{
			in_param = FALSE;							/* Nothing yet.			*/
			m = 0;
			for (j = 0; j < strlen(nbuf); j++)
			{
				if (nbuf[j] == '_')
				{
					buff[j] = ' ';
					if (!in_param)
					{
						in_param = TRUE;
						m = 1;
						row[k] = i;
						col[k] = j;
					}
					else m++;
				}
				else if (in_param)
				{
					in_param = FALSE;
					len[k] = m;
					k++;
				}
			}
			if (in_param)
			{
				in_param = FALSE;
				len[k] = m;
				k++;
			}
		}
		else pl(nbuf);									/* Push back, its data.		*/
		strcpy(tbuf[i],buff);								/* Copy to the buffer.		*/
	}

	glw(in,buff,"********* ********* ********* *********");

	ngl = 0;										/* Assume no lines.		*/

	for (i = 0; i < tl; i++)								/* Determine the horizontals.	*/
	{
		while(strlen(tbuf[i]) < 80) strcat(tbuf[i]," ");				/* Untrim.			*/
		for (j = 0; j < 80; j++)							/* Loop through each column.	*/
		{
			for (k = SINGLE_HORIZONTAL; k <= DOUBLE_HORIZONTAL; k++)		/* Both types of horizontal.	*/
			{
				if (isline(k,i,j))						/* Is this the start of a line?	*/
				{
					ltype[ngl] = 'h';					/* Record the type.		*/
					if (k == DOUBLE_HORIZONTAL) ltype[ngl] = 'H';
					srow[ngl] = i;						/* Record the start position.	*/
					scol[ngl] = j;
					llen[ngl] = 1;						/* Assume a length of 1.	*/
					j++;
					while ((j < 80) && (isline(k,i,j)))			/* Loop through each line.	*/
					{
						j++;
						llen[ngl]++;
					}
					ngl++;							/* Count the line.		*/
				}
			}
		}
	}

	for (j = 0; j < 80; j++)								/* Determine the verticals.	*/
	{
		for (i = 0; i < tl; i++)							/* Loop through each column.	*/
		{
			for (k = SINGLE_VERTICAL; k <= DOUBLE_VERTICAL; k++)			/* Both types of vertical.	*/
			{
				if (isline(k,i,j))						/* Is this the start of a line?	*/
				{
					ltype[ngl] = 'v';					/* Record the type.		*/
					if (k == DOUBLE_VERTICAL) ltype[ngl] = 'V';
					srow[ngl] = i;						/* Record the start position.	*/
					scol[ngl] = j;
					llen[ngl] = 1;						/* Assume a length of 1.	*/
					i++;
					while ((i < tl) && (isline(k,i,j)))			/* Loop through each line.	*/
					{
						i++;
						llen[ngl]++;
					}
					ngl++;							/* Count the line.		*/
				}
			}
		}
	}

	for (i = 0; i < ngl; i++)								/* Blank where lines will be.	*/
	{
		for (j = 0; j < llen[i]; j++)
		{
			if ((ltype[i] == 'H') || (ltype[i] == 'h')) tbuf[srow[i]][scol[i]+j] = ' ';
			if ((ltype[i] == 'V') || (ltype[i] == 'v')) tbuf[srow[i]+j][scol[i]] = ' ';
		} 
	}

	for (i = 0; i < tl; i++) fprintf(out,"%s\n",tbuf[i]);
	fprintf(out,"%d\n",ngl);								/* Number of lines.		*/

	for (i = 0; i < ngl; i++)
	{
		fprintf(out,"%c %d %d %d\n",ltype[i],srow[i]+1,scol[i]+1,llen[i]);
	}

	if ((temp = fopen("temp.tmp","w")) == NULL)
	{
		printf("Error: Open failed on work file temp.tmp.\n");
		exit();
	}

	scanning = TRUE;
	fc = 0;
	while (scanning)
	{
		if (!gl(in,buff)) scanning = FALSE;

		else if (strpos(buff,"Form: ") >= 0)
		{
			scanning = FALSE;							/* End of field scan.		*/
			pl(buff);								/* Put the buffer back.		*/
		}

		else
		{
			fprintf(temp,"%s\n",buff);
			if (strpos(buff,"Field: ") >= 0) fc++;
		}
	}

	fclose(temp);
	temp = fopen("temp.tmp","r");

	fprintf(out,"%d\n",fc);									/* Write num of fields.		*/
	if (verbosity >= 1) printf("   Field Count: %d\n",fc);

	j = pushed_line;									/* Save the input push.		*/
	strcpy(buff,pushed_text);
	pushed_line = FALSE;

	k = 0;
	for (i = 0; i < fc; i++) k = process_field(temp,out,row,col,len,k);

	if (pushed_line = j) strcpy(pushed_text,buff);
}

static int isline(lt,r,c) int lt,r,c;
{
	int i,j;
	char c0,c1,c2,c3,c4;

	if (lt <= DOUBLE_HORIZONTAL)								/* A horizontal line?		*/
	{
		if (tbuf[r][c] == ldc[lt])							/* The line's base character?	*/
		{
			if ((c > 0) && (tbuf[r][c-1] == ldc[lt])) return(TRUE);
			else if ((c < 79) && (tbuf[r][c+1] == ldc[lt])) return(TRUE);
			else if ((c >  0) && obedient(lt,r,c-1)) return(TRUE);
			else if ((c < 79) && obedient(lt,r,c+1)) return(TRUE);
		}
		else if (obedient(lt,r,c)) return(TRUE);
	}

	else											/* A vertical line.		*/
	{
		c0 = tbuf[r][c];
		if ((lt == DOUBLE_VERTICAL) && (c0 == 'D') && ((c > 77) || (c < 1))) c0 = ':';	/* Some funny special case???	*/
		if (c0 == ldc[lt])
		{
			if (r > 0)
			{
				c1 = tbuf[r-1][c];
				if ((lt == DOUBLE_VERTICAL) && (c1 == 'D') && ((c > 77) || (c < 1))) c1 = ':';
			}

			if (r < tl)
			{
				c2 = tbuf[r+1][c];
				if ((lt == DOUBLE_VERTICAL) && (c2 == 'D') && ((c > 77) || (c < 1))) c2 = ':';
			}

			if ((r > 0) && (c1 == ldc[lt]) && !adjacent(r,c,ldc[lt])) return(TRUE);
			else if ((r < tl) && (c2 == ldc[lt]) && !adjacent(r,c,ldc[lt])) return(TRUE);
			else if ((r >  0) && obedient(lt,r-1,c)) return(TRUE);
			else if ((r < tl) && obedient(lt,r+1,c)) return(TRUE);
		}
		else if (obedient(lt,r,c)) return(TRUE);
	}

	return(FALSE);
}

static int adjacent(r,c,k) int r,c; char k;
{
	if (c <= 0) return(FALSE);
	if (tbuf[r][c-1] == k) return(TRUE);
	if (c >= 79) return(FALSE);
	if (tbuf[r][c+1] == k) return(TRUE);
	return(FALSE);
}

static int rule(n,r,c) int n,r,c;
{
	int i,j;
	char k;

	switch(n)
	{
		case 2:									/* Top T single single.			*/
		{
			if (tbuf[r][c] != '7') return(FALSE);
			if ((c <= 0) || (c >= 79)) return(FALSE);
			if (tbuf[r][c+1] != ',') return(FALSE);
			if (tbuf[r][c-1] != ',') return(FALSE);
			if (r >= tl) return(FALSE);
			if (tbuf[r+1][c] != '.') return(FALSE);
			return(TRUE);
		}

		case 3:									/* Bottom T single single.		*/
		{
			if (tbuf[r][c] != '8') return(FALSE);
			if ((c <= 0) || (c >= 79)) return(FALSE);
			if (tbuf[r][c+1] != ',') return(FALSE);
			if (tbuf[r][c-1] != ',') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != '.') return(FALSE);
			return(TRUE);
		}

		case 6:									/* Top T single vert to double horiz.	*/
		{
			if (tbuf[r][c] != '#') return(FALSE);
			if ((c <= 0) || (c >= 79)) return(FALSE);
			if (tbuf[r][c+1] != ';') return(FALSE);
			if (tbuf[r][c-1] != ';') return(FALSE);
			if (r >= tl) return(FALSE);
			if (tbuf[r+1][c] != '.') return(FALSE);
			return(TRUE);
		}

		case 7:									/* Bottom T single vert to double hor.	*/
		{
			if (tbuf[r][c] != '$') return(FALSE);
			if ((c <= 0) || (c >= 79)) return(FALSE);
			if (tbuf[r][c+1] != ';') return(FALSE);
			if (tbuf[r][c-1] != ';') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != '.') return(FALSE);
			return(TRUE);
		}

		case 9:									/* Left T double vert to single horiz.	*/
		{
			if (tbuf[r][c] != '!') return(FALSE);
			if ((r <= 0) || (r >= tl)) return(FALSE);
			if ((tbuf[r-1][c] != ':') && (tbuf[r-1][c] != 'D')) return(FALSE);
			if ((tbuf[r+1][c] != ':') && (tbuf[r+1][c] != 'D')) return(FALSE);
			if (c >= 79) return(FALSE);
			if ((c > 0) && (tbuf[r][c+1] != ',')) return(FALSE);
			return(TRUE);
		}

		case 12:								/* Right T double vert to single horiz.	*/
		{
			if (tbuf[r][c] != '"') return(FALSE);
			if ((r <= 0) || (r >= tl)) return(FALSE);
			if ((tbuf[r-1][c] != ':') && (tbuf[r-1][c] != 'D')) return(FALSE);
			if ((tbuf[r+1][c] != ':') && (tbuf[r+1][c] != 'D')) return(FALSE);
			if (c <= 0) return(FALSE);
			if (tbuf[r][c-1] != ',') return(FALSE);
			return(TRUE);
		}

		case 13:								/* Left T double double.		*/
		{
			if (tbuf[r][c] != '1') return(FALSE);
			if ((r <= 0) || (r >= tl)) return(FALSE);
			if (!((tbuf[r-1][c] == ':') || (tbuf[r-1][c] == 'D')
			   || (tbuf[r+1][c] == ':') || (tbuf[r+1][c] == 'D'))) return(FALSE);
			if (c >= 79) return(FALSE);
			if ((c > 0) && (tbuf[r][c+1] != ';')) return(FALSE);
			return(TRUE);
		}

		case 14:								/* Top T double double.			*/
		{
			if (tbuf[r][c] != '3') return(FALSE);
			if ((c <= 0) || (c >= 79)) return(FALSE);
			if (tbuf[r][c+1] != ';') return(FALSE);
			if (tbuf[r][c-1] != ';') return(FALSE);
			if (r >= tl) return(FALSE);
			if (tbuf[r+1][c] != ':') return(FALSE);
			return(TRUE);
		}

		case 15:								/* Bottom T double double.		*/
		{
			if (tbuf[r][c] != '4') return(FALSE);
			if ((c <= 0) || (c >= 79)) return(FALSE);
			if (tbuf[r][c+1] != ';') return(FALSE);
			if (tbuf[r][c-1] != ';') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != ':') return(FALSE);
			return(TRUE);
		}

		case 16:								/* Right T double double.		*/
		{
			if (tbuf[r][c] != '2') return(FALSE);
			if ((r <= 0) || (r >= tl)) return(FALSE);
			if (!((tbuf[r-1][c] == ':') || (tbuf[r-1][c] == 'D')
			  || (tbuf[r+1][c] == ':') || (tbuf[r+1][c] == 'D'))) return(FALSE);
			if (c <= 0) return(FALSE);
			if (tbuf[r][c-1] != ';') return(FALSE);
			return(TRUE);
		}

		case 17:								/* Upper left corner, single single.	*/
		{
			if (tbuf[r][c] != 'R') return(FALSE);
			if (r >= tl) return(FALSE);
			if (tbuf[r+1][c] != '.') return(FALSE);
			if (c >= 79) return(FALSE);
			if ((c > 0) && (tbuf[r][c+1] != ',')) return(FALSE);
			return(TRUE);
		}

		case 18:								/* Upper right corner, single single.	*/
		{
			if (tbuf[r][c] != 'T') return(FALSE);
			if (r >= tl) return(FALSE);
			if (tbuf[r+1][c] != '.') return(FALSE);
			if (c <= 0) return(FALSE);
			if ((c < 78) && (tbuf[r][c-1] != ',')) return(FALSE);
			return(TRUE);
		}

		case 19:								/* Lower left corner, single single.	*/
		{
			if (tbuf[r][c] != 'F') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != '.') return(FALSE);
			if (c >= 79) return(FALSE);
			if ((c > 0) && (tbuf[r][c+1] != ',')) return(FALSE);
			return(TRUE);
		}

		case 20:								/* Lower right corner, single single	*/
		{
			if (tbuf[r][c] != 'G') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != '.') return(FALSE);
			if (c <= 0) return(FALSE);
			if ((c < 78) && (tbuf[r][c-1] != ',')) return(FALSE);
			return(TRUE);
		}

		case 23:								/* Lower Left corner, single vertical	*/
		{									/*   double horizontal.			*/
			if (tbuf[r][c] != 'A') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != '.') return(FALSE);
			if (c >= 79) return(FALSE);
			if (tbuf[r][c+1] != ';') return(FALSE);
			return(TRUE);
		}

		case 24:								/* Lower right corner, single vertical	*/
		{									/*   double horizontal.			*/
			if (tbuf[r][c] != 'S') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != '.') return(FALSE);
			if (c <= 0) return(FALSE);
			if (tbuf[r][c-1] != ';') return(FALSE);
			return(TRUE);
		}

		case 29:								/* Upper right corner, double double.	*/
		{
			if ((tbuf[r][c] != 'Q') && (tbuf[r][c] != 'q')) return(FALSE);
			if (r >= tl) return(FALSE);
			if ((tbuf[r+1][c] != ':') && (tbuf[r+1][c] != 'D')) return(FALSE);
			if (c >= 79) return(FALSE);
			if ((c > 0) && (tbuf[r][c+1] != ';')) return(FALSE);
			return(TRUE);
		}

		case 30:								/* Upper left corner, double double.	*/
		{
			if ((tbuf[r][c] != 'W') && (tbuf[r][c] != 'w')) return(FALSE);
			if (r >= tl) return(FALSE);
			if ((tbuf[r+1][c] != ':') && (tbuf[r+1][c] != 'D')) return(FALSE);
			if (c <= 0) return(FALSE);
			if ((c < 78) && (tbuf[r][c-1] != ';')) return(FALSE);
			return(TRUE);
		}

		case 31:								/* Lower left corner, double double.	*/
		{
			if (tbuf[r][c] != 'A') return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != ':') return(FALSE);
			if (c >= 79) return(FALSE);
			if ((c > 0) && (tbuf[r][c+1] != ';')) return(FALSE);
			return(TRUE);
		}

		case 32:								/* Lower right corner, double double.	*/
		{
			if ((tbuf[r][c] != 'S') && (tbuf[r][c] != 's')) return(FALSE);
			if (r <= 0) return(FALSE);
			if (tbuf[r-1][c] != ':') return(FALSE);
			if (c <= 0) return(FALSE);
			if ((c < 78) && (tbuf[r][c-1] != ';')) return(FALSE);
			return(TRUE);
		}

		case 33: return(crossing_lines(r,c,'/','.',','));			/* single vert, single horiz.		*/

		case 34: return(crossing_lines(r,c,'+','.',';'));			/* single vert, double horiz.		*/

		case 35: return(crossing_lines(r,c,'*',':',','));			/* double vert, single horiz.		*/

		case 36: return(crossing_lines(r,c,'0',':',';'));			/* double vert, double horiz.		*/

	}

	return(FALSE);
}

static int obedient(lt,r,c) int lt,r,c;					/* Does a character obey the rules for the line type?	*/
{
	int i,j;

	for (i = 0; i < 36; i++)
	{
		if (rule(rules[lt][i],r,c)) return(TRUE);
	}
	return(FALSE);
}

static void remove_file(path) char *path;
{
#ifdef VMS
	while (delete(path) != -1);
#else
	unlink(path);
#endif
}

crossing_lines(r,c,c0,c1,c2) int r,c; char c0,c1,c2;
{
	if (tbuf[r][c] != c0) return(FALSE);
	if ((r <= 0) || (r >= tl)) return(FALSE);
	if (tbuf[r-1][c] != c1) return(FALSE);
	if (tbuf[r+1][c] != c1) return(FALSE);
	if ((c <= 0) || (c >= 79)) return(FALSE);
	if (tbuf[r][c-1] != c2) return(FALSE);
	if (tbuf[r][c+1] != c2) return(FALSE);
	return(TRUE);
}
