static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
	Usage:	$ mergemod {modified} {original} {new} {modcode}

	modified	The modified filename
	original	The original filename
	new		The new filename with merged modcodes
	modcode		The modcode to add to changed records

	Example:

		$ for i in *.wcb
		> do
		> mergemod $i ../original/$i ../new/$i IDSI9407
		> done

		Will also create a merge log file in the new location.
*/

struct rec_s
{
	struct rec_s *next;
	long	relnum;
	long	num;
	char	lineno[7];
	char	data[67];
	char	modcode[9];
};

void print_usage(void);
int untabify(char *str,int size);
struct rec_s *get_next_rec(FILE *fd, char *filename);
struct rec_s *load_file(FILE *fd, char *filename, long *count, int *inseq);
struct rec_s *find_rec(struct rec_s *top, struct rec_s *curr, long num);

static FILE *fd_log;

main(int argc, char *argv[])
{
	FILE	*fd_modified, *fd_original, *fd_new;
	char	logfile[256];
	char	modcode[9];
	struct rec_s *top_modified, *top_original;
	struct rec_s *curr_modified, *curr_original, *temp_rec;
	long	count_modified, count_original, count_new;
	long	new_modcode_count, replace_modcode_count;
	int	inseq;

	if (argc != 5)
	{
		fprintf(stderr,"*** Wrong number of parameters ***\n");
		print_usage();
		exit(1);
	}

	if (!(fd_modified = fopen(argv[1],"r")))
	{
		fprintf(stderr,"*** Unable to open modified file [%s] ***\n",argv[1]);
		print_usage();
		exit(1);
	}
	if (!(fd_original = fopen(argv[2],"r")))
	{
		fprintf(stderr,"*** Unable to open original file [%s] ***\n",argv[2]);
		print_usage();
		exit(1);
	}
	if (!(fd_new = fopen(argv[3],"w")))
	{
		fprintf(stderr,"*** Unable to create new file [%s] ***\n",argv[3]);
		print_usage();
		exit(1);
	}
	sprintf(logfile,"%s.mergelog",argv[3]);
	if (!(fd_log = fopen(logfile,"w")))
	{
		fprintf(stderr,"*** Unable to create log file [%s] ***\n",logfile);
		print_usage();
		exit(1);
	}
	if (strlen(argv[4]) > 8)
	{
		fprintf(stderr,"*** MODCODE is too long [%s] ***\n",argv[4]);
		unlink(argv[3]);
		print_usage();
		exit(1);
	}
	memset(modcode,' ',8);
	memcpy(modcode,argv[4],strlen(argv[4]));
	modcode[8] = (char)0;

	fprintf(fd_log,"%s %s %s %s %s\n",argv[0], argv[1], argv[2], argv[3], argv[4]);

	/*
	**	Load both the modified and original files.
	*/
	top_original = load_file(fd_original, argv[2], &count_original, &inseq);
	fprintf(fd_log,"Original file record count = %ld\n",count_original);
	if (!inseq)
	{
		fprintf(stderr,"*** Original file contains invalid line numbers ***\n");
		fprintf(fd_log,"*** Original file contains invalid line numbers ***\n");
		unlink(argv[3]);
		exit(1);
	}

	top_modified = load_file(fd_modified, argv[1], &count_modified, &inseq);
	fprintf(fd_log,"Modified file record count = %ld\n",count_modified);

	printf("Original record count = %ld  [%s]\n", count_original, argv[2]);
	printf("Modified record count = %ld  [%s]\n", count_modified, argv[1]);

	/*
	**	Merge:
	*/
	new_modcode_count = 0;
	replace_modcode_count = 0;
	count_new = 0;

	curr_modified = top_modified;
	curr_original = top_original;

	while(curr_modified)
	{
		int new_modcode, replace_modcode;

		new_modcode = 0;
		replace_modcode = 0;

		if (0 == strcmp("      ",curr_modified->lineno) || 0 == curr_modified->num)
		{
			/*
			**	New record
			*/
			new_modcode = 1;
		}
		else
		{
			/*
			**	Find the matching original record
			*/
			if (temp_rec = find_rec(top_original,curr_original,curr_modified->num))
			{
				curr_original = temp_rec;
				if (0 == strcmp(curr_original->data, curr_modified->data))
				{
					if ( 0 != strcmp(curr_original->modcode, curr_modified->modcode))
					{
						/*
						**	Replace modcode
						*/
						replace_modcode = 1;
					}
				}
				else
				{
					/*
					**	Record has changed
					*/
					new_modcode = 1;
				}
			}
			else
			{
				/*
				**	Couldn't find a matching record
				*/
				new_modcode = 1;
			}
		}

		if (new_modcode)
		{
			new_modcode_count += 1;
			fprintf(fd_log, "N:%6ld:%s%s%s\n", curr_modified->relnum, 
					curr_modified->lineno, curr_modified->data, curr_modified->modcode);

			strcpy(curr_modified->modcode, modcode);
		}
		else if (replace_modcode)
		{
			replace_modcode_count += 1;
			fprintf(fd_log, "R:%6ld:%s%s%s\n", curr_modified->relnum, 
					curr_modified->lineno, curr_modified->data, curr_modified->modcode);

			strcpy(curr_modified->modcode, curr_original->modcode);
		}

		fprintf(fd_new, "%s%s%s\n", curr_modified->lineno, curr_modified->data, curr_modified->modcode);
		count_new += 1;

		curr_modified = curr_modified->next;
	}

	printf("New      record count = %ld  [%s]\n", count_new, argv[3]);
	printf("New     modcode count = %ld\n",new_modcode_count);
	printf("Replace modcode count = %ld\n",replace_modcode_count);

	fprintf(fd_log,"New file record count = %ld\n",count_new);
	fprintf(fd_log,"New     modcode count = %ld\n",new_modcode_count);
	fprintf(fd_log,"Replace modcode count = %ld\n",replace_modcode_count);

	fclose(fd_new);
	fclose(fd_modified);
	fclose(fd_original);
	fclose(fd_log);
	exit(0);
}

void print_usage(void)
{
	fprintf(stderr,"Usage:  $ mergemod {modified} {original} {new} {modcode}\n\n");
	fprintf(stderr,"   modified    The modified filename.\n");
	fprintf(stderr,"   original    The original filename.\n");
	fprintf(stderr,"   new         The new filename with merged modcodes.\n");
	fprintf(stderr,"   modcode     The modcode to add to changed records.\n");
}

struct rec_s *load_file(FILE *fd, char *filename, long *count, int *inseq)
{
	struct rec_s *top, *curr;
	long	lastnum;

	*count = 0;
	*inseq = 1;
	lastnum = 0;

	top = get_next_rec(fd,filename);
	curr = top;

	while(curr)
	{
		if (curr->num <= lastnum)
		{
			*inseq = 0;
		}

		lastnum = curr->num;

		*count += 1;
		curr->relnum = *count;
		curr->next = get_next_rec(fd,filename);
		curr = curr->next;
	}

	return top;
}

struct rec_s *get_next_rec(FILE *fd, char *filename)
{
	char	inbuff[256];
	int	size;
	char	*ptr;
	struct rec_s	*rec;

	if ( fgets(inbuff,sizeof(inbuff),fd) )
	{
		/*
		**	Cleanup the record:
		**		- remove the newline
		**		- expand tabs
		**		- expand to 80 columns
		*/
		ptr = strchr(inbuff,'\n');
		if (ptr) *ptr = (char)0;

		untabify(inbuff,sizeof(inbuff));
		
		size = strlen(inbuff);
		if (size < 80)
		{
			memset(&inbuff[size],' ',80-size);
		}
		inbuff[80] = (char)0;

		rec = (struct rec_s *)malloc(sizeof(struct rec_s));
		if (!rec)
		{
			fprintf(stderr,"*** Malloc failed ***\n");
			fprintf(fd_log,"*** Malloc failed ***\n");
			exit(1);
		}

		rec->next = NULL;
		rec->relnum = 0;
		memcpy(rec->lineno,inbuff,6);
		rec->lineno[6] = (char)0;
		memcpy(rec->data,&inbuff[6],66);
		rec->data[66] = (char)0;
		memcpy(rec->modcode,&inbuff[72],8);
		rec->modcode[8] = (char)0;
		rec->num = 0;
		sscanf(rec->lineno,"%ld",&rec->num);

		return rec;
	}
	else
	{
		if (feof(fd)) 
		{
			return NULL;
		}
		else
		{
			fprintf(stderr,"*** Error reading file [%s] ***\n",filename);
			fprintf(fd_log,"*** Error reading file [%s] ***\n",filename);
			exit(1);
		}
	}
}

/*
**	Routine:	untabify()
**
**	Function:	To change tabs into spaces.
**
**	Description:	Is passed a string that may contain tabs, change them to correct number
**			of spaces.
**
**	Arguments:
**	str		The incoming string
**	size		The maximum size of str.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	If the expanded string is greater then size then in will be truncated to size.
**
*/
int untabify(char *str,int size)
{
	char 	*tmpbuf;
	int 	iidx,oidx;
	
	tmpbuf = (char *)malloc(size);

	for (iidx=oidx=0; iidx < size && oidx < size && str[iidx]; )
	{
		switch (str[iidx])
		{
		case '\t':
			tmpbuf[oidx++]=' ';
			while (oidx % 8)
			  	tmpbuf[oidx++]=' ';
			break;
		default:
			tmpbuf[oidx++] = str[iidx];
			break;
		}
		++iidx;
	}
	tmpbuf[(oidx<size)?oidx:size-1]=(char)0;
	strncpy(str,tmpbuf,size-1);
	str[size-1]=(char)0;
	free(tmpbuf);
	return 0;
}

struct rec_s *find_rec(struct rec_s *top, struct rec_s *curr, long num)
{
	if (!curr) curr = top;

	if (curr->num > num) curr = top;

	while (curr->num < num)
	{
		curr = curr->next;
	}

	if (curr->num == num)
	{
		return curr;
	}
	else
	{
		return NULL;
	}
}
/*
**	History:
**	$Log: mergemod.c,v $
**	Revision 1.4  1996/07/23 18:12:55  gsl
**	drcs update
**	
**
**
*/
