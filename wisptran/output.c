static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		output.c
**
**	Purpose:	COBOL output stream routines
**
**	Routines:
**	tput_statement()	Write a statement using tput_token()
**	tput_fluff()		Write a statement's fluff using tput_token()
**	tput_block()		Write a block of text one line at a time using tput_line()
**	tput_clause()		Write a partial statement using tput_statement()
**	tput_line_at()		Write a line starting at a given column using tput_line()
**	tput_line()		Write a line using tput_statement()
**	tput_scomment()		Write a Special COMMENT using tput_token()
**	tput_noprocess()	Write a NOPROCESS line using tput_token()
**	tput_blank()		Write a special BLANK line using tput_token()
**	tput_flush()		Write the contents of tput_token()'s output buffer.
**	tput_token_cache()	Write the tokens in the token cache using tput_token().
**	override_output_stream() Override the tput_token() output stream.
**	release_output_stream()	Release the tput_token() output stream from a previous override.
**	override_cobfile()	The COBOL file that is to be written to for the override.
**	tput_token()		Write a token to the output stream. (Control routine for the main output stream.)
**	x_tput_token()		Write a token to a given output stream.
**	x_tput_flush_ctx()	Flush a reset an output stream by context.
**	put_cobol_line()	Write a COBOL line to a file.
**	write_file()		Write a buffer to a file.
**
**
**	History:
**	05/27/93	Split from wt_io.c. GSL
**
*/

#include <stdio.h>
#include <string.h>

#define EXT extern
#include "wisp.h"
#include "wispfile.h"
#include "token.h"
#include "node.h"
#include "output.h"
#include "statment.h"
#include "tokenize.h"


struct tput_context_struct
{
	int		last_line;					/* Last line number				*/
	cob_file	*the_cobfile;					/* Current output file				*/
#define SIZEOF_OUT_LINE	128
	char		out_line[SIZEOF_OUT_LINE];			/* Output line buffer				*/
	int		out_column;					/* Column of last written char in out_line	*/
	int		lint;						/* Does line contain only lint ?		*/
	int		write_comments;					/* Do we write out comments and lint		*/
};
typedef struct tput_context_struct tput_context;


static cob_file *override_cobfile(void);
static int x_tput_token(tput_context *ctx, int mincol, TOKEN *tokptr);
static int x_tput_flush_ctx(tput_context *ctx);

#define	TPUT_FLUSH	-1
#define	TPUT_OVERRIDE	-2
#define	TPUT_RELEASE	-3
#define	TPUT_BLANK	-4



/*
**	Routine:	tput_statement()
**
**	Function:	Write a statement using tput_token().
**
**	Description:	Write the given statement tree by calling tput_token() for each token node in the tree.
**			This routine traverses the tree in self-down-next order using recursion.
**
**	Arguments:
**	column		The minimum starting column for "floating" tokens.
**	tree		The statement node tree.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/07/93	Written by GSL
**
*/
NODE tput_statement ( int column, NODE tree )
{
	if (!tree) return(tree);					/* Nothing to do					*/

	tput_token(column,tree->token);					/* Write this token					*/
	tput_statement(column,tree->down);				/* Write everything beneath this node			*/
	tput_statement(column,tree->next);				/* Write everything to the right			*/

	return(tree);
}

/*
**	Routine:	tput_fluff()
**
**	Function:	Write a statement's fluff using tput_token().
**
**	Description:	Write out the fluff (not lint) of a statement. 
**			This is used when a statement is going to be deleted but we don't want to
**			loose important fluff like NOPROCESS and SCOMMENT tokens.
**			This routine traverses the tree in self-down-next order using recursion.
**
**	Arguments:
**	tree		The statement node tree.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_fluff ( NODE tree )
{
	if (!tree) return(0);

	if (tree->token)
	{
		if (fluff_token(tree->token) && !lint_token(tree->token))
		{
			tput_token(12,tree->token);
		}
	}
	tput_fluff(tree->down);
	tput_fluff(tree->next);
	return(0);
}

void tput_leading_fluff(NODE the_statement)
{
	if (the_statement->down)
	{
		tput_statement(12, the_statement->down);
		free_statement(the_statement->down);
		the_statement->down = NULL;
	}
}

static void *the_tput_use_context = NULL;

int tput_set_context ( void *the_context )
{
	the_tput_use_context = the_context;
	return 0;
}

void *tput_use_context (void)
{
	return(the_tput_use_context);
}

/*
**	Routine:	tput_block()
**
**	Function:	Write a block of text one line at a time using tput_line().
**
**	Description:	This routine is given a block of text containing multiple lines
**			that are separated by newlines, it writes them out by spliting
**			the block into individual lines and calling tput_line() with
**			each one.
**
**	Arguments:
**	the_block	The block of newline separated lines.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_block ( char *the_block )
{
	char	a_line[256];
	char	*ptr, *line;
	int	size;

	line = the_block;

	for(;;)
	{
		if (ptr = strchr(line,'\n'))
		{
			size = ptr-line;
			memcpy(a_line,line,size);
			a_line[size] = (char)0;
			tput_line("%s", a_line);
			line = ptr + 1;
		}
		else
		{
			if (line[0])
			{
				tput_line("%s", line);
			}
			return(0);
		}
	}
}

/*
**	Routine:	tput_clause()
**
**	Function:	Write a partial statement using tput_statement().
**
**	Description:	This routine is passed a string that contains a partial COBOL statement.
**			The string is made into statement tree with make_statement() then
**			it is printed tput_statement().
**			Before calling tput_statement() the statement is striped of column numbers
**			to make all the token into "floating" tokens.
**
**	Arguments:
**	col		The minimum starting column for "floating" tokens.
**	clause		The partial statement string (can contain printf type % arguments)
**	p0-p11		The printf type arguments to clause.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The clause and p0-p11 are used in a call to sprintf() so enough args must
**			be supplied.  To print a "%" or a literal that contains a percent you should
**			pass that portion in p0-p11 not as part of clause.
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_clause ( int col, char *clause, char *p0, char *p1, char *p2, char *p3, char *p4, char *p5, char *p6, char *p7, char *p8, char *p9, char *p10, char *p11 )
{
	NODE	the_statement;
	char	the_line[256];

	memset(the_line,' ',12);
	sprintf(&the_line[11],clause,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,"{ERROR}");

	the_statement = make_statement(the_line,tput_use_context());
	decontext_statement(the_statement);

	tput_statement(col, the_statement);
	free_statement(the_statement);
	return(0);
}

/*
**	Routine:	offset_statement()
**
**	Function:	To offset the token column positions by a given amount.
**
**	Description:	For each non-fixed column token in the statement it
**			will add num to the column position.
**
**	Arguments:
**	num		The amount to offset by.
**	curr		The node tree.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	It is possible to offset a token to an invalid
**			column position.  This should later be caught
**			by the output routines.
**
**	History:	
**	08/09/94	Written by GSL
**
*/
NODE offset_statement(int num, NODE curr, int nofixed)
{
	if (!curr) return NULL;

	if (curr->token)
	{		
		if (!curr->token->column_fixed || nofixed)
		{
			curr->token->column += num;
		}
	}
	offset_statement(num, curr->down, nofixed);
	offset_statement(num, curr->next, nofixed);
	return curr;
}

/*
**	Routine:	tput_line_at()
**
**	Function:	Write a line starting at a given column.
**
**	Description:	This routine is passed a string that contains a COBOL line.
**			The idea here is to build a line then shift it to the
**			desired column position, we do it this way to avoid 
**			overflowing the line.
**
**	Arguments:
**	col		The minimum starting column for "floating" tokens.
**	line		The COBOL line string (can contain printf type % arguments)
**	p0-p11		The printf type arguments to line.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The clause and p0-p11 are used in a call to sprintf() so enough args must
**			be supplied.  To print a "%" or a literal that contains a percent you should
**			pass that portion in p0-p11 not as part of clause.
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_line_at ( int col, char *line, char *p0, char *p1, char *p2, char *p3, char *p4, char *p5, 
		  char *p6, char *p7, char *p8, char *p9, char *p10, char *p11 )
{
	NODE	the_statement, the_node;
	char	the_line[256];
	int	len;

	/*
	**	Build the line at a known column position
	*/
	memset(the_line,' ',col);
	sprintf(&the_line[col-1],line,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,"{ERROR}");

	len = strlen(the_line);
	if (len > 72)
	{
		/*
		**	We have a problem, the line overflowed.
		**	Try the old way of writing into col 12 then shifting the 
		**	tokens back.
		*/

		if (col > 12)
		{
			sprintf(&the_line[11],line,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,"{ERROR}");
		}
	}
	

	the_statement = make_statement(the_line,tput_use_context());

	if (len > 72 && col > 12)
	{
		/*
		**	Shift the tokens to there desired position
		*/
		offset_statement(col-12,the_statement,1);
	}

	if (the_node = first_node_with_token(the_statement))
	{
		/*
		**	Mark the first token with a fixed column
		*/
		the_node->token->column_fixed = 1;
		tput_statement(col+4, the_statement);
	}
	else if (comments)
	{
		/*
		**	Blank line
		*/
		tput_blank();
	}

	free_statement(the_statement);
	return(0);
}

/*
**	Routine:	tput_line()
**
**	Function:	Write a line using tput_statement().
**
**	Description:	This routine is passed a string that contains a COBOL line.
**			The string is made into statement tree with make_statement() then
**			it is printed tput_statement().  The first token is given a
**			fixed column position for formating.
**
**	Arguments:
**	line		The COBOL line string (can contain printf type % arguments)
**	p0-p11		The printf type arguments to line.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The clause and p0-p11 are used in a call to sprintf() so enough args must
**			be supplied.  To print a "%" or a literal that contains a percent you should
**			pass that portion in p0-p11 not as part of clause.
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_line ( char *line, char *p0, char *p1, char *p2, char *p3, char *p4, char *p5, char *p6, char *p7, char *p8, char *p9, char *p10, char *p11 )
{
	NODE	the_statement, the_node;
	char	the_line[256];

	sprintf(the_line,line,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,"{ERROR}");

	the_statement = make_statement(the_line, tput_use_context());

	if (the_node = first_node_with_token(the_statement))
	{
		the_node->token->column_fixed = 1;
		tput_statement(12, the_statement);
	}
	else if (comments)
	{
		/*
		**	Blank line
		*/
		tput_blank();
	}

	free_statement(the_statement);
	return(0);
}

/*
**	Routine:	tput_scomment()
**
**	Function:	Write a Special COMMENT using tput_token().
**
**	Description:	This routine is passed a string that contains a Special COMMENT line.
**			(A Special COMMENT is one that always gets written regardless of the "comments" flag setting.)
**			The string is made into a SCOMMENT token and tput_token() is called to write it.
**
**	Arguments:
**	line		The SCOMMENT line string (can contain printf type % arguments)
**	p0-p11		The printf type arguments to line.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The clause and p0-p11 are used in a call to sprintf() so enough args must
**			be supplied.  To print a "%" or a literal that contains a percent you should
**			pass that portion in p0-p11 not as part of clause.
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_scomment ( char *line, char *p0, char *p1, char *p2, char *p3, char *p4, char *p5, char *p6, char *p7, char *p8, char *p9, char *p10, char *p11 )
{
	char	the_line[256];
	TOKEN	*tokptr;

	memset(the_line,' ',6);
	sprintf(&the_line[6],line,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,"{ERROR}");
	tokptr = make_token(SCOMMENT,the_line);
	tokptr->column = 1;
	tokptr->column_fixed = 1;
	tput_token(1,tokptr);
	return(0);
}

/*
**	Routine:	tput_noprocess()
**
**	Function:	Write a NOPROCESS line using tput_token().
**
**	Description:	This routine is passed a string that contains a NOPROCESS line.
**			(A NOPROCESS line always gets written without intrepretation.)
**			The string is made into a NOPROCESS token and tput_token() is called to write it.
**
**	Arguments:
**	line		The NOPROCESS line string (can contain printf type % arguments)
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	Do NOT pass Newline characters in a NOPROCESS line, this was corrupt the output stream.
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_noprocess ( char *line )
{
	TOKEN	*tokptr;

	tokptr = make_token(NOPROCESS,line);
	tokptr->column = 1;
	tokptr->column_fixed = 1;
	tput_token(1,tokptr);
	return(0);
}

/*
**	Routine:	tput_blank()
**
**	Function:	Write a special BLANK line using tput_token().
**
**	Description:	This routine forces a blank line to be written regardless of the "comments" flag.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_blank (void)
{
	tput_token(TPUT_BLANK,NULL);
	return 0;
}

/*
**	Routine:	tput_flush()
**
**	Function:	Write the contents of tput_token()'s output buffer.
**
**	Description:	This routine forces tput_token() to write it's output buffer and start a new line.
**			If there is nothing in the buffer then no action will be taken.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_flush (void)
{
	tput_token(TPUT_FLUSH,NULL);
	return 0;
}

/*
**	Routine:	tput_token_cache()
**
**	Function:	Write the tokens in the token cache using tput_token().
**
**	Description:	Unque all the tokens in the token cache and write each one with tput_token().
**			This is a "line" layer output as the token cache contains only the the unsed
**			tokens from the current line.  It is only used from the old "line" I/O routines
**			as a replacement for "put_line(inline)".
**
**	Arguments:	None
**
**	Globals:	The token_cache.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int tput_token_cache (void)
{
	int	tokcnt;
	TOKEN	tmptok;

	tokcnt = token_cache_count();

	while(tokcnt--)
	{
		token_cache_unque(&tmptok);
		tput_token(12,&tmptok);
		clean_token(&tmptok);
	}
	return(0);
}

/*
**	Routine:	override_output_stream()
**
**	Function:	Override the tput_token() output stream.
**
**	Description:	Override the output stream to redirect all tput output to an override file.
**
**	Arguments:
**	cob_file_ptr	The override file.
**
**	Globals:
**	the_override_cobfile	The override file. Used to communicate with tput_token().
**
**	Return:		None
**
**	Warnings:	Must be followed by a release_output_stream().
**
**	History:	
**	06/07/93	Written by GSL
**
*/
static cob_file *the_override_cobfile = NULL;

int override_output_stream ( cob_file *cob_file_ptr )
{
	if (the_override_cobfile)
	{
		write_log("WISP",'F',"OVERRIDE","Output stream can not be re-overriden, possible recursive logic error.");
		return 0;
	}
	
	the_override_cobfile = cob_file_ptr;
	tput_token(TPUT_OVERRIDE,NULL);
	return(0);
}

/*
**	Routine:	release_output_stream()
**
**	Function:	Release the tput_token() output stream from a previous override.
**
**	Description:	Release the output stream following an override.
**
**	Arguments:	None
**
**	Globals:
**	the_override_cobfile	The override file. Used to communicate with tput_token().
**
**	Return:		None
**
**	Warnings:	Must be proceeded by an override_output_stream().
**
**	History:	
**	06/07/93	Written by GSL
**
*/
int release_output_stream (void)
{
	tput_token(TPUT_RELEASE,NULL);
	the_override_cobfile = NULL;
	return(0);
}

/*
**	Routine:	override_cobfile()
**
**	Function:	The COBOL file that is to be written to for the override.
**
**	Description:	Return the override file.
**
**	Arguments:	None
**
**	Globals:
**	the_override_cobfile	The override file. Used to communicate with tput_token().
**
**	Return:		None
**
**	Warnings:	Must be proceeded by an override_output_stream().
**
**	History:	
**	06/07/93	Written by GSL
**
*/
static cob_file *override_cobfile (void)
{
	return(the_override_cobfile);
}


/*
**	Routine:	tput_token()
**
**	Function:	Write a token to the output stream. (Control routine for the main output stream.)
**
**	Description:	This is the token layer for the main COBOL output stream. 
**			All "tput" output goes thru this routine, hence this is the control point for the
**			main COBOL output stream.
**
**			This layer handles:
**				-	Setting up the output stream context structures for use by x_tput_token().
**
**				- 	Switching the output stream context between main and copybook files
**					when copybooks are being generated.
**
**				- 	Temporary overriding of the output stream into a different output file.
**
**				-	Splitting the output stream. (This is done when relocating paragrapghs.)
**
**			The output file to use is determined from the context pointer of the token.  If a token
**			doesn't have a context or the outfile is undefined the last file will be used.  If a copybook
**			file is being written to and the context changes then it will be closed. (Unless the output
**			stream is being overridden.)
**
**			After this routine has handled the switching on contexts it calls x_tput_token() to write
**			the token to an output stream based on the context.
**
**			The output stream can be overridden by calling override_output_stream().  When this is called
**			an override context is set up with the override file and this will be used until it is released
**			by a call to release_output_stream().
**
**	Arguments:
**	mincol		The mininum column to write this token out to (usually 12).
**	tokptr		Point to the token to write out.
**
**	Globals:
**	comments	Flag if comments are to be written out.
**	copylib		Flag if copylibs are being generated.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/27/93	Written by GSL
**
*/

int tput_token ( int mincol, TOKEN *tokptr )
{
	static	int		first = 1;
	static	tput_context	main_ctx;					/* The main context structure			*/
	static	tput_context	override_ctx;					/* The override context				*/

	static	int		use_main;					/* Flag, which context to use.			*/

	if (first)
	{
		if (!main_cob_context)
		{
			/*
			**	Trying to output a token before the COBOL context has been established.
			**	Probably doing a flush on an exit.
			*/
			return(0);
		}

		first = 0;

		/*
		**	Set up the main context structure.
		*/
		use_main = 1;
		memset(main_ctx.out_line,' ',SIZEOF_OUT_LINE);			/* Initialize the output buffer			*/
		main_ctx.out_line[SIZEOF_OUT_LINE-2] = '\n';
		main_ctx.out_line[SIZEOF_OUT_LINE-1] = (char)0;
		main_ctx.out_column = 0;
		main_ctx.last_line = 0;
		main_ctx.the_cobfile = main_cob_context->outfile;
		main_ctx.lint = 1;
		main_ctx.write_comments = comments;
	}

	if (!tokptr)
	{
		if (TPUT_OVERRIDE == mincol)
		{
			/*
			**	Code to override output stream:
			**
			**	Setup the override context structure and set flag to use 
			**	the override context.
			*/

			memset(override_ctx.out_line,' ',SIZEOF_OUT_LINE);
			override_ctx.out_line[SIZEOF_OUT_LINE-2] = '\n';
			override_ctx.out_line[SIZEOF_OUT_LINE-1] = (char)0;
			override_ctx.out_column = 0;
			override_ctx.last_line = 0;
			override_ctx.the_cobfile = override_cobfile();
			override_ctx.lint = 1;
			override_ctx.write_comments = comments;
			use_main = 0;

			return(0);
		}
		else if (TPUT_RELEASE == mincol)
		{
			/*
			**	Code to release output stream:
			**
			**	Flush the override context and flag to use the main context.
			*/

			x_tput_token(&override_ctx, TPUT_FLUSH, NULL);
			use_main = 1;

			return(0);
		}
	}

	if (tokptr && use_main)
	{
		cob_file_context *context;

		context = (cob_file_context *)tokptr->context;			/* Token doesn't known struct of context	*/

		if (copylib && context && context->outfile && context->outfile != main_ctx.the_cobfile)
		{
			/*
			**	If copybooks are being generated and the output file changes then flush 
			**	the buffer (close the previous file if was a copybook) then switch
			**	the output stream.
			*/

			x_tput_token(&main_ctx, TPUT_FLUSH, NULL);

			if (main_ctx.the_cobfile->is_copybook)
			{
				close_cob_file(main_ctx.the_cobfile);
			}

			main_ctx.the_cobfile = context->outfile;
			main_ctx.last_line = tokptr->line;
		}
	}

	if (use_main)
	{
		x_tput_token(&main_ctx, mincol, tokptr);

		if (copy_to_dcl_file)
		{
			split_token_to_dcl_file(mincol, tokptr);
		}

		if (copy_to_dtp_file)
		{
			split_token_to_dtp_file(mincol, tokptr);
		}
	}
	else
	{
		x_tput_token(&override_ctx, mincol, tokptr);
	}

	return(0);
}

/*
**	Routine:	x_tput_token()
**
**	Function:	Write a token to a given output stream.
**
**	Description:	This is the token output layer for COBOL output.
**
**			It takes one token at a time and loads it into a line buffer. When the line buffer is full
**			it writes it out and starts a new buffer. The buffer is flushed when the output line changes.
**
**			Fixed column tokens are always written to the requested column.  Non fixed "floating" tokens 
**			will be written to there requested column if possible otherwise they will be written to the 
**			next available column.  A token with a column==0 is a floating token and will use mincol.
**
**			If the token has both data and indata it will use indata unless data differs in more then just case.
**
**			To force a flush of the output buffer call with a mincol==TPUT_FLUSH and tokptr==NULL.
**
**	Arguments:
**	ctx		The output stream context structure pointer.
**	mincol		The mininum column to write this token out to (usually 12).
**	tokptr		Point to the token to write out.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/27/93	Written by GSL
**
*/
static int x_tput_token ( tput_context *ctx, int mincol, TOKEN *tokptr )
{
	int	column,len;
	char	*data;

	if (!tokptr)
	{
		if (TPUT_FLUSH == mincol)
		{
			/*
			**	Code to flush buffers
			*/
			x_tput_flush_ctx(ctx);
			return(0);
		}
		else if (TPUT_BLANK == mincol)
		{
			/*
			**	Code to force a blank line.
			*/
			x_tput_flush_ctx(ctx);
			put_cobol_line(ctx->the_cobfile,"\n");
			return(0);
		}
	}

	if (!tokptr || !tokptr->data) return(0);				/* Empty token.					*/

	if (!ctx->write_comments && COMMENT==tokptr->type) return(0);		/* If not writting comments return.		*/

	if (tokptr->line && tokptr->line != ctx->last_line)			/* Changed lines, write out buffer		*/
	{
		/*
		**	If anything is in the buffer write it out.
		*/

		x_tput_flush_ctx(ctx);

		ctx->last_line = tokptr->line;
	}

	column = tokptr->column;
	if (0==column)								/* If no column supplied then use mincol or 12	*/
	{
		column = mincol;
		if (0==column)
		{
			column = 12;
		}
	}

	len = strlen(tokptr->data);
	if (0==len) 
	{
	        if (COMMENT==tokptr->type)
		{
			/*
			**	blank line.
			*/
			x_tput_flush_ctx(ctx);
			put_cobol_line(ctx->the_cobfile,"\n");
	        }
	  
		return(0);
	}

	if (!tokptr->indata)
	{
		data = tokptr->data;
	}
	else if ((int)strlen(tokptr->indata) != len)
	{
		data = tokptr->data;
	}
	else
	{
		/* If not changed (except case) then use indata */
		if (noncase_eq(tokptr->data,tokptr->indata))
		{
			data = tokptr->indata;
		}
		else
		{
			data = tokptr->data;
		}
	}

	/*
	**	Now add the token to the buffer, if needed write out the buffer
	**	and start a new one.
	*/

	if (tokptr->column_fixed)
	{
		if ( ctx->out_column >= column ||
		     (column >= 12 && column <= 72 && ctx->out_line[column-2] != ' ') )
		{
			/*
			**	Write out the buffer
			**	  - when past the fixed column on the current line or
			**	  - when in Area-B and not a space separator
			*/
			x_tput_flush_ctx(ctx);
		}

		memcpy(&ctx->out_line[column-1], data, len);
		ctx->out_column = column+len-1;
	}
	else if (ctx->out_column < column-1 && (73 - column >= len ))
	{
		/*
		**	if requested column is available and there is at least one space between
		**	the last token then insert the new token, no further checking needed.
		*/
		memcpy(&ctx->out_line[column-1], data, len);
		ctx->out_column = column+len-1;
	}
	else
	{
		char	last_char, next_char;

		if (ctx->out_column < mincol-1)
		{
			/*
			**	pay attention to the mincol value.
			*/
			ctx->out_column = mincol-1;
		}

		/*
		**	Check if a space is required between the last token and this one.
		**	We are going to use out_column as the column number (repositioning token).
		*/

		last_char = ctx->out_line[ctx->out_column-1];
		next_char = data[0];

		if (' '==last_char)
		{
			/*
			**	Last char is a space so don't need to check further.
			**	This can happen if mincol causes the out_column to be bumped up.
			*/
		}
		else if (VERB==tokptr->type)
		{
			ctx->out_column++;
		}
		else if ( ('.'==last_char) || (','==last_char) || (';'==last_char))	/* If last char was punctuation		*/
		{
			ctx->out_column++;						/* Add a space				*/
		}
	/* 	else if ( ('.'==next_char) || (','==next_char) || (';'==next_char)) */	/* If next char is punctuation		*/
		else if (PERIOD==tokptr->type || PUNCT==tokptr->type)
		{
			/* If next char is punctuation don't add a space */
		}
		else if ( ('('==last_char) || (')'==next_char) || ('('==next_char))
		{
			/* Don't add a space */
		}
		else if ( ('"'==last_char) || (')'==last_char) || ('"'==next_char) )
		{
			ctx->out_column++;
		}
		else if ( (':'==last_char) || (':'==next_char) )		/* Reference modification			*/
		{
			/* Don't add a space */
		}
		else if ( OPERATOR==tokptr->type )
		{
			/* Always add a space before an operator */
			ctx->out_column++;
		}
		else if (  ('='==last_char) 
			|| ('*'==last_char) 
			|| ('+'==last_char)
			|| ('-'==last_char)
			|| ('/'==last_char) )
		{
			/* Always add a space after an operator */
			ctx->out_column++;
		}
		else if (isalnum(last_char))					/* If last char was alphanumeric		*/
		{
			ctx->out_column++;					/* Add a space					*/
		}
		else if (NUMBER==tokptr->type || isalnum(next_char))		/* If this char is alphanumeric			*/
		{
			ctx->out_column++;
		}

		if (72 - ctx->out_column >= len )
		{
			/*
			**	There is enough room on current line so added at position out_column.
			*/
			memcpy(&ctx->out_line[ctx->out_column], data, len);
			ctx->out_column += len;
		}
		else
		{
			/*
			**	If not enough room on current line then write it and start a new line.
			**	Since we started a new line use column not out_column (don't reposition token).
			*/

			x_tput_flush_ctx(ctx);

			if (72 - column < len)
			{
				/* 
				**	Token doesn't fit on line at requested column
				*/

				if (72 - mincol >= len)
				{
					/*
					**	It will fit at mincol
					*/
					column = mincol;
				}
				else
				{
					/*
					**	Last chance - use column 12
					*/
					column = 12;
				}
			}

			memcpy(&ctx->out_line[column-1], data, len);
			ctx->out_column = column+len-1;

			if (ctx->out_column >= 73)				/* Error Token overflowed into column 73	*/
			{
				write_log("WISP",'E',"LINE2LONG",
					"Line too long, token overflowed into column 73.\n%s\n",ctx->out_line);
			}
		}
	}

	if (!lint_token(tokptr))
	{
		/*
		**	Keep track if any non-lint tokens are added to the buffer, if only lint was put into the
		**	buffer then it may not have to be written out.
		*/
		ctx->lint = 0;
	}

	if (COMMENT==tokptr->type || SCOMMENT==tokptr->type || IDCOMMENT==tokptr->type || NOPROCESS==tokptr->type )
	{
		/*
		**	If a comment or noprocess token was put in the buffer then write it out as we
		**	don't want to append the next token to the end of this line.
		*/
		x_tput_flush_ctx(ctx);
	}

	return(0);
}

/*
**	Routine:	x_tput_flush_ctx()
**
**	Function:	To write out the output buffer then reset it.
**
**	Description:	If there is anything in the buffer then write it out and reset the buffer and flags.
**			If the buffer contains only lint and the write_lint flag is not set then
**			don't write it out.
**
**	Arguments:
**	ctx		The tput_token context structure
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/29/93	Written by GSL
**	06/05/93	Changed to use the tput_context (ctx). GSL
**
*/
static int x_tput_flush_ctx ( tput_context *ctx )
{
	if (ctx->out_column && (!ctx->lint || ctx->write_comments))
	{
		/*
		**	If something to write out and it is not lint or lint (write_comments) are allowed
		**	then write it out. 
		**
		**	NOTE:	Actual COMMENT tokens were filtered out earlier, this will filter out
		**		blank lines or lines that contain only LINECODE, MODCODE etc.
		*/
		ctx->out_line[ctx->out_column]   = '\n';
		ctx->out_line[ctx->out_column+1] = (char)0;
		put_cobol_line(ctx->the_cobfile,ctx->out_line);			/* Write out the buffer.			*/
	}

	memset(ctx->out_line,' ',sizeof(ctx->out_line));			/* Reset the output buffer.			*/
	ctx->out_line[sizeof(ctx->out_line)-2] = '\n';
	ctx->out_line[sizeof(ctx->out_line)-1] = (char)0;
	ctx->out_column = 0;
	ctx->lint = 1;
	return(0);
}

/*
**	Routine:	put_cobol_line()
**
**	Function:	To write out a cobol line to the given file.
**
**	Description:	This is the lowest "COBOL line" layer of the output stream.
**			This layer counts COBOL output statistics.
**
**	Arguments:
**	the_file	The (open) cobol output file.
**	the_line	The COBOL line to write (must include a terminating newline before the NULL).
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/27/93	Written by GSL
**
*/
int put_cobol_line ( cob_file *cob_file_ptr, char *the_line )
{
	if (!cob_file_ptr->is_open)
	{
		write_log("WISP",'F',"COBCLOSED","Attempted to write to a closed file %s",cob_file_ptr->a_file->name);
		exit_with_err();
	}

	increment_out_line_count();						/* count the output line numbers		*/
	cob_file_ptr->line_count++;

	write_file(cob_file_ptr->a_file,the_line);
	return 0;
}

/*
**	Routine:	write_file()
**
**	Function:	To write a buffer to a file.
**
**	Description:	This is the lowest layer for COBOL file output stream.
**
**	Arguments:
**	the_file	The (open) file to write to.
**	buff		The null terminated string to write.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/27/93	Written by GSL
**
*/
int write_file ( A_file *the_file, char *buff )
{
	if (fprintf(the_file->file,"%s",buff) < 0)
	{
		write_log("WISP",'F',"OUTPUT","Error writing output file %s.",the_file->name);
		exit_with_err();
	}
	return 0;
}

void split_token_to_dcl_file ( int mincol, TOKEN *tokptr )
{
	static	int		first = 1;
	static	tput_context	split_ctx;
	static	int	found_go	= 0;
	static	int	found_perform	= 0;
	static	int	found_para	= 0;
	static	int	found_section	= 0;

	char	buff[256];

	if (first)
	{
		first = 0;

		dcl_file_ptr = open_cob_file(dcl_fname,FOR_OUTPUT,1);		/* Now open the paragraph file for out		*/
		if (!dcl_file_ptr)
		{
			exit_with_err();
		}

		/*
		**	Set up the split context structure.
		*/
		memset(split_ctx.out_line,' ',SIZEOF_OUT_LINE);			/* Initialize the output buffer			*/
		split_ctx.out_line[SIZEOF_OUT_LINE-2] = '\n';
		split_ctx.out_line[SIZEOF_OUT_LINE-1] = (char)0;
		split_ctx.out_column = 0;
		split_ctx.last_line = 0;
		split_ctx.the_cobfile = dcl_file_ptr;
		split_ctx.lint = 1;
		split_ctx.write_comments = comments;
	}

	if (!tokptr || mincol < 0) 
	{
		x_tput_token(&split_ctx, mincol, tokptr);
		return;
	}

	if (!tokptr || !tokptr->data) return;					/* Empty token.					*/

	if (fluff_token(tokptr))
	{
		goto write_the_token;	/* Just pass thru */
	}

	if (PERIOD == tokptr->type)
	{
		found_go = 0;
		found_perform = 0;
		found_para = 0;
	}

	if (found_perform)
	{
		if (1==found_perform)
		{
			if (IDENTIFIER==tokptr->type)
			{
				if (paracmp(tokptr->data,proc_paras,proc_paras_cnt))	/* found it.				*/
				{							/* now process it.			*/
					make_fld(buff,tokptr->data,"D-");
					edit_token(tokptr,buff);			/* replace it in the line.		*/
				}
				found_perform = 2;
			}
			else
			{
				/* An in-line perform, no name to edit */
				found_perform = 0;
			}
		}
		else if (2==found_perform)
		{
			if (eq_token(tokptr,KEYWORD,"THROUGH") || eq_token(tokptr,KEYWORD,"THRU"))
			{
				found_perform = 3;
			}
			else
			{
				found_perform = 0;
			}
		}
		else if (3==found_perform)
		{
			if (IDENTIFIER==tokptr->type)
			{
				if (paracmp(tokptr->data,proc_paras,proc_paras_cnt))	/* found it.				*/
				{							/* now process it.			*/
					make_fld(buff,tokptr->data,"D-");
					edit_token(tokptr,buff);			/* replace it in the line.		*/
				}
			}
			found_perform = 0;
		}
	}

	if (found_go)
	{
		if (IDENTIFIER==tokptr->type)
		{
			if (paracmp(tokptr->data,proc_paras,proc_paras_cnt))	/* found it.				*/
			{							/* now process it.			*/
				make_fld(buff,tokptr->data,"D-");
				edit_token(tokptr,buff);			/* replace it in the line.		*/
			}
			found_go = 0;
		}
	}

	if (found_para)
	{
		found_para = 0;
		if (eq_token(tokptr,KEYWORD,"SECTION"))
		{
			found_section = 1;
			return;
		}
	}

	if (eq_token(tokptr,VERB,"GO"))
	{
		found_go = 1;
		found_perform = 0;
	}
	else if (eq_token(tokptr,VERB,"PERFORM"))
	{
		found_perform = 1;
		found_go = 0;
	}
	else if (NOPROCESS==tokptr->type && strpos(tokptr->data," COPY ") != -1)
	{
		tokptr->data[6] = '*';
	}

	if (IDENTIFIER==tokptr->type)
	{
		if (eq_token(tokptr,0,"WISP-EXIT-PROGRAM") || eq_token(tokptr,0,"WISP-STOP-RUN"))
		{
			make_fld(buff,tokptr->data,"D-");
			edit_token(tokptr,buff);
		}
		else if (0==memcmp(tokptr->data,"WDR-",4) || 0==memcmp(tokptr->data,"WGS-",4))
		{
			strcpy(buff,"D");
			strcat(buff,tokptr->data);
			edit_token(tokptr,buff);
		}
		else if (tokptr->column && tokptr->column < 12 && tokptr->column >= 8)
		{
			/* Area A IDENTIFIER - Paragraph name. */
			make_fld(buff,tokptr->data,"D-");
			edit_token(tokptr,buff);
			found_para = 1;
		}
	}

	
write_the_token:
	x_tput_token(&split_ctx, mincol, tokptr);



	if (found_section && PERIOD == tokptr->type)
	{
		TOKEN	*tok;
		extern	int 	sect_num;;						/* Current SECTION number.		*/

		found_section = 0;

		sprintf(buff,"           PERFORM WISP-SECTION-%d-BEGIN THRU",sect_num);
		tok = make_token(NOPROCESS,buff);
		tok->column = 1;
		tok->column_fixed = 1;
		x_tput_token(&split_ctx, mincol, tok);
		free_token(tok);

		sprintf(buff,"                   WISP-SECTION-%d-END.",sect_num);
		tok = make_token(NOPROCESS,buff);
		tok->column = 1;
		tok->column_fixed = 1;
		x_tput_token(&split_ctx, mincol, tok);
		free_token(tok);

		sprintf(buff,"       WISP-SECTION-%d-BEGIN.",sect_num);
		tok = make_token(NOPROCESS,buff);
		tok->column = 1;
		tok->column_fixed = 1;
		x_tput_token(&split_ctx, mincol, tok);
		free_token(tok);
	}
}

void split_token_to_dtp_file ( int mincol, TOKEN *tokptr )
{
	static	int		first = 1;
	static	tput_context	split_ctx;
	static	int	found_go	= 0;
	static	int	found_perform	= 0;

	char	buff[256];

	if (first)
	{
		first = 0;

		dtp_file_ptr = open_cob_file(dtp_fname,FOR_OUTPUT,1);		/* Now open the paragraph file for out		*/
		if (!dtp_file_ptr)
		{
			exit_with_err();
		}

		/*
		**	Set up the split context structure.
		*/
		memset(split_ctx.out_line,' ',SIZEOF_OUT_LINE);			/* Initialize the output buffer			*/
		split_ctx.out_line[SIZEOF_OUT_LINE-2] = '\n';
		split_ctx.out_line[SIZEOF_OUT_LINE-1] = (char)0;
		split_ctx.out_column = 0;
		split_ctx.last_line = 0;
		split_ctx.the_cobfile = dtp_file_ptr;
		split_ctx.lint = 1;
		split_ctx.write_comments = comments;
	}

	if (!tokptr || mincol < 0) 
	{
		x_tput_token(&split_ctx, mincol, tokptr);
		return;
	}

	if (!tokptr || !tokptr->data) return;					/* Empty token.					*/

	if (fluff_token(tokptr))
	{
		goto write_the_token; /* Just pass thru */
	}
	if (PERIOD == tokptr->type)
	{
		found_go = 0;
		found_perform = 0;
	}

	if (found_perform)
	{
		if (1==found_perform)
		{
			if (IDENTIFIER==tokptr->type)
			{
				if (paracmp(tokptr->data,proc_paras,proc_paras_cnt))	/* found it.				*/
				{							/* now process it.			*/
					make_fld(buff,tokptr->data,"D-");
					edit_token(tokptr,buff);			/* replace it in the line.		*/
				}
				found_perform = 2;
			}
			else
			{
				/* An in-line perform, no name to edit */
				found_perform = 0;
			}
		}
		else if (2==found_perform)
		{
			if (eq_token(tokptr,KEYWORD,"THROUGH") || eq_token(tokptr,KEYWORD,"THRU"))
			{
				found_perform = 3;
			}
			else
			{
				found_perform = 0;
			}
		}
		else if (3==found_perform)
		{
			if (IDENTIFIER==tokptr->type)
			{
				if (paracmp(tokptr->data,proc_paras,proc_paras_cnt))	/* found it.				*/
				{							/* now process it.			*/
					make_fld(buff,tokptr->data,"D-");
					edit_token(tokptr,buff);			/* replace it in the line.		*/
				}
			}
			found_perform = 0;
		}
	}

	if (found_go)
	{
		if (IDENTIFIER==tokptr->type)
		{
			if (paracmp(tokptr->data,proc_paras,proc_paras_cnt))	/* found it.				*/
			{							/* now process it.			*/
				make_fld(buff,tokptr->data,"D-");
				edit_token(tokptr,buff);			/* replace it in the line.		*/
			}
			found_go = 0;
		}
	}

	if (eq_token(tokptr,VERB,"GO"))
	{
		found_go = 1;
		found_perform = 0;
	}
	else if (eq_token(tokptr,VERB,"PERFORM"))
	{
		found_perform = 1;
		found_go = 0;
	}

	if (IDENTIFIER==tokptr->type)
	{
		if (eq_token(tokptr,0,"D-WISP-EXIT-PROGRAM") || eq_token(tokptr,0,"D-WISP-STOP-RUN"))
		{
			strcpy(buff,&tokptr->data[2]);
			edit_token(tokptr,buff);
		}
		else if (0==memcmp(tokptr->data,"DWDR-",5) || 0==memcmp(tokptr->data,"DWGS-",5))
		{
			strcpy(buff,&tokptr->data[1]);
			edit_token(tokptr,buff);
		}
		else if (tokptr->column && tokptr->column < 12 && tokptr->column >= 8)
		{
			/* Area A IDENTIFIER - Paragraph name. */
			make_fld(buff,tokptr->data,"D-");
			edit_token(tokptr,buff);
		}
	}

write_the_token:
	x_tput_token(&split_ctx, mincol, tokptr);

}

/*
**	History:
**	$Log: output.c,v $
**	Revision 1.13  2001-10-18 11:13:52-04  gsl
**	Change to treat a COMMENT token of zero lenght as a blank line
**
**	Revision 1.12  1999-09-07 10:36:27-04  gsl
**	Fix prototypes and return types.
**
**	Revision 1.11  1998-11-19 14:34:40-05  gsl
**	Fixed problem in tput_line_at() when the line contains fixed position token
**	and the column is greater then 12. (Spliting a pfkeys list for DISPLAY
**	AND READ).
**
**	Revision 1.10  1998-03-03 16:05:29-05  gsl
**	Add tput_leading_fluff() and
**	Fix tput_line() and tput_line_at() to not do the flush
**
**	Revision 1.9  1998-02-27 10:47:03-05  gsl
**	Add error detection logic to override_output_stream()
**
**	Revision 1.8  1996-10-09 12:30:27-04  gsl
**	fix warning
**
**	Revision 1.7  1996-08-30 18:56:07-07  gsl
**	drcs update
**
**
**
*/
