			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* VLIST.C ... This routine will generate a structure for a list and then display and mark items for a function.		*/
/*             This is a list manager.  You can scroll through the items and "mark" them.  A front end program will do the	*/
/* 	       procesing on the	marked items.											*/

/*						Include standard header files.							*/

#ifndef unix	/* VMS and MSDOS */
#include <stdlib.h>
#endif
#ifndef VMS	/* unix and MSDOS */
#include <malloc.h>
#endif

#include <stdio.h>									/* Get standard I/O definitions.	*/
#include <ctype.h>									/* Get character type macros.		*/
#include "video.h"									/* Include video definitions.		*/
#include "vlist.h"									/* Include struct for each item.	*/
#include "vlocal.h"
#include "vdata.h"

/*						Data Definitions.								*/
static char *copyright = " (c) 1988,89 International Digital Scientific Incorporated.";	/* Here so it shows up in object code.	*/

/*                                              Global Variables								*/
static int num_scr = MAX_LINES_PER_SCREEN;						/* Number of avail. lines in scroll reg.*/
static int st_scr = 0;
static int st_foot_scr = MAX_LINES_PER_SCREEN;
static struct active_list *headal = NULL;						/* Pointer to head of active lists.	*/
static struct active_list *currlist = NULL;						/* Pointer to traverse active lists.	*/

int vlist(function,list_id,item1,item2,item3,item4,item5,item6,item7,item8,item9,item10,item11,item12,item13,item14,item15,item16)
unsigned char *function,*list_id,
	*item1,*item2,*item3,*item4,*item5,*item6,*item7,*item8,*item9,*item10,*item11,*item12,*item13,*item14,*item15,*item16;
{
	struct tmpst *tmp;								/* Ptr to struct containing temp stuff.	*/
	long *fptr, vlist_func;

#if 0
	vkbmap(INITIALIZE);								/* Initialize keyboard mapping.		*/
#endif
	vcapload();

	fptr = (long *)function;
	vlist_func = *fptr;								/* Assign the vlist function to perform.*/

	switch (vlist_func)								/* What to do?				*/
	{
		case DISPLAY_LIST:							/* Display a list.			*/
		{
			long *lptr, list_num, *rst_row, *rst_col;			/* Pointers to long.			*/
			long *row_rtrn, *col_rtrn, *marked_rtrn, *key_rtrn, *rtrn_code; /* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			rst_row = (long *)item1;					/* Assign ptr to starting display row.	*/
			rst_col = (long *)item2;					/* Assign ptr to starting display col.	*/
			row_rtrn = (long *)item3;					/* Assign pointer to returned row pos.	*/
			col_rtrn = (long *)item4;					/* Assign pointer to returned col pos.	*/
			marked_rtrn = (long *)item5;					/* Assign ptr to array of marked items.	*/
			key_rtrn = (long *)item6;					/* Assign ptr to function key returned.	*/
			rtrn_code = (long *)item7;					/* Assign ptr to return code.		*/

			display_scan(list_num,rst_row,rst_col,row_rtrn,col_rtrn,marked_rtrn,key_rtrn,rtrn_code,1);
			break;
		}
		case NO_DISPLAY_LIST:							/* Do not re-display a list, allow	*/
		{									/* further processing.			*/
			long *lptr, list_num, *rst_row, *rst_col;			/* Pointers to long.			*/
			long *row_rtrn, *col_rtrn, *marked_rtrn, *key_rtrn, *rtrn_code; /* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			rst_row = (long *)item1;					/* Assign ptr to starting display row.	*/
			rst_col = (long *)item2;					/* Assign ptr to starting display col.	*/
			row_rtrn = (long *)item3;					/* Assign pointer to returned row pos.	*/
			col_rtrn = (long *)item4;					/* Assign pointer to returned col pos.	*/
			marked_rtrn = (long *)item5;					/* Assign ptr to array of marked items.	*/
			key_rtrn = (long *)item6;					/* Assign ptr to function key returned.	*/
			rtrn_code = (long *)item7;					/* Assign ptr to return code.		*/

			display_scan(list_num,rst_row,rst_col,row_rtrn,col_rtrn,marked_rtrn,key_rtrn,rtrn_code,0);
			break;
		}
		case DISP_ROW_ITEM:							/* Display an item in a given row and	*/
		{									/*  list.				*/
			long *lptr, list_num;
			long row, num_col;
			unsigned char *data_ptr;
			long *rtrn_code; 						/* Pointer to returned value.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			lptr = (long *)item1;
			row = *lptr;							/* Assign the row number.		*/
			lptr = (long *)item2;
			num_col = *lptr;						/* Assign the column number.		*/
			data_ptr = item3;						/* Assign pointer to data to display.	*/
			rtrn_code = (long *)item4;					/* Assign ptr to return code.		*/

			disp_one_item(list_num,row,num_col,data_ptr,rtrn_code);		/* Go display the item.			*/
			break;
		}
		case INIT_LIST:								/* Initialize a list.			*/
		{									/* Clear internal structure.		*/
			long *lptr, list_num, num_rows;					/* Pointers to long.			*/
			long *rtrn_code; 						/* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			lptr = (long *)item1;
			num_rows = *lptr;						/* Assign the number of row in list.	*/
			rtrn_code = (long *)item2;					/* Assign ptr to return code.		*/

			init_list(list_num,num_rows,rtrn_code);
			break;
		}
		case ADD_HEADER: case ADD_FOOTER:					/* Add header/footer text.		*/
		{
			long *lptr, list_num, num_items;				/* Pointers to long.			*/
			long *rtrn_code; 						/* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			lptr = (long *)item1;
			num_items = *lptr;						/* Assign the # lines in header/footer	*/
			rtrn_code = (long *)item2;					/* Assign ptr to return code.		*/

			if ((num_items < 1) && (num_items > 4))
			{
				vre("vlist()-Invalid number of items - %d.",num_items);	/* Report the situation.		*/
				return(FAILURE);					/* Return to the caller.		*/
			}
			if (!(tmp = (struct tmpst *)malloc(8+sizeof(struct tmpst)))) memory_err(); /* Get some memory.		*/
			if (num_items > 3)
			{
				tmp->it[3].dtext = item12;				/* Assign the passed data to text four.	*/
				tmp->it[3].lentext = *(long *)item13;			/* Assign the length of the text.	*/
				tmp->it[3].txtrend = *(long *)item14;			/* Assign the rendition of the text.	*/
			}
			else
			{
				tmp->it[3].dtext = NULL;				/* else init the array to no data.	*/
				tmp->it[3].lentext = 0;
				tmp->it[3].txtrend = 0;
			}
			if (num_items > 2)
			{
				tmp->it[2].dtext = item9;				/* Assign the passed data to text three.*/
				tmp->it[2].lentext = *(long *)item10;			/* Assign the length of the text.	*/
				tmp->it[2].txtrend = *(long *)item11;			/* Assign the rendition of the text.	*/
			}
			else
			{
				tmp->it[2].dtext = NULL;				/* else init the array to no data.	*/
				tmp->it[2].lentext = 0;
				tmp->it[2].txtrend = 0;
			}
			if (num_items > 1)
			{
				tmp->it[1].dtext = item6;				/* Assign the passed data to text two.	*/
				tmp->it[1].lentext = *(long *)item7;			/* Assign the length of the text.	*/
				tmp->it[1].txtrend = *(long *)item8;			/* Assign the rendition of the text.	*/
			}
			else
			{
				tmp->it[1].dtext = NULL;				/* else init the array to no data.	*/
				tmp->it[1].lentext = 0;
				tmp->it[1].txtrend = 0;
			}
			tmp->it[0].dtext = item3;					/* Assign the passed data to text one.	*/
			tmp->it[0].lentext = *(long *)item4;				/* Assign the length of the text.	*/
			tmp->it[0].txtrend = *(long *)item5;				/* Assign the rendition of the text.	*/

			add_head_foot(vlist_func,list_num,num_items,tmp,rtrn_code);
			free(tmp);							/* Free the temporary memory.		*/
			break;
		}
		case ADD_COLUMN: case INSERT_COLUMN: case REPLACE_COLUMN:		/* Add/replace a column in structure.	*/
		{									/* If insert then use 0 for column num.	*/
			long *lptr, list_num, type_col, disp_col_width, actual_col_width; /* Pointers to long.			*/
			long num_col;
			unsigned char *data_ptr;
			long *rtrn_code; 						/* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			lptr = (long *)item1;
			type_col = *lptr;						/* Assign the type of data in column.	*/
			lptr = (long *)item2;
			disp_col_width = *lptr;						/* Assign the display width of column.	*/
			data_ptr = item3;						/* Assign pointer to array of data.	*/
			lptr = (long *)item4;
			actual_col_width = *lptr;					/* Assign the actual width of column.	*/
			lptr = (long *)item5;
			num_col = *lptr;						/* Assign the column number.		*/
			rtrn_code = (long *)item6;					/* Assign ptr to return code.		*/

			add_col(vlist_func,list_num,type_col,disp_col_width,data_ptr,actual_col_width,num_col,rtrn_code);
			break;
		}
		case DELETE_COLUMN:							/* Delete column from structure.	*/
		{
			long *lptr, list_num, num_col;					/* Pointers to long.			*/
			long *rtrn_code; 						/* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			lptr = (long *)item1;
			num_col = *lptr;						/* Assign the column number to delete.	*/
			rtrn_code = (long *)item2;					/* Assign ptr to return code.		*/

			delete_col(list_num,num_col,rtrn_code);
			break;
		}
		case RESIZE_LIST:							/* Resize list structure, change number	*/
		{									/*  of rows.				*/
			long *lptr, list_num, num_rows;					/* Pointers to long.			*/
			long *rtrn_code; 						/* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			lptr = (long *)item1;
			num_rows = *lptr;						/* Assign the number of row in list.	*/
			rtrn_code = (long *)item2;					/* Assign ptr to return code.		*/

			resize_struct(list_num,num_rows,rtrn_code);
			break;
		}
		case SET_FUNCTIONS:							/* Set function key definitions.	*/
		{
			long *lptr, list_num;						/* Pointers to long.			*/
			unsigned char *key_data;
			long *rtrn_code; 						/* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			key_data = item1;						/* Assign ptr to key array definitions.	*/
			rtrn_code = (long *)item2;					/* Assign ptr to return code.		*/

			define_func_keys(list_num,key_data,rtrn_code);
			break;
		}
		case FREE_LIST:								/* Free the list from memory.		*/
		{
			long *lptr, list_num;						/* Pointers to long.			*/
			long *rtrn_code; 						/* Pointers to returned values.		*/

			lptr = (long *)list_id;
			list_num = *lptr;						/* Assign the list id number.		*/
			rtrn_code = (long *)item1;					/* Assign ptr to return code.		*/

			free_list_memory(list_num,rtrn_code);
			break;
		}
		default:								/* Everything else no good.		*/
		{
			vre("vlist(%d)-Invalid function value.",vlist_func);		/* Report the situation.		*/
			return(FAILURE);						/* Return to the caller.		*/
		}	
	}
	return(SUCCESS);								/* Return that we were successful.	*/
}

static int init_list(list_id,rows,retcd) long list_id, rows, *retcd;
{
	struct column *currcol, *ncol;
	struct list *a_def;
	struct active_list *lookup(), *newlist;
	register int i;									/* Working registers.			*/

	if (headal == NULL)
	{
		if (!(currlist = (struct active_list *)malloc(8+sizeof(struct active_list)))) memory_err(); /* Get some memory.	*/
		headal = currlist;							/* Init the head of list pointer.	*/
		currlist->l_id = list_id;						/* Init list id of the new structure.	*/
		currlist->func_keys = NULL;						/* Init to no functions available.	*/
		currlist->nextl = NULL;							/* Init the next list pointer to NULL.	*/
		*retcd = 1;								/* Set retcd to 1 for created new list.	*/
	}
	else
	{
		if ((currlist = lookup(list_id)) != NULL)				/* Returns a ptr to the list struct.	*/
		{									/* List already exists.			*/
			currcol = currlist->thedef->headcol;				/* Point to head of column list.	*/
			while (currcol != NULL)						/* Free column memory.			*/
			{
				ncol = currcol->nextc;					/* Remember ptr to next column.		*/
				free(currcol);						/* Free the memory.			*/
				currcol = ncol;						/* Step to next column.			*/
			}
			free(currlist->thedef->headcol);				/* Free the column memory.		*/
			free(currlist->thedef);						/* Free the column structure memory.	*/
			currlist->thedef = NULL;					/* Set ptr to column struct to NULL.	*/
			free(currlist->func_keys);					/* Free the func key structure memory.	*/
			currlist->func_keys = NULL;					/* Set the ptr to func key struct to NULL*/
			*retcd = 2;							/* Set retcd to 2 for existing list.	*/
		}
		else									/* Create new list.			*/
		{
			if (!(newlist = (struct active_list *)malloc(8+sizeof(struct active_list)))) memory_err(); /* Get memory.*/
			currlist = headal;						/* Set ptr to head of active lists.	*/
			while (currlist->nextl != NULL) currlist = currlist->nextl;	/* Traverse to end of list.		*/
			currlist->nextl = newlist;					/* Add new list to linked list.		*/
			newlist->l_id = list_id;					/* Init the list id for new list..	*/
			newlist->func_keys = NULL;					/* Set the ptr to func key array to NULL*/
			newlist->nextl = NULL;						/* Set the next ptr to NULL.		*/
			currlist = newlist;						/* Set current list pointer to new list.*/
			*retcd = 1;							/* Set retcd to 1 for created new list.	*/
		}
	}
	if (!(a_def = (struct list *)malloc(8+sizeof(struct list)))) memory_err();	/* Get some memory for the definition.	*/
	currlist->thedef = a_def;							/* Assign pointer to the def struct.	*/
	a_def->num_rows = rows;								/* Init the requested number of rows.	*/
	a_def->start_scroll_row = 0;							/* Init so scroll region is full screen.*/
	a_def->sfoot_row = MAX_LINES_PER_SCREEN;
	a_def->headcol = NULL;								/* Init ptr to column definitions.	*/
	for (i = 0; i < 4; i++)								/* Initialize the header and footer	*/
	{										/*  data.				*/
		a_def->thead[i].dtext = NULL;
		a_def->thead[i].lentext = 0;
		a_def->tfoot[i].dtext = NULL;
		a_def->tfoot[i].lentext = 0;
	}
	return(SUCCESS);
}

static int free_list_memory(list_id,retcd) long list_id, *retcd;			/* Free the memory assigne to list.	*/
{
	struct active_list *prevl;
	struct column *currcol, *ncol;
	register int i;									/* Working registers.			*/

	if (!headal) return(SUCCESS);							/* No list to free.			*/

	currlist = headal;								/* Set current ptr to head of list.	*/
	while (currlist->l_id != list_id)						/* Traverse to requested list.		*/
	{
		prevl = currlist;							/* Set the previous list ptr.		*/
		currlist = currlist->nextl;						/* Step to next list structure.		*/
		if (currlist == NULL)
		{									/* Report the situation.		*/
			vre("vlist(%d)-Invalid list id - does not exist.",list_id);
			*retcd = -1;							/* Return code is -1 for failure.	*/
			return(FAILURE);						/* Return to the caller.		*/
		}
	}
	if (currlist == headal)  headal = currlist->nextl;				/* Reset the head list ptr.		*/
	else  prevl->nextl = currlist->nextl;						/* Relink front of list to back of list.*/
	currcol = currlist->thedef->headcol;						/* Point to head of column list.	*/
	while (currcol != NULL)								/* Free column memory.			*/
	{
		ncol = currcol->nextc;							/* Remember ptr to next column.		*/
		free(currcol);								/* Free the memory.			*/
		currcol = ncol;								/* Step to next column.			*/
	}
	free(currlist->func_keys);							/* Free the memory held by func keys.	*/
	free(currlist->thedef);								/* Free the memory held by column def.	*/
	free(currlist);									/* Free the memory held by the list.	*/
	*retcd = 0;									/* Set the return code for success.	*/
	return(SUCCESS);
}

struct active_list *lookup(list_id) long list_id;
{
	currlist = headal;								/* Set current ptr to head of list.	*/
	if (headal == NULL) return (NULL);						/* There is no list.			*/
	while (currlist->l_id != list_id && currlist->nextl != NULL)
	{
		currlist = currlist->nextl;						/* Step to next list structure.		*/
	}
	if (currlist->l_id == list_id)
	{
		return(currlist);							/* Return ptr to list.			*/
	}
	return(NULL);									/* Return NULL for not found.		*/
}

static int add_head_foot(function,list_id,num_items,tp,retcd)
long function, list_id, num_items, *retcd;
struct tmpst *tp;
{
	struct active_list *lookup();
	struct list *a_def;
	register int i;									/* Working registers.			*/

	if ((currlist = lookup(list_id)) == NULL)					/* Returns a ptr to the list struct.	*/
	{
		vre("vlist(%d)-Invalid list id - does not exist.",list_id);		/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	a_def = currlist->thedef;							/* Set pointer to the list definition.	*/
	switch (function)
	{
		case ADD_HEADER:							/* Add the header.			*/
		{
			for (i = 0; i < 4; i++)
			{
				a_def->thead[i].lentext = tp->it[i].lentext;		/* Assign the lenght of the text item.	*/
				a_def->thead[i].dtext = tp->it[i].dtext;		/* Assign the ptr to text item.		*/
				a_def->thead[i].txtrend = tp->it[i].txtrend;
			}
			a_def->start_scroll_row = num_items;				/* Set start scroll for list.		*/
			st_scr = num_items;						/* Set beginning of scroll row.		*/
			*retcd = 0;							/* Return code is 0 for success.	*/
			break;
		}
		case ADD_FOOTER:							/* Add the footer.			*/
		{
			for (i = 0; i < 4; i++)
			{
				a_def->tfoot[i].lentext = tp->it[i].lentext;		/* Assign the lenght of the text item.	*/
				a_def->tfoot[i].dtext = tp->it[i].dtext;		/* Assign the ptr to text item.		*/
				a_def->tfoot[i].txtrend = tp->it[i].txtrend;
			}
			a_def->sfoot_row = MAX_LINES_PER_SCREEN - num_items;		/* Set start row for footer.		*/
			st_foot_scr = MAX_LINES_PER_SCREEN - num_items;			/* Set beginning of footer region.	*/
			*retcd = 0;							/* Return code is 0 for success.	*/
			break;
		}
	}
	return(SUCCESS);
}

static int add_col(function,list_id,type,width,itemlist,length,col_num,retcd)		/* Add column to the list.		*/
long function, list_id, type, width, length, col_num, *retcd;
unsigned char *itemlist;
{
	struct active_list *lookup();
	struct list *a_def;
	struct column *a_col, *currcol;
	long coln;
	register int i;									/* Working registers.			*/

	if ((currlist = lookup(list_id)) == NULL)					/* Returns a ptr to the list struct.	*/
	{
		vre("vlist(%d)-Invalid list id - does not exist.",list_id);		/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	a_def = currlist->thedef;							/* Set pointer to the list definition.	*/
	currcol = a_def->headcol;							/* Set ptr to the linked list of cols.	*/
	if ((function == INSERT_COLUMN || function == REPLACE_COLUMN) && currcol == NULL) /* Cannot insert/replace, no columns.	*/
	{
		vre("vlist(%d)-Invalid column %d. No columns in list.",function,col_num);/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	coln = 0;									/* Start with first column.		*/
	if (function != REPLACE_COLUMN)							/* If not replace.			*/
	{
		if (!(a_col = (struct column *)malloc(8+sizeof(struct column)))) memory_err(); /* Get some memory for new list.	*/
	}
	if (currcol == NULL)								/* If no columns in list yet.		*/
	{
  		a_def->headcol = a_col;							/* Init the head column ptr.		*/
		a_col->nextc = NULL;							/* No more columns.			*/
		a_col->prevc = NULL;							/* First column so no previous columns.	*/
	}
	else
	{
		if (function == ADD_COLUMN)						/* If add to end of list.		*/
		{
	 		while (currcol->nextc != NULL)					/* Traverse list to end of columns.	*/
			{
				currcol = currcol->nextc;
			}
			currcol->nextc = a_col;						/* Link new column to column list.	*/
			a_col->prevc = currcol;						/* Init the previous column link.	*/
		}
		else									/* Insert the column into the list.	*/
		{
			while (coln != col_num)						/* Traverse to column to insert/replace.*/
			{
				currcol = currcol->nextc;				/* Step to next column.			*/
				coln++;							/* Increment column counter.		*/
				if (currcol == NULL)
				{							/* Report the situation.		*/
					vre("vlist(%d)-Invalid column %d. Only %d column(s).",function,col_num,coln);
					*retcd = -1;					/* Return code is -1 for failure.	*/
					return(FAILURE);				/* Return to the caller.		*/
				}
			}
			if (function == REPLACE_COLUMN)					/* Set ptr to current column to replace	*/
			{								/*  data for column.			*/
				a_col = currcol;
			}
			else
			{								/* Reset the link pointers.		*/
				if (!coln)						/* Inserting at the head of list.	*/
				{
					a_def->headcol = a_col;				/* Init the head column ptr.		*/
					a_col->prevc = NULL;				/* Init the previous column ptr.	*/
				}
				else
				{
  					currcol->prevc->nextc = a_col;			/* Link front of list to new column.	*/
					a_col->prevc = currcol->prevc;
				}
				a_col->nextc = currcol;					/* Link end of list to new column.	*/
				currcol->prevc = a_col;
			}
		}
	}
	currcol = a_col;								/* Set ptr to new column.		*/
	currcol->type = type;								/* Assign type of data in column.	*/
	currcol->width = width;								/* Assign number of chars to display.	*/
	currcol->citems = itemlist;							/* Assign ptr to array of data.		*/
	currcol->length = length;							/* Assign actual length of data item.	*/
	if (function == ADD_COLUMN)  currcol->nextc = NULL;				/* End linked list.			*/
	*retcd = 0;									/* Return 0 for success.		*/
	return(SUCCESS);
}

static int delete_col(list_id,col_num,retcd) long list_id, col_num, *retcd;
{
	struct active_list *lookup();
	struct list *a_def;
	struct column *a_col, *currcol;
	long coln;
	register int i;									/* Working registers.			*/

	if ((currlist = lookup(list_id)) == NULL)					/* Returns a ptr to the list struct.	*/
	{
		vre("vlist(%d)-Invalid list id - does not exist.",list_id);		/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	a_def = currlist->thedef;							/* Set pointer to the list definition.	*/
	currcol = a_def->headcol;							/* Set ptr to the linked list of cols.	*/
	if (currcol == NULL)								/* Cannot insert because no columns.	*/
	{
		vre("vlist()-Invalid deletion of column %d. No columns in list.",col_num);/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	coln = 0;									/* Start with first column.		*/
	while (coln != col_num)								/* Traverse to column to delete.	*/
	{
		currcol = currcol->nextc;						/* Step to next column.			*/
		coln++;									/* Increment column counter.		*/
		if (currcol == NULL)
		{									/* Report the situation.		*/
			vre("vlist()-Invalid deletion of column %d. Only %d column(s).",col_num,coln);
			*retcd = -1;							/* Return code is -1 for failure.	*/
			return(FAILURE);						/* Return to the caller.		*/
		}
	}
	if (!coln)									/* If deleteing the head of the list.	*/
	{
		a_def->headcol = currcol->nextc;					/* Init the head column ptr.		*/
		currcol->nextc->prevc = NULL;						/* Init eh previous column ptr.		*/
	}
	else
	{
 		currcol->prevc->nextc = currcol->nextc;					/* Relink front of list to back of list.*/
		currcol->nextc->prevc = currcol->prevc;
	}
	free(currcol);									/* Free the memory held by column def.	*/
	*retcd = 0;									/* Set the return code for success.	*/
	return(SUCCESS);
}

static int resize_struct(list_id,rows,retcd) long list_id, rows, *retcd;
{
	struct active_list *lookup();
	struct list *a_def;
	register int i;									/* Working registers.			*/

	if ((currlist = lookup(list_id)) == NULL)					/* Returns a ptr to the list struct.	*/
	{
		vre("vlist(%d)-Invalid list id - does not exist.",list_id);		/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	a_def = currlist->thedef;							/* Set pointer to the list definition.	*/
	a_def->num_rows = rows;								/* Reset the number of rows in list.	*/
	*retcd = 0;									/* Set the return code for success.	*/
	return(SUCCESS);
}

static int define_func_keys(list_id,key_array,retcd)
long list_id, *retcd;
unsigned char *key_array;
{
	struct active_list *lookup();
	struct available_keys *fkeys, *fnk;
	register int i, j;								/* Working registers.			*/
	int cont;

	if ((currlist = lookup(list_id)) == NULL)					/* Returns a ptr to the list struct.	*/
	{
		vre("vlist(%d)-Invalid list id - does not exist.",list_id);		/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	fkeys = currlist->func_keys;							/* Set pointer to array of func keys.	*/
	if (fkeys != NULL)								/* Free memory holding local copy of	*/
	{										/*  struct array of function keys.	*/
		free(fkeys);
	}
	i = 0;
	cont = TRUE;
	fnk = (struct available_keys *)key_array;					/* Set ptr to beginning of passed array.*/
	while (cont)
	{										/* Get the number of available keys so	*/
		if (!fnk[i].meta_key)							/*  can malloc the array of structures.	*/
		{
			if (!fnk[i].list_function)  cont = FALSE;			/* Get out of loop.			*/
		}
		if (cont) i++;								/* Add one to available func keys.	*/
	}
	if (!(fkeys = (struct available_keys *)calloc(i,8+sizeof(struct available_keys)))) memory_err();	/* Get some memory.	*/
	currlist->func_keys = fkeys;							/* Set ptr in list structure.		*/
	fnk = (struct available_keys *)key_array;					/* Point to beginning of the list.	*/
	for (j = 0; j < i; j++)								/* Init th local copy of avail. funcs.	*/
	{
		fkeys[j].meta_key = fnk[j].meta_key;
		fkeys[j].list_function = fnk[j].list_function;
	}
	return(SUCCESS);
}

/********************************************************************************************************************************/
/* Scan a list stored in generated structure on the screen and let the user select items.					*/
/* Return the items selected to caller within a result bit mask array that corresponds to rows in list.				*/
/* Bit mask will be a 1 if selected, 0 otherwise.										*/
/********************************************************************************************************************************/

int display_scan(list_id,rst_row,rst_col,cursor_row,cursor_col,result_list,key_val,retcd,disp_fl)
long list_id, *rst_row, *rst_col, *cursor_row, *cursor_col, *result_list, *key_val, *retcd;
int disp_fl;										/* Flag to depict if redisplay or not.	*/
{
	struct active_list *lookup();
	struct list *a_def;
	struct column *hcol, *scr_col, *display_cols();
	struct available_keys *key_array;
	long *ipt;									/* Ptr to a long integer.		*/
	long ril;
	register int i, j;								/* Working registers.			*/
	int in_key, vgetm(), tx, ty, done;
	long currpos;
	int svo;
	short lf;

	svo = voptimize(DATA_AND_CONTROLS);
	vset(CURSOR,VISIBLE);
	if ((currlist = lookup(list_id)) == NULL)					/* Returns a ptr to the list struct.	*/
	{
		vre("vlist(%d)-Invalid list id - does not exist.",list_id);		/* Report the situation.		*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	a_def = currlist->thedef;							/* Set ptr to definition of list.	*/
	hcol = a_def->headcol;								/* Assign the ptr to the list of cols.	*/
	ril = a_def->num_rows;								/* Assign # avail. rows in list.	*/
	if (hcol == NULL)
	{
		vre("vlist()-No colums exist.");					/* Report the situation.		*/
		*retcd = -1;
		return(FAILURE);							/* Return to the caller.		*/
	}
	currpos = *cursor_row;
	ty = *cursor_col + 1;								/* Assign starting display cursor column.*/
	vroll((int)st_scr,(int)(st_foot_scr - 1));						/* Set the scroll region.		*/
	num_scr = st_foot_scr - st_scr;							/* Calculate the available scroll lines.*/
	if (disp_fl)									/* If display the re-init result list.	*/
	{
		for (i = 0; i < ril; i++)
		{
			ipt = result_list + i;						/* Get the address of each long.	*/
			*ipt = 0;							/* Init result list.			*/
		}
		display_header_footer(a_def,*rst_col);					/* Display the header and footer.	*/
		scr_col = display_cols(hcol,ril,*rst_row,*rst_col,currpos,result_list,retcd);
	}
	else scr_col = hcol; 
	for (;;)
	{
		int tmp;

		tx = st_scr + currpos;							/* Assign move position row on screen.	*/
		done = FALSE;								/* Set to not done so can do function.	*/
		tmp=optimization;
		optimization=OFF;
		vmove(tx,ty);								/* Move to current pos in scroll reg.	*/
		optimization=tmp;
		in_key = vgetm();							/* Input a meta character.		*/
		if (in_key == up_arrow_key)						/* Up one line.				*/
		{									/* If at the top of scroll region.	*/
			up_list(scr_col,rst_row,&currpos,result_list,cursor_row);
			done = TRUE;							/* Set so will not check for other keys.*/
		}
		else if (in_key == down_arrow_key || in_key == tab_key)			/* Down one line.			*/
		{									/* If at the bottom of scroll region.	*/
			down_list(scr_col,rst_row,ril,&currpos,result_list,cursor_row);
			done = TRUE;							/* Set so will not check for other keys.*/
		}
		else if (in_key == left_arrow_key)					/* Move cursor one character to left.	*/
		{
			if (ty - 1 < 1) vbell();					/* Can not move to left.		*/
			else
			{
				 ty--;							/* Move cursor to left one character.	*/
				*cursor_col = calc_col(scr_col,*rst_col,ty);		/* Get the current display column.	*/
			}
			done = TRUE;							/* Set so will not check for other keys.*/
		}
		else if (in_key == right_arrow_key)					/* Move cursor one character to right.	*/
		{
			if (ty + 1 > 79) vbell();					/* Can not move to right.		*/
			else
			{
				 ty++;							/* Move cursor to left one character.	*/
				*cursor_col = calc_col(scr_col,*rst_col,ty);		/* Get the current display column.	*/
			}
			done = TRUE;							/* Set so will not check for other keys.*/
		}
		else if (in_key == enter_key || in_key == return_key)			/* Terminate return to caller.		*/
		{
			*key_val = in_key;						/* Return the key pressed.		*/
			*retcd = 0;							/* Set to 0 for success.		*/
			done = TRUE;							/* Set so will not check for other keys.*/
			vroll(0,MAX_LINES_PER_SCREEN - 1);				/* Set scroll region back to full screen.*/
			voptimize(svo);
			return(SUCCESS);
		}
		if (currlist->func_keys == NULL && !done)
		{
			vbell();							/* If list has no active function keys.	*/
			*retcd = 0;							/* Set to 0 for success.		*/
		}
		else if (!done)								/* Check the available function keys.	*/
		{
			key_array = currlist->func_keys;				/* Set ptr to local copy of keys.	*/
			lf = get_function(in_key,key_array);				/* Get the list_function value.		*/
			switch(lf)
			{
				case ALLOW_KEY:						/* Terminate input, exit key.		*/
				{
					*key_val = in_key;				/* Return the function key pressed.	*/
					*retcd = 0;					/* Set to 0 for success.		*/
					vroll(0,MAX_LINES_PER_SCREEN - 1);		/* Set scroll region back to full screen.*/
					voptimize(svo);
					return(SUCCESS);
				}
				case UP_PAGE:						/* Scroll up a complete page.		*/
				{
					if (!(*rst_row))				/* Already at top.			*/
					{
						if (!currpos)  vbell();			/* Cursor already at top.		*/
						else
						{
							currpos = 0;			/* Set curent position to top.		*/
						}
					}
					else
					{
						if ((i = *rst_row - num_scr) <= 0)	/* If not a full page available.	*/
						{
							*rst_row = 0;
						}
						else *rst_row = *rst_row - num_scr;	/* Set start ptr to one page up.	*/
						scr_col = display_cols(hcol,ril,*rst_row,*rst_col,currpos,result_list,retcd);
					}
					*cursor_row = *rst_row + currpos;		/* Assign row index to return.		*/
					*retcd = 0;					/* Set to 0 for success.		*/
					break;
				}
				case DOWN_PAGE:						/* Scroll down a complete page.		*/
				{
					if (*rst_row + num_scr >= ril) 			/* Already at bottom.			*/
					{
						if (*rst_row + currpos == ril-1) vbell(); /* Cursor already at bottom.		*/
						else
						{
							currpos = ril - *rst_row - 1;	/* Set curent position to bottom.	*/
						}
					}
					else
					{
						*rst_row = *rst_row + num_scr;		/* Set start ptr to one page down.	*/
						while (*rst_row + num_scr > ril)	/* If not a full screen.		*/
						{
							(*rst_row)--;			/* Back up one row until full screen.	*/
						}
						if (currpos > ril - *rst_row - 1)	/* Reset cursor if curent pos too far.	*/
						{
							currpos = ril - *rst_row - 1;	/* Set to last one in list.		*/
						}
						scr_col = display_cols(hcol,ril,*rst_row,*rst_col,currpos,result_list,retcd);
					}
					*cursor_row = *rst_row + currpos;		/* Assign row index to return.		*/
					*retcd = 0L;					/* Set to 0 for success.		*/
					break;
				}
				case TOP:						/* Go to top of list.			*/
				{
					if (!(*rst_row))				/* Already at top.			*/
					{
						if (!currpos)  vbell();			/* Cursor already at top.		*/
						else
						{
							display_row(scr_col,*rst_row,0L,currpos,result_list);
							currpos = 0L;			/* Set curent position to top.		*/
							display_row(scr_col,*rst_row,currpos,*rst_row,result_list);
						}
					}
					else
					{
						*rst_row = 0L;				/* Set to top of list.			*/
						currpos = 0L;				/* Position cursor to top of list.	*/
						scr_col = display_cols(hcol,ril,*rst_row,*rst_col,currpos,result_list,retcd);
					}
					*cursor_row = *rst_row + currpos;		/* Assign row index to return.		*/
					*retcd = 0L;					/* Set to 0 for success.		*/
					break;
				}
				case BOTTOM:						/* Go to bottom of list.		*/
				{
					if (*rst_row + num_scr >= ril) 			/* Already at bottom.			*/
					{
						if (*rst_row + currpos == ril-1) vbell(); /* Cursor already at bottom.		*/
						else
						{
							display_row(scr_col,*rst_row,ril-*rst_row,currpos,result_list);
							currpos = ril - *rst_row - 1L;	/* Set curent position to bottom.	*/
							display_row(scr_col,*rst_row,currpos,currpos,result_list);
						}
					}
					else
					{
						*rst_row = ril - num_scr;		/* Set the start ptr to last page.	*/
						currpos = ril - *rst_row - 1L;		/* Set to last one in list.		*/
						scr_col = display_cols(hcol,ril,*rst_row,*rst_col,currpos,result_list,retcd);
					}
					*cursor_row = *rst_row + currpos;		/* Assign row index to return.		*/
					*retcd = 0L;					/* Set to 0 for success.		*/
					break;
				}
				case RIGHT_COL: case LEFT_COL: case RIGHT_PAGE: case LEFT_PAGE: /* Display scrolled columns.	*/
				{
					if (verify_columns(lf,scr_col,rst_col))		/* Verify that there are more columns.	*/
					{						/* Call display with new column.	*/
						display_header_footer(a_def,*rst_col); 	/* Display scrolled text.		*/
						scr_col = display_cols(hcol,ril,*rst_row,*rst_col,currpos,result_list,retcd);
						*cursor_col = calc_col(scr_col,*rst_col,ty); /* Get the current display column.	*/
					}
					else vbell();					/* Can not move right.			*/
					*retcd = 0;					/* Set to 0 for success.		*/
					break;
				}
				case SELECT_ROW: case DESELECT_ROW:			/* Select/Deselect a row.		*/
				{
					set_select_remove(lf,*rst_row,currpos,result_list);
					break;
				}
				default:						/* Unknown key. Ring bell.		*/
				{
					vbell();
					*retcd = 0;					/* Set to 0 for success.		*/
					break;
				}
			}
		}
	}										/* Repeat forever (Until exit key pressed)*/
}

static int disp_one_item(list_id,drow,col_num,itemlist,retcd)				/* Display one item in row.		*/
long list_id, drow, col_num, *retcd;
unsigned char *itemlist;
{
	struct active_list *lookup();
	struct list *a_def;
	struct column *a_col, *currcol;
	long coln, cwidth, clength;
	register int i, j;								/* Working registers.			*/
	char buff[81], *buf;								/* Ptr to buffer to hold display row.	*/
	long dcol, st_scrl;

	if ((currlist = lookup(list_id)) == NULL)					/* Returns a ptr to the list struct.	*/
	{
		vre("vlist(%d)-Invalid list id - does not exist.",list_id);		/* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	a_def = currlist->thedef;							/* Set pointer to the list definition.	*/
	currcol = a_def->headcol;							/* Set ptr to the linked list of cols.	*/
	st_scrl = a_def->start_scroll_row;						/* Get scroll region start row.		*/
	if (currcol == NULL) 								/* No columns.				*/
	{
		vre("vlist(%d)-Invalid column %d. No columns in list.",list_id,col_num); /* Report the situation.		*/
		*retcd = -1;								/* Return code is -1 for failure.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	coln = 0;									/* Start with first column.		*/
	dcol = 1;									/* Start at first column.		*/
	while (coln != col_num)								/* Traverse to column.			*/
	{
		dcol = dcol + currcol->width;						/* Add width so get correct column.	*/
		currcol = currcol->nextc;						/* Step to next column.			*/
		coln++;									/* Increment column counter.		*/
		if (currcol == NULL)
		{									/* Report the situation.		*/
			vre("vlist(%d)-Invalid column %d. Only %d column(s).",list_id,col_num,coln);
			*retcd = -1;							/* Return code is -1 for failure.	*/
			return(FAILURE);						/* Return to the caller.		*/
		}
	}
	cwidth = currcol->width;							/* Assign number of chars to display.	*/
	clength = currcol->length;							/* Assign actual length of data item.	*/
	memset(buff,' ',80);								/* Initialize the buffer to blank.	*/
	buff[80] = '\0';								/* Null terminate the buffer.		*/
	buf = buff;									/* Set pointer to buffer.		*/
	j = 0;										/* Set to zero characters momved.	*/
	while (*itemlist && j < cwidth)							/* Copy until null or string length.	*/
	{
		if (j >= clength) break;						/* Actual length is less than display.	*/
		*buf++ = *itemlist++;							/* Append text item to buffer.		*/
		j++;									/* Increment num chars moved.		*/
	}
	for (i = j; i < cwidth; i++) *buf++ = ' ';					/* Fill spaces for length to display.	*/
	*buf = '\0';									/* Null terminate the string.		*/
	vmove((int)(drow+st_scrl),(int)dcol);						/* Move to position to display.		*/
	vprint("%s",buff);								/*  Display the item.			*/
	*retcd = 0;									/* Return 0 for success.		*/
	return(SUCCESS);
}

static int display_header_footer(a_def,st_col)						/* Display the header and footer.	*/
struct list *a_def;
long st_col;
{
	struct column *a_col;
	long ccol;
	int i, cnt, stc, numf, rend;
	char *cp;									/* Ptr to a character.			*/
	char disp_text[81];								/* Temp var. to hold displyed text.	*/

	vmove(0,0);									/* Move cursor to home position.	*/
	verase(FULL_SCREEN);								/* Erase the screen.			*/
	ccol = 0;									/* Set the col to first one.		*/
	stc = 0;									/* Start at 0 position on screen.	*/
	st_scr = a_def->start_scroll_row;						/* Get current list scroll parameters.	*/
	st_foot_scr = a_def->sfoot_row;
	a_col = a_def->headcol;								/* Get address of starting column.	*/
	while (ccol < st_col && a_col != NULL)						/* Calculate the starting column text.	*/
	{
		stc = stc + a_col->width;						/* Increment cnt by width of column.	*/
		ccol++;									/* Increment column number.		*/
		a_col = a_col->nextc;							/* Point to next column on screen.	*/
	}
	for (i = 0; i < st_scr; i++)							/* Loop to print headers.		*/
	{
		if ((cp = (char *)a_def->thead[i].dtext) != NULL)			/* Set ptr to start of text.		*/
		{
			cp = cp + stc;							/* Move ptr to start display column.	*/
			cnt = a_def->thead[i].lentext - stc;				/* Assign number of chars to copy.	*/
			if (cnt > 80) cnt = 80;						/* disp_text var is only 80 chars.	*/
			if (cnt > 0)							/* If there is text at starting pos.	*/
			{
				strncpy(disp_text,cp,cnt);				/* Copy characters into temp string.	*/
				while (cnt < 80)					/* it is readily available.		*/
				{
					disp_text[cnt] = ' ';				/* Pad out string with spaces.		*/
					cnt++;
				}
				disp_text[cnt] = '\0';					/* Null terminate the string.		*/
			}
			else disp_text[0] = '\0';					/* Display a blank line.		*/
			rend = a_def->thead[i].txtrend;					/* Assign the line rendition.		*/
			vtext(rend,i,0,"%s",disp_text);					/* Print the header.			*/
		}
	}
	vroll(0,MAX_LINES_PER_SCREEN - 1);						/* Set so can display outside of scroll.*/
	vmove (st_foot_scr,0);								/* Move to footer region.		*/
	numf = MAX_LINES_PER_SCREEN - st_foot_scr;					/* Assign the number of footers.	*/
	for (i = 0; i < numf; i++)							/* Loop to print footers.		*/
	{
		if ((cp = (char *)a_def->tfoot[i].dtext) != NULL)			/* Set ptr to start of text.		*/
		{
			cp = cp + stc;							/* Move ptr to start display column.	*/
			cnt = a_def->tfoot[i].lentext - stc;				/* Assign number of chars to copy.	*/
			if (cnt > 80) cnt = 80;						/* disp_text var is only 80 chars.	*/
			if (cnt > 0)							/* If there is text at starting pos.	*/
			{
				strncpy(disp_text,cp,cnt);				/* Copy characters into temp string.	*/
				while (cnt < 80)					/* it is readily available.		*/
				{
					disp_text[cnt] = ' ';				/* Pad out string with spaces.		*/
					cnt++;
				}
				disp_text[cnt] = '\0';					/* Null terminate the string.		*/
			}
			else disp_text[0] = '\0';					/* Display a blank line.		*/
			rend = a_def->tfoot[i].txtrend;					/* Assign the line rendition.		*/
			vtext(rend,st_foot_scr + i,0,"%s",disp_text);			/* Print the footer.			*/
		}
	}
	vmode(CLEAR);									/* Set rendition back to normal.	*/
	vroll((int)st_scr,(int)(st_foot_scr - 1));					/* Set back to original scroll region.	*/
	return(SUCCESS);
}

struct column *display_cols(cp,rinl,st_row,st_col,currpos,result_list,retcd) 		/* Display columns.			*/
struct column *cp;
long rinl;
long st_row, st_col, currpos, *retcd, *result_list;
{
	long coln;
	long lin;
	register j;

	coln = 0L;									/* Init to beginning of columns.	*/
	while ((coln != st_col) && (cp != NULL))
	{
		cp = cp->nextc;								/* Traverse to start display column.	*/
		coln++;
	}
	if (cp == NULL)
	{
		vre("vlist()-Start column number %d does not exist.",st_col);		/* Report the situation.		*/
		*retcd = -1;
		return(FAILURE);							/* Return to the caller.		*/
	}
	for (lin = 0L; (lin < num_scr && lin < (rinl - st_row)); lin++)			/* Available lines in scroll region	*/
	{										/*  or while there is data.		*/
		display_row(cp,st_row,currpos,lin,result_list);				/* Display one row.			*/
	}
	for (j = lin + st_scr; j < num_scr + st_scr; j++)				/* Erase the rest of the scroll region.	*/
	{
		vmove(j,0);								/* Move to the line.			*/
		verase(CURRENT_LINE);							/* Erase it.				*/
	}
	return(cp);									/* Retrun ptr to starting scroll col.	*/
}

static int display_row(cp,st_row,currpos,row,result_list)				/* Display one row.			*/
struct column *cp;
long st_row, row, *result_list;
long currpos;
{
	int pia, drow, rend;
	long *ipt;
	char pref;
	int scr_fl;
	int tmp;

	scr_fl = FALSE;									/* Set to no scroll.			*/
	if (row == -1 || row == -2)							/* If going to scroll the screen.	*/
	{
		if (row == -1)								/* Set to scroll forward/down.		*/
		{
			drow = st_scr + currpos;					/* Assign the display row.		*/
			vmove(drow,79);
			vprint("\n");
		}
		else									/* Set to scroll backward/up.		*/
		{
			drow = st_scr;							/* Assign the display row.		*/
			vlinefeed(REVERSE);						/* Set for scrolling backward.		*/
			vprint("\n");							/* Linefeed forward first.		*/
		}
		row = currpos;								/* Set row to what it should be.	*/
		scr_fl = TRUE;								/* Is scroll down or up.		*/
	}
	else  drow = st_scr + row;							/* Calculate row position on screen.	*/
	pia = st_row + row;								/* Assign position in array.		*/
	tmp=optimization;
	optimization=OFF;
	vmove(drow,0);									/* Move to beginning of current line.	*/
	optimization=tmp;
	if (!scr_fl)  /* verase(CURRENT_LINE)	*/ ;					/* Erase the current line.		*/
	ipt = (long *)(result_list + pia);						/* Get the address of current long.	*/
	if (*ipt == 1)  pref = '*';							/* Item is marked.			*/
	else  pref = ' ';								/* Regular item.			*/
	generate_display(pref,cp,pia,row);						/* Generate the string and then display.*/
	return(SUCCESS);
}

static int generate_display(pref,colst,rndx,row)
int rndx;
long row;
char pref;
struct column *colst;
{
	long ctype, cwidth, clength;
	char *itlist, *pc;
	struct column *currc;
	int cos, i, j;									/* Counter for col position on screen.	*/
	short *ps;									/* Pointer to an integer.		*/
	long *pl;									/* pointer to a long.			*/
	char buff[81], *buf;								/* Ptr to buffer to hold display row.	*/
	char tmp[81], *tptr;								/* Variable to hold item to display.	*/

	memset(buff,' ',80);								/* Initialize the buffer to blank.	*/
	buff[80] = '\0';								/* Null terminate the buffer.		*/
	buf = buff;									/* Set pointer to buffer.		*/
	*buf++ = pref;									/* Put prefix into buffer.		*/
	currc = colst;									/* Set the starting column.		*/
	cos = 1;									/* Set to starting column of data.	*/
	while (currc != NULL && (cos + currc->width) <= 80)				/* Generate the string while still have	*/
	{										/*  cols and space on the screen.	*/
		ctype = currc->type;							/* Get the type of data.		*/
		cwidth = currc->width;							/* Get the num of chars to display.	*/
		clength = currc->length;						/* get the actual length of one item.	*/
		itlist = (char *)currc->citems;						/* Get ptr to the array of data.	*/
		switch (ctype)
		{
			case RSHORT: case USHORT:					/* Convert short -> string.		*/
			{
				ps = (short *)(itlist + (rndx * sizeof(short)));	/* Get the address of the item.		*/
				sprintf(tmp,"%d",*ps);					/* Convert to string in memory.		*/
				tptr = tmp;						/* Set pointer to point to temp var.	*/
				while (*tptr) *buf++ = *tptr++;				/* Append short item to buffer.		*/
				for (i = strlen(tmp); i < cwidth; i++) *buf++ = ' ';	/* Fill spaces for length to display.	*/
				break;
			}
			case RLONG: case ULONG:						/* Convert long -> string.		*/
			{
				pl = (long *)(itlist + (rndx * sizeof(long)));		/* Get the address of the item.		*/
				sprintf(tmp,"%ld",*pl);					/* Convert to string in memory.		*/
				tptr = tmp;						/* Set pointer to point to temp var.	*/
				while (*tptr) *buf++ = *tptr++;				/* Append long item to buffer.		*/
				for (i = strlen(tmp); i < cwidth; i++) *buf++ = ' ';	/* Fill spaces for length to display.	*/
				break;
			}
			case TEXTIT: case SEPARATOR: 					/* Text item or separator.		*/
			{
				if (ctype == SEPARATOR)  pc = itlist;			/* Has only one item in list.		*/
				else pc = itlist + (clength * rndx);			/* Calculate the address of the item.	*/
				j = 0;							/* Set to zero characters momved.	*/
				while (*pc && j < cwidth)				/* Copy until null or string length.	*/
				{
					if (j >= clength) break;			/* Actual length is less than display.	*/
					*buf++ = *pc++;					/* Append text item to buffer.		*/
					j++;						/* Increment num chars moved.		*/
				}
				for (i = j; i < cwidth; i++) *buf++ = ' ';		/* Fill spaces for length to display.	*/
				break;
			}
			default:							/* Everything else no good.		*/
			{
				vre("vlist()-Invalid column type - %d.",ctype);		/* Report the situation.		*/
				return(FAILURE);					/* Return to the caller.		*/
			}	
		}
		currc = currc->nextc;							/* Point to next column.		*/
		cos = cos + cwidth;							/* Add width of column to counter.	*/
	}
	while (cos < 80)
	{
		*buf++ = ' ';								/* Pad end of buffer with spaces.	*/
		cos++;
	}
	if (row != num_scr - 1)  *buf++ = '\n';						/* Add a line feed for scrolling.	*/
	*buf = '\0';									/* Null terminate the string.		*/
	vprint("%s",buff);								/*  Display the row.			*/
	return(SUCCESS);
}

static int set_select_remove(kfunc,st_row,currpos,result_list)				/* Set the select/remove bit.	*/
long st_row, *result_list;
int kfunc;
long currpos;
{
	long pia;
	long *ipt;

	pia = st_row + currpos;								/* Assign position in array.		*/
	ipt = (long *)(result_list + pia);						/* Get the address of current long.	*/
	if (kfunc == SELECT_ROW)
	{
		if (*ipt == 1)								/* Already selected.			*/
		{
			*ipt = 0;							/* Deselect the row.			*/
			vmove((int)(st_scr + currpos),0);				/* Move to mark area.			*/
			vprint(" ");							/* Display as not marked.		*/
		}
		else
		{
			*ipt = 1;							/* Select the row.			*/
			vmove((int)(st_scr + currpos),0);				/* Move to mark area.			*/
			vprint("*");							/* Display as marked.			*/
		}
	}
	else if (kfunc == DESELECT_ROW)
	{
		if (*ipt == 0)								/* Is not selected.			*/
		{
			*ipt = 1;							/* Select the row.			*/
			vmove((int)(st_scr + currpos),0);				/* Move to mark area.			*/
			vprint("*");							/* Display as marked.			*/
		}
		else 
		{
			*ipt = 0;							/* Deselect the row.			*/
			vmove((int)(st_scr + currpos),0);				/* Move to mark area.			*/
			vprint(" ");							/* Display as not marked.		*/
		}
	}
	else
	{
		vbell();
		return(FAILURE);
	}
	return(SUCCESS);
}

static int up_list(scr_col,st_row,currpos,result_list,cursor_row)			/* Up one line.				*/
struct column *scr_col;
long *result_list, *st_row, *cursor_row;
long *currpos;
{
	if (*currpos == 0)
	{
		if (*st_row == 0)							/* Already at beginning of list.	*/
		{
			vbell();							/* Ring bell.				*/
			*cursor_row = 0;						/* Assign row index to return.		*/
			return(SUCCESS);						/* No new display.			*/
		}
		(*st_row)--;								/* Set array index for top scroll row.	*/
		*cursor_row = *st_row;							/* Assign row index to return.		*/
		display_row(scr_col,*st_row,*currpos,-2L,result_list);			/* Scroll backwards one line.		*/
		return(SUCCESS);
	}
	(*currpos)--;									/* Set curent position to one line up.	*/
	*cursor_row = *st_row + *currpos;						/* Assign row index to return.		*/
	return(SUCCESS);
}

static int down_list(scr_col,st_row,ril,currpos,result_list,cursor_row)			/* Down one line.			*/
struct column *scr_col;
long *result_list, *cursor_row, *st_row, ril;
long *currpos;
{											/* End of scroll region or end of list.	*/
	if ((st_scr + *currpos == num_scr + (st_scr - 1)) || (*st_row + *currpos == ril - 1))
	{
		if (*st_row + *currpos == ril - 1)					/* Already at end of list.		*/
		{
			vbell();							/* Ring bell.				*/
		}
		else
		{
			(*st_row)++;							/* Set array index for bottom scroll row.*/
			display_row(scr_col,*st_row,*currpos,-1L,result_list);		/* Display next row.			*/
		}
	}
	else  (*currpos)++;								/* Set curent position to one line down.*/
	*cursor_row = *st_row + *currpos;						/* Assign row index to return.		*/
	return(SUCCESS);
}

static int verify_columns(lfunc,scr_col,st_col)						/* Verify that scroll can occur.	*/
struct column *scr_col;
long *st_col;
int lfunc;
{
	struct column *currcol;
	int ccol, cnt;
	long i;

	if (lfunc == LEFT_COL || lfunc == LEFT_PAGE)					/* If left scroll.			*/
	{
		ccol = *st_col - 1;							/* Test decrement the start column.	*/
		if (ccol < 0) return(FAILURE);						/* Already at left most column.		*/
		else									/* Try to scroll a full page left.	*/
		{
			if (lfunc == LEFT_COL)  (*st_col)--;				/* Decrement start counter.		*/
			else 								/* Try to scroll left a full page.	*/
			{
				ccol = *st_col;
				cnt = 1;						/* Set cnt to available characters.	*/
				currcol = scr_col->prevc;				/* Set ptr to one column left.		*/
				if (currcol == NULL) return(FAILURE);			/* Already at left most column.		*/
				while (currcol != NULL && (cnt + currcol->width) <= 80)
				{							/* Step backwards one column at a time.	*/
					cnt = cnt + currcol->width;			/* Decrement cnt by length of prev col.*/
					currcol = currcol->prevc;			/* Backup one column in list.		*/
					ccol--;						/* Decrement column number.		*/
				}
				*st_col = ccol;						/* Set start column to one page left.	*/
			}								/*  or available columns left.		*/
		}
	}
	else
	{
		ccol = *st_col;								/* Set the col to first one of display.	*/
		cnt = 1;								/* Start at 0 position on screen.	*/
		currcol = scr_col;
		while (currcol != NULL && (cnt + currcol->width) <= 80)			/* See if there are any more columns.	*/
		{
			cnt = cnt + currcol->width;					/* Increment cnt by width of column.	*/
			ccol++;								/* Increment column number.		*/
			currcol = currcol->nextc;					/* Point to next column on screen.	*/
		}
		if (currcol == NULL)  return(FAILURE);					/* Already at right most column.	*/
		if (lfunc == RIGHT_COL)  (*st_col)++;					/* Increment the start column.		*/
		else
		{									/* See if full screen is displayed.	*/
			if (currcol->nextc == NULL && currcol->width < 80)
			{
				cnt = currcol->width;					/* Start with num chars displayed.	*/
				while (cnt + currcol->prevc->width < 80)		/* Backup until full screen displayed.	*/
				{
					ccol--;
					currcol = currcol->prevc;
					cnt = cnt + currcol->width;			/* Add previous column chars.		*/
				}
			}
			*st_col = ccol;							/* Scroll right a page.			*/
		}
	}
	return(SUCCESS);								/* Is ok to scroll.			*/
}

static int calc_col(scr_col,st_col,ty)
struct column *scr_col;
long st_col;
int ty;
{
	int ccol, cnt;
	long i;

	ccol = st_col;									/* Set the col to first one of display.	*/
	cnt = 0;									/* Start at 0 position on screen.	*/
	while (cnt < 80 && scr_col != NULL)						/* While still on screen with columns.	*/
	{
		i = cnt + scr_col->width;						/* Get display width of column + cnt.	*/
		if (ty > cnt && ty <= i)						/* If cursor is in this column.		*/
		{
			return(ccol);							/* Return column number.		*/
		}
		cnt = i;								/* Increment cnt by width of column.	*/
		ccol++;									/* Increment column number.		*/
		scr_col = scr_col->nextc;						/* Point to next column on screen.	*/
	}
	return(ccol);									/* return last column on screen.	*/
}

static int get_function(in_key,key_array)
int in_key;
struct available_keys *key_array;
{
	short lfunc;
	int cont, i;

	cont = TRUE;
	i = 0;
	while (cont)
	{										/* Get the number of available keys so	*/
		if (!key_array[i].meta_key)						/*  can malloc the array of structures.	*/
		{
			if (!key_array[i].list_function)
			{
				lfunc = -1;						/* Assign a failure value.		*/
				cont = FALSE;						/* Get out of loop.			*/
			}
		}
		else
		{
			if (key_array[i].meta_key == in_key)				/* Compare the input key with array val.*/
			{
				lfunc = key_array[i].list_function;			/* Assign the appropriate list function.*/
				cont = FALSE;
			}
			else  i++;							/* Step to next array structure.	*/
		}
	}
	return(lfunc);
}

int memory_err()
{
	vre("Run-time error, unable to allocate memory for list structure");		/* Oops, cannot get memory.		*/
	vexit();									/* Unconditionally fatal.		*/
}                                         
