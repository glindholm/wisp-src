			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

			/*	        QATSTLST.C - Quality Assurance of vlist			*/

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>

#include <ctype.h>

#define MAXROW	50

char titem[MAXROW][15];									/* Array holding names of months.	*/
long days[MAXROW];									/* Array holding days in months.	*/
short num[MAXROW];									/* Array holding row number.		*/
char descr[MAXROW][40];									/* Array holding decription.		*/
char bday[MAXROW][20];									/* Array of birthdays this month.	*/
char separator[10];									/* Array to hold the column seperator.	*/
char csep[12];
long rows, numh, numf;
char *tstr[8];										/* Array to hold ptr to header/footer txt.*/
long tstrl[8];										/* Array to hold header & footer length.*/
int tstrrnd[8];
struct available_keys {
			short meta_key;							/* VIDEO meta_key value.		*/
			short list_function;						/* Value defined by vlist.		*/
		};
struct available_keys fn_keys[12];

extern int memory_err();

QATSTLST()
{
	long function, list_id, retcd;
	long rrow, rcol, key;
	long type, width, length, icol;
	long st_row, st_col;
	long result_list[MAXROW];
	register int i;									/* Working registers.			*/
	int pfk_val;
	void init_data();
	void init_fkeys();

	function = INIT_LIST;
	list_id = 28;
	rows = 24;
	vlist(&function,&list_id,&rows,&retcd);						/* Init the new list.			*/
	for (i = 0; i < MAXROW; i++)
	{
		titem[i][0] = '\0';							/* Init the data array.			*/
		descr[i][0] = '\0';
		bday[i][0] = '\0';
		days[i] = 0;
		num[i] = 0;
		result_list[i] = 0;							/* Init the result array.		*/
	}
	init_data(2);
	function = ADD_HEADER;								 /* Add the header.			*/
	vlist(&function,&list_id,&numh,&retcd,tstr[0],&tstrl[0],&tstrrnd[0],tstr[1],&tstrl[1],&tstrrnd[1],
					tstr[2],&tstrl[2],&tstrrnd[2]);
	function = ADD_FOOTER;								/* Add the footer.			*/
	vlist(&function,&list_id,&numf,&retcd,tstr[4],&tstrl[4],&tstrrnd[4],tstr[5],&tstrl[5],&tstrrnd[5],
					tstr[6],&tstrl[6],&tstrrnd[6],tstr[7],&tstrl[7],&tstrrnd[7]);
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 10;
	length = 15;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,titem,&length,&icol,&retcd);		/* Add a column.			*/
	function = ADD_COLUMN;
	type = SEPARATOR;
	width = 9;
	length = 9;
	strcpy(separator,"****!****");							/* Is the separator value.		*/
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,separator,&length,&icol,&retcd);		/* Add a column.			*/
	function = ADD_COLUMN;
	type = ULONG;
	width = 4;
	length = 2;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,days,&length,&icol,&retcd);		/* Add a column.			*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 19;
	length = 20;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,bday,&length,&icol,&retcd);		/* Add a column.			*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 30;
	length = 40;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,descr,&length,&icol,&retcd);		/* Add a column.			*/
	function = INSERT_COLUMN;
	type = RSHORT;
	width = 3;
	length = 2;
	icol = 0;									/* Is zero so will add to head of list.	*/
	vlist(&function,&list_id,&type,&width,num,&length,&icol,&retcd);		/* Insert a column.			*/
	function = INSERT_COLUMN;
	type = SEPARATOR;
	width = 12;
	strcpy(csep,"!!");								/* Is the separator value.		*/
	length = 2;
	icol = 1;
	vlist(&function,&list_id,&type,&width,csep,&length,&icol,&retcd);		/* Insert a column.			*/
	init_fkeys();
	function = SET_FUNCTIONS;	
	vlist(&function,&list_id,fn_keys,&retcd);					/* Set function key definitions.	*/
	function = DISPLAY_LIST;
	st_row = 0;
	st_col = 0;
	rrow = 0;
	rcol = 0;
	vlist(&function,&list_id,&st_row,&st_col,&rrow,&rcol,result_list,&key,&retcd);	/* Display list.			*/
	verase(FULL_SCREEN);
	pfk_val = get_func_val(key);
	vtext(CLEAR,3,5,"PF key pressed (Octal) = %o  Function = %d",key,pfk_val);
	vtext(CLEAR,4,5,"Array row = %d  Column = %d",rrow,rcol);
	vtext(CLEAR,7,1,"The next section will do a replace column function and reassign the start row value.");
	vtext(CLEAR,8,1,"The list will then be re-displayed.");
	vtext(CLEAR,23,1,"Push any key to continue.");
	vmove(23,26);
	i = vgetc();
	function = REPLACE_COLUMN;
	type = USHORT;
	width = 3;
	length = 2;
	icol = 2;
	vlist(&function,&list_id,&type,&width,num,&length,&icol,&retcd);		/* Replace a column.			*/
	function = DISPLAY_LIST;
	st_row = 3;
	st_col = 0;
	rrow = 0;
	rcol = 0;
	vlist(&function,&list_id,&st_row,&st_col,&rrow,&rcol,result_list,&key,&retcd);	/* Display list.			*/
	function = FREE_LIST;
	vlist(&function,&list_id,&retcd);						/* Free up memory taken by list.	*/
	verase(FULL_SCREEN);
}											/* Go back to cobol.			*/

void init_data(cnt)
int cnt;
{
	int i;
	char *calloc();
	char *cp;

	for (i = 0; i < cnt; i++)
	{
		strcpy(titem[(i * 12)],"January");					/* Init the arrays with given data.	*/
		days[(i * 12)] = 31;
		num[(i * 12)] = 1 + (i * 12);
		strcpy(descr[(i * 12)],"This is the first month of the year.");
		strcpy(bday[(i * 12)],"Mary Jo");
		strcpy(titem[1+(i*12)],"February");
		days[1+(i*12)] = 28;
		num[1+(i*12)] = 2 + (i * 12);
		strcpy(descr[1+(i * 12)],"This is the second month of the year.");
		strcpy(titem[2+(i*12)],"March");
		days[2+(i*12)] = 31;
		num[2+(i*12)] = 3 + (i * 12);
		strcpy(descr[2+(i * 12)],"This is the third month of the year.");
		strcpy(bday[2+(i * 12)],"Jennifer");
		strcpy(titem[3+(i*12)],"April");
		days[3+(i*12)] = 30;
		num[3+(i*12)] = 4 + (i * 12);
		strcpy(descr[3+(i * 12)],"This is the fourth month of the year.");
		strcpy(bday[3+(i * 12)],"Joesoph");
		strcpy(titem[4+(i*12)], "May");
		days[4+(i*12)] = 31;
		num[4+(i*12)] = 5 + (i * 12);
		strcpy(descr[4+(i * 12)],"This is the fifth month of the year.");
		strcpy(bday[4+(i * 12)],"Andrew");
		strcpy(titem[5+(i*12)],"June");
		days[5+(i*12)] = 30;
		num[5+(i*12)] = 6 + (i * 12);
		strcpy(descr[5+(i * 12)],"This is the sixth month of the year.");
		strcpy(bday[5+(i * 12)],"Steve, Peggy, Dad");
		strcpy(titem[6+(i*12)],"July");
		days[6+(i*12)] = 31;
		num[6+(i*12)] = 7 + (i * 12);
		strcpy(descr[6+(i * 12)],"This is the seventh month of the year.");
		strcpy(bday[6+(i * 12)],"Fabienne, Suzette");
		strcpy(titem[7+(i*12)],"August");
		days[7+(i*12)] = 31;
		num[7+(i*12)] = 8 + (i * 12);
		strcpy(descr[7+(i * 12)],"This is the eighth month of the year.");
		strcpy(titem[8+(i*12)],"September");
		days[8+(i*12)] = 30;
		num[8+(i*12)] = 9 + (i * 12);
		strcpy(descr[8+(i * 12)],"This is the ninth month of the year.");
		strcpy(bday[8+(i * 12)],"Mom, Ann");
		strcpy(titem[9+(i*12)],"October");
		days[9+(i*12)] = 31;
		num[9+(i*12)] = 10 + (i * 12);
		strcpy(descr[9+(i * 12)],"This is the tenth month of the year.");
		strcpy(bday[9+(i * 12)],"Terri Austin");
		strcpy(titem[10+(i*12)],"November");
		days[10+(i*12)] = 30;
		num[10+(i*12)] = 11 + (i * 12);
		strcpy(descr[10+(i * 12)],"This is the eleventh month of the year.");
		strcpy(titem[11+(i*12)],"December");
		days[11+(i*12)] = 31;
		num[11+(i*12)] = 12 + (i * 12);
		strcpy(descr[11+(i * 12)],"This is the twelfth and last month of the year.");
		strcpy(bday[11+(i * 12)],"Stefani, Tammy Rogers");
	}
	tstrl[0] = 53;									/* Init the array of lengths.		*/
	if ((cp = calloc(8+tstrl[0],sizeof(char))) == 0) memory_err();			/* Get some memory for the string.	*/
	tstr[0] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"                         List of Months and Birthdays");		/* Init the header string.		*/
	tstrrnd[0] = CLEAR;
	tstrl[1] = 1;									/* Init the array of lengths.		*/
	if ((cp = calloc(8+tstrl[1],sizeof(char))) == 0) memory_err();			/* Get some memory for the string.	*/
	tstr[1] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp," ");									/* Init the header string.		*/
	tstrrnd[1] = CLEAR;
	tstrl[2] = 69;									/* Init the array of lengths.		*/
	if ((cp = calloc(8+tstrl[2],sizeof(char))) == 0) memory_err();			/* Get some memory for the string.	*/
	tstr[2] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp," Num             Month           Days  Birthdays          Description");
	tstrrnd[2] = UNDERSCORE;
	numh = 3;									/* Init the footers.			*/
	tstrl[4] = 42;									/* Init the array of lengths.		*/
	if ((cp = calloc(8+tstrl[4],sizeof(char))) == 0) memory_err();			/* Get some memory for the string.	*/
	tstr[4] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"     This is the first line of the footer!");
	tstrrnd[4] = CLEAR;
	tstrl[5] = 80;									/* Init the array of lengths.		*/
	if ((cp = calloc(8+tstrl[5],sizeof(char))) == 0) memory_err();			/* Get some memory for the string.	*/
	tstr[5] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"Available functions:  (1) Toggle Mark/Unmark item                       (16)Exit");
	tstrrnd[5] = CLEAR;
	tstrl[6] = 68;									/* Init the array of lengths.		*/
	if ((cp = calloc(8+tstrl[6],sizeof(char))) == 0) memory_err();			/* Get some memory for the string.	*/
	tstr[6] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"                      (2)Top   (3)Bottom   (4)Up page   (5)Down page");
	tstrrnd[6] = CLEAR;
	tstrl[7] = 78;									/* Init the array of lengths.		*/
	if ((cp = calloc(8+tstrl[7],sizeof(char))) == 0) memory_err();			/* Get some memory for the string.	*/
	tstr[7] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"                      (8)Left col  (9)Right col  (10)Left page  (11)Right page");
	tstrrnd[7] = CLEAR;
	numf = 4;
}

void init_fkeys()								/* Assign values and functions to 	*/
{											/*  available keys.			*/
	fn_keys[0].list_function = SELECT_ROW;						/* Select this row.			*/
	fn_keys[0].meta_key = fn1_key;							/* Assign PF1.				*/
	fn_keys[1].list_function = DESELECT_ROW;					/* Deselect this row.			*/
	fn_keys[1].meta_key = fn1_key;							/* Assign PF1.				*/
	fn_keys[2].list_function = VLIST_TOP;						/* Go to top of list.			*/
	fn_keys[2].meta_key = fn2_key;							/* Assign PF2.				*/
	fn_keys[3].list_function = VLIST_BOTTOM;					/* Go to bottom of list.		*/
	fn_keys[3].meta_key = fn3_key;							/* Assign PF3.				*/
	fn_keys[4].list_function = UP_PAGE;						/* Scroll up a complete page.		*/
	fn_keys[4].meta_key = fn4_key;							/* Assign PF4.				*/
	fn_keys[5].list_function = DOWN_PAGE;						/* Scroll down a complete page.		*/
	fn_keys[5].meta_key = fn5_key;							/* Assign PF5.				*/
	fn_keys[6].list_function = LEFT_COL;						/* Display next available col on left.	*/
	fn_keys[6].meta_key = fn8_key;							/* Assign PF8.				*/
	fn_keys[7].list_function = RIGHT_COL;						/* Display next available col on right.	*/
	fn_keys[7].meta_key = fn9_key;							/* Assign PF9.				*/
	fn_keys[8].list_function = LEFT_PAGE;						/* Display next available col on left.	*/
	fn_keys[8].meta_key = fn10_key;							/* Assign PF10.				*/
	fn_keys[9].list_function = RIGHT_PAGE;						/* Display next page of col on right.	*/
	fn_keys[9].meta_key = fn11_key;							/* Assign PF11.				*/
	fn_keys[10].list_function = ALLOW_KEY;						/* Allow termination of input.		*/
	fn_keys[10].meta_key = fn16_key;						/* Assign PF16.				*/
	fn_keys[11].list_function = 0;							/* Define so can detect the end of the	*/
	fn_keys[11].meta_key = 0;							/*  list.				*/
	return;
}

static int get_func_val(key)								/* Return the actual integer value.	*/
long key;
{
	if (key == fn1_key) return(1);
	if (key == fn2_key) return(2);
	if (key == fn3_key) return(3);
	if (key == fn4_key) return(4);
	if (key == fn5_key) return(5);
	if (key == fn6_key) return(6);
	if (key == fn7_key) return(7);
	if (key == fn8_key) return(8);
	if (key == fn9_key) return(9);
	if (key == fn10_key) return(10);
	if (key == fn11_key) return(11);
	if (key == fn12_key) return(12);
	if (key == fn13_key) return(13);
	if (key == fn14_key) return(14);
	if (key == fn15_key) return(15);
	if (key == fn16_key) return(16);
	if (key == fn17_key) return(17);
	if (key == fn18_key) return(18);
	if (key == fn19_key) return(19);
	if (key == fn20_key) return(20);
	if (key == fn21_key) return(21);
	if (key == fn22_key) return(22);
	if (key == fn23_key) return(23);
	if (key == fn24_key) return(24);
	if (key == fn25_key) return(25);
	if (key == fn26_key) return(26);
	if (key == fn27_key) return(27);
	if (key == fn28_key) return(28);
	if (key == fn29_key) return(29);
	if (key == fn30_key) return(30);
	if (key == fn31_key) return(31);
	if (key == fn32_key) return(32);
	else return (0);
}
