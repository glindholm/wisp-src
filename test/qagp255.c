/********************************************************************************************************************************/
/*																*/
/*  QAGP255.C															*/
/*																*/
/********************************************************************************************************************************/
/*  This is a test program to pass more than 255 parameters to GETPARM.								*/
/*																*/
/*  The idea is to pass two parameters to GETPARM:										*/
/*	1 - a pointer to an array of pointers to the parmaeters.								*/
/*	2 - a pointer to an integer that is the number of parameters passed.							*/
/*																*/
/*  GETPARM has been modified to check for two parameters.  If GETPARM only has twp parameters then the arguments will be	*/
/*  obtained by reading addresses from the array instead of using a va_args call.						*/
/********************************************************************************************************************************/

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>

#define DISPLAY_AND_READ	3
#define BOLD_TEXT		0x84
#define PLAIN_TEXT		0x8C

struct putpkeys {									/* PUTPARM keyword structure.		*/
	char	kw[9];										/* keyword			*/
	char	val[10];									/* value of string		*/
	int	pplen;										/* length of the string.	*/
	};

struct kwfield {									/* GETPARM keyword field structure.	*/
	char	kwtype;										/* Key field type		*/
	char	kwkey[9];									/*	keyword			*/
	char	kwval[10];									/*	value			*/
	int	kwlen;										/*	length of the string	*/
	char	kwrf;										/*	row flag		*/
       	int	kwrow;	  									/*	row value		*/
	char	kwcf;										/*	column flag		*/
	int	kwcol;										/*	column value		*/
	char	kwdtype;									/*	data type		*/
	};

struct txtfield	{									/* GETPARM text field structure.	*/
	char	tftype;										/* Text field type		*/
	char	tfval[80];									/*	value			*/
	int	tflen;										/*	length of the string	*/
	char	tfrf;										/*	row flag		*/
       	int	tfrow;	  									/*	row value		*/
	char	tfcf;										/*	column flag		*/
	int	tfcol;										/*	column value		*/
	};

QAGP255()
{											/* PUTPARM parameters.			*/
	char	func;										/*	function		*/
	int	u_cnt;										/*	usage count		*/
	int	kw_cnt, kwcnt_save;								/*	keyword count		*/
	struct	putpkeys ppkw[20];								/* 	struct for 20 keywords	*/
	char	pfkey;										/*	PF key value		*/
	char	label[9];									/*	PUTPARM label		*/
	char	reflbl[9];									/*	reference label		*/
	char	cleanup;									/*	cleanup option		*/
	int	ret_code;									/*	return code		*/
											/* GETPARM parameters			*/
	char	type[3];									/* the type of GETPARM 		*/
	char	form;										/* the form			*/
	char	prname[9];									/* the paremeter reference name	*/
	char	pf_ret;										/* the pfkey return field	*/
	char	messid[5];									/* the message id		*/
	char	messiss[7];									/* the message issuer		*/
	int	messlines;									/* the number of lines		*/
	char	messtxt[2][80];									/* the message text		*/
	int	messlen[2];									/* the text length		*/
	struct kwfield gpkw[20];								/* Structure of 20 key words	*/
	struct txtfield gptxt[10];								/* Structure of 10 text fields	*/
	char	pfktype;									/* Pf key type of argument	*/
	unsigned long	pfkey_mask;								/* the mask of possible pfkeys	*/
	char	enter_fl;									/* Indicates ENTER key specs.	*/

	int	num_params;									/* Number of parameters.	*/
	char	*addrs[264];									/* Array of ptrs to parameters.	*/

	char	*screen;								/* Parameters for VWANG call.		*/
	char	vwfunc, lines, term[2], no_mod[2];
	register int i, j;
	int	tl;										/* Temp - length of kw value.	*/

	if ((screen = (char *)malloc(1924)) == 0) mem_error(1924);
	WL_wsc_init(screen,0,0);

	WL_put_screen_text(screen, 1,20,PLAIN_TEXT,"*** GETPARM TEST PASSING 2 PARAMETERS ***");
	WL_put_screen_text(screen, 3, 5,PLAIN_TEXT,"This routine will first issue a PUTPARM.  It will then generate an");
	WL_put_screen_text(screen, 4, 5,PLAIN_TEXT,"array that contains the addresses of 263 parameters and a pointer");
	WL_put_screen_text(screen, 5, 5,PLAIN_TEXT,"to the number of parameters.");
	WL_put_screen_text(screen, 7, 5,PLAIN_TEXT,"Next, it will issue a call to GETPARM.");
	WL_put_screen_text(screen,10, 5,PLAIN_TEXT,"The test is to verify that GETPARM can read the array of pointers");
	WL_put_screen_text(screen,11, 5,PLAIN_TEXT,"and obtain the information correctly.");
	WL_put_screen_text(screen,23,18,PLAIN_TEXT,"(PUSH <RETURN> TO CONTINUE, PF16 TO EXIT.)");

	vwfunc = DISPLAY_AND_READ;								/* Set function to call vwang.	*/
	lines = 24;
	vwang_ws_erap(FULL_SCREEN);

	vwang(&vwfunc,screen,&lines,"0016X",term,no_mod);

	if (term[0] == '0' && term[1] == '0')							/* Continue with the test?	*/
	{
		func = 'D';
		u_cnt = 1;
		strcpy(prname,"TEST    ");
		kw_cnt = kwcnt_save = 20;
		for (i = 0; i < kwcnt_save; i++)					/* Load keyword info into structure.	*/
		{
			sprintf(ppkw[i].kw,"KW%d",i+1);
			tl = strlen(ppkw[i].kw);
			for (j = tl; j < 9; j++) ppkw[i].kw[j] = ' ';			/* Fill with spaces to length of 8.	*/
			sprintf(ppkw[i].val,"VALUE%d",i+1);
			ppkw[i].pplen = strlen(ppkw[i].val);
			WL_wswap(&ppkw[i].pplen);
		}
		pfkey = '@';
		strcpy(label,"T1      ");
		strcpy(reflbl,"        ");
		cleanup = 'C';
		WL_wswap(&u_cnt);								/* Need to swap so PUTPARM deals with 	*/
		WL_wswap(&kw_cnt);								/* properly.				*/

		WL_set_va_count(72);
		PUTPARM(&func, &u_cnt, prname, &kw_cnt,						/* Yes, so call PUTPARM.	*/
				ppkw[0].kw,  ppkw[0].val,  &ppkw[0].pplen,  ppkw[1].kw,  ppkw[1].val,  &ppkw[1].pplen,
				ppkw[2].kw,  ppkw[2].val,  &ppkw[2].pplen,  ppkw[3].kw,  ppkw[3].val,  &ppkw[3].pplen,
				ppkw[4].kw,  ppkw[4].val,  &ppkw[4].pplen,  ppkw[5].kw,  ppkw[5].val,  &ppkw[5].pplen,
				ppkw[6].kw,  ppkw[6].val,  &ppkw[6].pplen,  ppkw[7].kw,  ppkw[7].val,  &ppkw[7].pplen,
				ppkw[8].kw,  ppkw[8].val,  &ppkw[8].pplen,  ppkw[9].kw,  ppkw[9].val,  &ppkw[9].pplen,
				ppkw[10].kw, ppkw[10].val, &ppkw[10].pplen, ppkw[11].kw, ppkw[11].val, &ppkw[11].pplen,
				ppkw[12].kw, ppkw[12].val, &ppkw[12].pplen, ppkw[13].kw, ppkw[13].val, &ppkw[13].pplen,
				ppkw[14].kw, ppkw[14].val, &ppkw[14].pplen, ppkw[15].kw, ppkw[15].val, &ppkw[15].pplen,
				ppkw[16].kw, ppkw[16].val, &ppkw[16].pplen, ppkw[17].kw, ppkw[17].val, &ppkw[17].pplen,
				ppkw[18].kw, ppkw[18].val, &ppkw[18].pplen, ppkw[19].kw, ppkw[19].val, &ppkw[19].pplen,
				ppkw[20].kw, ppkw[20].val, &ppkw[20].pplen,
				&pfkey, label, reflbl, &cleanup, &ret_code);

		strcpy(type,"I ");							/* Init the GETPARM parameters.		*/
		form = 'A';
		strcpy(messid,"TEST ");
		strcpy(messiss,"GP255 ");
		messlines = 2;
		WL_wswap(&messlines);
		strcpy(messtxt[0],"This is a test message and is line one.");
		messlen[0] = strlen(messtxt[0]);
		WL_wswap(&messlen[0]);
		strcpy(messtxt[1],"This is the second line of the test message.");
		messlen[1] = strlen(messtxt[1]);
		WL_wswap(&messlen[1]);
		for (i = 0; i < kwcnt_save; i++)						/* Init the keyword parameters.	*/
		{
			gpkw[i].kwtype = 'K';
			strcpy(gpkw[i].kwkey,ppkw[i].kw);					/* Copy PUTPARM kw to GETPARM kw.*/
			strcpy(gpkw[i].kwval,"       ");
			gpkw[i].kwlen = strlen(gpkw[i].kwval);
			WL_wswap(&gpkw[i].kwlen);
			gpkw[i].kwrf = 'A';							/* Absolute row so 9-24 avail.	*/
			if	(i < 4)	 gpkw[i].kwrow = 15;
			else if (i <  8) gpkw[i].kwrow = 16;  
			else if (i < 12) gpkw[i].kwrow = 17;  
			else if (i < 16) gpkw[i].kwrow = 18;  
			else 		 gpkw[i].kwrow = 19;
			WL_wswap(&gpkw[i].kwrow);
			gpkw[i].kwcf = 'A';							/* Absolute row so 2-80 avail.	*/
			if	(i == 0 || i == 4 || i ==  8 || i == 12 || i == 16) gpkw[i].kwcol =  3;
			else if (i == 1 || i == 5 || i ==  9 || i == 13 || i == 17) gpkw[i].kwcol = 23;
			else if (i == 2 || i == 6 || i == 10 || i == 14 || i == 18) gpkw[i].kwcol = 43;
			else							    gpkw[i].kwcol = 63;
			WL_wswap(&gpkw[i].kwcol);
			gpkw[i].kwdtype = 'C';
		}
		for (i = 0; i < 9; i++)								/* Init the text parameters.	*/
		{
			gptxt[i].tftype = 'T';
			sprintf(gptxt[i].tfval,"This is a text field line %d.",i+1);
			gptxt[i].tflen = strlen(gptxt[i].tfval);
			WL_wswap(&gptxt[i].tflen);
			gptxt[i].tfrf = 'A';							/* Absolute row so 9-24 avail.	*/
			if	(i < 2) gptxt[i].tfrow = 9;
			else if (i < 4) gptxt[i].tfrow = 10;  
			else if (i < 6) gptxt[i].tfrow = 11;  
			else if (i < 8) gptxt[i].tfrow = 12;  
			else		gptxt[i].tfrow = 13;
			WL_wswap(&gptxt[i].tfrow);
			gptxt[i].tfcf = 'A';							/* Absolute row so 2-80 avail.	*/
			if   (i == 0 || i == 2 || i == 4 || i == 6 || i == 8) gptxt[i].tfcol =  2;
			else 						      gptxt[i].tfcol = 35;
			WL_wswap(&gptxt[i].tfcol);
		}
		gptxt[i].tftype = 'T';								/* Init the last text field.	*/
		sprintf(gptxt[i].tfval,"Change the information as appropriate and depress <RETURN>, <PF16> to exit.");
		gptxt[i].tflen = strlen(gptxt[i].tfval);
		WL_wswap(&gptxt[i].tflen);
		gptxt[i].tfrf = 'A';								/* Absolute row so 9-24 avail.	*/
		gptxt[i].tfrow = 23;
		WL_wswap(&gptxt[i].tfrow);
		gptxt[i].tfcf = 'A';								/* Absolute row so 2-80 avail.	*/
		gptxt[i].tfcol = 3;
		WL_wswap(&gptxt[i].tfcol);
		pfktype = 'P';
		pfkey_mask = 0x8A010000;							/* Enable 1, 5, 7, 16 PF keys.	*/
		WL_wswap(&pfkey_mask);
		enter_fl = 'E';

		num_params = 264;								/* Init the # parameters.	*/

		j = 0;
		addrs[j++] = type;								/* Init the array of addresses.	*/
		addrs[j++] = &form;
		addrs[j++] = prname;
		addrs[j++] = &pf_ret;
		addrs[j++] = messid;
		addrs[j++] = messiss;
		addrs[j++] = (char *)&messlines;
		addrs[j++] = messtxt[0];
		addrs[j++] = (char *)&messlen[0];
		addrs[j++] = messtxt[1];
		addrs[j++] = (char *)&messlen[1];
		for (i = 0; i < kwcnt_save; i++)						/* Init addr of keyword params.	*/ 
		{
			addrs[j++] = &gpkw[i].kwtype;
			addrs[j++] = gpkw[i].kwkey;
			addrs[j++] = gpkw[i].kwval;
			addrs[j++] = (char *)&gpkw[i].kwlen;
			addrs[j++] = &gpkw[i].kwrf;
			addrs[j++] = (char *)&gpkw[i].kwrow;
			addrs[j++] = &gpkw[i].kwcf;
			addrs[j++] = (char *)&gpkw[i].kwcol;
			addrs[j++] = &gpkw[i].kwdtype;
		}
		for (i = 0; i < 10; i++)							/* Init addr of text params.	*/
		{
			addrs[j++] = &gptxt[i].tftype;
			addrs[j++] = gptxt[i].tfval;
			addrs[j++] = (char *)&gptxt[i].tflen;
			addrs[j++] = &gptxt[i].tfrf;
			addrs[j++] = (char *)&gptxt[i].tfrow;
			addrs[j++] = &gptxt[i].tfcf;
			addrs[j++] = (char *)&gptxt[i].tfcol;
		}
		addrs[j++] = &pfktype;
		addrs[j++] = (char *)&pfkey_mask;
		addrs[j++] = &enter_fl;

		GETPARM2(addrs,num_params);							/* and call GETPARM.		*/

		WL_wsc_init(screen,0,0);

		WL_put_screen_text(screen, 1,23,PLAIN_TEXT,"*** GETPARM TEST RETURN VALUES ***");
		WL_put_screen_text(screen, 3,  2,PLAIN_TEXT," KW1");	WL_put_screen_text(screen, 3,  7,BOLD_TEXT,gpkw[0].kwval);
		WL_put_screen_text(screen, 3, 20,PLAIN_TEXT," KW2");	WL_put_screen_text(screen, 3, 25,BOLD_TEXT,gpkw[1].kwval);
		WL_put_screen_text(screen, 3, 40,PLAIN_TEXT," KW3");	WL_put_screen_text(screen, 3, 45,BOLD_TEXT,gpkw[2].kwval);
		WL_put_screen_text(screen, 3, 60,PLAIN_TEXT," KW4");	WL_put_screen_text(screen, 3, 65,BOLD_TEXT,gpkw[3].kwval);

		WL_put_screen_text(screen, 5,  2,PLAIN_TEXT," KW5");	WL_put_screen_text(screen, 5,  7,BOLD_TEXT,gpkw[4].kwval);
		WL_put_screen_text(screen, 5, 20,PLAIN_TEXT," KW6");	WL_put_screen_text(screen, 5, 25,BOLD_TEXT,gpkw[5].kwval);
		WL_put_screen_text(screen, 5, 40,PLAIN_TEXT," KW7");	WL_put_screen_text(screen, 5, 45,BOLD_TEXT,gpkw[6].kwval);
		WL_put_screen_text(screen, 5, 60,PLAIN_TEXT," KW8");	WL_put_screen_text(screen, 5, 65,BOLD_TEXT,gpkw[7].kwval);

		WL_put_screen_text(screen, 7,  2,PLAIN_TEXT," KW9");	WL_put_screen_text(screen, 7,  7,BOLD_TEXT,gpkw[8].kwval);
		WL_put_screen_text(screen, 7, 20,PLAIN_TEXT,"KW10");	WL_put_screen_text(screen, 7, 25,BOLD_TEXT,gpkw[9].kwval);
		WL_put_screen_text(screen, 7, 40,PLAIN_TEXT,"KW11");	WL_put_screen_text(screen, 7, 45,BOLD_TEXT,gpkw[10].kwval);
		WL_put_screen_text(screen, 7, 60,PLAIN_TEXT,"KW12");	WL_put_screen_text(screen, 7, 65,BOLD_TEXT,gpkw[11].kwval);

		WL_put_screen_text(screen, 9,  2,PLAIN_TEXT,"KW13");	WL_put_screen_text(screen, 9,  7,BOLD_TEXT,gpkw[12].kwval);
		WL_put_screen_text(screen, 9, 20,PLAIN_TEXT,"KW14");	WL_put_screen_text(screen, 9, 25,BOLD_TEXT,gpkw[13].kwval);
		WL_put_screen_text(screen, 9, 40,PLAIN_TEXT,"KW15");	WL_put_screen_text(screen, 9, 45,BOLD_TEXT,gpkw[14].kwval);
		WL_put_screen_text(screen, 9, 60,PLAIN_TEXT,"KW16");	WL_put_screen_text(screen, 9, 65,BOLD_TEXT,gpkw[15].kwval);

		WL_put_screen_text(screen,11,  2,PLAIN_TEXT,"KW17");	WL_put_screen_text(screen,11,  7,BOLD_TEXT,gpkw[16].kwval);
		WL_put_screen_text(screen,11, 20,PLAIN_TEXT,"KW18");	WL_put_screen_text(screen,11, 25,BOLD_TEXT,gpkw[17].kwval);
		WL_put_screen_text(screen,11, 40,PLAIN_TEXT,"KW19");	WL_put_screen_text(screen,11, 45,BOLD_TEXT,gpkw[18].kwval);
		WL_put_screen_text(screen,11, 60,PLAIN_TEXT,"KW20");	WL_put_screen_text(screen,11, 65,BOLD_TEXT,gpkw[19].kwval);

		WL_put_screen_text(screen,23,25,PLAIN_TEXT,"(Push <RETURN> to continue.)");

		vwfunc = DISPLAY_AND_READ;							/* Set function to call vwang.	*/
		lines = 24;
		vwang_ws_erap(FULL_SCREEN);

		vwang(&vwfunc,screen,&lines,"00",term,no_mod);
	}

	free(screen);
}

static mem_error(amt)									/* display no memory error.		*/
long amt;
{
	VL_vmove(18,0);
	VL_verase(TO_EOS);
	VL_vtext(18,0,BOLD,"MEMORY-ERROR not  enough to allocate %d.");
	VL_vexit();									/* Unconditional exit.			*/
	WL_wexit(8);
}
