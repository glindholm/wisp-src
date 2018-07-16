/*
**	File:		wait5min.c
**
**	Program:	wait5min
**
**	Function:	This program simply waits 5 minuntes then returns.  It is used to test side effects of SUBMIT 
**			when you need a background job that takes some time to complete.
**
**	Input:		None
**			
**
**	Output:		None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	05/18/92	Written by GSL
**
*/

main()
{
	unsigned	cnt;

	cnt = 5*60;
	while(cnt)
	{
		cnt = sleep(cnt);		/* Sleep 5 min (300 seconds) */
	}
	return;
}
