/*  Terminates screen 'file'	*/

#define CLOSE_WORK_STATION	4

WSCLOSE()
{
	unsigned char function;
	
	function = CLOSE_WORK_STATION;							/* Set up for call to vwang.		*/
	vwang(&function);
}
