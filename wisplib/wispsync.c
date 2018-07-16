/*
**	WISPSYNC	This is a special wisp COBOL interface to vsynch() that is to be inserted into wang cobol
**			programs to let wisp and video known that the screen has been changed behind its back, so
**			that it can re-synchronize it's video maps.
*/

WISPSYNC()
{
	vsynch();
}



/*
**	WISPSHUT	This is a special wisp COBOL interface to vexit() that is to be inserted into wang cobol
**			programs to let wisp and video known that the screen is going to be altered by a non-WISP
**			routine, so it should turn off video and put the screen back into a known state.
*/

WISPSHUT()
{
	vexit();
}

