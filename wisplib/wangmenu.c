/* MENU  interface for WANG vs programs to the VAX menu system									*/

menu(name,rval)
char *name;									/* The name of the menu file			*/
char *rval;									/* The return value				*/
{
	int i;
	char lname[16],lval[6];

	for (i=0; i<8; i++)
	{
		if ((lname[i] = name[i]) == ' ') break;				/* copy the name into a local buffer		*/
	}
	lname[i++] = '.';
	lname[i++] = 'M';
	lname[i++] = 'E';
	lname[i++] = 'N';
	lname[i++] = 'U';
	lname[i++] = '\0';

	menu_go(lname,lval);

	rval[0] = lval[0];							/* copy the return value			*/
	rval[1] = lval[1];
}
