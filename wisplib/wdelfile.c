/*
	wdelfile	- delete one file (must work for LPI COBOL)
*/

#ifdef unix


wdelfile(wfile,wlib,wvol)
char	wfile[8];
char	wlib[8];
char	wvol[6];
{
	long	mode;
	char	name[80];
	char	prname[8];
	char	*ptr;
	int	i;

	mode=0;
	memcpy(prname,"SCRATCH ",8);
	
	ptr = (char *) wfname(mode,wfile,wlib,wvol,name,prname);
	*ptr = 0;

	if ( 0 == access(name,00) )
	{
		unlink(name);
		return;
	}

	for( i=strlen(name)-1; i>0;i-- )
	{
		if ( name[i] == '/' ) break;
		if ( name[i] == '.' ) return;
	}

	strcat(name,".dat");
	if ( 0 == access(name,00) )
	{
		unlink(name);
	}

	memcpy(&name[strlen(name) - 4],".idx",4);
	if ( 0 == access(name,00) )
	{
		unlink(name);
	}

}

#endif
