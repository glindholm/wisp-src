/* UPPER ... convert a string from lower case to upper case									*/

UPPER(line,len)
char *line;								/* the line of text to convert				*/
long  *len;
{
	int i;

	char *tline;

	tline = line;

	for (i=0; i < *len; i++)
	{
		*tline = toupper(*tline);
		tline++;
	}
}
