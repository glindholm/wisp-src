
int strpos(src,srch) unsigned char *src,*srch;						/* Search a string for a string.	*/
{
	int i,j0;
	j0 = strlen(src) - strlen(srch) + 1;						/* Stop search when j0 = 0.		*/
	i = 0;
	while (*src && (j0 > 0))
	{
		if (!memcmp(srch,src,strlen(srch))) return(i);				/* If compare is good, return.		*/
		src++;
		i++;
		j0--;
	}
	return(-1);
}

