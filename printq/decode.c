#define TYPE_LIT 0
#define TYPE_SYM 1
#define TYPE_NUM 2
#define TYPE_MLT 3

#define C_QUOTE '"'
#define C_ESC '\\'
#define C_CTL '^'
#define C_PIPE '|'
#define C_NULL (char)0

#define S_START 0
#define S_LIT0 1
#define S_LIT 2
#define S_SYM 3
#define S_CTL0 4
#define S_ESC0 5
#define S_CTL 6
#define S_ESC 7
#define S_END 8
#define S_TERM 9
#define S_ERROR -1

#define RV_STATE_CNT 10

#define E_NORM  0
#define E_QUOTE 1
#define E_PIPE 2
#define E_ESC 3
#define E_CTL 4
#define E_NULL 5

#define RV_EVENT_CNT 6

int stab[RV_EVENT_CNT][RV_STATE_CNT] =
{     /*     START    LIT0    LIT      SYM     CTL0     ESC0    CTL     ESC     S_TERM*/
/*NORM*/  { S_SYM,   S_LIT,  S_LIT,  S_SYM,   S_CTL,   S_ESC,  S_LIT,  S_LIT,   0 },
/*QUOTE*/ { S_LIT0,  S_END,  S_END,  S_ERROR, S_ERROR, S_ESC,  S_END,  S_END,   0 },
/*OR*/    { S_START, S_LIT,  S_LIT,  S_END,   S_CTL,   S_ESC,  S_LIT,  S_LIT,   0 },
/*ESC*/   { S_ERROR, S_ESC0, S_ESC0, S_ERROR, S_CTL,   S_ESC,  S_ESC0, S_ESC0,  0 },
/*CIRC*/  { S_ERROR, S_CTL0, S_CTL0, S_ERROR, S_CTL,   S_ESC,  S_CTL0, S_CTL0,  0 },
/*NULL*/  { S_TERM,  S_ERROR,S_ERROR,S_END,   S_ERROR, S_ERROR,S_ERROR,S_ERROR, 0 },
};

decode(sub,val,type)
char *sub,*val;
int *type;
{
	int state=S_START;								/* machine state */
	int event, ch;									/* event and current input char  */
	int len=0;
	
       	while (state!=S_END)
	{
		ch = *val++;
		switch (ch)								/* decide what event */
		{
		      case C_QUOTE: 
			event=E_QUOTE;
			break;
		      case C_PIPE:
			event=E_PIPE;
			break;
		      case C_ESC:
			event=E_ESC;
			break;
		      case C_CTL:
			event=E_CTL;
			break;
		      case C_NULL:
			event=E_NULL;
			--val;
			break;
		      default:
			event=E_NORM;
			break;
		}
		
		state = stab[event][state];						/* find next state */
		switch (state)
		{
		      case S_LIT0:
			*type=TYPE_LIT;							/* init the type */
			break;
		      case S_SYM:
			*type=TYPE_SYM;							/* fall thru */
		      case S_LIT: 
			*sub++ = ch;							/* copy back */
			++len;
			break;
		      case S_ESC:							/* is it 'E'? if so, pass ESC */
			switch (ch)
			{
			      case 'E': case 'e':
				*sub++ = 27;
				break;
			      case 'n':
				*sub++ = '\n';
				break;
			      case 'l':
				*sub++ = '\012';
				break;
			      case 'r':
				*sub++ = '\015';
				break;
			      case 't':
				*sub++ = '\011';
				break;
			      case 'b':
				*sub++ = '\010';
				break;
			      case 'f':
				*sub++ = '\014';
				break;
			      case 's':
				*sub++ = ' ';
				break;
			      default:
				if (ch >= '0' && ch <= '9')
				{
					int tmp;
					
					tmp = (ch-0x30)*64;
					tmp += (*val++ -0x30)*8;
					tmp += (*val++ -0x30);
					*sub++ = tmp;
				}
				else
				  *sub++ = ch;
				break;
			}
			++len;
			break;
		      case S_CTL:
		      {
			      int tmp;
			      tmp= ch=='?'?127: toupper(ch) & 0xbf;			/* is it '?'? if so, pass DEL */
			      *sub++ = tmp?tmp:0x80;
			      ++len;
			      break;			
		      }
		      case S_END:
			continue;							/* end of field */
		      case S_TERM:
			return 0;							/* no more data */
			break;
		      default:
			break;
		}
	}
	*sub=(char)0;

	return len;
}
