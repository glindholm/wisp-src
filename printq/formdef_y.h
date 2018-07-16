
typedef union  {
    int kw;
    int num;
    char *string;
} YYSTYPE;
extern YYSTYPE formyylval;
# define NUMBER 257
# define EQUAL 258
# define COLON 259
# define VAR 260
# define STUFF 261
# define MODEL 262
# define LPP 263
# define STOCK 264
# define INIT 265
# define FILTER 266
