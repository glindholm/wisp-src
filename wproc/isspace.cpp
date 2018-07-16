#if AIX
#include <ctype.h>
int isspace(char c) {
   return isspace((int) c);
}
#endif
