#include <stdlib.h>

void system_ (char* line, int linelen)
{
  char *com;
  com = malloc(linelen);
  if (com) {
    strncpy(com, line, linelen);
    line[linelen] = '\0';
    system(line);
    free(com);
  }
  
}
