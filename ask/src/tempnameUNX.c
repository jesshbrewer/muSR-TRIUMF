#include <stdio.h>
void tempname_(char *name, int namelen)
{
  char *tmp;
  tmp = tmpnam(NULL);
  strncpy(name, tmp, namelen);
  free(tmp);
  for (i=strlen(tmp); i<namelen; i++) name[i] = ' ';
}
