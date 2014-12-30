#include <unistd.h>

void deletefile_(char *name, int namelen)
{
  int i;
  char *newname;

  newname = malloc(namelen+1);
  if (newname == NULL) return;
  strncpy(newname, name);
  for (i=namelen-1; i>=0 && newname[i]==' '; i--) ;
  newname[i+1] = '\0';
  unlink(newname);
  free(newname);
}
