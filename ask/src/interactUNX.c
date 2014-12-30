#include <unistd.h>
int interact__()
{
#if 0
  if (isatty(0))
    printf ("Interactive mode.\n");
  else
    printf("Not in interactive mode.\n");
#endif
  return isatty(0);
}
