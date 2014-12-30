#include <unistd.h>
int loc_batch__()
/* int loc_batch_() */
{
#if 0
  if (isatty(0))
    printf ("Not in batch mode.\n");
  else
    printf("Is in batch mode.\n");
#endif
  return !isatty(0);
}
