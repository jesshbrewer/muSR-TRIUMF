#include <unistd.h>
#include <math.h>

void sleep_( double *s )
{
  /* printf("sleepUNX: sleeping for %lf seconds\n", *s); */
  sleep( (int)floor(fabs(*s)+0.5));
}
