#include "daed_builtins.h"
int main(void)
{
  double x;
  double res;

  x = DBETWEEN_WITH_ULP(-1.57079632679, 1.57079632679);

  SUBDIV_BEGIN(x, 20)

  res = x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0;

  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
