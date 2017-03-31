#include "daed_builtins.h"
int main()
{
  double t;
  double res;

  t = DBETWEEN_WITH_ULP(0.0, 999.0);

  res = t / (t+1);
  DSENSITIVITY(res);

  return 0;
}
