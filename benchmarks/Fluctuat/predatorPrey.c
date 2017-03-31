#include "daed_builtins.h"
int main(void)
{
  double r, K, x;
  double res;

  r = DBETWEEN_WITH_ULP(4.0, 4.0);
  K = DBETWEEN_WITH_ULP(1.11, 1.11);
  x = DBETWEEN_WITH_ULP(0.1, 0.3);

  res = (r*x*x) / (1 + (x/K)*(x/K));
  DSENSITIVITY(res);

  return 0;
}
