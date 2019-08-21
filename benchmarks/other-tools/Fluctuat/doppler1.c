#include "daed_builtins.h"
int main(void)
{
  double u, v, T;
  double t1;
  double res;
  u = DBETWEEN_WITH_ULP(-100.0, 100.0);
  v = DBETWEEN_WITH_ULP(20.0, 20000.0);
  T = DBETWEEN_WITH_ULP(-30.0, 50.0);

  t1 = 331.4 + 0.6 * T;
  res = (-t1 * v) / ((t1 + u) * (t1 + u));
  DSENSITIVITY(res);

  return 0;
}
