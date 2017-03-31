#include "daed_builtins.h"
int main(void)
{
  double u, v, T;
  double t1;
  double res;
  u = DBETWEEN_WITH_ULP(-125.0, 125.0);
  v = DBETWEEN_WITH_ULP(15.0, 25000.0);
  T = DBETWEEN_WITH_ULP(-40.0, 60.0);

  t1 = 331.4 + 0.6 * T;
  res = (-t1 * v) / ((t1 + u) * (t1 + u));
  DSENSITIVITY(res);

  return 0;
}
