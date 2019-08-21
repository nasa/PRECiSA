#include "daed_builtins.h"
int main(void)
{
  double u, v, T;
  double t1;
  double res;

  u = DBETWEEN_WITH_ULP(-30.0, 120.0);
  v = DBETWEEN_WITH_ULP(320.0, 20300.0);
  T = DBETWEEN_WITH_ULP(-50.0, 30.0);

  SUBDIV_BEGIN(T, 20)
  SUBDIV_BEGIN(v, 20)
  SUBDIV_BEGIN(u, 20)

  t1 = 331.4 + 0.6 * T;
  res = (-t1 * v) / ((t1 + u) * (t1 + u));

  SUBDIV_END
  SUBDIV_END
  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
