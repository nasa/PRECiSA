#include "daed_builtins.h"
int main(void)
{
  double v, w, r;
  double res;

  v = DBETWEEN_WITH_ULP(-4.5, -0.3);
  w = DBETWEEN_WITH_ULP(0.4, 0.9);
  r = DBETWEEN_WITH_ULP(3.8, 7.8);

  SUBDIV_BEGIN(v, 20)
  SUBDIV_BEGIN(w, 20)
  SUBDIV_BEGIN(r, 20)

  res = 3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5;

  SUBDIV_END
  SUBDIV_END
  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
