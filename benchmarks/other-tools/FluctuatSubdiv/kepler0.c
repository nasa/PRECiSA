#include "daed_builtins.h"
int main(void)
{
  double x1, x2, x3, x4, x5, x6;
  double res;

  x1 = DBETWEEN_WITH_ULP(4.0, 6.36);
  x2 = DBETWEEN_WITH_ULP(4.0, 6.36);
  x3 = DBETWEEN_WITH_ULP(4.0, 6.36);
  x4 = DBETWEEN_WITH_ULP(4.0, 6.36);
  x5 = DBETWEEN_WITH_ULP(4.0, 6.36);
  x6 = DBETWEEN_WITH_ULP(4.0, 6.36);

  SUBDIV_BEGIN(x1, 5)
  SUBDIV_BEGIN(x2, 5)
  SUBDIV_BEGIN(x3, 5)
  SUBDIV_BEGIN(x4, 5)
  SUBDIV_BEGIN(x5, 5)
  SUBDIV_BEGIN(x6, 5)

  res = x2 * x5 + x3 * x6 - x2 * x3 - x5 * x6 + x1 * (-x1 + x2 + x3 - x4 + x5 + x6);

  SUBDIV_END
  SUBDIV_END
  SUBDIV_END
  SUBDIV_END
  SUBDIV_END
  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
