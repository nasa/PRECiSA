#include "daed_builtins.h"
int main(void)
{
  double x1, x2;
  double res;

  x1 = DBETWEEN_WITH_ULP(-5.0, 5.0);
  x2 = DBETWEEN_WITH_ULP(-5.0, 5.0);

  SUBDIV_BEGIN(x1, 20)
  SUBDIV_BEGIN(x2, 20)

  res = (x1*x1 + x2 - 11)* (x1*x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7);

  SUBDIV_END
  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
