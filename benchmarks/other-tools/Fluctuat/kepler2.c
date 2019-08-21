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

  res = x1 * x4 * (-x1 + x2 + x3 - x4 + x5 + x6)
    + x2 * x5 * (x1 - x2 + x3 + x4 - x5 + x6)
    + x3 * x6 * (x1 + x2 - x3 + x4 + x5 - x6)
    - x2 * x3 * x4 - x1 * x3 * x5
    - x1 * x2 * x6 - x4 * x5 * x6;

  DSENSITIVITY(res);

  return 0;
}
