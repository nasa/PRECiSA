#include "daed_builtins.h"
int main(void)
{
  double x1, x2, x3;
  double res;

  x1 = DBETWEEN_WITH_ULP(-15.0, 15.0);
  x2 = DBETWEEN_WITH_ULP(-15.0, 15.0);
  x3 = DBETWEEN_WITH_ULP(-15.0, 15.0);

  res = 2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2;
  DSENSITIVITY(res);

  return 0;
}
