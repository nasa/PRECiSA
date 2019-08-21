#include "daed_builtins.h"

int main(void)
{
  double x,y,d;
  double res;

  x = DBETWEEN_WITH_ULP(-20000.0, 20000.0);
  y = DBETWEEN_WITH_ULP(-20000.0, 20000.0);
  d = DBETWEEN_WITH_ULP(-20000.0, 20000.0);

  if (sqrt(x*x + y*y) - d < - 0.00004290474734427739)
        res = 1;
    else
      if (sqrt(x*x + y*y) - d >= 0.00004290474734427739)
        res = -1;
      else
        res = 0;

  DSENSITIVITY(res);

  return 0;
}
