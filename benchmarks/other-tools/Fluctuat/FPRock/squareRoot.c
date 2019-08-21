#include "daed_builtins.h"

int main(void)
{
  double i;
  double res;

  i = DBETWEEN_WITH_ULP(1.0, 2.0);

  if (i >= 2.00000000000000022204460492503136)
      res = 1.414213538169860839843750 * (1.0 + (i/2 - 1) * (0.5 - 0.125 * ( i/2 - 1)));
    else if (i < 1.99999999999999977795539507496864)
      res = 1 + (i - 1) * (0.5 + (i-1) * (-0.125 + (i - 1) * 0.0625));
    else res = 0;

  DSENSITIVITY(res);

  return 0;
}
