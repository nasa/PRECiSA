#include "daed_builtins.h"

int main(void)
{
  double x;
  double res;

  x = DBETWEEN_WITH_ULP(-2.0, 2.0);

  if (x <= -1.00000000000000022204460492503136) {
      res = 0.25 * (x + 2)* (x + 2)* (x + 2);
    } else if (x <= -0.00000000000000022204460492503136) {
      res = 0.25*(-3*x*x*x - 6*x*x +4);
    } else if (x <= 0.9999999999999998) {
      res = 0.25*(3*x*x*x - 6*x*x +4);
    } else if (x > 1.00000000000000022204460492503136){
      res = 0.25*(2 - x)*(2 - x)*(2 - x);
    } else
      res = 0;

  DSENSITIVITY(res);

  return 0;
}
