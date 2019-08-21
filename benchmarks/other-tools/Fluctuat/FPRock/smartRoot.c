#include "daed_builtins.h"

int main(void)
{
  double a,b,c;
  double res;

  a = DBETWEEN_WITH_ULP(1.0, 10.0);
  b = DBETWEEN_WITH_ULP(20.0, 30.0);
  c = DBETWEEN_WITH_ULP(1.0, 10.0);

  if (b > 0.000000000000001776356839400251)
      res = c*2 / - (b + sqrt(b * b - (a*c)*4 ));
    else
      if (b <= -0.000000000000001776356839400251)
        res = (sqrt(b * b - (a*c)*4 ) - b) / (a*2);
      else res = 0;

  DSENSITIVITY(res);

  return 0;
}
