#include "daed_builtins.h"

int main(void)
{
  double e;
  double res;

  e = DBETWEEN_WITH_ULP(0.0, 100.0);

  if (e < -4.999999999999992894572642398997)
      res = e * 2.25;
    else
      if (e >= 5.000000000000007105427357601003)
        if (e < 24.999999999999992894572642398997)
          res = (e - 5) * 1.1 + (5 * 2.25);
        else
          if (e >= 25.000000000000007105427357601003)
            res = (5 * 2.25) + 20 * 1.1;
          else
            res = 0;
      else
        res = 0;

  DSENSITIVITY(res);

  return 0;
}
