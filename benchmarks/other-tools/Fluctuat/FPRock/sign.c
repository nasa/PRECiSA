#include "daed_builtins.h"

int main(void)
{
  double x;
  double res;

  x = DBETWEEN_WITH_ULP(-20000.0, 20000.0);

  if (x > 0.0000000000018189894035458565)
      res = 1;
    else
      if (x <= - 0.0000000000018189894035458565)
        res = -1;
      else res = 0;

  DSENSITIVITY(res);

  return 0;
}
