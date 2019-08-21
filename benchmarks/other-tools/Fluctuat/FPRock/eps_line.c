#include "daed_builtins.h"

int main(void)
{
  double sx,sy,vx,vy;
  double res;

  sx = DBETWEEN_WITH_ULP(-2000.0, 2000.0);
  sy = DBETWEEN_WITH_ULP(-2000.0, 2000.0);
  vx = DBETWEEN_WITH_ULP(-2000.0, 2000.0);
  vy = DBETWEEN_WITH_ULP(-2000.0, 2000.0);

  if ((sx*vx + sy*vy) * (sx*vx - sy*vy) > 0.03335932642221452)
      res = 1;
    else
      if ((sx*vx + sy*vy) * (sx*vx - sy*vy) <= - 0.03335932642221452)
        res = -1;
      else res = 0;

  DSENSITIVITY(res);

  return 0;
}
