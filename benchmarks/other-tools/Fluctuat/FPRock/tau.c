#include "daed_builtins.h"

int main(void)
{
  double s,v,t;
  double res;

  s = DBETWEEN_WITH_ULP(-200.0, 200.0);
  v = DBETWEEN_WITH_ULP(-200.0, 200.0);
  t = DBETWEEN_WITH_ULP(0.0, 200.0);

  if (s*v > 0.00000000008026290743146094)
      if (s*v - t*(v*v) > 0.000000003910145096597264)
        s*v;
      else
        if (s*v - t*(v*v) <= -0.000000003910145096597264)
          t*(v*v);
        else res = 0;
    else res = 0;

  DSENSITIVITY(res);

  return 0;
}
