#include "daed_builtins.h"
int main(void)
{
  double lat, tmp;
  double res;

  lat = DBETWEEN_WITH_ULP(-90.0, 90.0);

  tmp = 360.0/(60.0-0.0);
  res = (int)(((lat - tmp * (int)(lat/tmp))/ tmp) * 131072.0 + 0.5);
  
  DSENSITIVITY(res);

  return 0;
}
