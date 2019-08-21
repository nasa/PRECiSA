#include "daed_builtins.h"
int main()
{
  double t;
  double res;

  t = DBETWEEN_WITH_ULP(0.0, 999.0);

  SUBDIV_BEGIN(t, 20)

  res = t / (t+1);
  
  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
