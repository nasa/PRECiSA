#include "daed_builtins.h"
int main(void)
{
  double x1, x2;
  double t, res;

  x1 = DBETWEEN_WITH_ULP(-5.0, 5.0);
  x2 = DBETWEEN_WITH_ULP(-20.0, 5.0);

  t = (3*x1*x1 + 2*x2 - x1);

  SUBDIV_BEGIN(x1, 20)
  SUBDIV_BEGIN(x2, 20)

  res = x1 + ((2*x1*(t/(x1*x1 + 1)) *
	       (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
	      (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
	      3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)));

  SUBDIV_END
  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
