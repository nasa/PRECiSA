#include "daed_builtins.h"
int main(void)
{
  double x;
  double res;

  x = DBETWEEN_WITH_ULP(-2.0, 2.0);

  SUBDIV_BEGIN(x, 20)

  res = 0.954929658551372 * x -  0.12900613773279798*(x*x*x);

  SUBDIV_END

  DSENSITIVITY(res);

  return 0;
}
