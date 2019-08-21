#include "daed_builtins.h"

int main(void)
{
  double x;

  x = DBETWEEN_WITH_ULP(0.0, 5.0);
    
  int i;

  for(i=0;i<=10;i++) {
    x = (double)i * x;
  }
  
  DSENSITIVITY(x);

  return 0;
}
