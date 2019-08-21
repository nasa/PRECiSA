#include "daed_builtins.h"

double fabs(double x){
  if( x < 0) 
    return -x;
  else 	return x;
}

int main(void)
{
  double P_i_x,P_i_y,S_x,S_y,BUFF;
  double res;

   P_i_x = DBETWEEN_WITH_ULP(1.0, 10.0);
   P_i_y = DBETWEEN_WITH_ULP(1.0, 10.0);
   S_x   = DBETWEEN_WITH_ULP(1.0, 10.0);
   S_y   = DBETWEEN_WITH_ULP(1.0, 10.0);
   BUFF  = DBETWEEN_WITH_ULP(0.1, 0.3);

  if (P_i_y >= S_y-BUFF && fabs(P_i_x-S_x)<BUFF) {
    res = P_i_x - (2.0*BUFF);
  } else {
    res = P_i_x;
  }
  
  DSENSITIVITY(res);

  return 0;
}
