#include<stdbool.h>

#define round(X) (double) (X)

/*@

axiomatic fp_funs {

  logic double Dadd(double X, double Y) = round(X+Y);

  logic double Dsub(double X, double Y) = round(X-Y);

  logic double Dmul(double X, double Y) = round(X*Y);

  logic double Dneg(double X) = round(0-X);

  logic double Dabs(double X) = round(X); //TODO define correctly

  logic double Ddiv(double X, double Y) = 
  X;
//  (Y != 0.0 ? round(X/Y) : 0.0) ;

// }

// axiomatic int_funs {

  logic integer Iadd(integer X, integer Y) = X+Y;

  logic integer Isub(integer X, integer Y) = X-Y;

  logic integer Imul(integer X, integer Y) = X*Y;

  logic integer Ineg(integer X) = 0-X;

  // logic integer Idiv(integer X, integer Y) = (Y != 0 ? X/Y : 0) ;

// }

// axiomatic error_bounds {

  logic real ulp_dp(real X) = \round_double(\Up,X)-\round_double(\Down,X);

  logic real errAdd_dp(real X, real E_X, real Y, real E_Y) 
  = E_X + E_Y + ulp_dp(\abs(X + Y) + E_X + E_Y)/2;

  logic real errSub_dp(real X, real E_X, real Y, real E_Y) 
  = E_X + E_Y + ulp_dp(\abs(X - Y) + E_X + E_Y)/2;

  logic real errMul_dp(real X, real E_X, real Y, real E_Y) 
  = \abs(X)*E_Y+\abs(Y)*E_X+E_X*E_Y + ulp_dp(\abs(X)*\abs(Y) + \abs(X)*E_Y + E_X*\abs(Y) + E_X*E_Y)/2;

  logic real errDiv_dp(real X, real E_X, real Y, real E_Y) 
  = ( ((Y*Y - E_Y*\abs(Y)) != 0 && (\abs(Y) - E_Y) !=0)? 
      (\abs(Y)*E_X + \abs(X)*E_Y) / (Y*Y - E_Y*\abs(Y)) + ulp_dp((\abs(X) + E_X) / (\abs(Y) - E_Y)) / 2
      : 0 );

  logic real errNeg_dp(real X, real E_X) 
  = E_X;

  logic real errAdd_i(integer X, real E_X, integer Y, real E_Y) 
  = E_X + E_Y;

  logic real errSub_i(integer X, real E_X, integer Y, real E_Y) 
  = E_X + E_Y;

}

*/

struct maybeInt {
bool isValid;
int value;
};

/*@ assigns \nothing;
ensures ! \result.isValid;
*/
struct maybeInt none () {
struct maybeInt result = { false, 0 };
return result;
}

/*@ assigns \nothing;
ensures \result.isValid;
ensures \result.value == val;
*/
struct maybeInt some (int val) {
struct maybeInt result = { true, val };
return result;
}

struct maybeFloat {
bool isValid;
float value;
};

/*@ assigns \nothing;
ensures ! \result.isValid;
*/
struct maybeFloat noneFloat () {
struct maybeFloat result = { false, 0 };
return result;
}

/*@ assigns \nothing;
ensures \result.isValid;
ensures \result.value == val;
*/
struct maybeFloat someFloat (float val) {
struct maybeFloat result = { true, val };
return result;
}

struct maybeDouble {
bool isValid;
double value;
};

/*@ assigns \nothing;
ensures ! \result.isValid;
*/
struct maybeDouble noneDouble () {
struct maybeDouble result = { false, 0 };
return result;
}

/*@ assigns \nothing;
ensures \result.isValid;
ensures \result.value == val;
*/
struct maybeDouble someDouble (double val) {
struct maybeDouble result = { true, val };
return result;
}

struct maybeBool {
    bool isValid;
    bool value;
};

/*@ assigns \nothing;
 ensures ! \result.isValid;
 */
struct maybeBool noneBool () {
    struct maybeBool result = { false, false };
    return result;
}

/*@ assigns \nothing;
 ensures \result.isValid;
 ensures \result.value == val;
 */
struct maybeBool someBool (bool val) {
    struct maybeBool result = { true, val };
    return result;
}
