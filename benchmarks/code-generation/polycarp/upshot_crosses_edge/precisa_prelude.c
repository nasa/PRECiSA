#include<stdbool.h>

/*@
axiomatic error_bounds {
  logic real ulp(real X);

  logic real errAdd( real X, real Y, real E_X, real E_Y) 
  = E_X + E_Y + ulp(\abs(X + Y) + E_X + E_Y)/2;

  logic real errSub( real X, real Y, real E_X, real E_Y) 
  = E_X + E_Y + ulp(\abs(X - Y) + E_X + E_Y)/2;

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
    double value;
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
