import leon.real._
import RealOps._

object FPRoCK {

  //Robustness Analysis of Finite Precision Implementations, arxiv extended version,
  // E. Goubault, S. Putot
  def simpleInterpolator(e: Real): Real = {
    require(0.0 <= e && e <= 100.0)

    val const: Real = 2.25  // else this would be pre-evaluated by the parser
    val r2: Real = 5 * const
    val const2: Real = 1.1
    val r3: Real = r2 + 20 * const2  //same here
    val zero: Real = 0.0

    if (e < -4.999999999999992894572642398997)
      e * 2.25
    else
      if (e >= 5.000000000000007105427357601003)
        if (e < 24.999999999999992894572642398997)
          (e - 5) * 1.1 + r2
        else
          if (e >= 25.000000000000007105427357601003)
            r3
          else
            zero
      else 
        zero
  }

  def squareRoot(i: Real): Real = {
    require(1.0 <= i && i <= 2.0)

    val sqrt2: Real = 1.414213538169860839843750
    val zero: Real = 0.0

    if (i >= 2.00000000000000022204460492503136)
      sqrt2 * (1.0 + (i/2 - 1) * (0.5 - 0.125 * ( i/2 - 1)))
    else if (i < 1.99999999999999977795539507496864)
      1 + (i - 1) * (0.5 + (i-1) * (-0.125 + (i - 1) * 0.0625))
    else zero
  } 


  def cubicSpline(x: Real): Real = {
    require(-2 <= x && x <= 2)
    val zero: Real = 0.0

    if (x <= -1.00000000000000022204460492503136) {
      0.25 * (x + 2)* (x + 2)* (x + 2)
    } else if (x <= -0.00000000000000022204460492503136) {
      0.25*(-3*x*x*x - 6*x*x +4)
    } else if (x <= 0.9999999999999998) {
      0.25*(3*x*x*x - 6*x*x +4)
    } else if (x > 1.00000000000000022204460492503136){
      0.25*(2 - x)*(2 - x)*(2 - x)
    } else
      zero                                                               
  }  

 /* def cubicSpline(x: Real): Real = {
    require(-2 <= x && x <= 2)

    if (x <= -1) {
      0.25 * (x + 2)* (x + 2)* (x + 2)
    } else if (x <= 0) {
      0.25*(-3*x*x*x - 6*x*x +4)
    } else if (x <= 1) {
      0.25*(3*x*x*x - 6*x*x +4)
    } else {
      0.25*(2 - x)*(2 - x)*(2 - x)
    }
  }  
*/

  def linearFit(x: Real, y: Real): Real = {
    require(-4 <= x && x <= 4 && -4 <= y && y <= 4)
    val zero: Real = 0.0

    if (x <= -0.0000000000000004440892098500627) {
      if (y <= -0.0000000000000004440892098500627) {
        0.0958099 - 0.0557219*x - 0.0557219*y
      } else
      if (y > 0.0000000000000004440892098500627) {
        -0.0958099 + 0.0557219*x - 0.0557219*y
      } else zero

    } else if (x > 0.0000000000000004440892098500627) {
      if (y <= -0.0000000000000004440892098500627) {
        -0.0958099 - 0.0557219*x + 0.0557219*y
      } else 
      if (y > 0.0000000000000004440892098500627) {
        0.0958099 + 0.0557219*x + 0.0557219*y  
      } else zero
    }
    else zero
  }


  def quadraticFit(x: Real, y: Real): Real = {
    require(-4 <= x && x <= 4 && -4 <= y && y <= 4)
    val zero: Real = 0.0

    if (x <= -0.0000000000000004440892098500627) {
      if (y <= -0.0000000000000004440892098500627) {
        -0.0495178 - 0.188656*x - 0.0502969*x*x - 0.188656*y + 0.0384002*x*y - 0.0502969*y*y
      } else
      if (y > 0.0000000000000004440892098500627) {
        0.0495178 + 0.188656*x + 0.0502969*x*x - 0.188656*y + 0.0384002*x*y + 0.0502969*y*y
      } else zero

    } else if (x > 0.0000000000000004440892098500627){
      if (y <= -0.0000000000000004440892098500627) {
        0.0495178 - 0.188656*x + 0.0502969*x*x + 0.188656*y + 0.0384002*x*y + 0.0502969*y*y
      } else
      if (y > 0.0000000000000004440892098500627){
        -0.0495178 + 0.188656*x - 0.0502969*x*x + 0.188656*y + 0.0384002*x*y - 0.0502969*y*y
      } else zero
    }
    else zero
  }


  def styblinski(x: Real, y: Real): Real = {
    require(-5 <= x && x <= 5 && -5 <= y && y <= 5)
    val zero: Real = 0.0
    
    if (y <= -0.0000000000000004440892098500627)
      if (x <= -0.0000000000000004440892098500627)
        -1.4717 + 2.83079*x + 0.786996*x*x + 2.83079*y - 1.07939*1e-16*x*y + 0.786996*y*y
      else 
        if (x > 0.0000000000000004440892098500627)
          -1.4717 - 2.33079*x + 0.786996*x*x + 2.83079*y + 9.1748*1e-16*x*y + 0.786996*y*y
        else
          zero
    else if (y > 0.0000000000000004440892098500627)
      if (x <= -0.0000000000000004440892098500627)
        -1.4717 + 2.83079*x + 0.786996*x*x - 2.33079*y + 3.23816*1e-16*x*y + 0.786996*y*y
      else 
        if (x > 0.0000000000000004440892098500627)
          -1.4717 - 2.33079*x + 0.786996*x*x - 2.33079*y + 1.72702*1e-15*x*y + 0.786996*y*y
        else
          zero
    else zero
  }

 
  // w= 1.0, step = 0.01
  def jetApprox(x: Real, y: Real): Real = {
    require(-5 <= x && x <= 5 && -5 <= y && y <= 5)
    val e: Real = 0.0000000000000004440892098500627
    val zero: Real = 0.0

    if (y <= -e)
      if (x <= -e)
        -0.0939097 + 0.313122*x + 0.20583 *x*x + 0.00111573*y - 0.00514265*x*y + 
          0.00380476 *x*x*y - 0.000567088 *y*y + 0.000568992*x*y*y

      else 
        if (x > e)
        -0.0758695 - 0.300328*x + 0.168573 *x*x + 0.00633419*y - 0.00910356*x*y +
          0.0125128 *x*x*y + 0.000567088 *y*y + 0.000568992*x*y*y
        else zero
    else
        if (y > e)
          if (x <= -e)
            -0.0939097 + 0.313122*x + 0.20583 *x*x + 0.00604201*y + 0.0147615*x*y + 
            0.0087808 *x*x*y - 0.000567088 *y*y + 0.000568992*x*y*y
          else
            if (x > e)
              -0.0758695 - 0.300328*x + 0.168573 *x*x + 0.00140791*y + 0.0108006*x*y +
              0.00753679 *x*x*y + 0.000567088 *y*y + 0.000568992*x*y*y
            else zero
        else zero
  } 


  def los(x: Real, y: Real, d:Real): Real = {
    require(-20000 <= x && x <= 20000 && -20000 <= y && y <= 20000 && -20000 <= d && d <= 20000)
    val zero: Real = 0.0
    val one: Real = 1.0
    val negone: Real = -1.0
    val e: Real = 0.00004290474734427739

    if (sqrt(x*x + y*y) - d < - e)
        one
    else
      if (sqrt(x*x + y*y) - d >= e)
        negone
      else 
        zero
  } ensuring (res => res +/- 1e-1)


  def tau(s: Real, v: Real, t:Real): Real = {
    require(-200 <= s && s <= 2000 && -200 <= v && v <= 200 && 0 <= t && t <= 200)
    val e1: Real = 0.00000000008026290743146094
    val e2: Real = 0.000000003910145096597264
    val zero: Real = 0.0

    if (s*v > e1)
      if (s*v - t*(v*v) > e2)
        s*v
      else
        if (s*v - t*(v*v) <= -e2)
          t*(v*v)
        else zero
    else zero
  }


  def smartRoot(a: Real, b: Real, c:Real): Real = {
    require(1 <= a && a <= 10 && 20 <= b && b <= 30 && 1 <= c && c <= 10)
    val e: Real = 0.000000000000001776356839400251
    val zero: Real = 0.0

    if (b > e)
      c*2 / - (b + sqrt(b * b - (a*c)*4 ))
    else
      if (b <= -e)
          (sqrt(b * b - (a*c)*4 ) - b) / (a*2)
      else zero
             
  }


  def eps_line(sx: Real, sy: Real, vx:Real, vy:Real): Real = {
    require(-2000 <= sx && sx <= 2000 && -2000 <= sy && sy <= 2000  && -2000 <= vx && vx <= 2000 && -2000 <= vy && vy <= 2000)
    val e: Real = 0.03335932642221452
    val zero: Real = 0.0
    val one: Real = 1.0
    val negone: Real = -1.0

    if ((sx*vx + sy*vy) * (sx*vx - sy*vy) > e)
      one
    else
      if ((sx*vx + sy*vy) * (sx*vx - sy*vy) <= -e)
        negone
      else zero
  }


  def sign(x: Real): Real = {
    require(-20000 <= x && x <= 20000)
    val e: Real = 0.0000000000018189894035458565
    val zero: Real = 0.0
    val one: Real = 1.0
    val negone: Real = -1.0

    if (x > e)
      one
    else
      if (x <= -e)
        negone
      else zero
  }

}
