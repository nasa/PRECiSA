/* Original file:
https://github.com/malyzajko/rosa/blob/master/testcases/real-testcases/popl2014/ApproximationBenchmarks.scala
*/
import leon.real._
import RealOps._

object polycarp {

  def polycarp(Lat: Real): Real = {
    require(P_i_x >= 1 && P_i_x <= 10 && P_i_y >= 1 && P_i_y <= 10 && S_x >= 1 && S_x <= 10 && S_y >= 1 && S_y <= 10 && BUFF >= 0.1 && BUFF <= 0.3)

      if (P_i_y >= (S_y - BUFF) && abs(P_i_x - S_x)<BUFF) {
          P_i_x - (2 * BUFF)
      } else {
          P_i_x
      }

      } ensuring(res => res +/- 1e-1)

}