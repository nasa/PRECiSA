/* Original file:
https://github.com/malyzajko/rosa/blob/master/testcases/real-testcases/popl2014/ApproximationBenchmarks.scala
*/
import leon.real._
import RealOps._


object hartman3 {

  def hartman3(x1,x2,x3: Real): Real = {
    require(x1 >= 0 && x1 <= 1 && x2 >= 0 && x2 <= 1 && x3 >= 0 && x3 <= 1)
      e1 = 3.0 * ((x1 - 0.3689) * (x1 - 0.3689)) + 10.0 * ((x2 - 0.117) * (x2 - 0.117))
            + 30.0 * ((x3 - 0.2673) * (x3 - 0.2673))
      e2 = 0.1 * ((x1 - 0.4699) * (x1 - 0.4699)) + 10.0 * ((x2 - 0.4387) * (x2 - 0.4387))
                + 35.0 * ((x3 - 0.747) * (x3 - 0.747))
      e3 = 3.0 * ((x1 - 0.1091) * (x1 - 0.1091)) + 10.0 * ((x2 - 0.8732) * (x2 - 0.8732))
                + 30.0 * ((x3 - 0.5547) * (x3 - 0.5547))
      e4 = 0.1 * ((x1 - 0.03815) * (x1 - 0.03815)) + 10.0 * ((x2 - 0.5743) * (x2 - 0.5743))
                + 35.0 * ((x3 - 0.8828) * (x3 - 0.8828))

      -(1.0 * exp(-e1) + 1.2 * exp(-e2) + 3.0 * exp(-e3) + 3.2 * exp(-e4))

      } ensuring(res => res +/- 1e-1)

}