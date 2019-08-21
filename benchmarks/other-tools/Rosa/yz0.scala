/* Original file:
https://github.com/malyzajko/rosa/blob/master/testcases/real-testcases/popl2014/ApproximationBenchmarks.scala
*/
import leon.real._
import RealOps._


object cpr {

  def yz0(Lat: Real): Real = {
    require(Lat >= -90 && Lat <= 90)

     ((floor (131072 * (((lat / (360 / 60 - 0)) - floor(lat / (360 / 60 - 0))) / (360 / 60 - 0)) + 0.5)) / 131072 )
       - floor ((floor (131072 * (((lat / (360 / 60 - 0)) - floor(lat / (360 / 60 - 0))) / (360 / 60 - 0)) + 0.5)) / 131072 )

      } ensuring(res => res +/- 1e-1)

}