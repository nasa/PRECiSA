/* Original file:
https://github.com/malyzajko/rosa/blob/master/testcases/real-testcases/popl2014/ApproximationBenchmarks.scala
*/
import leon.real._
import RealOps._


object stynlinksi {

  def stynlinksi(x,y: Real): Real = {
    require(x >= -5 && x <= 5 && y>= -5 && y <= 5)

    if (y <= 0){
        if (x <= 0){
           res = -1.4717 + 2.83079*x + 0.786996*x*x + 2.83079*y - 1.07939*1e-16*x*y + 0.786996*y*y
        }
        else{
           res = -1.4717 - 2.33079*x + 0.786996*x*x + 2.83079*y + 9.1748*1e-16*x*y + 0.786996*y*y
        }
    }
    else {
        if (x <= 0){
            res = -1.4717 + 2.83079*x + 0.786996*x*x - 2.33079*y + 3.23816*1e-16*x*y + 0.786996*y*y
        }
        else{
            res = -1.4717 - 2.33079*x + 0.786996*x*x - 2.33079*y + 1.72702*1e-15*x*y + 0.786996*y*y
        }
    }

    } 
    ensuring(res => res +/- 1e-1)
}