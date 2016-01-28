package GA

/**
 * Created by jnu on 2015/12/16.
 */
object mainGA {
  def main(args: Array[String]) {
    val popSize=20
    //val chormSize=5//变量的维数
    val maxGen=2000
    val crossRate=0.5
    val mutateRate=0.3
    val upper=0.9*math.Pi
    val xminmax=Array[Double](0,upper,0,upper,0,upper,0,upper,0,upper)
    val chormSize=xminmax.length/2
    val result=(new GA).fun(popSize,chormSize,maxGen,crossRate,mutateRate,xminmax)
    result.map(1/_).foreach(println)
  }

}
