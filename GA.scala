package GA

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Created by jnu on 2015/12/16.
 */

class GA {
  /**
   * 遗传算法主函数程序
   * @param popSize 种群规模
   * @param chormSize   维度
   * @param maxGen   最大迭代次数
   * @param crossRate   交叉概率
   * @param mutateRate   变异概率
   */
  def fun(popSize:Int,chormSize:Int,maxGen:Int,crossRate:Double,mutateRate:Double,xminmax:Array[Double]):Array[Double]= {
    var matrix_X = initializeFault(popSize, chormSize, xminmax) //生成候选解集X
    val fitness_result = new Array[Double](popSize)
    for (i <- 0 until popSize) {
      fitness_result(i) = (new Fitness).function(matrix_X(i))
    }
    var bestFitness = fitness_result(0)
    var bestMatrix = matrix_X(0)
    var trace = ArrayBuffer[Double]() //bestFitness的轨迹
    for (i <- 1 until popSize) {
      if (fitness_result(i) > bestFitness) {
        bestFitness = fitness_result(i)
        bestMatrix = matrix_X(i)
      }
    }

    for (gen <- 0 until maxGen) {
      matrix_X = select(matrix_X, fitness_result, popSize)
      matrix_X = cross(matrix_X, crossRate, chormSize, popSize, xminmax)
      matrix_X = mutation(matrix_X, mutateRate, chormSize, popSize, xminmax, gen, maxGen)
      for (i <- 0 until popSize) {
        fitness_result(i) = (new Fitness).function(matrix_X(i))
        if (fitness_result(i) > bestFitness) {
          bestFitness = fitness_result(i)
          bestMatrix = matrix_X(i)
        }
      }
      trace += bestFitness
      //bestMatrix.foreach(println)
      println(gen)
    }
    trace.toArray
  }
  /**
   *
   * @param popSize
   * @param chromSize  种群维度
   * @return
   */
  def initializeFault(popSize:Int, chromSize:Int,xminmax:Array[Double]):Array[Array[Double]]={
    val matrix_X=Array.ofDim[Double](popSize,chromSize)
    for (i<-0 until popSize){
      for (j<-0 until chromSize){
        matrix_X(i)(j)=Random.nextDouble()*(xminmax(2*j+1)-xminmax(2*j))
      }
      boundaryRepair(matrix_X(i),xminmax,chromSize)
    }
    matrix_X
  }
  //边界修正
  def boundaryRepair(matrix_X:Array[Double],xminmax:Array[Double],Dim:Int):Array[Double]={
    for (i<-0 until Dim){
      if (matrix_X(i)<xminmax(2*i)){
        matrix_X(i)=xminmax(2*i)
      }
      if (matrix_X(i)>xminmax(2*i+1)){
        matrix_X(i)=xminmax(2*i+1)
      }
    }
    matrix_X
  }

  //初始化种群，二进制
  /**
   *
   * @param popSize 种群数量
   * @param chromSize  染色体长度
   * @return  初始化后的种群
   */
  def initializeBinary(popSize:Int, chromSize:Int,xminmax:Array[Double]):Array[Array[Int]]={
    val matrix_X=Array.ofDim[Int](popSize,chromSize)
    for (i<-0 until popSize){
      for (j<-0 until chromSize){
        matrix_X(i)(j)=Random.nextDouble().round.toInt
      }
    }
    matrix_X
  }
  // 二进制转十进制

  //select选择
  /**
   *
   * @param matrix_X  种群
   * @return  选择的染色体,大小和原种群相同
   */
  def select(matrix_X:Array[Array[Double]],fitness_result:Array[Double],popSize:Int): Array[Array[Double]] ={
    val probability=fitness_result.map(_./(fitness_result.sum))
    //val index=ArrayBuffer[Int]()
    var j=0
    var pick=(new Random).nextDouble()
    for (i<- 0 until popSize){
      pick=(new Random).nextDouble()
      while (pick==0) pick=(new Random).nextDouble()
      while (j<=popSize){
        pick=pick-probability(j)
        if (pick<0){
          matrix_X(i)=matrix_X(j)
          j=popSize+1
        }
        else{
          j+=1
        }
      }
    }
    matrix_X
  }

  //交叉
  /**
   *
   * @param matrix_X
   * @param crossRate
   * @param chromSize 染色体长度，实数编码为种群维度
   * @param popSize
   * @param xmaxmin
   * @return
   */
  def cross(matrix_X:Array[Array[Double]],crossRate:Double,chromSize:Int,
             popSize:Int,xmaxmin:Array[Double]):Array[Array[Double]]={
    //var flag=false
    for (i<-0 until popSize/2){
      //随机选择两条染色体进行交叉
      //val position1=((new Random).nextDouble()*(chromSize-1)).ceil.toInt
      //val position2=((new Random).nextDouble()*(chromSize-1)).ceil.toInt
      var pick=(new Random).nextDouble()
      //if (pick==0) pick=(new Random).nextDouble()
      if (pick>crossRate){
        //while (flag.equals(false)){
          //随机选择交叉位置
          val position=((new Random).nextDouble()*(chromSize-1)).ceil.toInt
          pick=(new Random).nextDouble()
          matrix_X(2*i)(position)=pick*matrix_X(2*i+1)(position)+(1-pick)*matrix_X(2*i)(position)
          matrix_X(2*i+1)(position)=pick*matrix_X(2*i)(position)+(1-pick)*matrix_X(2*i+1)(position)
          matrix_X(2*i)=boundaryRepair2(matrix_X(2*i),xmaxmin,position)
          matrix_X(2*i+1)=boundaryRepair2(matrix_X(2*i+1),xmaxmin,position)
          //判断边界条件
//          val flag1=boundary(matrix_X(2*i),xmaxmin,position)
//          val flag2=boundary(matrix_X(2*i+1),xmaxmin,position)
//          if (flag1 && flag2)
//            flag=true
        //}
      }
    }
    matrix_X
  }
  //边界修正
  def boundaryRepair2(matrix_X:Array[Double],xminmax:Array[Double],i:Int):Array[Double]={
      val pick=(new Random).nextDouble()
    if (matrix_X(i)<xminmax(2*i)){
        matrix_X(i)=xminmax(2*i)+pick
      }
      if (matrix_X(i)>xminmax(2*i+1)){
        matrix_X(i)=xminmax(2*i+1)-pick
      }
    matrix_X
  }
  //边界判断
  def boundary(matrix_Xn:Array[Double],xmaxmin:Array[Double],position:Int):Boolean={
    if (matrix_Xn(position)>=xmaxmin(2*position) && matrix_Xn(position)<= xmaxmin(2*position+1)) {
     return true
    }
    else {
     return false
    }
  }
  //变异
  def mutation(matrix_X:Array[Array[Double]],mutationRate:Double,chromSize:Int,
               popSize:Int,xmaxmin:Array[Double],gen:Int,maxgen:Int):Array[Array[Double]]={
    for (i<-0 until popSize){
      //该轮循环是否变异
      var pick=(new Random).nextDouble()
      var flag=false
      if (pick>mutationRate){
        //while (flag.equals(false)){
          //变异位置
          pick=(new Random).nextDouble()
          val position = (pick *(chromSize-1)).ceil.toInt
          val matrix_Xn=matrix_X(i)(position)//变异的地方
          val v1=matrix_Xn-xmaxmin(2*position)
          val v2=xmaxmin(2*position+1)-matrix_Xn
          //开始变异
          pick=(new Random).nextDouble()
          if (pick>0.5){
            val delta=v2*(1-pick)*math.pow(1-gen/maxgen,2)
            matrix_X(i)(position)=matrix_Xn+delta
          }
          else{
            val delta=v2*(1-pick)*math.pow(1-gen/maxgen,2)
            matrix_X(i)(position)=matrix_Xn-delta
          }
          matrix_X(i)=boundaryRepair2(matrix_X(i),xmaxmin,position)
          //flag=boundary(matrix_X(i),xmaxmin,position)
        //}
      }
    }
    matrix_X
  }
}
