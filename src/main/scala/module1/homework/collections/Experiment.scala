package module1.homework.collections

import scala.util.Random

class Experiment {

  val bucket: List[Int] = List(1,1,1,0,0,0)
  def pickUp(): Boolean = bucket.map(_ == 1).apply(new Random().nextInt(bucket.length))


}

object Main {

  def main(args: Array[String]): Unit = {
    val listExperiment:List[Boolean] = (0 to 10000).map(_ => new Experiment().pickUp()).toList

    val countTrue:Double = listExperiment.count(x => x).toDouble
    val resultExperiment:Double = countTrue / listExperiment.length
    println(s"$countTrue  / ${listExperiment.length} = ${resultExperiment * 100}" )
  }



}
