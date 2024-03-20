package module1.homework.collections

import scala.util.Random

class Experiment {

  val bucket: List[Int] = List.fill(3)(1) ::: List.fill(3)(0)

  val pickUpTwice: Boolean = {
    val firstPick: Boolean = bucket(new Random().nextInt(bucket.length)) == 1
    var list: List[Int] = List.empty[Int]

    if (firstPick) {
      list = bucket.slice(1, 6)
    } else {
      list = bucket.slice(0,5)
    }
    val secondPick: Boolean = list(new Random().nextInt(list.length)) == 1

    if (secondPick || firstPick) true
    else false
  }


}

object Main {

  def main(args: Array[String]): Unit = {
    val result: Int = List.fill(10000)(new Experiment().pickUpTwice).count(x => x)
    println(s"Result =  ${(result.toDouble / 100)} %")

  }


}
