package module1.homework

object LinearAlgebraOps {

  def sum(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length != v2.length) throw new Exception("Operation is not supported")
    val result = v1.clone
    for (x <- v1.indices) {
      result.update(x, v1(x) + v2(x))
    }
    result
  }

  def scale: PartialFunction[(Int, Array[Int]), Array[Int]] = {
    case x if x._2.length != 0 => {
      val result = x._2.clone
      for (i <- result.indices) {
        result.update(i, result(i) * x._1)
      }
      result
    }
  }



  def axpy(a: Int, v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length != v2.length) throw new Exception("Operation is not supported")

    val result = v1.clone
    for (i <- v1.indices) {
      result(i) = v1(i) * a + v2(i)
    }
    result
  }

  def axpy2(a: Int, v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length != v2.length) throw new Exception("Operation is not supported")
    sum(scale(a, v1), v2)
  }

  def main(args: Array[String]): Unit = {
    val v1 = Array(1, 2, 3)
    val v2 = Array(4, 5, 6)

    val a = 3

    val resultSum = sum(v1, v2)
    println("Result sum" + resultSum.mkString("Array(", ", ", ")"))

    val resultScale = scale(a, v1)
    println("Result scale: " + resultScale.mkString("Array(", ", ", ")"))

    val resultAxpy1 = axpy(a, v1, v2)
    println("Result axpy: " + resultAxpy1.mkString("Array(", ", ", ")"))


    val resultAxpy2 = axpy2(a, v1, v2)
    println("Result axpy2: " + resultAxpy2.mkString("Array(", ", ", ")"))
  }
}