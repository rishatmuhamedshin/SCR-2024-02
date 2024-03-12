import scala.annotation.tailrec

object opt {


  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  /**
   * Реализован
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */
  /**
   * Реализован
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */

  /**
   * Реализован
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

  sealed trait Option[+T] {

    def isEmpty: Boolean = this match {
      case Some(_) => false
      case None => true
    }

    def printIfAny: Unit = this match {
      case Some(_) => print(_)
      case None => throw new Exception("get on empty option")
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty option")
    }

    def zip[A](option: Option[A]): Option[(T, A)] = (this, option) match {
      case (Some(v), Some(v2)) => Some((v, v2))
    }


    def filter[B](f: T => Boolean): Option[T] = this match {
      case Some(v) if f(v) => Some(v)
      case _ => None
    }

    //    def map[B](f: T => B): Option[B] = flatMap(t => Option(f(t)))

    //    def flatMap[B](f: T => Option[B]): Option[B] = this match {
    //      case Some(v) => f(v)
    //      case None => None
    //    }
    case class Some[V](v: V) extends Option[V]

    case object None extends Option[Nothing]
  }

  object Option {

  }


  object list {

    /**
     *
     * Реализовать односвязанный иммутабельный список List
     * Список имеет два случая:
     * Nil - пустой список
     * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
     */
    /**
     * Реализован
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     */

    /**
     *
     * Реализован
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     *
     */

    /**
     * Реализован
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */

    /**
     * Реализован
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */


    /**
     *Реализован
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    /**
     *Реализован
     * Написать функцию incList котрая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     */
    /**
     *Реализован
     * Написать функцию shoutString котрая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */

    sealed trait List[+T] {

      def ::[A >: T](head: A): List[A] = List.::(head, this)

      def mkString(string: String): String = this match {
        case Nil => ""
        case ::(head, Nil) => s"$head"
        case ::(head, tail) => s"$head + $string" + tail.mkString(string)
      }

      def reverse: List[T] = {
        @tailrec
        def reverseFool(list: List[T], acc: List[T]): List[T] = {
          list match {
            case Nil => acc
            case List.::(head,tail) => reverseFool(tail,head :: acc)
          }
        }
        reverseFool(this,List())
      }

      def map[B](f: T => B):List[T] = {
        @tailrec
        def loop(list: List[T], acc:List[T] =List[T]()):List[T] ={
          list match {
            case Nil => acc.reverse
            case List.::(head,tail) => loop(tail,f(head)::acc)
          }
        }
        loop(this)
      }


      def filter(f: T => Boolean): List[T] = {
        @tailrec
        def loop(list: List[T], acc: List[T] = List()):List[T] ={
          list match {
            case Nil => acc
            case List.::(head,tail) if f(head) => loop(tail, head :: acc)
          }
        }
        loop(this).reverse
      }

      //def incList(list: List[Int]):List[Int] = list.map(_ + 1)
      def incList(list:List[Int]): List[Int] = {
        @tailrec
        def loop(inputList: List[Int], acc: List[Int]): List[Int] = {
          inputList match {
            case List.Nil => acc
            case List.::(head,tail) => loop(tail, head + 1 :: acc)
          }
        }
        loop(list, List[Int]()).reverse
      }

      //def shoutString(list: List[String]):List[String] = list.map(_ + "!")

      def shoutString(list:List[String]): List[String] = {
        @tailrec
        def loop(inputList: List[String], acc: List[String]): List[String] = {
          inputList match {
            case List.Nil => acc
            case List.::(head,tail) => loop(tail, head + "!" :: acc)
          }
        }
        loop(list, List[String]()).reverse
      }
      object List{
        case class ::[A](head: A, tail: List[A]) extends List[A]
        case object Nil extends List[Nothing]

        def apply[A](v: A*): List[A] =
          if(v.isEmpty) Nil else ::(v.head, apply(v.tail:_*))
      }


    }

  }
}
