package module1.homework.futures

import module1.homework.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val result = futures.foldRight(Future.successful(List[A](), List[Throwable]())) {
      (future, lists) =>
        lists.flatMap {
          l =>
            future.map { f => (f :: l._1, l._2) }
              .recover {
                case ex => (l._1, ex :: l._2)
              }
        }
    }
    result

  }
}
