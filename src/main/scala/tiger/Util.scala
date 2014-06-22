package tiger

import scala.annotation.tailrec
import scala.util.{Success, Failure, Try}

/**
 * User: jose
 * Date: 10/5/13
 * Time: 9:21 PM
 */
object Util {
  def tsort[A](edges: Traversable[(A, A)]): Try[Iterable[A]] = {

    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Try[Iterable[A]] = {
      val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) Success(done) else Failure(new RuntimeException(hasPreds.toString()))
      } else {
        val found = noPreds.map { _._1 }
        tsort(hasPreds.mapValues { _ -- found }, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }
}
