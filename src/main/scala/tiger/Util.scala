package tiger

import scala.annotation.tailrec
import scala.collection.mutable
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

  var nro = 0

  def printgraph(r: RegisterAllocation, name:String): Unit = {

    val c = Map ( 0 -> "red", 1 -> "blue", 2 -> "yellow", 3 -> "green")

    println(s"graph $name {")

    for ( (n, color) <- r.color )
      println(s"$n [style=filled, fillcolor=${c(color)}]")



    def remove(l:mutable.HashSet[(Temp.Temp, Temp.Temp)]) = l.map {
      case (i, j) if i > j => (j, i)
      case x => x
    }

    val nodes = remove(r.adjSet)
    val moves = remove(r.workListMoves.map ( x => (x.dst, x.src) ))


    val d = "[style=\"dashed\"]"

    for ( (u,v) <- nodes ) {
      println(s"$u -- $v")
    }

    for ( (u,v) <- moves ) {
      println(s"$u -- $v $d")
    }


    println("label=\"graphnro: " + nro + "\"")

    nro = nro + 1

    println("}")
  }
}
