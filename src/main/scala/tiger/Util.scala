package tiger

import tiger.Tree._

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

  def printsmt(stm:Stm): String = {

    var i = 0

    val links = mutable.ListBuffer.empty[(String, String)]

    val strings = mutable.ListBuffer.empty[String]


    def node(label:String): String = {
      i += 1
      val node = s"node$i"
      strings += s"""\t$node[label = "$label"]"""
      node
    }


    def exp(parent:String, ex:Expr):Unit = ex match {
      case BINOP(o, l, r) => {

        val currentNode = node(s"BINOP $o")

        links += parent -> currentNode

        exp(currentNode, l)
        exp(currentNode, r)

      }
      case MEM(e) => {
        val currentNode = node("MEM")

        links += parent -> currentNode

        exp(currentNode, e)
      }
      case CALL(NAME(n), args) => {
        val currentNode = node(s"CALL($n)")

        links += parent -> currentNode
        args foreach { exp(currentNode, _) }

      }
      case ESEQ(s, e) => {

        val currentNode = node("ESEQ")

        links += parent -> currentNode

        exp(currentNode, e)
        doit(currentNode, s)
      }

      case _ => {

        val currentNode = node(ex.toString)
        links += parent -> currentNode
      }

    }


    def doit(parent:String, ast:Stm):Unit = ast match {
      case MOVE(d, s) => {
        val currentNode = node("MOVE")

        links += parent -> currentNode

        exp(currentNode, d)
        exp(currentNode, s)
      }
      case EXP(e) => {
        val currentNode = node("EXP")

        links += parent -> currentNode
        exp(currentNode, e)
      }
      case JUMP(e, labs) => {
        val currentNode = node(s"JUMP( $e | $labs)")

        links += parent -> currentNode

      }
      case CJUMP(o, e1, e2, t, f) => {

        val currentNode = node(s"CJUMP($o f:$f t:$t)")

        links += parent -> currentNode

        exp(currentNode, e1)
        exp(currentNode, e2)

      }
      case SEQ(s1, s2) => {
        val currentNode = node("SEQ")

        links += parent -> currentNode

        doit(currentNode, s1)
        doit(currentNode, s2)
      }
      case LABEL(n) => {

        val currentNode = node(s"LABEL($n)")

        links += parent -> currentNode
      }
    }

    strings += "digraph g {"
    strings += "\troot"
    doit("root", stm)


    links foreach { case(s,d) => strings += s"$s -> $d" }

    strings += "}"

    strings mkString "\n"

  }

  def printgraph(r: RegisterAllocation, name:String): Unit = {

    val c = Map ( 0 -> "red", 1 -> "blue", 2 -> "yellow", 3 -> "green")

    println(s"graph $name {")


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
