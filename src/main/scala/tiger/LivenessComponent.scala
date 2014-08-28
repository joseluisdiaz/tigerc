package tiger

import tiger.Asm.{Instr, MOVE, LABEL, OPER}

object LivenessComponent {

  case class FlowNode(i: Instr) {

    def defs(): Set[Temp.Temp] = i match {
      case OPER(_, _, dst, _) => dst.toSet
      case MOVE(_, _, dst) => Set(dst)
      case _ => throw new Error("Jump on a node")
    }

    def uses(): Set[Temp.Temp] = i match {
      case OPER(_, src, _, _) => src.toSet
      case MOVE(_, src, _) => Set(src)
      case _ => throw new Error("Jump on a node")
    }

    override def toString = s"(${i.asm})"

    def isMove: Boolean = i.isInstanceOf[MOVE]
  }

  class Graph[T] {

    import scala.collection.mutable.{Set, HashMap, MultiMap}

    // Los nodos predecesores de un nodo dado
    private val predM = new HashMap[T, Set[T]] with MultiMap[T, T]

    // Los nodos sucesores de un nodo dado
    private val succM = new HashMap[T, Set[T]] with MultiMap[T, T]

    val nodes = Set[T]()

    def mkEdge(from: T, to: T): Unit = {
      predM.addBinding(to, from)
      succM.addBinding(from, to)
      nodes ++= Set(from, to)
    }

    def rmEdge(from: T, to: T): Unit = {
      predM.removeBinding(to, from)
      succM.removeBinding(from, to)
    }

    def pred(n: T) = predM.getOrElse(n, Set()).toSet

    def succ(n: T) = succM.getOrElse(n, Set()).toSet

    def adj(n: T) = pred(n) | succ(n)

  }

  object Interference {

    type nodeMap = Map[FlowNode, Set[Temp.Temp]]

    def inferenceGraph(g: Graph[FlowNode], liveOut: nodeMap): Graph[Temp.Temp] = {
      val ret = new Graph[Temp.Temp]

      def moveEsp(g: FlowNode, t: Temp.Temp): Boolean = false

      val v = for {
        node <- g.nodes
        temp <- node.defs()
        outNode <- liveOut(node) if temp != outNode && !moveEsp(node, outNode)

      } yield {
        ret.mkEdge(outNode, temp)
        ret.mkEdge(temp, outNode)
      }

      ret
    }

    def liveness(g: Graph[FlowNode]) = {

      val in: nodeMap = g.nodes.map {
        _ -> Set[Temp.Temp]()
      }.toMap
      val out: nodeMap = g.nodes.map {
        _ -> Set[Temp.Temp]()
      }.toMap

      def flowEq(in: nodeMap, out: nodeMap): (nodeMap, nodeMap) = {

        val in2 = in.map { case (node, _) =>
          node -> (node.uses() union (out(node) -- node.defs()))
        }.toMap

        val out2 = out.map { case (node, _) =>
          node -> (g.succ(node) flatMap in)
        }.toMap


        if (in != in2 || out != out2) {
          flowEq(in2, out2)
        } else {
          (in2, out2)
        }

      }

      flowEq(in, out)
    }

  }

  object Flow {

    import scala.collection.mutable

    def instrs2graph(ins: List[Asm.Instr]): Graph[FlowNode] = {

      val g = new Graph[FlowNode]()

      val labels = mutable.HashMap.empty[Temp.Label, FlowNode]

      val jumps = mutable.ArrayBuffer.empty[(FlowNode, Temp.Label)]

      //
      def convert(i: List[Asm.Instr]): List[FlowNode] = i match {
        case LABEL(_, l) :: n :: xs => {
          val node = FlowNode(n)
          labels.put(l, node)
          node :: convert(xs)
        }

        case (n@OPER(_, _, _, Some(ls))) :: xs => {
          val node = FlowNode(n)
          ls.foreach {
            jumps += node → _
          }
          node :: convert(xs)
        }

        case n :: xs => FlowNode(n) :: convert(xs)

        case Nil => Nil
      }

      val nodes = convert(ins)

      // All Nodes
      nodes.sliding(2).foreach {
        case List(from, to) => g.mkEdge(from, to)
      }

      // all references to jumps
      jumps.foreach {
        case (node, l) => g.mkEdge(node, labels(l))
      }

      g
    }


  }


}
