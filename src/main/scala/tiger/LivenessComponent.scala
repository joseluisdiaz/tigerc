package tiger

import tiger.Asm.{Instr, MOVE, LABEL, OPER}

object LivenessComponent {

  case class FlowNode(i: Instr) {

    def defs(): Set[Temp.Temp] = i match {
      case OPER(_, _, dst, _) => dst.toSet -- Frame.notAsignables
      case MOVE(_, _, dst) => Set(dst) -- Frame.notAsignables
      case _ => Set()
    }

    def uses(): Set[Temp.Temp] = i match {
      case OPER(_, src, _, _) => src.toSet -- Frame.notAsignables
      case MOVE(_, src, _) => Set(src) -- Frame.notAsignables
      case _ => Set()
    }

    override def toString = s"(${i.asm})"

    def isMove: Boolean = i.isMove
  }

  class Graph[T] {

    import scala.collection.mutable.{Set, HashMap, MultiMap}

    // Los nodos predecesores de un nodo dado
    private val predM = new HashMap[T, Set[T]] with MultiMap[T, T]

    // Los nodos sucesores de un nodo dado
    private val succM = new HashMap[T, Set[T]] with MultiMap[T, T]

    def mkEdge(from: T, to: T): Unit = {
      predM.addBinding(to, from)
      succM.addBinding(from, to)
    }

    def rmEdge(from: T, to: T): Unit = {
      predM.removeBinding(to, from)
      succM.removeBinding(from, to)
    }

    def pred(n: T) = predM.getOrElse(n, Set()).toSet

    def succ(n: T) = succM.getOrElse(n, Set()).toSet

    def nodes = predM.keys.toSet | succM.keys.toSet

    def adj(n: T) = pred(n) | succ(n)

  }

  object Interference {

    type nodeMap = Map[FlowNode, Set[Temp.Temp]]

    def inferenceGraph(g: Graph[FlowNode], nodes: List[FlowNode], liveOut: nodeMap): Graph[Temp.Temp] = {
      val ret = new Graph[Temp.Temp]

      def moveEsp(g: FlowNode, t: Temp.Temp): Boolean = false

      val v = for {
        node <- nodes
        temp <- node.defs()
        outNode <- liveOut(node) if temp != outNode && !moveEsp(node, outNode)
      }
      yield {
        ret.mkEdge(outNode, temp)
        ret.mkEdge(temp, outNode)
      }

      ret
    }

    def liveness(g: Graph[FlowNode], l: List[FlowNode]) = {

      val in: nodeMap = l.map {_ -> Set[Temp.Temp]() }.toMap
      val out: nodeMap = l.map { _ -> Set[Temp.Temp]() }.toMap

      def flowEq(in: nodeMap, out: nodeMap): (nodeMap, nodeMap) = {

        val (in2, out2) = l.foldLeft((in, out)) { case ((i, o), node) =>
          val _i = i + (node -> (node.uses() union (out(node) -- node.defs())))
          val _o = o + (node -> (g.succ(node) flatMap _i))
          (_i, _o)
        }

//        println(s"node\t\tin\tout")
//        l.foreach { node =>
//          println(s"$node\t\t|\t${in2(node)}\t|\t${out2(node)}")
//        }

        if (in == in2 && out == out2) {
          (in2, out2)
        } else {
          flowEq(in2, out2)
        }

      }

      flowEq(in, out)
    }

  }

  object Flow {

    import scala.collection.mutable

    def instrs2graph(ins: List[Asm.Instr]): Graph[FlowNode] = {

      val g = new Graph[FlowNode]()

      val labels = mutable.HashMap.empty[Temp.Label, Asm.Instr]

      val jumps = mutable.ArrayBuffer.empty[(Asm.Instr, Temp.Label)]

      implicit def inst2node(i:Asm.Instr): FlowNode = FlowNode(i)

      def findLabels(i: List[Asm.Instr]): Unit = i match {
        case LABEL(asm, l) :: n :: xs => {
          labels.put(l, n)
          findLabels(n :: xs)
        }

        case n :: xs => findLabels(xs)

        case Nil => ()
      }

      // do the harlem shake!
      findLabels(ins)

      // All Nodes
      ins.sliding(2).foreach {
        case List(o@OPER(_,_,_,Some(ls)), to) => {
          ls foreach { x => g.mkEdge(o, labels(x)) }
        }
        case List(from, to) => g.mkEdge(from, to)
        case List(node) => ()
      }

      g
    }


  }


}
