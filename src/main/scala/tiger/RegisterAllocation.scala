package tiger

import tiger.Asm.{OPER, LABEL, MOVE, Instr}
import tiger.Frame.{InReg, InFrame}
import tiger.LivenessComponent.Interference.nodeMap
import tiger.LivenessComponent.{Interference, FlowNode}

import scala.collection.immutable.Nil
import scala.collection.mutable

trait MultiSet[A, T] extends mutable.Map[A, T] {

  def setCount(e: A, count: T)(implicit num: Numeric[T]): Unit = {
    this.update(e, num.plus(this.getCount(e), count))
  }

  def getCount(e: A)(implicit num: Numeric[T]): T = this.getOrElse(e, num.zero)

}

class RegisterAllocation(_precolored: List[Temp.Temp]) {

  /* Amount of registers */
  val K: Int = 3

  /*
   * Node work-list, sets and stacks
   */

  /* machine registers, preassigned colors */
  val precolored = mutable.HashSet[Temp.Temp](_precolored: _*)


  /* Temporary registers, not precolored and not yet procesed. */
  val initial = mutable.HashSet.empty[Temp.Temp]

  /*  list of low-dregree non-move-related nodes */
  val simplfiyWorklist = mutable.HashSet.empty[Temp.Temp]

  /*  list of low-dregree non-move-related nodes */
  val freezeWorklist = mutable.HashSet.empty[Temp.Temp]

  /*  high-dregree nodes */
  val spillWorkList = mutable.HashSet.empty[Temp.Temp]

  /*  nodes marked for spilling during this round; initial empty */
  val spilledNodes = mutable.HashSet.empty[Temp.Temp]

  /* register than have been coalesced; when u <- v is coalesced, b is added to this set and u put back on some work-list (or vice versa) */
  val coalescedNodes = mutable.HashSet.empty[Temp.Temp]

  /*  nodes successfully colored */
  val coloredNodes = mutable.HashSet.empty[Temp.Temp]

  /* stack containing temps removed from the graph */
  val selectStack = mutable.ArrayStack[Temp.Temp]()

  /* stack containing temps removed from the graph */
  //  /* Nodes succefully colored */
  //  val coloredNodes = mutable.HashSet.empty[Temp.Temp]

  /*
   * Move Sets
   */

  /* moves that has been coalesced*/
  val coalescedMoves = mutable.HashSet.empty[MOVE]

  /* moves whose source and target interfere */
  val constrainedMoves = mutable.HashSet.empty[MOVE]

  /* moves that will no longer be consider for coalescing */
  val frozenMoves = mutable.HashSet.empty[MOVE]

  /* moves enabled for possible coalescing */
  val workListMoves = mutable.HashSet.empty[MOVE]

  /* Moves not yet ready fo coalescing */
  val activeMoves = mutable.HashSet.empty[MOVE]


  /*
   * Other data structures
   */

  /* The set of the interference graph edge (u,v). If (u,v) ∈ adjSet then (v,u) ∈ adjSet */
  val adjSet = mutable.HashSet.empty[(Temp.Temp, Temp.Temp)]

  /* Adjacency list representation of the graph; for each non-precolored temporary u, adjList(u) is the set of nodes that interfere with u */
  val adjList = new mutable.HashMap[Temp.Temp, mutable.Set[Temp.Temp]] with mutable.MultiMap[Temp.Temp, Temp.Temp]

  /* contains a list of the degree of each node*/
  val degree = new mutable.HashMap[Temp.Temp, Int]() with MultiSet[Temp.Temp, Int]

  /* a maping from a node to the list of move associated with */
  val moveList = new mutable.HashMap[Temp.Temp, mutable.Set[MOVE]] with mutable.MultiMap[Temp.Temp, MOVE]

  /* when a move (u,v) has been coaleased, and v put in coalescedNode, then alias(v) = u */
  val alias = new mutable.HashMap[Temp.Temp, Temp.Temp]

  /* the chosen color by the algorithm for a node; for precolored nodes this is initialized to the given color  */
  val color = new mutable.HashMap[Temp.Temp, Int]


  def build(instructions: List[Instr], liveOut: nodeMap): Unit = {

    for (i <- instructions.reverse) {
      val node = FlowNode(i)

      var live = liveOut(node)

      if (node.isMove) {
        live = live -- node.uses()

        for (n <- node.defs() | node.uses()) {
          moveList.addBinding(n, i.asInstanceOf[MOVE])
        }

        workListMoves += i.asInstanceOf[MOVE]
      }

      live = live | node.defs()

      for {
        d <- node.defs()
        l <- live
      } addEdge(l, d)

    }

  }

  def addEdge(u: Temp.Temp, v: Temp.Temp) = {
    if (!adjSet.contains((u, v)) && u != v) {

      adjSet ++= Set(u -> v, v -> u)


      if (!precolored.contains(u)) {
        adjList.addBinding(u, v)
        degree.setCount(u, 1)
      }

      if (!precolored.contains(v)) {
        adjList.addBinding(v, u)
        degree.setCount(v, 1)
      }

    }
  }

  def makeWorkList(): Unit = {
    for (n <- initial) {
      initial -= n

      if (degree(n) >= K) {
        spillWorkList += n
      }
      else if (moveRelated(n)) {
        freezeWorklist += n
      }
      else {
        simplfiyWorklist += n
      }
    }
  }

  def adjacent(n: Temp.Temp) = adjList(n) -- (coalescedNodes | selectStack.toSet)

  def moveNodes(n: Temp.Temp) = moveList(n) & (activeMoves | workListMoves)

  def moveRelated(n: Temp.Temp) = moveNodes(n).isEmpty

  def simplify(): Unit = {
    val n = simplfiyWorklist.head
    simplfiyWorklist -= n

    selectStack.push(n)

    adjacent(n).foreach(decrementDegree)
  }

  def decrementDegree(n: Temp.Temp): Unit = {
    degree.setCount(n, -1)

    if (degree.getCount(n) == K) {
      enableMoves(adjacent(n) + n)

      spillWorkList -= n

      if (moveRelated(n)) {
        freezeWorklist -= n
      } else {
        simplfiyWorklist -= n
      }
    }
  }

  def enableMoves(nodes: mutable.Set[Temp.Temp]): Unit = {
    for (n <- nodes) {
      for (m <- moveNodes(n) if activeMoves.contains(m)) {
        activeMoves -= m
        workListMoves += m
      }
    }
  }

  def getAlias(n: Temp.Temp): Temp.Temp = if (coalescedNodes.contains(n)) getAlias(alias(n)) else n

  def addWorkList(u: Temp.Temp): Unit = if (!precolored.contains(u) && !moveRelated(u) && degree(u) < K) {
    freezeWorklist -= u
    simplfiyWorklist += u
  }

  def ok(t: Temp.Temp, r: Temp.Temp): Boolean = degree(t) < K || precolored.contains(t) || adjSet.contains(t -> r)

  def conservative(temps: mutable.Set[Temp.Temp]): Boolean = temps.count(degree(_) >= K) < K

  def combine(u: Temp.Temp, v: Temp.Temp): Unit = {
    if (freezeWorklist.contains(v))
      freezeWorklist -= v
    else
      spillWorkList -= v

    coalescedNodes += v
    alias.put(v, u)

    moveList.update(u, moveList(u) ++ moveList(v))
    enableMoves(mutable.Set(v))

    for (t <- adjacent(v)) {
      addEdge(t, u)
      decrementDegree(t)
    }

    if (degree(u) >= K && freezeWorklist.contains(u)) {
      freezeWorklist -= u
      spillWorkList += u
    }

  }

  def freeze(): Unit = {
    val u = freezeWorklist.head
    freezeWorklist -= u
    simplfiyWorklist + u
    freezeMoves(u)
  }

  def freezeMoves(u: Temp.Temp): Unit = {
    moveNodes(u).foreach { case m@MOVE(_, x, y) =>
      val v = if (getAlias(y) == getAlias(u)) getAlias(x) else getAlias(y)

      activeMoves -= m
      frozenMoves += m

      if (moveNodes(v).isEmpty && degree(v) < K) {
        freezeWorklist -= v
        simplfiyWorklist += v
      }

    }
  }

  def coalesce() = {
    val m = workListMoves.head

    val x = getAlias(m.dst)
    val y = getAlias(m.src)

    val (u, v) = if (precolored.contains(y)) (y, x) else (x, y)

    workListMoves -= m

    if (u == v) {

      coalescedMoves += m
      addWorkList(u)

    } else if (precolored.contains(v) || adjSet.contains(u -> v)) {

      constrainedMoves += m
      addWorkList(u)
      addWorkList(v)

    } else if ((precolored.contains(u) && adjacent(v).forall(t => ok(t, u))) ||
      (!precolored.contains(u) && conservative(adjacent(u) ++ adjacent(v)))) {

      coalescedMoves += m
      combine(u, v)
      addWorkList(u)

    } else {
      activeMoves += m
    }


  }

  def selectSpill(): Unit = {
    val m = spillWorkList.head // Debería usar alguna herustica

    spillWorkList -= m
    spillWorkList += m

    freezeMoves(m)
  }

  def asignColors(): Unit = {
    while (selectStack.nonEmpty) {
      val n = selectStack.pop()

      val okColors: mutable.Set[Int] = mutable.HashSet[Int](0 to K - 1: _*)

      val union = coloredNodes | precolored

      for (w <- adjList(n); x = getAlias(w) if union.contains(x)) {
        okColors -= color(x)
      }

      if (okColors.isEmpty) {
        spilledNodes += n
      } else {
        coloredNodes += n
        color.put(n, okColors.head)
      }
    }

    for (n <- coalescedNodes) {
      color.put(n, color(getAlias(n)))
    }
  }

  def rewriteProgram(instr: List[Instr], frame: Frame): List[Instr] = {
    /*
     * ldr r1, [r0 + n]   /* r1 ← (*r0 + n) */
     * str r1, [r0 + n]   /* (*r0 + n) ← r1 */
     */

    def alloc = frame.allocLocal(esc = false) match {
      case InFrame(i) => i
      case InReg(l) => throw new Error("cuanak")
    }

    val newTemps = mutable.HashSet.empty[Temp.Temp]

    def newTemp = {
      val t = Temp.newTemp()
      newTemps += t
      t
    }

    def storeGen(t: Temp.Temp, offset: Int) = OPER(asm = s"str    `s0, [`s1 + $offset]\n", dst = List(), src = List(t, Frame.FP))

    def fetchGen(t: Temp.Temp, offset: Int) = OPER(asm = s"ldr    `d0, [`s0 + $offset]\n", dst = List(t), src = List(Frame.FP))

    def rewrite(instr: List[Instr]): List[Instr] = instr match {

      case MOVE(asm, src, dst) :: tl if spilledNodes.contains(src) => {
        val t = newTemp

        fetchGen(t, alloc) :: MOVE(asm = asm, src = t, dst = dst) :: rewrite(tl)
      }

      case MOVE(asm, src, dst) :: tl if spilledNodes.contains(dst) => {
        val t = newTemp

        MOVE(asm = asm, src, t) :: storeGen(t, alloc) :: rewrite(tl)
      }

      case OPER(asm, src, dst, None) :: tl if spilledNodes.exists(src.contains(_)) => {
        val t = newTemp

        val newSrc = src.map { x => if (spilledNodes.contains(x)) t else x}

        fetchGen(t, alloc) :: OPER(asm = asm, src = newSrc, dst = dst, jump = None) :: rewrite(tl)
      }

      case OPER(asm, src, dst, None) :: tl if spilledNodes.exists(dst.contains(_)) => {
        val t = newTemp

        val newDst = dst.map { x => if (spilledNodes.contains(x)) t else x}

        OPER(asm = asm, src = src, dst = newDst, jump = None) :: storeGen(t, alloc) :: rewrite(tl)
      }

      case x :: xs => x :: rewrite(xs)
      case Nil => Nil
    }


    rewrite(instr)
  }

}


