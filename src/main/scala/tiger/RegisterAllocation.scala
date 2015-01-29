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

object RegisterAllocation {
  def apply(i: List[Instr], f: Frame) = new RegisterAllocation(i, f)
}

class RegisterAllocation(var instructions: List[Instr], frame: Frame) {

  /* Amount of registers */
  val K: Int = frame.registers.length

  /*
   * Node work-list, sets and stacks
   */

  /* machine registers, preassigned colors */
  val precolored = mutable.HashSet[Temp.Temp](frame.registers: _*)

  val _initial = for {
    i <- instructions; x = FlowNode(i)
    s <- x.defs() ++ x.uses()
    if !precolored.contains(s)
  } yield s

  /* Temporary registers, not precolored and not yet procesed. */
  val initial = mutable.HashSet[Temp.Temp](_initial.toArray: _*)

  /*  list of low-dregree non-move-related nodes */
  val simplfiyWorklist = mutable.HashSet.empty[Temp.Temp]

  /*  list of low-dregree non-move-related nodes */
  val freezeWorklist = mutable.HashSet.empty[Temp.Temp]

  /*  high-dregree nodes */
  val spillWorkList = mutable.HashSet.empty[Temp.Temp]

  /*  nodes marked for spilling during this round; initial empty */
  val spilledNodes = mutable.HashSet.empty[Temp.Temp]

  /* register than have been coalesced; when u <- v is coalesced, v is added to this set and u put back on some work-list (or vice versa) */
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


  def rename() = {

    def c(t: Temp.Temp) =
      if (precolored.contains(t)) t
      else {
        val c = color(t)
        frame.registers(c)
      }

    instructions = instructions map {
      case OPER(asm, src, dst, jump) => OPER(asm, src map c, dst map c, jump)
      case MOVE(asm, src, dst) => MOVE(asm, c(src), c(dst))
      case a => a
    }

  }

  def remove() = {
    instructions = instructions filter {
      case m@MOVE(_, src, dst) => src != dst
      case _ => true
    }
  }

  def get(): (List[Instr], Frame) = {

    val t = Temp.newTemp()

    loop()
    println(s"TEMP---> $t <--> ${Temp.newTemp()}")


    rename()
    remove()

    val (epilog, x, prolog) = frame.procEntryExit3(instructions)

    instructions = x


    color.foreach { case (t, c) => println(s"$t\t$c")}

    (instructions, frame)
  }


  def loop(): Unit = {
    build()
    makeWorkList()

    do {

      if (simplfiyWorklist.nonEmpty)
        simplify()

      if (workListMoves.nonEmpty)
        coalesce()

      if (freezeWorklist.nonEmpty)
        freeze()

      if (spillWorkList.nonEmpty)
        selectSpill()

    } while (simplfiyWorklist.nonEmpty || workListMoves.nonEmpty || freezeWorklist.nonEmpty || spillWorkList.nonEmpty)

    assignColors()

    if (spilledNodes.nonEmpty) {
      rewriteProgram()
      loop()
    }
  }

  def liveness(): nodeMap = {

    // Flow Graph
    val g = LivenessComponent.Flow.instrs2graph(instructions)

    // Liveness analisys
    val (in, out) = LivenessComponent.Interference.liveness(g, instructions.map(FlowNode))

    out
  }

//  def buildDistanceMap() = {
//    val nodes = instructions map { FlowNode } zipWithIndex
//    val definesAt = nodes map { case (n, i) => n.defs() map { x => x -> i} } flatten
//    val definesAtMap = definesAt toMap
//    val distances = nodes map { case (n, i) => n.uses() map { x => x -> ( i - definesAtMap.getOrElse(x, 10000) ) } } flatten
//
//    distances filter { case (_, d) => d >= 0 } toMap
//  }
//
//
//  var distanceMap = Map.empty[Temp.Temp, Int]

  def build(): Unit = {

    val liveOut = liveness()

    for (i <- instructions) {
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

//    distanceMap = buildDistanceMap()

    precolored.foreach { x => degree.setCount(x, Int.MaxValue) }

    Util.printgraph(this, "Inference")

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
      if (degree.getOrElse(n, 0) >= K) {
        spillWorkList += n
      }
      else if (moveRelated(n)) {
        freezeWorklist += n
      }
      else {
        simplfiyWorklist += n
      }
    }
    initial.clear()
  }

  def adjacent(n: Temp.Temp) = adjList.getOrElse(n, mutable.Set.empty[Temp.Temp]) -- (coalescedNodes | selectStack.toSet)

  def moveNodes(n: Temp.Temp) = if (moveList.contains(n))
    moveList(n) & (activeMoves | workListMoves)
  else
    mutable.Set.empty[Asm.MOVE]

  def moveRelated(n: Temp.Temp) = moveNodes(n).nonEmpty

  def simplify(): Unit = {
    val n = simplfiyWorklist.head
    simplfiyWorklist -= n

    selectStack.push(n)

    adjacent(n) foreach decrementDegree
  }

  def decrementDegree(n: Temp.Temp): Unit = {
    degree.setCount(n, -1)

    if (degree.getCount(n) == K) {
      enableMoves(adjacent(n) + n)

      spillWorkList -= n

      if (moveRelated(n)) {
        freezeWorklist += n
      } else {
        simplfiyWorklist += n
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

  def addWorkList(u: Temp.Temp): Unit = if (!precolored.contains(u) && !moveRelated(u) && degree.getOrElse(u, 0) < K) {
    freezeWorklist -= u
    simplfiyWorklist += u
  }

  def ok(t: Temp.Temp, r: Temp.Temp): Boolean = precolored.contains(t) || degree(t) < K || adjSet.contains(t -> r)

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

    if (degree.getOrElse(u, 0) >= K && freezeWorklist.contains(u)) {
      freezeWorklist -= u
      spillWorkList += u
    }

  }

  def freeze(): Unit = {
    val u = freezeWorklist.head
    freezeWorklist -= u
    simplfiyWorklist += u
    freezeMoves(u)
  }

  def freezeMoves(u: Temp.Temp): Unit = {
    moveNodes(u).foreach {
      case m@MOVE(_, x, y) =>
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

    val x = getAlias(m.src)
    val y = getAlias(m.dst)

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
//    val list = spillWorkList.toList.sortBy(distanceMap)

    val m = spillWorkList.head
    spillWorkList -= m
    simplfiyWorklist += m

    freezeMoves(m)
  }

  def assignColors(): Unit = {

    color.clear()
    frame.registers.zipWithIndex.foreach { case (r, c) => color.put(r, c)}

    while (selectStack.nonEmpty) {
      val n = selectStack.pop()

      val okColors: mutable.Set[Int] = mutable.SortedSet[Int](0 to K - 1: _*)

      val union = coloredNodes | precolored

      for (w <- adjList.getOrElse(n, mutable.HashSet.empty[Temp.Temp]); x = getAlias(w) if union contains x) {
        okColors -= color(x)
      }

      if (okColors.isEmpty) {
        spilledNodes += n
      } else {
        coloredNodes += n

        color.put(n, okColors.min)
      }
    }

    var i = 1000
    def f: Int = {
      i += 1; i
    }
    for (n <- coalescedNodes) {
      color.put(n, color.getOrElseUpdate(getAlias(n), f))
    }

    println("Spilled")
    println(spilledNodes)
    color.foreach { case (k,v) => if (v > 999 ) println(k)}

  }

  def rewriteProgram(): Unit = {
    /*
     * ldr r1, [r0 + n]   /* r1 ← (*r0 + n) */
     * str r1, [r0 + n]   /* (*r0 + n) ← r1 */
     */
    def aux = frame.allocLocal(esc = true) match {
      case InFrame(i) => i
      case _ => throw new Error("cuanak")
    }

    val allocated = spilledNodes map { reg => reg -> aux} toMap

    // spill -> dst
    val newTemps = mutable.HashSet.empty[Temp.Temp]

    def newTemp = {
      val t = Temp.newTemp()
      newTemps += t
      t
    }

    def storeGen(t: Temp.Temp, offset: Int) = OPER(asm = s"str     's0, [fp, #$offset]", dst = List(), src = List(t))

    def fetchGen(t: Temp.Temp, offset: Int) = OPER(asm = s"ldr     'd0, [fp, #$offset]", dst = List(t), src = List())


    def convert(genFunction: (Temp.Temp, Int) => Instr): Temp.Temp => (Temp.Temp, Option[Instr]) = { x =>
      if (spilledNodes.contains(x)) {
        val t = newTemp
        (t, Some(genFunction(t, allocated(x))))
      }
      else (x, None)
    }

    def rewrite(instr: List[Instr]): List[Instr] = instr match {
      // Insert a fetch before each use
      // Insert a store after each definition

      case MOVE(asm, src, dst) :: tl => {
        val (newDst, store) = convert(storeGen)(dst)

        val (newSrc, fetch) = convert(fetchGen)(src)

        val head = List(fetch, Some(MOVE(asm = asm, src = newSrc, dst = newDst)), store)

        (head flatten) ++ rewrite(tl)
      }


      case OPER(asm, src, dst, None) :: tl => {
        val (newDst, stores) = dst map convert(storeGen) unzip

        val (newSrc, fetchs) = src map convert(fetchGen) unzip


        val head = fetchs ++ List(Some(OPER(asm = asm, src = newSrc, dst = newDst, jump = None))) ++ stores

        (head flatten) ++ rewrite(tl)
      }

      case x :: xs => x :: rewrite(xs)

      case Nil => Nil
    }


    val i = rewrite(instructions)
    instructions = i

    spilledNodes.clear()
    initial.clear()
    initial ++= (coloredNodes | coalescedNodes | newTemps)

    coloredNodes.clear()
    coalescedNodes.clear()

    simplfiyWorklist.clear()
    freezeWorklist.clear()
    spillWorkList.clear()
    selectStack.clear()

    coalescedMoves.clear()
    constrainedMoves.clear()
    frozenMoves.clear()
    workListMoves.clear()
    activeMoves.clear()

    adjSet.clear()
    adjList.clear()
    degree.clear()
    moveList.clear()
    alias.clear()
    color.clear()
  }

}


