package tiger

import org.scalatest.{Matchers, FlatSpec}
import tiger.Asm
import tiger.LivenessComponent.FlowNode


/**
 * Created by jose on 19/08/14.
 */
class LivenessAnalysisSpec extends FlatSpec with Matchers {

  def fixture = new {
    val asm1 = List(Asm.OPER(asm = " a: = 0     ", src = List(), dst = List("a")),
      Asm.LABEL(asm = "L1:", l = "L1"),
      Asm.OPER(asm = " b: = a + 1 ", src = List("a"), dst = List("b")),
      Asm.OPER(asm = " c: = c + b ", src = List("b", "c"), dst = List("c")),
      Asm.OPER(asm = " a: = b * 2 ", src = List("b"), dst = List("a")),
      Asm.OPER(asm = "   a < N    ", src = List("a"), dst = List(), jump = Some(List("L1"))),
      Asm.OPER(asm = "   c        ", src = List("c"), dst = List()))

    val asm2 = List(Asm.OPER(asm = " a: = 0     ", src = List(), dst = List("a")),
      Asm.LABEL(asm = "L1:", l = "L1"),
      Asm.OPER(asm = " b: = a + 1 ", src = List("a"), dst = List("b")),
      Asm.OPER(asm = " c: = c + b ", src = List("b", "c"), dst = List("c")),
      Asm.OPER(asm = " a: = b * 2 ", src = List("b"), dst = List("a")),
      Asm.OPER(asm = "   a < N    ", src = List("a"), dst = List(), jump = Some(List("L1","next"))),
      Asm.LABEL(asm = "next:", l = "next"),
      Asm.OPER(asm = "   c        ", src = List("c"), dst = List()))

    val asm3 = List(
      Asm.LABEL(asm = "enter:", l = "enter"),
      Asm.MOVE(asm = "1)  c <- r3      ", src = "r3", dst = "c"),
      Asm.MOVE(asm = "2)  a <- r1      ", src = "r1", dst = "a"),
      Asm.MOVE(asm = "3)  b <- r2      ", src = "r2", dst = "b"),
      Asm.OPER(asm = "4)  d <- 0       ", src = List(), dst = List("d")),
      Asm.MOVE(asm = "5)  e <- a       ", src = "a", dst = "e"),
      Asm.LABEL(asm = "6) loop:", l = "loop"),
      Asm.OPER(asm = "7)  d <- d + b   ", src = List("d", "b"), dst = List("d")),
      Asm.OPER(asm = "8)  e <- e - 1   ", src = List("e"), dst = List("e")),
      Asm.OPER(asm = "9)  if (e <  0) goto loop   ", src = List("e"), dst = List(), jump = Some(List("loop", "next"))),
      Asm.LABEL(asm = "next:  ", l = "next"),
      Asm.MOVE(asm = "10)  r1 <- d      ", src = "d", dst = "r1"),
      Asm.MOVE(asm = "11)  r3 <- c      ", src = "c", dst = "r3"),
      Asm.OPER(asm = " return ", src=List("r1", "r3"), dst = List())
    )
  }

  "LivenessComponent" should "blah blah blah" in {
    val i = fixture.asm2(7)

    FlowNode(i).defs() should be(i.asInstanceOf[Asm.OPER].dst.toSet)
    FlowNode(i).uses() should be(i.asInstanceOf[Asm.OPER].src.toSet)

  }

  "RegisterAllocation" should "create interference graph like a boss!!!" in {
    val r = new RegisterAllocation(List())
    val g = LivenessComponent.Flow.instrs2graph(fixture.asm1)
    val nodes = fixture.asm1.map(FlowNode)

    // Liveness analisys
    val (in, out) = LivenessComponent.Interference.liveness(g, nodes)

    r.build(fixture.asm1, out)

    println("graph {")
    for ( (n,v) <- r.adjSet ) { println(s"$n -- $v") }
    println("}")
  }


  "LivenessComponent" should "create a correct adjacency graph" in {
    val g1 = LivenessComponent.Flow.instrs2graph(fixture.asm1)

    val g2 = LivenessComponent.Flow.instrs2graph(fixture.asm2)

    g1.succ(FlowNode(fixture.asm1(5))) should be(Set(FlowNode(fixture.asm1(2))))
    g1.succ(FlowNode(fixture.asm1(0))) should be(Set(FlowNode(fixture.asm1(1))))


    g2.succ(FlowNode(fixture.asm2(5))) should be(Set(FlowNode(fixture.asm1(2)), FlowNode(fixture.asm1(6))))
    g2.succ(FlowNode(fixture.asm2(0))) should be(Set(FlowNode(fixture.asm2(1))))

    // Liveness analisys
  }

  "InferenceGraph" should "work" in {
    val asm = fixture.asm1
    val nodes = asm.map(FlowNode)

    val g = LivenessComponent.Flow.instrs2graph(asm)

    // Liveness analisys
    val (in, out) = LivenessComponent.Interference.liveness(g, nodes)

    out(FlowNode(asm(0))) should be(Set("a", "c"))
    out(FlowNode(asm(2))) should be(Set("b", "c"))
    out(FlowNode(asm(3))) should be(Set("b", "c"))
    out(FlowNode(asm(4))) should be(Set("a", "c"))
    out(FlowNode(asm(5))) should be(Set("a", "c"))
    out(FlowNode(asm(6))) should be(Set())

    in(FlowNode(asm(0))) should be(Set("c"))
    in(FlowNode(asm(2))) should be(Set("a", "c"))
    in(FlowNode(asm(3))) should be(Set("b", "c"))
    in(FlowNode(asm(4))) should be(Set("b", "c"))
    in(FlowNode(asm(5))) should be(Set("a", "c"))
    in(FlowNode(asm(6))) should be(Set("c"))

    // Interference Graph
    val g2 = LivenessComponent.Interference.inferenceGraph(g, nodes, out)

    g2.succ("a") should be(Set("c"))

    g2.succ("b") should be(Set("c"))

    g2.succ("c") should be(Set("a", "b"))

//    g2.nodes should be(Set("a", "b", "c"))
  }


}
