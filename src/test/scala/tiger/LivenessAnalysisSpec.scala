package tiger

import org.scalatest.{Matchers, FlatSpec}
import tiger.LivenessComponent.FlowNode


/**
 * Created by jose on 19/08/14.
 */
class LivenessAnalysisSpec extends FlatSpec with Matchers {

  def fixture = new {
    val asm1 =  List( Asm.OPER(asm = " a: = 0     ", src = List(), dst = List("a")),
      Asm.LABEL(asm = "L1:", l = "L1"),
      Asm.OPER(asm = " b: = a + 1 ", src = List("a"), dst = List("b")),
      Asm.OPER(asm = " c: = c + b ", src = List("b", "c"), dst = List("c")),
      Asm.OPER(asm = " a: = b * 2 ", src = List("b"), dst = List("a")),
      Asm.OPER(asm = "   a < N    ", src = List("a"), dst = List(), jump = Some(List("L1"))),
      Asm.OPER(asm = "   c        ", src = List("c"), dst = List()))
  }

  "InferenceGraph" should "create graph without crash" in {
    val asm = fixture.asm1

    val g = LivenessComponent.Flow.instrs2graph(asm)

    // Liveness analisys
    val (in,out) = LivenessComponent.Interference.liveness(g)

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
    val g2 = LivenessComponent.Interference.inferenceGraph(g, out)

    g2.succ("a") should be(Set("c"))

    g2.succ("b") should be(Set("c"))

    g2.succ("c") should be(Set("a", "b"))

    g2.nodes should be(Set("a", "b", "c"))



  }

}
