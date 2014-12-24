package tiger

import tiger.Asm.Instr
import tiger.Tree._

import scala.collection.mutable.ListBuffer

object Asm {

  sealed abstract class Instr {
    def isMove:Boolean
    def isLabel:Boolean
    def asm:String

    protected def f(reg:String, v:List[String], base:String) = v.zipWithIndex.foldLeft(base) {
      case (str, (value, index)) => str.replaceFirst(s"'$reg$index", value)
    }

    def code:String

  }

  case class OPER(asm: String, src: List[Temp.Temp], dst: List[Temp.Temp], jump: Option[List[Temp.Label]]) extends Instr {
    override def isMove = false
    override def isLabel = false
    override def code = {
      val v = f("s", src, f("d", dst, asm))
      v
    }
  }

  object OPER {
    def apply(asm: String, src: List[Temp.Temp], dst: List[Temp.Temp]):OPER = OPER(asm, src, dst, None)
    def apply(asm: String, jump: List[Temp.Label]):OPER = OPER(asm, List(), List(), Some(jump))
  }

  case class LABEL(asm: String, l: Temp.Label) extends Instr {
    override def isMove = false
    override def isLabel = true

    override def code = asm
  }

  case class MOVE(asm: String, src: Temp.Temp, dst: Temp.Temp) extends Instr {
    override def isMove = true
    override def isLabel = false

    override def code = f("s", List(src), f("d", List(dst), asm))

  }

}

object CodeGen {
  import tiger.Tree.Stm

  def apply(l: List[Stm]) = {
    val gen = new CodeGen()
    l foreach gen.munchStm
    gen.instr.toList
  }
}

class CodeGen {

  import tiger.{Tree => T}
  import tiger.{Asm => A}

  val instr = ListBuffer.empty[Instr]

  def emit(i: A.Instr): Unit = instr += i

  def munchStm(s: T.Stm): Unit = s match {

    case T.SEQ(a, b) => {
      munchStm(a)
      munchStm(b)
    }

    // casos con offset
    case T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, T.CONST(i), e2))) => {
      emit(A.OPER(asm = s"str    's0, ['s1, #$i]", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
    }

    case T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, e2, T.CONST(i)))) => {
      emit(A.OPER(asm = s"str    's0, ['s1, #$i]", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
    }

    // mem to mem
    case T.MOVE(T.MEM(e1), T.MEM(e2)) => {
      val tmp = Temp.newTemp()
      val te1 = munchExpr(e1)
      val te2 = munchExpr(e2)

      emit(A.OPER(asm = s"ldr    'd0, ['s0]", dst = List(tmp), src = List(te2)))
      emit(A.OPER(asm = s"str    's0, ['d0]", dst = List(te1), src = List(tmp)))

    }

    // algo a mem
    case T.MOVE(T.MEM(e1), e2) => {
      val te1 = munchExpr(e1)
      val te2 = munchExpr(e2)

      emit(A.OPER(asm = s"str    's1, ['s0]", src = List(te1, te2), dst = List()))
    }

    case T.MOVE(T.TEMP(i), T.CONST(n)) => {
      emit(A.OPER(asm = s"mov    'd0, #$n", src = List(), dst = List(i)))
    }

    case T.MOVE(T.TEMP(i), e) => {
      val te = munchExpr(e)

      emit(A.MOVE(asm = s"mov    'd0, 's0", src = te, dst = i))
    }

    case T.LABEL(lab) => {
      emit(A.LABEL(s".$lab:", lab))
    }

    case T.JUMP(T.NAME(f), l) => {
      emit(A.OPER(asm = s"b    .$f", jump = l))
    }

    case T.CJUMP(relop, e1, e2, t, f) => {
      def toAsm(r: T.Relop) = r match {
        case T.EQ => "beq"
        case T.NE => "bne"
        case T.LT => "blt"
        case T.LE => "ble"
        case T.GT => "bgt"
        case T.GE => "bge"
      }

      val te1 = munchExpr(e1)
      val te2 = munchExpr(e2)


      emit(A.OPER(asm = s"cmp     's0, 's1", src = List(te2, te1), dst = List()))
      emit(A.OPER(asm = s"${toAsm(relop)}     $t", jump = List(t,f)))
    }

    case T.EXP(T.CALL(T.NAME(f), args)) => {
      val v = munchArgs(args)
      emit(A.OPER(asm = s"bx     $f", src = v, dst = Frame.callerSave))
    }
    case T.EXP(e) => munchExpr(e)

    case _ => throw new Error("munchStm")

  }

  def result(gen: (Temp.Temp) => Unit) = {
    val r = Temp.newTemp()
    gen(r)
    r
  }

  def munchExpr(expr: T.Expr): Temp.Temp = {
    expr match {
      case CONST(n) => result(r => emit(A.OPER(asm = s"mov    'd0, #$n", src = List(), dst = List(r))))
      case NAME(l) => result(r => emit(A.OPER(asm = s"mov    'd0, .$l", src = List(), dst = List(r))))

      case TEMP(t) => t

      case BINOP(T.PLUS, left, right) =>
        result(r => emit(A.OPER(asm = s"add     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      case BINOP(T.MINUS, left, right) =>
        result(r => emit(A.OPER(asm = s"sub     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      case BINOP(T.MUL, left, right) =>
        result(r => emit(A.OPER(asm = s"mul     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      /* está bien devolver el .RV? */
      case BINOP(T.DIV, left, right) => {
        munchStm(T.EXP(T.CALL(T.NAME(Temp.namedLabel("_idiv_runtime")), List(left, right))))
        Frame.RV
      }

      case BINOP(T.AND, left, right) =>
        result(r => emit(A.OPER(asm = s"and     'd0, 's0, 's1", src = List(r), dst = List(munchExpr(left), munchExpr(right)))))

      case BINOP(T.OR, left, right) =>
        result(r => emit(A.OPER(asm = s"orr     'd0, 's0, 's1", src = List(r), dst = List(munchExpr(left), munchExpr(right)))))

      case MEM(e) => result(r => emit(A.OPER(asm = "mov     'd0, 's0 ", src = List(r), dst = List(munchExpr(e)))))

      case ESEQ(s, e) => {
        munchStm(s)
        munchExpr(e)
      }

      case _ => throw new Error("munchExp")
    }
  }

  def munchArgs(l: List[T.Expr]): List[Temp.Temp] = {
    val (toReg, toStack) = l.splitAt(Frame.argsRegisters.size)

    val l1 = for ((e, r) <- toReg zip Frame.argsRegisters) yield munchArgsReg(e, r)

    val l2 = for ((e, o) <- toStack.zipWithIndex) yield munchArgsStack(e, o)

    val l3 = for ((instr, regs) <- l2 ++ l1) yield {
      emit(instr)
      regs
    }

    l3.flatten.distinct.toList
  }

  def munchArgsReg(exp: T.Expr, reg: Temp.Temp): (A.Instr, List[Temp.Temp]) = exp match {
    case CONST(c) => (A.OPER(asm = s"mov     'd0, #$c ", src = List(), dst = List(reg), jump = None), List(reg))
    case T.TEMP(t) => (A.MOVE(asm = s"mov     'd0, 's0 ", src = t, dst = reg), List(reg, t))
    case _ =>
      val e = munchExpr(exp)
      (A.MOVE(asm = s"mov     'd0, 's0 ", src = e, dst = reg), List(reg, e))
  }

  def munchArgsStack(exp: T.Expr, offset: Int): (A.Instr, List[Temp.Temp]) = {
    val e = munchExpr(exp)
    (A.OPER(asm = s"str     's0, ['d0, #${offset * Frame.WS}]", src = List(e), dst = List(Frame.SP), jump = None), List(e, Frame.SP))
  }


}
