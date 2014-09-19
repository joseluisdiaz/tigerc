package tiger

import tiger.Asm.Instr
import tiger.Tree._

object Asm {

  sealed abstract class Instr {
    def isMove:Boolean
    def isLabel:Boolean
    def asm:String
  }

  case class OPER(asm: String, src: List[Temp.Temp], dst: List[Temp.Temp], jump: Option[List[Temp.Label]]) extends Instr {
    override def isMove = false
    override def isLabel = false
  }

  object OPER {
    def apply(asm: String, src: List[Temp.Temp], dst: List[Temp.Temp]):OPER = OPER(asm, src, dst, None)
    def apply(asm: String, jump: List[Temp.Label]):OPER = OPER(asm, List(), List(), Some(jump))
  }

  case class LABEL(asm: String, l: Temp.Label) extends Instr {
    override def isMove = false
    override def isLabel = true
  }

  case class MOVE(asm: String, src: Temp.Temp, dst: Temp.Temp) extends Instr {
    override def isMove = true
    override def isLabel = false
  }

}

class CodeGen {

  import tiger.{Tree => T}
  import tiger.{Asm => A}


  def emit(i: A.Instr): Unit = println()

  def munchStm(s: T.Stm): Unit = s match {

    case T.SEQ(a, b) => {
      munchStm(a)
      munchStm(b)
    }

    // casos con offset
    case T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(i), e1)), e2) => {
      emit(A.OPER(asm = s"str    `s1, [`s0, #$i]\n", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
    }

    case T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST(i))), e2) => {
      emit(A.OPER(asm = s"str    `s1, [`s0, #$i]\n", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
    }

    // mem to mem
    case T.MOVE(T.MEM(e1), T.MEM(e2)) => {
      val tmp = Temp.newTemp()
      val te1 = munchExpr(e1)
      val te2 = munchExpr(e2)

      emit(A.OPER(asm = s"ldr    `d0, [`s0]\n", dst = List(tmp), src = List(te2)))
      emit(A.OPER(asm = s"str    `s0, [`d0]\n", dst = List(te1), src = List(tmp)))

    }

    // algo a mem
    case T.MOVE(T.MEM(e1), e2) => {
      val te1 = munchExpr(e1)
      val te2 = munchExpr(e2)

      emit(A.OPER(asm = s"str    `s1, [`s0]\n", src = List(te1, te2), dst = List()))
    }

    case T.MOVE(T.TEMP(i), T.CONST(n)) => {
      emit(A.OPER(asm = s"mov    `d0, #$n\n", src = List(), dst = List(i)))
    }

    case T.MOVE(T.TEMP(i), e) => {
      val te = munchExpr(e)

      emit(A.MOVE(asm = s"mov    `d0, `s0\n", src = te, dst = i))
    }

    case T.LABEL(lab) => {
      emit(A.LABEL(s".$lab:", lab))
    }

    case T.JUMP(T.NAME(f), l) => {
      emit(A.OPER(asm = s"b    .$s\n", jump = l))
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

      val next = Temp.newLabel()

      emit(A.OPER(asm = s"cmp     `s0, `s1\n", src = List(te1, te2), dst = List()))
      emit(A.OPER(asm = s"${toAsm(relop)}     $t", jump = List(t,next)))
      emit(A.LABEL(asm = s"$next:             \n", l = next))
    }

    case T.EXP(T.CALL(T.NAME(f), args)) => {
      munchArgs(args)
      emit(A.OPER(asm = s"bx     $f\n", src = munchArgs(args), dst = Frame.callerSave))
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
      case CONST(n) => result(r => emit(A.OPER(asm = s"mov    `d0, #$n\n", src = List(r), dst = List())))
      case NAME(l) => result(r => emit(A.OPER(asm = s"mov    `d0, .$l\n", src = List(r), dst = List())))

      case TEMP(t) => t

      case BINOP(T.AND, left, right) =>
        result(r => emit(A.OPER(asm = s"add     `d0, `s0, `s1\n", src = List(r), dst = List(munchExpr(left), munchExpr(right)))))


      case BINOP(T.MINUS, left, right) =>
        result(r => emit(A.OPER(asm = s"sub     `d0, `s0, `s1\n", src = List(r), dst = List(munchExpr(left), munchExpr(right)))))


      case BINOP(T.MINUS, left, right) =>
        result(r => emit(A.OPER(asm = s"min     `d0, `s0, `s1\n", src = List(r), dst = List(munchExpr(left), munchExpr(right)))))


      case BINOP(T.AND, left, right) =>
        result(r => emit(A.OPER(asm = s"and     `d0, `s0, `s1\n", src = List(r), dst = List(munchExpr(left), munchExpr(right)))))


      case BINOP(T.OR, left, right) =>
        result(r => emit(A.OPER(asm = s"orr     `d0, `s0, `s1\n", src = List(r), dst = List(munchExpr(left), munchExpr(right)))))


      /* está bien devolver el .RV? */
      case BINOP(T.DIV, left, right) => {
        munchStm(T.EXP(T.CALL(T.NAME(Temp.namedLabel("_idiv_runtime")), List(left, right))))
        Frame.RV
      }

      case MEM(e) => result(r => emit(A.OPER(asm = "mov     `d0, %`s0 \n", src = List(r), dst = List(munchExpr(e)))))

      case ESEQ(s, e) => {
        munchStm(s)
        munchExpr(e)
      }

      case _ => throw new Error("munchExp")
    }
  }

  def munchArgs(l: List[T.Expr]): List[Temp.Temp] = {
    val (toReg, toStack) = l.splitAt(Frame.argsRegistersSize)

    val l1 = for ((e, r) <- toReg zip Frame.argsRegisters) yield munchArgsReg(e, r)

    val l2 = for ((e, o) <- toStack.zipWithIndex) yield munchArgsStack(e, o)

    val l3 = for ((instr, regs) <- l2 ++ l1) yield {
      emit(instr)
      regs
    }

    l3.flatten.distinct.toList
  }

  def munchArgsReg(exp: T.Expr, reg: Temp.Temp): (A.Instr, List[Temp.Temp]) = exp match {
    case CONST(c) => (A.OPER(asm = s"mov     `d0, #$c \n", src = List(), dst = List(reg), jump = None), List(reg))
    case T.TEMP(t) => (A.MOVE(asm = s"mov     `d0, `s0 \n", src = t, dst = reg), List(reg, t))
    case _ =>
      val e = munchExpr(exp)
      (A.MOVE(asm = s"mov     `d0, `s0 \n", src = e, dst = reg), List(reg, e))
  }

  def munchArgsStack(exp: T.Expr, offset: Int): (A.Instr, List[Temp.Temp]) = {
    val e = munchExpr(exp)
    (A.OPER(asm = s"str     `s0, [`d0, #${offset * Frame.WS}]\n", src = List(e), dst = List(Frame.SP), jump = None), List(e, Frame.SP))
  }


}
