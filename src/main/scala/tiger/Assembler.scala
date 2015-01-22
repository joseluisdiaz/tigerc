package tiger

import tiger.Asm.Instr
import tiger.Temp._
import tiger.Tree._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object Asm {

  sealed abstract class Instr {
    def isMove: Boolean

    def isLabel: Boolean

    def asm: String

    protected def f(reg: String, v: List[String], base: String) = v.zipWithIndex.foldLeft(base) {
      case (str, (value, index)) => str.replaceFirst(s"'$reg$index", value)
    }

    def code: String

  }

  case class OPER(asm: String, src: List[Temp.Temp], dst: List[Temp.Temp], jump: Option[List[Temp.Label]]) extends Instr {
    override def isMove = false

    override def isLabel = false

    override def code = f("s", src, f("d", dst, asm))

    override def toString = s"OPER( '$asm' // src: $src // dst: $dst // jump: $jump )"
  }

  object OPER {
    def apply(asm: String, src: List[Temp.Temp], dst: List[Temp.Temp]): OPER = OPER(asm, src, dst, None)

    def apply(asm: String, jump: List[Temp.Label]): OPER = OPER(asm, List(), List(), Some(jump))
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
  import tiger.{Asm => A}

  def apply(frames: Map[Temp.Label, Frame], l: List[Stm]) = {
    val gen = new CodeGen(frames)
    l foreach gen.munchStm
    (gen.instr.toList, Data(gen.label, gen.seenLabels.keys.toList))
  }
}

case class Data(l:Temp.Label, ls:List[Temp.Label])

class CodeGen(frames: Map[Temp.Label, Frame]) {

  import tiger.{Tree => T}
  import tiger.{Asm => A}

  val instr = ListBuffer.empty[Instr]
  val label = Temp.newLabel()
  val seenLabels = mutable.Map.empty[String, String]

  def labels(s:String) = seenLabels.getOrElseUpdate(s, s"${label}+${seenLabels.size * Frame.WS}")

  def emit(i: A.Instr): Unit = instr += i

  def munchStm(s: T.Stm): Unit = s match {

    case T.SEQ(a, b) => {
      munchStm(a)
      munchStm(b)
    }

    // casos con offset
    case T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, T.CONST(offset), e2))) => {
      emit(A.OPER(asm = s"ldr     's0, ['s1, #$offset]", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
    }

    case T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, e2, T.CONST(offset)))) => {
      emit(A.OPER(asm = s"ldr    's0, ['s1, #$offset]", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
    }

    case T.MOVE(e1, T.MEM(T.BINOP(T.MINUS, T.CONST(offset), e2))) => {
      emit(A.OPER(asm = s"ldr     's0, ['s1, #-$offset]", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
    }

    case T.MOVE(e1, T.MEM(T.BINOP(T.MINUS, e2, T.CONST(offset)))) => {
      emit(A.OPER(asm = s"ldr    's0, ['s1, #-$offset]", src = List(munchExpr(e1), munchExpr(e2)), dst = List()))
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

    case T.MOVE(T.TEMP(t), T.CONST(n)) => {
      emit(A.OPER(asm = s"mov    'd0, #$n", src = List(), dst = List(t)))
    }

    case T.MOVE(T.TEMP(t), e) => {
      val te = munchExpr(e)

      emit(A.MOVE(asm = s"mov    'd0, 's0", src = te, dst = t))
    }

    case T.LABEL(lab) => {
      emit(A.LABEL(s".$lab:", lab))
    }

    case T.JUMP(T.NAME(f), l) => {
      emit(A.OPER(asm = s"b    .$f", jump = l))
    }

    case T.CJUMP(relop, e1, e2, t, f) => {

      // inverted relation
      def toAsm(r: T.Relop) = r match {
        case T.EQ => "bne"
        case T.NE => "beq"
        case T.GE => "blt"
        case T.LT => "bge"
        case T.LE => "bgt"
        case T.GT => "ble"
      }


      val te1 = munchExpr(e1)
      val te2 = munchExpr(e2)

      emit(A.OPER(asm = s"cmp     's0, 's1", src = List(te1, te2), dst = List()))
      emit(A.OPER(asm = s"${toAsm(relop)}     $f", jump = List(t, f)))
    }

    case T.EXP(T.CALL(T.NAME(f), args)) => {

      val amount = frames(f).actualArg() * Frame.WS


      if (amount != 0) emit(A.OPER(asm = s"sub     sp, sp, #$amount", src = List(), dst = List()))
      val v = munchArgs(args, frames(f))
      emit(A.OPER(asm = s"bl     $f", src = v, dst = Frame.callerSaves))
      if (amount != 0) emit(A.OPER(asm = s"add     sp, sp, #$amount", src = List(), dst = List()))



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
      case NAME(l) => result(r => emit(A.OPER(asm = s"ldr    'd0, .${labels(l)}", src = List(), dst = List(r))))

      case TEMP(t) => t

      case BINOP(T.PLUS, left, right) =>
        result(r => emit(A.OPER(asm = s"add     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      case BINOP(T.MINUS, left, right) =>
        result(r => emit(A.OPER(asm = s"sub     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      case BINOP(T.MUL, left, right) =>
        result(r => emit(A.OPER(asm = s"mul     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      case BINOP(T.DIV, left, right) => {
        munchStm(T.EXP(T.CALL(T.NAME(Temp.namedLabel("_idiv_runtime")), List(left, right))))
        Frame.RV
      }

      case BINOP(T.AND, left, right) =>
        result(r => emit(A.OPER(asm = s"and     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      case BINOP(T.OR, left, right) =>
        result(r => emit(A.OPER(asm = s"orr     'd0, 's0, 's1", src = List(munchExpr(left), munchExpr(right)), dst = List(r))))

      case MEM(BINOP(oper,TEMP(t),CONST(offset))) => {

        val sign = if (oper == MINUS) "-" else ""

        result { r =>
          emit(A.OPER(asm = s"ldr     'd0, ['s0, #${sign}$offset]", src = List(t), dst = List(r), jump = None))
        }

      }

      case MEM(e) => result(r => emit(A.MOVE(asm = "mov     'd0, 's0", src = munchExpr(e), dst = r)))

      case ESEQ(s, e) => {
        munchStm(s)
        munchExpr(e)
      }

      case _ => throw new Error("munchExp")
    }
  }

  /*
   *
   */

  def munchArgs(l: List[T.Expr], frame:Frame): List[Temp.Temp] = {
    val formalsValues = frame.formals.map { x => x.exp(Frame.FP) } zip l

    val argsRegisters = frame.argsRegisters.iterator
    var i = 0

    val regs = formalsValues map {
      case (T.TEMP(t), exp) => munchArgsReg(argsRegisters.next(), exp)
      case (T.MEM(_), exp) => {
        val r = munchArgsStack(i, exp)
        i += 4
        r
      }
      case _ => sys.error("exploto munchArgs")
    }

    regs.flatten.distinct
  }

   def munchArgsReg(reg: Temp.Temp, exp: T.Expr): List[Temp.Temp] = exp match {
    case CONST(c) => {
      emit(A.OPER(asm = s"mov     'd0, #$c ", src = List(), dst = List(reg), jump =None))
      List(reg)
    }
    case T.TEMP(t) => {
      emit(A.MOVE(asm = s"mov     'd0, 's0 ", src = t , dst = reg))
      List(reg, t)
    }

    case _ => {
      val e = munchExpr(exp)
      emit(A.MOVE(asm = s"mov     'd0, 's0 ", src = e, dst = reg))
      List(reg, e)
    }
  }

  def munchArgsStack(offset: Int, exp: T.Expr): List[Temp.Temp] = {
    val e = munchExpr(exp)

    val asm = if (offset == 0) s"str     's0, [sp]" else s"str     's0, [sp, #$offset]"

    emit(A.OPER(asm = asm, src = List(e), dst = List(), jump = None))
    List(e)
  }


}
