package tiger

import tiger.Tree._
import tiger.Tree.MEM
import tiger.Tree.CALL
import tiger.Tree.TEMP
import tiger.Tree.CONST
import tiger.Tree.BINOP

/**
 * |    argn    |  fp+4*(n+1)
 * |    ...     |
 * |    arg2    |  fp+16
 * |    arg1    |  fp+12
 * |  fp level  |  fp+8
 * |  retorno   |  fp+4
 * |   fp ant   |  fp
 * --------------  fp
 * |   local1   |  fp-4
 * |   local2   |  fp-8
 * |    ...     |
 * |   localn   |  fp-4*n
 */

trait Frame {
  type Access = Frame.Access

  def name(): Temp.Label
  def formals(): List[Access]
  def allocLocal(esc: Boolean): Access
  def allocArg(esc: Boolean): Access

  val calleeSave = List()
  val callerSave = List()

}

trait ArmConstants {
  val WS = 4
  val localGap = 4
  val localIncrement = 1
  val argsLocal = 4
  val argsIncrement = 1
}

class ArmFrame(n: Temp.Label, f: List[Boolean]) extends Frame with ArmConstants {
  import tiger.Frame.{InReg, InFrame}

  var actualArg = 0
  var actualLocal = 0
  var actualReg = 0
  var formals: List[Access] = Nil

  //  override def formals(): List[Access] = Nil

  override def name(): Temp.Label = n

  override def allocLocal(esc: Boolean): Access = if (esc) {
    val ret = InFrame((actualLocal * WS) + localGap)
    actualLocal = actualLocal - localIncrement
    ret
  } else {
    InReg(Temp.newTemp())
  }

  override def allocArg(esc: Boolean): Access = if (esc)  {
    val ret = InFrame((actualArg * WS) + argsLocal)
    actualArg = actualArg + argsIncrement
    ret
  }
  else {
    InReg(Temp.newTemp())
  }

}

object Frame extends ArmConstants {
  def apply(name: Temp.Label, formals: List[Boolean]) = new ArmFrame(name, formals)

  sealed class Frag
  case class PROC(body: Stm, frame: Frame) extends Frag
  case class STRING(label: Temp.Label, s: String) extends Frag

  def extenalCall(name:String, args:Exp*) = CALL(NAME(Temp.namedLabel(name)), args.toList)

  sealed class Access
  case class InFrame(i: Int) extends Access
  case class InReg(l: Temp.Label) extends Access

  val FP = "FP"
  val RV = "RV"
  val SL = "R1"

  def exp(access:Access, fp:Tree.Exp) = access match {
    case InFrame(i) => MEM(BINOP(PLUS, fp, CONST(i))
    case InReg(l) => TEMP(l)
  }

}


